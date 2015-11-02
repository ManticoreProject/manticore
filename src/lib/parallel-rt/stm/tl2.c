/* tl2.c
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * STM related functions
 */

#include "manticore-rt.h"
#include <stdarg.h>
#include <string.h>
#include "value.h"
#include "vproc.h"
#include "remember-set.h"
#include "eventlog.h"
#include <stdio.h>
#include "atomic-ops.h"
#include <stddef.h>
#include "fls-dictionary-offsets.h"

#define CFENCE __asm__ volatile ("":::"memory")

struct tl2_tvar{
    volatile Value_t contents;
    volatile unsigned long currentLock;
    volatile unsigned long oldLock;
    volatile unsigned long refCount;
};

struct write_set{
    struct tl2_tvar * tvar;
    Value_t val;
    struct write_set * next;
};

struct tl2_read_log{
    Value_t tag;
    struct tl2_tvar * tvar;
    struct tl2_read_log * next;
    Word_t * k;
    struct write_set * writeSet;
    struct tl2_read_log * nextK;
};

struct tl2_read_set{
    int numK;
    struct tl2_read_log * head;
    struct tl2_read_log * lastK;
    struct tl2_read_log * tail;
};


#define WITHK_TAG 1
#define WITHOUTK_TAG 3
#define LOCKED(x) (x & 1)
#define READ_TVAR(v1, v2, tv, contents, checkpoint) \
    tv = checkpoint->tvar;			    \
    v1 = tv->currentLock;			    \
    CFENCE;					    \
    contents = tv->contents;			    \
    CFENCE;					    \
    v2 = tv->currentLock;

static Value_t revalidate(struct tl2_read_set * readSet, VProc_t * vp, unsigned long * myStamp,
			  volatile unsigned long * clock, struct tl2_read_log * sentinel){
    struct tl2_tvar * tvar;
    unsigned long v1, v2;
    Value_t contents;
    while(1){
	unsigned long newStamp = clock[0];
	unsigned long rawStamp = myStamp[0];
	int kCount = 0;
	struct tl2_read_log * checkpoint = readSet->head;
	struct tl2_read_log * ptr = readSet->head->next;
	while(ptr != sentinel){
	    if(ptr->tag == WITHOUTK_TAG){  //WithoutK
		unsigned long stamp = ptr->tvar->currentLock;
		if(stamp > rawStamp || LOCKED(stamp)){
		    break;
		}
	    }else{  //WithK
		unsigned long stamp = ptr->tvar->currentLock;
		if(stamp > rawStamp || LOCKED(stamp)){
		    //we have a checkpoint here, but its out of date
		    checkpoint = ptr;
		    kCount++;
		    break;
		}
		checkpoint = ptr;
		kCount++;
	    }
	    ptr = ptr->next;
	}
	
	myStamp[0] = newStamp;
	READ_TVAR(v1, v2, tvar, contents, checkpoint);
	if(!LOCKED(v1) && v1 == v2 && v1 <= newStamp){
	    checkpoint->next = M_NIL;
	    return AllocNonUniform(vp, 4, INT(kCount), PTR(readSet->head), PTR(checkpoint), PTR(contents));
	}
	sentinel = checkpoint->next; //only go up to checkpoint next time
    }
}

static Value_t ffFinishTL2(struct tl2_read_set * readSet, struct tl2_read_log * checkpoint, int kCount, VProc_t * vp, unsigned long * myStamp,
			   volatile unsigned long * clock){
    struct tl2_tvar * tv = checkpoint->tvar;
    
    unsigned long v1, v2;
    v1 = tv->currentLock;
    CFENCE;
    Value_t contents = tv->contents;
    CFENCE;
    v2 = tv->currentLock;

    if(LOCKED(v1) || v1 != v2 || v1 > myStamp[0]){
	return  revalidate(readSet, vp, myStamp, clock, checkpoint->next);
    }

    checkpoint->next = (struct tl2_read_log *)M_NIL;
    
    Value_t newRS = AllocNonUniform(vp, 4, INT(kCount), PTR(readSet->head), PTR(checkpoint), PTR(contents));
    return newRS;
}

static Value_t ffValidateTL2(struct tl2_read_set * readSet, struct tl2_read_log * oldRS, unsigned long * myStamp,
			     volatile unsigned long * clock, VProc_t * vp, unsigned long oldStamp){
    int kCount = readSet->numK;
    struct tl2_read_log * checkpoint = oldRS;
    struct tl2_read_log * orig = oldRS;
    while((Value_t)oldRS != M_NIL){
        if(oldRS->tag == WITHOUTK_TAG){  //WithoutK
	    unsigned long stamp = oldRS->tvar->currentLock;
	    if(stamp > oldStamp || LOCKED(stamp)){
		return ffFinishTL2(readSet, checkpoint, kCount, vp, myStamp, clock);
	    }
        }else{  //WithK
	    unsigned long stamp = oldRS->tvar->currentLock;
	    if(stamp > oldStamp || LOCKED(stamp)){
		//we have a checkpoint here, but its out of date
		return ffFinishTL2(readSet, oldRS, kCount + 1, vp, myStamp, clock);
	    }
	    checkpoint = oldRS;
	    kCount++;
        }
        oldRS = oldRS->next;
    }
    return ffFinishTL2(readSet, checkpoint, kCount, vp, myStamp, clock);
}

static void clearBits(struct tl2_read_log * ff, unsigned long tid){
    unsigned long mask = ~(1 << tid);
    while((Value_t) ff != M_NIL){
        FetchAndAndU64((volatile int64_t *)&(ff->tvar->refCount), mask);
        ff = ff->nextK;
    }
}

/*
 * readSet - the current read set for the transaction
 * ffInfo - fast forward read set including the time stamp of the previous attempt
 * writeSet - write set
 * tv - tvar we are currently reading from / trying to fast forward on
 * retK - our current continuation
 * myStamp - read_version time stamp (myStamp[3] contains the thread's id myStamp[2] contains the old timestamp)
 * clock - global version clock
 * vp - vproc structure
 */
Value_t fastForwardTL2(struct tl2_read_set * readSet, struct tl2_read_log * ffInfo, struct write_set * writeSet, struct tl2_tvar* tv,
		       Word_t* retK, Word_t* myStamp, volatile unsigned long * clock, VProc_t * vp){
    struct tl2_read_log * shortPath = ffInfo;
    while((Value_t)shortPath != M_NIL){
        if(shortPath->tvar == tv){
            if (M_PolyEq(retK, shortPath->k)){
                if(shortPath->writeSet == writeSet){
		    clearBits(ffInfo, myStamp[3]);
		    shortPath->nextK = readSet->lastK;
		    readSet->tail->next = shortPath;
		    
		    RS_t * remSet = vp->rememberSet;
		    Value_t remSet2 = AllocNonUniform (vp, 4, PTR(shortPath), INT(5), INT(myStamp[3]), PTR(remSet));
		    vp->rememberSet = remSet2;
		    
		    Value_t result = ffValidateTL2(readSet, shortPath, myStamp, clock, vp, myStamp[2]);
                    return result;
                }
            }
        }
        shortPath = shortPath->nextK;
    }
    return M_UNIT;
}

/*
 * This sets the thread's bit in every tref on the short path of the newly created fast
 * forward read set.  This will also split the read set at the appropriate place
 */
static void setBits(struct tl2_read_log * ff, struct tl2_read_log * sentinel, unsigned long tid){
    if(ff == sentinel)
	return;
    unsigned long mask = (1 << tid);
    while( (Value_t) ff != M_NIL){
        FetchAndOrU64((volatile int64_t *)&(ff->tvar->refCount), mask);

	if(ff->nextK == sentinel){
	    ff->nextK = M_NIL;
	}
	
        ff = ff->nextK;
    }
}

bool ff_tl2_try_extend(unsigned long * myStamp, volatile unsigned long * clock, VProc_t * vp, struct tl2_read_set * readSet){

    struct tl2_tvar * tv;
    unsigned long v1, v2;
    Word_t * contents;
 
    struct tl2_read_log * sentinel = (struct tl2_read_log *) M_NIL;
    RETRY:
    {
	unsigned long rawStamp = myStamp[0];
	unsigned long newStamp = clock[0];
	int kCount = 0;
	struct tl2_read_log * checkpoint = readSet->head;
	struct tl2_read_log * ptr = readSet->head->next;
	while(ptr != sentinel){
	    if(ptr->tag == WITHOUTK_TAG){  //WithoutK
		unsigned long stamp = ptr->tvar->currentLock;
		if(stamp > rawStamp || LOCKED(stamp)){
		    READ_TVAR(v1, v2, tv, contents, checkpoint)
		    myStamp[0] = newStamp;
		    if(!LOCKED(v1) && v1 == v2 && v1 <= newStamp){
			checkpoint->next = M_NIL;
			//pass back the result
			setBits(readSet->lastK, checkpoint, myStamp[3]);
			readSet->numK = kCount;
			readSet->lastK = checkpoint;
			readSet->tail = (struct tl2_read_log *) contents;
			return false;
		    }
		    sentinel = checkpoint; //don't go past this checkpoint the next time around
		    goto RETRY;
		}
	    }else{  //WithK
		unsigned long stamp = ptr->tvar->currentLock;
		if(stamp > rawStamp || LOCKED(stamp)){
		    READ_TVAR(v1,v2,tv,contents,ptr);
		    myStamp[0] = newStamp;
		    if(!LOCKED(v1) && v1 == v2 && v1 <= newStamp){
			ptr->next = M_NIL;
			//pass back the result
			setBits(readSet->lastK, ptr, myStamp[3]);
			readSet->numK = kCount + 1;
			readSet->lastK = ptr;
			readSet->tail = (struct tl2_read_log *) contents;
			return false;
		    }
		    sentinel = ptr; //don't go past this checkpoint the next time around
		    goto RETRY;
		}
		checkpoint = ptr;
		kCount++;
	    }
	    ptr = ptr->next;
	}
	myStamp[0] = newStamp;
	if((Value_t)ptr == M_NIL){
	    //this was our first pass through
	    return true;
	}else{
	    READ_TVAR(v1,v2,tv,contents,ptr);
	    if(!LOCKED(v1) && v1 == v2 && v1 <= newStamp){
		ptr->next = M_NIL;
		setBits(readSet->lastK, ptr, myStamp[3]);
		readSet->numK = kCount;
		readSet->lastK = ptr;
		readSet->tail = (struct tl2_read_log *) contents;
		return false;
	    }
	    sentinel = ptr;
	    goto RETRY;
	}
    }
}

Value_t ff_tl2_read_tvar(struct tl2_tvar * tvar, VProc_t * vp, unsigned long * myStamp, volatile unsigned long * clock, struct tl2_read_set * readSet){

 RETRY:
    {
	unsigned long v1 = tvar->currentLock;
	CFENCE;
	Word_t * val = tvar->contents;
	CFENCE;
	unsigned long v2 = tvar->currentLock;
	
	if(LOCKED(v1) || v1 != v2 || v1 > myStamp[0]){
	    unsigned long oldStamp = myStamp[0];
	    bool res = ff_tl2_try_extend(myStamp, clock, vp, readSet);
	    if(res) //time stamp extended!
		goto RETRY;
	    else{
		myStamp[2] = oldStamp;
		//the read set contains the necessary information to finish the abort in BOM
		return readSet;  //validation failed!
	    }
	}
	return val;
    }
}

