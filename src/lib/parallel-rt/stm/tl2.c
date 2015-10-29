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

#define CFENCE __asm__ volatile ("":::"memory")

struct tvar{
    volatile Word_t * contents;
    volatile unsigned long currentLock;
    volatile unsigned long oldLock;
    volatile unsigned long refCount;
};

struct write_set{
    struct tvar * tvar;
    Value_t * val;
    struct write_set * next;
};

struct tl2_read_log{
    Value_t tag;
    struct tvar * tvar;
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

struct ff_read_set{
    unsigned long stamp;
    struct tl2_read_log * lastK;
};

#define WITHK_TAG 1
#define WITHOUTK_TAG 3
#define LOCKED(x) (x & 1)

static Value_t revalidate(struct tl2_read_set * readSet, VProc_t * vp, unsigned long * myStamp, volatile unsigned long * clock){
    
    while(1){
	unsigned long rawStamp = clock[0];
	int kCount = 0;
	struct tl2_read_log * checkpoint = readSet->head;
	struct tl2_read_log * ptr = readSet->head->next;
	while((Value_t)ptr != M_NIL){
	    if(ptr->tag == WITHOUTK_TAG){  //WithoutK
		unsigned long stamp = ptr->tvar->currentLock;
		if(stamp > rawStamp || (stamp & 1) == 1){
		    checkpoint->next = M_NIL;
		    break;
		}
	    }else{  //WithK
		unsigned long stamp = ptr->tvar->currentLock;
		if(stamp > rawStamp || (stamp & 1) == 1){
		    //we have a checkpoint here, but its out of date
		    checkpoint = ptr;
		    checkpoint->next = M_NIL;
		    kCount++;
		    break;
		}
		checkpoint = ptr;
		kCount++;
	    }
	    ptr = ptr->next;
	}
	//read the checkpointed tvar
	struct tvar * tv = checkpoint->tvar;
	
	unsigned long v1, v2;

	v1 = tv->currentLock;
	CFENCE;
	Word_t * contents = tv->contents;
	CFENCE;
	v2 = tv->currentLock;
	
	if(!LOCKED(v1) && v1 == v2 && v1 <= rawStamp){
	    myStamp[0] = rawStamp;
	    return AllocNonUniform(vp, 4, INT(kCount), PTR(readSet->head), PTR(checkpoint), PTR(contents));
	}
    }
}

static Value_t ffFinishTL2(struct tl2_read_set * readSet, struct tl2_read_log * checkpoint, int kCount, VProc_t * vp, unsigned long * myStamp,
		    volatile unsigned long * clock){
    struct tvar * tv = checkpoint->tvar;
    
    unsigned long v1, v2;
    v1 = tv->currentLock;
    CFENCE;
    Word_t * contents = tv->contents;
    CFENCE;
    v2 = tv->currentLock;

    checkpoint->next = (struct tl2_read_log *)M_NIL;
    
    if(LOCKED(v1) || v1 != v2 || v1 > myStamp[0]){
	return M_UNIT; //revalidate(readSet, vp, myStamp, clock);
    }
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
 * myStamp - read_version time stamp (myStamp[3] contains the thread's id)
 * clock - global version clock
 * vp - vproc structure
 */
Value_t fastForwardTL2(struct tl2_read_set * readSet, struct ff_read_set * ffInfo, struct write_set * writeSet, Word_t* tv,
		       Word_t* retK, Word_t* myStamp, volatile unsigned long * clock, VProc_t * vp){
    struct tl2_read_log * shortPath = ffInfo->lastK;
    while((Value_t)shortPath != M_NIL){
        if(shortPath->tvar == tv){
            if (M_PolyEq(retK, shortPath->k)){
                if(shortPath->writeSet == writeSet){
                    Value_t result = ffValidateTL2(readSet, shortPath, myStamp, clock, vp, ffInfo->stamp);
		    if(result != M_UNIT){
			clearBits(ffInfo->lastK, myStamp[3]);

			shortPath->nextK = readSet->lastK;
			readSet->tail->next = shortPath;
			
			RS_t * remSet = vp->rememberSet;
			Value_t remSet2 = AllocNonUniform (vp, 4, PTR(shortPath), INT(5), INT(myStamp[3]), PTR(remSet));
			vp->rememberSet = remSet2;
		    }
                    return result;
                }
            }
        }
        shortPath = shortPath->nextK;
    }
    return M_UNIT;
}





/*
Word_t * ff_tl2_try_extend(struct read_set * readSet, unsigned long * myStamp, volatile unsigned long * clock){
    unsigned long rawStamp = clock[0];
    int kCount = 0;
    struct tl2_read_log * checkpoint = readSet->head;
    struct tl2_read_log * ptr = readSet->head->next;
    while((Value_t)ptr != M_NIL){
	if(ptr->tag == WITHOUTK_TAG){  //WithoutK
	    unsigned long stamp = ptr->tvar->currentLock;
	    if(stamp > rawStamp || (stamp & 1) == 1){
		checkpoint->next = M_NIL;
		break;
	    }
	}else{  //WithK
	    unsigned long stamp = ptr->tvar->currentLock;
	    if(stamp > rawStamp || (stamp & 1) == 1){
		//we have a checkpoint here, but its out of date
		checkpoint = ptr;
		checkpoint->next = M_NIL;
		kCount++;
		break;
	    }
	    checkpoint = ptr;
	    kCount++;
	}
	ptr = ptr->next;
    }
    //read the checkpointed tvar
    struct tvar * tv = checkpoint->tvar;
    
    unsigned long v1, v2;
    
    v1 = tv->currentLock;
    CFENCE;
    Word_t * contents = tv->contents;
    CFENCE;
    v2 = tv->currentLock;
    
    if(!LOCKED(v1) && v1 == v2 && v1 <= rawStamp){
	myStamp[0] = rawStamp;
	return AllocNonUniform(vp, 4, INT(kCount), PTR(readSet->head), PTR(checkpoint), PTR(contents));
    }
}

Word_t * ff_tl2_read_tvar(struct tvar * tvar, VProc_t * vp, unsigned long * myStamp, struct read_set * readSet, volatile unsigned long * clock){

    unsigned long inital_stamp = myStamp[0];
 RETRY:
    unsigned long v1 = tvar->currentLock;
    CFENCE;
    Word_t * val = tvar->contents;
    CFENCE;
    unsigned long v2 = tvar->currentLock;

    if(LOCKED(v1) || v1 != v2 || v1 > myStamp[0]){
	Word_t * res = ff_tl2_try_extend(readSet, myStamp, clock);
	if(res == NULL)
	    goto RETRY;
	else
	    return res;
    }
    
    return val;
}
*/
