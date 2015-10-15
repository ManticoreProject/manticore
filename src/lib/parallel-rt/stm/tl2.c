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

struct tvar{
    volatile Word_t * contents;
    volatile unsigned long currentLock;
    volatile unsigned long oldLock;
    volatile unsigned long refCount;
};

struct write_set{
    Value_t tag;
    struct tvar * tvar;
    Value_t * val;
    struct write_set * next;
};

struct read_log{
    Value_t tag;
    struct tvar * tvar;
    struct read_log * next;
    Word_t * k;
    struct write_set * writeSet;
    struct read_log * nextK;
};

struct read_set{
    unsigned long numK;
    struct read_log * head;
    struct read_log * lastK;
    struct read_log * tail;
};

struct ff_read_set{
    unsigned long stamp;
    struct read_log * firstK;
    struct read_log * lastK;
};



Value_t ffFinishTL2(struct read_set * readSet, struct read_log * checkpoint, unsigned long kCount, VProc_t * vp){
    struct tvar * tv = checkpoint->tvar;
    


    
    checkpoint->next = (struct read_log *)M_NIL;
    //printf("Fast forwarding through %lu checkpoints\n", kCount - readSet->numK);
    Value_t newRS = AllocNonUniform(vp, 4, PTR(readSet->head), PTR(checkpoint), PTR(checkpoint), INT(kCount));
    return newRS;
}

Value_t ffValidateTL2(struct read_set * readSet, struct read_log * oldRS, unsigned long * myStamp, volatile unsigned long * clock, VProc_t * vp){
    unsigned long kCount = readSet->numK;
    struct read_log * checkpoint = oldRS;

    unsigned long rawStamp = *myStamp;
    
    while((Value_t)oldRS != M_NIL){
        if(oldRS->tag == (Value_t)5){  //WithoutK
	    unsigned long stamp = oldRS->tvar->currentLock;
	    if(stamp > rawStamp || (stamp & 1) == 1){
		return ffFinishTL2(readSet, checkpoint, kCount, vp);
	    }
        }else if (oldRS->tag == (Value_t) 3){  //WithK
	    unsigned long stamp = oldRS->tvar->currentLock;
	    if(stamp > rawStamp || (stamp & 1) == 1){
		//we have a checkpoint here, but its out of date
		return ffFinishTL2(readSet, checkpoint, kCount, vp);
	    }
	    checkpoint = oldRS;
	    kCount++;
        }
        oldRS = oldRS->next;
    }
    return ffFinishTL2(readSet, checkpoint, kCount, vp);
}

Value_t fastForwardTL2(struct read_set * readSet, struct ff_read_set * ffInfo, struct write_set * writeSet, Word_t* tv,
		       Word_t* retK, Word_t* myStamp, volatile unsigned long * clock, VProc_t * vp, struct tvar * dummy){
    struct read_log * shortPath = ffInfo->lastK;
    while((Value_t)shortPath != M_NIL){
        if(shortPath->tvar == tv){
            if (M_PolyEq(retK, shortPath->k)){
                if(shortPath->writeSet == writeSet){
                    decCounts(ffInfo->lastK);

                    shortPath->nextK = readSet->lastK;
                    readSet->tail->next = shortPath;

                    RS_t * remSet = vp->rememberSet;
                    Value_t remSet2 = AllocNonUniform (vp, 4, PTR(shortPath), INT(6), INT(myStamp[3]), PTR(remSet));
                    vp->rememberSet = remSet2;

                    Value_t result = ffValidateTL2(readSet, shortPath, myStamp, clock, vp);
                    return result;
                }else{
		    
                    /*
                    struct write_set * oldWS = shortPath->writeSet;
                    struct write_set * currentWS = writeSet;

                    ws_res res = classify_writesets(oldWS, currentWS);

                    if(res == NEQ){
                        return M_UNIT;
                    }
                    //hook read sets together
                    decCounts(ffInfo->lastK);
                    shortPath->nextK = readSet->lastK;
                    readSet->tail->next = shortPath;
                    RS_t * remSet = vp->rememberSet;
                    Value_t remSet2 = AllocNonUniform (vp, 4, PTR(shortPath), INT(6), INT(myStamp[3]), PTR(remSet));

                    if(res == EQ){//simple validation
                        vp->rememberSet = remSet2;
                        Value_t result = ffValidate(readSet, shortPath, myStamp, clock, vp);
                        return result;
                    }else{//check local reads are consistent
                        //merge write sets together
                        shortPath->writeSet->tvar = dummy;
                        shortPath->writeSet->next = currentWS;
                        Value_t remSet3 = AllocNonUniform(vp, 4, PTR(shortPath->writeSet), INT(3), INT(myStamp[3]), PTR(remSet2));
                        vp->rememberSet = remSet3;
                        return ffValidate2(readSet, shortPath, myStamp, clock, vp);
                    }  */
                }
            }
        }
        shortPath = shortPath->nextK;
    }
    return M_UNIT;
}







