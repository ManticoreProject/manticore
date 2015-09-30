/* stm.c
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
    Word_t * contents;
    unsigned long refCount;
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
    Word_t * readContents;
    struct read_log * next;
    struct write_set * writeSet;
    Word_t * k;
    struct read_log * nextK;
};

struct read_set{
    struct read_log * head;
    struct read_log * tail;
    struct read_log * lastK;
    unsigned long numK;
};

Value_t STM_Validate(unsigned long * myStamp, volatile unsigned long * clock, struct read_log * head, VProc_t * vp){
    RETRY:
    while(true){
        unsigned long time = * clock;
        if((time & 1) != 0){
            continue;
        }

        bool full = true;

        int kCount = 0;
        /* This head node is a dummy node with a tref that no one has access to, meaning it
         * is necessarily valid.  The continuation associated is the full abort continuation
         * this way we don't have to distinguish between full and partial aborts
         */
        struct read_log * checkpoint = head;  
        struct read_log * rs = head->next; //first real entry
        while(rs != (struct read_log * )M_NIL){
            if(rs->tag != (Value_t) 9){  //not a local read
                if(rs->tvar->contents != rs->readContents){
                    if(rs->tag == (Value_t) 3 && rs->k != M_UNIT){ //reread
                        Word_t * contents = rs->tvar->contents;
                        if(time != *clock)
                            goto RETRY;
                        rs->readContents = contents;  //contents is necessarily in the global heap
                        *myStamp = time;

                        vp->counter[0]++; //necessarily a partial abort
                        return AllocNonUniform(vp, 4, PTR(head), PTR(rs), PTR(rs), INT(kCount));
                    }
                    if(!full)
                        vp->counter[0]++;
                    *myStamp = time;
                    return AllocNonUniform(vp, 4, PTR(head), PTR(checkpoint), PTR(checkpoint), INT(kCount));
                }
                if(rs->tag == (Value_t) 3 && rs->k != M_UNIT){ //checkpointed entry
                    full = false;
                    checkpoint = rs;
                    kCount++;
                }
            }
            rs = rs->next;
        }
        if(time == *clock){
            *myStamp = time;
            return M_UNIT;
        }
    }
}

Value_t validate(unsigned long * myStamp, volatile unsigned long * clock, struct read_log * readSet, VProc_t * vp){
    struct read_log * checkpoint = (struct read_log *)M_NIL;
    int kCount = 0;
    while(true){
        unsigned long time = * clock;
        if((time & 1) != 0){
            continue;
        }

        struct read_log * rs = readSet;
        while(rs != (struct read_log * )M_NIL){
            if(rs->tag != (Value_t) 9){  //not a local read
                if(rs->tvar->contents != rs->readContents){
                    return AllocNonUniform(vp, 4, PTR(readSet), PTR(checkpoint), PTR(checkpoint), INT(kCount));
                }
                if(rs->tag == (Value_t) 3 && rs->k != M_UNIT){ //checkpointed entry
                    checkpoint = rs;
                    kCount++;
                }
            }
            rs = rs->next;
        }
        if(time == *clock){
            *myStamp = time;
            return M_UNIT;
        }
    }
}

void decCounts(struct read_log * ff){
    while((Value_t) ff != M_NIL){
        FetchAndAdd64((volatile int64_t *)&(ff->tvar->refCount), -1);
        ff = ff->nextK;
    }
}

Value_t ffFinish(struct read_set * readSet, struct read_log * checkpoint, unsigned long kCount, VProc_t * vp, int skipped){
#ifdef EVENT_LOGGING
    postAbortTX(vp, skipped, 17);
#endif
   // vp->counter[2] ++;
    checkpoint->next = (struct read_log *)M_NIL;
    //printf("Fast forwarding through %lu checkpoints\n", kCount - readSet->numK);
    Value_t newRS = AllocNonUniform(vp, 4, PTR(readSet->head), PTR(checkpoint), PTR(checkpoint), INT(kCount));
    return newRS;
}

Value_t ffValidate(struct read_set * readSet, struct read_log * oldRS, unsigned long * myStamp, volatile unsigned long * clock, VProc_t * vp){
    unsigned long kCount = readSet->numK;
    struct read_log * checkpoint = oldRS;

    int i = 0, j = 0;

    while((Value_t)oldRS != M_NIL){
        if(oldRS->tag == (Value_t)5){  //WithoutK
            if(oldRS->tvar->contents != oldRS->readContents){
                return ffFinish(readSet, checkpoint, kCount, vp, i);
            }
        }else if (oldRS->tag == (Value_t) 3){  //WithK
            if(oldRS->tvar->contents == oldRS->readContents){
                if(oldRS->k != M_UNIT){
                    i += j;
                    j = 0;
                    checkpoint = oldRS;
                    kCount++;
                }
            }else{      //oldRS->tag == (Value_t) 3
                if(oldRS->k != M_UNIT){
                    //we have a checkpoint here, but its out of date
                    Value_t v = oldRS->tvar->contents;
                    if(*clock != *myStamp){
                        do{
                            Value_t abortInfo = validate(myStamp, clock, readSet->head, vp);
                            if(abortInfo != M_UNIT){
                                return abortInfo;
                            }
                            v = oldRS->tvar->contents;
                        }while(*clock != *myStamp);
                    }
                    oldRS->readContents = v;
                    oldRS->next = M_NIL;
                    return AllocNonUniform(vp, 4, PTR(readSet->head), PTR(oldRS), PTR(oldRS), INT(kCount));
                }else{
                    return ffFinish(readSet, checkpoint, kCount, vp, i);
                }
            }
        }//else local read, do nothing...
        j++;
        oldRS = oldRS->next;
    }
    return ffFinish(readSet, checkpoint, kCount, vp, i);
    //return AllocNonUniform(vp, 4, PTR(readSet->head), PTR(checkpoint), PTR(checkpoint), INT(kCount)); 
}

bool local_valid(struct write_set * ws, struct tvar * tv, Value_t val){
    while(ws != M_NIL){
        if(ws->tvar == tv){
            return ws->val == val;
        }
        ws = ws->next;
    }
    return false;
}

//verify local reads
Value_t ffValidate2(struct read_set * readSet, struct read_log * oldRS, unsigned long * myStamp, volatile unsigned long * clock, VProc_t * vp){
    unsigned long kCount = readSet->numK;
    struct read_log * checkpoint = oldRS;

    int i = 0, j = 0;
    while((Value_t)oldRS != M_NIL){
        if(oldRS->tag == (Value_t)5){  //WithoutK
            if(oldRS->tvar->contents != oldRS->readContents){
                return ffFinish(readSet, checkpoint, kCount, vp, i);
            }
        }else if (oldRS->tag == (Value_t) 3){  //WithK
            if(oldRS->tvar->contents == oldRS->readContents){
                if(oldRS->k != M_UNIT){
                    checkpoint = oldRS;
                    i += j;
                    j = 0;
                    kCount++;
                }
            }else{      //oldRS->tag == (Value_t) 3
                if(oldRS->k != M_UNIT){
                    //we have a checkpoint here, but its out of date
                    Value_t v = oldRS->tvar->contents;
                    if(*clock != *myStamp){
                        do{
                            Value_t abortInfo = validate(myStamp, clock, readSet->head, vp);
                            if(abortInfo != M_UNIT){
                                return abortInfo;
                            }
                            v = oldRS->tvar->contents;
                        }while(*clock != *myStamp);
                    }
                    oldRS->readContents = v;
                    oldRS->next = M_NIL;
                    return AllocNonUniform(vp, 4, PTR(readSet->head), PTR(oldRS), PTR(oldRS), INT(kCount));

                }else{
                    return ffFinish(readSet, checkpoint, kCount, vp, i);
                }
            }
        }else{ //local read
            struct write_set * ws = oldRS->writeSet;
            if(!local_valid(ws, oldRS->tvar, oldRS->readContents)){
                return ffFinish(readSet, checkpoint, kCount, vp, i);
            }
        }
        j++;
        oldRS = oldRS->next;
    }
    return ffFinish(readSet, checkpoint, kCount, vp, i);
}

typedef enum {EQ, SUBSEQ, NEQ} ws_res;

ws_res classify_writesets(struct write_set * oldWS, struct write_set * currentWS){
    ws_res res = EQ;
    while(currentWS != M_NIL){
        if(oldWS == M_NIL)
            return NEQ; //current is not a subsequence of the old 

        if(oldWS->tvar != currentWS->tvar){  //we can still try and match a subsequence
            res = SUBSEQ;
            oldWS = oldWS->next;
            continue;
        }

        if(oldWS->val != currentWS->val)
            res = SUBSEQ;

        currentWS = currentWS->next;
        oldWS = oldWS->next;
    }
    if(oldWS == M_NIL){
        return res;
    }else{
        return SUBSEQ;
    }
}

Value_t fastForward(struct read_set * readSet, struct read_set * ffInfo, struct write_set * writeSet, Word_t* tv, Word_t* retK, Word_t* myStamp, volatile unsigned long * clock, VProc_t * vp, struct tvar * dummy){
    struct read_log * shortPath = ffInfo->lastK;
    bool polyUnEq = false;
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

                    Value_t result = ffValidate(readSet, shortPath, myStamp, clock, vp);
                    return result;
                }else{
                    
                    struct write_set * oldWS = shortPath->writeSet;
                    struct write_set * currentWS = writeSet;

                    ws_res res = classify_writesets(oldWS, currentWS);

                    if(res == NEQ){
                        return M_UNIT;
                    }

                    //vp->counter[2]++;

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
                    }
                }
            }
            polyUnEq = true;
        }
        shortPath = shortPath->nextK;
    }
    return M_UNIT;
}

Value_t abortFlag = M_TRUE;

Value_t M_ToggleAbort(){
    if(abortFlag == M_TRUE){
        abortFlag = M_FALSE;
        return M_TRUE;
    }
    abortFlag = M_TRUE;
    return M_FALSE;
}



























