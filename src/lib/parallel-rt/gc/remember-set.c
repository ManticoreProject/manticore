#include "vproc.h"
#include "value.h"
#include <stdio.h>
#include <string.h>
#include "remember-set.h"
#include "internal-heap.h"
#include "bibop.h"
#include "gc-inline.h"
#include "heap.h"
#include "eventlog.h"

/*
 * this serves as a place to set a breakpoint
 * once the breakoint is hit, execute the "up" command to 
 * move up the stack one level.  This saves you from having 
 * to set multiple breakpoints, instead just have all error/interesting
 * cases call this function
 */
void somethingBadHappened(){

}

int M_NumRSRoots(VProc_t * vp){
	int count = 0;
	RS_t * rememberSet = ValueToPtr(vp->rememberSet);
    while (rememberSet != (RS_t *)M_NIL) {
    	rememberSet = (RS_t *) rememberSet->next;
    	count++;
    }
    return count;
}

int inGlobal(Value_t v){
    return AddrToChunk(ValueToAddr(v))->sts == TO_SP_CHUNK;
}

volatile unsigned long dumpLock = 0;

int toGenNum(Value_t * x, Addr_t heapBase, Addr_t oldSzB, Addr_t nurseryBase, Addr_t nurserySize){
    if(inGlobal((Value_t)x)){
        return 2;
    }
    if(inAddrRange(heapBase, oldSzB, (Addr_t)x)){
        return 1;
    }else if(inAddrRange(nurseryBase, nurserySize, (Addr_t) x)){
        return 0;
    }else{
        if(isPtr(x)){
            somethingBadHappened();
	    while(CompareAndSwapValue(&dumpLock, 0, 1) != 0);

	    endEventLogging();
	    
            printf("Pointer is in unrecognized region\n");
	    return -1;
        }
        return 3;
    }
}

void checkInvariant(Word_t * source, Word_t * dest, VProc_t * vp, char * context){
    Addr_t heapBase = vp->heapBase;
    Addr_t oldSzB = vp->oldTop - heapBase;
    Addr_t nurseryBase = vp->nurseryBase;
    Addr_t nurserySize= vp->allocPtr - nurseryBase;
    if(!isPtr(dest)){
        return;
    }
    int sourceGen = toGenNum(source, heapBase, oldSzB, nurseryBase, nurserySize);
    int destGen = toGenNum(dest, heapBase, oldSzB, nurseryBase, nurserySize);
    if(sourceGen > destGen){

	RS_t * rs = (RS_t*)vp->rememberSet;
	while(rs != (RS_t *)M_NIL){
	    if(rs->source == source){
		return;
	    }
	    rs = rs->next;
	}
	printf("Updating source (%p) in generation %d to point to %p in geneartion %d from %s\n", source, sourceGen, dest, destGen, context);
    }

    //printf("source generation = %d, destination generation = %d\n", sourceGen, destGen);
}

Value_t ** M_AddRSElts(VProc_t * vp, Value_t ** rootPtr){
    RS_t * rememberSet = (RS_t*)vp->rememberSet;
    Addr_t heapBase = vp->heapBase;
    Addr_t oldSzB = vp->oldTop - heapBase;
    Addr_t nurseryBase = vp->nurseryBase;
    Addr_t nurserySize= vp->allocPtr - nurseryBase;
    RS_t ** trailer = &(vp->rememberSet);
    int id = 0;

    // printf("%d: processing remember set\n", vp->id);
    
    while (rememberSet != (RS_t *)M_NIL) {
        Value_t * source = rememberSet->source;

        RS_t ** prev = &(rememberSet->next);
        RS_t * ptr = rememberSet->next;
        while((Value_t)ptr != M_NIL){
            if(ptr->source == source && ptr->offset == rememberSet->offset){
		//	printf("%d: dropping %p from remember set (duplicate)\n", vp->id, ptr->source);
                *prev = ptr->next;
                ptr = ptr->next;
            }else{
                prev = &(ptr->next);
                ptr = ptr->next;
            }
        }

        Value_t dest = source[rememberSet->offset];

        int sourceGen = toGenNum(source, heapBase, oldSzB, nurseryBase, nurserySize);
        int destGen = toGenNum(dest, heapBase, oldSzB, nurseryBase, nurserySize);
        if(sourceGen > destGen){  //pointer from old gen to new gen
            id++;
            *rootPtr++ = rememberSet->source + rememberSet->offset;
            trailer = &(rememberSet->next);
            rememberSet = rememberSet->next;
        }else{
	    //printf("%d: Dropping %p from remember set (benign)\n", vp->id, rememberSet->source);
            *trailer = rememberSet->next;
            rememberSet = rememberSet->next;
        }
    }

    numRememberSetElements = id;
    return rootPtr;
}

//datatype 'a item = Write of 'a * 'a * 'a | NilItem 
typedef struct write_set{
    struct tvar * tvar;
    Word_t * newVal;
    struct write_set * next;
}WS_t;

void examineWriteSets(WS_t * currentWS, WS_t * oldWS){

}

void M_PruneRemSetAll(VProc_t * vp, long threadID, const char * context){
    RS_t * remSet = (RS_t *)vp->rememberSet;
    RS_t ** trailer = &(vp->rememberSet);
    //printf("%d starting to prune remember set at time %f (%s)\n", vp->id, (double)time_ns() / (double)1e9, context);
    while((Addr_t) remSet != M_NIL){
        if(remSet->threadId == threadID){
	    // printf("%d: dropping %p from remember set (M_PruneRemSetAll)\n", vp->id, remSet->source);
            *trailer = remSet->next;
            remSet = remSet->next;
        }else{
            trailer = &(remSet->next);
            remSet = remSet->next;
        }
    }
}

void M_PrintAllocPtr(VProc_t * vp, const char * context){
    printf("VProc[%d]: Allocation pointer is now at %p (%s)\n", vp->id, vp->allocPtr, context);
}

/*Functions to help debug*/

struct fls{
    int vpID;
    Word_t * ite;
    int dictCounter;
    ListCons_t * dictionary;
    Word_t * computationallyIntensive;
    int counter1;
    int counter2;
};

struct dict_element{
    int * key;
    Word_t * val;
};

struct read_log{
    Value_t tag;
    struct tvar * tvar;
    Word_t * readContents;
    struct read_log * next;
    Word_t * writeSet;
    Word_t * k;
    struct read_log * nextK;
};

struct read_set{
    struct read_log * head;
    struct read_log * tail;
    struct read_log * lastK;
    unsigned long numK;
};

volatile int shortPathLock = 0;

void printRemSet(VProc_t * vp){
    RS_t * remSet = (RS_t*)vp->rememberSet;
    int i = 0;
    while((Value_t) remSet != M_NIL){
        int g1 = toGenNum(remSet->source, vp->heapBase, vp->oldTop - vp->heapBase, vp->nurseryBase, vp->allocPtr - vp->nurseryBase);
        int g2 = toGenNum(remSet->source[remSet->offset], vp->heapBase, vp->oldTop - vp->heapBase, vp->nurseryBase, vp->allocPtr - vp->nurseryBase);
        int g3 = toGenNum(remSet, vp->heapBase, vp->oldTop - vp->heapBase, vp->nurseryBase, vp->allocPtr - vp->nurseryBase);
        printf("%d: source = %p, dest = %p, offset = %d, source gen = %d, dest gen = %d, remSet cell is in gen %d\n", i, remSet->source, remSet->source[remSet->offset], remSet->offset, g1, g2, g3);
        i++;
        remSet = remSet->next;
    }
    printf("\n");
}

RS_t * getRemElement(RS_t * remSet, int i){
    while((Value_t) remSet != M_NIL){
        if(i == 0)
            return remSet;
        i--;
        remSet = remSet->next;
    }
    printf("Remember set item not found!\n");
    return remSet;
}

struct read_log * getElement(struct read_log * ptr, int i){
    while((Value_t) ptr != M_NIL){
        if(i == 0){
            return ptr;
        }
        i--;
        ptr = ptr->next;
    }
    printf("element not found!\n");
    return ptr;
}

struct read_log * getSPElement(struct read_log * ptr, int i){
    while((Value_t) ptr != M_NIL){
        if(i == 0){
            return ptr;
        }
        i--;
        ptr = ptr->nextK;
    }
    printf("element not found!\n");
    return ptr;
}

void printShortPath(struct read_log * ptr, VProc_t * vp){
    while(CompareAndSwapValue(&shortPathLock, 0, 1) != 0);
    int i = 0;
    while((Value_t) ptr != M_NIL){
        int g = toGenNum(ptr, vp->heapBase, vp->oldTop - vp->heapBase, vp->nurseryBase, vp->allocPtr - vp->nurseryBase);
        printf("%d: %p, in generation %d\n", i, ptr, g);
        ptr = ptr->nextK;
        i++;
    }
    shortPathLock = 0;
}

void checkDuplicates(VProc_t * vp){
    RS_t * remSet = (RS_t *)vp->rememberSet;
    while((Value_t)remSet != M_NIL){
        RS_t * ptr = remSet->next;
        while((Value_t)ptr != M_NIL){
            if(remSet->source == ptr->source && remSet->offset == ptr->offset){
                printf("%lu: Found duplicate!\n", vp->id);
		//somethingBadHappened();
            }
            ptr = ptr->next;
        }
        remSet = remSet->next;
    }
}

void checkRS(VProc_t * vp, struct read_set * rs, const char * context){
    RS_t * remSet = (RS_t*)vp->rememberSet;

    //checkDuplicates(vp);

    if(isForwardPtr(((Word_t*)rs)[-1])){
        somethingBadHappened();
        printf("Read set is a forwarding pointer\n");
    }

    int i = 0;
    struct read_log * trailer = NULL;
    struct read_log * ptr = rs->head;
    while((Value_t) ptr != M_NIL){
	int g1 = toGenNum(ptr, vp->heapBase, vp->oldTop - vp->heapBase, vp->nurseryBase, vp->allocPtr - vp->nurseryBase);
	int g2 = toGenNum(ptr->next, vp->heapBase, vp->oldTop - vp->heapBase, vp->nurseryBase, vp->allocPtr - vp->nurseryBase);

	if(g1 == -1 || g2 == -1){
	    printf("%lu: heap object in unknown region\n");
	    somethingBadHappened();
	}
	
        Word_t header = ((Word_t*)ptr)[-1];
        if(header != 262147 && header != 458755){
            somethingBadHappened();
            printf("Found incorrect header in read set! (%s)\n", context);
        }
        trailer = ptr;
        ptr = ptr->next;
        i++;
    }

    i = 0;
    trailer = NULL;
    ptr = rs->lastK;
    while((Value_t) ptr != M_NIL){
        Word_t header = ((Word_t*)ptr)[-1];
        if(isForwardPtr(header)){
            somethingBadHappened();
            printf("Found forwarding pointer in read set! (%s)\n", context);
        }
        if(ptr->tag != 3){
            somethingBadHappened();
            printf("Expected WithK tag, instead found %lu (%s)\n", ptr->tag, context);
        }
        if(isPtr(ptr->nextK)){
            //(Value_t * x, Addr_t heapBase, Addr_t oldSzB, Addr_t nurseryBase, Addr_t nurserySize);
            int g1 = toGenNum(ptr, vp->heapBase, vp->oldTop - vp->heapBase, vp->nurseryBase, vp->allocPtr - vp->nurseryBase);
            int g2 = toGenNum(ptr->nextK, vp->heapBase, vp->oldTop - vp->heapBase, vp->nurseryBase, vp->allocPtr - vp->nurseryBase);
	    if(g1 == -1 || g2 == -1){
		somethingBadHappened();
		printf("%lu: heap object in unknown region\n");
	    }
            if(g1 > g2){ //source is in older generation than destination
                //verify g1 is in the remember set
                RS_t * remSetPtr = remSet;
                while((Value_t) remSetPtr != M_NIL){
                    if(remSetPtr->source == ptr){
                        break;
                    }
                    remSetPtr = remSetPtr->next;
                }
                if(remSetPtr == M_NIL){
                    somethingBadHappened();
                    printf("nextK is in older generation than current node (%s)\n", context);
                }
            }
        }
        i++;
        trailer = ptr;
        ptr = ptr->nextK;
    }
}

struct read_set * getRS(int key, struct fls * fls){
    ListCons_t * consCell;
    Value_t dict = fls->dictionary;
    struct read_set * rs = M_NIL;
    while(dict != M_NIL){
        consCell = (ListCons_t *) dict;
        if(((struct dict_element *)consCell->hd)->key[0] == key){
            rs = ((struct dict_element *)consCell->hd)->val;
            break;
        }
        dict = consCell->tl;
    }
    return rs;
}

void checkReadSet(VProc_t * vp, char * context){
    struct fls * fls = (struct fls *) vp->currentFLS;
    int READ_SET_KEY = 3;
    int FF_INFO_KEY = 7;
    struct read_set * rs = getRS(READ_SET_KEY, fls);
    if(!isPtr(rs)){
        return;
    }
    checkRS(vp, rs, context);
}

struct ws{
    struct tvar * tvar;
    Value_t expected;
    struct ws * next;
};

volatile unsigned long checkWSlock = 0;
  

void checkWS(VProc_t * vp, struct ws * merged, struct ws * currentWS){
    while(CompareAndSwapValue(&checkWSlock, 0, 1) != 0);
    struct ws * ptr = merged;
    while((Value_t)ptr != M_NIL){
        int g = toGenNum(ptr, vp->heapBase, vp->oldTop - vp->heapBase, vp->nurseryBase, vp->allocPtr - vp->nurseryBase);
        printf("VProc[%d]: genNum = %d\n", vp->id, g);
        ptr = ptr->next;
    }
    printf("\n\n");
    checkWSlock = 0;
}



/*expr -- struct read_log * $prev = getSPElement(rs->lastK, i-1)*/
