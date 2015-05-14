#include "vproc.h"
#include "value.h"
#include <stdio.h>
#include <string.h>
#include "remember-set.h"
#include "internal-heap.h"
#include "bibop.h"

int M_NumRSRoots(VProc_t * vp){
	int count = 0;
	RS_t * rememberSet = ValueToPtr(vp->rememberSet);
    while (rememberSet != (RS_t *)M_NIL) {
    	rememberSet = (RS_t *) rememberSet->next;
    	count++;
    }
    return count;
}


int isInGlobalHeap2(Value_t v){
    return AddrToChunk(ValueToAddr(v))->sts == TO_SP_CHUNK;
}

Value_t ** M_AddRSElts(VProc_t * vp, Value_t ** rootPtr){
    RS_t * rememberSet = (RS_t*)vp->rememberSet;
    while (rememberSet != (RS_t *)M_NIL) {
    	if(rememberSet->offset >= 0){
    		if(isInGlobalHeap2(rememberSet->source[rememberSet->offset])){
    			printf("source is in global heap\n");
    		}
	    	*rootPtr++ = rememberSet->source + rememberSet->offset;
		}
		rememberSet = (RS_t *) rememberSet->next;
    }
    return rootPtr;
}



