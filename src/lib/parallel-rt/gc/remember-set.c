#include "vproc.h"
#include "value.h"
#include <stdio.h>
#include <string.h>
#include "remember-set.h"
#include "internal-heap.h"
#include "bibop.h"
#include "gc-inline.h"

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

Value_t ** M_AddRSElts(VProc_t * vp, Value_t ** rootPtr){
    RS_t * rememberSet = (RS_t*)vp->rememberSet;
    Addr_t heapBase = vp->heapBase;
    Addr_t oldSzB = vp->oldTop - heapBase;
    Addr_t nurseryBase = vp->nurseryBase;
    Addr_t nurserySize = vp->allocPtr - nurseryBase;

    Value_t * trailer = &(vp->rememberSet);

    while (rememberSet != (RS_t *)M_NIL) {
        Value_t * source = rememberSet->source;
        Value_t dest = source[rememberSet->offset];
        if(inGlobal((Value_t)source) && inGlobal(dest)){
            *trailer = rememberSet->next;
            rememberSet = rememberSet->next;
            continue;
        }
        if(inAddrRange(heapBase, oldSzB, source) && inAddrRange(heapBase, oldSzB, dest)){
            *trailer = rememberSet->next;
            rememberSet = rememberSet->next;
            continue;
        }

        if(inAddrRange(vp->nurseryBase, nurserySize, source) && inAddrRange(nurseryBase, nurserySize, dest)){
            *trailer = rememberSet->next;
            rememberSet = rememberSet->next;
            continue;
        }

    	*rootPtr++ = rememberSet->source + rememberSet->offset;
        trailer = &(rememberSet->next);
		rememberSet = (RS_t *) rememberSet->next;
    }
    return rootPtr;
}



