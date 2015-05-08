#include "vproc.h"
#include "value.h"
#include <stdio.h>
#include <string.h>


int M_NumRSRoots(VProc_t * vp){
	int count = 0;
	ListCons_t * rememberSet = ValueToPtr(vp->rememberSet);
    while (rememberSet != (ListCons_t *)M_NIL) {
    	rememberSet = rememberSet->tl;
    	count++;
    }
    return count;
}


