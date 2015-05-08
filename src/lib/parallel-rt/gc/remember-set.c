#include "vproc.h"
#include "value.h"
#include <stdio.h>
#include <string.h>
#include "remember-set.h"

int M_NumRSRoots(VProc_t * vp){
	int count = 0;
	RS_t * rememberSet = ValueToPtr(vp->rememberSet);
    while (rememberSet != (RS_t *)M_NIL) {
    	rememberSet = (RS_t *) rememberSet->next;
    	count++;
    }
    return count;
}




