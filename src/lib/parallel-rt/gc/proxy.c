/*! \file proxy.c — Search Results (proxy)
 *
 * \author Sven Auhagen
 */

/*
 * COPYRIGHT (c) 2010 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 */

#include "manticore-rt.h"
#include "value.h"
#include "vproc.h"

#include "gc-inline.h"
#include "gc.h"
#include "gc-scan.h"

#include <stdio.h>

int isFree (VProc_t *vp) {
	//if no entries left return 1 else 0
	if (vp->proxyTableentries == 1000) return 1;
	else return 0;
}

void createList (VProc_t *vp) {
	//set List of empty entries
	for (int i = 0; i < vp->maxProxy-1;i++) {
		vp->proxyTable[i].proxyObj=(Value_t)(long long int)(i+1);
	}
	vp->proxyTable[vp->maxProxy-1].proxyObj = (Value_t)(1000);
	vp->proxyTableentries = 0;
}

Value_t createProxy (VProc_t *vp, Value_t fls) {
    
    int next = vp->proxyTableentries;
	
    vp->proxyTableentries = (long long int)(vp->proxyTable[next].proxyObj);
    
    vp->proxyTable[next].proxyObj = AllocProxy(vp,2,vp,next);
    vp->proxyTable[next].localObj = fls;

    return (vp->proxyTable[next].proxyObj);

}


void isProxy (VProc_t *vp, int id) {
    
    if ((id > -1) && (id < 512)){
	Word_t *p = ValueToPtr(vp->proxyTable[id].proxyObj);
	Word_t hdr = p[-1];
    
	if (getID(hdr) == 2) 
		printf("Found Proxy\n");
    }
}

void promoteCont (VProc_t *vp, int id) {
	
	vp->proxyTable[id].localObj = PromoteObj(vp,vp->proxyTable[id].localObj);
	
	Word_t * scanP = (Word_t *)(vp->proxyTable[id].proxyObj);
	
	*(scanP+1) = (Word_t)vp->proxyTable[id].localObj;
	
	deleteProxy(vp,id);
}


void deleteProxy (VProc_t *vp, int id) {

	//list empty?
	if (vp->proxyTableentries != 1000) {
		vp->proxyTable[id].proxyObj = (Value_t)(long long int)(vp->proxyTableentries);
	} else {
		vp->proxyTable[id].proxyObj = (Value_t) (1000);
	}
	vp->proxyTableentries = id;
}


Value_t returnCont (VProc_t *vp,int id) {

	return (vp->proxyTable[id].localObj);
}

