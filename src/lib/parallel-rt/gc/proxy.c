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
		vp->proxyTable[i].proxyObj=(Value_t)(i+1);
		//printf("%d = %llu\n",i,(long long int) (vp->proxyTable[i].proxyObj));
	}
	vp->proxyTable[vp->maxProxy-1].proxyObj = (Value_t)(1000);
	vp->proxyTableentries = 0;
	//printf("last = %d, pointer = %d\n",(int) (vp->proxyTable[vp->maxProxy-1].proxyObj),vp->proxyTableentries);
}

Value_t createProxy (VProc_t *vp, Value_t fls) {
    
    //printf("Create proxy\n");
    
    int next = vp->proxyTableentries;
    vp->proxyTableentries = (long long int)(vp->proxyTable[next].proxyObj);
    
    vp->proxyTable[next].proxyObj = AllocProxy(vp,2,vp,next);
    vp->proxyTable[next].localObj = fls;

	Word_t * test = ValueToPtr(vp->proxyTable[next].proxyObj);
    /*
	printf("Proxy created, vpid = %d, id = %d, ptr = %p, localobj = %p, valueoflocal = %llu\n",
	    vp->id, next, (void *)vp->proxyTable[next].proxyObj,(void *)vp->proxyTable[next].localObj,(long long int)vp->proxyTable[next].localObj);
     */
    return (vp->proxyTable[next].proxyObj);

}


void isProxy (VProc_t *vp, int id) {
    
    //printf("Check proxy %d\n",id);
    
    if ((id > -1) && (id < 512)){
	Word_t *p = ValueToPtr(vp->proxyTable[id].proxyObj);
	Word_t hdr = p[-1];
    
	if (getID(hdr) == 2) 
		printf("Found Proxy\n");
    }
}

void isCont (Value_t fls) {
	
	printf("Check cont\n");
	
	Word_t * test = (Word_t *) fls;
	
	int id = getID(test[-1]);
	
	printf("ID = %d, hdr = %d\n",id,GetLength(test[-1]));
}


void deleteProxy (VProc_t *vp, int id) {
    
   // printf("Delete proxy id = %d\n",id);
	//list empty?
	if (vp->proxyTableentries != 1000) {
		vp->proxyTable[id].proxyObj = (Value_t) (vp->proxyTableentries);
	} else {
		vp->proxyTable[id].proxyObj = (Value_t) (1000);
	}
	vp->proxyTableentries = id;
	
}


Value_t returnCont (VProc_t *vp,int id) {
    /*
    Value_t entr = vp->proxyTable[id].localObj;
	printf("Pos cont %d = %p, value = %llu\n",id,(void *)entr,(long long int)entr);
	return entr;
     */
	return (vp->proxyTable[id].localObj);
}

