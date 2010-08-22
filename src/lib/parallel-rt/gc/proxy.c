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


int findEmptyEntry (VProc_t *vp) {
    
    if (vp->proxyTableentries == vp->maxProxy) return -1;
    
    for (int i = 0;i < vp->maxProxy; i++) {
	if (vp->proxyTable[i].proxyObj == NULL) return i;
    }
    
    return -2;
}

int createProxy (VProc_t *vp, Value_t fls) {
    
    printf("Create proxy\n");
    
    int id = findEmptyEntry(vp);
    
    if (id == -1)
	printf("Error finding empty entry\n");
    else {
	vp->proxyTable[id].proxyObj = AllocProxy(vp,1,INT(vp->id));
	vp->proxyTable[id].localObj = fls;
    //vp->proxyTable[id] = AllocNonUniform(vp,1,INT(vp->id));
	vp->proxyTableentries++;
    
	printf("Proxy created, vpid = %d, id = %d, ptr = %p\n",
	    vp->id, id, (void *)vp->proxyTable[id].proxyObj);
    }

    return (id);

}


void isProxy (VProc_t *vp, int id) {
    
    printf("Check proxy %d\n",id);
    
    if (id > -1) {
	Word_t *p = ValueToPtr(vp->proxyTable[id].proxyObj);
	Word_t hdr = p[-1];
    
	if (getID(hdr) == 3) 
	    printf("Found Proxy\n");
    }
}

void deleteProxy (VProc_t *vp, int id) {
    
    printf("Delete proxy\n");
    
    vp->proxyTable[id].proxyObj=NULL;
    vp->proxyTableentries--;
    
    printf("Entry %d = %p\n",id,vp->proxyTable[id].proxyObj);
    
}


int promotedProxy (VProc_t *vp,int id) {
    
    Word_t *p = ValueToPtr(vp->proxyTable[id].proxyObj);
    Word_t hdr = p[-1];
    
    if (p[1] == 0) {
	printf("Not promoted\n");
	return 0;
    } else {
	printf("Promoted\n");
	return 1;
    }
    /*
    if ( (hdr >> 63) == 0 ) {
	printf("Not promoted\n");
	return 0;
    } else {
	printf("Promoted\n");
	return 1;
    }
     */
}


void setPromote (VProc_t *vp, int id) {
    
    Word_t *p = ValueToPtr(vp->proxyTable[id].proxyObj);
    printf("Before Entry %d = %llu\n",id,p[1]);
    
    //p[-1] ^= (0x8000000000000000);
    p[1]=1;
    
    printf("After Entry %d = %llu\n",id,p[1]);
}