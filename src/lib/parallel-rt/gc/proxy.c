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
	if (vp->proxyTableentries == 512) return 1;
	else return 0;
}

void createList (VProc_t *vp) {
	//printf("Reset Proxy List\n");
	
	//set List of empty entries
	for (int i = 0; i < vp->maxProxy-1;i++) {
		vp->proxyTable[i].proxyObj=(Value_t)(long long int)(i+1);
	}
	vp->proxyTable[vp->maxProxy-1].proxyObj = (Value_t)(1000);
	vp->proxyTableentries = 0;
}

Value_t createProxy (VProc_t *vp, Value_t fls) {
    
    int next = vp->proxyTableentries;
	
	vp->proxyTableentries++;
	
	//printf("Create Proxy pointer = %d\n",vp->proxyTableentries);
    
    vp->proxyTable[next].proxyObj = AllocProxy(vp,2,vp,next);
    vp->proxyTable[next].localObj = fls;

    return (vp->proxyTable[next].proxyObj);

}

void isPrintProxy (int id) {
	
	printf("Integer = %d\n",id);
}

void globalCheck (VProc_t *vp) {
	
	printf("Proxy Memory Check");
	
	if ((vp->globNextW + WORD_SZB * 3) >= vp->globLimit) {
		AllocToSpaceChunk(vp);
		printf("AllocProxy need more space\n");
	}
}

void isProxy (VProc_t *vp, int id) {
	
	
	 printf("Proxy check, vproc %d id %d\n",vp->id,id);
    
    if ((id > -1) && (id < 512)){
	    printf("Hallo 1 \n");
	Word_t *p = (Word_t *)(vp->proxyTable[id].proxyObj);
	    printf("Hallo 2 %llu, pos = %p\n",(long long int)vp->proxyTable[id].proxyObj,(void*)vp->proxyTable[id].proxyObj);
	    
	Word_t hdr = p[-1];
	    printf("Hallo 3 \n");
    
	if (getID(hdr) == 2) 
		printf("Found Proxy, vproc %d id %d, length = %d\n",vp->id,id,GetLength(hdr));
	        printf("Position in Memory Proxy, vproc %p\n",(void *) vp);
    } else { 
	    printf("ERROR NO RPOXY\n");
    }
}


void deleteProxy (VProc_t *vp, int id) {
	
	int last = vp->proxyTableentries -1;
	
	if (id != last) {
		vp->proxyTable[id].proxyObj = vp->proxyTable[last].proxyObj;
		vp->proxyTable[id].localObj = vp->proxyTable[last].localObj;
		
		vp->proxyTable[last].proxyObj = (Value_t)(0);
		vp->proxyTable[last].localObj = (Value_t)(0);
		
		//if (isPtr(vp->proxyTable[last].localObj)) printf("id = %d is ptr\n",last);
		//if (isForwardPtr(vp->proxyTable[last].localObj)) printf("id = %d is fwd ptr\n",last);
		
		Word_t * scanP = (Word_t *)(vp->proxyTable[id].proxyObj);
		
		*(scanP+1) = (Word_t)(id);
		
		Value_t v = vp->proxyTable[id].localObj;
		Word_t	*p = ((Word_t *)ValueToPtr(v));
		Word_t	hdr = p[-1];
		if (hdr == 0) printf("MinorNonProm Null pointer in vp %d, id = %d\n",vp->id,id);
		if (getID(hdr) != 1) printf("MinorNonProm HeaderID Error hdrid = %d in vp %d, id = %d\n",getID(hdr),vp->id,id);
		if (GetLength(hdr) != 2) printf("MinorNonProm LengthHDR Error length = %d in vp %d, id = %d\n",GetLength(hdr),vp->id,id);
		
		//printf("delete id = %d , last = %d, scanp = %llu\n",id,last,(long long int)(vp->proxyTable[id].localObj));
	}
	//printf("Max = %d, last = %d\n",vp->proxyTableentries,last);
	vp->proxyTableentries = last;
}

void promoteCont (VProc_t *vp, int id) {
	
	vp->proxyTable[id].localObj = PromoteObj(vp,vp->proxyTable[id].localObj);
	
	Word_t * scanP = (Word_t *)(vp->proxyTable[id].proxyObj);
	
	*(scanP+1) = (Word_t)vp->proxyTable[id].localObj;
	
	deleteProxy(vp,id);
}


Value_t returnCont (VProc_t *vp,int id) {
	
	//printf("return cont Vproc = %d, id = %d\n",vp->id,id);

	return (vp->proxyTable[id].localObj);
}

