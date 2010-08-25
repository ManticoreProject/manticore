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
	if (vp->proxyTableentries == -1) return 1;
	else return 0;
}

int findEmptyEntry (VProc_t *vp) {
    
	if (vp->proxyTableentries == -1) deleteProxy(vp,511);
	//if (vp->proxyTableentries == -1) return -1;
	
	Value_t next = (Value_t)(vp->proxyTableentries);
	Value_t before = next;
	
	while (vp->proxyTable[(int)next].proxyObj != 0) {
		before = next;
		next = vp->proxyTable[(int)before].proxyObj;
	}
	
	//last free element popped out
	if (((int)before == vp->proxyTableentries) && ((int)next == vp->proxyTableentries)) {
		//printf("End of Table before = %d ,return = %d\n",(int)before,(int)next);
		vp->proxyTableentries = -1;
		return((int)next);
	}
	//set element before to 0
	vp->proxyTable[(int)before].proxyObj = (Value_t)(0);
	//printf("before = %d ,return = %d\n",(int)before,(int)next);
	return ((int)next);
	
}

void createList (VProc_t *vp) {
	//set List of empty entries
	for (int i = 0; i < vp->maxProxy-1;i++) {
		vp->proxyTable[i].proxyObj=(Value_t)(i+1);
		//printf("%d = %llu\n",i,(long long int) (vp->proxyTable[i].proxyObj));
	}
	vp->proxyTable[vp->maxProxy].proxyObj = (Value_t)(0);
	//printf("last = %llu\n",(long long int) (vp->proxyTable[vp->maxProxy].proxyObj));
}

Value_t createProxy (VProc_t *vp, Value_t fls) {
    
    //printf("Create proxy\n");
    
    int getid = findEmptyEntry(vp);
    
    if (getid == -1) {
	printf("Error finding empty entry\n");
	   // return(0);
    }
    else {
	vp->proxyTable[getid].proxyObj = AllocProxy(vp,2,vp->id,getid);
	vp->proxyTable[getid].localObj = fls;
    //vp->proxyTable[id] = AllocNonUniform(vp,1,INT(vp->id));
	//vp->proxyTableentries++;
	    
	   Word_t * test = ValueToPtr(vp->proxyTable[getid].proxyObj);
    
	printf("Proxy created, vpid = %d, id = %d, ptr = %p\n",
	    vp->id, getid, (void *)vp->proxyTable[getid].proxyObj);
    }

    return (vp->proxyTable[getid].proxyObj);

}


void isProxy (VProc_t *vp, int id) {
    
    //printf("Check proxy %d\n",id);
    
    if ((id > -1) && (id != NULL)){
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
    
    printf("Delete proxy\n");
    //No empty entries
	if (vp->proxyTableentries == -1) {
		vp->proxyTableentries=id;
		vp->proxyTable[id].proxyObj=(Value_t)(0);
	} else {
		Value_t next = (Value_t) (vp->proxyTableentries);
		while (vp->proxyTable[(int)next].proxyObj != (Value_t)(0)) {
			next = vp->proxyTable[(int)next].proxyObj;
		}
		//last next is end of list
		vp->proxyTable[(int)next].proxyObj = (Value_t)(id);
		vp->proxyTable[id].proxyObj=(Value_t)(0);
		printf("Entry %d = %d\n",id,(int)(vp->proxyTable[(int)next].proxyObj));
	}
}


Value_t returnCont (VProc_t *vp,int id) {
    
    return vp->proxyTable[id].localObj;
}

