/*! \file proxy.c — Search Results (proxy)
 *
 * \author Sven Auhagen
 */

/*
 * COPYRIGHT (c) 2014 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 */

#include "manticore-rt.h"
#include "value.h"
#include "vproc.h"

#include "gc-inline.h"
#include "gc.h"
#include "gc-scan.h"

#include <stdio.h>
#include <string.h>

inline void deleteProxy (VProc_t *vp, const int id) {
    assert(vp->proxyTableentries > 1);

    const int last = vp->proxyTableentries - 1;

    assert(id <= last);

    // update proxy object in the global heap
    Word_t *proxyObj = (Word_t *)(vp->proxyTable[last].proxyObj);
    proxyObj[1] = (Word_t)id;

    // copy the last value in the table in place of the deleted proxy.
    vp->proxyTable[id].proxyObj = vp->proxyTable[last].proxyObj;
    vp->proxyTable[id].localObj = vp->proxyTable[last].localObj;
    vp->proxyTableentries = last;   
}

/**
 * Performs a promotion of the proxy object in the local heap
 * into the global heap, and then deletes the proxy table entry.
 */
void promoteProxy (VProc_t *vp, int id) {
    assert(id >= 0 && id <= vp->maxProxy);

    vp->proxyTable[id].localObj = PromoteObj(vp,vp->proxyTable[id].localObj);
    
    Word_t *proxyObj = (Word_t *)(vp->proxyTable[id].proxyObj);
    
    proxyObj[1] = (Word_t)vp->proxyTable[id].localObj;
    
    deleteProxy(vp, id);
}


/////////
// below are ancient artifacts and debugging functions :)
////////

#ifndef NDEBUG

/**
 * NOTE: This function is currently unused because it's
 *       got a BOM implementation, but we're keeping
 *       it around for now.
 */
Value_t returnCont (Value_t proxyObj) {
    
    Word_t  *p2 = ((Word_t *)ValueToPtr(proxyObj));
    Value_t v;
    VProc_t * vp = (VProc_t *)p2[0];

#ifndef NDEBUG
    printf("return cont Vproc = %d, id = %lu, proxy is at %p\n",vp->id,(unsigned long)p2[1],(void*)proxyObj);
#endif

    if (((unsigned long)p2[1]) < vp->maxProxy) { 
        v = (Value_t)ValueToPtr(vp->proxyTable[p2[1]].localObj);
        deleteProxy(vp,(int)p2[1]);

#ifndef NDEBUG
        printf("return cont Vproc = %d, id = %lu, cont is at %p\n",vp->id,(unsigned long)p2[1],(void*)v);
#endif

    } else { 
        v = (Value_t)ValueToPtr((Value_t)p2[1]);

#ifndef NDEBUG
        printf("return global cont proxy = %p, returncont %p\n",(void*)(p2),(void*)v);
#endif        

    }
        
    return v;
}

/**
 * NOTE: This function is currently unused because it's
 *       got a BOM implementation, but we're keeping
 *       it around for now.
 */
Value_t createProxy (VProc_t *vp, Word_t * fls) {
    
    int next = vp->proxyTableentries;
        
    Value_t p = AllocProxy(vp,2,vp,next);
    
    vp->proxyTable[next].proxyObj = p;
    vp->proxyTable[next].localObj = PtrToValue(fls);
    
#ifndef NDEBUG
    printf("Create Proxy pointer = %d, next is %d, the continuation is %p and as prtoval %p, the proxy %p, vproc %d\n",vp->proxyTableentries + 1,next, (void*)(fls),(void*)PtrToValue(fls),(void*)p ,vp->id);
#endif

    vp->proxyTableentries++;
    
    return p;
    
}

/**
 * if no entries left return 1 else 0
 */
int isFree (VProc_t *vp) {
    return (vp->proxyTableentries == 512 ? 1 : 0);
}

/**
 * Creates a dummy list of items in the vproc.
 */
void createList (VProc_t *vp) {
    //printf("Reset Proxy List\n");
    
    //set List of empty entries
    for (int i = 0; i < vp->maxProxy-1; i++) {
        vp->proxyTable[i].proxyObj=(Value_t)(long long int)(i+1);
    }
    vp->proxyTable[vp->maxProxy-1].proxyObj = (Value_t)(1000);
    vp->proxyTableentries = 0;
}

/**
 * Prints the integer to stdout.
 */
void isPrintProxy (int id) {
    printf("Integer = %d\n", id);
}

void globalCheck (VProc_t *vp) {
    
    //printf("Proxy Memory Check, global alloc ptr is %p, the limit is %p\n",(void *)vp->globNextW,(void*)vp->globLimit);
    
    if ((vp->globNextW + WORD_SZB * 3) >= vp->globLimit) {
        GetChunkForVProc(vp);
        printf("AllocProxy need more space\n");
    }
}

void checkTable(VProc_t *self) {
        
        for (int i=0; i < self->proxyTableentries;i++) {
                Value_t p = self->proxyTable[i].proxyObj;
                //assert (isFromSpacePtr(p));
                
                Word_t  *p2 = ((Word_t *)ValueToPtr(p));
                
                if ( p2[1] != i ) { 
                        
                        printf("Error at slot %d, the value is %lu\n",i,(unsigned long)p2[1]);
                        
                        assert(p2[1] == i);
                }
        }
        
        
}

void isProxy (VProc_t *vp, int id) {
    
    
     printf("Proxy check, vproc %d id %d\n",vp->id,id);
    
    if ((id > -1) && (id < 512)){
        printf("Hallo 1 \n");
    Word_t *p = (Word_t *)(vp->proxyTable[id].proxyObj);
            printf("Hallo 2 localobj %llu, pos = %p\n",(long long int)vp->proxyTable[id].localObj,(void*)vp->proxyTable[id].localObj);
        printf("Hallo 2 %llu, pos = %p\n",(long long int)vp->proxyTable[id].proxyObj,(void*)vp->proxyTable[id].proxyObj);
        
    Word_t hdr = p[-1];
        printf("Hallo 3 \n");
    
            if (getID(hdr) == 2) {
        printf("Found Proxy, vproc %d id %d, length = %d and slot entry is %lu\n",vp->id,id,GetLength(hdr),(unsigned long)p[1]);
            printf("Position in Memory Proxy, vproc %p, globalptr %p, limit is %p there is %d left\n",(void *) vp,(void*)vp->globNextW,(void*)vp->globLimit, (int) ( vp->globLimit - vp->globNextW ));
            } else printf("NO PROXY HEADER\n");
    } else { 
        printf("ERROR NO RPOXY\n");
    }
        
        checkTable(vp);
}

#endif

