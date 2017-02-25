/* stacks.c
 * 
 * utilities for initializing and allocating stacks.
 *
 */



#include "manticore-rt.h"
#include <unistd.h>
#include <sys/mman.h>
#include <errno.h>
#include "os-memory.h"
#include "heap.h"
#include "gc.h"
#include "internal-heap.h"
#include <stdio.h>

extern int ASM_DS_Return;

size_t dfltStackSz = 1048576; // TODO make this a parameter of the compiler

Value_t NewStack (VProc_t *vp, Value_t finishClos, Value_t funClos) {
    StackInfo_t* stkInfo;
    if (vp->freeStacks == NULL) {
        // get a fresh stack
        
    } else {
        // pop an existing stack
        stkInfo = vp->freeStacks;
        vp->freeStacks = stkInfo->next;
    }
    
    return 0;
}

StackInfo_t* NewMainStack (VProc_t* vp, void** initialSP) {
    StackInfo_t* info;
    if (vp->freeStacks == NULL) {
        // get a fresh stack
        info = AllocStack(dfltStackSz);
    } else {
        // pop an existing stack
        info = vp->freeStacks;
        vp->freeStacks = info->next;
    }
    
    // push on alloc'd list
    info->next = vp->allocdStacks;
    vp->allocdStacks = info;
    
    // initialize stack for a return from manticore's main fun.
    void* stkPtr = info->initialSP;
    uint64_t* ptrToRetAddr = (uint64_t*)stkPtr;
    *ptrToRetAddr = (uint64_t)&ASM_DS_Return;
    
    // return values
    *initialSP = stkPtr;
    return info;
}
