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
#include "value.h"
#include <stdio.h>

extern int ASM_DS_Return;
extern int ASM_DS_ApplyClos;
extern int ASM_DS_EscapeThrow;

uint64_t invalidRetAddr = 0xDEADACE;

size_t dfltStackSz = 1048576; // TODO make this a parameter of the compiler

// Retrieves an unused stack for the given vproc.
StackInfo_t* GetStack(VProc_t *vp) {
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
    
    return info;
}

Value_t NewStack (VProc_t *vp, Value_t funClos) {
    StackInfo_t* info = GetStack(vp);
    
    uint64_t* sp = (uint64_t*)(info->initialSP);
    
    /* we initialize one frame:
        low                                            high
                                              16-byte
                                                 v
        [ &ApplyClos | funClos ][ invalidRetAddr ]
        ^                       ^
  returned stkPtr            initial sp             
                                                                 
    */
    sp[0] = invalidRetAddr; // funClos should not try to return!
    sp[-1] = funClos;
    sp[-2] = &ASM_DS_ApplyClos;
    sp = sp - 2;
    
    // now we need to allocate the stack cont object
    Value_t resumeK = AllocStkCont(vp, (Addr_t)&ASM_DS_EscapeThrow,
                                        sp, // stack ptr
                                        info); // stack info
    
    return resumeK;
}

StackInfo_t* NewMainStack (VProc_t* vp, void** initialSP) {
    StackInfo_t* info = GetStack(vp);
    
    // initialize stack for a return from manticore's main fun.
    void* stkPtr = info->initialSP;
    uint64_t* ptrToRetAddr = (uint64_t*)stkPtr;
    *ptrToRetAddr = (uint64_t)&ASM_DS_Return;
    
    // return values
    *initialSP = stkPtr;
    return info;
}
