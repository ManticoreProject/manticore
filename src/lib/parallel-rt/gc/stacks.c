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
extern int ASM_DS_SegUnderflow;

uint64_t invalidRetAddr = 0xDEADACE;

size_t dfltStackSz = 4096; // TODO make this a parameter of the compiler

// Retrieves an unused stack for the given vproc.
StackInfo_t* GetStack(VProc_t *vp) {
    StackInfo_t* info;
    if (vp->freeStacks == NULL) {
        // get a fresh stack
#ifdef SEGSTACK
        info = AllocStackSegment(dfltStackSz);
#else
        info = AllocStack(dfltStackSz);
#endif
    } else {
        // pop an existing stack
        info = vp->freeStacks;
        vp->freeStacks = info->next;
    }
    
    // push on alloc'd list
    StackInfo_t* cur = vp->allocdStacks;
    if (cur != NULL) {
        cur->prev = info;
    }
    info->next = cur;
    info->prev = NULL;
    
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

StackInfo_t* StkSegmentOverflow (VProc_t* vp, uint8_t* old_origStkPtr) {
    StackInfo_t* fresh = GetStack(vp);
    StackInfo_t* old = vp->stdCont;
    
    uint8_t* old_stkPtr = old_origStkPtr;
    const int maxFrames = 4; // TODO make this a parameter of the compiler
    const uint64_t szOffset = 2 * sizeof(uint64_t);
    
    for(int i = 0; i < maxFrames; i++) {
        // grab the size field
        uint64_t* p = (uint64_t*)(old_stkPtr + szOffset); 
        uint64_t sz = *p;
        
        if(sz == ~0ULL)
            break; // hit the end of the segment
        
        // include this frame
        old_stkPtr += sz + sizeof(uint64_t);
    }
    
    uint64_t bytesToCopy = old_stkPtr - old_origStkPtr;
    
    // fprintf(stderr, "copying %llu bytes\n", bytesToCopy);
    
    // stkPtr now points to the ret addr of the new top of old segment
    
    /* Goal:
    
    high addresses                               low addresses
                 
                                        ptrB          ptrA
                                         v             v
        [ &UnderflowHandler ][ remainder | copiedData ]     <- old segment
                             
        [ &UnderflowHandler ][ copiedData ]       <- fresh segment
                                          ^
                                         ptrC
        
        where:
        ptrA = old_origStkPtr
        ptrB = ptrA + bytesToCopy
        
        old->currentSP = ptrB
        returned SP = ptrC
        
        
    */
    
    uint8_t* newStkPtr = fresh->initialSP;
    
    // install underflow handler
    *((uint64_t*)newStkPtr) = &ASM_DS_SegUnderflow;
    // pull pointer down
    newStkPtr -= bytesToCopy; 
    // copy frames to fresh segment. realignment should be unnessecary
    memcpy(newStkPtr, old_origStkPtr, bytesToCopy); 
    
    // initialize backwards link and save old segment's new top
    fresh->prevSegment = old;
    old->currentSP = old_stkPtr;
    
    // install the fresh segment as the current stack descriptor
    vp->stdCont = fresh;
    vp->stdEnvPtr = fresh->stkLimit;
    
    // return the new SP in the new segment
    return newStkPtr;
}

void* GetStkLimit(StackInfo_t* info) {
    return info->stkLimit;
}
