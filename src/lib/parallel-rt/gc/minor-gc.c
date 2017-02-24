/* minor-gc.c
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Minor GCs are local collections of a vproc's allocation space.
 */

#include <strings.h>
#include <stdio.h>

#include "manticore-rt.h"
#include "heap.h"
#include "gc.h"
#include "vproc.h"
#include "value.h"
#include "internal-heap.h"
#include "gc-inline.h"
#include "event-log.h"
#include "work-stealing-deque.h"
#include "bibop.h"
#include "gc-scan.h"

extern Addr_t   MajorGCThreshold;   /* when the size of the nursery goes below */
                    /* this limit it is time to do a GC. */

//ForwardObject of MinorGC
/* Copy an object to the old region */
Value_t ForwardObjMinor (Value_t v, Word_t **nextW)
{
    Word_t  *p = (Word_t *)ValueToPtr(v);
    Word_t  hdr = p[-1];
    
    if (isForwardPtr(hdr)) {
        return PtrToValue(GetForwardPtr(hdr));
    } else {
        int len = GetLength(hdr);
        Word_t *newObj = *nextW;
        newObj[-1] = hdr;
        for (int i = 0;  i < len;  i++) {
            newObj[i] = p[i];
        }
        *nextW = newObj+len+1;
        
        p[-1] = MakeForwardPtr(hdr, newObj);
        return PtrToValue(newObj);
    }
    
}

#ifndef NDEBUG
static void CheckMinorGC (VProc_t *self, Value_t **roots);
#endif

void ScanStackMinor (
    void* origStkPtr,
    StackInfo_t* stkInfo,
    Addr_t nurseryBase,  
    Addr_t allocSzB,
    Word_t **nextW) {

#define DEBUG_STACK_SCAN_MINOR

    uint64_t framesSeen = 0;

/* TODO: 
    - the stack scanner should overwrite nursery water marks (the zeros) of frames
        it encounters
    - the stack scanner should stop at the high water mark of nursery frame roots.
*/
        
    frame_info_t* frame;
    uint64_t stackPtr = (uint64_t)origStkPtr;
    
    uint64_t deepest = (uint64_t)stkInfo->deepestScan;
    if(deepest <= (uint64_t)origStkPtr) {
        return; // this part of the stack has already been scanned.
    }
    
    stkInfo->deepestScan = origStkPtr; // mark that we've seen this stack
    
    while ((frame = lookup_return_address(SPTbl, *(uint64_t*)(stackPtr))) != 0) {

#ifdef DEBUG_STACK_SCAN_MINOR
        framesSeen++;
        print_frame(stderr, frame);    
#endif
        
        // step into frame
        stackPtr += sizeof(uint64_t);
        
        // process pointers
        for (uint16_t i = 0; i < frame->numSlots; i++) {
            pointer_slot_t slotInfo = frame->slots[i];
            if (slotInfo.kind >= 0) {
                assert(false && "unexpected derived pointer\n");
            }
            
            Value_t *root = (Value_t *)(stackPtr + slotInfo.offset);
            Value_t p = *root;
            
            if (isPtr(p) && inAddrRange(nurseryBase, allocSzB, ValueToAddr(p))) {
#ifdef DEBUG_STACK_SCAN_MINOR
                fprintf(stderr, "[slot %u : %p] forward %p --> %p\n", i, root, p, *nextW);
#endif
                *root = ForwardObjMinor(p, nextW);
            }
        } // end for
        
#ifdef DEBUG_STACK_SCAN_MINOR
        fprintf(stderr, "------------------------------------------\n");
#endif
        
        // move to next frame
        stackPtr += frame->frameSize;
        
    } // end while
    
    // the roots have been forwarded out of the nursery.
    stkInfo->age = AGE_Major;
    
#ifdef DEBUG_STACK_SCAN_MINOR
        if (framesSeen == 0) {
            Die("MinorGC: Should have seen at least one frame!");
        }
        fprintf(stderr, "##########################################\n");
#endif

    return;
}

/*
 * We perform a pass over the allocated list of stacks,
 * freeing any unmarked stacks who are young enough.
 *
 * The value returned indicates how many bytes were reclaimed.
 *
 * This function is also used by later GCs.
 */
size_t FreeStacks(VProc_t *vp, Age_t epoch) {
    StackInfo_t* allocd = vp->allocdStacks;
    StackInfo_t* prev = NULL;
    StackInfo_t* nextChk;
    size_t freedBytes = 0;
    
    while (allocd != NULL) {
        nextChk = allocd->next;
        
        if ((allocd->age <= epoch)  // young enough
            && (allocd->deepestScan == allocd) // unmarked
           ) {
            
            freedBytes += allocd->mmapSize;
            
            // unlink
            if (prev != NULL) {
                prev->next = nextChk;
            }
            
            // put allocd on free list
            StackInfo_t* freeTop = vp->freeStacks;
            allocd->next = freeTop;
            allocd->age = AGE_Minor;
            vp->freeStacks = allocd;
            
            // look at next one, prev remains the same
            allocd = nextChk;
            
        } else {
            // advance position
            prev = allocd;
            allocd = nextChk;
        }
    }
    return freedBytes;
}

/* unmarks all stacks in the alloc'd list (for the ones who survived). */
void UnmarkStacks(VProc_t *vp) {
    StackInfo_t* cur = vp->allocdStacks;
    while (cur != NULL) {
        cur->deepestScan = cur;
        cur = cur->next;
    }
    return;
}

/* MinorGC:
 */
void MinorGC (VProc_t *vp)
{
    Addr_t  nurseryBase = vp->nurseryBase;
    Addr_t  allocSzB = vp->allocPtr - nurseryBase - WORD_SZB;
    Word_t  *nextScan = (Word_t *)(vp->oldTop); /* current top of to-space */
    Word_t  *nextW = nextScan + 1;      /* next object address in to-space */

    //LogMinorGCStart (vp, (uint32_t)allocSzB);
    LogStartGC(vp);

#ifndef NO_GC_STATS
    TIMER_Start(&(vp->minorStats.timer));
#endif

    assert (vp->heapBase <= (Addr_t)nextScan);
    assert ((Addr_t)nextScan < vp->nurseryBase);
    assert (vp->nurseryBase < vp->allocPtr);

#ifndef NDEBUG
    if (GCDebug >= GC_DEBUG_MINOR) {
        SayDebug("[%2d] Minor GC starting\n", vp->id);
    }
#endif

  /* gather the roots.  The protocol is that the stdCont register holds
   * the return address (which is not in the heap) and that the stdEnvPtr
   * holds the GC root.
   */
    int nWorkStealingRoots = M_NumDequeRoots (vp);
    Value_t *roots[18 + nWorkStealingRoots], **rp;
    rp = roots;
    *rp++ = &(vp->currentFLS);
    *rp++ = &(vp->actionStk);
    *rp++ = &(vp->schedCont);
    *rp++ = &(vp->dummyK);
    *rp++ = &(vp->wakeupCont);
    *rp++ = &(vp->shutdownCont);
    *rp++ = &(vp->rdyQHd);
    *rp++ = &(vp->rdyQTl);
    *rp++ = &(vp->sndQHd);
    *rp++ = &(vp->sndQTl);
    *rp++ = &(vp->landingPad);

#ifndef DIRECT_STYLE
    /* with the DS runtime, the env ptr holds the current stack pointer,
       and this root is handled seperately. */
    *rp++ = &(vp->stdEnvPtr);
#endif // DIRECT_STYLE
    
    rp = M_AddDequeEltsToLocalRoots(vp, rp);
    *rp++ = 0;

#ifndef NDEBUG
  /* nullify non-live registers */
    vp->stdArg = M_UNIT;
    vp->stdExnCont = M_UNIT;
#endif

  /* process the roots */
    for (int i = 0;  roots[i] != 0;  i++) {
        Value_t p = *roots[i];

        if (isPtr(p) && inAddrRange(nurseryBase, allocSzB, ValueToAddr(p))) {
            *roots[i] = ForwardObjMinor(p, &nextW);
        }

    }
    
#ifdef DIRECT_STYLE
    /* scan the current stack. */
    StackInfo_t* stkInfo = vp->stdCont;
    void* stkPtr = vp->stdEnvPtr;
    ScanStackMinor(stkPtr, stkInfo, nurseryBase, allocSzB, &nextW);
#endif

  /* scan to space */
    while (nextScan < nextW-1) {

        assert ((Addr_t)(nextW-1) <= vp->nurseryBase);

        Word_t hdr = *nextScan++;   // get object header
        
        if (isVectorHdr(hdr)) {
            //Word_t *nextScan = ptr;
            int len = GetLength(hdr);
            for (int i = 0;  i < len;  i++, nextScan++) {

                Value_t *scanP = (Value_t *)nextScan;
                Value_t v = *scanP;

                if (isPtr(v) && inAddrRange(nurseryBase, allocSzB, ValueToAddr(v))) {
                    *scanP = ForwardObjMinor(v, &nextW);
                }

            }
            
            
        } else if (isRawHdr(hdr)) {
            nextScan += GetLength(hdr);
            
        }
        
#ifdef DIRECT_STYLE
        else if (isStackHdr(hdr)) {
            
            int len = GetLength(hdr);
            const int expectedLen = 3;
            assert(len == expectedLen && "ASM code doesn't match GC assumptions");
            
            void* stkPtr = nextScan[1];
            StackInfo_t* stkInfo = nextScan[2];
            
            ScanStackMinor(stkPtr, stkInfo, nurseryBase, allocSzB, &nextW);
            
            nextScan += expectedLen;
            
        }
#endif
        
        else {
            //printf("MinorGC id = %d, length = %d, scan = %p\n",getID(hdr),GetLength(hdr),(void *)nextScan);
            
            // TODO assert its a mixed header
            nextScan = table[getID(hdr)].minorGCscanfunction(nextScan,&nextW, allocSzB,nurseryBase);
            
            //printf("scan after = %p\n",(void *)nextScan);
        }

    }

#ifdef DIRECT_STYLE
    /* try to free unreachable stacks */
    int numFreed = FreeStacks(vp, AGE_Minor);
#endif

    assert ((Addr_t)nextScan >= vp->heapBase);
    Addr_t avail = VP_HEAP_SZB - ((Addr_t)nextScan - vp->heapBase);

#ifndef NO_GC_STATS
    vp->nMinorGCs++;
    vp->minorStats.nBytesAlloc += vp->allocPtr - vp->nurseryBase - WORD_SZB;
    vp->minorStats.nBytesCollected = vp->minorStats.nBytesAlloc;
    vp->minorStats.nBytesCopied += (Addr_t)nextScan - vp->oldTop;
    vp->majorStats.nBytesAlloc += (Addr_t)nextScan - vp->oldTop;
    TIMER_Stop(&(vp->minorStats.timer));
#endif

#ifndef NDEBUG
    if (GCDebug >= GC_DEBUG_MINOR) {
        bzero(nextScan, avail); /* clear unused part of local heap */
        SayDebug("[%2d] Minor GC finished: %ld/%ld bytes live; %d available\n",
                vp->id, (Addr_t)nextScan - vp->oldTop,
                vp->allocPtr - vp->nurseryBase - WORD_SZB,
                (int)avail);
    }
#endif /* !NDEBUG */

    //LogMinorGCEnd (vp, (uint32_t)((Addr_t)nextScan - vp->oldTop), (uint32_t)avail);

    if ((avail < MajorGCThreshold) || vp->globalGCPending) {
        /* time to do a major collection. */
        MajorGC (vp, roots, (Addr_t)nextScan);
    } else {
        /* remember information about the final state of the heap */
        vp->oldTop = (Addr_t)nextScan;
    }

#ifndef NDEBUG
    if (HeapCheck >= GC_DEBUG_MINOR) {
        CheckMinorGC (vp, roots);
    }
#endif


#ifdef DIRECT_STYLE
    /* unmark all surviving stacks */
    UnmarkStacks(vp);
#endif

    /* reset the allocation pointer */
    SetAllocPtr (vp);
    LogEndGC(vp);

}

#ifndef NDEBUG
void CheckLocalPtrMinor (VProc_t *self, void *addr, const char *where)
{
    Value_t v = *(Value_t *)addr;
    if (isPtr(v)) {
        MemChunk_t *cq = AddrToChunk(ValueToAddr(v));
        if (cq->sts == TO_SP_CHUNK) {
            return;
        } else if (cq->sts == FROM_SP_CHUNK) {
            SayDebug("CheckLocalPtrMinor: unexpected from-space pointer %p at %p in %s\n",
                        ValueToPtr(v), addr, where);
        } else if (IS_VPROC_CHUNK(cq->sts)) {

            if (cq->sts != VPROC_CHUNK(self->id)) {
                SayDebug("CheckLocalPtrMinor: unexpected remote pointer %p at %p in %s\n",
                    ValueToPtr(v), addr, where);
            } else if (! inAddrRange(self->heapBase, self->oldTop - self->heapBase, ValueToAddr(v))) {
                SayDebug("CheckLocalPtrMinor: local pointer %p at %p in %s is out of bounds\n",
                    ValueToPtr(v), addr, where);
            }

        } else if (cq->sts == FREE_CHUNK) {
            SayDebug("CheckLocalPtrMinor: unexpected free-space pointer %p at %p in %s\n",
            ValueToPtr(v), addr, where);
        }
    }
}

static void CheckMinorGC (VProc_t *self, Value_t **roots)
{

    char buf[32];
    // check the roots
    for (int i = 0;  roots[i] != 0;  i++) {
        sprintf(buf, "root[%d]", i);
        Value_t v = *roots[i];
        CheckLocalPtrMinor (self, roots[i], buf);
    }

    // check the local heap
    {

    Word_t *top = (Word_t *)(self->oldTop);
    Word_t *p = (Word_t *)self->heapBase;
    while (p < top) {
        
        Word_t hdr = *p++;
        Word_t *scanptr = p;
        
        if (isForwardPtr(hdr)) {
          // forward pointer
            Word_t *forwardPtr = GetForwardPtr(hdr);
            CheckLocalPtrMinor(self, forwardPtr, "forward pointer");
            Word_t hdr = forwardPtr[-1];
            
            p += GetLength(hdr);
        
        } else {    
            tableDebug[getID(hdr)].minorGCdebug(self,scanptr);
            p += GetLength(hdr);
        }
    }

    }

    // check the global heap allocation space
    MemChunk_t  *cp = self->globAllocChunk;
    assert (cp->sts = TO_SP_CHUNK);
    Word_t *p = (Word_t *)(cp->baseAddr);
    Word_t *top = UsedTopOfChunk(self, cp);
    while (p < top) {
        Word_t hdr = *p++;
        Word_t *scanptr = p;
        
        tableDebug[getID(hdr)].minorGCdebugGlobal(self,scanptr);
        p += GetLength(hdr);
    }
}
#endif /* NDEBUG */
