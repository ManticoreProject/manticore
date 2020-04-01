#include <stdint.h>
#include <stdio.h>

#include "gc-scan.h"
#include "gc-inline.h"

Word_t * majorGCscanRAWpointer (Word_t* scanPtr, VProc_t *vp, Addr_t heapBase)  {
Word_t hdr = scanPtr[-1];   // get object header
assert (isRawHdr(hdr));
return (scanPtr + GetLength(hdr));
}
Word_t * majorGCscanVECTORpointer (Word_t* ptr, VProc_t *vp, Addr_t heapBase) {

Word_t *scanPtr = ptr;
Word_t hdr = scanPtr[-1];   // get object header
assert (isVectorHdr(hdr));
int len = GetLength(hdr);
for (int i = 0;  i < len;  i++, scanPtr++) {
    Value_t *scanP = (Value_t *)scanPtr;
    Value_t v = *scanP;
    if (isPtr(v) && inVPHeap(heapBase, ValueToAddr(v))) {
        *scanP = ForwardObjMajor(vp, v);
    }
}
return scanPtr;
}

Word_t * majorGCscanSTKCONTpointer (Word_t* ptr, VProc_t *vp, Addr_t heapBase)  {
Word_t *scanPtr = ptr;
Word_t hdr __attribute__((unused));
hdr = scanPtr[-1];   // get object header
assert (isStackHdr(hdr));
const int expectedLen = 3;
assert(GetLength(hdr) == expectedLen);

void* stkPtr = (void*)(scanPtr[1]);
StackInfo_t* stkInfo = (StackInfo_t*)(scanPtr[2]);

ScanStackMajor(stkPtr, stkInfo, heapBase, vp);

scanPtr += expectedLen;
return scanPtr;
}

Word_t * majorGCscanLINKFRAMEpointer (Word_t* nextScan, VProc_t *vp, Addr_t heapBase)  {

    Word_t hdr = nextScan[-1];
    assert(isLinkedFrameHdr(hdr));
    int len = GetLength(hdr);

    enum LimitState {
        LS_NoMark,
        LS_Stop
    };

/////////////////////// phase specific code
    const Age_t promoteGen = AGE_Global;
///////////////////////

    enum LimitState state = LS_NoMark;
    uint64_t* curFrame = (uint64_t*)nextScan;

    while (state != LS_Stop) {
        uint64_t linkPtr = curFrame[0];
        uint64_t retAddr = curFrame[1];
        uint64_t* watermark = curFrame + 2;
        uint64_t contentsBase = (uint64_t)( curFrame + 2 ); // base starts at watermark.

        // get info
        frame_info_t* frame = lookup_return_address(SPTbl, retAddr);

        // have we hit the end of the stack?
        if (frame == 0) {
            // if it's non-zero, then we're not at the base; the retAddr is bad!
            assert(linkPtr == 0);
            break;
        }


        // check the watermark
        if (*watermark >= promoteGen) {
            // this is the last frame we'll check.
            state = LS_Stop;
        } else {
            // overwrite the watermark
            *watermark = promoteGen;
        }


        // update pointers in curFrame.
        for (uint16_t i = 0; i < frame->numSlots; i++) {
            pointer_slot_t slotInfo = frame->slots[i];
            if (slotInfo.kind >= 0) {
                Die("unexpected derived pointer\n");
            }

            Value_t *root = (Value_t *)(contentsBase + slotInfo.offset);
            Value_t p = *root;

/////////////////////// phase specific code
            if (isPtr(p) && inVPHeap(heapBase, ValueToAddr(p))) {
                *root = ForwardObjMajor(vp, p);
            }
///////////////////////

        } // end for


        // determine what to do with the link pointer.

        if (linkPtr == 0) {
            // this was the last frame, which we scanned.
            break;

/////////////////////// phase specific code
        } else if (inVPHeap(heapBase, (Addr_t)linkPtr)) {
            *curFrame = (uint64_t) ForwardObjMajor(vp, (Value_t)linkPtr);

///////////////////////
            break;
        }

        // otherwise, the previous frame is not located in the Major Heap,
        // but we must scan it.
        curFrame = (uint64_t*)linkPtr;

    } // end while

    return (nextScan + len);

}
Word_t * majorGCscanBITPATpointer (Word_t* ptr, VProc_t *vp, Addr_t heapBase)  {
Word_t *scanPtr = ptr;
Word_t hdr = scanPtr[-1];   // get object header
assert(isBitPatHdr(hdr));
uint32_t len = GetLength(hdr);
uint16_t pat = GetPattern(hdr);

for (; pat > 0; pat >>= 1, scanPtr++) {
  if (pat & 1) {
    Value_t *scanP = (Value_t *)scanPtr;
    Value_t v = *scanP;
    if (isPtr(v) && inVPHeap(heapBase, ValueToAddr(v))) {
        *scanP = ForwardObjMajor(vp, v);
    }
  }
}
return (ptr+len);
}

Word_t * majorGCscanPROXYpointer (Word_t* ptr, VProc_t *vp, Addr_t heapBase)  {
  Die("majorGCscan encountered a Proxy object.");
}
