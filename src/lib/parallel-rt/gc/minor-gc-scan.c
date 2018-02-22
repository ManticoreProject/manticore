#include <stdint.h>
#include <stdio.h>

#include "gc-scan.h"
#include "gc-inline.h"

Word_t * minorGCscanRAWpointer (Word_t* nextScan, Word_t **nextW, Addr_t allocSzB, Addr_t nurseryBase) {

Word_t hdr = nextScan[-1];   // get object header
assert (isRawHdr(hdr));
return (nextScan + GetLength(hdr));

  }
  
Word_t * minorGCscanVECTORpointer (Word_t* nextScan, Word_t **nextW, Addr_t allocSzB, Addr_t nurseryBase) {

Word_t hdr = nextScan[-1];   // get object header
assert(isVectorHdr(hdr));
int len = GetLength(hdr);
for (int i = 0;  i < len;  i++, nextScan++) {
    Value_t *scanP = (Value_t *)nextScan;
    Value_t v = *scanP;
    if (isPtr(v) && inAddrRange(nurseryBase, allocSzB, ValueToAddr(v))) {
        *scanP = ForwardObjMinor(v, nextW);
    }
}
return nextScan;
}

Word_t * minorGCscanSTKCONTpointer (Word_t* nextScan, Word_t **nextW, Addr_t allocSzB, Addr_t nurseryBase) {

Word_t hdr = nextScan[-1];   // get object header
assert(isStackHdr(hdr));
int len = GetLength(hdr);
const int expectedLen = 3;
assert(len == expectedLen);

void* stkPtr = (void*)(nextScan[1]);
StackInfo_t* stkInfo = (StackInfo_t*)(nextScan[2]);

ScanStackMinor(stkPtr, stkInfo, nurseryBase, allocSzB, nextW);

nextScan += expectedLen;
return nextScan;
  }
  
Word_t * minorGCscanLINKFRAMEpointer (Word_t* nextScan, Word_t **nextW, Addr_t allocSzB, Addr_t nurseryBase) {
    Word_t hdr = nextScan[-1];
    assert(isLinkedFrameHdr(hdr)); 
    int len = GetLength(hdr);
    
    uint64_t* curFrame = (uint64_t*)nextScan;
    
    while (true) {
        uint64_t* linkPtr = (uint64_t*) ( curFrame[0] );
        uint64_t retAddr = curFrame[1];
        // uint64_t watermark = p[2];
        uint64_t contentsBase = (uint64_t)( curFrame+2 ); // base starts at watermark.
        
        // get info
        frame_info_t* frame = lookup_return_address(SPTbl, retAddr);
        
        // have we hit the end of the stack?
        if (frame == 0) {
            // if it's non-zero, then we're not at the base; the retAddr is bad!
            assert(linkPtr == 0);
            break;
        }
            
        // update pointers in curFrame.
        for (uint16_t i = 0; i < frame->numSlots; i++) {
            pointer_slot_t slotInfo = frame->slots[i];
            if (slotInfo.kind >= 0) {
                Die("unexpected derived pointer\n");
            }
            
            Value_t *root = (Value_t *)(contentsBase + slotInfo.offset);
            Value_t p = *root;
            
            if (isPtr(p) && inAddrRange(nurseryBase, allocSzB, ValueToAddr(p))) {
                *root = ForwardObjMinor(p, nextW);
            }
        } // end for
        
        
        // determine what to do with the link pointer.
        
        if (linkPtr == 0) {
            // this was the last frame, which we scanned.
            break;
            
        } else if (inAddrRange(nurseryBase, allocSzB, (Addr_t)linkPtr)) {
            // the link to the next frame frame is a nursery pointer, so we simply
            // forward it like a normal pointer and stop here.
            *curFrame = (uint64_t) ForwardObjMinor((Value_t)linkPtr, nextW);
            break;
            
        } 

        // otherwise, the previous frame is not located in the nursery, but
        // we must scan it for pointers _into_ the nursery.
        curFrame = linkPtr;
        
    } // end while
    
    return (nextScan + len);
}

  
Word_t * minorGCscanBITPATpointer (Word_t* ptr, Word_t **nextW, Addr_t allocSzB, Addr_t nurseryBase) { 
Word_t *nextScan = ptr; 
Word_t hdr = nextScan[-1];   // get object header 
assert(isBitPatHdr(hdr)); 
uint32_t len = GetLength(hdr); 
uint16_t pat = GetPattern(hdr); 
 
for (; pat > 0; pat >>= 1, nextScan++) { 
  if (pat & 1) { 
    Value_t *scanP = (Value_t *)nextScan; 
    Value_t v = *scanP; 
    if (isPtr(v) && inAddrRange(nurseryBase, allocSzB, ValueToAddr(v))) { 
        *scanP = ForwardObjMinor(v, nextW); 
    } 
  } 
} 
 
return (ptr+len); 
} 
