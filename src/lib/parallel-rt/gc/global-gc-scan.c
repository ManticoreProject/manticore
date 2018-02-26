#include <stdint.h>
#include <stdio.h>

#include "gc-scan.h"
#include "gc-inline.h"


Word_t * globalGCscanRAWpointer (Word_t* ptr, VProc_t *vp) {

Word_t hdr = ptr[-1];
assert (isRawHdr(hdr));

return (ptr + GetLength(hdr));
}
Word_t * globalGCscanVECTORpointer (Word_t* ptr, VProc_t *vp) {

Word_t *scanPtr = ptr;
Word_t hdr = scanPtr[-1];
int len = GetLength(hdr);
assert (isVectorHdr(hdr));
for (int i = 0;  i < len;  i++, scanPtr++) { 
    Value_t *scanP = (Value_t *)scanPtr; 
    Value_t v = *scanP; 
    if (isFromSpacePtr(v)) { 
        *scanP = ForwardObjGlobal(vp, v); 
    } 
     
    assert (!(isPtr(v) && IS_VPROC_CHUNK(AddrToChunk(ValueToAddr(v))->sts))); 
} 
return scanPtr;
}

Word_t * globalGCscanSTKCONTpointer (Word_t* ptr, VProc_t *vp) {

Word_t *scanPtr = ptr;
Word_t hdr = scanPtr[-1];
int len = GetLength(hdr);
assert (isStackHdr(hdr));
const int expectedLen = 3; 
assert(len == expectedLen); 
 
void* stkPtr = (void*)(scanPtr[1]); 
StackInfo_t* stkInfo = (StackInfo_t*)(scanPtr[2]); 
 
ScanStackGlobal(stkPtr, stkInfo, vp); 
 
scanPtr += expectedLen; 
return scanPtr;
}
Word_t * globalGCscanLINKFRAMEpointer (Word_t* ptr, VProc_t *vp) {
    Word_t *nextScan = ptr;
    Word_t hdr = nextScan[-1];
    assert(isLinkedFrameHdr(hdr)); 
    int len = GetLength(hdr);
    
/////////////////////// phase specific code
    const Age_t promoteGen = AGE_Global;
///////////////////////

    uint64_t* curFrame = (uint64_t*)nextScan;
    
    while (true) {
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
        
        // it's all in the global heap.
        *watermark = promoteGen;
            
        // update pointers in curFrame.
        for (uint16_t i = 0; i < frame->numSlots; i++) {
            pointer_slot_t slotInfo = frame->slots[i];
            if (slotInfo.kind >= 0) {
                Die("unexpected derived pointer\n");
            }
            
            Value_t *root = (Value_t *)(contentsBase + slotInfo.offset);
            Value_t p = *root;
            
/////////////////////// phase specific code
            if (isFromSpacePtr(p)) { 
                *root = ForwardObjGlobal(vp, p);
            } 
///////////////////////
            
        } // end for
        
        
        // determine what to do with the link pointer.
        
        if (linkPtr == 0) {
            // this was the last frame, which we scanned.
            break;
            
/////////////////////// phase specific code
        } else if (isFromSpacePtr((Value_t)linkPtr)) { 
                // we need to forward the frame pointed to by the link field.
                *curFrame = (uint64_t) ForwardObjGlobal(vp, (Value_t)linkPtr);
            
///////////////////////
            break;
        }
        
        // TODO the frame is somewhere else. perhaps in the vproc's local heap?
        curFrame = (uint64_t*)linkPtr;
        
    } // end while
    
    return (nextScan + len);
}

Word_t * globalGCscanBITPATpointer (Word_t* ptr, VProc_t *vp) { 
Word_t *scanPtr = ptr;    
Word_t hdr = scanPtr[-1];   // get object header    
assert(isBitPatHdr(hdr));    
uint32_t len = GetLength(hdr);    
uint16_t pat = GetPattern(hdr);    
   
for (; pat > 0; pat >>= 1, scanPtr++) {   
  if (pat & 1) {   
    Value_t *scanP = (Value_t *)scanPtr;  
    Value_t v = *scanP;  
    if (isFromSpacePtr(v)) {  
        *scanP = ForwardObjGlobal(vp, v);  
    } 
  } 
} 
return (ptr+len); 
} 
