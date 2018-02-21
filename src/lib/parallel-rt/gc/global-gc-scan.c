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
Die("unable to scan a link-frame pointer!");
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
