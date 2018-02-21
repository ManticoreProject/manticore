#include <stdint.h>
#include <stdio.h>

#include "gc-scan.h"
#include "gc-inline.h"


Word_t * majorGCscanRAWpointer (Word_t* nextScan, VProc_t *vp, Addr_t oldSzB, Addr_t heapBase) {

Word_t hdr = nextScan[-1];   // get object header
assert (isRawHdr(hdr));
return (nextScan + GetLength(hdr));

  }
Word_t * majorGCscanVECTORpointer (Word_t* ptr, VProc_t *vp, Addr_t oldSzB, Addr_t heapBase) {

Word_t *nextScan = ptr;
Word_t hdr = nextScan[-1];   // get object header
assert (isVectorHdr(hdr));
int len = GetLength(hdr); 
for (int i = 0;  i < len;  i++, nextScan++) { 
    Value_t v = *(Value_t *)nextScan; 
    if (isPtr(v)) { 
        if (inAddrRange(heapBase, oldSzB, ValueToAddr(v))) { 
            *nextScan =(Word_t)ForwardObjMajor(vp, v); 
        } 
        else if (inVPHeap(heapBase, (Addr_t)v)) { 
            // p points to another object in the young region, 
            // so adjust it. 
            *nextScan = (Word_t)((Addr_t)v - oldSzB); 
        } 
    } 
} 
return nextScan; 
}

Word_t * majorGCscanSTKCONTpointer (Word_t* ptr, VProc_t *vp, Addr_t oldSzB, Addr_t heapBase) {
Word_t *nextScan = ptr;
Word_t hdr = nextScan[-1];   // get object header
assert (isStackHdr(hdr));
int len = GetLength(hdr); 
const int expectedLen = 3; 
assert(len == expectedLen); 
 
void* stkPtr = (void*)(nextScan[1]); 
StackInfo_t* stkInfo = (StackInfo_t*)(nextScan[2]); 
 
ScanStackMajor(stkPtr, stkInfo, heapBase, oldSzB, vp, false); 
 
nextScan += expectedLen; 
return nextScan;
}
Word_t * majorGCscanLINKFRAMEpointer (Word_t* ptr, VProc_t *vp, Addr_t oldSzB, Addr_t heapBase) {
Die("unable to scan a link-frame pointer!");
}
Word_t * majorGCscanBITPATpointer (Word_t* ptr, VProc_t *vp, Addr_t oldSzB, Addr_t heapBase) { 
Word_t *nextScan = ptr;  
Word_t hdr = nextScan[-1];   // get object header  
assert(isBitPatHdr(hdr));  
uint32_t len = GetLength(hdr);  
uint16_t pat = GetPattern(hdr);  
 
for (; pat > 0; pat >>= 1, nextScan++) { 
  if (pat & 1) { 
    Value_t v = *(Value_t *)nextScan;  
    if (isPtr(v)) {  
        if (inAddrRange(heapBase, oldSzB, ValueToAddr(v))) {  
            *nextScan =(Word_t)ForwardObjMajor(vp, v);  
        }  
        else if (inVPHeap(heapBase, (Addr_t)v)) {  
            // p points to another object in the young region,  
            // so adjust it.  
            *nextScan = (Word_t)((Addr_t)v - oldSzB);  
        }  
    } 
  } 
}
}


///////////////////////////////


Word_t * ScanGlobalToSpaceRAWfunction (Word_t* scanPtr, VProc_t *vp, Addr_t heapBase)  {
Word_t hdr = scanPtr[-1];   // get object header
assert (isRawHdr(hdr));
return (scanPtr + GetLength(hdr));
}
Word_t * ScanGlobalToSpaceVECTORfunction (Word_t* ptr, VProc_t *vp, Addr_t heapBase) {

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

Word_t * ScanGlobalToSpaceSTKCONTfunction (Word_t* ptr, VProc_t *vp, Addr_t heapBase)  {
Word_t *scanPtr = ptr;
Word_t hdr = scanPtr[-1];   // get object header
assert (isStackHdr(hdr));
int len = GetLength(hdr); 
const int expectedLen = 3; 
assert(len == expectedLen); 
 
void* stkPtr = (void*)(scanPtr[1]); 
StackInfo_t* stkInfo = (StackInfo_t*)(scanPtr[2]); 
 
ScanStackMajor(stkPtr, stkInfo, heapBase, 0, vp, true); 
 
scanPtr += expectedLen; 
return scanPtr;
}
Word_t * ScanGlobalToSpaceLINKFRAMEfunction (Word_t* ptr, VProc_t *vp, Addr_t heapBase)  {
Die("unable to scan a link-frame pointer!");
}
Word_t * ScanGlobalToSpaceBITPATfunction (Word_t* ptr, VProc_t *vp, Addr_t heapBase)  { 
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
