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
    Die("TODO: scan the linked frame!");
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
