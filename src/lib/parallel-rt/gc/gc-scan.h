#ifndef _GC_SCAN_H_
#define _GC_SCAN_H_

#include "vproc.h"

//new table structure
typedef struct{
	void (*minorGCscanfunction) (Word_t *,  Word_t**, Addr_t,Addr_t);
	void (*majorGCscanfunction) (Word_t *,  VProc_t *, Addr_t,Addr_t);
	void (*globalGCscanfunction) (Word_t *,  VProc_t *);
	void (*ScanGlobalToSpacefunction) (Word_t *,  VProc_t *,Addr_t);
} tableentry;

//table array to match the tagbits with the entries
extern tableentry table[];

#endif /* !_GC-SCAN_H_ */
