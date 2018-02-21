#ifndef _GC_SCAN_H_
#define _GC_SCAN_H_

#include "vproc.h"

//new table structure
typedef struct{
	Word_t * (*minorGCscanfunction) (Word_t *,  Word_t**, Addr_t,Addr_t);
	Word_t * (*majorGCscanfunction) (Word_t *,  VProc_t *, Addr_t,Addr_t);
	Word_t * (*globalGCscanfunction) (Word_t *,  VProc_t *);
	Word_t * (*ScanGlobalToSpacefunction) (Word_t *,  VProc_t *,Addr_t);
} tableentry;

//table array to match the tagbits with the entries
extern tableentry table[];

// pre-defined table entries

extern Word_t * minorGCscanRAWpointer (Word_t*, Word_t**, Addr_t, Addr_t);
extern Word_t * minorGCscanVECTORpointer (Word_t*, Word_t**, Addr_t, Addr_t);
extern Word_t * minorGCscanSTKCONTpointer (Word_t*, Word_t**, Addr_t, Addr_t);
extern Word_t * minorGCscanBITPATpointer (Word_t*, Word_t**, Addr_t, Addr_t);
extern Word_t * minorGCscanLINKFRAMEpointer (Word_t*, Word_t**, Addr_t, Addr_t);

extern Word_t * majorGCscanRAWpointer (Word_t*, VProc_t*, Addr_t, Addr_t);
extern Word_t * majorGCscanVECTORpointer (Word_t*, VProc_t*, Addr_t, Addr_t);
extern Word_t * majorGCscanSTKCONTpointer (Word_t*, VProc_t*, Addr_t, Addr_t);
extern Word_t * majorGCscanBITPATpointer (Word_t*, VProc_t*, Addr_t, Addr_t);
extern Word_t * majorGCscanLINKFRAMEpointer (Word_t*, VProc_t*, Addr_t, Addr_t);

extern Word_t * ScanGlobalToSpaceRAWfunction (Word_t*, VProc_t *, Addr_t);
extern Word_t * ScanGlobalToSpaceVECTORfunction (Word_t*, VProc_t *, Addr_t);
extern Word_t * ScanGlobalToSpaceSTKCONTfunction (Word_t*, VProc_t *, Addr_t);
extern Word_t * ScanGlobalToSpaceBITPATfunction (Word_t*, VProc_t *, Addr_t);
extern Word_t * ScanGlobalToSpaceLINKFRAMEfunction (Word_t*, VProc_t *, Addr_t);

extern Word_t * globalGCscanRAWpointer (Word_t*, VProc_t *);
extern Word_t * globalGCscanVECTORpointer (Word_t*, VProc_t *);
extern Word_t * globalGCscanSTKCONTpointer (Word_t*, VProc_t *);
extern Word_t * globalGCscanBITPATpointer (Word_t*, VProc_t *);
extern Word_t * globalGCscanLINKFRAMEpointer (Word_t*, VProc_t *);



#ifndef NDEBUG

extern void CheckLocalPtrMinor (VProc_t *, void *, const char *);
extern void CheckLocalPtrGlobal (VProc_t *, void *, const char *);
extern bool isGlobalHeapPtr (Value_t v);

typedef struct{
	void (*minorGCdebug) (VProc_t *, Word_t *);
	void (*minorGCdebugGlobal) (VProc_t *, Word_t *);
	
	void (*globalGCdebug) (VProc_t *, Word_t *);
	void (*globalGCdebugGlobal) (VProc_t *, Word_t *);
	
	void (*gc_debug)(Word_t * , Addr_t , Addr_t );
} tableentryDebug;

//table array to match the tagbits with the entries
extern tableentryDebug tableDebug[];
#endif /* NDEBUG */

#endif /* !_GC-SCAN_H_ */
