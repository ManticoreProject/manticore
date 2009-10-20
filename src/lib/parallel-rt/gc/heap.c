/* heap.c
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 */

#include "manticore-rt.h"
#include "heap.h"
#include "vproc.h"
#include "topology.h"
#include "bibop.h"
#include "os-memory.h"
#include "os-threads.h"
#include "options.h"
#include "internal-heap.h"
#ifndef NO_GC_STATS
#include <string.h>
#include <stdio.h>
#endif

Mutex_t		HeapLock;	/* lock for protecting heap data structures */

Addr_t		GlobalVM;	/* amount of memory allocated to Global heap (including */
				/* free chunks). */
Addr_t		FreeVM;		/* amount of free memory in free list */
Addr_t		ToSpaceSz;	/* amount of memory being used for to-space */
Addr_t		ToSpaceLimit;	/* if ToSpaceSz exceeds this value, then do a */
				/* global GC */
Addr_t		TotalVM = 0;	/* total memory used by heap (including vproc local heaps) */
Addr_t		MaxNurserySzB;	/* limit on size of nursery in vproc heap */
Addr_t		MajorGCThreshold; /* when the size of the nursery goes below this limit */
				/* it is time to do a GC. */
MemChunk_t	*FromSpaceChunks; /* list of chunks is from-space */
MemChunk_t	**FreeChunks;	/* lists of free chunks, one per node */
uint32_t	NumGlobalGCs = 0;


/* The BIBOP maps addresses to the memory chunks containing the address.
 * It is used by the global collector and access to it is protected by
 * the HeapLock.
 */
#ifdef SIXTYFOUR_BIT_WORDS
MemChunk_t		**BIBOP[L1_TBLSZ];
static MemChunk_t	*FreeL2Tbl[L2_TBLSZ];
#else
MemChunk_t		*BIBOP[BIBOP_TBLSZ];
#endif
static MemChunk_t	UnmappedChunk;

#ifndef NDEBUG
static GCDebugLevel_t ParseGCLevel (const char *debug);

GCDebugLevel_t		GCDebug;	// Flag that controls GC debugging output
GCDebugLevel_t		HeapCheck;	// Flag that controls heap checking
#endif

#ifndef NO_GC_STATS
static void ParseGCStatsOptions (Options_t *opts);

static bool	ReportStatsFlg = false;	// true for report enabled
static bool	DetailStatsFlg = false;	// true for detailed report (per-vproc)
static bool	CSVStatsFlg = false;	// true for CSV-format report
#endif

/* HeapInit:
 *
 */
void HeapInit (Options_t *opts)
{
    MaxNurserySzB = GetSizeOpt (opts, "-nursery", ONE_K, VP_HEAP_SZB/2);
    if (MaxNurserySzB < MIN_NURSERY_SZB)
	MaxNurserySzB = MIN_NURSERY_SZB;

    MajorGCThreshold = VP_HEAP_SZB / 10;
    if (MajorGCThreshold < MIN_NURSERY_SZB)
	MajorGCThreshold = MIN_NURSERY_SZB;

#ifndef NO_GC_STATS
    ParseGCStatsOptions (opts);
#endif

#ifndef NDEBUG
    const char *debug = GetStringOpt (opts, "-gcdebug", DebugFlg ? GC_DEBUG_DEFAULT : "none");
    GCDebug = ParseGCLevel (debug);

    debug = GetStringOpt (opts, "-heapcheck", DebugFlg ? HEAP_DEBUG_DEFAULT : "none");
    HeapCheck = ParseGCLevel (debug);

    if (GCDebug > GC_DEBUG_NONE)
	SayDebug("HeapInit: max nursery = %d, threshold = %d\n", (int)MaxNurserySzB, (int)MajorGCThreshold);
#endif

  /* initialize the BIBOP */
#ifdef SIXTYFOUR_BIT_WORDS
    for (int i = 0;  i < L2_TBLSZ; i++)
	FreeL2Tbl[i] = &UnmappedChunk;
    for (int i = 0;  i < L1_TBLSZ;  i++)
	BIBOP[i] = FreeL2Tbl;
#else
    for (int i = 0;  i < BIBOP_TBLSZ;  i++)
	BIBOP[i] = &UnmappedChunk;
#endif
    UnmappedChunk.sts = UNMAPPED_CHUNK;

  /* initialize the heap data structures */
    MutexInit (&HeapLock);
    GlobalVM = 0;
    FreeVM = 0;
    ToSpaceSz = 0;
    ToSpaceLimit = BASE_GLOBAL_HEAP_SZB; // we don't know the number of vprocs yet!
    TotalVM = 0;
    FromSpaceChunks = (MemChunk_t *)0;
    FreeChunks = NEWVEC(MemChunk_t *, NumHWNodes);
    for (int i = 0;  i < NumHWNodes;  i++)
	FreeChunks[i] = 0;

    InitGlobalGC ();

} /* end of HeapInit */

/* InitVProcHeap:
 */
void InitVProcHeap (VProc_t *vp)
{
    vp->globToSpHd = (MemChunk_t *)0;
    vp->globToSpTl = (MemChunk_t *)0;
    vp->globalGCPending = false;

  /* allocate the initial chunk for the vproc */
    AllocToSpaceChunk (vp);

}


/*! \brief Allocate a global-heap memory chunk for the vproc.
 *
 * Get a memory chunk from the free list or by allocating fresh memory; the
 * size of the chunk will be #HEAP_CHUNK_SZB bytes.  The chunk is added to the
 * to-space list.
 * NOTE: this function should only be called with the #HeapLock is held.
 */
void AllocToSpaceChunk (VProc_t *vp)
{
    void	*memObj;
    MemChunk_t	*chunk;
    int		node = LocationNode(vp->location);

    MutexLock (&HeapLock);
	if (FreeChunks[node] == (MemChunk_t *)0) {
	  /* no free chunks on this node, so allocate storage from OS */
	    int nPages = HEAP_CHUNK_SZB >> PAGE_BITS;
	    memObj = AllocMemory(&nPages, BIBOP_PAGE_SZB, nPages);
	    chunk = NEW(MemChunk_t);
	    if ((memObj == (void *)0) || (chunk == (MemChunk_t *)0)) {
		Die ("unable to allocate memory for global heap\n");
	    }
	    chunk->baseAddr = (Addr_t)memObj;
	    chunk->szB = nPages * BIBOP_PAGE_SZB;
	    chunk->where = node;
	    UpdateBIBOP (chunk);
	}
	else {
	    chunk = FreeChunks[node];
	    FreeChunks[node] = chunk->next;
	    assert (chunk->where == node);
	}
	chunk->sts = TO_SP_CHUNK;
	ToSpaceSz += HEAP_CHUNK_SZB;
    MutexUnlock (&HeapLock);

  /* add to the tail of the vproc's list of to-space chunks */
    chunk->next = (MemChunk_t *)0;
    if (vp->globToSpHd == (MemChunk_t *)0) {
	vp->globToSpHd = chunk;
	vp->globToSpTl = chunk;
    }
    else {
	vp->globToSpTl->usedTop = vp->globNextW - WORD_SZB;
	vp->globToSpTl->next = chunk;
	vp->globToSpTl = chunk;
    }

    vp->globNextW = chunk->baseAddr + WORD_SZB;
    vp->globLimit = chunk->baseAddr + chunk->szB;

#ifndef NDEBUG
    if (GCDebug > GC_DEBUG_NONE)
	SayDebug("[%2d] AllocToSpaceChunk: %ld Kb at %p..%p (node %d)\n",
	    vp->id, chunk->szB/1024, (void *)(chunk->baseAddr),
	    (void *)(chunk->baseAddr+chunk->szB), chunk->where);
#endif

}

/*! \brief Allocate a VProc's local memory object.
 */
Addr_t AllocVProcMemory (int id, Location_t loc)
{
    assert (VP_HEAP_SZB >= BIBOP_PAGE_SZB);

    int nPages = VP_HEAP_SZB / BIBOP_PAGE_SZB;
    MutexLock (&HeapLock);
	Addr_t vpHeap = (Addr_t) AllocMemory (&nPages, BIBOP_PAGE_SZB, nPages);
	if (vpHeap == 0) {
	    MutexUnlock (&HeapLock);
	    return 0;
	}
      /* allocate a BIBOP chunk descriptor for this object */
	MemChunk_t *chunk = NEW(MemChunk_t);
	if (chunk == (MemChunk_t *)0) {
	    MutexUnlock (&HeapLock);
	    return 0;
	}
	chunk->baseAddr = vpHeap;
	chunk->szB = VP_HEAP_SZB;
	chunk->sts = VPROC_CHUNK(id);
	chunk->where = LocationNode(loc);
	UpdateBIBOP (chunk);
    MutexUnlock (&HeapLock);

#ifndef NDEBUG
    if (GCDebug > GC_DEBUG_NONE)
	SayDebug("     AllocVProcMemory(%d): %ld Kb at %p..%p (node %d)\n",
	    id, chunk->szB/1024, (void *)(chunk->baseAddr),
	    (void *)(vpHeap+chunk->szB), chunk->where);
#endif

    return vpHeap;

}


/* UpdateBIBOP:
 *
 * Update the BIBOP to point to the freshly allocated chunk.
 *
 * NOTE: this function must be called with the HeapLock held.
 */
void UpdateBIBOP (MemChunk_t *chunk)
{
    Addr_t addr = chunk->baseAddr;
    Addr_t top = addr + chunk->szB;
    while (addr < top) {
#ifdef SIXTYFOUR_BIT_WORDS
	MemChunk_t	**l2 = BIBOP[addr >> L1_SHIFT];
	assert (l2[(addr >> L2_SHIFT) & L2_MASK] == &UnmappedChunk);
	if (l2 == FreeL2Tbl) {
	  /* we need to allocate a new L2 table for this range */
	    l2 = NEWVEC(MemChunk_t *, L2_TBLSZ);
	    for (int i = 0;  i < L2_TBLSZ;  i++)
		l2[i] = &UnmappedChunk;
	    BIBOP[addr >> L1_SHIFT] = l2;
	}
	l2[(addr >> L2_SHIFT) & L2_MASK] = chunk;
#else /* !SIXTYFOUR_BIT_WORDS */
	assert (BIBOP[addr >> PAGE_BITS] == 0);
	BIBOP[addr >> PAGE_BITS] = chunk;
#endif /* SIXTYFOUR_BIT_WORDS */
	addr += BIBOP_PAGE_SZB;
    } /* while */

}

#ifndef NDEBUG
static GCDebugLevel_t ParseGCLevel (const char *debug)
{

    if (strcmp(debug, "none") == 0) return GC_DEBUG_NONE;
    else if (strcmp(debug, "minor") == 0) return GC_DEBUG_MINOR;
    else if (strcmp(debug, "major") == 0) return GC_DEBUG_MAJOR;
    else if (strcmp(debug, "global") == 0) return GC_DEBUG_GLOBAL;
    else if (strcmp(debug, "all") == 0) return GC_DEBUG_ALL;
    else return GC_DEBUG_NONE;

}
#endif

#ifndef NO_GC_STATS

/* process command-line args */
static void ParseGCStatsOptions (Options_t *opts)
{
    const char *report = GetStringEqOpt (opts, "-gcstats", "summary");

    if (report != 0) {
	ReportStatsFlg = true;
	if (strstr(report, "csv") != 0) CSVStatsFlg = true;
	if (strstr(report, "all") != 0) DetailStatsFlg = true;
    }

}

STATIC_INLINE void PrintNum (FILE *f, int wid, Addr_t nbytes)
{
    if (nbytes < 64 * ONE_K)
	fprintf (f, " %*d", wid, (int)nbytes);
    else if (nbytes < 64 * ONE_MEG)
	fprintf (f, " %*dk", wid-1, (int)(nbytes >> 10));
    else
	fprintf (f, " %*dM", wid-1, (int)(nbytes >> 20));

}

STATIC_INLINE void PrintPct (FILE *f, uint64_t n, uint64_t m)
{
    if (m == 0)
	fprintf (f, "( na )");
    else {
	double pct = 100.0 * (double)n / (double)m;
	if (pct < 1.0)
	    fprintf (f, "(%3.1f%%)", pct);
	else if (pct < 100.0)
	    fprintf (f, "(%2.0f%%) ", pct);
	else
	    fprintf (f, "(100%%)");
    }

}

void ReportGCStats ()
{
    char buffer[256], *bp;

    if (! ReportStatsFlg)
	return;

    FILE *outF = stdout;

  // compute summary information
    uint32_t nPromotes = 0;
    uint32_t nMinorGCs = 0;
    uint32_t nMajorGCs = 0;
    GCCntrs_t totMinor = { 0, 0 };
    GCCntrs_t totMajor = { 0, 0 };
    GCCntrs_t totGlobal = { 0, 0 };
    uint64_t nBytesPromoted = 0;
    for (int i = 0;  i < NumVProcs;  i++) {
	VProc_t *vp = VProcs[i];
      // include any memory allocated since the last minor GC
	vp->minorStats.nBytesAlloc += vp->allocPtr - vp->nurseryBase - WORD_SZB;
      // count the stats for this VProc
	nPromotes += vp->nPromotes;
	nMinorGCs += vp->nMinorGCs;
	nMajorGCs += vp->nMajorGCs;
	totMinor.nBytesAlloc += vp->minorStats.nBytesAlloc;
	totMinor.nBytesCopied += vp->minorStats.nBytesCopied;
	totMajor.nBytesAlloc += vp->majorStats.nBytesAlloc;
	totMajor.nBytesCopied += vp->majorStats.nBytesCopied;
	totGlobal.nBytesAlloc += vp->globalStats.nBytesAlloc;
	totGlobal.nBytesCopied += vp->globalStats.nBytesCopied;
	nBytesPromoted += vp->nBytesPromoted;
    }

  // print the header
    if (CSVStatsFlg) {
    }
    else {
	fprintf(outF, "             Minor GCs                  Major GCs               Promotions           Global GCs\n");
	fprintf(outF, "      num   alloc     copied      num   alloc      copied      num    bytes   num   alloc     copied\n");
	fprintf(outF, "--- ------ ------- ------------- ----- ------- ------------- ------- ------- ----- ------- -------------\n");
    }

    if (DetailStatsFlg) {
      // report per-vproc stats
	for (int i = 0;  i < NumVProcs;  i++) {
	    VProc_t *vp = VProcs[i];
	    if (CSVStatsFlg) {
	      // use comma-separated-values format
		fprintf (outF,
		    "p%02d, %d, %lld, %lld, %d, %lld, %lld, %d, %lld, %d, %lld, %lld\n",
		    i,
		    vp->nMinorGCs, vp->minorStats.nBytesAlloc, vp->minorStats.nBytesCopied,
		    vp->nMajorGCs, vp->majorStats.nBytesAlloc, vp->majorStats.nBytesCopied,
		    vp->nPromotes, vp->nBytesPromoted,
		    NumGlobalGCs, vp->globalStats.nBytesAlloc, vp->globalStats.nBytesCopied);
	    }
	    else {
	      // minor GCs
		fprintf (outF, "p%02d %6d", i, vp->nMinorGCs);
		PrintNum (outF, 7, vp->minorStats.nBytesAlloc);
		PrintNum (outF, 7, vp->minorStats.nBytesCopied);
		PrintPct (outF, vp->minorStats.nBytesCopied, vp->minorStats.nBytesAlloc);
	      // major GCs
		fprintf (outF, " %5d", vp->nMajorGCs);
		PrintNum (outF, 7, vp->majorStats.nBytesAlloc);
		PrintNum (outF, 7, vp->majorStats.nBytesCopied);
		PrintPct (outF, vp->majorStats.nBytesCopied, vp->majorStats.nBytesAlloc);
	      // promotions
		PrintNum (outF, 7, vp->nPromotes);
		PrintNum (outF, 7, vp->nBytesPromoted);
	      // global GCs
		fprintf (outF, " %5d", NumGlobalGCs);
		PrintNum (outF, 7, vp->globalStats.nBytesAlloc);
		PrintNum (outF, 7, vp->globalStats.nBytesCopied);
		PrintPct (outF, vp->globalStats.nBytesCopied, vp->globalStats.nBytesAlloc);
		fprintf (outF, "\n");
	    }
	}
    }

  // report the summary stats
    if (CSVStatsFlg) {
      // use comma-separated-values format
	fprintf (outF,
	    "TOT, %d, %lld, %lld, %d, %lld, %lld, %d, %lld, %d, %lld, %lld\n",
	    nMinorGCs, totMinor.nBytesAlloc, totMinor.nBytesCopied,
	    nMajorGCs, totMajor.nBytesAlloc, totMajor.nBytesCopied,
	    nPromotes, nBytesPromoted,
	    NumGlobalGCs, totGlobal.nBytesAlloc, totGlobal.nBytesCopied);
    }
    else {
      // minor GCs
	fprintf (outF, "TOT %6d", nMinorGCs);
	PrintNum (outF, 7, totMinor.nBytesAlloc);
	PrintNum (outF, 7, totMinor.nBytesCopied);
	PrintPct (outF, totMinor.nBytesCopied, totMinor.nBytesAlloc);
      // major GCs
	fprintf (outF, " %5d", nMajorGCs);
	PrintNum (outF, 7, totMajor.nBytesAlloc);
	PrintNum (outF, 7, totMajor.nBytesCopied);
	PrintPct (outF, totMajor.nBytesCopied, totMajor.nBytesAlloc);
      // promotions
	PrintNum (outF, 7, nPromotes);
	PrintNum (outF, 7, nBytesPromoted);
      // global GCs
	fprintf (outF, " %5d", NumGlobalGCs);
	PrintNum (outF, 7, totGlobal.nBytesAlloc);
	PrintNum (outF, 7, totGlobal.nBytesCopied);
	PrintPct (outF, totGlobal.nBytesCopied, totGlobal.nBytesAlloc);
	fprintf (outF, "\n");
    }

}

#endif
