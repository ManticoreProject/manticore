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
#ifndef NDEBUG
#include <string.h>
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

/* HeapInit:
 *
 */
void HeapInit (Options_t *opts)
{
    MaxNurserySzB = GetSizeOpt (opts, "-nursery", ONE_K, VP_HEAP_DATA_SZB/2);
    if (MaxNurserySzB < MIN_NURSERY_SZB)
	MaxNurserySzB = MIN_NURSERY_SZB;

    MajorGCThreshold = VP_HEAP_DATA_SZB / 10;
    if (MajorGCThreshold < MIN_NURSERY_SZB)
	MajorGCThreshold = MIN_NURSERY_SZB;

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
#ifndef NDEBUG
//    ToSpaceLimit = 16 * ONE_MEG;  /* FIXME: what should this be? */
    ToSpaceLimit = 8 * ONE_MEG;
#else
    ToSpaceLimit = 1024 * ONE_MEG;
#endif
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
	    vp->id, chunk->szB/1024, chunk->baseAddr,
	    chunk->baseAddr+chunk->szB, chunk->where);
#endif

}

/*! \brief Allocate a VProc's local memory object.
 */
VProc_t *AllocVProcMemory (int id, Location_t loc)
{
    assert (VP_HEAP_SZB >= BIBOP_PAGE_SZB);

    int nPages = 1;
    MutexLock (&HeapLock);
	VProc_t *vproc = (VProc_t *)AllocMemory (&nPages, VP_HEAP_SZB, nPages);
	if (vproc == 0) {
	    MutexUnlock (&HeapLock);
	    return 0;
	}
      /* allocate a BIBOP chunk descriptor for this object */
	MemChunk_t *chunk = NEW(MemChunk_t);
	if (chunk == (MemChunk_t *)0) {
	    MutexUnlock (&HeapLock);
	    return 0;
	}
	chunk->baseAddr = (Addr_t)vproc;
	chunk->szB = VP_HEAP_SZB;
	chunk->sts = VPROC_CHUNK(id);
	chunk->where = LocationNode(loc);
	UpdateBIBOP (chunk);
    MutexUnlock (&HeapLock);

#ifndef NDEBUG
    if (GCDebug > GC_DEBUG_NONE)
	SayDebug("     AllocVProcMemory(%d): %ld Kb at %p..%p (node %d)\n",
	    id, chunk->szB/1024, chunk->baseAddr,
	    chunk->baseAddr+chunk->szB, chunk->where);
#endif

    return vproc;

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

