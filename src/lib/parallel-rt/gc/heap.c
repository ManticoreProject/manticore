/* heap.c
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 */

#include "manticore-rt.h"
#include "heap.h"
#include "vproc.h"
#include "bibop.h"
#include "os-memory.h"
#include "os-threads.h"
#include "options.h"
#include "internal-heap.h"

Mutex_t		HeapLock;	/* lock for protecting heap data structures */

Addr_t		GlobalVM;	/* amount of memory allocated to Global heap (including */
				/* free chunks). */
Addr_t		FreeVM;		/* amount of free memory in free list */
Addr_t		TotalVM = 0;	/* total memory used by heap (including vproc local heaps) */
Addr_t		MaxNurserySzB;	/* limit on size of nursery in vproc heap */
Addr_t		MajorGCThreshold; /* when the size of the nursery goes below this limit */
				/* it is time to do a GC. */

static MemChunk_t *FreeChunks;	/* list of free chunks */

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
    SayDebug("HeapInit: max nursery = %d, threshold = %d\n", (int)MaxNurserySzB, (int)MajorGCThreshold);
#endif
  /* initialize the BIBOP */
#ifdef SIXTYFOUR_BIT_WORDS
    for (int i = 0;  i < L2_TBLSZ; i++)
	FreeL2Tbl[i] = 0;
    for (int i = 0;  i < L1_TBLSZ;  i++)
	BIBOP[i] = FreeL2Tbl;
#else
    for (int i = 0;  i < BIBOP_TBLSZ;  i++)
	BIBOP[i] = 0;
#endif

  /* initialize the heap data structures */
    MutexInit (&HeapLock);
    GlobalVM = 0;
    FreeVM = 0;
    FreeChunks = 0;
/* ??? */

} /* end of HeapInit */

/* InitVProcHeap:
 */
void InitVProcHeap (VProc_t *vp)
{
    MutexLock (&HeapLock);  /* can we do this inside AllocChunk? */

      /* provision the vproc with a to-space chunk in the global heap */
/* FIXME: eventually, we should check the free list first! */
	MemChunk_t *chunk = AllocChunk (HEAP_CHUNK_SZB);
	if (chunk == 0)
	    Die ("unable to allocate vproc to-space chunk\n");
	chunk->sts = VPROC_CHUNK(vp->id);
	vp->globToSpace = chunk;
	vp->globNextW = chunk->baseAddr + WORD_SZB;
	vp->globLimit = chunk->baseAddr + chunk->szB;

	UpdateBIBOP (chunk);

    MutexUnlock (&HeapLock);

}

MemChunk_t *AllocChunk (Addr_t szb)
{
  /* round size up to multiple of BIBOP pagesize */
    szb = ROUNDUP(szb, BIBOP_PAGE_SZB);
    int nPages = szb >> PAGE_BITS;

    void *memObj = AllocMemory(&nPages, BIBOP_PAGE_SZB);
    if (memObj == 0)
	return 0;

    MemChunk_t *chunk = NEW(MemChunk_t);
    if (chunk == 0)
	Die("unable to malloc memory\n");

    chunk->baseAddr = (Addr_t)memObj;
    chunk->szB = nPages * BIBOP_PAGE_SZB;
    chunk->next = 0;

#ifndef NDEBUG
    if (DebugFlg)
	SayDebug("AllocChunk: %ld Kb at %p\n", szb/1024, memObj);
#endif

    return chunk;

} /* end of AllocChunk */

/* UpdateBIBOP:
 *
 * Update the BIBOP to point to the freshly allocated chunk.
 */
void UpdateBIBOP (MemChunk_t *chunk)
{
    Addr_t addr = chunk->baseAddr;
    Addr_t top = addr + chunk->szB;
    while (addr < top) {
#ifdef SIXTYFOUR_BIT_WORDS
	MemChunk_t	**l2 = BIBOP[addr >> L1_SHIFT];
	assert (l2[(addr >> L2_SHIFT) & L2_MASK] == 0);
	if (l2 == FreeL2Tbl) {
	  /* we need to allocate a new L2 table for this range */
	    l2 = NEWVEC(MemChunk_t *, L2_TBLSZ);
	    for (int i = 0;  i < L2_TBLSZ;  i++)
		l2[i] = 0;
	    l2[(addr >> L2_SHIFT) & L2_MASK] = chunk;
	    BIBOP[addr >> L1_SHIFT] = l2;
	}
	else {
	    l2[(addr >> L2_SHIFT) & L2_MASK] = chunk;
	}
#else /* !SIXTYFOUR_BIT_WORDS */
	assert (BIBOP[addr >> PAGE_BITS] == 0);
	BIBOP[addr >> PAGE_BITS] = chunk;
#endif /* SIXTYFOUR_BIT_WORDS */
	addr += BIBOP_PAGE_SZB;
    } /* while */

}
