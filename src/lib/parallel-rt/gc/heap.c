/* heap.c
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 */

#include "manticore-rt.h"
#include "heap.h"
#include "vproc.h"
#include "bibop.h"

typedef struct struct_chunk MemChunk_t;

typedef enum {
    FREE_CHUNK,
    TO_SP_CHUNK,
    FROM_SP_CHUNK,
    VPROC_CHUNK_TAG,
} Status_t;

#define VPROC_CHUNK(id)		((Status_t)((id) << 4) | VPROC_CHUNK_TAG)

struct struct_chunk {
    Addr_t	baseAddr;	/* chunk base address */
    Addr_t	szB;		/* chunk size in bytes */
    MemChunk_t	*next;		/* link field */
    Status_t	sts;		/* current status of chunk */
};

Addr_t		GlobalVM;	/* amount of memory allocated to Global heap (including */
				/* free chunks). */
Addr_t		FreeVM;		/* amount of free memory in free list */
Addr_t		TotalVM = 0;	/* total memory used by heap (including vproc local heaps) */

static MemChunk_t *FreeChunks;	/* list of free chunks */

#ifdef SIXTYFOUR_BIT_WORDS
MemChunk_t		**BIBOP[L1_TBLSZ];
static MemChunk_t	*FreeL2Tbl[L2_TBLSZ];
#else
MemChunk_t		**BIBOP[BIBOP_TBLSZ];
#endif

/* forward declarations */
static MemChunk_t *AllocChunk (Addr_t szb);


/* InitHeap:
 *
 * At this point, we assume that VProc initialization has already been
 * done, so the vproc-local heaps have been allocated.  Here we need to
 * allocate an initial collection of memory chunks for the global heap
 * and provide each vproc with a chunk for major collections.
 */
void InitHeap (Options_t *opts)
{

  /* initialize the BIBOP */
#ifdef SIXTYFOUR_BIT_WORDS
    for (int i = 0;  i < 1<<L2_TBLSZ; i++)
	FreeL2Tbl[i] = 0;
    for (int i = 0;  i < L1_TBLSZ;  i++)
	BIBOP[i] = L2FreeTbl;
#else
    for (int i = 0;  i < L1_TBLSZ;  i++)
	BIBOP[i] = 0;
#endif

  /* initialize the heap data structures */
    GlobalVM = 0;
    FreeVM = 0;
/* ??? */

  /* provision the VProcs with to-space chunks in the global heap */
    for (int i = 0;  i < NumVProcs;  i++) {
	MemChunk_t *chunk = AllocChunk (HEAP_CHUNK_SZB);
	if (chunk == 0)
	    Die ("unable to allocate vproc to-space chunk\n");
	chunk->sts = VPROC_CHUNK(i);
	VProcs[i].globToSpace = chunk;
	VProcs[i].globNextW = chunk->baseAddr + WORD_SZB;
	VProcs[i].globLimit = chunk->baseAddr + chunk->szB;
    }

} /* end of InitHeap */

static MemChunk_t *AllocChunk (Addr_t szb)
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

    chunk->addr = (Addr_t)memObj;
    chunk->szB = nPages * BIBOP_PAGE_SZB;
    chunk->next = 0;

} /* end of AllocChunk */
