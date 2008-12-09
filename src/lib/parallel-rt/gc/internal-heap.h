/* internal-heap.h
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Internal heap data structures.
 */

#ifndef _INTERNAL_HEAP_H_
#define _INTERNAL_HEAP_H_

#include "manticore-rt.h"
#include "heap.h"

typedef enum {
    FREE_CHUNK,			/*!< chunk that is available for allocation */
    TO_SP_CHUNK,		/*!< to-space chunk in the global heap */
    FROM_SP_CHUNK,		/*!< from-space chunk in the global heap */
    VPROC_CHUNK_TAG,		/*!< low four bits of VProc chunk (see #VPROC_CHUNK) */
    UNMAPPED_CHUNK		/*!< special status used for the dummy chunk that represents
				 *   unmapped regions of the memory space.
				 */
} Status_t;

#define VPROC_CHUNK(id)		((Status_t)((id) << 4) | VPROC_CHUNK_TAG)

struct struct_chunk {
    Addr_t	baseAddr;	/*!< chunk base address */
    Addr_t	szB;		/*!< chunk size in bytes */
    Addr_t	usedTop;	/*!< [baseAddr..usedTop) is the part of the chunk in use */
    MemChunk_t	*next;		/*!< link field */
    Status_t	sts;		/*!< current status of chunk */
};

/********** Global heap **********/

#define HEAP_CHUNK_SZB		((Addr_t)(4*ONE_MEG))

extern Mutex_t		HeapLock;	/* lock for protecting heap data structures */
extern Addr_t		GlobalVM;	/* amount of memory allocated to Global heap */
					/*  (includingfree chunks). */
extern Addr_t		FreeVM;		/* amount of free memory in free list */
extern Addr_t		ToSpaceSz;	/* amount of memory being used for to-space */
extern Addr_t		ToSpaceLimit;	/* if ToSpaceSz exceeds this value, then do a */
					/* global GC */
extern Addr_t		TotalVM;	/* total memory used by heap (including vproc */
					/* local heaps) */
extern MemChunk_t	*FromSpaceChunks; /* list of chunks is from-space */
extern MemChunk_t	*FreeChunks;	/* list of free chunks */
extern bool		GlobalGCInProgress; /* true, when a global GC has been initiated */

/* Get a memory chunk from the free list or by allocating fresh memory; the
 * size of the chunk will be HEAP_CHUNK_SZB bytes.  The chunk is added to the
 * to-space list.
 * NOTE: this function should only be called when the HeapLock is held.
 */
extern void GetChunkForVProc ();

extern void UpdateBIBOP (MemChunk_t *chunk);

extern void FreeChunk (MemChunk_t *);

/* GC routines */
extern void InitGlobalGC ();
extern void StartGlobalGC (VProc_t *self, Value_t **roots);

#endif /* !_INTERNAL_HEAP_H_ */
