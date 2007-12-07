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
    FREE_CHUNK,
    TO_SP_CHUNK,
    FROM_SP_CHUNK,
    VPROC_CHUNK_TAG,
} Status_t;

#define VPROC_CHUNK(id)		((Status_t)((id) << 4) | VPROC_CHUNK_TAG)

struct struct_chunk {
    Addr_t	baseAddr;	/* chunk base address */
    Addr_t	szB;		/* chunk size in bytes */
    Addr_t	usedTop;	/* [baseAddr..usedTop) is the part of the chunk in use */
    MemChunk_t	*next;		/* link field */
    Status_t	sts;		/* current status of chunk */
};

Mutex_t		HeapLock;	/* lock for protecting heap data structures */
MemChunk_t	*ToSpaceChunks; /* list of chunks in to-space */
MemChunk_t	*FromSpaceChunks; /* list of chunks is from-space */
MemChunk_t	*FreeChunks;	/* list of free chunks */

/* Get a memory chunk from the free list or by allocating fresh memory; the
 * size of the chunk will be HEAP_CHUNK_SZB bytes.  The chunk is added to the
 * to-space list.
 * NOTE: this function should only be called when the HeapLock is held.
 */
extern MemChunk_t *GetChunk ();

extern void UpdateBIBOP (MemChunk_t *chunk);

/* interface to the OS memory system */
extern MemChunk_t *AllocChunk (Addr_t szb);
extern void FreeChunk (MemChunk_t *);

#endif /* !_INTERNAL_HEAP_H_ */
