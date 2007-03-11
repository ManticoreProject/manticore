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

extern MemChunk_t *AllocChunk (Addr_t szb);
extern void FreeChunk (MemChunk_t *);
extern void UpdateBIBOP (MemChunk_t *chunk);

#endif /* !_INTERNAL_HEAP_H_ */
