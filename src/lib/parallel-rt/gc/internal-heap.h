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
    UNMAPPED_CHUNK		/*!< special status used for the dummy chunk that
				 *   represents unmapped regions of the memory space.
				 */
} Status_t;

#define VPROC_CHUNK(id)		((Status_t)((id) << 4) | VPROC_CHUNK_TAG)
#define IS_VPROC_CHUNK(sts)	(((sts)&0xF) == VPROC_CHUNK_TAG)

struct struct_chunk {
    Addr_t	baseAddr;	/*!< chunk base address */
    Addr_t	szB;		/*!< chunk size in bytes */
    Addr_t	usedTop;	/*!< [baseAddr..usedTop) is the part of the
				 *   chunk in use
				 */
    MemChunk_t	*next;		/*!< link field */
    Status_t	sts;		/*!< current status of chunk */
    int		where;		/*!< the node of the vproc that allocated
				 *   this chunk.
				 */
};

/********** Global heap **********/

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
extern MemChunk_t	**FreeChunks;	/* list of free chunks, one per node */

extern void UpdateBIBOP (MemChunk_t *chunk);

extern void FreeChunk (MemChunk_t *);

/* GC routines */
extern void InitGlobalGC ();
extern void StartGlobalGC (VProc_t *self, Value_t **roots);

/* GC debugging support */
#ifndef NDEBUG
typedef enum {
    GC_DEBUG_ALL	= 4,		/* all debug messages (including promotions) */
    GC_DEBUG_MINOR	= 3,
    GC_DEBUG_MAJOR	= 2,
    GC_DEBUG_GLOBAL	= 1,
    GC_DEBUG_NONE	= 0
} GCDebugLevel_t;

extern GCDebugLevel_t	GCDebug;	//!\brief Flag that controls GC debugging output
extern GCDebugLevel_t	HeapCheck;	//!\brief Flag that controls heap checking

#define GC_DEBUG_DEFAULT "major"	/* default level */
#define HEAP_DEBUG_DEFAULT "global"	/* default level */
#endif

#endif /* !_INTERNAL_HEAP_H_ */
