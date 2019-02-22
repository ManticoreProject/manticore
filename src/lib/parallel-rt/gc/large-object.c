/* large-object.c
 *
 * COPYRIGHT (c) 2019 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Fast, thread-safe memory management for large objects.
 * These objects can be efficiently allocated and freed by any VProc.
 */

#include "manticore-rt.h"
#include "large-object.h"

#include <stdlib.h>

// NOTE: For now, we assume C's stdlib is fast enough.
// This may change in the future, so we require the VProc to be provided.

void* lo_alloc(VProc_t *vp, size_t numBytes) {
  #ifndef NO_GC_STATS
    vp->nLargeObjs += 1;
    vp->largeObjStats.nBytesAlloc += numBytes;
  #endif

  return malloc(numBytes);
}

void* lo_alloc_aligned(VProc_t *vp, size_t numBytes, size_t alignment) {
  assert(numBytes % alignment == 0); // must be a multiple.
  #ifndef NO_GC_STATS
    vp->nLargeObjs += 1;
    vp->largeObjStats.nBytesAlloc += numBytes;
  #endif
  return aligned_alloc(alignment, numBytes);
}

void lo_free(VProc_t *vp, void* ptr) {
  free(ptr);
}
