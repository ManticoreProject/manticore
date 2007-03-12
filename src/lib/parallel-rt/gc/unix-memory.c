/* unix-memory.c
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Allocated BLOCK_SZB aligned memory chunks using mmap.  The public API for
 * this code is in os-memory.h.
 */

#include "manticore-rt.h"
#include <unistd.h>
#include <sys/mman.h>
#include <errno.h>
#include "os-memory.h"
#include "heap.h"

#define PROT_ALL        PROT_EXEC|PROT_READ|PROT_WRITE

static void *MapMemory (void *base, int *nPages, int blkSzB, int flags);
STATIC_INLINE void UnmapMemory (void *base, size_t szb)
{
    TotalVM -= szb;
    munmap (base, szb);
}

/* AllocMemory:
 *
 * Allocate nBlocks of blkSzB bytes (aligned on blkSzB boundary).  A
 * pointer to the memory is returned and nBlocks is set to the number
 * of allocated blocks.
 */
void *AllocMemory (int *nBlocks, int blkSzB)
{
    void	*memObj, *base;
 
  /* first we allocate the object and then check it for alignment */
    if ((memObj = MapMemory(0, nBlocks, blkSzB, 0)) == MAP_FAILED) {
	nBlocks = 0;
	return 0;
    }

    if (((Addr_t)memObj & (blkSzB-1)) == 0) {
      /* object is properly aligned */
	return memObj;
    }

  /* The object is not aligned, so free it up and try again with a
   * fixed address.
   */
    base = (void *)(((Addr_t)memObj & ~(blkSzB-1)) + blkSzB);
    UnmapMemory (base, *nBlocks * blkSzB);
    if ((memObj = MapMemory(base, nBlocks, blkSzB, MAP_FIXED)) == MAP_FAILED)
	return 0;
    else
	return memObj;

} /* end of AllocMemory */

/* FreeMemory:
 *
 * free a memory object allocated by AllocMemory (its size is 
 * szB bytes).
 */
void FreeMemory (void *base, int szB)
{
    UnmapMemory (base, szB);

} /* end of FreeMemory */

/* MapMemory:
 */
static void *MapMemory (void *base, int *nBlocks, int blkSzB, int flags)
{
    void	*memObj;

    do {
	size_t length = *nBlocks * blkSzB;
	memObj = mmap(base, length, PROT_ALL, MAP_PRIVATE|MAP_ANON|flags, 0, 0);
        if (memObj == MAP_FAILED) {
	    if (errno == ENOMEM) {
	      /* try a smaller request */
		(*nBlocks)--;
		continue;
	    }
	    else {
		*nBlocks = 0;
		return MAP_FAILED;
	    }
	}
	else {
	    TotalVM += length;
	    return memObj;
	}
    } while (*nBlocks > 0);

  /* here, we were unable to allocate any memory */
    *nBlocks = 0;
    return MAP_FAILED;

} /* end of MapMemory */
