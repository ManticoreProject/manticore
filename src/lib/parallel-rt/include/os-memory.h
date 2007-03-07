/* os-memory.h
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * An abstract interface to OS memory allocation/free mechanisms.
 */

#ifndef _OS_MEMORY_H_
#define _OS_MEMORY_H_

#include "manticore-rt.h"

/*! \brief allocate nBlocks of blkSzB bytes aligned on a blkSzB boundary.
 *
 * \param nBlocks the number of blocks to allocate.
 * \param blkSzB the size of a block (a power of 2)
 * \return the allocated object or 0 on failure.
 *
 * This function allocates a memory object that is at least (*nBlocks)*blkSzB
 * bytes in size.  The actual size of the object is returned in nBlocks.
 */
extern void *AllocMemory (int *nBlocks, int blkSzB);

/*! \brief free a memory object allocated by AllocMemory.
 *
 * \param base the object to be freed.
 * \param szB the size of the object in bytes.
 */
extern void FreeMemory (void *base, int szB);

#endif /* !_OS_MEMORY_H_ */
