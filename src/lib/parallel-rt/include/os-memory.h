/* os-memory.h
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * An abstract interface to OS memory allocation/free mechanisms.
 *
 * NOTE: these functions should only be called when the HeapLock is held!
 */

#ifndef _OS_MEMORY_H_
#define _OS_MEMORY_H_

#include "manticore-rt.h"

/*! \brief allocate nBlocks of blkSzB bytes aligned on a blkSzB boundary.
 *
 * \param nBlocks the requested number of blocks to allocate.
 * \param blkSzB the size of a block (a power of 2)
 * \param minNumBlocks the minimum number of blocks required.
 * \return the allocated object or 0 on failure.
 *
 * This function allocates a memory object that is at least minNumBlocks*blkSzB
 * bytes in size.  The actual size of the object is returned in nBlocks.
 */
extern void *AllocMemory (int *nBlocks, int blkSzB, int minNumBlocks, void **unalignedBase);

/*! \brief free a memory object allocated by AllocMemory.
 *
 * \param base the object to be freed.
 * \param szB the size of the object in bytes.
 */
extern void FreeMemory (void *base, int szB);


// the functions below involving stacks do not require that the HeapLock is held.

// allocates a region of memory suitable for
// use as a stack. Note that it returns the base
// of the block so it can be freed, you must prepare
// the stack before use. Returns 0 if failed.
extern void* AllocStack(size_t numBytes);

// frees a stack allocated by AllocStack.
extern void FreeStack(void* base, size_t numBytes);

// Takes a region of memory from AllocStack and returns a pointer suitable
// for use as a stack pointer into that region of memory, ignoring any
// data already in the region.
//
// The pointer returned is guarenteed to be 16-byte aligned, and
// have the 8 bytes ahead of it available to write
// a value such as a return address.
// Here's a picture (where numBytes is approximate):
//
//                16-byte aligned
//                      v
// | guard |  numBytes  |bbbbbbbb |  high addresses >
// ^                    ^
// base ptr             returned ptr 
//
extern void* GetStackPtr(void* base, size_t numBytes);

#endif /* !_OS_MEMORY_H_ */
