/* large-object.h
 *
 * COPYRIGHT (c) 2019 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Fast, thread-safe memory management for large objects.
 * These objects can be efficiently allocated and freed by any VProc.
 */

#ifndef _LARGE_OBJECT_H_
#define _LARGE_OBJECT_H_

/**
 * Allocates memory in the context of the given vproc, returning
 * a pointer to the allocated region.
 */
void* lo_alloc(VProc_t *vp, size_t numBytes);

/**
 * Allocates memory in the context of the given vproc, returning
 * an _aligned_ pointer to the allocated region.
 */
void* lo_alloc_aligned(VProc_t *vp, size_t numBytes, size_t alignment);

/**
 * Frees any pointer returned by the lo_alloc functions.
 */
void lo_free(VProc_t *vp, void* ptr);

#endif
