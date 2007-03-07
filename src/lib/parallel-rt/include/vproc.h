/* vproc.h
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 */

#ifndef _VPROC_H_
#define _VPROC_H_

#include <pthread.h>
#include "manticore-rt.h"
#include "os-threads.h"

struct struct_vproc {
    Word_t	inManticore;	/*!< true, when executing Manticore code */
    Word_t	atomic;		/*!< true, when in a vproc-atomic region */
    Word_t	sigPending;	/*!< true, when there is a pending signal */
			      /* VProc registers */
    Addr_t	allocPtr;	/*!< allocation pointer */
    Addr_t	limitPtr;	/*!< heap-limit pointer */
    OSThread_t	hostID;	      /*!< PThread ID of host */
			      /* GC parameters */
    Addr_t	allocBase;	/*!< base address of nursery */
    Addr_t	oldTop;		/*!< Old objects live in the space from the */
				/* heap base to the oldTop. */
    Addr_t	globNextW;	/*!< pointer to next word to allocate in */
				/* global heap */
    Addr_t	globLimit;	/*!< limit pointer for global heap */
};

/* Return the base address of the VProc's heap */
STATIC_INLINE Addr_t VProcHeap (VProc_t *vp)
{
    return ((Addr_t)vp) + sizeof(VProc_t);
}

/* the array of vprocs */
extern int		NumVProcs;
extern VProc_t		*VProcs[MAX_NUM_VPROCS];

typedef enum {
    GCSignal,
    PreemptSignal
} VPSignal_t;

extern VProc_t *VProcSelf ();
extern void VProcSignal (VProc_t *vp, VPSignal_t sig);

#endif /* !_VPROC_H_ */
