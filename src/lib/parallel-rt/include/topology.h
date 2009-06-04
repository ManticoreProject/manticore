/*! \file topology.h
 *
 * \author John Reppy
 *
 * An abstract interface to the 
 */

/*
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 */

#ifndef _TOPOLOGY_H_
#define _TOPOLOGY_H_

#include "manticore-rt.h"
#include "os-threads.h"

/* Location_t is defined in manticore-rt.h */

#define LOC_THREAD_BITS	8	//!< max 2^8 hardware threads per core
#define LOC_CORE_BITS	8	//!< max 2^8 cores per node
#define LOC_NODE_BITS	10	//!< max 2^10 nodes per system

/* globals */
extern int		NumHWNodes;	//!< \brief number of (possibly multicore) processors
extern int		NumHWCores;	//!< \brief total number of (possibly
					//!  mulithreaded) cores
extern int		NumHWThreads;	//!< \brief total number of hardware threads
extern int		NumCoresPerNode; //!< \brief number of cores per thread
extern int		NumThdsPerCore; //!< \brief number of threads per core
extern Location_t	*Locations;	//!< \brief an array of the available
					//!  locations indexed by processor ID

/*! \brief initialization function that determines the hardware topology.
 */
void DiscoverTopology ();

/*! \brief spawn an OS thread at the given location
 *  \param tidOut output parameter for the thread ID
 *  \param f the thread's main function
 *  \param arg the argument that is passed to \arg f
 *  \param loc the location to run the thread at
 *  \return true if the spawn is successful.
 */
bool SpawnAt (OSThread_t *tidOut, void * (*f)(void *), void *arg, Location_t loc);

STATIC_INLINE Location_t Location (int nd, int core, int thd)
{
    return ((nd << (LOC_THREAD_BITS+LOC_CORE_BITS)) | (core << LOC_THREAD_BITS) | thd);
}

/*! \brief return the node of a location */
STATIC_INLINE int LocationNode (Location_t loc)
{
    return (loc >> (LOC_THREAD_BITS+LOC_CORE_BITS));
}

/*! \brief return the logical processor ID of the location. */
STATIC_INLINE int LogicalId (Location_t loc)
{
/* FIXME: this works for Linux, but what about Mac OS X? */
    int id = LocationNode(loc);
    id = id * NumCoresPerNode + (loc >> LOC_THREAD_BITS) & ((1 << LOC_CORE_BITS) - 1);
    id = id * NumThdsPerCore + (loc & ((1 << LOC_THREAD_BITS) - 1));
    return id;
}

/*! \brief are two locations on the same node? */
STATIC_INLINE bool SameNodeLocation (Location_t loc1, Location_t loc2)
{
    return LocationNode(loc1) == LocationNode(loc2);
}

/*! \brief are two locations on the same core? */
STATIC_INLINE bool SameCoreLocation (Location_t loc1, Location_t loc2)
{
    return ((loc1 >> LOC_THREAD_BITS) == (loc2 > LOC_THREAD_BITS));
}

/*! \brief put a string representation of the location in the buffer.
 *  \param buf the string buffer
 *  \param bufSz the size of the buffer.
 *  \param loc the location.
 *  \return the number of characters (not counting the '\0')
 */
int LocationString (char *buf, size_t bufSz, Location_t loc);

#endif /* !_TOPOLOGY_H_ */
