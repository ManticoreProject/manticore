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
extern int		NumHWCores;	//!< \brief number of (possibly mulithreaded) cores
extern int		NumHWThreads;	//!< \brief number of hardware threads
extern Location_t	*Locations;	//!< \brief an array of the availble locations

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

/*! \brief return the location of the current thread */
Location_t GetLocation ();

/*! \brief return the node of a location */
STATIC_INLINE int LocationNode (Location_t loc)
{
    return (loc >> (LOC_THREAD_BITS+LOC_CORE_BITS));
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

#endif /* !_TOPOLOGY_H_ */
