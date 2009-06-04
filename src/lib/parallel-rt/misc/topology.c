/*! \file topology.c
 *
 * \author John Reppy
 *
 * This file contains an abstract interface to system-specific mechanisms for
 * managing the layout of vprocs onto hardware processors/cores/contexts.
 */

/*
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 */

#include "manticore-rt.h"
#ifdef HAVE_LIBNUMA
#  include "numa.h"
#endif
#if defined (TARGET_DARWIN)
#  include <sys/sysctl.h>
#endif
#include "topology.h"
#include "os-threads.h"
#include <errno.h>
#include <stdio.h>


/* globals */
int		NumHWNodes;	//!< \brief number of (possibly multicore) processors
int		NumHWCores;	//!< \brief number of (possibly mulithreaded) cores
int		NumHWThreads;	//!< \brief number of hardware threads
int		NumCoresPerNode; //!< \brief number of cores per thread
int		NumThdsPerCore; //!< \brief number of threads per core
Location_t	*Locations;	//!< \brief an array of the availble locations

/* local functions */
static bool GetNumCPUs ();


/*! \brief initialization function that determines the hardware topology.
 */
void DiscoverTopology ()
{
    if (! GetNumCPUs()) {
	Die ("unable to determine hardware topology");
    }

    Locations = NEWVEC(Location_t, NumHWThreads);
    if (Locations == 0) {
	Die("unable to allocation locations array");
    }
    for (int nd = 0, i = 0;  nd < NumHWNodes;  nd++) {
	for (int core = 0;  core < NumCoresPerNode;  core++) {
	    for (int thd = 0;  thd < NumThdsPerCore;  thd++) {
		Locations[i++] = Location(nd, core, thd);
	    }
	}
    }

#ifdef HAVE_LIBNUMA
    if (numa_available() >= 0) {
    }
#endif

#ifndef NDEBUG
    SayDebug ("%d nodes, %d cores, and %d threads\n",
	NumHWNodes, NumHWCores, NumHWThreads);
#endif

}

typedef struct {
    void		*arg;
    ThreadInitFn_t	init;
    Location_t		loc;
} LocArg_t;

/*! \brief initialize a thread at a given location */
static void *InitWithLocation (void *arg)
{
    LocArg_t *locArg = (LocArg_t *)arg;
    void *arg2 = locArg->arg;
    ThreadInitFn_t f = locArg->init;

#ifdef HAVE_SCHED_SETAFFINITY
    cpu_set_t	cpus;
    CPU_ZERO(&cpus);
    CPU_SET(LocationId(locArg->loc), &cpus);
    if (sched_setaffinity (0, sizeof(cpu_set_t), &cpus) == -1) {
	Warning("[%2d] unable to set affinity\n", initData->id);
    }
#endif

    FREE (arg);

    return f (arg2);

}

/*! \brief spawn an OS thread at the given location */
bool SpawnAt (OSThread_t *tidOut, ThreadInitFn_t f, void *arg, Location_t loc)
{
    LocArg_t *locArg = NEW(LocArg_t);
    locArg->arg = arg;
    locArg->init = f;
    locArg->loc = loc;

    return ThreadCreate (tidOut, InitWithLocation, locArg);

}

/*! \brief determine the number of CPUs */
static bool GetNumCPUs ()
{
#if defined(HAVE__PROC_CPUINFO)
  /* Get information from /proc/cpuinfo.  The interesting
   * fields are:
   *
   *	processor	: <id>		# logical processor id
   *	physical id	: <id>		# node id
   *	core id		: <id>		# core id (per node)
   *	cpu cores	: <n>		# number of cores per node
   */
    FILE *cpuinfo = fopen("/proc/cpuinfo", "r");
    char buf[1024];
    if (cpuinfo != NULL) {
	int maxProcId = 0, maxNodeId = 0, maxCoreId = 0, nCores = 0;
	while (fgets(buf, sizeof(buf), cpuinfo) != 0) {
	    int tmp;
	    if (sscanf(buf, "processor : %d", &tmp) == 1) {
		maxProcId = (tmp > maxProcId) ? tmp : maxProcId;
	    }
	    else if (sscanf(buf, "physical id : %d", &tmp) == 1) {
		maxNodeId = (tmp > maxNodeId) ? tmp : maxNodeId;
	    }
	    else if (sscanf(buf, "core id : %d", &tmp) == 1) {
		maxCoreId = (tmp > maxCoreId) ? tmp : maxCoreId;
	    }
	    else if (sscanf(buf, "cpu cores : %d", &tmp) == 1) {
		nCores = (tmp > nCores) ? tmp : nCores;
	    }
	}
	fclose (cpuinfo);

	NumHWNodes = maxNodeId + 1;
	NumHWCores = NumHWNodes * nCores;
	NumHWThreads = maxProcId + 1;
	NumCoresPerNode = nCores;
	NumThdsPerCore = NumHWThreads / NumHWCores;

	return true;
    }
    else {
	Warning("unable to determine the number of processors\n");
	return false;
    }
#elif defined(TARGET_DARWIN)
    size_t	len = sizeof(int);

  /* the number of nodes */
    if (sysctlbyname("hw.packages", &NumHWNodes, &len, 0, 0) < 0) {
	if (errno == ENOENT) {
	  // "hw.packages" is not known
	    NumHWNodes = 1;
	}
	else {
	    Warning("unable to determine the number of nodes\n");
	    return false;
	}
    }

  /* the number of cores */
    if (sysctlbyname("hw.physicalcpu", &NumHWCores, &len, 0, 0) < 0) {
	Warning("unable to determine the number of physical CPUs\n");
	return false;
    }

  /* the number of hardware threads */
    if (sysctlbyname("hw.logicalcpu", &NumHWThreads, &len, 0, 0) < 0) {
	if (errno == ENOENT) {
	  // "hw.packages" is not known
	    NumHWThreads = NumHWCores;
	}
	else {
	    Warning("unable to determine the number of logical CPUs\n");
	    return false;
	}
    }

    NumCoresPerNode = NumHWCores / NumHWNodes;
    NumThdsPerCore = NumHWThreads / NumHWCores;

    return true;
#else
    return false;
#endif

} /* end of GetNumCPUs */

/*! \brief put a string representation of the location in the buffer.
 *  \param buf the string buffer
 *  \param bufSz the size of the buffer.
 *  \param loc the location.
 *  \return the number of characters (not counting the '\0')
 */
int LocationString (char *buf, size_t bufSz, Location_t loc)
{
    int nd = LocationNode(loc);
    int core = (loc >> LOC_THREAD_BITS) & ((1 << LOC_CORE_BITS) - 1);

    if (NumThdsPerCore == 1)
	return snprintf (buf, bufSz, "%d:%d", nd, core);
    else {
	int thd = (loc & ((1 << LOC_THREAD_BITS) - 1));
	return snprintf (buf, bufSz, "%d:%d:%d", nd, core, thd);
    }

}
