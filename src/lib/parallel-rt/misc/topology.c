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


/* globals */
int		NumHWNodes;	//!< \brief number of (possibly multicore) processors
int		NumHWCores;	//!< \brief number of (possibly mulithreaded) cores
int		NumHWThreads;	//!< \brief number of hardware threads
Location_t	*Locations;	//!< \brief an array of the availble locations

/* local functions */
static int GetNumCPUs ();


/*! \brief initialization function that determines the hardware topology.
 */
void DiscoverTopology ()
{
    NumHardwareProcs = GetNumCPUs();

    Locations = (Locations_t *)malloc(NumHWThreads * sizeof(Location_t));
    if (Locations == 0) {
	Die("unable to allocation locations array");
    }

#ifdef HAVE_LIBNUMA
    if (numa_available() >= 0) {
    }
#endif

}

/*! \brief spawn an OS thread at the given location */
bool SpawnAt (OSThread_t *tidOut, void * (*f)(void *), void *arg, Location_t loc)
{
}

/*! \brief return a location ID for the current thread */
Location_t GetLocation ()
{
}

/*! \brief determine the number of CPUs */
static bool GetNumCPUs ()
{
#if defined(HAVE__PROC_CPUINFO)
  /* Get the number of hardware processors on systems that have /proc/cpuinfo */
    FILE *cpuinfo = fopen("/proc/cpuinfo", "r");
    char buf[1024];
    if (cpuinfo != NULL) {
	int n = 0;
	while (fgets(buf, sizeof(buf), cpuinfo) != 0) {
	    int id;
	    if (sscanf(buf, "processor : %d", &id) == 1)
		n++;
	}
	fclose (cpuinfo);
	NumCPUs = n;
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

    return false;
#else
    return false;
#endif

} /* end of GetNumCPUs */
