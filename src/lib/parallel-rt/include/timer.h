/*! \file timer.h
 *
 * \author John Reppy
 */

/*
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 */

#ifndef _TIMER_H_
#define _TIMER_H_

#define MS_PER_SECOND		1000
#define US_PER_SECOND		(1000 * MS_PER_SECOND)
#define NS_PER_USECOND		1000
#define NS_PER_SECOND		(NS_PER_USECOND * US_PER_SECOND)

/* used to mark a stoped timer */
#define TIMER_STOPPED	0

#include "manticore-config.h"
#if defined(HAVE_MACH_ABSOLUTE_TIME)
#  include <mach/mach_time.h>
#elif defined(HAVE_CLOCK_GETTIME)
#  include <time.h>
#else
#  include <time.h>
#  include <sys/time.h>
#endif

/*! \brief a timer */
typedef struct {
    uint64_t	startTime;	//!< for a running timer, the time it started
    uint64_t	totalTime;	//!< total time recorded by this timer
} Timer_t;

/*! \brief return the current time in nanoseconds */
STATIC_INLINE uint64_t TIMER_Now ()
{
#if defined(HAVE_MACH_ABSOLUTE_TIME)
    return mach_absolute_time();
#elif defined(HAVE_CLOCK_GETTIME)
    struct timespec t;
    clock_gettime (CLOCK_REALTIME, &t);
    return (NS_PER_SECOND * (uint64_t)t.tv_sec) + (uint64_t)t.tv_nsec;
#else
    struct timeval t;
    gettimeofday (&t, 0);
    return (NS_PER_SECOND * (uint64_t)t.tv_sec) + (NS_PER_USECOND * (uint64_t)t.tv_usec);
#endif
}

/*! \brief initialize a timer */
STATIC_INLINE void TIMER_Init (Timer_t *t)
{
    t->startTime = TIMER_STOPPED;
    t->totalTime = 0;
}

/*! \brief start a timer */
STATIC_INLINE void TIMER_Start (Timer_t *t)
{
    assert (t->startTime == TIMER_STOPPED);
    t->startTime = TIMER_Now ();
}

/*! \brief stop a timer */
STATIC_INLINE void TIMER_Stop (Timer_t *t)
{
    assert (t->startTime != TIMER_STOPPED);
    t->totalTime += TIMER_Now() - t->startTime;
#ifndef NDEBUG
    t->startTime = TIMER_STOPPED;
#endif
}

/*! \brief return the recorded time in seconds */
STATIC_INLINE double TIMER_GetTime (Timer_t *t)
{
    return 1.0e-9 * (double)(t->totalTime);
}

#endif /* !_TIMER_H_ */
