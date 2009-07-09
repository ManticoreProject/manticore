/*! \file Structures.h
 *   Define structures to be used in shape representations
 *
 *  Created by Korei Klein on 7/7/09.
 *
 */


/// Pointer to corresponding event
typedef void *event;

/// Structure to represent an interval of time in floating point.
typedef struct _timeRange {
/// Point at which the TimeRange starts
    float x;
/// How long the TimeRange lasts for
    float width;
} TimeRange;

