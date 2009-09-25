/** \file LogDataUtils.h
 * \author Korei Klein
 * \date 8/24/09
 *
 */

#import <stdlib.h>

typedef struct Detail_Size_Info
{
	uint64_t current;
	size_t stateGroups;
	size_t intervalGroups;
} Detail_Size_Info;

/// precondition: info points to an allocated data structure, which may contain garbage.
/// postcondition: info poins to an allocated data structure which contains info for a detail of size 0
void init_detail_size_info(struct Detail_Size_Info *info)
{
    info->current = 0;
    info->stateGroups = 0;
    info->intervalGroups = 0;
}

/// increment the Detail_Size_Info structure to reflect the fact that there are now more
/// stateGroups and intervalGroups
void detail_size_add(struct Detail_Size_Info *info, int stateGroups, int intervalGroups)
{
    info->stateGroups += stateGroups;
    info->intervalGroups += intervalGroups;
}


uint64_t detail_size_current(struct Detail_Size_Info *info)
{
    return info->current;
}

void detail_size_incr_current(struct Detail_Size_Info *info)
{
    info->current++;
}


size_t detail_size_total(struct Detail_Size_Info *info)
{
    size_t res = 0;

    res += info->stateGroups;
    res += info->intervalGroups;

    return res;
}







// Structure to contain the maps that a single vproc will use to connect pairs of
// events in the logfile with each other to create details

typedef struct VProc_Maps
{
    StateMap *stateMap;
    IntervalMap *intervalMap;
} VProc_Maps;

// precondition: maps points to an allocated data structure, which may contain garbage
// postcondition: maps points to an allocated data structure, which contains empty freshly initialized maps
void init_vproc_maps(struct VProc_Maps *maps)
{
    maps->stateMap = [[StateMap alloc] init];
    maps->intervalMap = [[IntervalMap alloc] init];
}


