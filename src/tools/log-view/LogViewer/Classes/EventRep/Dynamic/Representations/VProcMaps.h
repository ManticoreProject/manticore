/** \file  VProcMaps.h
 * \author Korei Klein
 * \date 7/31/09

 Contains various classes which serve as maps from one kind of data to another
 The can't all simply be dictionaries, because dictionaries only work on NSObjects
 and many of the keys and values of the maps are not NSObjects.
 Thus these maps identify each non-NSObject key or value set with some set
 of NSObjects, usually strings, which they can use in the underlying map
 *
 */

#import <Cocoa/Cocoa.h>
#import "Detail.h"
struct Group;
struct StateGroup;
struct IntervalGroup;



/// Map stateGroups to details
@interface StateMap : NSObject {
    NSMutableDictionary *dict;
}

- (void)addDetail:(struct State_Detail *)d forStateGroup:(struct StateGroup *)g;
- (State_Detail *)getDetailForStateGroup:(struct StateGroup *)g;

@end

/// Map intervalGroups to details
@interface IntervalMap : NSObject {
    NSMutableDictionary *dict;
}

- (void)addDetail:(struct Interval_Detail *)d forIntervalGroup:(struct IntervalGroup *)g;
- (Interval_Detail *)getDetailForIntervalGroup:(struct IntervalGroup *)g;

@end

/// Map dependentGroups to details
@interface DependentMap : NSObject {
    NSMapTable *table;
}
- (NSArray *)toArray;
- (void)addDetail:(struct TaggedDetail_struct *)d forIdentifier:(uint64_t)i;
- (struct TaggedDetail_struct *)getDetailForIdentifier:(uint64_t)i;

@end

/// Map the identifiers of dependent events to some number,
/// and allow incrementing of that number
@interface DependentSizeMap : NSObject {
    NSMutableDictionary *dict;
}
- (void)incrementCountForIdentifier:(uint64_t)i;
- (int)countForIdentifier:(uint64_t)i;

@end



