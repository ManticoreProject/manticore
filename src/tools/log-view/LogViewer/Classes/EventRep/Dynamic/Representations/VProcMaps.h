/** \file  VProcMaps.h
 * \author Korei Klein
 * \date 7/31/09
 *
 */

#import <Cocoa/Cocoa.h>
#import "Detail.h"
struct Group;
struct StateGroup;
struct IntervalGroup;

@interface StateMap : NSObject {
    NSMutableDictionary *dict;
}

- (void)addDetail:(struct State_Detail *)d forStateGroup:(struct StateGroup *)g;
- (State_Detail *)getDetailForStateGroup:(struct StateGroup *)g;

@end

@interface IntervalMap : NSObject {
    NSMutableDictionary *dict;
}

- (void)addDetail:(struct Interval_Detail *)d forIntervalGroup:(struct IntervalGroup *)g;
- (Interval_Detail *)getDetailForIntervalGroup:(struct IntervalGroup *)g;

@end

@interface DependentMap : NSObject {
    NSMapTable *table;
}
- (NSArray *)toArray;
- (void)addDetail:(struct TaggedDetail_struct *)d forIdentifier:(uint64_t)i;
- (struct TaggedDetail_struct *)getDetailForIdentifier:(uint64_t)i;

@end



