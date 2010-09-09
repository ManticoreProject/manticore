/** \file  GroupFilter.h
 * \author Korei Klein
 * \date 7/30/09
 *
 */

#import <Cocoa/Cocoa.h>
@class LogDoc;

/// Abstract class for objects which can be
/// used to filter groups
@interface GroupFilter : NSObject {
    IBOutlet LogDoc *logDoc;

}

@property (readwrite, assign) LogDoc *logDoc;

/** returns 0, 1, -1 iff details belonging to group g
 *  are  disabled, enabled, mixed
 *  Enabled details should be displayed
 *  Disabled details should not be displayed
 *  Something should be done with mixed details
 */
- (NSNumber *)enabled:(struct Group *)g;

@end
