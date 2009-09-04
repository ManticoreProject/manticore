/*! \file EventShape.h
 \author Korei Klein
 \date 7/8/09

 Common superclass to all shape objects.
*/

#import <Cocoa/Cocoa.h>
#define TINY_WIDTH ( 1 )


typedef enum {
    SIMPLE_SHAPE,
    STATE_SHAPE,
    INTERVAL_SHAPE,
    MESSAGE_SHAPE
} shapeTag;

/// A superclass to shapes.
/*! After initialization, shapes all export sort of the same interface.
 * Methods of EventShape should encompass all this generic functionality.
 *
 * The event in the log file which corresponds to a shape is not generic
 * because certain shapes correspond to multiple events.
 */
@interface EventShape : NSObject {
    NSColor *color; //!< Color of the shape. not Null.
    NSString *description;
}


/// Draw the shape
- (void)drawShape;

/// Determine if a point lies within the area this shape is drawn on
- (BOOL)containsPoint:(NSPoint)p;

/// Each concrete instance of EventShape should implement kind differently to distinguish itself
- (shapeTag)kind;

@property (readwrite, assign) NSString *description;

@end
