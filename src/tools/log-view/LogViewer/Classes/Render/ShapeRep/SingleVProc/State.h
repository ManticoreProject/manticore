/*! \file State.h
    \author Korei Klein
    \date 7/7/09
*/

#import <Cocoa/Cocoa.h>
#import "EventShape.h"
#import "Detail.h"

/// Represents a state event by drawing a rectangle
@interface State : EventShape {
    NSRect rect; //!< Rectangle representing the event. Not null.
    event *start;
    event *end;
}



@property (readwrite, assign) NSRect rect;
@property (readonly) event *start;
@property (readonly) event *end;

///Initialize
/*! initialize
 \param r rectangle to draw
 \param c color of the rectangle
 \return the initialized object
 */
- (State *)initWithRect:(NSRect)r
		  color:(NSColor *)c
		  start:(event *)startVal
		    end:(event *)endVal;


@end
