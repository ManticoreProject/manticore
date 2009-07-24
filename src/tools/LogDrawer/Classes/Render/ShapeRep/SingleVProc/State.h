/*! \file State.h
    \author Korei Klein
    \date 7/7/09
*/

#import <Cocoa/Cocoa.h>
#import "EventShape.h"

/// Represents a state event by drawing a rectangle
@interface State : EventShape {
	NSRect rect; //!< Rectangle representing the event. Not null.
    
	/// The event corresponding to the begginig of the interval. Null for the first state.
	void *start; 
	State *end; //!< The event corresponding to the end of the interval. Not null.
}

/// Testing function. Generates a warning.
- (State *)initWithRect:(NSRect)r;

@property (readwrite, assign) void *start;
@property (readwrite, assign) State *end;
@property (readwrite, assign) NSRect rect;

///Initialize
/*! initialize
 \param r rectangle to draw
 \param c color of the rectangle
 \param s event marking the start of this interval
 \param e event marking the end of this interval

 \return the initialized object
 */
- (State *)initWithRect:(NSRect)r color:(NSColor *)c start:(void *)s;


@end
