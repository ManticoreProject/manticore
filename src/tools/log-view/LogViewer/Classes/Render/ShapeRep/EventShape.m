/*! \file EventShape.m
 \author Korei Klein
 \date 7/8/09

 Common superclass to all shape objects.
 Currently, EventShapes are to diverse to implement any common methods.
 All implementations throw exceptions.
*/

#import "Exceptions.h"
#import "EventShape.h"


@implementation EventShape

@synthesize description;
@synthesize color;

- (void)drawShape
{
	[Exceptions raise:@"EventShape: instances of eventShape must override drawShape"];
    return;
}

- (NSRect)bounds
{
    [Exceptions raise:@"EventShape: instances of eventShape must override bounds"];
    return NSZeroRect;
}

- (BOOL)containsPoint:(NSPoint)p;
{
	[Exceptions raise:@"EventShape: instances of eventShape must override containsPoint"];
    return YES;
}

- (shapeTag)kind
{
    [Exceptions raise:@"EventShape: instances of eventShape must override kind"];
    return STATE_SHAPE;
}


@end
