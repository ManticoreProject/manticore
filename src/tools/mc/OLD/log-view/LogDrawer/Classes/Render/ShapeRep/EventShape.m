/*! \file EventShape.m
 \author Korei Klein
 \date 7/8/09

 Common superclass to all shape objects.
 Currently, EventShapes are to diverse to implement any common methods.
 All implementations throw exceptions.
*/

#import "Exceptions.h"
#import "EventShape.h"
#import "Utils.h"


@implementation EventShape


- (void)drawShape
{
	[Exceptions raise:@"EventShape: instances of eventShape must override drawShape"];
    return;
}

- (BOOL)containsPoint:(NSPoint)p;
{
	[Exceptions raise:@"EventShape: instances of eventShape must override containsPoint"];
    return YES;
}



@end
