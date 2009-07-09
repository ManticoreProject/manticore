/*! \file EventShape.m
 \author Korei Klein
 \date 7/8/09

 Common superclass to all shape objects.
 Currently, EventShapes are to diverse to implement any common methods.
 All implementations throw exceptions.
*/

#import "EventShape.h"


@implementation EventShape


- (void)drawShape
{
	@throw @"EventShape: instances of eventShape must override drawShape";
}

- (BOOL)containsPoint:(NSPoint)p;
{
	@throw @"EventShape: instances of eventShape must override containsPoint";
}



@end
