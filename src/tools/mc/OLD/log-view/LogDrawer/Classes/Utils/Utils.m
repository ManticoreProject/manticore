/*! \file Utils.m
 \author Korei Klein
 \date 7/7/09
 */


#import <Foundation/Foundation.h>
#import "Utils.h"


BOOL between(CGFloat a, CGFloat b, CGFloat c)
{
	if (a <= b)
	{
		return a >= c;
	}
	else
	{
		return a <= c;
	}
}

CGFloat euclideanDistance(NSPoint a, NSPoint b)
{
	return sqrt( (b.x - a.x) * (b.x - a.x) +
		(b.y - a.y) * (b.y - a.y) );
}

CGFloat pointToLineSegmentDistance(NSPoint p, NSPoint start, NSPoint end)
{
	return
		fabs(  (end.x - start.x) * (start.y - p.y) -
		       (start.x - p.x)   * (end.y - start.y) )
		/ euclideanDistance(start, end);
}




@implementation Utils

+ (void)foo
{
    return;
}

+ (void)exn:(NSString *)s
{
    @throw [NSException exceptionWithName:@"Visualizer Exception"
				   reason:s
				 userInfo:nil];
    return;
}



@end


