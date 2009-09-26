/*! \file Utils.m
 \author Korei Klein
 \date 7/7/09
 */


#import <Foundation/Foundation.h>
#import "Utils.h"
#import "Exceptions.h"


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


#pragma mark Colors

+ (NSColor *)colorFromFormatString:(const char *)s
{
    if (s == NULL)
    {
	NSLog(@"***** BandView.mm: a color in log-view.json was uninitialized.\n\
	      \t\tReturning a default color instead");
	return DEFAULT_COLOR;
    }
    
    uint32_t r, g, b, a;
    
    int nItems = sscanf(s, "#%2x%2x%2x%2x", &r, &g, &b, &a);
    if (nItems == 3) {
        a = 255;
    }
    else if (nItems != 4) {
	[Exceptions raise:@"BandView.mm: sscanf failure"];
    }
    
    CGFloat R, G, B, A;
    R = (CGFloat)r / 255.0;
    G = (CGFloat)g / 255.0;
    B = (CGFloat)b / 255.0;
    A = (CGFloat)a / 255.0;
    
    // NSLog(@"returning a new color %f %f %f %f", R, G, B, A);
    
    NSColor *ret = [NSColor colorWithCalibratedRed:R green:G blue:B alpha:A];
    return ret;
}

#pragma mark Allocators

+ (void *)calloc:(size_t)a size:(size_t)b
{
    void * ret = calloc(a, b);
    if (ret == NULL)
    {
	[Exceptions raise:@"Utils: calloc: failed to callocate memory"];
    }
    return ret;
}
+ (void *)realloc:(void *)a size:(size_t)b
{
    NSLog(@" ************ warning: realloc is slow.  You shouldn't use it.");
    void * ret = realloc(a, b);
    if (ret == NULL)
    {
	[Exceptions raise:@"Utils: realloc: failed to reallocate memory"];
    }
    return ret;
}


@end
