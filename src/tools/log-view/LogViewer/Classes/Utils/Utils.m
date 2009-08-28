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
    
    double scale_max = 255.0;
    uint32_t r, g, b, a;
    
    char *format = "#%2x%2x%2x%2x";
    if (sscanf(s, format, &r, &g, &b, &a) != 4)
    {
	[Exceptions raise:@"BandView.mm: sscanf failure"];
    }
    
    CGFloat R, G, B, A;
    R = r / scale_max;
    G = g / scale_max;
    B = b / scale_max;
    A = a / scale_max;
    
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
    void * ret = realloc(a, b);
    if (ret == NULL)
    {
	[Exceptions raise:@"Utils: realloc: failed to reallocate memory"];
    }
    return ret;
}


@end
