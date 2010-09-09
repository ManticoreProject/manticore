/*! \file Utils.h
 \author Korei Klein
 \date 7/7/09
 */


/// Returns the euclidean distance between its arguments
CGFloat euclideanDistance(NSPoint a, NSPoint b);


/*!  Find the distance between a point and a line
 * \param p the point
 * \param start one point on the line
 * \param end another point on the line
 *
 * \return the distance between p and the line defined by start and end
 */
CGFloat pointToLineSegmentDistance(NSPoint p, NSPoint start, NSPoint end);
/// Determine if a is between b and c
BOOL between(CGFloat a, CGFloat b, CGFloat c);

#define DEFAULT_COLOR ( [NSColor redColor] )

/// Dummy utility class
/// We only keep this class around so we can import its static methods
@interface Utils : NSObject
{
}

+ (NSColor *)colorFromFormatString:(const char *)s;
+ (void *)calloc:(size_t)a size:(size_t)b;
+ (void *)realloc:(void *)a size:(size_t)b;
+ (NSString *)rectString:(NSRect)rect;

@end
