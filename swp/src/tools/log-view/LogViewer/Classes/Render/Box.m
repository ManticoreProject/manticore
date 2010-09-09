/** \file  Box.m
 * \author Korei Klein
 * \date 7/29/09
 *
 */

#import "Box.h"


@implementation Box

+ (Box *)box:(void *)valueVal
{
    Box *b = [[Box alloc] init];
    b.value = valueVal;
    return b;
}

- (void)setValue:(id)valueVal
{
    value = valueVal;
}


- (void *)unbox
{
    return value;
}

- (BOOL)isEqual:(Box *)e
{
    return ([e unbox] == value);
}

@end

@implementation EventGroupBox

+ (EventGroupBox *)box:(void *)valueVal withType:(enum EventGroupBoxT)type
{
    EventGroupBox *b = [[EventGroupBox alloc] init];
    b.value = valueVal;
    b.type = type;
    return b;
}

@synthesize type;

@end