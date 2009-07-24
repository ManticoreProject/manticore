/** \file  MessageView.m
 * \author Korei Klein
 * \date 7/7/09
 */

#import "MessageView.h"


@implementation MessageView

- (id)initWithFrame:(NSRect)frame {
    self = [super initWithFrame:frame];
    if (self) {
        // Initialization code here.
    }
    return self;
}

- (void)drawRect:(NSRect)rect {
    // Drawing code here.
}


- (void)mouseDown:(NSEvent *)event
{
    NSLog(@"Message view received a mouse down");
    [self.superview mouseDown:event];
}

- (BOOL)isOpaque
{
    return NO;
}

@end
