/** \file  LogView.m
 * \author Korei Klein
 * \date 7/7/09
 *
 */

#import "LogView.h"


@implementation LogView

@synthesize start;
@synthesize end;

- (id)initWithFrame:(NSRect)frame {
    self = [super initWithFrame:frame];
    if (self) { 
        splitView = [[CustomSplitView alloc] initWithFrame:[self bounds]];
	messageView = [[MessageView alloc] initWithFrame:[self bounds]];
	[self addSubview:splitView];
	[self addSubview:messageView];
	start = 0;
	end = 2000;
     
    }
    return self;
}

- (void)drawRect:(NSRect)rect {
    // Drawing code here.
    NSLog(@"LogView is asked to draw a rect");
    [[NSColor blueColor] set];
    [NSBezierPath fillRect:[self bounds]];
}

- (void)acquireBands:(NSMutableArray *)bands
{    
    for (BandView *band in bands)
    {
	[splitView addSubview:band];
    }
    [splitView adjustSubviews];
}

@end
