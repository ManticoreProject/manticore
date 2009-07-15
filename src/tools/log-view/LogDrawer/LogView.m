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
        // Initialization code here.
    }
    return self;
}

- (void)drawRect:(NSRect)rect {
    // Drawing code here.
}

- (LogView *)initWithBands:(NSMutableArray *)bands
{
    if (![super init])
	return nil;
    
    for (BandView *band in bands)
    {
	[splitView addSubview:band];
    }
    return self;
}

@end
