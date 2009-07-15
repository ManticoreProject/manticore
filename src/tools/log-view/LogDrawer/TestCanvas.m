/*! \file TestCanvas.m
 * \author Korei Klein
 * \date 7/8/09
 *
 * A simple canvas used to periodicaly test the application's drawing functions.
 */

#import "TestCanvas.h"
#import "ShapeRep.h"
#import "LogFile.h"


@implementation TestCanvas

- (id)initWithFrame:(NSRect)frame {
    self = [super initWithFrame:frame];
    if (self) {
	NSRect bounds = [self bounds];

	// add many things that can draw to the objects array
	objects = [[NSMutableArray alloc] init];
	[objects addObject:
		[[State alloc] initWithRect:NSMakeRect(100, bounds.origin.y, 300, bounds.size.height)]
	];
	[objects addObject:
		[[State alloc] initWithRect:NSMakeRect(0, bounds.origin.y, 100, bounds.size.height)
				      color:[NSColor orangeColor]
				      start:nil
					end:nil]
	];
	[objects addObject:
		[[Interval alloc] initWithRect:NSMakeRect(10, 10, 50, 20)]
	];
	[objects addObject:
		[[Interval alloc] initWithRect:NSMakeRect(250, 200, 120, 20)]
	];
	[objects addObject:
		[[Singleton alloc] initWithPoint:NSMakePoint(20, 100)
					   color:[NSColor yellowColor]
					   start:nil]
	];
	[objects addObject:
		[[Message alloc] initArrowFromPoint:NSMakePoint(0, 0)
					    toPoint:NSMakePoint(110, 100)]
	];
	/*
	[objects addObject:
	];
	[objects addObject:
	];
	*/
    }
    return self;
}

- (void)awakeFromNib
{
    NSString *root = @"/Users/koreiklein/workspace/manticore/trunk/src/tools/log-view/LogDrawer/";
    LogFile *lf = [[LogFile alloc] initWithFilename:[root stringByAppendingString:@"fact.mlg"]
		 andEventDescFilename:[root stringByAppendingString:@"event-view.json"]
		   andLogDescFilename:[root stringByAppendingString:@"log-events.json"]];
    NSLog(@" LogFile is \n%@",lf);
}

- (void)drawRect:(NSRect)rect {
	for (id obj in objects)
	{
		[obj drawShape];
	}
}

#pragma mark Mouse Events

- (void)mouseDown:(NSEvent *)event
{
	NSPoint p = [self convertPoint:[event locationInWindow] fromView:nil];
	for (id obj in objects)
	{
		if ([obj containsPoint:p])
		{
			NSLog(@"shape %@ received a mouse click", obj);
		}
	}
}

@end
