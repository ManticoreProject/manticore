/*! \file LogDoc.mm
 *
 * \author Korei Klein
 */

/*
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 */

#import "LogDoc.h"
#import "LogData.h"
#import "default-log-paths.h"
#import "Exceptions.h"
#import "LogView.h"
#import "OutlineViewDataSource.h"
#import "log-desc.hxx"
#import "TimeDisplay.h"
#import "DetailAccess.h"
#import "DetailInfoController.h"
#import "DetailInfoView.h"
#import "ShapeRep.h"
#import "GroupFilter.h"
#import "Summary.h"
#import "SummaryView.h"
#import "Box.h"

/* keep a cache of the log-file description structure.  Note that this
 * code will have to be changed if we ever want to support multiple
 * descriptions.
 */
static LogFileDesc *LFDCache = 0;

/// Name of the nib file which contains the detail info view
#define DETAIL_INFO_NIB_NAME ( @"DetailInfo" )

/// Name of the nib whose views this controller will manage
#define WINDOW_NIB_NAME ( @"LogDoc" )

/// Determines how much bigger/smaller the view get when zooming in/out
/// sqrt(10) is a fun ZOOM_FACTOR
#define DEFAULT_ZOOM_FACTOR ( 3.16227 )

/// Do not display events whose timespan (in floats) would be less than MIN_LOGINTERVAL_WIDTH
#define MIN_LOGINTERVAL_WIDTH ( 20 )


@implementation LogDoc

- (GroupFilter *)filter
{
    return outlineViewDataSource;
}

- (LogFileDesc *)logDesc
{
    return LFDCache;
}

- (IBAction)drewTicks:(LogView *)sender
{
    [timeDisplay drewTicks:sender];
}

/// Cause logView to display logData according to currently set parameters
- (void)flush
{
    [logView displayInterval:logInterval
		 atZoomLevel:[self zoomLevelForInterval:logInterval]
		 fromLogData:self.logData
		  filteredBy:self.filter];

    StateGroup *resourceState;
    {
	if (logData.allStates == NULL)
    	{
    	    [Exceptions raise:@"LogDoc: can't flush when logData has no allStates property"];
    	}
    	if (logData.allStates.count <= 0)
    	{
    	    [Exceptions raise:@"LogDoc: can't flush when logData's allStates has no first element"];
    	}
    	Box *b = [logData.allStates objectAtIndex:0];
	resourceState = (StateGroup *) [b unbox];
    }

   // NSLog(@"RESOURCE state is %s", resourceState->Desc());

#pragma mark Display Summary View

    CGFloat summary_view_column_width = DEFAULT_SUMMARY_VIEW_COLUMN_WIDTH;
    // FIXME only looks at the first vproc, really this initialization should set summary
    // to an average of all vprocs
    double viewWidth = scrollView.bounds.size.width;
    double scale = logInterval->width / viewWidth;
    summary = [Summary coarseSummaryFromLogData:logData
				       forState:resourceState
				       forVProc:0
				       withSize:scale *     summary_view_column_width
				       andStart:logInterval->x
				      andNumber:viewWidth / summary_view_column_width];
    assert (summaryViewTarget != nil);
    NSRect frame = summaryViewTarget.bounds;
    frame.size.width = viewWidth;



    SummaryView *oldSummaryView = summaryView;
    summaryView = [[SummaryView alloc] initWithFrame:frame
					  andSummary:summary
					 columnWidth:summary_view_column_width];

    if (summaryViewTarget.subviews.count == 0)
    {
	[summaryViewTarget addSubview:summaryView];
    }
    else
    {
	[summaryViewTarget replaceSubview:oldSummaryView with:summaryView];
    }

    summaryView.needsDisplay = true;

    logView.needsDisplay = true;
}
- (NSString *)filename
{
    return logData.filename;
}

- (void)setNilValueForKey:(NSString *)key
{
    if ([key isEqualToString:@"horizontalPosition"])
	self.horizontalPosition = 0;
    else
	[super setNilValueForKey:key];
}

- (void)setHorizontalPosition:(float)n
{
  //  NSLog(@"LogDoc is setting the horizontal position to %f", n);
    horizontalPosition = n;
    timeDisplay.needsDisplay = true;
}
- (float)horizontalPosition
{
 //   NSLog(@"LogDoc is returning the horizontal position");
    return horizontalPosition;
}

#pragma mark Synthesis
@synthesize timeDisplay;
@synthesize zoomFactor;
@synthesize logView;
@synthesize logData;
@synthesize outlineView;
@synthesize outlineViewDataSource;
@synthesize logInterval;
@synthesize enabled;


#pragma mark Initializations
+ (void)initialize
{
    LFDCache = LoadLogDesc(DEFAULT_LOG_EVENTS_PATH, DEFAULT_LOG_VIEW_PATH);
    if (LFDCache == 0)
    {
	[Exceptions raise:@"Could not load the two log description files"];
    }

}

- (LogDoc *)init
{
    if (![super init]) return nil;

    logData = nil;
   // NSLog(@"LogDoc: setting enabled to false");
    logInterval = nil;
    zoomFactor = DEFAULT_ZOOM_FACTOR;
    enabled = false;

    detailInfoController = nil;


    return self;
}

/** When logView is to display some portion of LogData for the first time
  * it needs to know what portion of the data to display.
  * Configure how that portion is to be computed by implementing initialLogInterval
  */
- (struct LogInterval *)initialLogInterval:(LogData *)logDataVal
{
    struct LogInterval *i = (LogInterval *) malloc(sizeof(struct LogInterval));
    i->x = 0;
    i->width = logDataVal.lastTime - logDataVal.firstTime;
    return i;
}

#pragma mark logData Initialization


- (BOOL)readFromURL:(NSURL *)absoluteURL ofType:(NSString *)typeName error:(NSError **)outError
{

    // Get the filename
    if (!absoluteURL.isFileURL)
    {
	[Exceptions raise:@"LogFile was asked to read data that was not from a file"];
    }
    NSString *filename = absoluteURL.absoluteString;
   // NSLog(@" URL filename %@", filename);
    filename = [filename substringFromIndex:16];
   // NSLog(@" actual filename is %@", filename);
    if (!filename)
    {
	[Exceptions raise:@"LogFile could not get a name for given fileWrapper"];
    }

    logData = [[LogData alloc] initWithFilename:filename
				 andLogFileDesc:self.logDesc];
    // Initialize logInterval according to the initialLogInterval configuration function
    self.logInterval = [self initialLogInterval:logData];


    outlineViewDataSource = [[OutlineViewDataSource alloc]
			     initWithLogDesc:self.logDesc
			     logDoc:self];
   // NSLog(@"LogDoc: setting enabled = true");
    enabled = true;

    return YES;
}


- (void)windowControllerDidLoadNib:(NSWindowController *)windowController
{
    [super windowControllerDidLoadNib:windowController];

    if (!logView) [Exceptions raise:@"LogDoc was not properly initialized with a logView"];
    if (!outlineView) [Exceptions raise:@"LogDoc was not properly initialized with a outlineView"];

#pragma mark tableColumns Initialization
    NSArray *columns = outlineView.tableColumns;
    int i = 0;
    for (NSTableColumn *column in columns)
    {
	if (i >= 2) [Exceptions raise:@"Too many columns in NSOutlineView"];
	
	column.identifier = [NSNumber numberWithInt:i];
	
	++i;
    }

    if (self.enabled)
    {
	if (!detailInfoTarget)
	{
	    [Exceptions raise:@"LogDoc: did not have an initiailized detailInfoTarget"];
	}
	LogFileDesc *lfd = self.logDesc;


	// Because some of the UI is created programmatically and some of the UI is created
	// in interface builder, it is necessary to do a small dance here to get things into the right places
	{
	    // Load the nib into memory and get its controller
	    detailInfoController = [[DetailInfoController alloc] initWithNibName:DETAIL_INFO_NIB_NAME
	    							      bundle:(NSBundle *)nil
								      logDesc:lfd];

	    // Get the detailInfoView, but do not display it yet
	    DetailInfoView *detailInfoView = detailInfoController.div;

	    // detailInfoTarget is a placeholder view. it is used as follows
	    // detailInfoTarget is created in interface builder as a custom view with nothing in it
	    // the only important properties of detailInfoTarget are
		    // 0. it is a view
		    // 1. its frame is the frame we would like to use for the detailInfoView
	    // as such, we now get rid of detailInfoTarget, and use its frame to initialize detailInfoView
	    // so that detailInfoView takes up the same space tha detailInfoTarget used to take up
	    NSRect newFrame = detailInfoTarget.frame;
	    //NSLog(@"newFrame is %f %f %f %f'",
	    //      newFrame.origin.x, newFrame.origin.y,
	    //      newFrame.size.width, newFrame.size.height);

	    [detailInfoView setFrame:newFrame];
	    [detailInfoTarget.superview addSubview:detailInfoView];
	    //NSLog(@"added view %@ to logView %@ in that frame", detailInfoView, logView);
	    detailInfoView.needsDisplay = true;
	    detailInfoView.autoresizingMask = NSViewHeightSizable | NSViewWidthSizable;
	}
	
	
	if (!outlineViewDataSource)
	{
	    NSLog(@"enabled state = %d", self.enabled);

	    [Exceptions raise:@"Did not have an initialized outlineViewDataSource while enabled"];
	}
	outlineView.dataSource = outlineViewDataSource;
	outlineView.delegate = outlineViewDataSource;
		
	[self flush];
	
	//NSLog(@"Log Doc has logInterval %qu, %qu, for bounds from %f to %f",
	//    logInterval->x, logInterval->width, logView.splitView.shapeBounds.origin.x,
	//      logView.splitView.shapeBounds.size.width);
	
	//NSLog(@"LogDoc is opening a drawer %@", drawer);
	[drawer open];
    }
}


#pragma mark NSDocument Settings

/// It is not possible to edit log files
- (BOOL)isDocumentEdited
{
    return NO;
}


- (NSData *)dataOfType:(NSString *)typeName error:(NSError **)outError
{
    [Exceptions raise:@"LogDoc can't write data"];
    return nil;
}

- (NSString *)windowNibName
{
    return WINDOW_NIB_NAME;
}

#pragma mark Zooming

/// The largest number of nanoseconds that can be displayed at deep zoom, given in uint64_t
#define MAX_DEEP_ZOOM_WIDTH ( -1 )
/// The largest number of nanoseconds that can be displayed at medium zoom
#define MAX_MEDIUM_ZOOM_WIDTH ( 10000000 )
- (enum ZoomLevel)zoomLevelForInterval:(struct LogInterval *)logIntervalVal
{
    uint64_t width = logIntervalVal->width;
    if (width < MAX_DEEP_ZOOM_WIDTH)
    {
	return zoomLevelDeep;
    }
    else if (width < MAX_MEDIUM_ZOOM_WIDTH)
    {
	return zoomLevelMedium;
    }
    else
    {
	return zoomLevelShallow;
    }
}

- (void)printLogInterval
{
   // NSLog(@"LogDoc->LogInterval = { %qu, %qu }", logInterval->x, logInterval->width);
}

/// Take a point in the logData to the corresponding point in the logView
- (CGFloat)image:(uint64_t)p
{
    NSRect shapeBounds = self.logView.splitView.shapeBounds;
    double scale = shapeBounds.size.width / (logInterval->width);
    return shapeBounds.origin.x + scale * (p - logInterval->x);
}

/// Take a point in the logView to the corresponding point in the logData
- (uint64_t)preImage:(CGFloat)p
{
    NSRect shapeBounds = logView.splitView.shapeBounds;
    double scale = logInterval->width / shapeBounds.size.width;
    return logInterval->x + scale * (p - shapeBounds.origin.x);
}

/// Horizontal midpoint of an NSRect
- (CGFloat)xMidPoint:(NSRect)r
{
    return r.origin.x + r.size.width / 2;
}


- (void)zoomBy:(double)scale aboutPivot:(uint64_t)pivot
{
   // NSLog(@"pivot = %qu", pivot);
    self.printLogInterval;
    logInterval->x = pivot - scale * (pivot - logInterval->x);
    logInterval->width = logInterval->width * scale;
    [self flush];
}
- (void)zoomBy:(double)scale
{
    uint64_t pivot = [self preImage:[self xMidPoint:logView.splitView.visibleRect]];
    [self zoomBy:scale aboutPivot:pivot];
}

- (void)zoomInAboutPivot:(uint64_t)pivot
{
    [self zoomBy:1 / zoomFactor aboutPivot:pivot];
}
- (void)zoomOutAboutPivot:(uint64_t)pivot
{
    [self zoomBy:1 * zoomFactor aboutPivot:pivot];
}



- (IBAction)zoomIn:(id)sender
{
    self.printLogInterval;
    double scale = 1 / zoomFactor;
    if (scale * logInterval->width < MIN_LOGINTERVAL_WIDTH)
    {
	NSLog(@"Not continuing to zoom.  Reached minimum width");
	return;
    }
    [self zoomBy:scale];
    self.printLogInterval;
}

- (IBAction)zoomOut:(id)sender
{
    self.printLogInterval;
    double scale = 1 * zoomFactor;
    [self zoomBy:scale];
    self.printLogInterval;
}



- (IBAction)zoom:(NSSegmentedControl *)sender
{
    NSInteger n = sender.selectedSegment;
    if (n == 0)
    {
	[self zoomOut:sender];
    }
    else if (n == 1)
    {
	[self zoomIn:sender];
    }
    else
    {
	[Exceptions raise:@"LogDoc: asked to zoom, but no segment of the sender is selected"];
    }
}

// For debugging purposes
uint64_t g_counter = 0;

- (BOOL)isInInterval:(Detail)d
{
    uint64_t fst = logInterval->x;
    uint64_t lst = logInterval->width + fst;
    Group *g = Detail_Type(d);
    event *c, *b;
    switch (g->Kind()) {
        case EVENT_GROUP: {
	    uint64_t a = Event_Time(*Detail_Simple_value(d));
	    return (fst <= a) && (a <= lst);
	  } break;
	case INTERVAL_GROUP:
	    c = Detail_Interval_start(d);
	    b = Detail_Interval_end(d);
	    if (c == NULL || b == NULL) return true;
	    if ((Event_Time(*c) > lst) || (Event_Time(*b) < fst))
		return false;
	    else
		return true;
	    break;
	case STATE_GROUP:
	    c = Detail_State_start(d);
	    b = Detail_State_end(d);
	    if (c == NULL && b == NULL) return true;
	    if (c == NULL) { // && b != NULL
		return Event_Time(*b) >= fst;
	    }
	    if (b == NULL) {// && c != NULL
		BOOL ret = Event_Time(*c) <= lst;
		if (ret) NSLog(@"Found a stategroup in interval, whose start is not in the interval");
		return ret;
	    }
	    if ((Event_Time(*c) > lst) || (Event_Time(*b) < fst))
		return false;
	    else
		return true;
	    break;
	case DEPENDENT_GROUP:
	    return false; /// XXX FIXME
    }

    //NSLog(@"g = %s, g->Kind() = %d", g->Desc(), g->Kind());
    ++g_counter;
    // int n = * ((int *)0);
    //NSLog(@"%d", n);
    [Exceptions raise:@"Controll should not reach here"];
    return false;
}


#pragma mark Detail Info

- (void)displayDetail:(EventShape *)d
{
    [detailInfoController displayDetail:d];
}


#pragma mark Printing

- (void)printShowingPrintPanel:(BOOL)showPanels
{
    NSLog(@"LogDoc is being asked to print");
    NSPrintOperation *op = [NSPrintOperation
			    printOperationWithView:scrollView
			    printInfo:[self printInfo]];
    op.showsPrintPanel = showPanels;

    [self runModalPrintOperation:op
			delegate:nil
		  didRunSelector:NULL
		     contextInfo:NULL];
}




@end





