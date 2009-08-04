/** \file  LogDoc.m
 * \author Korei Klein
 * \date 730/09
 *
 */

#import "LogDoc.h"
#import "LogData.h"
#import "default-log-paths.h"
#import "Exceptions.h"
#import "LogView.h"
#import "OutlineViewDataSource.h"
#import "log-desc.hxx"

#define DEFAULT_ZOOM_FACTOR ( 1.2 )

@implementation LogDoc

static LogFileDesc *logDesc;
- (LogFileDesc *)logDesc
{
    return logDesc;
}

/// Cause logView to display logData according to
- (void)flush
{
    [logView displayInterval:logInterval
		 atZoomLevel:[self zoomLevelForInterval:logInterval]
		 fromLogData:self.logData
		  filteredBy:outlineViewDataSource];
    logView.needsDisplay = true;
}
- (NSString *)filename
{
    return logData.filename;
}

#pragma mark Synthesis
@synthesize zoomFactor;
@synthesize logView;
@synthesize logData;
@synthesize outlineView;
@synthesize outlineViewDataSource;
@synthesize logInterval;
@synthesize logDesc;
@synthesize enabled;


#pragma mark Initializations
+ (void)initialize
{
    logDesc = LoadLogDesc(DEFAULT_LOG_EVENTS_PATH, DEFAULT_LOG_VIEW_PATH);
    if (logDesc == NULL)
    {
	[Exceptions raise:@"Could not load the two log description files"];
    }
    
}

- (LogDoc *)init
{
    if (![super init]) return nil;
    
    logData = nil;
    NSLog(@"LogDoc: setting enabled to false");
    logInterval = nil;
    zoomFactor = DEFAULT_ZOOM_FACTOR;
    enabled = false;
    
    
    return self;
}

- (struct LogInterval *)initialLogInterval:(LogData *)logDataVal
{
    struct LogInterval *i = (LogInterval *) malloc(sizeof(struct LogInterval));
    i->x = logDataVal.firstTime;
    i->width = logDataVal.lastTime - i->x;
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
    NSLog(@" URL filename %@", filename);
    filename = [filename substringFromIndex:16];
    NSLog(@" actual filename is %@", filename);
    if (!filename)
    {
	[Exceptions raise:@"LogFile could not get a name for given fileWrapper"];
    }
    
    logData = [[LogData alloc] initWithFilename:filename
				 andLogFileDesc:logDesc];
    // Read the default interval into logDoc
    self.logInterval = [self initialLogInterval:logData];

    
    outlineViewDataSource = [[OutlineViewDataSource alloc]
			     initWithLogDesc:logDesc
			     logDoc:self];
    NSLog(@"LogDoc: setting enabled = true");
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
	if (!outlineViewDataSource)
	{
	    [Exceptions raise:@"Did not have an initialized outlineViewDataSource while enabled"];
	}
	outlineView.dataSource = outlineViewDataSource;
		
	[self flush];
	
	NSLog(@"Log Doc has logInterval %qu, %qu, for bounds from %f to %f",
	    logInterval->x, logInterval->width, logView.splitView.shapeBounds.origin.x,
	      logView.splitView.shapeBounds.size.width);
    }
}


#pragma mark NSDocument Settings

/// It is not possible to edit log files
- (BOOL)isDocumentEdited
{
    return NO;
}
- (BOOL)applicationShouldOpenUntitledFile:(NSApplication *)sender
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
    return @"LogDoc";
}

#pragma mark Zooming

/// The largest number of nanoseconds that can be displayed at deep zoom
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

- (CGFloat)xMidPoint:(NSRect)r
{
    return r.origin.x + r.size.width / 2;
}

- (void)zoomBy:(double)scale
{
    uint64_t pivot = [self preImage:[self xMidPoint:logView.splitView.bounds]];
    self.logInterval->x = pivot - scale * (pivot - logInterval->x);
    self.logInterval->width = logInterval->width * scale;
}

- (IBAction)zoomIn:(id)sender
{
    double scale = logInterval->width / zoomFactor;
    [self zoomBy:scale];
}

- (IBAction)zoomOut:(id)sender
{
    double scale = logInterval->width * zoomFactor;
    [self zoomBy:scale];
}


@end
