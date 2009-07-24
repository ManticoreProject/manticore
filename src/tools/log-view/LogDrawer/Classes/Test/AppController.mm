/** \file AppController.mm
 * \author Korei Klein
 * \date 7/14/09
 *
 */

#import "AppController.h"
#import "log-desc.hxx"
#import "event-desc.hxx"
#import "LogView.h"
#import "VProc.hxx"
#import "LogFile.h"
#import "DynamicEventRep.hxx"
#import "default-log-paths.h"

#define BAND_HEIGHT ( 200 )

@implementation AppController



- (NSColor *)groupColor:(Group *)g
{
    return [NSColor redColor]; // FIXME
}

- (IBAction)open:(id)sender
{
    
}

- (IBAction)test:(id)sender
{
    /*
    scrollView.hasHorizontalRuler = YES;
    scrollView.rulersVisible = YES;
    logView.ruler = scrollView.horizontalRulerView;
    
    uint64_t fraction = 5; //< The number of times what we will display can fit into the whole log
    NSString *logEvents = [NSString stringWithCString:DEFAULT_LOG_EVENTS_PATH encoding:NSASCIIStringEncoding];
    NSString *logViewFile = [NSString stringWithCString:DEFAULT_LOG_VIEW_PATH encoding:NSASCIIStringEncoding];
    LogFile *lf = [[LogFile alloc] initWithFilename:@"/Users/koreiklein/workspace/primes-p4.mlg"
			       andLogEventsFilename:logEvents
				 andLogViewFilename:logViewFile];
    uint64_t ft = [lf firstTime];
    uint64_t lt = [lf lastTime];
    uint64_t width = (lt - ft) / fraction;
    [logView setStart:lt - width andWidth:width];
    [logView setLogFile:lf];
     */
    

}


@end
