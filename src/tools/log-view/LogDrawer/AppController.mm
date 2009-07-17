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

#define BAND_HEIGHT ( 200 )

@implementation AppController



- (NSColor *)groupColor:(Group *)g
{
    return [NSColor redColor]; // FIXME
}



- (IBAction)test:(id)sender
{
    uint64_t fraction = 2500; //< The number of times what we will display can fit into the whole log
    NSString *root = @"/Users/koreiklein/workspace/manticore/trunk/src/tools/log-view/LogDrawer/";
    LogFile *lf = [[LogFile alloc] initWithFilename:[root stringByAppendingString:@"fib.mlg"]
		 andEventDescFilename:[root stringByAppendingString:@"event-view.json"]
		   andLogDescFilename:[root stringByAppendingString:@"log-events.json"]];
    uint64_t ft = [lf firstTime];
    uint64_t lt = [lf lastTime];
    [logView setStart:lt - (lt - ft) / fraction andEnd:lt];
    [logView setLogFile:lf];
}


@end
