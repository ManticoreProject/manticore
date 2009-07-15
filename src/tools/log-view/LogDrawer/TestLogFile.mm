/** \file  TestLogFile.m
 * \author Korei Klein
 * \date 7/10/09
 */

#import "TestLogFile.h"
#import "DynamicRep.hxx"


@implementation TestLogFile
    




- (IBAction)loadLog:(id)sender
{
    NSString *root = @"/Users/koreiklein/workspace/manticore/trunk/src/tools/log-view/LogDrawer/";
    [[LogFile alloc] initWithFilename:[root stringByAppendingString:@"fact2.mlg"]
		 andEventDescFilename:[root stringByAppendingString:@"event-view.json"]
		   andLogDescFilename:[root stringByAppendingString:@"log-events.json"]];
}
@end
