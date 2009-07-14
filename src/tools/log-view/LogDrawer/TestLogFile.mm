/** \file  TestLogFile.m
 * \author Korei Klein
 * \date 7/10/09
 */

#import "TestLogFile.h"
#import "DynamicRep.hxx"


@implementation TestLogFile
    




- (IBAction)loadLog:(id)sender
{
    [[LogFile alloc] initWithFilename:@"fact.mlg"
		 andEventDescFilename:@"event-view.json"
		   andLogDescFilename:@"log-events.json"];
}
@end
