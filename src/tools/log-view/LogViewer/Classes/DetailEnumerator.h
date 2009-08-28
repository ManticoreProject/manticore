//
//  DetailEnumerator.h
//  LogViewer
//
//  Created by Korei Klein on 8/14/09.
//  Copyright 2009 __MyCompanyName__. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import "LogData.h"
#import "Detail.h"
#import "VProc.h"
struct StateGroup;


@interface DetailEnumerator : NSObject {
    int cur_detail;
    struct StateGroup *group;
    Detail *details;
    int num_details;
}

/** Initializes this enumerator to enumerate all state events
 for the given vproc and groupval as read from logData
 */

- (DetailEnumerator *)initWithLogData:(LogData *)logDataVal
			     andVProc:(int32_t)vpVal
			     andGroup:(struct StateGroup *)groupVal;


/// \return the next event or NULL if there is none
- (Detail)next;

@end
