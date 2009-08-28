//
//  Summary.h
//  LogViewer
//
//  Created by Korei Klein on 8/13/09.
//  Copyright 2009 __MyCompanyName__. All rights reserved.
//

#import <Cocoa/Cocoa.h>
@class LogData;
struct StateGroup;


@interface Summary : NSObject {
    /// The pies,
    /// each representing how resource was used in an interval of length size.
    /// Pies is sorted
    NSMutableArray *pies;
    uint64_t size, start;
    
    /// Feel free to change the type of resource if the need arises
    struct StateGroup *resource;
}

/// Use this method for creating new summary data, do not call allocation and init functions
+ (Summary *)coarseSummaryFromLogData:(LogData *)logData
			     forState:(StateGroup *)state
			     forVProc:(int32_t)vp
			     withSize:(uint64_t)s
			     andStart:(uint64_t)st
			    andNumber:(uint64_t)n;


@property (readonly) NSMutableArray *pies;

/// Feel free to change the return type if desired
@property (readonly) struct StateGroup *resource;

@property (readonly) uint64_t size;
@property (readonly) uint64_t start;

/// For internal use mostly
+ (Summary *)fineSummaryFromLogData:(LogData *)logData
			   forState:(StateGroup *)state
			   forVProc:(int32_t)vp
			   withSize:(uint64_t)size
			   andStart:(uint64_t)st
			  andNumber:(uint64_t)n;

@end


