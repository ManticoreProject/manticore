/** \file  SummaryView.h
 * \author Korei Klein
 * \date 8/17/09
 *
 */

#import "LogDoc.h"
@class Summary;

/// Defines the default amount of horizontal screen space to be used to draw each pie
#define DEFAULT_SUMMARY_VIEW_COLUMN_WIDTH ( 2 )

/// A view object to display the data associated with a summary
@interface SummaryView : NSView {
    /// Associated Summary from which the data will come
    Summary *summary;
    /// Actualy amount of horizontal screen space to be used to draw each pie
    CGFloat width;
    
    struct LogInterval *hilightInterval;
}


@property Summary *summary;
@property CGFloat width;
@property struct LogInterval *hilightInterval;

@end



