/** \file  SummaryView.h
 * \author Korei Klein
 * \date 8/17/09
 *
 */

#import <Cocoa/Cocoa.h>
@class Summary;

#define DEFAULT_SUMMARY_VIEW_COLUMN_WIDTH ( 10 )

@interface SummaryView : NSView {
    Summary *summary;
    CGFloat width;
}



- (SummaryView *)initWithFrame:(NSRect)frame
		    andSummary:(Summary *)summary
		   columnWidth:(CGFloat)widthVal;

@end



