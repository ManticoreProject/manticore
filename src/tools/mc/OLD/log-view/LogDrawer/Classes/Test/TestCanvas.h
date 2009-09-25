/*! \file TestCanvas.h
 * \author Korei Klein
 * \date 7/8/09
 *
 * A simple canvas used to periodicaly test the application's drawing functions.
 */

#import <Cocoa/Cocoa.h>
#import "AppController.h"

/// Draws a few objects using the interfaces in ShapeRep.h
/*! This canvas is not set up the way that the logview will eventually be set up.
 Instead, it simply places a few shapes at some positions on its canvas.
 */
@interface TestCanvas : NSView {
/// Shapes that this TestCanvas will draw
    NSMutableArray *objects;
    IBOutlet AppController *appController;
}

@end
