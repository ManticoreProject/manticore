//
//  MessageView.h
//  Visualizer
//
//  Created by Korei Klein on 7/7/09.
//  Copyright 2009 __MyCompanyName__. All rights reserved.
//

#import <Cocoa/Cocoa.h>

/// Draws message arrows and other foreground events
/** The MessageView will be placed on top of the BandViews in a LogView.
 This way, it will be able to draw shapes that appear on top of all the bands.
 Currently, it is usefull for drawing message arrows.
 It may need to draw more foreground shapes later.
 */
@interface MessageView : NSView {

}

@end
