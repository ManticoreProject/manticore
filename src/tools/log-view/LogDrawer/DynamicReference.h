/** \file  DynamicReference.h
 * \author Korei Klein
 *
 */

#import <Cocoa/Cocoa.h>
#import "DynamicEventRep.hxx"

/// Indicate the type of reference being made
enum ReferenceTag {
    SRC;
    DSTS;
    START;
    END;
};

/// Contains the reference, access it based on the tag
typedef union Reference_union {
    DynamicEvent_struct *src; ///< For a Dependent Event which is a destination
    NSMutableArray *dsts; ///< An array of DynamicEvent *, each of which is a destination of this source
    DynamicEvent_struct *start; ///< For an Interval Event which is an end
    DynamicEvent_struct *end; ///< For an Interval Event which is a start
} Reference;


/// A reference object.  Represents some relationship this event has with some other events
@interface DynamicReference : NSObject {
    ReferenceTag tag; ///< The tag
    Reference ref;    ///< The data
}

@property (readwrite, assign) ReferenceTag tag;
@property (readwrite, assign) Reference ref;

@end
