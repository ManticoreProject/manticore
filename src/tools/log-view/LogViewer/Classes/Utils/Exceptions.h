/** \file  Exceptions.h
 * \author Korei Klein
 * \date 7/22/09
 *
 * Small Utility module to allow for a briefer syntax for raising exceptions.
 */

#import <Cocoa/Cocoa.h>


/// Dummy object to hold exception method
@interface Exceptions : NSObject {

}

/// Raise an exception described by the given string
+ (void)raise:(NSString *)s;

@end
