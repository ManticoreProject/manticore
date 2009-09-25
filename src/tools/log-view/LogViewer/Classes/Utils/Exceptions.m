/** \file  Exceptions.m
 * \author Korei Klein
 * \date 7/22/09
 *
 */

#import "Exceptions.h"


@implementation Exceptions



+ (void)raise:(NSString *)s
{
    NSLog(@" ****** Uncaught Exception");
    NSLog(s);
    int n = * ( (int *) 0);
    NSLog(@"%d", n);
    return;
}

@end
