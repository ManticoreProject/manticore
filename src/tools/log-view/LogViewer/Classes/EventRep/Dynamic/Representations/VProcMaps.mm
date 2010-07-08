/** \file  VProcMaps.mm
  * \author Korei Klein
  * \date 7/31/09
  *
  *
  */

#import "VProcMaps.h"
#import "Box.h"
#import "log-desc.hxx"
#import "Exceptions.h"

/// This file converts so frequently between ascii strings and NSString that
/// it is convenient to abstract that work into a function call
NSString *asciiToNSString(const char *s)
{
    NSString *ret = [NSString stringWithCString:s encoding:NSASCIIStringEncoding];
    return ret;
}

@implementation StateMap

- (StateMap *)init
{
    if (![super init]) return nil;
    dict = [[NSMutableDictionary alloc] init];

    return self;
}
- (void)addDetail:(struct State_Detail *)d forStateGroup:(struct StateGroup *)g
{
    const char *s= g->Desc();
    NSString *S = asciiToNSString(s);
    [dict setObject:[Box box:d] forKey:S];
}
- (struct State_Detail *)getDetailForStateGroup:(struct StateGroup *)g
{
    const char *s= g->Desc();
    NSString *S = asciiToNSString(s);
    Box *b = [dict objectForKey:S];
    struct State_Detail *d = (State_Detail *) [b unbox];
    return d;
}

@end

@implementation IntervalMap

- (IntervalMap *)init
{
    if (![super init]) return nil;
    dict = [[NSMutableDictionary alloc] init];
    return self;
}
- (void)addDetail:(struct Interval_Detail *)d forIntervalGroup:(struct IntervalGroup *)g
{
    const char *s= g->Desc();
    NSString *S = asciiToNSString(s);
    [dict setObject:[Box box:d] forKey:S];
}
- (struct Interval_Detail *)getDetailForIntervalGroup:(struct IntervalGroup *)g
{
    const char *s= g->Desc();
    NSString *S = asciiToNSString(s);
    Box *b = [dict objectForKey:S];
    struct Interval_Detail *d = (Interval_Detail *) [b unbox];
    return d;
}

@end

@implementation DependentMap

- (DependentMap *)init
{
    if (![super init]) return nil;
    table = [[NSMutableDictionary alloc] init];
/*    table = [NSMapTable
	    mapTableWithKeyOptions:
		NSPointerFunctionsIntegerPersonality |
		NSPointerFunctionsOpaqueMemory
	    valueOptions:
		NSPointerFunctionsObjectPersonality |
		NSPointerFunctionsOpaqueMemory
	    ];
  */
    return self;
}
- (NSArray *)toArray
{
 //   NSLog(@"VProcMaps: converting to an array of dependents");
    NSMutableArray *ret = [[NSMutableArray alloc] init];
    NSEnumerator *objects = [table objectEnumerator];

    Box *b;
    while (b = [objects nextObject])
    {
	[ret addObject:b];
    }

    return ret;
}
- (void)addDetail:(struct TaggedDetail_struct *)d forIdentifier:(uint64_t)i
{
    NSString *S = [NSString stringWithFormat:@"%qu", i];
    [table setObject:[Box box:d] forKey:S];
}
- (struct TaggedDetail_struct *)getDetailForIdentifier:(uint64_t)i
{
    NSString *S = [NSString stringWithFormat:@"%qu", i];
    Box *b = (Box *)[table objectForKey:S];
    if (b == NULL) return NULL;
    struct TaggedDetail_struct *d = (TaggedDetail_struct *) [b unbox];
    return d;
}

@end

NSNumber *incr(NSNumber *n)
{
    NSNumber *ret;
    if (n == NULL)
    {
	[Exceptions raise:@"VprocMaps.mm: incr: can't increment a null number"];
    }
    else
    {
	int intValue = n.intValue;
	++intValue;
	ret = [NSNumber numberWithInt:intValue];
    }
    return ret;
}


@implementation DependentSizeMap

- (DependentSizeMap *)init
{
    if (![super init]) return nil;
    dict = [[NSMutableDictionary alloc] init];
    //table = [NSMapTable
	     //mapTableWithKeyOptions:NSPointerFunctionsOpaqueMemory | NSPointerFunctionsIntegerPersonality
	     //valueOptions:NSPointerFunctionsOpaqueMemory | NSPointerFunctionsObjectPersonality];
    return self;
}

- (void)incrementCountForIdentifier:(uint64_t)u
{
    NSNumber *key = [NSNumber numberWithUnsignedLongLong:u];
    NSNumber *n = [dict objectForKey:key];
    if (n == NULL) n = [NSNumber numberWithInt:0];

    [dict setObject:incr(n) forKey:key];
}

- (int)countForIdentifier:(uint64_t)u
{
    NSNumber *key = [NSNumber numberWithUnsignedLongLong:u];
    NSNumber *n = [dict objectForKey:key];
    if (n == NULL) return 0;
    else return n.intValue;
}

@end




