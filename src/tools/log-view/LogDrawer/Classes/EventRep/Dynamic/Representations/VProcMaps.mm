/** \file  VProcMaps.mm
  * \author Korei Klein
  * \date 7/31/09
  *
  *
  */

#import "VProcMaps.h"
#import "Box.h"

NSString *asciiToNSString(const char *s)
{
    NSString *ret = [NSString stringWithCString:s encoding:NSASCIIStringEncoding];
    ret = [NSString stringWithString:ret];
    return ret;
}

@implementation StateMap

- (StateMap *)init
{
    if (![super init]) return nil;
    dict = [[NSMutableDictionary alloc] init];
}
- (void)addDetail:(struct State_Detail *)d forStateGroup:(struct StateGroup *)g
{
    const char *s= g->Desc();
    NSString *S = asciiToNSString(s);
    [dict setObject:[Box d] forKey:S];
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

- (DependentMap *)init
{
    if (![super init]) return nil;
    dict = [[NSMutableDictionary alloc] init];
}
- (void)addDetail:(struct Interval_Detail *)d forIntervalGroup:(struct IntervalGroup *)g
{
    const char *s= g->Desc();
    NSString *S = asciiToNSString(s);
    [dict setObject:[Box d] forKey:S];
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
    table = [NSMapTable
	    mapTableWithKeyOptions:
		NSPointerFunctionsIntegerPersonality |
		NSPointerFunctionsOpaqueMemory
	    valueOptions:
		NSPointerFunctionsObjectPersonality |
		NSPointerFunctionsOpaqueMemory
	    ];
}
- (void)addDetail:(struct Dependent_Detail *)d forIdentifier:(uint64_t)i
{
    [table setObject:[Box d] forKey:i];
}
- (struct Dependent_Detail *)getDetailForIdentifier:(uint64_t)i
{
    Box *b = [table objectForKey:i];
    if (b == NULL) return NULL;
    struct Dependent_Detail *d = (Dependent_Detail *) [b unbox];
    return d;
}

@end



