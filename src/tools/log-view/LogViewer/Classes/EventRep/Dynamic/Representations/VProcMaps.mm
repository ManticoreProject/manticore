/** \file  VProcMaps.mm
  * \author Korei Klein
  * \date 7/31/09
  *
  *
  */

#import "VProcMaps.h"
#import "Box.h"
#import "log-desc.hxx"

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



