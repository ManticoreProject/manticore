//
//  SelectedEvent.mm
//  LogDrawer
//
//  Created by Korei Klein on 7/30/09.
//  Copyright 2009 __MyCompanyName__. All rights reserved.
//

#import "SelectedEvent.h"
#import "DynamicEventRep.hxx"
#import "event-desc.hxx"
#import "Exceptions.h"


@implementation EventArg

@synthesize name;
@synthesize size;
@synthesize type;
@synthesize description;
@synthesize value;

- (EventArg *)initWithArgDesc:(struct ArgDesc)argDesc
		  andArgValue:(union ArgValue)argValue
{
    if (![super init]) return nil;

    self.description = [NSString stringWithCString:argDesc.desc
					  encoding:NSASCIIStringEncoding];
    self.name = [NSString stringWithCString:argDesc.name
				   encoding:NSASCIIStringEncoding];
    switch (argDesc.ty)
    {
	case ADDR:
	    self.size = @"64 bit";
	    self.type = @"address";
	    self.value = [NSString stringWithFormat:
		@"%qu", argValue.a];
	    break;
    	case INT:
	    self.size = @"32 bit";
	    self.type = @"signed int";
	    self.value = [NSString stringWithFormat:
		@"%d", argValue.i];
	    break;
    	case WORD:
	    self.size = @"32 bit";
	    self.type = @"unsigned int";
	    self.value = [NSString stringWithFormat:
		@"0x%X", argValue.w];
	    break;
    	case FLOAT:
	    self.size = @"32 bit";
	    self.type = @"float";
	    // XXX will this print correctly?
	    self.value = [NSString stringWithFormat:
		@"%f", argValue.f];
	    break;
    	case DOUBLE:
	    self.size = @"64 bit";
	    self.type = @"float";
	    self.value = [NSString stringWithFormat:
		@"%f", argValue.f];
	    break;
    	case NEW_ID:
	    self.size = @"64 bit";
	    self.type = @"new unique event identifier";
	    self.value = [NSString stringWithFormat:
		@"%qu", argValue.id];
	    break;
    	case EVENT_ID:
	    self.size = @"64 bit";
	    self.type = @"event identifier";
	    self.value = [NSString stringWithFormat:
		@"%qu", argValue.id];
	    break;
    	case STR0:
	    self.size = [NSString stringWithFormat:
		@"%d chars", (MAX_STRLEN + 1)];
	    self.type = @"fixed length string";
	    const char *s = (const char *) malloc(MAX_STRLEN + 1);
	    memcpy((void *)s, &(argValue.str), MAX_STRLEN + 1);
	    NSString *S = [NSString stringWithCString:s
					     encoding:NSASCIIStringEncoding];
	    
	    self.value = S;
	    break;
	default:
	    [Exceptions raise:@"unrecognized argument type found in event description"];
    }
    return self;
}


@end

@implementation SelectedEvent

@synthesize name;
@synthesize arguments;
@synthesize description;

- (struct DynamicEvent_struct *)value
{
    return value;
}

- (SelectedEvent *)init
{
    if (![super init]) return nil;

    self.name = @"";
    self.description = @"";
    self.arguments = [[NSMutableArray alloc] init];
    value = nil;


    return self;
}

- (SelectedEvent *)initWithValue:(struct DynamicEvent_struct *)valueVal
{
    if (![super init]) return nil;

    self.value = valueVal;

    return self;
}

- (void)setValue:(struct DynamicEvent_struct *)valueVal
{
    value = valueVal;
    struct struct_log_event v = value->value;
    EventDesc *eventDesc = value->desc;


    self.name = [NSString stringWithCString:eventDesc->Name()
				   encoding:NSASCIIStringEncoding];
    self.description = [NSString stringWithCString:eventDesc->Description()
					  encoding:NSASCIIStringEncoding];

    self.arguments = [[NSMutableArray alloc] init];
    for (int i = 0; i < eventDesc->NArgs(); ++i)
    {
	// ArgDesc *argDesc = eventDesc->GetArgDesc(i)
	[arguments addObject:
	    [[EventArg alloc] initWithArgDesc:*eventDesc->GetArgDesc(i)
				  andArgValue:eventDesc->GetArg(&v, i)]
	];
    }
}






@end
