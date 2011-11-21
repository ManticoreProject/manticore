/** \file  EventArg.m
\author Korei Klein
\date 8/7/09

*/

#import "EventArg.h"
#import "log-file.h"
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
	    self.type = @"addr";
	    self.value = [NSString stringWithFormat:
			  @"%qu", argValue.a];
	    break;
    	case INT:
	    self.size = @"32 bit";
	    self.type = @"int";
	    self.value = [NSString stringWithFormat:
			  @"%d", argValue.i];
	    break;
    	case WORD:
	    self.size = @"32 bit";
	    self.type = @"uint";
	    self.value = [NSString stringWithFormat:
			  @"0x%X", argValue.w];
	    break;
    	case FLOAT:
	    self.size = @"32 bit";
	    self.type = @"float";
	    // XXX will this always print correctly?
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
	    self.type = @"new id";
	    self.value = [NSString stringWithFormat:
			  @"%qu", argValue.id];
	    break;
    	case EVENT_ID:
	    self.size = @"64 bit";
	    self.type = @"existing id";
	    self.value = [NSString stringWithFormat:
			  @"%qu", argValue.id];
	    break;
    	case STR0: {
		self.size = [NSString stringWithFormat: @"%d chars", (MAX_STRLEN + 1)];
		self.type = @"fixed length string";
		const char *s = (const char *) malloc(MAX_STRLEN + 1);
		memcpy((void *)s, &(argValue.str), MAX_STRLEN + 1);
		NSString *S = [NSString stringWithCString:s encoding:NSASCIIStringEncoding];
		self.value = S;
	    } break;
	default:
	    [Exceptions raise:@"unrecognized argument type found in event description"];
	    break;
    }
    return self;
}


@end
