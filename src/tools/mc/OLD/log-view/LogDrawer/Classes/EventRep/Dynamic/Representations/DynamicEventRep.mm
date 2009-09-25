/** \file  DynamicEventRep.c
 * \author Korei Klein
 * \date 7/15/09
 *
 */

#import "DynamicEventRep.hxx"

NSString *DynamicEventDescription(DynamicEvent e)
{
    return [NSString stringWithFormat:
	@"< Event at time %x >", e.timestamp];
}


