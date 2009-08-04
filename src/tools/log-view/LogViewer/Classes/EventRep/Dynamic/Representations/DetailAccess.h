/** \file DetailAccess.h
 * \author Korei Klein
 * \date 7/30/09
 *
 */

#import "Detail.h"

#define STATIC_INLINE //static inline

STATIC_INLINE uint32_t Event_Id(event e)
{
    return e.value.event;
}
STATIC_INLINE struct Group *Detail_Type(Detail d)
{
    return d->type;
}
STATIC_INLINE event *Detail_State_start(Detail d)
{
    return d->data.state.start;
}
STATIC_INLINE event *Detail_State_end(Detail d)
{
    return d->data.state.end;
}
STATIC_INLINE int Detail_State_state(Detail d)
{
    return d->data.state.state;
}
STATIC_INLINE event *Detail_Simple_value(Detail d)
{
    return d->data.simple.value;
}
STATIC_INLINE event *Detail_Interval_start(Detail d)
{
    return d->data.interval.start;
}
STATIC_INLINE event *Detail_Interval_end(Detail d)
{
    return d->data.interval.end;
}
STATIC_INLINE event *Detail_Dependent_src(Detail d)
{
    return d->data.dependent.src;
}
STATIC_INLINE int Detail_Dependent_n_dsts(Detail d)
{
    return d->data.dependent.n_dsts;
}
STATIC_INLINE Dependent_Dst *Detail_Dependent_dst(Detail d, int i)
{
    return *(d->data.dependent.dsts + i);
}

STATIC_INLINE uint64_t Event_Time(event e)
{
    return e.timestamp;
}

