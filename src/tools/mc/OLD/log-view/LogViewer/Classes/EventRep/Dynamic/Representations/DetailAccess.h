/** \file DetailAccess.h
 * \author Korei Klein
 * \date 7/30/09
 *
 */

#import "Detail.h"

#define STATIC_INLINE static //static inline


STATIC_INLINE struct Group *Detail_Type(Detail d)
{
    return d->type;
}
STATIC_INLINE struct EventDesc *Detail_EventDesc(Detail d)
{
    return d->eventDesc;
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
STATIC_INLINE double Detail_Interval_height(Detail d)
{
    return d->data.interval.height;
}
STATIC_INLINE event *Detail_Dependent_src(Detail d)
{
    return d->data.dependent.src;
}
STATIC_INLINE int32_t Detail_Dependent_src_VpId(Detail d)
{
    return d->data.dependent.vpId;
}
STATIC_INLINE int Detail_Dependent_n_dsts(Detail d)
{
    return d->data.dependent.n_dsts;
}
STATIC_INLINE struct Dependent_Dst *Detail_Dependent_dst(Detail d, int i)
{
    return *(d->data.dependent.dsts + i);
}
STATIC_INLINE int32_t Detail_Dependent_Dst_VpId(struct Dependent_Dst *dd)
{
    return dd->vpId;
}
STATIC_INLINE event *Detail_Dependent_Dst_Event(struct Dependent_Dst *dd)
{
    return dd->value;
}

STATIC_INLINE uint64_t Event_Time(event e)
{
    return e.timestamp;
}
STATIC_INLINE uint32_t Event_Id(event e)
{
    return e.value.event;
}
STATIC_INLINE struct struct_log_event Event_Value(event e)
{
    return e.value;
}
