/** \file DetailAccess.h
 * \author Korei Klein
 * \date 7/30/09
 *
 * The behavior of all functions defined in this module should be
 * apparent from their names.
 *
 */

#import "Detail.h"

static inline struct Group *Detail_Type(Detail d)
{
    return d->type;
}
static inline struct EventDesc *Detail_EventDesc(Detail d)
{
    return d->eventDesc;
}
static inline event *Detail_State_start(Detail d)
{
    return d->data.state.start;
}
static inline event *Detail_State_end(Detail d)
{
    return d->data.state.end;
}
static inline int Detail_State_state(Detail d)
{
    return d->data.state.state;
}
static inline event *Detail_Simple_value(Detail d)
{
    return d->data.simple.value;
}
static inline event *Detail_Interval_start(Detail d)
{
    return d->data.interval.start;
}
static inline event *Detail_Interval_end(Detail d)
{
    return d->data.interval.end;
}
static inline double Detail_Interval_height(Detail d)
{
    return d->data.interval.height;
}
static inline event *Detail_Dependent_src(Detail d)
{
    return d->data.dependent.src;
}
static inline int32_t Detail_Dependent_src_VpId(Detail d)
{
    return d->data.dependent.vpId;
}
static inline int Detail_Dependent_n_dsts(Detail d)
{
    return d->data.dependent.n_dsts;
}
static inline struct Dependent_Dst *Detail_Dependent_dst(Detail d, int i)
{
    return *(d->data.dependent.dsts + i);
}
static inline int32_t Detail_Dependent_Dst_VpId(struct Dependent_Dst *dd)
{
    return dd->vpId;
}
static inline event *Detail_Dependent_Dst_Event(struct Dependent_Dst *dd)
{
    return dd->value;
}

static inline uint64_t Event_Time(event e)
{
    return e.timestamp;
}
static inline uint32_t Event_Id(event e)
{
    return e.value.event;
}
static inline struct struct_log_event Event_Value(event e)
{
    return e.value;
}

