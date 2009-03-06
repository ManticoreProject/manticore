/* load-log-desc.h
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 */

#ifndef _LOG_DESC_H_
#define _LOG_DESC_H_

typedef enum {
    ADDR,
    INT,
    WORD,
    FLOAT,
    DOUBLE,
    STR0
} ArgType_t;

#define STR(n)		(STR0+(n))
#define isSTR(ty)	((ty) > STR0)
#define STRLEN(ty)	((ty) - STR0)

typedef struct {
    char	*name;
    ArgType_t	ty;
    int		loc;
    char	*desc;
} ArgDesc_t;

typedef enum {
    LOG_GROUP,		/* a group of events */
    LOG_EVENT,		/* an independent event */
    LOG_START,		/* the start of an interval; the next event code will be the */
			/* end of the interval */
    LOG_END,		/* the end of an interval; the previous event code will be the */
			/* start of the interval */
    LOG_SRC,		/* the source of a dependent event */
    LOG_DST,		/* the destination of a dependent event */
} EventKind_t;

typedef struct_event_desc *EventDesc_t;

struct struct_event_desc {
    char	*name;		/* the event's name */
    EventKind_t	kind;		/* the kind of event */
    EventDesc_t	*grp;		/* the group that this belongs to */
};

typedef {
    char	*name;		/* the event's name */
    EventKind_t	kind;		/* the kind of event */
    EventDesc_t	*grp;		/* the group that this belongs to */
    int		nEvents;
    EventDesc_t	**events;
} Group_t;

typedef struct {
    char	*name;		/* the event's name */
    EventKind_t	kind;		/* the kind of event */
    EventDesc_t	*grp;		/* the group that this belongs to */
    int		nArgs;
    ArgDesc_t	*args;
    char	*desc;
} Event_t;

typedef struct {
    int		nEvents;
    EventDesc_t	**events;
} LogFileDesc_t;

extern LogFileDesc_t *LoadLogDesc (const char *logDescFile);

/* inline functions */

static inline bool isGroup (EventDesc_t *ed) { return (ed->kind == LOG_GROUP); }
static inline GrouP_t *asGroup (EventDesc_t *ed) { return (ed->kind == LOG_GROUP) ? (Group_t *)ed : 0; }
static inline Event_t *asEvent (EventDesc_t *ed) { return (ed->kind != LOG_GROUP) ? (Event_t *)ed : 0; }

#endif /* !_LOG_DESC_H_ */
