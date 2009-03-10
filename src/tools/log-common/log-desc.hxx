/* load-log-desc.hxx
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 */

#ifndef _LOG_DESC_HXX_
#define _LOG_DESC_HXX_

#include <vector>

typedef enum ArgType {
    ADDR,
    INT,
    WORD,
    FLOAT,
    DOUBLE,
    EVENT_ID,
    STR0
};

#define STR(n)		(STR0+(n))
#define isSTR(ty)	((ty) > STR0)
#define STRLEN(ty)	((int)(ty) - (int)STR0)
#define MAX_STRLEN	20

struct ArgDesc {
    char	*name;		// the argument's name
    ArgType	ty;		// the argument's type
    int		loc;		// the offset of the field from the start of the event (in bytes)
    char	*desc;
} ;

//! \brief An event-argument value
union ArgValue {
    uint64_t		a;
    int32_t		i;
    uint32_t		w;
    float		f;
    double		d;
    uint64_t		id;
    char		str[MAX_STRLEN+1];
};

//! \brief An event-argument value tagged with its type
struct TaggedArgValue {
    const ArgDesc	*desc;
    ArgValue		val;
};

enum EventKind {
    LOG_GROUP,		/* a group of events */
    LOG_EVENT,		/* an independent event */
/* FIXME: replace START/STOP classification with START/STOP/MIDDLE to allow tracking of status
 * changes over an interval (e.g., running/idle/sleeping).
 */
    LOG_START,		/* the start of an interval; the next event code will be the */
			/* end of the interval */
    LOG_END,		/* the end of an interval; the previous event code will be the */
			/* start of the interval */
    LOG_SRC,		/* the source of a dependent event */
    LOG_DST,		/* the destination of a dependent event */
};

/* helper class for loading the log-file description */
class LogFileDescLoader;

/* event descriptor are either groups of events or actual events. */
class EventOrGroup;	// either a group (EventGroup) or event (EventDesc)
class EventGroup;	// a group of events
class EventDesc;	// an event description.

class EventOrGroup {
  public:
    const char *Name () const	{ return this->_name; }
    bool isGroup () const	{ return this->_kind == LOG_GROUP; }
    EventGroup *Group () const	{ return this->_grp; }
    EventKind Kind() const	{ return this->_kind; }
    bool isRoot () const	{ return (this->_grp == 0); }

  protected:
    const char	*_name;		/* the event's name */
    EventKind	_kind;		/* the kind of event */
    EventGroup	*_grp;		/* the group that this belongs to */

    EventOrGroup (const char *name, EventKind kind);
    virtual ~EventOrGroup ();

    void SetGroup (EventGroup *grp) { this->_grp = grp; }

    friend class EventGroup;

};

class EventGroup : public EventOrGroup {
  public:
    ~EventGroup ();

    int NumKids () const { return this->_kids.size(); }
    EventOrGroup *Kid (int i) const { return this->_kids.at(i); }

  protected:
    std::vector<EventOrGroup *>	_kids;

    EventGroup (const char *name, int n);

    void Add (int i, EventOrGroup *item);

    friend class LogFileDescLoader;

};

class EventDesc : public EventOrGroup {
  public:
    int Id () const { return this->_id; }
    int NArgs () const { return this->_nArgs; }
    const ArgDesc *Args () const { return this->_args; }
    const char *Description () const { return this->_desc; }

    ArgDesc *GetArgDesc (int i)	{ return &(this->_args[i]); }
    ArgType GetArgType (int i)	{ return this->_args[i].ty; }

    ArgValue GetArg (struct struct_log_event *evtData, int i);

    ~EventDesc ();

  protected:
    int		_id;
    int		_nArgs;
    ArgDesc	*_args;
    char	*_desc;

    EventDesc (const char *name, EventKind kind);

    friend class LogFileDescLoader;

};

//! \brief abstract virtual class for traversing the event hierarchy
class LogDescVisitor {
  public:
    virtual void VisitGroup (EventGroup *grp) = 0;
    virtual void VisitEvent (EventDesc *evt) = 0;
};

class LogFileDesc {
  public:
    int NumEventKinds () const { return this->_events->size(); }
    EventDesc *FindEventById (int id) { return this->_events->at(id); }

  /* visitor walks of the event hierarchy */
    void PreOrderWalk (LogDescVisitor *visitor);
    void PostOrderWalk (LogDescVisitor *visitor);

    ~LogFileDesc ();

  protected:
    EventGroup			*_root;
    std::vector<EventDesc *>	*_events;

    LogFileDesc (EventGroup *root);

    friend class LogFileDescLoader;

};

extern LogFileDesc *LoadLogDesc (const char *logDescFile);

#endif /* !_LOG_DESC_HXX_ */
