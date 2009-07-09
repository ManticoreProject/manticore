/*! \file log-desc.hxx
 *
 * \author John Reppy
 */

/*
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 */

#ifndef _LOG_DESC_HXX_
#define _LOG_DESC_HXX_

#include <vector>

class EventDesc;

//! \brief the different kinds of groups of events
enum GroupKind {
    EVENT_GROUP,	/*!< a group of events and/or groups */
    STATE_GROUP,	/*!< a group of events that represent state transitions */
    INTERVAL_GROUP,
    DEPENDENT_GROUP
};

/*! \brief the base of the group-class hierarchy */
class Group {
  public:
  /// the description of the group
    const char *Desc () const	{ return this->_desc; }
  /// the parent group; 0 for the root
    Group *Parent () const	{ return this->_grp; }
  /// the kind of the group
    GroupKind Kind() const	{ return this->_kind; }
  /// is this group the root?
    bool isRoot () const	{ return (this->_grp == 0); }

    void SetGroup (Group *grp) { this->_grp = grp; }

  protected:
    const char	*_desc;		/*!< the event's description */
    GroupKind	_kind;		/*!< the kind of group */
    Group	*_grp;		/*!< the group that this group belongs to */

    explicit Group (const char *desc, GroupKind kind);
    virtual ~Group ();

};

/*! \brief the representation of a group of events and/or groups */
class EventGroup : public Group {
  public:
    ~EventGroup ();

  /// return the number of events in this group
    int NumEvents () const { return this->_events.size(); }
  /// return the i'th event in this group
    EventDesc *Event (int i) const { return this->_events.at(i); }

  /// return the number of subgroups in this group
    int NumKids () const { return this->_groups.size(); }
  /// return the i'th subgroup in this group
    Group *Kid (int i) const { return this->_groups.at(i); }

  protected:
    EventGroup (const char *desc, int nEvents, int nGroups);

    void AddEvent (int i, EventDesc *item);
    void AddGroup (int i, Group *item);

  private:
    std::vector<EventDesc *>	_events;
    std::vector<Group *>	_groups;

    friend class LogFileDescLoader;

};

//! \brief state transitions in a state group
struct StateTransition {
    EventDesc	*_event;	//!< the event that causes the transition
    int		_nextState;	//!< the state being transitioned to.

    StateTransition () : _event(0), _nextState(0) { }
};

/*! \brief the representation of a state group */
class StateGroup : public Group {
  public:
    ~StateGroup ();

    int StartState () const	{ return this->_start; }
    int NextState (int st, EventDesc *evt) const;

    const char *StateName (int i) const	{ return this->_stateNames.at(i); }

  protected:
    StateGroup (const char *desc, int nStates, int nTransitions);

    void AddState (int i, const char *st);
    void AddTransition (int i, EventDesc *evt, const char *st);

  private:
    int				_start;		//!< the initial state
    std::vector<const char *>	_stateNames;	//!< the state names
    std::vector<StateTransition> _transitions;

    friend class LogFileDescLoader;

};

/*! \brief the representation of an interval group */
class IntervalGroup : public Group {
  public:
    ~IntervalGroup ();

    EventDesc *Start () const	{ return this->_start; }
    EventDesc *End () const	{ return this->_end; }

  protected:
    explicit IntervalGroup (const char *desc, EventDesc *a, EventDesc *b);

  private:
    EventDesc	*_start;
    EventDesc	*_end;

    friend class LogFileDescLoader;

};

/*! \brief the representation of a group of dependent events */
class DependentGroup : public Group {
  public:
    ~DependentGroup ();

    EventDesc *Src () const	{ return this->_src; }
    EventDesc *Dst () const	{ return this->_dst; }

  protected:
    explicit DependentGroup (const char *desc, EventDesc *src, EventDesc *dst);

  private:
    EventDesc	*_src;
    EventDesc	*_dst;

    friend class LogFileDescLoader;

};

//! \brief abstract virtual class for traversing the event hierarchy
class LogDescVisitor {
  public:
    virtual void VisitGroup (EventGroup *grp) = 0;
    virtual void VisitEvent (EventDesc *evt) = 0;
};

/*! \brief the description of a log file.  This class combines the
 *  information from the log-events.json and even-view.json files.
 */
class LogFileDesc {
  public:
    int NumEventKinds () const { return this->_events->size(); }
    EventDesc *FindEventById (int id) const { return this->_events->at(id); }
    EventDesc *FindEventByName (const char *name) const;

  /* visitor walks of the event hierarchy */
    void PreOrderWalk (LogDescVisitor *visitor);
    void PostOrderWalk (LogDescVisitor *visitor);

    ~LogFileDesc ();

  protected:
    EventGroup			*_root;
    std::vector<EventDesc *>	*_events;

    LogFileDesc (std::vector<EventDesc *> *evts)
    {
	this->_root = 0;
	this->_events = evts;
    }

    friend class LogFileDescLoader;

};

extern LogFileDesc *LoadLogDesc (const char *logDescFile, const char *logViewDesc);

#endif /* !_LOG_DESC_HXX_ */
