/* log-desc.cxx
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 */

#include "log-desc.hxx"
#include <string.h>

inline char *CopyString (const char *s)
{
    if (s == 0) return 0;
    return strcpy (new char[strlen(s)+1], s);
}

/***** class EventOrGroup member functions *****/

EventOrGroup::EventOrGroup (const char *name, EventKind kind)
{
    this->_name = CopyString(name);
    this->_kind = kind;
}

EventOrGroup::~EventOrGroup ()
{
    delete this->_name;
}



/***** class EventGroup member functions *****/

EventGroup::EventGroup (const char *name, int n)
    : EventOrGroup (name, LOG_GROUP), _kids(n, (EventOrGroup *)0)
{
}

EventGroup::~EventGroup ()
{
    /* FIXME: delete the kids */
}

void EventGroup::Add (int i, EventOrGroup *item)
{
    this->_kids.at(i) = item;
    item->SetGroup (this);
}


/***** class EventDesc member functions *****/

EventDesc::EventDesc (const char *name, EventKind kind)
    : EventOrGroup (name, kind)
{
}

EventDesc::~EventDesc ()
{
    delete this->_name;
    if (this->_args != 0) delete this->_args;
}


/***** class LogFileDesc member functions *****/

LogFileDesc::LogFileDesc (EventGroup *root)
{
    this->_root = root;
    this->_events = 0;
}

LogFileDesc::~LogFileDesc ()
{
    delete this->_root;
    delete this->_events;
}
