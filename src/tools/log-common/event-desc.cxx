/*! \file event-desc.cxx
 *
 * \author John Reppy
 */

/*
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 */

#include "event-desc.hxx"
#include "log-file.h"
#include <string.h>
#include <assert.h>

inline char *CopyString (const char *s)
{
    if (s == 0) return 0;
    return strcpy (new char[strlen(s)+1], s);
}

/***** class EventDesc member functions *****/

EventDesc::EventDesc ()
{
    this->_name		= "NoEvent";
    this->_id		= 0;
    this->_nArgs	= 0;
    this->_args		= 0;
    this->_desc		= "not an event";
}

EventDesc::EventDesc (
    const char *name, int id,
    int nArgs, ArgDesc *args,
    const char *d)
{
    this->_name		= CopyString(name);
    this->_id		= id;
    this->_nArgs	= nArgs;
    this->_args		= args;
    this->_desc		= CopyString(d);
}

EventDesc::~EventDesc ()
{
    delete this->_name;
    if (this->_args != 0) delete this->_args;
}

ArgValue EventDesc::GetArg (LogEvent_t *evtData, int i)
{
    assert ((0 <= i) && (i < this->_nArgs));

    ArgValue value;

    ArgType ty = this->_args[i].ty;
    void *p = (void *)((uint64_t)evtData + this->_args[i].loc);
    switch (ty) {
      case ADDR:
	value.a = *(uint64_t *)p;
	break;
      case INT:
	value.i = *(int32_t *)p;
	break;
      case WORD:
	value.w = *(uint32_t *)p;
	break;
      case FLOAT:
	value.f = *(float *)p;
	break;
      case DOUBLE:
	value.d = *(double *)p;
	break;
      case NEW_ID:
      case EVENT_ID:
	value.id = *(uint64_t *)p;
	break;
      default: {
	int len = STRLEN(ty);
	assert ((0 < len) && (len <= MAX_STRLEN));
	strncpy (value.str, (char *)p, len);
	value.str[len] = '\0';
	} break;
    }

    return value;
}


