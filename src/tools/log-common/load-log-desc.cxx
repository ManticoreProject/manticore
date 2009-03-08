/* load-log-desc.cxx
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 */

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "log-desc.hxx"
#include "json.h"

class LogFileDescLoader : public LogDescVisitor {
  public:
    LogFileDescLoader () { this->_nextId = 0; }

    LogFileDesc *GetFile (JSON_Value_t *v);
    EventOrGroup *GetEventOrGroup (JSON_Value_t *v);
    EventDesc *NewEvent (const char *Name, const char *kindStr, JSON_Value_t *v);
    EventGroup *NewGroup (const char *Name, JSON_Value_t *events);

    LogFileDesc *Load (const char *logDescFile);

  /* used to initialize event IDs */
    void VisitGroup (EventGroup *grp);
    void VisitEvent (EventDesc *evt);

  protected:
    LogFileDesc			*_desc;
    int				_nextId;

};

inline char *CopyString (const char *s)
{
    if (s == 0) return 0;
    return strcpy (new char[strlen(s)+1], s);
}

static ArgDesc *GetArgs (JSON_Value_t *v)
{
    unsigned int location = 12;  /* the argument area starts at byte 12 */

    assert ((v->tag == JSON_array) || (v->u.array.length > 0));

    ArgDesc *ads = new ArgDesc[v->u.array.length];

    for (int i = 0;  i < v->u.array.length;  i++) {
	JSON_Value_t *arg = JSON_GetElem(v, i);
	const char *name = JSON_GetString(JSON_GetField(arg, "name"));
	const char *tyStr = JSON_GetString(JSON_GetField(arg, "ty"));
	JSON_Value_t *loc = JSON_GetField(arg, "loc");
	const char *desc = JSON_GetString(JSON_GetField(arg, "desc"));
	if ((name == 0) || (tyStr == 0) || (desc == 0)) {
	    free (ads);
	    return 0;
	}

      /* translate the argument type */
	ArgType ty;
	int n;
	if (strcasecmp(tyStr, "addr") == 0) ty = ADDR;
	else if (strcasecmp(tyStr, "int") == 0) ty = INT;
	else if (strcasecmp(tyStr, "word") == 0) ty = WORD;
	else if (strcasecmp(tyStr, "float") == 0) ty = FLOAT;
	else if (strcasecmp(tyStr, "double") == 0) ty = DOUBLE;
	else if (strcasecmp(tyStr, "event") == 0) ty = EVENT_ID;
	else if (sscanf(tyStr, "str%d", &n) == 1) ty = (ArgType)((int)STR0 + n);
	else {
	    free (ads);
	    return 0;
	}

      /* compute the location (if not given) */
	/* FIXME */

	ads[i].name = CopyString(name);
	ads[i].ty;
	ads[i].loc = location;
	ads[i].desc;
    }

    return ads;
}

/*! \brief construct a log-file description by reading in the JSON file.
 */
LogFileDesc *LoadLogDesc (const char *logDescFile)
{
    LogFileDescLoader loader;

    return loader.GetFile (JSON_ParseFile (logDescFile));

}


/***** class LogFileDescLoader member functions *****/

LogFileDesc *LogFileDescLoader::GetFile (JSON_Value_t *v)
{
    if (v == 0) return 0;

    const char *date = JSON_GetString(JSON_GetField(v, "date"));
    JSON_Value_t *version = JSON_GetField(v, "version");

    EventGroup *grp = this->NewGroup ("root", JSON_GetField(v, "events"));

    if (grp == 0) return 0;

    std::vector<EventDesc *> *events = new std::vector<EventDesc *> (this->_nextId, (EventDesc *)0);
    LogFileDesc *lfd = new LogFileDesc (grp);
    lfd->_events = events;

  /* initialize the events array */
    this->_desc = lfd;
    lfd->PreOrderWalk (this);

    return lfd;

}

EventOrGroup *LogFileDescLoader::GetEventOrGroup (JSON_Value_t *v)
{
    const char *name = JSON_GetString(JSON_GetField(v, "name"));
    const char *kindStr = JSON_GetString(JSON_GetField(v, "kind"));

    if ((name == 0) || (kindStr == 0))
	return 0;

    if (strcasecmp(kindStr, "group") == 0)
      // this is an event group
	return this->NewGroup (name, JSON_GetField(v, "events"));
    else
      // this is an event descriptor
	return this->NewEvent (name, kindStr, v);

}

EventDesc *LogFileDescLoader::NewEvent (const char *name, const char *kindStr, JSON_Value_t *v)
{
	JSON_Value_t *args = JSON_GetField(v, "args");
	const char *desc = JSON_GetString(JSON_GetField(v, "desc"));

	if ((args == 0) || (desc == 0) || (args->tag != JSON_array))
	    return 0;

	ArgDesc *ads;
	if (args->u.array.length == 0)
	    ads = 0;
	else if ((ads = GetArgs(args)) == 0) {
	    fprintf (stderr, "bad argument for event %s\n", name);
	    return 0;
	}

	EventKind kind;
	if (strcasecmp(kindStr, "event") == 0) kind = LOG_EVENT;
	else if (strcasecmp(kindStr, "start") == 0) kind = LOG_START;
	else if (strcasecmp(kindStr, "end") == 0) kind = LOG_END;
	else if (strcasecmp(kindStr, "src") == 0) kind = LOG_SRC;
	else if (strcasecmp(kindStr, "dst") == 0) kind = LOG_DST;
	else {
	    fprintf (stderr, "bad kind for event %s\n", name);
	    return 0;
	}

	EventDesc *ed = new EventDesc (name, kind);
	ed->_id		= this->_nextId++;
	ed->_nArgs	= args->u.array.length;
	ed->_args	= ads;
	ed->_desc	= CopyString(desc);

	return ed;
   
}

EventGroup *LogFileDescLoader::NewGroup (const char *name, JSON_Value_t *events)
{
    if ((events == 0) || (events->tag != JSON_array)) return 0;

    EventGroup *grp = new EventGroup (name, events->u.array.length);

    for (int i = 0;  i < events->u.array.length;  i++) {
	EventOrGroup *ed = GetEventOrGroup (JSON_GetElem(events, i));
	if (ed == 0) {
	    fprintf (stderr, "bad event description\n");
	    return 0;
	}
	grp->Add (i, ed);
    }

    return grp;

}

/* visitors used to initialize event IDs */

void LogFileDescLoader::VisitGroup (EventGroup *grp)
{
}

void LogFileDescLoader::VisitEvent (EventDesc *evt)
{
    this->_desc->_events->at(evt->_id) = evt;
}
