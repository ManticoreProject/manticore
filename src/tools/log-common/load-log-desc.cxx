/* load-log-desc.cxx
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 */

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "event-desc.hxx"
#include "log-desc.hxx"
#include "json.h"

class LogFileDescLoader {
  public:
    LogFileDescLoader () { this->_desc = 0; this->_nextId = 0; }

    LogFileDesc *FileDesc () const { return this->_desc; }

    bool GetLogEventsFile (JSON_Value_t *v);
    bool GetLogViewFile (JSON_Value_t *v);

    EventDesc *NewEvent (JSON_Value_t *v);
    Group *NewGroup (JSON_Value_t *obj);

    EventDesc *GetEventField (JSON_Value_t *v, const char *name);

  protected:
    LogFileDesc		*_desc;
    int			_nextId;

};

inline char *CopyString (const char *s)
{
    if (s == 0) return 0;
    return strcpy (new char[strlen(s)+1], s);
}

/*! \brief process the "args" array of an event descriptor.
 *  \param v the JSON object that represents the array of argument descriptors.
 *  \return the argument descriptors.
 */
static ArgDesc *GetArgs (JSON_Value_t *v)
{
    unsigned int location = 12;  /* the argument area starts at byte 12 */

    assert ((v->tag == JSON_array) || (v->u.array.length > 0));

    ArgDesc *ads = new ArgDesc[v->u.array.length];

    for (int i = 0;  i < v->u.array.length;  i++) {
	JSON_Value_t *arg = JSON_GetElem(v, i);

      /* get the fields */
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
	else if (strcasecmp(tyStr, "new-id") == 0) ty = NEW_ID;
	else if (strcasecmp(tyStr, "id") == 0) ty = EVENT_ID;
	else if (sscanf(tyStr, "str%d", &n) == 1) ty = (ArgType)((int)STR0 + n);
	else {
	    free (ads);
	    return 0;
	}

      /* compute the location (if not given) */
	/* FIXME */

	ads[i].name = CopyString(name);
	ads[i].ty = ty;
	ads[i].loc = location;
	ads[i].desc = CopyString(desc);
    }

    return ads;
}

/*! \brief construct a log-file description by reading in the JSON file.
 *  \param logDescFile the path to the log-events.json file
 *  \param logViewFile the path to the log-view.json file
 *  \return the log-file descriptor or 0 if there was an error.
 */
LogFileDesc *LoadLogDesc (const char *logDescFile, const char *logViewFile)
{
    LogFileDescLoader loader;

    if (loader.GetLogEventsFile (JSON_ParseFile (logDescFile))
    && loader.GetLogViewFile (JSON_ParseFile (logViewFile)))
	return loader.FileDesc();
    else
	return 0;

}


/***** class LogFileDescLoader member functions *****/

bool LogFileDescLoader::GetLogEventsFile (JSON_Value_t *v)
{
    if (v == 0) return false;

    const char *date = JSON_GetString(JSON_GetField(v, "date"));
    JSON_Value_t *version = JSON_GetField(v, "version");

    JSON_Value_t *events = JSON_GetField(v, "events");

    if ((events == 0) || events->tag != JSON_array) return 0;

  // allocate the events vector, including a slot for NoEvent
    std::vector<EventDesc *> *eds =
	new std::vector<EventDesc *> (events->u.array.length + 1, (EventDesc *)0);
    this->_desc = new LogFileDesc (eds);;

  /* initialize the events array */
    eds->at(0) = new EventDesc ();  /* NoEvent */
    for (int i = 1;  i < eds->size();  i++) {
	EventDesc *ed = NewEvent (events->u.array.elems[i-1]);
	if (ed == 0) return false;
	eds->at(i) = ed;
    }

    return true;

}

bool LogFileDescLoader::GetLogViewFile (JSON_Value_t *v)
{
    if ((v == 0) || (this->_desc == 0)) return false;

    const char *date = JSON_GetString(JSON_GetField(v, "date"));
    JSON_Value_t *version = JSON_GetField(v, "version");
    JSON_Value_t *root = JSON_GetField(v, "root");

/* FIXME: we should check consistency between the log-events file
 * and the log-view file.
 */

    if ((date == 0) || (version == 0) || (root == 0)
    || (root->tag != JSON_object))
	return false;

    Group *grp = this->NewGroup (root);
    if ((grp == 0) || (grp->Kind() != EVENT_GROUP))
	return false;
    this->_desc->_root = dynamic_cast<EventGroup *>(grp);

    assert (this->_desc->_root != 0);

    return true;

}

EventDesc *LogFileDescLoader::NewEvent (JSON_Value_t *v)
{
    const char *name = JSON_GetString(JSON_GetField(v, "name"));
    JSON_Value_t *args = JSON_GetField(v, "args");
    const char *desc = JSON_GetString(JSON_GetField(v, "desc"));

    if ((name == 0) || (args == 0) || (desc == 0) || (args->tag != JSON_array))
	return 0;

    ArgDesc *ads;
    if (args->u.array.length == 0)
	ads = 0;
    else if ((ads = GetArgs(args)) == 0) {
	fprintf (stderr, "bad argument for event %s\n", name);
	return 0;
    }

  /* get optional "attrs" field */
    JSON_Value_t *attrs = JSON_GetField (args, "attrs");
    if (attrs != 0) {
	if (attrs->tag != JSON_array) {
	    delete ads;
	    return 0;
	}
/* FIXME: process attrs field */
    }

    EventDesc *ed = new EventDesc (
	    name, this->_nextId++,
	    args->u.array.length, ads,
	    CopyString(desc));

    return ed;

}

Group *LogFileDescLoader::NewGroup (JSON_Value_t *v)
{
    const char *desc = JSON_GetString(JSON_GetField(v, "desc"));
    const char *kindStr = JSON_GetString(JSON_GetField(v, "kind"));

    if ((desc == 0) || (kindStr == 0)) return 0;

    if (strcasecmp(kindStr, "group") == 0) {
	JSON_Value_t *events = JSON_GetField(v, "events");
	JSON_Value_t *groups = JSON_GetField(v, "groups");
	if ((events == 0) || (events->tag != JSON_array)
	|| (groups == 0) || (groups->tag != JSON_array))
	    return 0;
	EventGroup *grp =
	    new EventGroup (desc,
		events->u.array.length, groups->u.array.length);
/* FIXME: add events and groups */
	return grp;
    }
    else if (strcasecmp(kindStr, "state") == 0) {
	JSON_Value_t *states = JSON_GetField(v, "states");
	JSON_Value_t *trans = JSON_GetField(v, "transitions");
	if ((states == 0) || (states->tag != JSON_array)
	|| (trans == 0) || (trans->tag != JSON_array))
	    return 0;
	int nStates = states->u.array.length;
	int nTrans = trans->u.array.length;
	if ((nStates == 0) || (nTrans == 0)) return 0;
	StateGroup *grp = new StateGroup (desc, nStates, nTrans);
      /* add the state names */
	for (int i = 0;  i < nStates;  i++) {
	    grp->AddState(i,
		CopyString(JSON_GetString(states->u.array.elems[i])));
	}
      /* add the transitions */
	for (int i = 0;  i < nTrans;  i++) {
/* FIXME: add transitions */
	}
	return grp;
    }
    else if (strcasecmp(kindStr, "interval") == 0) {
	EventDesc *a = this->GetEventField (v, "start");
	EventDesc *b = this->GetEventField (v, "end");
	if ((a == 0) || (b == 0))
	    return 0;
	else
	    return new IntervalGroup (desc, a, b);
    }
    else if (strcasecmp(kindStr, "dependent") == 0) {
	EventDesc *src = this->GetEventField (v, "src");
	EventDesc *dst = this->GetEventField (v, "dst");
	if ((src == 0) || (dst == 0))
	    return 0;
	else
	    return new DependentGroup (desc, src, dst);
    }
    else {
	fprintf (stderr, "bad group kind %s\n", kindStr);
	return 0;
    }

}

EventDesc *LogFileDescLoader::GetEventField (JSON_Value_t *v, const char *name)
{
    const char *evtName = JSON_GetString(JSON_GetField(v, name));

    if (evtName == 0)
	return 0;
    else
	return this->_desc->FindEventByName (evtName);

}

