/* load-log-desc.c
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 */

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "log-desc.h"
#include "json.h"

static inline char *CopyString (const char *s)
{
    if (s == 0) return 0;
    return strcpy ((char *)malloc(strlen(s)+1), s);
}

static ArgDesc_t *GetArgs (JSON_Value_t *v)
{
    unsigned int location = 12;  /* the argument area starts at byte 12 */

    assert ((v->tag == JSON_array) || (v->u.array.length > 0));

    ArgDesc_t *ads = (ArgDesc_t *)malloc(v->u.array.length * sizeof(ArgDesc_t));

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
	ArgType_t ty;
	int n;
	if (strcasecmp(tyStr, "addr") == 0) ty = ADDR;
	else if (strcasecmp(tyStr, "addr") == 0) ty = INT;
	else if (strcasecmp(tyStr, "word") == 0) ty = WORD;
	else if (strcasecmp(tyStr, "float") == 0) ty = FLOAT;
	else if (strcasecmp(tyStr, "double") == 0) ty = DOUBLE;
	else if (sscanf(tyStr, "str%d", &n) == 1) ty = STR0 + n;
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

static EventDesc_t *GetEventDesc (JSON_Value_t *v)
{
    const char *name = JSON_GetString(JSON_GetField(v, "name"));
    JSON_Value_t *args = JSON_GetField(v, "args");
    const char *kindStr = JSON_GetString(JSON_GetField(v, "kind"));
    const char *desc = JSON_GetString(JSON_GetField(v, "desc"));

    if ((name == 0) || (args == 0) || (kindStr == 0) || (desc == 0) || (args->tag != JSON_array))
	return 0;

    ArgDesc_t *ads;
    if (args->u.array.length == 0)
	ads = 0;
    else if ((ads = GetArgs (args)) == 0) {
	fprintf (stderr, "bad argument for event %s\n", name);
	return 0;
    }

    EventKind_t kind;
    if (strcasecmp(kindStr, "start") == 0) kind = LOG_START;
    else if (strcasecmp(kindStr, "end") == 0) kind = LOG_END;
    else if (strcasecmp(kindStr, "event") == 0) kind = LOG_EVENT;
    else {
	fprintf (stderr, "bad kind for event %s\n", name);
	return 0;
    }

    EventDesc_t *ed = (EventDesc_t *)malloc(sizeof(EventDesc_t));
    ed->name	= CopyString(name);
    ed->nArgs	= args->u.array.length;
    ed->args	= ads;
    ed->kind	= kind;
    ed->desc	= CopyString(desc);

    return ed;
}

LogFileDesc_t *LoadLogDesc (const char *logDescFile)
{
    JSON_Value_t *v = JSON_ParseFile (logDescFile);

    if (v == 0) return 0;

    const char *date = JSON_GetString(JSON_GetField(v, "date"));
    JSON_Value_t *version = JSON_GetField(v, "version");
    JSON_Value_t *events = JSON_GetField(v, "events");

    if (events->tag != JSON_array) return 0;

    EventDesc_t **eds = (EventDesc_t **)malloc(events->u.array.length * sizeof(EventDesc_t));
    for (int i = 0;  i < events->u.array.length;  i++) {
	EventDesc_t *ed = GetEventDesc(JSON_GetElem(events, i));
	if (ed == 0) {
	    fprintf (stderr, "bad event description\n");
	    free (eds);
	    return 0;
	}
	eds[i] = ed;
    }

    LogFileDesc_t *lfd = (LogFileDesc_t *)malloc(sizeof(LogFileDesc_t));
    lfd->nEvents = events->u.array.length;
    lfd->events = eds;

    return lfd;

}
