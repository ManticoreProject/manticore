/* load-log-desc.c
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 */

#include <stdlib.h>
#include <string.h>
#include "log-desc.h"
#include "json.h"

static inline char *CopyString (const char *s)
{
    if (s == 0) return 0;
    return strcpy ((char *)malloc(strlen(s)+1), s);
}

EventDesc_t *GetEventDesc (JSON_Value_t *v)
{
    const char *name = JSON_GetString(JSON_GetField(v, "name"));
    JSON_Value_t *args = JSON_GetField(v, "args");
    const char *kindStr = JSON_GetString(JSON_GetField(v, "desc"));
    const char *desc = JSON_GetString(JSON_GetField(v, "desc"));

    if ((name == 0) || (args == 0) || (kind == 0) || (desc == 0) || (args->tag != JSON_array))
	return 0;

    ArgDesc_t *ads;
    if (args->u.array.length == 0)
	ads = 0;
    else if ((ads = GetArgs (args)) == 0)
	return 0;

    EventKind_t kind;
    if (strcmp(kindStr, "START") == 0) kind = LOG_START;
    else if (strcmp(kindStr, "END") == 0) kind = LOG_END;
    else if (strcmp(kindStr, "EVENT") == 0) kind = LOG_EVENT;
    else return 0;

    EventDesc_t *ed = (EventDesc_t *)malloc(sizeof(EventDesc_t));
    ed->name	= CopyString(name);
    ed->nArgs	= args->u.array.length;
    ed->args	= ads;
    ed->kind	= kind;
    ed->desc	= desc;

}

LogFileDesc_t *LoadLogDesc (const char *logDescFile)
{
    JSON_Value_t *v = JSON_ParseFile (logDescFile);

    if (v == 0) return 0;

    const char *version = JSON_GetString(JSON_GetField(v, "version"));
    JSON_Value_t *events = JSON_GetField(v, "events");

    if (events->tag != JSON_array) return 0;

    for (int i = 0;  i < events-u.array.length;  i++) {
	EventDesc_t *ed = GetEventDesc(JSON_GetElem(events, i));
    }

}
