/* gen-log-events.c
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Generate logging event codes for use in HLOps.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

/* define the predefined log-event codes */
#define DEF_EVENT(NAME, SZ, KIND, DESC)	NAME,
enum {
#include "log-events.h"
    NumLogEvents
};
#undef DEF_EVENT

#define DEF_EVENT(NAME, SZ, KIND, DESC)	\
        printf ("#define " #NAME " %d\n", NAME);

int main ()
{
    printf ("(* log-events.def\n");
    printf ("*\n");
    printf (" * WARNING: this file is generated; do not edit!!!\n");
    printf (" *)\n");
    printf ("\n#ifndef _LOG_EVENTS_\n");
    printf ("\n#define _LOG_EVENTS_\n\n");

#include "log-events.h"

    printf ("\n#endif /* !_LOG_EVENTS_DEF_ */\n");

    return 0;
}
