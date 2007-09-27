/* basis.c
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 */

#include "manticore-rt.h"
#include <stdio.h>
#include "vproc.h"
#include "value.h"

/* M_Print:
 */
void M_Print (char *s)
{
    Say("[%2d] %s", VProcSelf()->id, s);
}

/* M_LongToString:
 */
Value_t M_LongToString (long n)
{
    char buf[32];
    snprintf(buf, sizeof(buf), "%dl", n);
    return AllocString (VProcSelf(), buf);
}

