/* basis.c
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 */

#include "manticore-rt.h"
#include "vproc.h"

/* M_Print:
 */
void M_Print (char *s)
{
    Say("[%2d] %s", VProcSelf()->id, s);
}
