/* basis.c
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 */

#include "manticore-rt.h"
#include <stdio.h>
#include <string.h>
#include "vproc.h"
#include "value.h"
#include "heap.h"
#include "../gc/gc-inline.h"

/* M_IntToString:
 */
Value_t M_IntToString (int32_t n)
{
    char buf[32];
    snprintf(buf, sizeof(buf), "%d", n);
    return AllocString (VProcSelf(), buf);
}

/* M_LongToString:
 */
Value_t M_LongToString (int64_t n)
{
    char buf[32];
    snprintf(buf, sizeof(buf), "%ld", n);
    return AllocString (VProcSelf(), buf);
}

/* M_FloatToString:
 */
Value_t M_FloatToString (float f)
{
    char buf[64];
    snprintf(buf, sizeof(buf), "%f", f);
    return AllocString (VProcSelf(), buf);
}

/* M_DoubleToString:
 */
Value_t M_DoubleToString (double f)
{
    char buf[64];
    snprintf(buf, sizeof(buf), "%f", f);
    return AllocString (VProcSelf(), buf);
}

/* M_Print:
 */
void M_Print (char *s)
{
    Say("[%2d] %s", VProcSelf()->id, s);
}

void M_PrintDebug (char *s)
{
  if (DebugFlg)
    Say("[%2d] %s", VProcSelf()->id, s);  
}

/* M_PrintLong:
 */
void M_PrintLong (int64_t n)
{
    Say("%", n);
}

Value_t M_PrintFloat (float f)
{
  Say ("%f\n",f);
}

/* M_StringConcat2:
 */
Value_t M_StringConcat2 (Value_t a, Value_t b)
{
    SequenceHdr_t	*s1 = (SequenceHdr_t *)ValueToPtr(a);
    SequenceHdr_t	*s2 = (SequenceHdr_t *)ValueToPtr(b);

    if (s1->len == 0) return b;
    else if (s2->len == 0) return a;
    else {
	VProc_t *vp = VProcSelf();
	uint32_t len = s1->len + s2->len;
	Value_t data = AllocRaw(vp, len + 1);
      // initialize the data object
	Byte_t *p = (Byte_t *)ValueToPtr(data);
	memcpy (p, ValueToPtr(s1->data), s1->len);
	memcpy (p+s1->len, ValueToPtr(s2->data), s2->len);
	p[len] = 0;
      // allocate the sequence-header object
	return AllocNonUniform(vp, 2, PTR(data), INT((Word_t)len));
    }

}

Value_t M_Test () {
  return Some(VProcSelf(), AllocUniform (VProcSelf(), 1, 2));
}

void M_AssertNotLocalPtr (Value_t item)
{
    /* item must be a pointer in the global queue, and thus we can
     * at least be sure that it is not in the local queue
     */
  if (inVPHeap ((Addr_t)VProcSelf(), (Addr_t)item)) {
    Die ("Pointer %p is in the local heap when it should be in the global heap\n", item);
  }

}

