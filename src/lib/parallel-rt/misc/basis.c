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
#include <sys/time.h>
#include <math.h>

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
void M_Print (const char *s)
{
#ifdef NDEBUG
    Say("%s", s);
#else  
    Say("[%2d] %s", VProcSelf()->id, s);
#endif
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

/* M_StringConcatList:
 * XXX - Could this somehow be defined in the HLOp language?  I'm not
 * familiar enough with it, so I don't know. -JDR
 */
Value_t M_StringConcatList (Value_t l)
{
    VProc_t *vp = VProcSelf();
    uint32_t len = 0;
    Value_t data = M_NIL;
    ListCons_t * list_p = (ListCons_t *)ValueToPtr(l);
    ListCons_t * crnt_p = NULL;
    SequenceHdr_t * crnt_str = NULL;
    Byte_t * buf = NULL;

    crnt_p = list_p;
    while (crnt_p != (ListCons_t *)M_NIL) {
        crnt_str = (SequenceHdr_t *)ValueToPtr(crnt_p->hd);
        /* XXX Do we support/want to support really long strings?  If
           so, overflow detection would be a good thing here. */
        len += crnt_str->len;
        crnt_p = (ListCons_t *)ValueToPtr(crnt_p->tl);
    }

    data = AllocRaw(vp, len + 1);
    buf = (Byte_t *)ValueToPtr(data);

    crnt_p = list_p;
    while (crnt_p != (ListCons_t *)M_NIL) {
        crnt_str = (SequenceHdr_t *)crnt_p->hd;
        memcpy(buf, ValueToPtr(crnt_str->data), crnt_str->len);
        buf += crnt_str->len;
        crnt_p = (ListCons_t *)ValueToPtr(crnt_p->tl);
    }

    *buf = 0;
    return AllocNonUniform(vp, 2, PTR(data), INT((Word_t)len));
}

/* M_GetTimeOfDay:
 */
double M_GetTimeOfDay ()
{
    struct timeval	now;

    gettimeofday (&now, 0);

    return (double)(now.tv_sec) + 0.000001 * (double)(now.tv_usec);

}

/***** functions to support debugging *****/

Value_t M_Test ()
{
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

Value_t M_Die (const char *message)
{
    Die ("%s\n", message);
}

#ifndef NDEBUG
Value_t M_AssertFail (const char *check, char *file, int line)
{
    Die ("Assert failed at %s:%d (%s)\n", file, line, check);
}
#endif

void M_PrintDebug (const char *s)
{
#ifndef NDEBUG
    if (DebugFlg)
	SayDebug("[%2d] %s", VProcSelf()->id, s);  
#endif
}

void M_PrintDebugMsg (const char *msg, char *file, int line)
{
#ifndef NDEBUG
    if (DebugFlg)  
      SayDebug ("[ %s ] at %s:%d\n", msg, file, line);
#endif
}

void M_PrintPtr (const char *name, void *ptr)
{
    Say("[%2d] &%s=%p\n", VProcSelf()->id, name, ptr);  
}

/* M_PrintLong:
 */
void M_PrintLong (int64_t n)
{
    Say("%ld", n);
}

/* M_PrintInt:
 */
void M_PrintInt (int32_t n)
{
    Say("%d\n", n);
}

void M_PrintFloat (float f)
{
    Say ("%f\n",f);
}

/* FIXME: eventually, this code should be in assembler to reduce overhead */
#include "inline-log.h"
void M_LogEvent0 (void *vp, int evt) 
{
    LogEvent0 (vp, evt);
}

void M_LogEventPtr (void *vp, int evt, uint64_t ptr)
{
    LogEvent2 (vp, evt, (uint32_t)(ptr << 32l), (uint32_t)ptr);
}
