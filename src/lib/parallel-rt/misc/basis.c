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
    snprintf(buf, sizeof(buf), "%f", (double)f);
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

/* Return the time of day as an integer number of microseconds.
 */
Word_t M_GetTime ()
{
    struct timeval t;
    
    gettimeofday (&t, NULL);
    
    return 1000000*(Word_t)t.tv_sec + (Word_t)t.tv_usec;

}

/* M_GetNumProcs:
 *
 * Return the number of hardware processors.
 */
int M_GetNumProcs ()
{
    return NumHardwareProcs;
}

/* M_GetNumVProcs:
 *
 * Return the number of enabled vprocs.
 */
int M_GetNumVProcs ()
{
    return NumVProcs;
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

Value_t M_AssertFail (const char *check, char *file, int line)
{
    if ((check != (const char *)M_NIL) && (file != (const char *)M_NIL))
	Die ("Assert failed at %s:%d (%s).\n", file, line, check);
    else
	Die ("Assert failed with corrupted diagnostic information.\n");
}

void M_PrintDebug (const char *s)
{
#ifndef NDEBUG
    if (DebugFlg)
	SayDebug("[%2d] %s", VProcSelf()->id, s);  
#endif
}

void M_PrintDebugMsg (Value_t alwaysPrint, const char *msg, char *file, int line)
{
#ifndef NDEBUG
    if (DebugFlg || (alwaysPrint==M_TRUE))
	SayDebug ("[%2d] \"%s\" at %s:%d\n", VProcSelf()->id, msg, file, line);
#endif
}

void M_PrintTestingMsg (const char *msg, char *file, int line)
{
    Say ("[%2d] Test failed: \"%s\" at %s:%d\n", VProcSelf()->id, msg, file, line);
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
    Say("%d\t", n);
}

void M_PrintFloat (float f)
{
    Say ("%f\n",(double)f);
}

int M_ReadInt ()
{
    int i;
    scanf ("%d\n", &i);
    return i;
}

float M_ReadFloat ()
{
    float i;
    scanf ("%f", &i);
    return (float)i;
}

double M_ReadDouble ()
{
    double i;
    scanf ("%lf", &i);
    return i;
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

void M_LogEvent1 (void *vp, int evt, uint32_t v)
{
    LogEvent1 (vp, evt, v);
}

double M_DRand (double lo, double hi)
{
    return (((double)rand() / ((double)(RAND_MAX)+(double)(1)) ) * (hi-lo)) + lo;
}

Word_t M_Random (Word_t lo, Word_t hi)
{
    return (random() % (hi - lo)) + lo;
}

int32_t M_RandomInt (int32_t lo, int32_t hi)
{
  return (int)M_Random((int)lo, (int)hi);
}

void M_SeedRand ()
{
    srand(time(NULL));
}

/*! \brief allocate an array in the global heap
 *  \param vp the host vproc
 *  \param nElems the size of the array
 *  \param elt the initial value for the array elements
 */
Value_t M_NewArray (VProc_t *vp, int nElems, Value_t elt)
{

  /* the array must fit into a global chunk */
    assert((vp->globLimit-vp->globNextW) > WORD_SZB*(nElems+1));

    if (vp->globNextW + WORD_SZB * (nElems+1) >= vp->globLimit) {
	GetChunkForVProc(vp);
    }

    Word_t *obj = (Word_t*)(vp->globNextW);
    obj[-1] = VEC_HDR(nElems);
    for (int i = 0;  i < nElems;  i++) {
	obj[i] = (Word_t)elt;
    }

    vp->globNextW += WORD_SZB * (nElems+1);
    return PtrToValue(obj);
}

float M_Powf (float x, float y)
{
  return powf(x, y);
}

float M_Cosf (float x)
{
  return cosf(x);
}

float M_Sinf (float x)
{
  return sinf(x);
}

float M_Tanf (float x)
{
  return tanf(x);
}

double M_Pow (double x, double y)
{
  return pow(x, y);
}

double M_Cos (double x)
{
  return cos(x);
}

double M_Sin (double x)
{
  return sin(x);
}

double M_Tan (double x)
{
  return tan(x);
}
