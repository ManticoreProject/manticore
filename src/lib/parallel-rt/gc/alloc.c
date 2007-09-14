/* alloc.c
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 */

#include "manticore-rt.h"
#include <stdarg.h>
#include "value.h"
#include "vproc.h"
#include "gc-inline.h"
#include "gc.h"

/*! \brief allocate a tuple of uniform values.
 *  \param vp the host vproc
 *  \param nElems the number of tuple elements.
 */
Value_t AllocUniform (VProc_t *vp, int nElems, ...)
{
    Word_t	*obj = (Word_t *)(vp->allocPtr);
    va_list	ap;

    va_start(ap, nElems);
    obj[-1] = VEC_HDR(nElems);
    for (int i = 0;  i < nElems;  i++) {
	Value_t arg = va_arg(ap, Value_t);
	obj[i] = (Word_t)arg;
    }
    va_end(ap);

    vp->allocPtr += WORD_SZB * (nElems+1);
    return PtrToValue(obj);
}

/*! \brief allocate a wrapped integer value.
 */
Value_t WrapInt (VProc_t *vp, long i)
{
    Word_t	*obj = (Word_t *)(vp->allocPtr);
    obj[-1] = RAW_HDR(1);
    obj[0] = (Word_t)i;

    vp->allocPtr += WORD_SZB * 2;
    return PtrToValue(obj);
}

void SayValue (Value_t v)
{
    if (ValueIsBoxed(v) && (v != 0)) {
	Value_t *obj = (Value_t *)ValueToPtr(v);
	Word_t hdr = ((Word_t *)obj)[-1];
	int n = GetLength(hdr);
	Say("[");
	for (int i = 0;  i < n; i++) {
	    if (i != 0) Say(", ");
	    Say("%p", ValueToPtr(obj[i]));
	}
	Say ("]");
    }
    else
	Say("%ld", ValueToWord(v));
}

/*! \brief allocate a tuple of uniform values on the global heap.
 *  \param vp the host vproc
 *  \param nElems the number of tuple elements.
 */
Value_t GlobalAllocUniform (VProc_t *vp, int nElems, ...)
{
    Value_t	elems[nElems];
    va_list	ap;

    assert (vp == VProcSelf());

  /* first we must ensure that the elements are in the global heap */
    va_start(ap, nElems);
    for (int i = 0;  i < nElems;  i++) {
	elems[i] = PromoteObj (vp, va_arg(ap, Value_t));
    }
    va_end(ap);

    Word_t *obj = (Word_t *)(vp->globNextW);
/* FIXME: what if there isn't enough space!!! */
    obj[-1] = VEC_HDR(nElems);
    for (int i = 0;  i < nElems;  i++) {
	obj[i] = (Word_t)elems[i];
    }

    vp->globNextW += WORD_SZB * (nElems+1);
    return PtrToValue(obj);
}
