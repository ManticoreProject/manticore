/* alloc.c
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 */

#include "manticore-rt.h"
#include <stdarg.h>
#include <string.h>
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

/*! \brief allocate a non-uniform tuple of values.
 *  \param vp the host vproc
 *  \param nElems the number of tuple elements.
 */
Value_t AllocNonUniform (VProc_t *vp, int nElems, ...)
{
    Word_t	*obj = (Word_t *)(vp->allocPtr);
    Word_t	bits = 0;
    va_list	ap;

    va_start(ap, nElems);
    for (int i = 0;  i < nElems;  i++) {
	int tag = va_arg(ap, int);
	assert ((tag == RAW_FIELD) || (tag == PTR_FIELD));
	bits |= (tag << i);
	Value_t arg = va_arg(ap, Value_t);
	obj[i] = (Word_t)arg;
    }
    va_end(ap);
    obj[-1] = MIXED_HDR(bits, nElems);

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

/*! \brief allocate an ML string from a C string.
 */
Value_t AllocString (VProc_t *vp, const char *s)
{
    int len = strlen(s) + 1;

  /* allocate the raw data object */
    Word_t	*obj = (Word_t *)(vp->allocPtr);
    int nWords = BYTES_TO_WORDS(len);
    obj[-1] = RAW_HDR(nWords);
    memcpy (obj, s, len);
    vp->allocPtr += WORD_SZB * (nWords+1);

  /* allocate the string header object */
    Word_t	*hdr = (Word_t *)(vp->allocPtr);
    hdr[-1] = VEC_HDR(2);
    hdr[0] = (Word_t)(PtrToValue(obj));
    hdr[1] = (Word_t)len;
    vp->allocPtr += WORD_SZB * 3;

    return PtrToValue(hdr);

}

/*! \brief allocate raw-data object that can hold the given number of bytes
 */
Value_t AllocRaw (VProc_t *vp, UInt32_t len)
{
    Word_t	*obj = (Word_t *)(vp->allocPtr);
    int nWords = BYTES_TO_WORDS(len);
    obj[-1] = RAW_HDR(nWords);
    vp->allocPtr += WORD_SZB * (nWords+1);

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

/*! \brief allocate a non-uniform tuple of values.
 *  \param vp the host vproc
 *  \param nElems the number of tuple elements.
 */
Value_t GlobalAllocNonUniform (VProc_t *vp, int nElems, ...)
{
    Value_t	elems[nElems];
    Word_t	bits = 0;
    va_list	ap;

    assert (vp == VProcSelf());

  /* first we must ensure that the elements are in the global heap */
    va_start(ap, nElems);
    for (int i = 0;  i < nElems;  i++) {
	int tag = va_arg(ap, int);
	assert ((tag == RAW_FIELD) || (tag == PTR_FIELD));
	bits |= (tag << i);
	elems[i] = PromoteObj (vp, va_arg(ap, Value_t));
    }
    va_end(ap);

    Word_t *obj = (Word_t *)(vp->globNextW);
/* FIXME: what if there isn't enough space!!! */
    obj[-1] = MIXED_HDR(bits, nElems);
    for (int i = 0;  i < nElems;  i++) {
	obj[i] = (Word_t)elems[i];
    }

    vp->globNextW += WORD_SZB * (nElems+1);
    return PtrToValue(obj);
}
