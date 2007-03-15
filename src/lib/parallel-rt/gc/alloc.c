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

/*! \brief allocate a tuple of uniform values.
 */
Value_t AllocUniform (VProc_t *vp, int nItems, ...)
{
    Word_t	*obj = (Word_t *)(vp->allocPtr);
    va_list	ap;

    va_start(ap, nItems);
    obj[-1] = VEC_HDR(nItems);
    for (int i = 0;  i < nItems;  i++) {
	Value_t arg = va_arg(ap, Value_t);
	obj[i] = (Word_t)arg;
    }
    va_end(ap);

    vp->allocPtr += WORD_SZB * (nItems+1);
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
    if (ValueIsBoxed(v)) {
	Value_t *obj = (Value_t *)ValueToPtr(v);
	Word_t hdr = ((Word_t *)obj)[-1];
	int n = GetLength(hdr);
	Say("[");
	for (int i = 0;  i < n; i++) {
	    Say("%p", ValueToPtr(obj[i]));
	    if (i != 0) Say(", ");
	}
	Say ("]");
    }
    else
	Say("%d", ValueToWord(v));
}
