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

/* in the flat-heap model, we always allocate from the current chunk
 */
#define globNextW allocPtr
#define globLimit allocTop

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

/*! \brief allocate a vector seeded with some initial values.
 *  \param vp the host vproc
 *  \param values the values used to initialize the vector
 *  \return the allocated and initialized vector
 *  The vector type is defined in basis/sequential/vector.pml.
 */
Value_t AllocVector (VProc_t *vp, Value_t values)
{
    Word_t	*obj = (Word_t *)(vp->allocPtr);    
    int         i    = 0;

    while (values != M_NIL) {
	ListCons_t *valueList = (ListCons_t*)ValueToPtr(values);
	obj[i] = (Word_t)valueList->hd;
	values = valueList->tl;
	i++;
    }

    obj[-1] = VEC_HDR(i);
    vp->allocPtr += WORD_SZB * (i+1);
    
    return AllocNonUniform (vp, 2, PTR(PtrToValue(obj)), INT(i));
}

/*! \brief allocate a vector seeded with some initial values, which are provided in reverse order.
 *  \param vp the host vproc
 *  \param values the values used to initialize the vector
 *  \param len the size of the vector
 *  \return the allocated and initialized vector
 *  The vector type is defined in basis/sequential/vector.pml.
 */
Value_t AllocVectorRev (VProc_t *vp, Value_t values, int len)
{
    Word_t	*obj = (Word_t *)(vp->allocPtr);    
    int         i    = 0;

    while (values != M_NIL) {
	ListCons_t *valueList = (ListCons_t*)ValueToPtr(values);
	obj[len - i - 1] = (Word_t)valueList->hd;
	values = valueList->tl;
	i++;
    }

    obj[-1] = VEC_HDR(i);
    vp->allocPtr += WORD_SZB * (i+1);

    assert (len == i);
    
    return AllocNonUniform (vp, 2, PTR(PtrToValue(obj)), INT(i));
}

/*! \brief allocate a wrapped word value.
 */
Value_t WrapWord (VProc_t *vp, Word_t i)
{
    Word_t	*obj = (Word_t *)(vp->allocPtr);
    obj[-1] = RAW_HDR(1);
    obj[0] = i;

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
    hdr[1] = (Word_t)len-1;
    vp->allocPtr += WORD_SZB * 3;

    return PtrToValue(hdr);

}

/*! \brief allocate raw-data object that can hold the given number of bytes
 */
Value_t AllocRaw (VProc_t *vp, uint32_t len)
{
    Word_t	*obj = (Word_t *)(vp->allocPtr);
    int nWords = BYTES_TO_WORDS(len);
    obj[-1] = RAW_HDR(nWords);
    vp->allocPtr += WORD_SZB * (nWords+1);

    return PtrToValue(obj);

}

/* FIXME: this function does not belong here! */
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

    if (vp->globNextW + WORD_SZB * (nElems+1) >= vp->globLimit) {
	AllocToSpaceChunk(vp);
    }

    assert (AddrToChunk(vp->globNextW)->sts == TO_SP_CHUNK);

  /* first we must ensure that the elements are in the global heap */
    va_start(ap, nElems);
    for (int i = 0;  i < nElems;  i++) {
	elems[i] = va_arg(ap, Value_t);
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

    if (vp->globNextW + WORD_SZB * (nElems+1) >= vp->globLimit) {
	AllocToSpaceChunk(vp);
    }

    assert (AddrToChunk(vp->globNextW)->sts == TO_SP_CHUNK);

  /* first we must ensure that the elements are in the global heap */
    va_start(ap, nElems);
    for (int i = 0;  i < nElems;  i++) {
	int tag = va_arg(ap, int);
	assert ((tag == RAW_FIELD) || (tag == PTR_FIELD));
	bits |= (tag << i);
	elems[i] = va_arg(ap, Value_t);
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

/*! \brief allocate an array in the global heap
 *  \param vp the host vproc
 *  \param nElems the size of the array
 *  \param elt the initial value for the array elements
 *  \return pointer to the beginning of the array
 */
Value_t GlobalAllocArray (VProc_t *vp, int nElems, Value_t elt)
{
  /* the array must fit into a global chunk */
    assert (HEAP_CHUNK_SZB > WORD_SZB*(nElems+1) && nElems >= 0);

    if (vp->globNextW + WORD_SZB * (nElems+1) >= vp->globLimit) {
	AllocToSpaceChunk(vp);
    }

    Word_t *obj = (Word_t*)(vp->globNextW);
    obj[-1] = VEC_HDR(nElems);
    for (int i = 0;  i < nElems;  i++) {
	obj[i] = (Word_t)elt;
    }

    vp->globNextW += WORD_SZB * (nElems+1);
    return PtrToValue(obj);
}

/*! \brief allocate an array of floats in the global heap
 *  \param vp the host vproc
 *  \param nElems the number of elements in the array
 *  \param elt the initial value for the array elements
 *  \return pointer to the beginning of the array
 */
Value_t GlobalAllocFloatArray (VProc_t *vp, int nElems, float elt)
{
    int nWords = BYTES_TO_WORDS(nElems * sizeof(float));
  /* the array must fit into a global chunk */
    assert(HEAP_CHUNK_SZB > WORD_SZB*(nWords+1) && nElems >= 0);

    if (vp->globNextW + WORD_SZB * (nWords+1) >= vp->globLimit) {
	AllocToSpaceChunk(vp);
    }

    Word_t *obj = (Word_t*)(vp->globNextW);
    obj[-1] = RAW_HDR(nWords);
    float *arr = (float*)obj;
    for (int i = 0;  i < nElems;  i++) {
	arr[i] = elt;
    }

    vp->globNextW += WORD_SZB * (nWords+1);
    return PtrToValue(obj);
}

/*! \brief allocate an array of ints in the global heap
 *  \param vp the host vproc
 *  \param nElems the number of elements in the array
 *  \param elt the initial value for the array elements
 *  \return pointer to the beginning of the array
 */
Value_t GlobalAllocIntArray (VProc_t *vp, int nElems, int32_t elt)
{
    int nWords = BYTES_TO_WORDS(nElems * sizeof(int32_t));
  /* the array must fit into a global chunk */
    assert(HEAP_CHUNK_SZB > WORD_SZB*(nWords+1) && nElems >= 0);

    if (vp->globNextW + WORD_SZB * (nWords+1) >= vp->globLimit) {
	AllocToSpaceChunk(vp);
    }

    Word_t *obj = (Word_t*)(vp->globNextW);
    obj[-1] = RAW_HDR(nWords);
    int *arr = (int*)obj;
    for (int i = 0;  i < nElems;  i++) {
	arr[i] = elt;
    }

    vp->globNextW += WORD_SZB * (nWords+1);
    return PtrToValue(obj);
}

/*! \brief allocate an array of word64s in the global heap
 *  \param vp the host vproc
 *  \param nElems the number of elements in the array
 *  \param elt the initial value for the array elements
 *  \return pointer to the beginning of the array
 */
Value_t GlobalAllocWord64Array (VProc_t *vp, int nElems, uint64_t elt)
{
    int nWords = BYTES_TO_WORDS(nElems * sizeof(uint64_t));
  /* the array must fit into a global chunk */
    assert(HEAP_CHUNK_SZB > WORD_SZB*(nWords+1));

    if (vp->globNextW + WORD_SZB * (nWords+1) >= vp->globLimit) {
	AllocToSpaceChunk(vp);
    }

    Word_t *obj = (Word_t*)(vp->globNextW);
    obj[-1] = RAW_HDR(nWords);
    uint64_t *arr = (uint64_t*)obj;
    for (int i = 0;  i < nElems;  i++) {
	arr[i] = elt;
    }

    vp->globNextW += WORD_SZB * (nWords+1);
    return PtrToValue(obj);
}
