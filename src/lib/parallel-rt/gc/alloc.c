/* alloc.c
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Routines for allocating heap objects; both in the nursery and in the
 * global heap.
 */

#include "manticore-rt.h"
#include <stdarg.h>
#include <string.h>
#include "value.h"
#include "vproc.h"
#include "gc-inline.h"
#include "gc.h"
#include "gc-scan.h"

#include <stdio.h>

//predefined table entries, important for AllocUniform and GlobalAllocUniform
int predefined = 3;

Value_t AllocProxy (VProc_t *vp,int nElems, ...)
{
        Value_t	elems[nElems];
        va_list	ap;
        
        if (vp->globNextW + WORD_SZB * (nElems+1) >= vp->globLimit) {
                
		AllocToSpaceChunkScan(vp);
        }
	
	//printf("Global alloc before = %llu\n",(long long int)vp->globNextW);
	
        assert (AddrToChunk(vp->globNextW)->sts == TO_SP_CHUNK);
	
        /* first we must ensure that the elements are in the global heap */
        va_start(ap, nElems);
        for (int i = 0;  i < nElems;  i++) {
                elems[i] = va_arg(ap, Value_t);
        }
        va_end(ap);
	
        Word_t *obj = (Word_t *)(vp->globNextW);
        /* FIXME: what if there isn't enough space!!! */
        obj[-1] = PXY_HDR(nElems);
        for (int i = 0;  i < nElems;  i++) {
                obj[i] = (Word_t)elems[i];
        }
	
        vp->globNextW += WORD_SZB * (nElems+1);
	
	//printf("Global alloc after = %llu\n",(long long int)vp->globNextW);
	
#ifndef NO_GC_STATS
        vp->globalStats.nBytesAlloc += WORD_SZB * (nElems+1);
#endif	
	
        return PtrToValue(obj);
}

/*! \brief allocate a tuple of uniform values in the nursery
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

/*! \brief allocate a non-uniform tuple of values in the nursery.
 *  \param vp the host vproc
 *  \param nElems the number of tuple elements.
 */
Value_t AllocNonUniform (VProc_t *vp, int nElems, ...)
{
    Word_t	*obj = (Word_t *)(vp->allocPtr);
    char	bits[5];
    va_list	ap;
	
    bits[0]='\0';
	
    va_start(ap, nElems);
    for (int i = 0;  i < nElems;  i++) {
	int tag = va_arg(ap, int);
	assert ((tag == RAW_FIELD) || (tag == PTR_FIELD));
	if (tag == 0) strcat(bits,"0");
	else strcat(bits,"1");
	Value_t arg = va_arg(ap, Value_t);
	obj[i] = (Word_t)arg;
    }
    va_end(ap);
	
    bits[strlen(bits)]='\0';
    //compare strings are reversed due to strcat(dst,src)
    if (strcmp(bits,"0") == 0) obj[-1] = MIXED_HDR(predefined, nElems);
    else if (strcmp(bits,"10") == 0) obj[-1] = MIXED_HDR(predefined+1, nElems);
    else if (strcmp(bits,"1") == 0) obj[-1] = MIXED_HDR(predefined+2, nElems);
    else if (strcmp(bits,"0101") == 0) obj[-1] = MIXED_HDR(predefined+3, nElems);
    else { printf("Error AllocNonUniform\n"); exit(5);}

    vp->allocPtr += WORD_SZB * (nElems+1);
    return PtrToValue(obj);
}

/*! \brief allocate raw-data object  in the nursery
 *  \param vp the host vproc
 *  \param len the number of bytes to allocate
 *  \return the allocated heap object
 */
Value_t AllocRaw (VProc_t *vp, uint32_t len)
{
    Word_t	*obj = (Word_t *)(vp->allocPtr);
    int nWords = BYTES_TO_WORDS(len);
    obj[-1] = RAW_HDR(nWords);
    vp->allocPtr += WORD_SZB * (nWords+1);

    return PtrToValue(obj);

}

/*! \brief allocate in the local heap an array of raw values
 *  \param vp the host vproc
 *  \param nElems the length of the array
 *  \param size in bytes of the raw values
 *  \return pointer to the new array
 */
Value_t AllocRawArray (VProc_t *vp, int nElems, int szBOfElt)
{
    Value_t data = AllocRaw (vp, nElems * szBOfElt);
    return AllocNonUniform (vp, 2, PTR(data), INT(nElems));
}

/*! \brief allocate in the global heap an array of raw values
 *  \param vp the host vproc
 *  \param nElems the number of elements in the array
 *  \param size in bytes of the raw values
 *  \return pointer to the beginning of the array
 */
Value_t GlobalAllocRawArray (VProc_t *vp, int nElems, int szBOfElt)
{
    int nArrayBytes = nElems * szBOfElt; 
                      /* number of bytes consumed by the array */
    int nObjBytes = WORD_SZB + nArrayBytes;
                      /* number of bytes consumed by the array heap object */
    Word_t *obj;
    assert(nElems >= 0);
    assert(nArrayBytes < HEAP_CHUNK_SZB); /* the array has to fit inside a heap chunk */
        
    //check if we have enough global memory in the current chunk, if not we have to allocate a new one
    if (vp->globNextW + nObjBytes >= vp->globLimit) {     
        //save the old global allocation pointer 
        MemChunk_t *oldGlobalChunk = vp->globAllocChunk;
        //allocate a new chunk of global memory
        AllocToSpaceChunk(vp);
        //add the old global memory chunk to the unscanned to space list for the global GC
        PushToSpaceChunks (vp, oldGlobalChunk, false);
    }
            
    obj = (Word_t*)(vp->globNextW);
    obj[-1] = RAW_HDR(BYTES_TO_WORDS(nArrayBytes));
    vp->globNextW += nObjBytes;
#ifndef NO_GC_STATS
    vp->globalStats.nBytesAlloc += WORD_SZB + nArrayBytes;
#endif
    return PtrToValue(obj);
}

/*! \brief allocate a vector seeded with some initial values in the nursery.
 *  \param vp the host vproc
 *  \param values the list of values used to initialize the vector
 *  \return the allocated and initialized vector
 *  The vector type is defined in basis/sequential/vector.pml.
 */
Value_t AllocVector (VProc_t *vp, Value_t values)
{
    Value_t retval;
    Word_t	*obj = (Word_t *)(vp->allocPtr);    
	int i = 0;

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

/*! \brief allocate a vector seeded with some initial values, which
 *         are provided in reverse order.
 *  \param vp the host vproc
 *  \param len the size of the vector
 *  \param values the values used to initialize the vector
 *  \return the allocated and initialized vector
 *  The vector type is defined in basis/sequential/vector.pml.
 */
Value_t AllocVectorRev (VProc_t *vp, int len, Value_t values)
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

/*! \brief allocate a wrapped word value in the nursery.
 */
Value_t WrapWord (VProc_t *vp, Word_t i)
{
    Word_t	*obj = (Word_t *)(vp->allocPtr);
    obj[-1] = RAW_HDR(1);
    obj[0] = i;

    vp->allocPtr += WORD_SZB * 2;
    return PtrToValue(obj);
}

/*! \brief allocate an ML string from a C string in the nursery.
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

/*! \brief allocate a tuple of uniform values on the global heap.
 *  \param vp the host vproc
 *  \param nElems the number of tuple elements.
 */
Value_t GlobalAllocUniform (VProc_t *vp, int nElems, ...)
{
    Value_t	elems[nElems];
    va_list	ap;

    //check if we have enough global memory in the current chunk, if not we have to allocate a new one
    if (vp->globNextW + WORD_SZB * (nElems+1) >= vp->globLimit) {     
        //save the old global allocation pointer 
        MemChunk_t *oldGlobalChunk = vp->globAllocChunk;
        //allocate a new chunk of global memory
        AllocToSpaceChunk(vp);
        //add the old global memory chunk to the unscanned to space list for the global GC
        PushToSpaceChunks (vp, oldGlobalChunk, false);
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

#ifndef NO_GC_STATS
    vp->globalStats.nBytesAlloc += WORD_SZB * (nElems+1);
#endif
	
    return PtrToValue(obj);
}

/*! \brief allocate a non-uniform tuple of values.
 *  \param vp the host vproc
 *  \param nElems the number of tuple elements.
 */
Value_t GlobalAllocNonUniform (VProc_t *vp, int nElems, ...)
{
    Value_t	elems[nElems];
    char	bits[5];
    va_list	ap;

    bits[0]='\0';

    //check if we have enough global memory in the current chunk, if not we have to allocate a new one
    if (vp->globNextW + WORD_SZB * (nElems+1) >= vp->globLimit) {     
        //save the old global allocation pointer 
        MemChunk_t *oldGlobalChunk = vp->globAllocChunk;
        //allocate a new chunk of global memory
        AllocToSpaceChunk(vp);
        //add the old global memory chunk to the unscanned to space list for the global GC
        PushToSpaceChunks (vp, oldGlobalChunk, false);
    }

    assert (AddrToChunk(vp->globNextW)->sts == TO_SP_CHUNK);

  /* first we must ensure that the elements are in the global heap */
    va_start(ap, nElems);
    for (int i = 0;  i < nElems;  i++) {
	int tag = va_arg(ap, int);
	assert ((tag == RAW_FIELD) || (tag == PTR_FIELD));
	if (tag == 0) strcat(bits,"0");
	else strcat(bits,"1");
	elems[i] = va_arg(ap, Value_t);
    }
    va_end(ap);

    Word_t *obj = (Word_t *)(vp->globNextW);
/* FIXME: what if there isn't enough space!!! */
    
   bits[strlen(bits)]='\0';
   //compare strings are reversed due to strcat(dst,src)
   if (strcmp(bits,"0") == 0) obj[-1] = MIXED_HDR(predefined, nElems);
   else if (strcmp(bits,"10") == 0) obj[-1] = MIXED_HDR(predefined+1, nElems);
   else if (strcmp(bits,"1") == 0) obj[-1] = MIXED_HDR(predefined+2, nElems);
   else if (strcmp(bits,"0101") == 0) obj[-1] = MIXED_HDR(predefined+3, nElems);
   else { printf("Error GlobalAllocNonUniform\n"); exit(5);}	
	
    for (int i = 0;  i < nElems;  i++) {
	obj[i] = (Word_t)elems[i];
    }

    vp->globNextW += WORD_SZB * (nElems+1);

#ifndef NO_GC_STATS
    vp->globalStats.nBytesAlloc += WORD_SZB * (nElems+1);
#endif

    return PtrToValue(obj);
}

/*! \brief allocate a vector seeded with some initial values.
 *  \param vp the host vproc
 *  \param len the size of the vector
 *  \param values the values used to initialize the vector
 *  \return the allocated and initialized vector
 *  The vector type is defined in basis/sequential/vector.pml.
 *  Precondition: the car of each cons cell from the values list points to the global heap
 */
Value_t GlobalAllocVector (VProc_t *vp, int len, Value_t values)
{
  /* the array must fit into a global chunk */
    assert (HEAP_CHUNK_SZB > WORD_SZB*(len+1) && len >= 0);

    //check if we have enough global memory in the current chunk, if not we have to allocate a new one
    if (vp->globNextW + WORD_SZB * (len+1) >= vp->globLimit) {     
        //save the old global allocation pointer 
        MemChunk_t *oldGlobalChunk = vp->globAllocChunk;
        //allocate a new chunk of global memory
        AllocToSpaceChunk(vp);
        //add the old global memory chunk to the unscanned to space list for the global GC
        PushToSpaceChunks (vp, oldGlobalChunk, false);
    }

    Word_t *obj = (Word_t*)(vp->globNextW);
    obj[-1] = VEC_HDR(len);
    int i = 0;
    while (values != M_NIL) {
	ListCons_t *valueList = (ListCons_t*)ValueToPtr(values);
	obj[i++] = (Word_t)valueList->hd;
	values = valueList->tl;
    }
    assert (i == len);

    vp->globNextW += WORD_SZB * (len+1);
    
    return AllocNonUniform (vp, 2, PTR(PtrToValue(obj)), INT(len));
}

/*! \brief allocate a polymorphic array in the global heap
 *  \param vp the host vproc
 *  \param nElems the size of the array
 *  \param init the initial value for the array elements
 *  \return pointer to the beginning of the array
 */
Value_t GlobalAllocPolyArray (VProc_t *vp, int nElems, Value_t init)
{
    assert (HEAP_CHUNK_SZB > WORD_SZB*(nElems+1) && nElems >= 0);
        
    //check if we have enough global memory in the current chunk, if not we have to allocate a new one
    if (vp->globNextW + WORD_SZB * (nElems+1) >= vp->globLimit){     
        //save the old global allocation pointer 
        MemChunk_t *oldGlobalChunk = vp->globAllocChunk;
        //allocate a new chunk of global memory
        AllocToSpaceChunk(vp);
        //add the old global memory chunk to the unscanned to space list for the global GC
        PushToSpaceChunks (vp, oldGlobalChunk, false);
    }
        
    Word_t *obj = (Word_t*)(vp->globNextW);
    obj[-1] = VEC_HDR(nElems);
    for (int i = 0;  i < nElems; i++)
	obj[i] = (Word_t)init;
    vp->globNextW += WORD_SZB * (nElems+1);
#ifndef NO_GC_STATS
    vp->globalStats.nBytesAlloc += WORD_SZB * (nElems+1);
#endif
    return PtrToValue(obj);
}

/*! \brief allocate a big polymorphic array
 *  \param vp the host vproc
 *  \param n the number of elements in the array
 *  \return pointer to the beginning of the array
 */
Value_t AllocBigPolyArray (VProc_t *vp, int nElems, Value_t init)
{

  if ((nElems+1) * WORD_SZB < HEAP_CHUNK_SZB)
    return GlobalAllocPolyArray (vp, nElems, init);
  else {
    // TODO
    assert(0);
    return 0;
  }
}

/*! \brief allocate in the local heap an array of ints
 *  \param vp the host vproc
 *  \param n the length of the array
 *  \return pointer to the new array
 */
Value_t AllocIntArray (VProc_t *vp, int n)
{
    return AllocRawArray (vp, n, sizeof(int32_t));
}

/*! \brief allocate a big array of ints
 *  \param vp the host vproc
 *  \param nElems the number of elements in the array
 *  \return pointer to the beginning of the array
 */
Value_t AllocBigIntArray (VProc_t *vp, int nElems)
{
  /*
    if (nElems * sizeof(int32_t) + WORD_SZB < HEAP_CHUNK_SZB)
	return GlobalAllocRawArray (vp, nElems, sizeof(int32_t));
	else */
	/* FIXME: this array will never get collected */
	/* TODO: extend the GC to support large arrays of raw values */
	return (valloc (nElems * sizeof(int)));
}

/*! \brief allocate in the local heap an array of longs
 *  \param vp the host vproc
 *  \param n the length of the array
 *  \return pointer to the new array
 */
Value_t AllocLongArray (VProc_t *vp, int n)
{
    return AllocRawArray (vp, n, sizeof(int64_t));
}

/*! \brief allocate a big array of longs 
 *  \param vp the host vproc
 *  \param nElems the number of elements in the array
 *  \return pointer to the beginning of the array
 */
Value_t AllocBigLongArray (VProc_t *vp, int nElems)
{
  /*
    if (nElems * sizeof(int64_t) + WORD_SZB < HEAP_CHUNK_SZB)
	return GlobalAllocRawArray (vp, nElems, sizeof(int64_t));
	else  */
	/* FIXME: this array will never get collected */
	/* TODO: extend the GC to support large arrays of raw values */
	return (valloc (nElems * sizeof(int)));
}

/*! \brief allocate in the local heap an array of floats
 *  \param vp the host vproc
 *  \param n the length of the array
 *  \return pointer to the new array
 */
Value_t AllocFloatArray (VProc_t *vp, int n)
{
    return AllocRawArray (vp, n, sizeof(float));
}

/*! \brief allocate a big array of floats
 *  \param vp the host vproc
 *  \param nElems the number of elements in the array
 *  \return pointer to the beginning of the array
 */
Value_t AllocBigFloatArray (VProc_t *vp, int nElems)
{
  /*    if (nElems * sizeof(float) + WORD_SZB < HEAP_CHUNK_SZB)
	return GlobalAllocRawArray (vp, nElems, sizeof(float));
    else
  */
	/* FIXME: this array will never get collected */
	/* TODO: extend the GC to support large arrays of raw values */
	return (valloc (nElems * sizeof(float)));
}

/*! \brief allocate in the local heap an array of doubles
 *  \param vp the host vproc
 *  \param n the length of the array
 *  \return pointer to the new array
 */
Value_t AllocDoubleArray (VProc_t *vp, int n)
{
    return AllocRawArray (vp, n, sizeof(double));
}

/*! \brief allocate a big array of doubles
 *  \param vp the host vproc
 *  \param nElems the number of elements in the array
 *  \return pointer to the beginning of the array
 */
Value_t AllocBigDoubleArray (VProc_t *vp, int nElems)
{
  /*
    if (nElems * sizeof(double) + WORD_SZB < HEAP_CHUNK_SZB)
	GlobalAllocRawArray (vp, nElems, sizeof(double));
	else  */
	/* FIXME: this array will never get collected */
	/* TODO: extend the GC to support large arrays of raw values */
	return (valloc (nElems * sizeof(double)));
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
