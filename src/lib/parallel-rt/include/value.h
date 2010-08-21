/* value.h
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 */

#ifndef _VALUE_H_
#define _VALUE_H_

#include "manticore-rt.h"

#define	M_FALSE	((Value_t)1)
#define	M_TRUE	((Value_t)3)
#define M_UNIT	((Value_t)1)
#define M_NIL	((Value_t)1)
#define M_NONE	((Value_t)1)

/* function closures are represented by code-pointer/env-pointer pairs */
typedef struct {
    Value_t	cp;
    Value_t	ep;
} FunClosure_t;

/* continuation closures are represented by a code pointer followed by their environment */
typedef struct {
    Value_t	cp;
    Value_t	env[];
} ContClosure_t;

/* a cons cell in a list */
typedef struct {
    Value_t	hd;
    Value_t	tl;
} ListCons_t;

/* a SOME option value */
typedef struct {
    Value_t	aValue;
} OptionSome_t;

/* A vector/array/string header */
typedef struct {
    Value_t	data;
    uint32_t	len;	/* length as untagged int */
} SequenceHdr_t;

STATIC_INLINE FunClosure_t *ValueToClosure (Value_t v)	{ return (FunClosure_t *)ValueToPtr(v); }
STATIC_INLINE ContClosure_t *ValueToCont (Value_t v)	{ return (ContClosure_t *)ValueToPtr(v); }

/* heap allocate a tuple of uniformly represented values in the local heap */
extern Value_t AllocUniform (VProc_t *vp, int nItems, ...);
extern Value_t AllocNonUniform (VProc_t *vp, int nItems, ...);
extern Value_t WrapWord (VProc_t *vp, Word_t i);
extern Value_t AllocString (VProc_t *vp, const char *s);
extern Value_t AllocRaw (VProc_t *vp, uint32_t sz);

extern Value_t AllocProxy (VProc_t *vp, int nItems, ...);

STATIC_INLINE Value_t ManticoreBool (bool b)  { return b ? M_TRUE : M_FALSE; }

STATIC_INLINE Value_t Cons (VProc_t *vp, Value_t a, Value_t b)
{
    return AllocUniform (vp, 2, a, b);
}

STATIC_INLINE Value_t Some (VProc_t *vp, Value_t a)
{
    return AllocUniform (vp, 1, a);
}

/* heap allocation in the global heap */
extern Value_t GlobalAllocUniform (VProc_t *vp, int nItems, ...);
extern Value_t GlobalAllocNonUniform (VProc_t *vp, int nItems, ...);
extern Value_t GlobalAllocArray (VProc_t *vp, int nElems, Value_t elt);

STATIC_INLINE Value_t GlobalCons (VProc_t *vp, Value_t a, Value_t b)
{
    return GlobalAllocUniform (vp, 2, a, b);
}

STATIC_INLINE Value_t GlobalSome (VProc_t *vp, Value_t a)
{
    return GlobalAllocUniform (vp, 1, a);
}

/* Tags for non-uniform object allocation */
#define PTR_FIELD	1
#define RAW_FIELD	0
#define PTR(p)		PTR_FIELD, (Value_t)(p)
#define INT(i)		RAW_FIELD, (Value_t)((Word_t)(i))
#define DOUBLE(d)	RAW_FIELD, (Value_t)((d).w)

/* use to coerce doubles to Word_t's as is done in DOUBLE(d) */
typedef union {
    Word_t w;
    double d;
} RawDouble_t;

/* print a value to stdout */
extern void SayValue (Value_t v);

#endif /* !_VALUE_H_ */
