/* value.h
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 */

#ifndef _VALUE_H_
#define _VALUE_H_

#include "manticore-rt.h"

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

FunClosure_t *ValueToClosure (Value_t v)	{ return (FunClosure_t *)ValueToPtr(v); }
ContClosure_t *ValueToCont (Value_t v)		{ return (ContClosure_t *)ValueToPtr(v); }

#endif /* !_VALUE_H_ */
