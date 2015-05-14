/* remember-set.h
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * remember set structure
 */

#ifndef _REMEMBER_SET_H_
#define _REMEMBER_SET_H_

#include "manticore-rt.h"
#include "vproc.h"
#include "value.h"

/* remember set element structure
 */
struct RS_s {
	Value_t * source;		//source pointer 
	int offset;			//offset of source pointer to remember
	struct RS_s * next;		//next element of the remember set
};
typedef struct RS_s RS_t;

/* \brief determine the number of root elements in the remember set
 * \param self the host vproc
 * \return the number of root elements
 */
int M_NumRSRoots(VProc_t *self);

Value_t ** M_AddRSElts(VProc_t * vp, Value_t ** rootPtr);

#endif /*! _REMEMBER_SET_H_ */
