/*! \file proxy.c — Search Results (proxy)
 *
 * \author Sven Auhagen
 */

/*
 * COPYRIGHT (c) 2010 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 */

#include "manticore-rt.h"
#include "value.h"
#include "vproc.h"

#include "gc-inline.h"
#include "gc.h"
#include "gc-scan.h"

#include <stdio.h>


void createList (VProc_t *vp) {
	//set List of empty entries
	for (int i = 0; i < vp->maxProxy;i++) {
		vp->proxyTable[i].proxyObj=(Value_t)(long long int)(i+1);
	}
	vp->proxyTableentries = 0;
}


