/* request-codes.h
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 */

#ifndef _REQUEST_CODES_H_
#define _REQUEST_CODES_H_

typedef enum {
    REQ_GC=1,		/* request a minor GC */
    REQ_Return=2,		/* returning from a function call */
    REQ_UncaughtExn=3,	/* raising an exception */
    REQ_Sleep=4,		/* make the VProc idle */
} RequestCode_t;

#endif /* !_REQUEST_CODES_H_ */
