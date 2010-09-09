/* request-codes.h
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 */

#ifndef _REQUEST_CODES_H_
#define _REQUEST_CODES_H_

typedef enum {
    REQ_GC,		/* request a minor GC */
    REQ_Return,		/* returning from a function call */
    REQ_UncaughtExn,	/* raising an exception */
    REQ_Sleep,		/* make the VProc idle */
} RequestCode_t;

#endif /* !_REQUEST_CODES_H_ */
