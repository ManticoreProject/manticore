/* request-codes.h
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 */

#ifndef _REQUEST_CODES_H_
#define _REQUEST_CODES_H_

typedef enum {
    REQ_GC,
    REQ_Return,
    REQ_UncaughtExn
} RequestCode_t;

#endif /* !_REQUEST_CODES_H_ */
