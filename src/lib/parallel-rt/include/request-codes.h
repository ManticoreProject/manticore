/* request-codes.h
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 */

#ifndef _REQUEST_CODES_H_
#define _REQUEST_CODES_H_

typedef enum {
    REQ_RequestGC,
    REQ_Return,
    REQ_RaiseExn
} RequestCode_t;

#endif /* !_REQUEST_CODES_H_ */
