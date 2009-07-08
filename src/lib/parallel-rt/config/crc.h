/*! \file crc.h
 *
 * \author John Reppy
 */

/*
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 */

#ifndef _CRC_H_
#define _CRC_H_

#include <string.h>

extern uint32_t CRC32 (void *buf, int nBytes);

/* add an entry to the buffer that we'll compute the CRC of */
static inline char *AddCRCData (char *buf, const char *lab, int offset)
{
    int n = strlen(lab);
    strncpy(buf, lab, n);
    buf += n;
    *buf++ = ((offset >> 8) & 0xff);
    *buf++ = offset & 0xff;

    return buf;
}

#endif /* !_CRC_H_ */

