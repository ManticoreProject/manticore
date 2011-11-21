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
#include <stdint.h>

extern uint32_t CRC32 (void *buf, int nBytes);

/*! \brief add an entry to the CRC buffer.
 *  \param buf the pointer to the next location in the buffer
 *  \param lab the symbol being added
 *  \param offset the symbol's value
 *  \return the next buffer location to store data.
 */
static inline char *AddCRCData (char *buf, const char *lab, uint64_t offset)
{
    int n = strlen(lab);
    strncpy(buf, lab, n);
    buf += n;
  // Note: we only record 32-bits of the offset, since the high bits will usually be 0
  // (orelse all 1's).
    *buf++ = ((offset >> 24) & 0xff);
    *buf++ = ((offset >> 16) & 0xff);
    *buf++ = ((offset >> 8) & 0xff);
    *buf++ = offset & 0xff;

    return buf;
}

#endif /* !_CRC_H_ */

