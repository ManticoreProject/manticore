/* crc.c
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * This code is based on public domain code.  It computes a CRC32 hash of
 * the VProc offsets, which is used to check that the runtime system and
 * compiler are using the same offset values.
 */

#include "manticore-rt.h"

#define POLYNOMIAL (UInt32_t)0xedb88320

static UInt32_t	Tbl[256];

static UInt32_t Compute (UInt32_t crc, const unsigned char *buf, int nBytes)
{
    crc ^= 0xffffffff;
    while (nBytes-- > 0) {
	crc = (crc >> 8) ^ Tbl[(crc ^ *buf++) & 0xff];
    }

    return crc ^ 0xffffffff;

}

/* compute the CRC32 hash of nBytes in the given buffer.
 */
UInt32_t CRC32 (void *buf, int nBytes)
{
  /* initialize the table */
    UInt32_t	i, j;
    UInt32_t	h = 1;
    Tbl[0] = 0;
    for (i = 128; i != 0; i >>= 1) {
	h = (h >> 1) ^ ((h & 1) ? POLYNOMIAL : 0);
      /* h is now Tbl[i] */
	for (j = 0; j < 256; j += 2*i)
	    Tbl[i+j] = Tbl[j] ^ h;
    }

    UInt32_t crc = Compute(0, (unsigned char *)0, 0);
    return Compute(crc, (unsigned char *)buf, nBytes);

} /* end of CRC32 */
