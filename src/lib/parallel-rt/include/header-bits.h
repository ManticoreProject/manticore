/* header-bits.h
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * The following three objects can occur in the Manticore heap. We pair each with
 * a distinct header format.
 * 
 *  Mixed-Type Object: pointer and raw values
 *
 *  ---------------------------------------------- 
 *  | -- 56 bits -- | -- 6-bits -- | -- 1 bit -- |
 *  | pointer mask  |     length   |      1      |
 *  ----------------------------------------------
 *
 *  Raw-data object: raw values
 *
 *  ---------------------------------------------- 
 *  |        -- 61 bits --        | -- 3 bits -- |
 *  |           length            |     010      |
 *  ----------------------------------------------
 *
 *  Vector object: pointer values
 *
 *  ---------------------------------------------- 
 *  |        -- 61 bits --        | -- 3 bits -- |
 *  |           length            |     100      |
 *  ----------------------------------------------
 * 
 * We also have a header format for forwarding pointers.
 *
 *  Forwarding pointers:
 *
 *  ---------------------------------------------- 
 *  |        -- 61 bits --        | -- 3 bits -- |
 *  |  left forward pointer bits  |     000      |
 *  ---------------------------------------------- 
 *
 */

#ifndef _HEADER_BITS_H_
#define _HEADER_BITS_H_

/* Tags for forward pointers */
#define FWDPTR_TAG_BITS	3
#define FWDPTR_TAG	0
#define FWDPTR_TAG_MASK	7

/* Tags for mixed objects */
#define MIXED_TAG_BITS	1
#define MIXED_TAG	1
#define MIXED_TAG_MASK	1
#define MIXED_LEN_BITS	6

#define MIXED_HDR(bits, len)	\
	(((bits) << (MIXED_LEN_BITS+MIXED_TAG_BITS)) | \
	((len)<< MIXED_TAG_BITS) | \
	MIXED_TAG)

/* Tags for vectors of pointers and tagged ints */
#define VEC_TAG_BITS	3
#define VEC_TAG		4
#define VEC_TAG_MASK	7

#define VEC_HDR(len)	(((len) << VEC_TAG_BITS) | VEC_TAG)

/* Tags for raw data */
#define RAW_TAG_BITS	3
#define RAW_TAG		2
#define RAW_TAG_MASK	7

#define RAW_HDR(len)	(((len) << RAW_TAG_BITS) | RAW_TAG)

#endif /* !_HEADER_BITS_H_ */
