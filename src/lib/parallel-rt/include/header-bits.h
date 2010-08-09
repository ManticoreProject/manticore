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
 *  | -- 48 bits -- | -- 15 bits -- | -- 1 bit -- |
 *  |	  length    |      ID       |      1      |
 *  ----------------------------------------------
 *
 *  Raw-data object: raw values
 *
 *  ---------------------------------------------- 
 *  | -- 48 bits -- | -- 15 bits -- | -- 1 bit -- |
 *  |	  length    |       0       |      1      |
 *  ---------------------------------------------- 
 *
 *  Vector object: pointer values
 *
 *  ---------------------------------------------- 
 *  | -- 48 bits -- | -- 15 bits -- | -- 1 bit -- |
 *  |	  length    |       1       |      1      |
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
#define MIXED_LEN_ID	15

#define MIXED_HDR(id, len)	\
	(((len) << (MIXED_LEN_ID+MIXED_TAG_BITS)) | \
	((id)<< MIXED_TAG_BITS) | \
	MIXED_TAG)

/* Tags for vectors of pointers and tagged ints */
#define VEC_TAG_BITS	1
#define VEC_TAG		1
#define VEC_TAG_MASK	1
#define VEC_LEN_ID	15

#define VEC_HDR(len)	(((len) << (VEC_TAG_BITS+VEC_LEN_ID)) | ((1) << VEC_TAG_BITS) | VEC_TAG)

/* Tags for raw data */
#define RAW_TAG_BITS	1
#define RAW_TAG		1
#define RAW_TAG_MASK	1
#define RAW_LEN_ID	15

#define RAW_HDR(len)	(((len) << (RAW_TAG_BITS+RAW_LEN_ID)) | ((0) << RAW_TAG_BITS) | RAW_TAG)

#endif /* !_HEADER_BITS_H_ */
