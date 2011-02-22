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
 *  Proxy object: pointer values
 *
 *  --------------------------------------------------------- 
 *  | -- 48 bits -- | -- 15 bits -- | -- 1 bit -- |
 *  |     length    |       2       |      1      |
 *  ---------------------------------------------------------  
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


/* Tags for Table objects */
#define TABLE_TAG_BITS	1
#define TABLE_TAG	1
#define TABLE_LEN_ID	15

#define VEC_TAG_BITS 1
#define RAW_TAG_BITS 0

//Mixed object
#define MIXED_HDR(id, len) (((len) << (TABLE_LEN_ID+TABLE_TAG_BITS)) | ((id)<< TABLE_TAG_BITS) | TABLE_TAG)

//Vector object
#define VEC_HDR(len)	(((len) << (TABLE_LEN_ID+TABLE_TAG_BITS)) | ((VEC_TAG_BITS) << TABLE_TAG_BITS) | TABLE_TAG)

//Raw object
#define RAW_HDR(len)	(((len) << (TABLE_LEN_ID+TABLE_TAG_BITS)) | ((RAW_TAG_BITS) << TABLE_TAG_BITS) | TABLE_TAG)

//Proxy object
#define PXY_HDR(len)	(((len) << (TABLE_LEN_ID+TABLE_TAG_BITS)) | ((2) << TABLE_TAG_BITS) | TABLE_TAG)

#endif /* !_HEADER_BITS_H_ */
