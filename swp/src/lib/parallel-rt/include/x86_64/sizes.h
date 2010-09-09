/* sizes.h
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Various sizes for the x86_64 (aka AMD64).
 */

#ifndef _SIZES_H_
#define _SIZES_H_

/* log2 of the BIBOP page size */
#define PAGE_BITS	20	/* one-megabyte pages in the global heap */

/* size of VProc local heap */
#ifndef VP_HEAP_SZB
#  define VP_HEAP_SZB		ONE_MEG
#endif

/* sizes for the stack frame used to run Manticore code */
#define SPILL_SZB	2048	/* for register spilling */
#define SAVE_AREA	(5*8)	/* for callee saves %rbx, %r12-%r15 */
#define PAD_SZB		8	/* pad so that frame size (plus saved PC and FP */
				/* is a multiple of 16 bytes */
#define FRAME_SZB	(SPILL_SZB+SAVE_AREA+PAD_SZB)

#endif /* !_SIZES_H_ */
