/* asm-scheduler.S
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 */

#include "asm-defs.h"
#include "asm-offsets.h"
#include "registers.h"	

/* ASM_Resume:
 *
 * This function is the code for the resume continuation created by preemption.
 * It assumes that the standard environment pointer points to a tuple with the
 * following layout:
 *
 *		+----------------+
 *	ep ---> |   ASM_Resume   |
 *		+----------------+
 *		| GC return addr |
 *		+----------------+
 *		|    GC root     |
 *		+----------------+
 */
	.text
	.p2align 3
	.globl	_GSYM(ASM_Resume)
_GSYM(ASM_Resume):
	movq	8(STD_EP_REG),%r9		/* fetch code address */
	movq	16(STD_EP_REG),STD_EP_REG	/* set EP to point to GC root */
	jmpq	*%r9				/* jump to the GC return code */
