/* asm-scheduler.S
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Manticore runtime conventions on the AMD64:
 *
 *	%rax	-- standard arg (a.k.a. arg0)
 *	%rdi	-- standard ep (a.k.a. arg1)
 *	%rsi	-- standard return continuation (a.k.a. arg2)
 *	%rbx	-- standard exception handler (a.k.a. arg3)
 *	%rcx	-- allocation pointer
 *	%r11	-- limit pointer
 */

#include "asm-defs.h"
#include "asm-offsets.h"

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
	movq	8(%rdi),%r9	/* fetch code address */
	movq	16(%rdi),%rdi	/* set EP to point to GC root */
	jmpq	*%r9		/* jump to the GC return code */
