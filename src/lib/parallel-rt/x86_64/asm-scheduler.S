/* asm-scheduler.S
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Manticore runtime conventions on the AMD64:
 *
 *	%rax	-- standard arg
 *	%rdi	-- standard ep
 *	%rsi	-- standard return continuation
 *	%rbx	-- standard exception handler
 *	%rcx	-- allocation pointer
 *	%r11	-- limit pointer
 */

#ifndef NOT_C_SOURCE
#  define NOT_C_SOURCE
#endif
#include "asm-offsets.h"

/* ASM_Run:
 *
 * This function is invoked by Manticore code to do a scheduler run operation.
 * The incoming register state is:
 *
 *	%rax	-- holds signal action continuation
 *	%rdi	-- holds fiber to run
 */
	.text
	.p2align 3
	.global	ASM_Run
ASM_Run:
      /* compute the vproc pointer from the allocation pointer */
	movq	%rcx,%r10
	andq	$VP_MASK,%r10
      /* allocate stack cell and push on stack */
	movq	$ACTCONS_HDR,-8(%rcx)		/* store header */
	movq	%rax,ACTCONS_ACT_OFF(%rcx)	/* store signal-action continuation */
	movq	ACTION_STK(%r10),%r9
	movq	%r9,ACTCONS_LINK_OFF(%rcx)	/* store top of stack in the link field */
	movq	%rcx,ACTION_STK(%r10)		/* set top of stack to new cell */
	leaq	24(%rcx),%rcx			/* bump up allocation pointer */
      /* transfer control to the fiber */
	movl	$M_UNIT,%eax
	jmpq	*(%rdi)

/* ASM_Forward:
 *
 * This function is invoked by Manticore code to do a scheduler forward operation.
 * The incoming register state is:
 *
 *	%rax	-- holds signal to forward
 */
	.text
	.p2align 3
	.global	ASM_Forward
ASM_Forward:
      /* compute the vproc pointer from the allocation pointer */
	movq	%rcx,%r10
	andq	$VP_MASK,%r10
      /* fetch top of sigaction stack */
	movq	ACTION_STK(%r10),%r9
	movq	ACTCONS_ACT_OFF(%r9),%rdi	/* stdEP gets signal-action continuation */
	movq	ACTCONS_LINK_OFF(%r9),%r9	/* pop the cell */
	movq	%r9,ACTION_STK(%r10)
      /* transfer control to the signal-action continuation */
	jmpq	*(%rdi)
