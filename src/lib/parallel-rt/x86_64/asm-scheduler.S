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
      /* mark that we are out of the atomic region */
	movl	$M_FALSE,%eax
	movq	%rax,ATOMIC(%r10)
      /* transfer control to the fiber */
	movl	$M_UNIT,%eax
	jmpq	*(%rdi)

/* ASM_Forward:
 *
 * This function is invoked by Manticore code to do a scheduler forward operation.
 * The incoming register state is:
 *
 *	%rax	-- holds the signal to forward
 */
	.text
	.p2align 3
	.global	ASM_Forward
ASM_Forward:
      /* compute the vproc pointer from the allocation pointer */
	movq	%rcx,%r10
	andq	$VP_MASK,%r10
      /* mark that we are running in atomic mode */
	movl	$M_TRUE,%edi
	movq	%rdi,ATOMIC(%r10)
      /* fetch top of sigaction stack */
	movq	ACTION_STK(%r10),%r9
	movq	ACTCONS_ACT_OFF(%r9),%rdi	/* stdEP gets signal-action continuation */
	movq	ACTCONS_LINK_OFF(%r9),%r9	/* pop the cell */
	movq	%r9,ACTION_STK(%r10)
      /* transfer control to the signal-action continuation */
	jmpq	*(%rdi)

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
	.global	ASM_Resume
ASM_Resume:
	movq	8(%rdi),%r9	/* fetch code address */
	movq	16(%rdi),%rdi	/* set EP to point to GC root */
	jmpq	*%r9		/* jump to the GC return code */

/* ASM_Enqueue:
 *
 * This function enqueues a (fiber, thread ID) pair in the calling vproc's
 * ready queue.  It takes three arguments in registers as follows:
 *
 *	%rax	-- fiber
 *	%rdi	-- thread ID
 *	%rsi	-- host vproc (preserved across the call)
 */
	.text
	.p2align 3
	.global	ASM_Enqueue
ASM_Enqueue:
	movq	$RDYQ_CONS_HDR,-8(%rcx)
	movq	%rax,(%rcx)
	movq	%rdi,(%rcx)
	movq	VP_RDYQ_TL(%rsi),%rax
	movq	%rax,(%rcx)
	movq	%rcx,VP_RDYQ_TL(%rsi)
	leaq	RDYQ_CONS_SZB(%rcx),%rcx
	ret

/* ASM_Dequeue:
 *
 * This function dequeues an element from the calling vproc's ready queue.
 *
 *	%rax	-- result points to dequeued item
 *	%rsi	-- host vproc (reserved across the call)
 */
	.text
	.p2align 3
	.global	ASM_Dequeue
ASM_Dequeue:
