/*  mantentry.s
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 *  Glue code for executing CFG code via C.
 */

	.global mantentryglue
	.global returnloc

	.text
mantentryglue:
	movq %rdi, %rax  /* initialize the argument register */
	xorl %edi, %edi  /* nil closure register */
	xorl %ebx, %ebx  /* nil exception handler */
	/* save the callee-save regs*/
	pushq %rbx
	pushq %rbp
	pushq %r12
	pushq %r13
	pushq %r14
	pushq %r15
	/* */
	subq $2048, %rsp  /* allocate space for spills */
	movq %rsi, %rbp  /* use %rbp as the limit pointer */
	jmp mantentry    /* entry function for CFG programs */
returnloc:		
	addq $2048, %rsp  /* de-allocate the spill area */
	/* restore the callee-save regs*/
	popq %r15
	popq %r14
	popq %r13
	popq %r12
	popq %rbp	
	popq %rbx
	/* */	
	ret
	