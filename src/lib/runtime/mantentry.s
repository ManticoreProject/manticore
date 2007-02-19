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
	movq %rdi, %rax
	xorl %edi, %edi
	xorl %ebx, %ebx
	/* save the callee-save regs*/
	pushq %rbx
	pushq %rbp
	pushq %r12
	pushq %r13
	pushq %r14
	pushq %r15
	/* */
	jmp mantentry    /* entry function for CFG programs */
returnloc:
	/* restore the callee-save regs*/
	popq %r15
	popq %r14
	popq %r13
	popq %r12
	popq %rbp	
	popq %rbx
	/* */	
	ret
	