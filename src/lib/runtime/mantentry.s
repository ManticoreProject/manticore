/*  mantentry.s
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 *  Glue code for executing CFG code via C.
 */
	
	.global mantentryglue

	.text
mantentryglue:
	movq %rdi, %rax
	xorl %rdi, %rdi
	xorl %rbx, %rbx
	jmp mantentry    /* entry function for CFG programs */
returnloc:
	ret
	