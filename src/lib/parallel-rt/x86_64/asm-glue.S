/* asm-glue.S
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

/* Stack frame layout */
#define SPILL_SZB	2048	/* for register spilling */
#define SAVE_AREA	(5*8)	/* for callee saves %rbx, %r12-%r15 */
#define PAD_SZB		8	/* pad so that frame size (plus saved PC and FP */
				/* is a multiple of 16 bytes */
#define FRAME_SZB	(SPILL_SZB+SAVE_AREA+PAD_SZB)
#define RBX_OFF		(-2*8)	/* offset off %rbp for saving %rbx */
#define R12_OFF		(-3*8)	/* offset off %rbp for saving %r12 */
#define R13_OFF		(-4*8)	/* offset off %rbp for saving %r13 */
#define R14_OFF		(-5*8)	/* offset off %rbp for saving %r14 */
#define R15_OFF		(-6*8)	/* offset off %rbp for saving %r15 */


/* ASM_Apply:
 *
 *	ReturnCode_t ASM_Apply (
 *	    VProc_t *vp,	-- host vproc; in %rdi
 *	    Addr_t cp,		-- code address of function; in %rsi
 *	    Value_t arg,	-- function argument; in %rdx
 *	    Value_t ep,		-- function environment ptr.; in %rcx
 *	    Value_t rk,		-- function return continuation; in %r8
 *	    Value_t ek)		-- function exception continuation; in %r9
 */
	.text
	.p2align 3
	.global	ASM_Apply
ASM_Apply:
	pushq	%rbp			/* save frame pointer */
	movq	%rsp,%rbp		/* set new frame pointer */
	leaq	FRAME_SZB(%rsp),%rsp	/* allocate the stack frame */
      /* save C callee-save registers */
	movq	%rbx,RBX_OFF(%rbp)
	movq	%r12,R12_OFF(%rbp)
	movq	%r13,R13_OFF(%rbp)
	movq	%r14,R14_OFF(%rbp)
	movq	%r15,R15_OFF(%rbp)
      /* setup standard arguments to the Manticore function */
	movq	%rdi,%r10		/* save vproc pointer in %r10 */
	movq	%rsi,%r12		/* save code pointer in %r12 */
	movq	%rdx,%rax		/* standard argument */
	movq	%rcx,%rdi		/* environment pointer */
	movq	%r8,%rsi		/* return continuation */
	movq	%r9,%rbx		/* exception continuation */
      /* load Manticore state */
	movq	ALLOC_PTR(%r10),%rcx	/* load allocation pointer */
	movq	LIMIT_PTR(%r10),%r11	/* limit pointer */
	movq	$M_TRUE,IN_MANTICORE(%r10) /* we are running manticore code now! */
	jmp	*%r12

/* code address for uncaught exception continuation */
	.p2align 3
	.global	ASM_UncaughtExn
ASM_UncaughtExn:
	movq	$REQ_UncaughtExn, %r9
	jmp	switch_to_c

/* code address for return continuation */
	.p2align 3
	.global	ASM_Return
ASM_Return:
	movq	$REQ_Return, %r9
	jmp	switch_to_c

	.p2align 3
	.global	ASM_InvokeGC
ASM_InvokeGC:
	movq	$REQ_GC, %r9

switch_to_c:
	/* the sequential GC does not store the VProc at the base of its heap */
#ifdef SEQUENTIAL_RT
	call	VProcSelf
	movq	%rax,%r10
#else
      /* compute the vproc pointer from the allocation pointer */
	movq	%rcx,%r10
	andq	$VP_MASK,%r10
#endif
      /* mark that we are not running Manticore code anymore */
	movq	$M_FALSE,IN_MANTICORE(%r10)
      /* save Manticore state */
	movq	%rax,STD_ARG(%r10)
	movq	%rdi,STD_EP(%r10)
	movq	%rsi,STD_CONT(%r10)
	movq	%rbx,STD_EXH(%r10)
	movq	%rcx,ALLOC_PTR(%r10)
      /* restore C callee-save registers */
	movq	RBX_OFF(%rbp),%rbx
	movq	R12_OFF(%rbp),%r12
	movq	R13_OFF(%rbp),%r13
	movq	R14_OFF(%rbp),%r14
	movq	R15_OFF(%rbp),%r15
      /* return request code */
	movq	%r9,%rax
	leave
	ret

