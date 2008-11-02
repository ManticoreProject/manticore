/* asm-glue.S
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 */

#include "asm-defs.h"
#include "asm-offsets.h"
#include "registers.h"	

#define TMP_REG1            %r10          /* must be caller save */
#define TMP_REG2            %r15          /* must be caller save */	

/* Stack frame layout */
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
	.globl	_GSYM(ASM_Apply)
_GSYM(ASM_Apply):
	pushq	%rbp			/* save frame pointer */
	movq	%rsp,%rbp		/* set new frame pointer */
	leaq	-FRAME_SZB(%rsp),%rsp	/* allocate the stack frame */
      /* save C callee-save registers */
	movq	%rbx,RBX_OFF(%rbp)
	movq	%r12,R12_OFF(%rbp)
	movq	%r13,R13_OFF(%rbp)
	movq	%r14,R14_OFF(%rbp)
	movq	%r15,R15_OFF(%rbp)
      /* setup standard arguments to the Manticore function */
	movq	%rdi,%r10		/* save vproc pointer in %r10 */
	movq	%rsi,%r12		/* save code pointer in %r12 */
	movq	%rdx,STD_ARG_REG	/* standard argument */
	movq	%r8,STD_CONT_REG	/* return continuation */	
	movq	%rcx,STD_EP_REG		/* environment pointer */
	movq	%r9,STD_EXH_REG		/* exception continuation */
      /* load Manticore state */
	movq	ALLOC_PTR(%r10),ALLOC_PTR_REG	/* load allocation pointer */
	movq	LIMIT_PTR(%r10),LIMIT_PTR_REG	/* limit pointer */
	movq	$M_TRUE,IN_MANTICORE(%r10) /* we are running manticore code now! */
	jmp	*%r12

/* code address for uncaught exception continuation */
	.p2align 3
	.globl	_GSYM(ASM_UncaughtExn)
_GSYM(ASM_UncaughtExn):
	movq	$REQ_UncaughtExn, TMP_REG1
	jmp	switch_to_c

/* code address for return continuation */
	.p2align 3
	.globl	_GSYM(ASM_Return)
_GSYM(ASM_Return):
	movq	$REQ_Return, TMP_REG1
	jmp	switch_to_c

/* code address for putting the host vproc to sleep */
	.p2align 3
	.globl	_GSYM(ASM_VProcSleep)
_GSYM(ASM_VProcSleep):
	movq	$REQ_Sleep, TMP_REG1
	jmp	switch_to_c

	.p2align 3
	.globl	_GSYM(ASM_InvokeGC)
_GSYM(ASM_InvokeGC):
	movq	$REQ_GC, TMP_REG1

switch_to_c:
	/* the sequential GC does not store the VProc at the base of its heap */
#ifdef SEQUENTIAL_RT
	call	VProcSelf
	movq	%rax,TMP_REG2
#else
      /* compute the vproc pointer from the allocation pointer */
	movq	ALLOC_PTR_REG,TMP_REG2
	andq	$VP_MASK,TMP_REG2
#endif
      /* mark that we are not running Manticore code anymore */
	movq	$M_FALSE,IN_MANTICORE(TMP_REG2)
      /* save Manticore state */
	movq	STD_ARG_REG,STD_ARG(TMP_REG2)
	movq	STD_EP_REG,STD_EP(TMP_REG2)
	movq	STD_CONT_REG,STD_CONT(TMP_REG2)
	movq	STD_EXH_REG,STD_EXH(TMP_REG2)
	movq	ALLOC_PTR_REG,ALLOC_PTR(TMP_REG2)
      /* restore C callee-save registers */
	movq	RBX_OFF(%rbp),%rbx
	movq	R12_OFF(%rbp),%r12
	movq	R13_OFF(%rbp),%r13
	movq	R14_OFF(%rbp),%r14
	movq	R15_OFF(%rbp),%r15
      /* return request code */
	movq	TMP_REG1,%rax
	leave
	ret

