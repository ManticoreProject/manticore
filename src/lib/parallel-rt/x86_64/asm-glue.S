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

/* ASM_ProxyCont
 *
 * Code for the proxy continuation. It will check if the proxied continuation
 * is in the local or global heap. In the prior case we retrieve the object's
 * location in the local heap from the table and then zero out the entry before
 * throwing it.
 *
 * NOTES: The vproc throwing this continuation must be the same as its creator, so
 * we do not check this. Additionally, the offsets on r11 are lifted from an auto-
 * generated file of vproc data structure offsets, so right now these are fragile!
 */
 	.text
	.p2align 3
	.globl	_GSYM(ASM_ProxyCont)
_GSYM(ASM_ProxyCont):
	movq	8(%rdi), %rdx		/* place ptr to proxy in rdx */
	movslq	156(%r11), %r10		/* place vproc->maxproxy in r10 */
	movq	8(%rdx), %rdx		/* rdx = rdx[1], now is 2nd field of proxy */

	/* if rdx >= r10, thus proxy was promoted, throw continuation */
	cmpq	%r10, %rdx
	movq	%rdx, %r12			/* save ptr to proxy in r12 */
	jge	throwContinuation

	/* fetch proxy from table */
	movl	152(%r11), %r13d
	leaq	(%rdx,%rdx), %rbx
	movq	160(%r11), %r10
	subl	$1, %r13d
	leal	(%r13,%r13), %ecx
	leaq	8(%r10,%rbx,8), %r14
	movslq	%ecx, %rcx

	/* if it was the last entry, we don't move an entry in the table */
	cmpq	%rbx, %rcx
	movq	(%r14), %r12		
	movl	%r13d, 152(%r11)	/* update proxy table entry count in vproc. */
	je	throwContinuation

	/* relocate the last entry and update its proxy */
	movq	(%r10,%rcx,8), %r13
	movq	%rdx, 8(%r13)
	movq	%r13, (%r10,%rbx,8)
	movq	8(%r10,%rcx,8), %rdx
	movq	%rdx, (%r14)

throwContinuation:
	/* throw the proxied continuation's closure pointer currently in r12 */
	movq	(%r12), %rcx	/* get code pointer from closure and put in rcx */
	movq	%r12, %rdi		/* set environment to be that closure */
	jmp	*%rcx



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
	movq	%rdi,VPROC_PTR_REG	/* save vproc pointer */
	movq	%rsi,%r12		/* save code pointer in %r12 */
	movq	%rdx,STD_ARG_REG	/* standard argument */
	movq	%rcx,STD_EP_REG		/* environment pointer */
	movq	%r8,STD_CONT_REG	/* return continuation */	
	movq	%r9,STD_EXH_REG		/* exception continuation */
      /* load Manticore state */
	movq	ALLOC_PTR(VPROC_PTR_REG),ALLOC_PTR_REG	/* load allocation pointer */
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

#ifndef NDEBUG
/* r14 points at the current element of the spill area
 *  r15 points at the last element of the spill area
 */
	.p2align 3
	.globl	_GSYM(ASM_InvalidateSpillArea)
_GSYM(ASM_InvalidateSpillArea):
invalidate_spill_area:
	cmpq	%r14,%r15
	je	continue_to_c
	movq	$0xdeadb,(%r14)
	subq	$8,%r14
	jmp	invalidate_spill_area
#endif
	
	.p2align 3
	.globl	_GSYM(ASM_InvokeGC)
_GSYM(ASM_InvokeGC):
	movq	$REQ_GC, TMP_REG1

switch_to_c:
#ifndef NDEBUG
/* FIXME: disable this check once we've fixed the issues with missing roots */
/* consistency check for the garbage collector: overwrite all the entries in the spill area,
 * all none of these entries should be live at this point.
 */
	movq	%rbp,%r14
	subq	$56,%r14
	movq	%rbp,%r15
	subq	$2104,%r15
	jmp 	invalidate_spill_area
continue_to_c:		
#endif
      /* save Manticore state */
	movq	STD_ARG_REG,STD_ARG(VPROC_PTR_REG)
	movq	STD_EP_REG,STD_EP(VPROC_PTR_REG)
	movq	STD_CONT_REG,STD_CONT(VPROC_PTR_REG)
	movq	STD_EXH_REG,STD_EXH(VPROC_PTR_REG)
	movq	ALLOC_PTR_REG,ALLOC_PTR(VPROC_PTR_REG)
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

#ifdef ENABLE_LOGGING
/* void ASM_GetTimeStamp (void *ts)
 *
 * Fill in the time stamp using a system-dependent call.
 */
	.p2align 3
	.globl	_GSYM(ASM_GetTimeStamp)
_GSYM(ASM_GetTimeStamp):
	pushq	%rbp
	movq	%rsp, %rbp

#if defined(HAVE_MACH_ABSOLUTE_TIME)
    /* ts->ts_mach = mach_absolute_time(); */
	pushq	%rbx
	subq	$8, %rsp
	movq	%rdi, %rbx
	call	_GSYM(mach_absolute_time)
	movq	%rax, (%rbx)
	addq	$8, %rsp
	popq	%rbx
#elif defined(HAVE_CLOCK_GETTIME)
    /* struct timespec t;
     * clock_gettime (CLOCK_REALTIME, &t);
     * ts->sec = t.tv_sec;
     * ts->frac = t.tv_nsec;
     */
	pushq	%rbx
	movq	%rdi, %rbx
	xorl	%edi, %edi
	subq	$16, %rsp
	movq	%rsp, %rsi
	call	_GSYM(clock_gettime)
	movq	(%rsp), %rax
	movl	%eax, (%rbx)
	movq	8(%rsp), %rax
	movl	%eax, 4(%rbx)
	addq	$16, %rsp
	popq	%rbx
#else
    /* struct timeval t;
     * gettimeofday (&t, 0);
     * ts->sec = t.tv_sec;
     * ts->frac = t.tv_usec;
     */
	pushq	%rbx
	movq	%rdi, %rbx
	leaq	-32(%rbp), %rdi
	subq	$24, %rsp
	xorl	%esi, %esi
	call	_GSYM(gettimeofday)
	movq	-32(%rbp), %rax
	movl	%eax, (%rbx)
	movl	-24(%rbp), %eax
	movl	%eax, 4(%rbx)
	addq	$24, %rsp
	popq	%rbx
#endif

	leave
	ret

#endif /* ENABLE_LOGGING */
