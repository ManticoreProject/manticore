/* asm-glue-ds.S
 *
 * COPYRIGHT (c) 2017 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 */

#include "asm-defs.h"
#include "asm-offsets.h"
#include "registers.h"
// TODO move the GC header below into header-bits.h
// #include "header-bits.h"

/* regs rarely used by manticore (11th-13th args)
   and unused by C call-convention. */
#define TMP_REG1            %r13    
#define TMP_REG2            %r14
#define TMP_REG3            %r15

// TODO get the right value
#define STACK_HDR   12345

    .text
    .p2align 3
    .globl	_GSYM(ASM_DS_Return)
_GSYM(ASM_DS_Return):   // the Manticore main function returns here.
    movq    $REQ_Return, TMP_REG1
    jmp     asm_get_service
    

.p2align 3
.globl	_GSYM(ASM_DS_UncaughtExn)
_GSYM(ASM_DS_UncaughtExn):   // the Manticore main function returns here.
movq    $REQ_UncaughtExn, TMP_REG1
jmp     asm_get_service

/*
    ~~~~~~~~~~~~~~~~~~~
    statepoint-saved roots
    such as clos, exh, etc.
    ===================
      manti ret addr    
    ------------------- 
        Stack Lock
    ------------------- <-- ptr to this stack cont
        GC Header
    ===================
                        low addrs
*/

/* TODO consider writing a magic value to where
   the return address would be after adding an addl 16 bytes
   to the metadata. for debugging purposes. */

asm_get_service:
// live-ins
// 1. these std-ds calling convention registers: 
//      alloc, vproc, exh, stdArg
// 2. TMP_REG1 -- request code
    // save the current stack cont to TMP_REG2
    leaq    -8(%rsp), TMP_REG2  
    
    // initialize metadata
    subq    $16, %rsp
    movq    $STACK_HDR, (%rsp)
    movq    $1, 8(%rsp)     // 1 == LOCKED
    
    /* TODO reorder the args of RequestService
       and pass via registers */
    
      /* save Manticore state */
    movq    ALLOC_PTR_REG, ALLOC_PTR(VPROC_PTR_REG)
    movq    TMP_REG2, STD_CONT(VPROC_PTR_REG)
    movq    STD_EXH_REG, STD_EXH(VPROC_PTR_REG)
    movq    STD_ARG_REG, STD_ARG(VPROC_PTR_REG)
    /* NOTE one thing we don't set is the STD_EP field,
       since the roots are already in the stack. */
    
    /* move vproc and request code to the right registers */
    movq    VPROC_PTR_REG, %rdi // rdi = arg 1
    movq    TMP_REG1, %rsi      // rsi = arg 2
    jmp     _GSYM(RequestService)


retry_resume:
    pause
    pause
    // fall-through to ResumeStack

.p2align 3
.globl	_GSYM(ASM_DS_ResumeStack)
_GSYM(ASM_DS_ResumeStack):
    // args: vp, stack cont, stdarg, exh, ep
    // NOTE this function performs a longjmp, discarding
    // the current stack pointer
    
    cmpq    $0, (%rsi)
    jne     retry_resume
    
    /* set stack ptr */
    leaq    8(%rsi), %rsp
    // rsi is now dead
    
    /* restore manticore registers.
       be careful about order */
    movq    %rdi, VPROC_PTR_REG
    movq    %rdx, STD_ARG_REG
    movq    %rcx, STD_EXH_REG
    movq    %r8, STD_EP_REG
    movq    ALLOC_PTR(VPROC_PTR_REG), ALLOC_PTR_REG
    ret
    
    
