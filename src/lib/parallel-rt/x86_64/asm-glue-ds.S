/* asm-glue-ds.S
 *
 * COPYRIGHT (c) 2017 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 */

#ifdef DIRECT_STYLE

#include "asm-defs.h"
#include "asm-offsets.h"
#include "registers.h"

/* regs rarely used by manticore (11th-13th args)
   and unused by C call-convention. */
#define TMP_REG1            %r13    
#define TMP_REG2            %r14
#define TMP_REG3            %r15
#define TMP_REG4            %r12

// stack descriptor offsets
#define CUR_SP      0
#define PREV_SEG    8
#define LIST_NEXT   16
#define INIT_SP     24
#define STK_LIM     32
#define LIST_PREV   40
#define AGE         56

    .text  
    
.p2align 3
.globl	_GSYM(ASM_DS_StartStack)
_GSYM(ASM_DS_StartStack):
    // this is a special manticore routine used to start a NewStack.
    popq    TMP_REG1                // retrieve the closure from our stack frame
                                    // and adjust SP so it points to an invalid ret addr.
    movq    (TMP_REG1), STD_EP_REG  // set EP
    movq    8(TMP_REG1), TMP_REG2   // grab the CP
    jmp     *TMP_REG2
    

// used by RequestService to call a function on the given stack,
// and additionally pop any data RequestService placed on the stack.
    .p2align 3
    .globl	_GSYM(ASM_DS_Apply)
_GSYM(ASM_DS_Apply):
    // setup standard convention registers
    movq    %r9, %rsp
    movq    %rdi, VPROC_PTR_REG
    movq    %rsi, TMP_REG1      // code ptr
    movq    %rdx, STD_EP_REG
    movq    %rcx, STD_EXH_REG
    movq    %r8, STD_ARG_REG
    movq	ALLOC_PTR(VPROC_PTR_REG),ALLOC_PTR_REG
    // call the function
    jmp     *TMP_REG1
    

// called by RequestService to return from GC
    .p2align 3
    .globl	_GSYM(ASM_Resume_Stack)
_GSYM(ASM_Resume_Stack):
    // setup standard convention registers
    movq    %rdi, VPROC_PTR_REG
    movq    %rsi, %rsp          // change the stack ptr
    movq    %rdx, STD_EP_REG
    movq    %rcx, STD_EXH_REG
    movq    %r8, STD_ARG_REG
    movq    ALLOC_PTR(VPROC_PTR_REG),ALLOC_PTR_REG
    ret
    
.p2align 3
.globl	_GSYM(ASM_LongJmp)
_GSYM(ASM_LongJmp):
    // setup args
    movq    STD_ARG_REG, %rdx       // shift the arg to the 2nd arg reg
    movq    $0, STD_ARG_REG         // set 1st arg reg to 0 indicating an escape throw
    // **** fall-through to EscapeThrow ****
    
.p2align 3
.globl	_GSYM(ASM_DS_EscapeThrow)
_GSYM(ASM_DS_EscapeThrow):
    // set the stack ptr.
    movq    8(STD_EP_REG), %rsp
    // set stack descriptor
    movq    16(STD_EP_REG), %rbp
    movq    %rbp, STD_CONT(VPROC_PTR_REG)
#ifdef SEGSTACK
    // set the stack limit
    movq    STK_LIM(%rbp), %rbp
    movq    %rbp, STD_EP(VPROC_PTR_REG)
#endif
    // do a pop;jmp to resume the stack
    ret
    
    
    
/*

    caller2 addr
    ==========      -|
        ...          |     frame sz
     frame sz        |
     watermark      _|
    caller addr
    ===========
    resume addr
    -----------  <- RSP
*/

// This version is for segmented stacks a la Bruggeman et al.
.p2align 3
.globl	_GSYM(__manti_growstack)
_GSYM(__manti_growstack):
    // NOTE we actually start off with RSP 16-byte aligned!
#ifdef NOCOPY_OVERFLOW
    movq    $0, TMP_REG3
#else
    movq    $1, TMP_REG3      // 0 indicates no copying. NOTE the clobber!
#endif
    
    
_GSYM(__manti_growstack_noimplicitcopy):
    
    leaq    8(%rsp), %rbp   // save the current top of segment
    // save manticore registers
    // NOTE: we are omitting xmm registers here!
    pushq   ALLOC_PTR_REG
    pushq   VPROC_PTR_REG
    pushq   STD_EP_REG
    pushq   STD_EXH_REG
    pushq   STD_ARG_REG
    pushq   %rdx            // arg 2
    pushq   %rcx            // arg 3
    pushq   %r10            // arg 4
    // the rest of the manticore registers are 
    // saved by the C callee
    // setup args
    movq    VPROC_PTR_REG, %rdi     // vproc is arg 1
    movq    %rbp, %rsi              // current top is arg 2
    movq    TMP_REG3, %rdx              // whether to copy is arg 3
    callq   _GSYM(StkSegmentOverflow)
    
    movq    %rax, %rbp  // move new SP out of the way
    
    // restore manticore registers
    popq    %r10
    popq    %rcx
    popq    %rdx
    popq   STD_ARG_REG
    popq   STD_EXH_REG
    popq   STD_EP_REG
    popq   VPROC_PTR_REG
    popq   ALLOC_PTR_REG
    
    // swap out the stack pointer
    xchgq   %rsp, %rbp
    movq    (%rbp), %rbp // load return addr
    jmpq    *%rbp        // resume


// This version is for segmented stacks a la Bruggeman et al.
.p2align 3
.globl	_GSYM(ASM_DS_SegUnderflow)
_GSYM(ASM_DS_SegUnderflow):    
    // grab current segment
    movq    STD_CONT(VPROC_PTR_REG), TMP_REG1

#ifndef LAZY_UNDERFLOW
    /////////
    // remove current segment from allocated list, 
    // which is doubly linked to support this operation.
    // refer to FreeStacks in minor-gc.c
    /////////
    
    movq    LIST_NEXT(TMP_REG1), TMP_REG2  // get allocdNext
    movq    LIST_PREV(TMP_REG1), TMP_REG3  // get allocdPrev
    
    // allocdNext == NULL?
    testq   TMP_REG2, TMP_REG2
    jz  nextIsNull
    // allocdNext->prev = allocdPrev
    movq    TMP_REG3, LIST_PREV(TMP_REG2)
nextIsNull:
    // allocdPrev == NULL?
    testq   TMP_REG3, TMP_REG3
    jz      prevIsNull
    // allocdPrev->next = allocdNext
    movq    TMP_REG2, LIST_NEXT(TMP_REG3)
    jmp     prevTestJoin
    
prevIsNull:
    // vp->allocdStacks = allocdNext
    movq    TMP_REG2, ALLOCD_STKS(VPROC_PTR_REG)

prevTestJoin:

    // now, we demote the segment we removed
    movq    $0, AGE(TMP_REG1)         // info->age = AGE_Minor
    movq    $0, LIST_PREV(TMP_REG1)   // info->prev = NULL
    // push it on the free list
    movq    FREE_STKS(VPROC_PTR_REG), TMP_REG2
    movq    TMP_REG2, LIST_NEXT(TMP_REG1)
    movq    TMP_REG1, FREE_STKS(VPROC_PTR_REG)
    
    // done with freeing the current segment
    /////////////
#endif
    
    // grab the previous segment descriptor 
    // from the current segment
    movq    PREV_SEG(TMP_REG1), TMP_REG1
    
    // install the segment descriptor
    movq    TMP_REG1, STD_CONT(VPROC_PTR_REG)
    
    // change the SP and limit, then return
    movq    CUR_SP(TMP_REG1), %rsp
    movq    STK_LIM(TMP_REG1), TMP_REG1
    movq    TMP_REG1, STD_EP(VPROC_PTR_REG)
    retq
    


// note: this has been computed manually for the object below (len = 3 words)
// hdr = (len << 16 | 2 << 1 | 1)
#define STACK_HDR    196613

/*
    the layout of stack continuation is as follows
    ======================
          GC header
    ----------------------
          CODE PTR           <------ cont ptr
    ----------------------
        stack ptr
    ----------------------
     stack descriptor ptr 
    ======================
        high addresses
*/
.p2align 3
.globl	_GSYM(ASM_Callec)
_GSYM(ASM_Callec):  /* uses the direct-style manticore calling convention */

    movq    STD_CONT(VPROC_PTR_REG), TMP_REG1     // grab the stack descriptor
    
#ifdef SEGSTACK
    movq    %rsp, TMP_REG4                      // save the SP
    // switch to a new segment without copying
    xorq    TMP_REG3, TMP_REG3              // 0 in TMP_REG3 indicates no copying
    callq   _GSYM(__manti_growstack_noimplicitcopy)
    // at this point we're operating in a new stack segment.
    // TMP_REG1 and TMP_REG4 were preserved by growstack.
#endif

    /***** alloc cont object ****/
    leaq    _GSYM(ASM_LongJmp)(%rip), TMP_REG2    // load the resume address
    
    // ** initialize allocation **
    movq    $STACK_HDR, -8(ALLOC_PTR_REG)       // init header
    movq    TMP_REG2, (ALLOC_PTR_REG)           // init code ptr
    
#ifdef SEGSTACK
    movq    TMP_REG4, 8(ALLOC_PTR_REG)          // init captured stack ptr
#else
    movq    %rsp, 8(ALLOC_PTR_REG)              // capture current stack ptr
#endif
    movq    TMP_REG1, 16(ALLOC_PTR_REG)         // init stack descriptor
    // ** done with initialization **
    
    // update pointers
    movq    ALLOC_PTR_REG, TMP_REG3             // save ptr to the cont object
    addq    $32, ALLOC_PTR_REG                  // bump alloc ptr, (numItems * 8) + 8
    
    /***** invoke the function we were given *****/
    movq    (STD_ARG_REG), STD_EP_REG       // load environment pointer
    movq    8(STD_ARG_REG), TMP_REG1        // load code pointer
    movq    TMP_REG3, STD_ARG_REG           // pass the cont object
    jmp     *TMP_REG1                       // do the call, leaving stack pointer as-is
    
    
    
    .p2align 3
    .globl	_GSYM(ASM_DS_Return)
_GSYM(ASM_DS_Return):
    // the Manticore main function returns here.
    // When it does perform a retq on this address,
    // the stack is 16-byte aligned, and we
    // push a dummy return address before jumping
    // to RequestService to preserve ABI requirements.
    leaq    _GSYM(InvalidReturnAddr)(%rip), TMP_REG1
    pushq   TMP_REG1
    movq    $REQ_Return, TMP_REG1
    jmp     asm_get_service
    

    .p2align 3
    .globl	_GSYM(ASM_DS_UncaughtExn)
_GSYM(ASM_DS_UncaughtExn):
    movq    $REQ_UncaughtExn, TMP_REG1
    jmp     asm_get_service
    

    .p2align 3
    .globl	_GSYM(ASM_DS_VProcSleep)
_GSYM(ASM_DS_VProcSleep):
    movq    $REQ_Sleep, TMP_REG1
    jmp     asm_get_service


/* NOTE: this function only accepts vproc and alloc ptrs.
   might be worth clearing the other registers so they're 
   not seen as ptrs. Do that also in the CPS version.
   those fields are not checked for roots anyways though. */
   
    .p2align 3
    .globl	_GSYM(ASM_InvokeGC_DS_LLVM)
_GSYM(ASM_InvokeGC_DS_LLVM):
    movq    $REQ_GC, TMP_REG1
    jmp     asm_get_service


asm_get_service:
// live-ins
// 1. these std-ds calling convention registers: 
//      alloc, vproc, exh, stdArg
// 2. TMP_REG1 -- request code
    // save the current stack cont to TMP_REG2
    // leaq    -8(%rsp), TMP_REG2  
    
    // initialize metadata
    // subq    $16, %rsp
    // movq    $STACK_HDR_STKDATA, (%rsp)
    // movq    $1, 8(%rsp)     // 1 == LOCKED

      /* save Manticore state */
    movq    ALLOC_PTR_REG, ALLOC_PTR(VPROC_PTR_REG)
    movq    STD_EXH_REG, STD_EXH(VPROC_PTR_REG)   // NOTE I don't think we need to save this reg.
    movq    STD_ARG_REG, STD_ARG(VPROC_PTR_REG)
    movq    %rsp, STD_EP(VPROC_PTR_REG)
    /* in essence, the environment (with all roots)
       is the stack. the stack's descriptor should
       already be in STD_CONT. */
    
    /* move vproc and request code to the right registers */
    movq    VPROC_PTR_REG, %rdi // rdi = arg 1
    movq    TMP_REG1, %rsi      // rsi = arg 2
    jmp     _GSYM(RequestService)


#endif   // DIRECT_STYLE
