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

// 0xDEADBED == 233495533
#define INVALID_RET_ADDR1 233495533

// 0xCAFEBED == 212855789
#define INVALID_RET_ADDR2 212855789

    .text  
    
.p2align 3
.globl	_GSYM(ASM_DS_ApplyClos)
_GSYM(ASM_DS_ApplyClos):
    // this is a special manticore routine used to start a NewStack.
    popq    TMP_REG1                // retrieve the closure from our stack frame
                                    // and adjust SP so it points to an invalid ret addr.
    movq    (TMP_REG1), STD_EP_REG  // set EP
    movq    8(TMP_REG1), TMP_REG2   // grab the CP
    jmp     *TMP_REG2
    

// Apply_StdDS using the given stackptr.
// note that the stackptr is assumed to be
// pointing to the return address.
    .p2align 3
    .globl	_GSYM(ASM_Apply_StdDS_WithStk)
_GSYM(ASM_Apply_StdDS_WithStk):
    movq    %r9, %rsp
    jmp     _GSYM(ASM_Apply_StdDS)
    
    
// Apply_StdDS on the current stack, but
// returning to C is an unexpected error so
// we overwrite the return address.
    .p2align 3
    .globl	_GSYM(ASM_Apply_StdDS_NoRet)
_GSYM(ASM_Apply_StdDS_NoRet):
    movq    $INVALID_RET_ADDR1, (%rsp)
    jmp     _GSYM(ASM_Apply_StdDS)
  
      
// used by RequestService to invoke the scheduler,
// since RequestService is already running on a
// Manticore stack, and has a pointer to the stack's lock.
    .p2align 3
    .globl	_GSYM(ASM_Apply_StdDS)
_GSYM(ASM_Apply_StdDS):
    // setup standard convention registers
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
    movq	ALLOC_PTR(VPROC_PTR_REG),ALLOC_PTR_REG
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
    // set the stack ptr. set the stack descriptor in the vproc.
    movq    8(STD_EP_REG), %rsp
    movq    16(STD_EP_REG), TMP_REG1
    movq    TMP_REG1, STD_CONT(VPROC_PTR_REG)
    ret                             // do a pop;jmp to resume invoke the stack
    
    
    
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



// stack descriptor offsets
#define CUR_SP      0
#define PREV_SEG    8
#define LIST_NEXT   16
#define INIT_SP     24

// probably should be a compiler parameter
#define MAX_FRAMES  4



// This version is for segmented stacks a la Bruggeman et al.
.p2align 3
.globl	_GSYM(__manti_growstack)
_GSYM(__manti_growstack):
    leaq    8(%rsp), %r15   // clobber r15 to save the current top of segment
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
    subq    $8, %rsp        // realign
    // setup args
    movq    VPROC_PTR_REG, %rdi     // vproc is arg 1
    movq    %r15, %rsi              // current top is arg 2
    callq   _GSYM(StkSegmentOverflow)
    
    movq    %rax, %r15  // move new SP out of the way
    
    // restore manticore registers
    addq    $8, %rsp
    popq    %r10
    popq    %rcx
    popq    %rdx
    popq   STD_ARG_REG
    popq   STD_EXH_REG
    popq   STD_EP_REG
    popq   VPROC_PTR_REG
    popq   ALLOC_PTR_REG
    
    // swap out the stack pointer
    xchgq   %rsp, %r15
    movq    (%r15), %r15 // load return addr
    jmpq    *%r15        // resume



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
    
    /***** alloc cont object ****/
    
    // get additional values
    movq    STD_CONT(VPROC_PTR_REG), TMP_REG1     // load the stack descriptor
    leaq    _GSYM(ASM_LongJmp)(%rip), TMP_REG2    // load the resume address
    
    // initialize allocation
    movq    $STACK_HDR, -8(ALLOC_PTR_REG)       // init header
    movq    TMP_REG2, (ALLOC_PTR_REG)           // init code ptr
    movq    %rsp, 8(ALLOC_PTR_REG)              // save stack ptr
    movq    TMP_REG1, 16(ALLOC_PTR_REG)         // save stack descriptor
    
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
    pushq   $INVALID_RET_ADDR2
    movq    $REQ_Return, TMP_REG1
    jmp     asm_get_service
    

    .p2align 3
    .globl	_GSYM(ASM_DS_UncaughtExn)
_GSYM(ASM_DS_UncaughtExn):
    movq    $REQ_UncaughtExn, TMP_REG1
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
    movq    STD_EXH_REG, STD_EXH(VPROC_PTR_REG)
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
