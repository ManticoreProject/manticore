/* asm-glue-ds.S
 *
 * COPYRIGHT (c) 2017 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 */

#include "asm-defs.h"
#include "asm-offsets.h"
#include "registers.h"

/* regs rarely used by manticore (11th-13th args)
   and unused by C call-convention. */
#define TMP_REG1            %r13    
#define TMP_REG2            %r14
#define TMP_REG3            %r15

// 0xDEADBED == 233495533
#define INVALID_RET_ADDR1 233495533

// 0xCAFEBED == 212855789
#define INVALID_RET_ADDR2 212855789

    .text  

// Apply_StdDS using the given stackptr.
// note that the stackptr is assumed to be
// pointing to the 16-byte aligned return address.
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
    
    
    
/*
    the EP of this function is as follows:
    ====================
        &ASM_SwitchFrom    <------ EP
    --------------------
     toStack's stack cont Ptr
    --------------------
    toStack's allocation
       record entry Ptr
    ====================
        high addresses
        
    it is used to resume a preempted stack.
*/
    .p2align 3
    .globl	_GSYM(ASM_SwitchFrom)
_GSYM(ASM_SwitchFrom):
// calling convention is ds-stdfun
// the arg is the stack we're switching from.
// the stack we will try to switch to is
// already in our environment.

/* NOTE we should probably return to our caller
   in the case of the stack being claimed by some
   other thread? The difficulty is that
   if we unlocked our own thread, and then it was
   taken away while we were trying to acquire another
   stack, we're hosed. To support work-stealing
   I think we would need a more complicated protocol.
 */
    
    movq    (STD_ARG_REG), TMP_REG1  // unbox the arg
    movq    $0, (TMP_REG1)    // unlock the current stack
    movq    8(STD_EP_REG), TMP_REG2  // grab the target stack
    jmp     do_switch

retry_switch:
    pause
    pause
do_switch:
/* NOTE probably need a cmpxchg if there's a chance two
   threads will try unlocking the same stack,
   to mark it as claimed?  */
    cmpq    $0, (TMP_REG2)  // make sure its unlocked
    jne     retry_switch
    
    // now we can switch
    leaq    8(TMP_REG2), %rsp
    
    // set ep, arg, and exh to unit
    movq    $0, STD_ARG_REG
    movq    $0, STD_EP_REG
    movq    $0, STD_EXH_REG
    ret
    
/////////////////////////////////////////
    
    







    
    
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
    movq    $STACK_HDR_STKDATA, (%rsp)
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
