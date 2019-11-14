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
#include "header-bits.h"

/* registers that are unused in manticore's calling convention. */
#define TMP_REG0            %rbx     // NOTE: callee-saved in C-convention
#define TMP_REG1            %rcx

// stack descriptor offsets
#define CUR_SP      0*8
#define PREV_SEG    1*8
#define LIST_NEXT   2*8
#define INIT_SP     3*8
#define STK_LIM     4*8
#define LIST_PREV   5*8
#define DEEP_SCAN   6*8
#define AGE         7*8
#define OWNER       8*8
#define CAN_COPY    9*8
#define CONTEXT    10*8



    .text

.p2align 4, 0xCC
.globl	_GSYM(ASM_DS_StartStack)
_GSYM(ASM_DS_StartStack):
    // this is a special manticore routine used to start a NewStack.
    movq    16(%rsp), TMP_REG1      // retrieve the closure from our stack frame
    addq    $24, %rsp               // pop this frame
    movq    (TMP_REG1), STD_EP_REG  // set EP
    movq    8(TMP_REG1), TMP_REG0   // grab the CP
    jmp     *TMP_REG0


// used by RequestService to call a function on the given stack,
// and additionally pop any data RequestService placed on the stack.
    .p2align 4, 0xCC
    .globl	_GSYM(ASM_DS_Apply)
_GSYM(ASM_DS_Apply):
    // setup standard convention registers
    movq    %r9, %rsp
    movq    %rdi, VPROC_PTR_REG
    movq    %rsi, TMP_REG0      // code ptr
    movq    %rdx, STD_EP_REG
    movq    %rcx, STD_EXH_REG
    movq    %r8, STD_ARG_REG
    movq	ALLOC_PTR(VPROC_PTR_REG),ALLOC_PTR_REG
    // call the function
    jmp     *TMP_REG0


// called by RequestService to return from GC
    .p2align 4, 0xCC
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

.p2align 4, 0xCC
.globl _GSYM(ASM_CallTakeOwnership)
_GSYM(ASM_CallTakeOwnership):
// convention: call/ret
// ASSUMPTION: EscapeThrow is our only caller
// INPUTS: TMP_REG0  -- segment we want to transition to
//         XXX  -- Manticore registers, etc.

    // we need to switch to FFI stack, so we move the return address
    popq  TMP_REG1

    // switch to FFI stack
    movq    FFI_STK(VPROC_PTR_REG), %rsp

    pushq   TMP_REG1  // put return address in the right place to enable a ret
    pushq   TMP_REG0  // save caller's temporary, plus fix alignment

    // save manticore registers
    pushq   ALLOC_PTR_REG
    pushq   VPROC_PTR_REG
    pushq   STD_EP_REG
    pushq   STD_EXH_REG
    pushq   STD_ARG_REG
    pushq   %r10            // arg 2

    // NOTE: no need to save XMM regs since that's not possible to be
    // live in EscapeThrow, which is our only current caller.

    movq    VPROC_PTR_REG, %rdi            // vproc is arg 1
    movq    TMP_REG0, %rsi                 // target segment is arg 2
    callq   _GSYM(TakeOwnership)

    // restore other manticore regs
    popq    %r10
    popq   STD_ARG_REG
    popq   STD_EXH_REG
    popq   STD_EP_REG
    popq   VPROC_PTR_REG
    popq   ALLOC_PTR_REG

    // restore caller's temporary
    popq   TMP_REG0

    // consume the return address that was left on the FFI stack
    retq


.p2align 4, 0xCC
.globl	_GSYM(ASM_LongJmp2)
.globl	_GSYM(ASM_DS_EscapeThrow)
_GSYM(ASM_LongJmp2):
    // setup args
    movq    $0, %r10         // set 2nd arg reg to 0 indicating an escape throw
    // **** fall-through to EscapeThrow ****
.p2align 4, 0x90              // must pad with NOPs!
_GSYM(ASM_DS_EscapeThrow):

#if (defined(SEGSTACK) || defined(RESIZESTACK)) && !defined(LAZY_UNDERFLOW)

  // we will inspect the old segment to try and free it eagarly,
  // so load it into reg
  movq    STD_CONT(VPROC_PTR_REG), TMP_REG1

  #if defined(NOSEALING_CAPTURE)

    // Freeing on throw is trickier for NOSEALING_CAPTURE, i.e., when we do _not_
    // seal off on every capture, because the current segment may have a live
    // cont capture still in it!
    //
    // Consider code that captures the current continuation, stores it somewhere,
    // and then immediately throws to a continuation with a different root context.
    // Then the old segment still is a valid and live continuation in memory!
    //
    // We have to check if the root contexts are the same.
    // A throw from current to new segment, where both are in the same context,
    // means the current segment _cannot_ be live anymore even under the
    // NOSEALING_CAPTURE implementation, because of the lifetime
    // restrictions imposed by callec.
    // ~kavon

    // 1. if the segments are equal, then it's not safe to free.
    movq    16(STD_EP_REG), TMP_REG0
    cmpq    TMP_REG1, TMP_REG0
    je      notSafeToFree

    // 2. if the segments are from different contexts, it's also not safe to free.
    movq    CONTEXT(TMP_REG0), TMP_REG0
    cmpq    CONTEXT(TMP_REG1), TMP_REG0
    jne     notSafeToFree

  #endif

    // Then whichever segment we were operating in is no longer live, so
    // we can try to free it if it belongs to us.
    // NOTE: the segment to be freed should be in TMP_REG1 !
    callq     _GSYM(ASM_TryFreeSegment)
notSafeToFree:
#endif

    // set stack descriptor
    movq    16(STD_EP_REG), TMP_REG0
    movq    TMP_REG0, STD_CONT(VPROC_PTR_REG)

    // finally move to the new segment
    movq    8(STD_EP_REG), %rsp

#if defined(SEGSTACK) || defined(RESIZESTACK)
    // set the stack limit
    movq    STK_LIM(TMP_REG0), TMP_REG0
    movq    TMP_REG0, STD_EP(VPROC_PTR_REG)
#endif
    // do a pop;jmp to resume the stack
    ret
/////////////////////////////////





#if defined(SEGSTACK) || defined(RESIZESTACK)
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

.p2align 4, 0xCC
.globl _GSYM(__manti_growstack_callec)
_GSYM(__manti_growstack_callec):
    movq    $0, TMP_REG1      // never copy frames to new segment for callec.
    jmp     manti_growstack_impl

// This version is for segmented stacks a la Bruggeman et al.
.p2align 4, 0xCC
.globl	_GSYM(__manti_growstack)
_GSYM(__manti_growstack):
    // NOTE we actually start off with RSP 16-byte aligned!
#ifdef NOSEALING_CAPTURE
    movq    STD_CONT(VPROC_PTR_REG), TMP_REG0     // grab the stack descriptor
    movq    CAN_COPY(TMP_REG0), TMP_REG1          // conditionally copy
#else
    movq    $1, TMP_REG1      // 1 indicates copying.
#endif
////// FALL THROUGH //////

manti_growstack_impl:

    // switch to FFI stack
    movq    %rsp, TMP_REG0   // we use this copied value later too.
    movq    FFI_STK(VPROC_PTR_REG), %rsp

    // save manticore registers
    pushq   ALLOC_PTR_REG
    pushq   VPROC_PTR_REG
    pushq   STD_EP_REG
    pushq   STD_EXH_REG
    pushq   STD_ARG_REG
    pushq   %r10            // arg 2

    // save old stack's return address
    movq   (TMP_REG0), %r10
    pushq  %r10               // this fixes the alignment for us.

    // RSP should be 16-byte aligned at this point.
    // now save XMM registers, which are 128 bit / 16 bytes wide
    movaps %xmm2, -1*16(%rsp)
    movaps %xmm3, -2*16(%rsp)
    movaps %xmm4, -3*16(%rsp)
    movaps %xmm5, -4*16(%rsp)
    movaps %xmm6, -5*16(%rsp)
    movaps %xmm7, -6*16(%rsp)
    leaq   -6*16(%rsp), %rsp

    // the rest of the manticore registers are
    // saved by the C callee
    // setup args
    movq    VPROC_PTR_REG, %rdi            // vproc is arg 1
    leaq    8(TMP_REG0), %rsi              // current top is arg 2
    movq    TMP_REG1, %rdx                 // whether to copy is arg 3
    callq   _GSYM(StkSegmentOverflow)

    movq    %rax, TMP_REG0  // move new SP out of the way

    // restore manticore registers
    leaq   6*16(%rsp), %rsp
    movaps -1*16(%rsp), %xmm2
    movaps -2*16(%rsp), %xmm3
    movaps -3*16(%rsp), %xmm4
    movaps -4*16(%rsp), %xmm5
    movaps -5*16(%rsp), %xmm6
    movaps -6*16(%rsp), %xmm7

    // obtain the return address
    popq   TMP_REG1

    // restore other manticore regs
    popq    %r10
    popq   STD_ARG_REG
    popq   STD_EXH_REG
    popq   STD_EP_REG
    popq   VPROC_PTR_REG
    popq   ALLOC_PTR_REG

    // switch to new stack & resume execution.
    movq    TMP_REG0, %rsp                   // establish new stack ptr
    jmpq    *TMP_REG1                        // resume

#endif // segmented or resize stack

.p2align 4, 0xCC
.globl	_GSYM(ASM_TryFreeSegment)
_GSYM(ASM_TryFreeSegment):
// because we need the ability to free the current segment in-line with manticore code,
// this function will do so without clobbering manticore registers
// NOTE: convention is to use call/ret

// INPUTS:
// VPROC_PTR_REG = vproc
// TMP_REG1      = the segment to be freed.
// (%rsp)        = address to return to when done.
//
// OUTPUTS:
// VPROC_PTR_REG = vproc ptr
// TMP_REG1      = the (maybe now freed) segment input to this function.


    // Check if this segment can be reclaimed by the current
    // vproc. If we're not the one who allocated it,
    // it is part of some other VProc's allocated
    // list, and it's unsafe to free it as ours without synchronization.
    movq   OWNER(TMP_REG1), TMP_REG0
    cmpq   VPROC_PTR_REG, TMP_REG0      // is this segment in our alloc queue?
    jne    finished                     // if not, we can't free it now.

// this is okay since it's definitely not live during function entry/exit.
#define SPARE_REG    %r8

    movq    LIST_NEXT(TMP_REG1), TMP_REG0  // get allocdNext
    movq    LIST_PREV(TMP_REG1), SPARE_REG  // get allocdPrev

    // allocdNext == NULL?
    testq   TMP_REG0, TMP_REG0
    jz  nextIsNull
    // allocdNext->prev = allocdPrev
    movq    SPARE_REG, LIST_PREV(TMP_REG0)
nextIsNull:
    // allocdPrev == NULL?
    testq   SPARE_REG, SPARE_REG
    jz      prevIsNull
    // allocdPrev->next = allocdNext
    movq    TMP_REG0, LIST_NEXT(SPARE_REG)
    jmp     prevTestJoin

#undef SPARE_REG

prevIsNull:
    // vp->allocdStacks = allocdNext
    movq    TMP_REG0, ALLOCD_STKS(VPROC_PTR_REG)

prevTestJoin:

    // now, we demote the segment we removed
    movq    $0, AGE(TMP_REG1)         // info->age = AGE_Minor
    movq    $0, LIST_PREV(TMP_REG1)   // info->prev = NULL
    movq    $1, CAN_COPY(TMP_REG1)    // info->canCopy = 1
    // push it on the free list
    movq    FREE_STKS(VPROC_PTR_REG), TMP_REG0
    movq    TMP_REG0, LIST_NEXT(TMP_REG1)
    movq    TMP_REG1, FREE_STKS(VPROC_PTR_REG)

    // done with freeing the current segment
    /////////////
finished:
    retq




// This version is primarily for segmented stacks a la Bruggeman et al.
.p2align 4, 0xCC
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
    callq  _GSYM(ASM_TryFreeSegment)
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
.p2align 4, 0xCC
.globl	_GSYM(ASM_Callec1)
.globl	_GSYM(ASM_Callec2)
/* uses the direct-style manticore calling convention */

// using the 3rd through 4th arg registers to hold important values
// that must be preserved, since we know Callec only takes 1 argument.
#define OLD_SP       %r10
#define RESUME_ADDR  %r12
#define STACK_DISC   %r13

_GSYM(ASM_Callec1):
  leaq    _GSYM(ASM_DS_EscapeThrow)(%rip), RESUME_ADDR    // load the resume address
  jmp callec_body

_GSYM(ASM_Callec2):
    leaq    _GSYM(ASM_LongJmp2)(%rip), RESUME_ADDR    // load the resume address

callec_body:

    movq    STD_CONT(VPROC_PTR_REG), STACK_DISC     // grab the stack descriptor

#if defined(NOSEALING_CAPTURE)
    // no need for a new segment. we disable future copying instead.
    movq    $0, CAN_COPY(STACK_DISC)    // info->canCopy = 0   disable frame copying.

#elif defined(SEGSTACK) || defined(RESIZESTACK)
    movq    %rsp, OLD_SP                      // stash the sp
    // switch to a new segment without copying
    // This call will clobber TMP_REG1
    callq   _GSYM(__manti_growstack_callec)
    // at this point forward we're operating in a new stack segment.
#endif

    /***** alloc cont object ****/

    // ** initialize allocation **
    movq    $STACK_HDR(3), -8(ALLOC_PTR_REG)       // init header
    movq    RESUME_ADDR, (ALLOC_PTR_REG)           // init code ptr

#if !defined(NOSEALING_CAPTURE) && (defined(SEGSTACK) || defined(RESIZESTACK))
    movq    OLD_SP, 8(ALLOC_PTR_REG)          // init captured stack ptr
#else
    movq    %rsp, 8(ALLOC_PTR_REG)              // capture current stack ptr
#endif
    movq    STACK_DISC, 16(ALLOC_PTR_REG)         // init stack descriptor
    // ** done with initialization **

    // update pointers
    movq    ALLOC_PTR_REG, TMP_REG0             // save ptr to the cont object
    addq    $32, ALLOC_PTR_REG                  // bump alloc ptr, (numItems * 8) + 8

    /***** invoke the function we were given *****/
    movq    (STD_ARG_REG), STD_EP_REG       // load environment pointer
    movq    8(STD_ARG_REG), TMP_REG1        // load code pointer
    movq    TMP_REG0, STD_ARG_REG           // pass the cont object
    jmp     *TMP_REG1                       // do the call, leaving stack pointer as-is

#undef OLD_SP
#undef RESUME_ADDR
#undef STACK_DISC

    .p2align 4, 0xCC
    .globl	_GSYM(InvalidReturnAddr)
_GSYM(InvalidReturnAddr):
// if control-flow ends up here, an unexpected return occurred.
    ud2

    .p2align 4, 0xCC
    .globl	_GSYM(EndOfStack)
_GSYM(EndOfStack):
// if control-flow ends up here, unexpected stack underflow has occurred.
    ud2

    .p2align 4, 0xCC
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


    .p2align 4, 0xCC
    .globl	_GSYM(ASM_DS_UncaughtExn)
_GSYM(ASM_DS_UncaughtExn):
    movq    $REQ_UncaughtExn, TMP_REG1
    jmp     asm_get_service


    .p2align 4, 0xCC
    .globl	_GSYM(ASM_DS_VProcSleep)
_GSYM(ASM_DS_VProcSleep):
    movq    $REQ_Sleep, TMP_REG1
    jmp     asm_get_service


/* NOTE: this function only accepts vproc and alloc ptrs.
   might be worth clearing the other registers so they're
   not seen as ptrs. Do that also in the CPS version.
   those fields are not checked for roots anyways though. */

    .p2align 4, 0xCC
    .globl	_GSYM(ASM_InvokeGC_DS_LLVM)
_GSYM(ASM_InvokeGC_DS_LLVM):
    movq    $REQ_GC, TMP_REG1
    jmp     asm_get_service


asm_get_service:
// live-ins
// 1. these std-ds calling convention registers:
//      alloc, vproc, exh, stdArg
// 2. TMP_REG1 -- request code

      /* save Manticore state */
    movq    ALLOC_PTR_REG, ALLOC_PTR(VPROC_PTR_REG)
    movq    STD_EXH_REG, STD_EXH(VPROC_PTR_REG)   // NOTE I don't think we need to save this reg.
    movq    STD_ARG_REG, STD_ARG(VPROC_PTR_REG)
    movq    %rsp, STD_EP(VPROC_PTR_REG)
    /* in essence, the environment (with all roots)
       is the stack. the stack's descriptor should
       already be in STD_CONT. */

    // this service request will occur on the FFI stack
    movq    FFI_STK(VPROC_PTR_REG), %rsp

    /* move vproc and request code to the right registers */
    movq    VPROC_PTR_REG, %rdi // rdi = arg 1
    movq    TMP_REG1, %rsi      // rsi = arg 2
    jmp     _GSYM(RequestService)


#endif   // DIRECT_STYLE
