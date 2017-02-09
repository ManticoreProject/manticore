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

/* regs rarely used by manticore (11th-13th args) */
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
*/

/* TODO consider writing a magic value to where
   the return address would be after adding an addl 16 bytes
   to the metadata. for debugging purposes. */

asm_get_service:
// live-ins
//      rsi -- alloc ptr
//      r11 -- vproc ptr
//      rsp -- ptr to ret addr
//      TMP_REG1 -- request code

    leaq    -8(%rsp), TMP_REG2  // save the current stack cont
    
    // initialize metadata
    subq    16, %rsp
    movq    $STACK_HDR, (%rsp)
    movq    $1, 8(%rsp)     // 1 == LOCKED
    
    /* TODO mov args to right registers */
    
    jmp     _GSYM(RequestService)
