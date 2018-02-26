/* asm-ffishim.S
 *
 * COPYRIGHT (c) 2018 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 */

#include "asm-defs.h"
#include "asm-offsets.h"
#include "registers.h"

/*
  This FFI shim is only used if the cshim options is enabled.
  It is used when making C calls directly from Manticore functions,
  where these C calls do _not_ trigger a GC.
  
  It is completely optional for any strategy for allocating call stacks,
  except for mutable, linked frames. I decided to make it available for
  all strategies (when using LLVM of course) to allow for a comparison
  of the overhead of using stack switching for C calls instead of
  a number of other solutions, like keeping two stacks around.
  
  ~kavon (2/7/2018)
*/

/* NOTE
	1. The FFI stack pointer, SP, starts off with SP+8 being 16-byte aligned.
	Thus, pushing the vproc gets us back to 16-byte aligned,
	and then the callq puts us back to SP+8 is 16-byte aligned.
	
	2. ASM_doCCall has a special calling convention!! Its convention is
	that, the first two arguments, vproc and function pointer, are passed
	in r11 and r10. Then, the rest of the arguments are passed in the same
	registers that the SysV ABI specifies. We chose r11 and r10 very specifically,
	because those are temporary registers in the SysV ABI convention.
	Thus, this "cshim" convention is a superset of the SysV ABI convention,
	(probably without support for varargs).
*/

	.text
	.p2align 4, 0xCC
	.globl	_GSYM(ASM_doCCall)
_GSYM(ASM_doCCall):
// vproc = r11, fun = r10.
	movq	FFI_STK(VPROC_PTR_REG), %rax
	movq	%rsp, FFI_STK(VPROC_PTR_REG)
	movq	%rax, %rsp
	pushq	VPROC_PTR_REG       # save the vproc on the c-stack
	callq	*%r10
	popq	VPROC_PTR_REG
	movq	FFI_STK(VPROC_PTR_REG), %r10
	movq	%rsp, FFI_STK(VPROC_PTR_REG)
	movq	%r10, %rsp
	retq
