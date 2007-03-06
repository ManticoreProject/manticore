#include "trivial_cheney_gc.h"
		
	.global asm_init_gc
	.text
asm_init_gc:
	movq %rsi, %rdi		  /* return pointer goes in arg0 */
	movq %rax, %rdx       /* allocation pointer goes in arg2 */
	movq %rcx, %rsi	      /* root pointer goes in arg1 */
	call init_gc
	movq 8(%rax), %rcx    /* restore the allocation pointer */
	movq 16(%rax), %rsi   /* restore the return pointer */

	movq %rax, %r9
	call limit_ptr
	movq %rax, %rbp       /* restore the limit pointer */
	movq %r9, %rax
		
	movq (%rax), %rax     /* return the fresh root pointer */
	ret

	