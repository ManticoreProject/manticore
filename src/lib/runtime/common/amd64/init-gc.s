
	.global asm_init_gc

	.text
asm_init_gc:
			  /* return pointer goes in arg1 */
	movq %rcx, %rdi		 /* allocation pointer goes in arg0 */
	movq %rax, %rdx  /* root pointer goes in arg2 */
	call init_gc
	movq 8(%rax), %rcx    /* restore the allocation pointer */
	movq 16(%rax), %rsi   /* restore the return pointer */
	movq (%rax), %rax      /* return the fresh root pointer */
	ret

	