
	.global asm_init_gc

	.text
asm_init_gc:
	movq %rax, %rdi  /* root pointer goes in arg0 */
	movq %rsi, %rdx  /* return pointer goes in arg2 */
	movq %rcx, %rsi  /* allocation pointer goes in arg0 */
	call init_gc
	ret