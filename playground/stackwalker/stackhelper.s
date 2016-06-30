	.text
	.globl stackHelper
	.extern stackWalker
stackHelper:
	mov %rsp, %rdi
	jmp stackWalker

