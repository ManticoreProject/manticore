	.text
	.globl stackHelper
	.extern stackWalker
stackHelper:
	mov %rsp, %rdi
	mov __LLVM_StackMaps, %rsi
	jmp stackWalker

