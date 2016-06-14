	.section	__TEXT,__text,regular,pure_instructions
	.macosx_version_min 10, 11
	.globl	_main
	.align	4, 0x90
_main:                                  ## @main
	.cfi_startproc
## BB#0:
	pushq	%rax
Ltmp0:
	.cfi_def_cfa_offset 16
	movl	$8, %edi
	callq	_fib
	movq	%rax, %rcx
	leaq	L_.str(%rip), %rdi
	xorl	%eax, %eax
	movq	%rcx, %rsi
	callq	_printf
	xorl	%eax, %eax
	popq	%rcx
	retq
	.cfi_endproc

	.globl	_fib
	.align	4, 0x90
_fib:                                   ## @fib
	.cfi_startproc
## BB#0:
	cmpq	%gs:816, %rsp
	ja	LBB1_2
## BB#1:
	movabsq	$24, %r10
	movabsq	$0, %r11
	callq	___morestack
	retq
LBB1_2:                                 ## %entry
	pushq	%r14
Ltmp1:
	.cfi_def_cfa_offset 16
	pushq	%rbx
Ltmp2:
	.cfi_def_cfa_offset 24
	pushq	%rax
Ltmp3:
	.cfi_def_cfa_offset 32
Ltmp4:
	.cfi_offset %rbx, -24
Ltmp5:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	cmpq	$2, %rbx
	jg	LBB1_5
## BB#3:                                ## %c1
	movl	$1, %eax
	jmp	LBB1_4
LBB1_5:                                 ## %c2
	leaq	-1(%rbx), %rdi
	addq	$-2, %rbx
	callq	_fib
	movq	%rax, %r14
	movq	%rbx, %rdi
	callq	_fib
	addq	%r14, %rax
LBB1_4:                                 ## %c1
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
	.cfi_endproc

	.section	__TEXT,__cstring,cstring_literals
L_.str:                                 ## @.str
	.asciz	"%ld\n"


.subsections_via_symbols
