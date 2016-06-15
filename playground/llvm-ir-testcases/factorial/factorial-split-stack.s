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
	movl	$4, %edi
	callq	_fact
	movq	%rax, %rcx
	leaq	L_.str(%rip), %rdi
	xorl	%eax, %eax
	movq	%rcx, %rsi
	callq	_printf
	xorl	%eax, %eax
	popq	%rcx
	retq
	.cfi_endproc

	.globl	_fact
	.align	4, 0x90
_fact:                                  ## @fact
	.cfi_startproc
## BB#0:
	cmpq	%gs:816, %rsp
	ja	LBB1_2
## BB#1:
	movabsq	$8, %r10
	movabsq	$0, %r11
	callq	___morestack
	retq
LBB1_2:                                 ## %entry
	pushq	%rbx
Ltmp1:
	.cfi_def_cfa_offset 16
Ltmp2:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	cmpq	$1, %rbx
	jg	LBB1_4
## BB#3:                                ## %c1
	movq	%rbx, %rax
	popq	%rbx
	retq
LBB1_4:                                 ## %c2
	leaq	-1(%rbx), %rdi
	callq	_fact
	imulq	%rbx, %rax
	popq	%rbx
	retq
	.cfi_endproc

	.section	__TEXT,__cstring,cstring_literals
L_.str:                                 ## @.str
	.asciz	"%ld\n"


.subsections_via_symbols
