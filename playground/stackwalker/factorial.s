	.text
	.file	"factorial.ll"
	.globl	main
	.align	16, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# BB#0:
	pushq	%rax
.Ltmp0:
	.cfi_def_cfa_offset 16
	movl	$7, %edi
	xorl	%esi, %esi
	callq	fact
	movq	%rax, %rcx
	movl	$.L.str, %edi
	xorl	%eax, %eax
	movq	%rcx, %rsi
	callq	printf
	xorl	%eax, %eax
	popq	%rcx
	retq
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
	.cfi_endproc

	.globl	fact
	.align	16, 0x90
	.type	fact,@function
fact:                                   # @fact
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp1:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp2:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp3:
	.cfi_def_cfa_offset 32
.Ltmp4:
	.cfi_offset %rbx, -32
.Ltmp5:
	.cfi_offset %r14, -24
.Ltmp6:
	.cfi_offset %rbp, -16
	movl	%esi, %ebp
	movq	%rdi, %rbx
	cmpq	$1, %rbx
	jg	.LBB1_3
# BB#1:                                 # %c1
	movq	%rbx, %rax
	jmp	.LBB1_2
.LBB1_3:                                # %c2
	leaq	-1(%rbx), %r14
	cmpl	$3, %ebp
	jle	.LBB1_4
# BB#5:                                 # %c4
	callq	stackHelper
.Ltmp7:
.LBB1_4:                                # %c3
	incl	%ebp
	movq	%r14, %rdi
	movl	%ebp, %esi
	callq	fact
	imulq	%rbx, %rax
.LBB1_2:                                # %c1
	popq	%rbx
	popq	%r14
	popq	%rbp
	retq
.Lfunc_end1:
	.size	fact, .Lfunc_end1-fact
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"%ld\n"
	.size	.L.str, 5


	.section	".note.GNU-stack","",@progbits
	.section	.llvm_stackmaps,"a",@progbits
__LLVM_StackMaps:
	.byte	1
	.byte	0
	.short	0
	.long	1
	.long	0
	.long	1
	.quad	fact
	.quad	24
	.quad	12345
	.long	.Ltmp7-fact
	.short	0
	.short	3
	.byte	4
	.byte	8
	.short	0
	.long	0
	.byte	4
	.byte	8
	.short	0
	.long	0
	.byte	4
	.byte	8
	.short	0
	.long	0
	.short	0
	.short	0
	.align	8

