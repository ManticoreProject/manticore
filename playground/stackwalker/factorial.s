	.text
	.file	"factorial.ll"
	.globl	main
	.align	16, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# BB#0:
	subq	$24, %rsp
.Ltmp0:
	.cfi_def_cfa_offset 32
	movl	$4, %eax
	movl	%eax, %edi
	movq	%rdi, 16(%rsp)          # 8-byte Spill
	callq	stackWalker
	movq	16(%rsp), %rdi          # 8-byte Reload
	callq	fact
	movabsq	$.L.str, %rdi
	movq	%rax, %rsi
	movb	$0, %al
	callq	printf
	xorl	%ecx, %ecx
	movl	%eax, 12(%rsp)          # 4-byte Spill
	movl	%ecx, %eax
	addq	$24, %rsp
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
	pushq	%rax
.Ltmp1:
	.cfi_def_cfa_offset 16
	cmpq	$1, %rdi
	movq	%rdi, (%rsp)            # 8-byte Spill
	jg	.LBB1_2
# BB#1:                                 # %c1
	movq	(%rsp), %rax            # 8-byte Reload
	popq	%rcx
	retq
.LBB1_2:                                # %c2
	movq	(%rsp), %rax            # 8-byte Reload
	subq	$1, %rax
	movq	%rax, %rdi
	callq	fact
	movq	(%rsp), %rdi            # 8-byte Reload
	imulq	%rax, %rdi
	movq	%rdi, %rax
	popq	%rcx
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
