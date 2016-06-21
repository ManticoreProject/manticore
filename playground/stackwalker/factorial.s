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
	xorl	%esi, %esi
	movl	$10, %eax
	movl	%eax, %edi
	callq	fact
	movabsq	$.L.str, %rdi
	movq	%rax, %rsi
	movb	$0, %al
	callq	printf
	xorl	%ecx, %ecx
	movl	%eax, 4(%rsp)           # 4-byte Spill
	movl	%ecx, %eax
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
	subq	$24, %rsp
.Ltmp1:
	.cfi_def_cfa_offset 32
	cmpq	$1, %rdi
	movq	%rdi, 16(%rsp)          # 8-byte Spill
	movl	%esi, 12(%rsp)          # 4-byte Spill
	jg	.LBB1_2
# BB#1:                                 # %c1
	movq	16(%rsp), %rax          # 8-byte Reload
	addq	$24, %rsp
	retq
.LBB1_2:                                # %c2
	movq	16(%rsp), %rax          # 8-byte Reload
	subq	$1, %rax
	movl	12(%rsp), %ecx          # 4-byte Reload
	cmpl	$5, %ecx
	movq	%rax, (%rsp)            # 8-byte Spill
	jg	.LBB1_4
.LBB1_3:                                # %c3
	movl	12(%rsp), %eax          # 4-byte Reload
	addl	$1, %eax
	movq	(%rsp), %rdi            # 8-byte Reload
	movl	%eax, %esi
	callq	fact
	movq	16(%rsp), %rdi          # 8-byte Reload
	imulq	%rax, %rdi
	movq	%rdi, %rax
	addq	$24, %rsp
	retq
.LBB1_4:                                # %c4
	movl	12(%rsp), %edi          # 4-byte Reload
	callq	stackWalker
	jmp	.LBB1_3
.Lfunc_end1:
	.size	fact, .Lfunc_end1-fact
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"%ld\n"
	.size	.L.str, 5


	.section	".note.GNU-stack","",@progbits
