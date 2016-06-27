	.text
	.file	"test.ll"
	.globl	test1
	.align	16, 0x90
	.type	test1,@function
test1:                                  # @test1
	.cfi_startproc
# BB#0:
	pushq	%rbx
.Ltmp0:
	.cfi_def_cfa_offset 16
.Ltmp1:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movl	$111, %edi
	callq	foo
.Ltmp2:
	movq	%rbx, %rax
	popq	%rbx
	retq
.Lfunc_end0:
	.size	test1, .Lfunc_end0-test1
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
	.section	.llvm_stackmaps,"a",@progbits
__LLVM_StackMaps:
	.byte	1
	.byte	0
	.short	0
	.long	1
	.long	0
	.long	1
	.quad	test1
	.quad	8
	.quad	2882400000
	.long	.Ltmp2-test1
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

