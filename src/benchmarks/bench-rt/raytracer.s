	.text
letJoinK.6:
	/* Liveout:  GP={%r8 %rdi}  */
	movq	%r9, %rbx
	movq	%r8, %rdx
	movq	%rdi, %rcx
	movq	8(%rcx), %r10
	movq	%rbx, 72(%r10)
	movq	$12, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rsi, %r12
	addq	$16, %rsi
	movq	16(%rcx), %rdi
	movq	%r12, %r8
	jmp	letJoinK.7
	.text
letJoinK.D:
	movq	%rdi, %r12
	movq	8(%r12), %r10
	movq	8(%r10), %r13
	cmpq	$1, %r13
	je	L_trueE
	movq	8(%r12), %r14
	movq	16(%r14), %r15
else.B:
	/* block else<B5CF> (ep<B5CC>,qTl<B5CE>,qHd<B5CD>) */
	movq	8(%r13), %r14
	cmpq	%r15, %r13
	je	L_trueC
letJoinK.9:
	/* flushLoads */
	/* block letJoinK<B5D5> (ep<B5D2>,qHd<B5D4>,qNext<B5D3>) */
	movq	8(%r12), %rdx
	movq	%r14, 8(%rdx)
	movq	$12, -8(%rsi)
	movq	(%r13), %rbx
	movq	%rbx, (%rsi)
	movq	%rsi, %r10
	addq	$16, %rsi
	jmp	letJoinK.A
L_trueE:
then.F:
	/* block then<B5CA> (ep<B5C9>) */
	movq	$1, %r10
letJoinK.A:
	/* flushLoads */
	/* block letJoinK<B5A8> (ep<B5A6>,elt<B5A7>) */
	movq	8(%r12), %r15
	movl	$0, (%r15)
	cmpq	$1, %r10
	jne	L_true10
else.11:
	/* Liveout:  GP={%rdi}  */
	/* block else<B5C6> (ep<B5C5>) */
	movq	32(%r12), %rdi
	jmp	dispatch.13
L_true10:
then.12:
	/* Liveout:  GP={%rax %rdi}  */
	/* block then<B5AE> (ep<B5AC>,elt<B5AD>) */
	movq	(%r10), %rbx
	movq	$-1048576, %rcx
	andq	%rsi, %rcx
	movq	32(%rcx), %rcx
	movq	$12, -8(%rsi)
	movq	8(%rbx), %r10
	movq	%r10, (%rsi)
	movq	%rsi, %rdx
	addq	$16, %rsi
	movq	$1289, -8(%rsi)
	movl	(%rcx), %r14d
	movl	%r14d, (%rsi)
	movq	%rdx, 8(%rsi)
	movl	16(%rcx), %r15d
	movl	%r15d, 16(%rsi)
	movq	24(%rcx), %rcx
	movq	%rcx, 24(%rsi)
	movq	%rsi, %r13
	addq	$40, %rsi
	movq	$-1048576, %rdx
	andq	%rsi, %rdx
	movq	%r13, 32(%rdx)
	movq	(%rbx), %rbx
	movq	$20, -8(%rsi)
	movq	16(%r12), %r10
	movq	%r10, (%rsi)
	movq	24(%r12), %r13
	movq	40(%r13), %r14
	movq	%r14, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	movq	24(%r12), %r15
	movq	%rdx, 40(%r15)
	movq	24(%r12), %rcx
	movq	$1, 8(%rcx)
	movq	(%rbx), %rdx
	movq	$1, %r10
	movq	%r10, %rax
	movq	%rbx, %rdi
	jmp	*%rdx
L_trueC:
then.8:
	/* flushLoads */
	/* block then<B5DC> (ep<B5D9>,qHd<B5DB>,qNext<B5DA>) */
	movq	8(%r12), %rcx
	movq	$1, 16(%rcx)
	jmp	letJoinK.9
	.text
letJoinK.19:
	movq	%rdi, %rdx
	movq	8(%rdx), %r10
	movq	8(%r10), %rcx
	movq	8(%rdx), %r12
	movq	16(%r12), %rbx
	cmpq	$1, %rbx
	je	L_true1A
else.14:
	/* flushLoads */
	/* block else<B64D> (ep<B64A>,qHd<B64C>,qTl<B64B>) */
	movq	24(%rdx), %r10
	movq	%r10, 8(%rbx)
	jmp	letJoinK.15
L_true1A:
letJoinK.15:
	/* flushLoads */
	/* block letJoinK<B637> (ep<B635>,qHd<B636>) */
	movq	8(%rdx), %r13
	movq	24(%rdx), %r14
	movq	%r14, 16(%r13)
	cmpq	$1, %rcx
	jne	letJoinK.17
L_true16:
then.18:
	/* flushLoads */
	/* block then<B642> (ep<B641>) */
	movq	8(%rdx), %r15
	movq	24(%rdx), %rcx
	movq	%rcx, 8(%r15)
letJoinK.17:
	/* Liveout:  GP={%rdi}  */
	/* flushLoads */
	/* block letJoinK<B63C> (ep<B63B>) */
	movq	8(%rdx), %rbx
	movl	$0, (%rbx)
	movq	16(%rdx), %rdi
	jmp	dispatch.13
	.text
letJoinK.27:
	movq	%rdi, %rdx
	movq	8(%rdx), %r15
	movq	8(%r15), %rcx
	movq	8(%rdx), %rbx
	movq	16(%rbx), %rbx
	cmpq	$1, %rbx
	je	L_true28
else.1C:
	/* flushLoads */
	/* block else<B7BB> (ep<B7B8>,qHd<B7BA>,qTl<B7B9>) */
	movq	32(%rdx), %r14
	movq	%r14, 8(%rbx)
	jmp	letJoinK.1D
L_true28:
letJoinK.1D:
	/* flushLoads */
	/* block letJoinK<B785> (ep<B783>,qHd<B784>) */
	movq	8(%rdx), %rbx
	movq	32(%rdx), %r10
	movq	%r10, 16(%rbx)
	cmpq	$1, %rcx
	je	L_true1E
	jmp	letJoinK.1F
L_true1E:
then.20:
	/* flushLoads */
	/* block then<B7B0> (ep<B7AF>) */
	movq	8(%rdx), %r12
	movq	32(%rdx), %r13
	movq	%r13, 8(%r12)
letJoinK.1F:
	/* flushLoads */
	/* block letJoinK<B78A> (ep<B789>) */
	movq	8(%rdx), %r14
	movl	$0, (%r14)
	movq	$261, -8(%rsi)
	movabsq	$letJoinK.21, %r15
	movq	%r15, (%rsi)
	movq	16(%rdx), %rcx
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	24(%rdx), %r10
	movq	16(%r10), %rbx
	cmpq	$1, %rbx
	jne	L29
S_case22:
case.23:
	/* Liveout:  GP={%rdi}  */
	/* block case<B798> (ep<B796>,letJoinK<B797>) */
	movq	24(%rdx), %r14
	movq	$1, 8(%r14)
	movq	%r13, %rdi
	jmp	letJoinK.21
L29:
	cmpq	$3, %rbx
	jne	S_case22
S_case24:
case.25:
	/* Liveout:  GP={%rax %rdi}  */
	/* block case<B79E> (ep<B79C>,letJoinK<B79D>) */
	movq	24(%rdx), %r12
	movq	$1, 16(%r12)
	movq	$20, -8(%rsi)
	movabsq	$k.26, %r14
	movq	%r14, (%rsi)
	movq	%r13, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movq	$1, (%rsi)
	movq	%r13, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	24(%rdx), %rcx
	movq	40(%rcx), %r15
	movq	8(%r15), %rbx
	movq	24(%rdx), %r10
	movq	%rbx, 40(%r10)
	movq	(%r15), %r10
	movq	(%r10), %r13
	movq	%r12, %rax
	movq	%r10, %rdi
	jmp	*%r13
	.text
letJoinK.2C:
	movq	%rdi, %rdx
	movq	$261, -8(%rsi)
	movabsq	$letJoinK.2D, %r14
	movq	%r14, (%rsi)
	movq	8(%rdx), %r15
	movq	%r15, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	16(%rdx), %r10
	movq	16(%r10), %rbx
	cmpq	$1, %rbx
	je	S_case2E
	cmpq	$3, %rbx
	je	S_case30
S_case2E:
case.2F:
	/* Liveout:  GP={%rdi}  */
	/* block case<B818> (ep<B816>,letJoinK<B817>) */
	movq	16(%rdx), %r12
	movq	$1, 8(%r12)
	movq	%rcx, %rdi
	jmp	letJoinK.2D
S_case30:
case.2A:
	/* Liveout:  GP={%rax %rdi}  */
	/* block case<B81E> (ep<B81C>,letJoinK<B81D>) */
	movq	16(%rdx), %r12
	movq	$1, 16(%r12)
	movq	$20, -8(%rsi)
	movabsq	$k.2B, %r14
	movq	%r14, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movq	$1, (%rsi)
	movq	%r13, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	16(%rdx), %rcx
	movq	40(%rcx), %r15
	movq	8(%r15), %rbx
	movq	16(%rdx), %r12
	movq	%rbx, 40(%r12)
	movq	(%r15), %rbx
	movq	(%rbx), %r13
	movq	%r10, %rax
	movq	%rbx, %rdi
	jmp	*%r13
	.text
get_D_ite.37:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest39
	/* live= GP={%rcx %rdx %rbx} spilled=  */
retGC38:
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest39:
	movq	%r11, %r10
	subq	%rsi, %r10
	jle	doGC3A
check.31:
	/* block check<B964> (ep<B30E>,retK<B30F>,exh<B310>) */
	movq	$-1048576, %r13
	andq	%rsi, %r13
	movq	32(%r13), %r14
	movq	8(%r14), %r12
	cmpq	$1, %r12
	je	L3B
L_true32:
then.34:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block then<B31B> (retK<B31A>,_t<B319>) */
	movq	(%rdx), %r13
	movq	%rdx, %rdi
	movq	(%r12), %r14
	movq	(%r14), %r8
	jmp	*%r13
doGC3A:
	movq	$28, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%rsi, %r15
	addq	$32, %rsi
	movq	%r15, %rdi
	movabsq	$retGC38, %r8
	jmp	ASM_InvokeGC
L3B:
else.33:
	/* Liveout:  GP={%rax %rdi}  */
	/* block else<B322> (exh<B321>) */
	movq	$133, -8(%rsi)
	movabsq	$str35, %rdx
	movq	%rdx, (%rsi)
	movl	$51, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movabsq	$tag36, %rbx
	movq	%rbx, (%rsi)
	movq	%r15, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	(%rcx), %r12
	movq	%r10, %rax
	movq	%rcx, %rdi
	jmp	*%r12
	.text
set_D_ite.3D:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest3F
	/* live= GP={%rcx %rdx %rbx} spilled=  */
retGC3E:
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest3F:
	movq	%r11, %r14
	subq	%rsi, %r14
	jle	doGC40
check.3C:
	/* Liveout:  GP={%rdi}  */
	/* block check<B968> (ep<B32B>,ite<B32C>,retK<B32D>) */
	movq	$-1048576, %r10
	andq	%rsi, %r10
	movq	32(%r10), %r13
	movq	$12, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rsi, %r14
	addq	$16, %rsi
	movq	$1289, -8(%rsi)
	movl	(%r13), %edx
	movl	%edx, (%rsi)
	movq	%r14, 8(%rsi)
	movl	16(%r13), %ebx
	movl	%ebx, 16(%rsi)
	movq	24(%r13), %r10
	movq	%r10, 24(%rsi)
	movq	%rsi, %r15
	addq	$40, %rsi
	movq	$-1048576, %r12
	andq	%rsi, %r12
	movq	%r15, 32(%r12)
	movq	(%rcx), %r12
	movq	%rcx, %rdi
	jmp	*%r12
doGC40:
	movq	$28, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%rsi, %r13
	addq	$32, %rsi
	movq	%r13, %rdi
	movabsq	$retGC3E, %r8
	jmp	ASM_InvokeGC
	.text
k.42:
	movq	%rax, %rcx
	movq	%rdi, %rdx
	jmp	gcTest44
	/* live= GP={%rcx %rdx} spilled=  */
retGC43:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest44:
	movq	%r11, %r13
	subq	%rsi, %r13
	jle	doGC45
check.41:
	/* Liveout:  GP={%rdi}  */
	/* block check<B96B> (ep<B361>,x<B360>) */
	movq	8(%rdx), %rbx
	movq	(%rbx), %r10
	movq	%rbx, %rdi
	jmp	*%r10
doGC45:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	%r12, %rdi
	movabsq	$retGC43, %r8
	jmp	ASM_InvokeGC
	.text
k.48:
	movq	%rax, %rcx
	movq	%rdi, %rdx
	jmp	gcTest4A
	/* live= GP={%rcx %rdx} spilled=  */
retGC49:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest4A:
	movq	%r11, %r13
	subq	%rsi, %r13
	jle	doGC4B
check.46:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<B96E> (ep<B374>,x<B371>) */
	movq	$-1048576, %rbx
	andq	%rsi, %rbx
	movq	$3, 8(%rbx)
	pause
	movq	8(%rdx), %r10
	movq	(%r10), %rdi
	movq	16(%rdx), %r8
	jmp	barrierSpin.47
doGC4B:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	%r12, %rdi
	movabsq	$retGC49, %r8
	jmp	ASM_InvokeGC
	.text
barrierSpin.47:
	movq	%r8, %rcx
	movq	%rdi, %rdx
gcTest55:
	movq	%r11, %rbx
	subq	%rsi, %rbx
	jle	doGC56
check.4C:
	/* block check<B971> (ep<B347>,retK<B348>) */
	movq	(%rdx), %r14
	movq	(%rdx), %r15
	movl	(%r14), %ebx
	cmpl	8(%r15), %ebx
	je	L_true4D
else.4E:
	/* Liveout:  GP={%rax %rdi}  */
	/* block else<B370> (ep<B36E>,retK<B36F>) */
	movq	$133, -8(%rsi)
	movq	%rdx, (%rsi)
	movabsq	$barrierSpin.47, %r12
	movq	%r12, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	$28, -8(%rsi)
	movabsq	$k.48, %r14
	movq	%r14, (%rsi)
	movq	%r10, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%rsi, %r13
	addq	$32, %rsi
	movq	$20, -8(%rsi)
	movq	$1, (%rsi)
	movq	%r13, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	8(%rdx), %rcx
	movq	40(%rcx), %r15
	movq	8(%r15), %rbx
	movq	8(%rdx), %r10
	movq	%rbx, 40(%r10)
	movq	(%r15), %r12
	movq	(%r12), %r14
	movq	%r13, %rax
	movq	%r12, %rdi
	jmp	*%r14
doGC56:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	%r15, %rdi
	movabsq	$retGC54, %r8
	jmp	ASM_InvokeGC
	/* live= GP={%rcx %rdx} spilled=  */
retGC54:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
	jmp	gcTest55
L_true4D:
then.4F:
	/* block then<B352> (ep<B350>,retK<B351>) */
	movq	8(%rdx), %rbx
	movq	16(%rbx), %r15
	cmpq	$1, %r15
	jne	L57
S_case50:
case.51:
	/* Liveout:  GP={%rdi}  */
	/* block case<B357> (ep<B355>,retK<B356>) */
	movq	8(%rdx), %r13
	movq	$1, 8(%r13)
	movq	(%rcx), %r14
	movq	%rcx, %rdi
	jmp	*%r14
L57:
	cmpq	$3, %r15
	jne	S_case50
S_case52:
case.53:
	/* Liveout:  GP={%rax %rdi}  */
	/* block case<B35D> (ep<B35B>,retK<B35C>) */
	movq	8(%rdx), %r10
	movq	$1, 16(%r10)
	movq	$20, -8(%rsi)
	movabsq	$k.42, %r13
	movq	%r13, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movq	$1, (%rsi)
	movq	%r12, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	8(%rdx), %r15
	movq	40(%r15), %r14
	movq	8(%r14), %rcx
	movq	8(%rdx), %rdx
	movq	%rcx, 40(%rdx)
	movq	(%r14), %rbx
	movq	(%rbx), %r12
	movq	%r10, %rax
	movq	%rbx, %rdi
	jmp	*%r12
	.text
wait.59:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest5B
	/* live= GP={%r10 %rcx %rdx %rbx} spilled=  */
retGC5A:
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest5B:
	movq	%r11, %r12
	subq	%rsi, %r12
	jle	doGC5C
check.58:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<B976> (ep<B33C>,b<B33D>,retK<B33E>,exh<B33F>) */
	movq	$-1048576, %r10
	andq	%rsi, %r10
	movq	$3, 8(%r10)
	movq	$133, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%r10, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	$133, -8(%rsi)
	movq	%r12, (%rsi)
	movabsq	$barrierSpin.47, %r14
	movq	%r14, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	(%r13), %rdi
	movq	%rcx, %r8
	jmp	barrierSpin.47
doGC5C:
	movq	$36, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %r15
	addq	$40, %rsi
	movq	%r15, %rdi
	movabsq	$retGC5A, %r8
	jmp	ASM_InvokeGC
	.text
wakeupK.5F:
	movq	%rax, %rcx
	movq	%rdi, %rdx
	jmp	gcTest61
	/* live= GP={%rcx %rdx} spilled=  */
retGC60:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest61:
	movq	%r11, %r10
	subq	%rsi, %r10
	jle	doGC62
check.5D:
	/* Liveout:  GP={%rdi}  */
	/* block check<B979> (ep<B3B7>,x<B3B5>) */
	movq	8(%rdx), %rdi
	jmp	dispatch.5E
doGC62:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	%rbx, %rdi
	movabsq	$retGC60, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.64:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest66
	/* live= GP={%rcx %rdx} spilled=  */
retGC65:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest66:
	movq	%r11, %rbx
	subq	%rsi, %rbx
	jle	doGC67
check.63:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<B97F> (ep<B3DE>,rest<B3DD>) */
	movq	$28, -8(%rsi)
	movq	8(%rdx), %r10
	movq	(%r10), %r12
	movq	%r12, (%rsi)
	movq	8(%rdx), %r13
	movq	8(%r13), %r14
	movq	%r14, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%rsi, %rbx
	addq	$32, %rsi
	movq	16(%rdx), %r15
	movq	(%r15), %rcx
	movq	%r15, %rdi
	movq	%rbx, %r8
	jmp	*%rcx
doGC67:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	movq	%rdx, %rdi
	movabsq	$retGC65, %r8
	jmp	ASM_InvokeGC
	.text
append.6C:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest6E
	/* live= GP={%rcx %rdx %rbx} spilled=  */
retGC6D:
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest6E:
	movq	%r11, %r15
	subq	%rsi, %r15
	jle	doGC6F
check.68:
	/* block check<B983> (ep<B3CB>,queue1<B3CC>,retK<B3CD>) */
	cmpq	$1, %rdx
	jne	L70
L_true69:
then.6B:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block then<B3D5> (ep<B3D3>,retK<B3D4>) */
	movq	(%rcx), %r13
	movq	%rcx, %rdi
	movq	(%rbx), %r8
	jmp	*%r13
doGC6F:
	movq	$28, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%rsi, %r14
	addq	$32, %rsi
	movq	%r14, %rdi
	movabsq	$retGC6D, %r8
	jmp	ASM_InvokeGC
L70:
else.6A:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block else<B3DB> (ep<B3D8>,queue1<B3DA>,retK<B3D9>) */
	movq	$775, -8(%rsi)
	movabsq	$letJoinK.64, %r12
	movq	%r12, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%rsi, %r10
	addq	$32, %rsi
	movq	%rbx, %rdi
	movq	16(%rdx), %r8
	movq	%r10, %r9
	jmp	append.6C
	.text
letJoinK.72:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest74
	/* live= GP={%rcx %rdx} spilled=  */
retGC73:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest74:
	movq	%r11, %r14
	subq	%rsi, %r14
	jle	doGC75
check.71:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<B986> (ep<B3EC>,newHd<B3EA>) */
	movq	8(%rdx), %rbx
	movq	%rcx, 72(%rbx)
	movq	$12, -8(%rsi)
	movq	16(%rdx), %r12
	movq	%r12, (%rsi)
	movq	%rsi, %r10
	addq	$16, %rsi
	movq	24(%rdx), %rdi
	movq	%r10, %r8
	jmp	letJoinK.7
doGC75:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	%r13, %rdi
	movabsq	$retGC73, %r8
	jmp	ASM_InvokeGC
	.text
revQueue.7A:
	movq	%r12, %rbx
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %r12
	jmp	gcTest7C
	/* live= GP={%r13 %rbx %r10 %r15 %r14 %rcx} spilled=  */
retGC7B:
	movq	40(%rdi), %r13
	movq	32(%rdi), %rbx
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %r12
gcTest7C:
	movq	%r11, %r14
	subq	%rsi, %r14
	jle	doGC7D
	movq	%rcx, %r15
	movq	%rdx, %r14
	movq	%r12, %rcx
check.76:
	/* block check<B98D> (ep<B424>,fls<B425>,k<B426>,rest<B427>,acc<B428>,retK<B429>) */
	movq	$28, -8(%rsi)
	movq	%r14, (%rsi)
	movq	%r15, 8(%rsi)
	movq	%rbx, 16(%rsi)
	movq	%rsi, %r12
	addq	$32, %rsi
	cmpq	$1, %r10
	jne	L7E
L_true77:
then.79:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block then<B434> (acc<B433>,retK<B432>,acc<B431>) */
	movq	%r13, %rdi
	movq	%r12, %r8
	movq	%rbx, %r9
	jmp	letJoinK.6
doGC7D:
	movq	$52, -8(%rsi)
	movq	%r12, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rbx, 32(%rsi)
	movq	%r13, 40(%rsi)
	movq	%rsi, %rdx
	addq	$56, %rsi
	movq	%rdx, %rdi
	movabsq	$retGC7B, %r8
	jmp	ASM_InvokeGC
L7E:
else.78:
	/* Liveout:  GP={%r13 %r12 %r10 %r9 %r8 %rdi}  */
	/* block else<B43A> (ep<B436>,rest<B439>,retK<B438>,acc<B437>) */
	movq	%rcx, %rdi
	movq	(%r10), %r8
	movq	8(%r10), %r9
	movq	16(%r10), %r10
	jmp	revQueue.7A
	.text
letJoinK.80:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest82
	/* live= GP={%rcx %rdx} spilled=  */
retGC81:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest82:
	movq	%r11, %rbx
	subq	%rsi, %rbx
	jle	doGC83
check.7F:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<B990> (ep<B483>,rest<B482>) */
	movq	$28, -8(%rsi)
	movq	8(%rdx), %r10
	movq	(%r10), %r12
	movq	%r12, (%rsi)
	movq	8(%rdx), %r13
	movq	8(%r13), %r14
	movq	%r14, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%rsi, %rbx
	addq	$32, %rsi
	movq	16(%rdx), %r15
	movq	(%r15), %rcx
	movq	%r15, %rdi
	movq	%rbx, %r8
	jmp	*%rcx
doGC83:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	movq	%rdx, %rdi
	movabsq	$retGC81, %r8
	jmp	ASM_InvokeGC
	.text
append.88:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest8A
	/* live= GP={%rcx %rdx %rbx} spilled=  */
retGC89:
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest8A:
	movq	%r11, %r15
	subq	%rsi, %r15
	jle	doGC8B
check.84:
	/* block check<B994> (ep<B470>,queue1<B471>,retK<B472>) */
	cmpq	$1, %rdx
	jne	L8C
L_true85:
then.87:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block then<B47A> (ep<B478>,retK<B479>) */
	movq	(%rcx), %r13
	movq	%rcx, %rdi
	movq	(%rbx), %r8
	jmp	*%r13
doGC8B:
	movq	$28, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%rsi, %r14
	addq	$32, %rsi
	movq	%r14, %rdi
	movabsq	$retGC89, %r8
	jmp	ASM_InvokeGC
L8C:
else.86:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block else<B480> (ep<B47D>,queue1<B47F>,retK<B47E>) */
	movq	$775, -8(%rsi)
	movabsq	$letJoinK.80, %r12
	movq	%r12, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%rsi, %r10
	addq	$32, %rsi
	movq	%rbx, %rdi
	movq	16(%rdx), %r8
	movq	%r10, %r9
	jmp	append.88
	.text
letJoinK.8E:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest90
	/* live= GP={%rcx %rdx} spilled=  */
retGC8F:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest90:
	movq	%r11, %r14
	subq	%rsi, %r14
	jle	doGC91
check.8D:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<B997> (ep<B491>,newHd<B48F>) */
	movq	8(%rdx), %rbx
	movq	%rcx, 72(%rbx)
	movq	$12, -8(%rsi)
	movq	24(%rdx), %r12
	movq	%r12, (%rsi)
	movq	%rsi, %r10
	addq	$16, %rsi
	movq	16(%rdx), %rdi
	movq	%r10, %r8
	jmp	letJoinK.7
doGC91:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	%r13, %rdi
	movabsq	$retGC8F, %r8
	jmp	ASM_InvokeGC
	.text
dispatch.5E:
	movq	%rdi, %rcx
gcTestAF:
	movq	%r11, %rdx
	subq	%rsi, %rdx
	jle	doGCB0
check.92:
	/* block check<B999> (ep<B38C>) */
	movq	16(%rcx), %rbx
	movq	72(%rbx), %r13
	movq	16(%rcx), %r10
	movq	80(%r10), %r14
	movq	16(%rcx), %r12
	movq	312(%r12), %rdx
	cmpq	$1, %rdx
	jne	LB1
L_true93:
then.95:
	/* block then<B4C2> (ep<B4BF>,hd<B4C1>,tl<B4C0>) */
	movq	$1, %r15
	jmp	letJoinK.98
doGCB0:
	movq	$12, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%rsi, %r15
	addq	$16, %rsi
	movq	%r15, %rdi
	movabsq	$retGCAE, %r8
	jmp	ASM_InvokeGC
	/* live= GP={%rcx} spilled=  */
retGCAE:
	movq	(%rdi), %rcx
	jmp	gcTestAF
LB1:
else.94:
	/* block else<B4C9> (ep<B4C5>,hd<B4C8>,tl<B4C7>,ldgPadOrig<B4C6>) */
	movq	16(%rcx), %rbx
	leaq	312(%rbx), %r15
	movq	%rdx, %rax
	movq	$1, %r10
	movq	16(%rcx), %r12
	lock
	cmpxchgq	%r10, 312(%r12)
	movq	%rax, %r15
	cmpq	%rdx, %r15
	jne	LB2
L_true96:
letJoinK.98:
	/* block letJoinK<B397> (ep<B393>,hd<B396>,tl<B395>,landingPadItems<B394>) */
	movq	$1289, -8(%rsi)
	movabsq	$letJoinK.7, %rdx
	movq	%rdx, (%rsi)
	movq	8(%rcx), %rbx
	movq	%rbx, 8(%rsi)
	movq	16(%rcx), %r10
	movq	%r10, 16(%rsi)
	movq	%rcx, 24(%rsi)
	movq	%rsi, %r12
	addq	$40, %rsi
	cmpq	$1, %r15
	jne	L_true99
else.9A:
	/* block else<B3FA> (ep<B3F6>,hd<B3F9>,tl<B3F8>,letJoinK<B3F7>) */
	cmpq	$1, %r13
	jne	L_true9C
else.9D:
	/* block else<B409> (ep<B405>,hd<B408>,tl<B407>,letJoinK<B406>) */
	cmpq	$1, %r14
	jne	L_true9F
else.A0:
	/* block else<B45A> (ep<B457>,hd<B459>,letJoinK<B458>) */
	movq	16(%rcx), %r14
	movq	312(%r14), %r15
	cmpq	$1, %r15
	jne	LB3
L_trueA2:
then.A4:
	/* block then<B4A2> (ep<B49F>,hd<B4A1>,letJoinK<B4A0>) */
	movq	$1, %r14
letJoinK.A7:
	/* block letJoinK<B45F> (ep<B45B>,hd<B45E>,letJoinK<B45D>,landingPadItems<B45C>) */
	cmpq	$1, %r14
	je	L_trueA8
else.A9:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block else<B46B> (ep<B467>,hd<B46A>,letJoinK<B469>,landingPadItems<B468>) */
	movq	$12, -8(%rsi)
	movq	%r13, (%rsi)
	movq	%rsi, %r15
	addq	$16, %rsi
	movq	$133, -8(%rsi)
	movq	%r15, (%rsi)
	movabsq	$append.88, %rbx
	movq	%rbx, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	movq	$1545, -8(%rsi)
	movabsq	$letJoinK.8E, %r13
	movq	%r13, (%rsi)
	movq	16(%rcx), %r15
	movq	%r15, 8(%rsi)
	movq	%r12, 16(%rsi)
	movq	%r14, 24(%rsi)
	movq	%rsi, %r10
	addq	$40, %rsi
	movq	(%rdx), %rdi
	movq	16(%r14), %r8
	movq	%r10, %r9
	jmp	append.88
L_trueA8:
then.AA:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block then<B464> (letJoinK<B463>) */
	movq	%r12, %rdi
	movq	$1, %r8
	jmp	letJoinK.7
L_true99:
then.9B:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block then<B3C6> (ep<B3C2>,hd<B3C5>,landingPadItems<B3C4>,letJoinK<B3C3>) */
	movq	$12, -8(%rsi)
	movq	%r13, (%rsi)
	movq	%rsi, %rdx
	addq	$16, %rsi
	movq	$133, -8(%rsi)
	movq	%rdx, (%rsi)
	movabsq	$append.6C, %r10
	movq	%r10, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	$1545, -8(%rsi)
	movabsq	$letJoinK.72, %r13
	movq	%r13, (%rsi)
	movq	16(%rcx), %r14
	movq	%r14, 8(%rsi)
	movq	%r15, 16(%rsi)
	movq	%r12, 24(%rsi)
	movq	%rsi, %r12
	addq	$40, %rsi
	movq	(%rbx), %rdi
	movq	16(%r15), %r8
	movq	%r12, %r9
	jmp	append.6C
L_true9C:
then.9E:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block then<B400> (ep<B3FD>,hd<B3FF>,letJoinK<B3FE>) */
	movq	16(%r13), %r14
	movq	16(%rcx), %r15
	movq	%r14, 72(%r15)
	movq	$12, -8(%rsi)
	movq	%r13, (%rsi)
	movq	%rsi, %rcx
	addq	$16, %rsi
	movq	%r12, %rdi
	movq	%rcx, %r8
	jmp	letJoinK.7
LB3:
else.A3:
	/* block else<B4A9> (ep<B4A5>,hd<B4A8>,letJoinK<B4A7>,ldgPadOrig<B4A6>) */
	movq	16(%rcx), %rbx
	leaq	312(%rbx), %rdx
	movq	%r15, %rax
	movq	$1, %r10
	movq	16(%rcx), %r14
	lock
	cmpxchgq	%r10, 312(%r14)
	movq	%rax, %r14
	cmpq	%r15, %r14
	jne	LB4
L_trueA5:
	jmp	letJoinK.A7
LB4:
else.A6:
	/* block else<B4B8> (ep<B4B5>,hd<B4B7>,letJoinK<B4B6>) */
	movq	$1, %r14
	jmp	letJoinK.A7
L_true9F:
then.A1:
	/* block then<B40F> (ep<B40C>,tl<B40E>,letJoinK<B40D>) */
	movq	16(%rcx), %rdx
	movq	$1, 80(%rdx)
	movq	16(%r14), %rbx
	movq	$519, -8(%rsi)
	movabsq	$letJoinK.6, %r10
	movq	%r10, (%rsi)
	movq	16(%rcx), %r13
	movq	%r13, 8(%rsi)
	movq	%r12, 16(%rsi)
	movq	%rsi, %r13
	addq	$32, %rsi
	movq	$18, -8(%rsi)
	movq	$1, (%rsi)
	movabsq	$revQueue.7A, %r15
	movq	%r15, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	$1, %rdx
	movq	$28, -8(%rsi)
	movq	(%r14), %rcx
	movq	%rcx, (%rsi)
	movq	8(%r14), %r12
	movq	%r12, 8(%rsi)
	movq	%rdx, 16(%rsi)
	movq	%rsi, %r12
	addq	$32, %rsi
	cmpq	$1, %rbx
	jne	LB5
L_trueAB:
then.AD:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block then<B449> (_t<B448>,acc<B447>,letJoinK<B446>) */
	movq	%r13, %rdi
	movq	%r12, %r8
	movq	%rdx, %r9
	jmp	letJoinK.6
LB5:
else.AC:
	/* Liveout:  GP={%r13 %r12 %r10 %r9 %r8 %rdi}  */
	/* block else<B450> (_t<B44F>,revQueue<B44E>,acc<B44D>,letJoinK<B44C>) */
	movq	(%r10), %rdi
	movq	(%rbx), %r8
	movq	8(%rbx), %r9
	movq	16(%rbx), %r10
	jmp	revQueue.7A
LB2:
else.97:
	/* block else<B4D8> (ep<B4D5>,hd<B4D7>,tl<B4D6>) */
	movq	$1, %r15
	jmp	letJoinK.98
	.text
switch.BE:
	movq	%rax, %rcx
	movq	%rdi, %rdx
gcTestC0:
	movq	%r11, %rbx
	subq	%rsi, %rbx
	jle	doGCC1
check.B6:
	/* block check<B99C> (ep<B388>,s<B387>) */
	movq	$-1048576, %r13
	andq	%rsi, %r13
	movq	$263, -8(%rsi)
	movabsq	$dispatch.5E, %r14
	movq	%r14, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%r13, 16(%rsi)
	movq	%rsi, %r12
	addq	$32, %rsi
	cmpq	$1, %rcx
	jne	L_trueB7
else.B8:
	/* Liveout:  GP={%rdi}  */
	/* block else<B4F6> (dispatch<B4F5>) */
	movq	%r12, %rdi
	jmp	dispatch.5E
doGCC1:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	%r15, %rdi
	movabsq	$retGCBF, %r8
	jmp	ASM_InvokeGC
	/* live= GP={%rcx %rdx} spilled=  */
retGCBF:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
	jmp	gcTestC0
L_trueB7:
then.B9:
	/* block then<B4E0> (ep<B4DC>,s<B4DF>,self<B4DE>,dispatch<B4DD>) */
	cmpq	$1, (%rcx)
	jne	LC2
L_trueBA:
then.BC:
	/* Liveout:  GP={%rdi}  */
	/* block then<B4E8> (self<B4E7>,dispatch<B4E6>,s<B4E5>) */
	movq	$28, -8(%rsi)
	movq	32(%r13), %rbx
	movq	%rbx, (%rsi)
	movq	8(%rcx), %r10
	movq	%r10, 8(%rsi)
	movq	80(%r13), %r14
	movq	%r14, 16(%rsi)
	movq	%rsi, %rdx
	addq	$32, %rsi
	movq	%rdx, 80(%r13)
	movq	%r12, %rdi
	jmp	dispatch.5E
LC2:
else.BB:
	/* Liveout:  GP={%rax %rdi}  */
	/* block else<B4EF> (ep<B4EE>) */
	movq	$12, -8(%rsi)
	movabsq	$tagBD, %r15
	movq	%r15, (%rsi)
	movq	%rsi, %r10
	addq	$16, %rsi
	movq	8(%rdx), %rbx
	movq	(%rbx), %rcx
	movq	%r10, %rax
	movq	%rbx, %rdi
	jmp	*%rcx
	.text
mkSwitch.C4:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTestC6
	/* live= GP={%r10 %rcx %rdx %rbx} spilled=  */
retGCC5:
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTestC6:
	movq	%r11, %r13
	subq	%rsi, %r13
	jle	doGCC7
check.C3:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<B9A1> (ep<B4FA>,_wild<B4FB>,retK<B4FC>,exh<B4FD>) */
	movq	(%rcx), %r10
	movq	%rcx, %rdi
	movq	(%rbx), %r8
	jmp	*%r10
doGCC7:
	movq	$1673, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %r12
	addq	$40, %rsi
	movq	%r12, %rdi
	movabsq	$retGCC5, %r8
	jmp	ASM_InvokeGC
	.text
schedCont.C9:
	movq	%rax, %rcx
	movq	%rdi, %rdx
	jmp	gcTestCB
	/* live= GP={%rcx %rdx} spilled=  */
retGCCA:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTestCB:
	movq	%r11, %rbx
	subq	%rsi, %rbx
	jle	doGCCC
check.C8:
	/* Liveout:  GP={%rax %rdi}  */
	/* block check<B9A4> (ep<B50F>,k<B50E>) */
	movq	$20, -8(%rsi)
	movq	$1, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	$-1048576, %rbx
	andq	%rsi, %rbx
	movq	$3, 8(%rbx)
	movq	40(%rbx), %r13
	movq	8(%r13), %r14
	movq	%r14, 40(%rbx)
	movq	(%r13), %r10
	movq	(%r10), %r15
	movq	%r12, %rax
	movq	%r10, %rdi
	jmp	*%r15
doGCCC:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	%rcx, %rdi
	movabsq	$retGCCA, %r8
	jmp	ASM_InvokeGC
	.text
dummyK.CE:
	movq	%rax, %rcx
	movq	%rdi, %rdx
	jmp	gcTestD0
	/* live= GP={%rcx %rdx} spilled=  */
retGCCF:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTestD0:
	movq	%r11, %rbx
	subq	%rsi, %rbx
	jle	doGCD1
check.CD:
	/* Liveout:  GP={%rax %rdi}  */
	/* block check<B9A7> (ep<B529>,x<B528>) */
	movq	$-1048576, %rbx
	andq	%rsi, %rbx
	movq	$3, 8(%rbx)
	movq	40(%rbx), %r12
	movq	8(%r12), %r13
	movq	%r13, 40(%rbx)
	movq	(%r12), %r10
	movq	(%r10), %r14
	movq	$1, %r15
	movq	%r15, %rax
	movq	%r10, %rdi
	jmp	*%r14
doGCD1:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	%rcx, %rdi
	movabsq	$retGCCF, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.D3:
	movq	%r8, %r13
	movq	%rdi, %r12
	jmp	gcTestD5
	/* live= GP={%r13 %r12} spilled=  */
retGCD4:
	movq	8(%rdi), %r13
	movq	(%rdi), %r12
gcTestD5:
	movq	%r11, %r14
	subq	%rsi, %r14
	jle	doGCD6
check.D2:
	/* Liveout:  GP={%rdi}  */
	/* block check<B9AA> (ep<B535>,act<B534>) */
	movq	$20, -8(%rsi)
	movq	%r13, (%rsi)
	movq	8(%r12), %r14
	movq	40(%r14), %r15
	movq	%r15, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	$-1048576, %r13
	andq	%rsi, %r13
	movq	%rsi, 120(%r13)
	movq	$1, (%r13)
	movq	%rax, -80(%rbp)
	movq	%rcx, -96(%rbp)
	movq	%rdx, %r15
	movq	%rdi, -56(%rbp)
	movq	%r8, -64(%rbp)
	movq	%r9, -88(%rbp)
	movq	%r10, -72(%rbp)
	movq	%r11, %rbx
	movq	$-1048576, %r10
	andq	%rsi, %r10
	movq	%r10, %rdi
	movq	%r14, %rsi
	call	PromoteObj
	movq	%rax, %r14
	movq	-80(%rbp), %rax
	movq	-96(%rbp), %rcx
	movq	%r15, %rdx
	movq	-56(%rbp), %rdi
	movq	-64(%rbp), %r8
	movq	-88(%rbp), %r9
	movq	-72(%rbp), %r10
	movq	%rbx, %r11
	movq	120(%r13), %rsi
	movq	$3, (%r13)
	movq	8(%r12), %r15
	movq	%r14, 40(%r15)
	movq	16(%r12), %rcx
	movq	(%rcx), %rdx
	movq	%rcx, %rdi
	jmp	*%rdx
doGCD6:
	movq	$20, -8(%rsi)
	movq	%r12, (%rsi)
	movq	%r13, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	%rbx, %rdi
	movabsq	$retGCD4, %r8
	jmp	ASM_InvokeGC
	.text
initVPFields.D8:
	movq	%r10, %r14
	movq	%r9, %r13
	movq	%r8, %r12
	movq	%rdi, %rbx
	jmp	gcTestDA
	/* live= GP={%r14} spilled= GP={%r~1 %r~1 %r~1}  */
retGCD9:
	movq	24(%rdi), %r14
	movq	16(%rdi), %r13
	movq	8(%rdi), %r12
	movq	(%rdi), %rbx
gcTestDA:
	movq	%r11, %r15
	subq	%rsi, %r15
	jle	doGCDB
	movq	%r13, -64(%rbp)
	movq	%r12, -72(%rbp)
	movq	%rbx, -56(%rbp)
check.D7:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block check<B9AF> (ep<B507>,vp<B508>,retK<B509>,exh<B50A>) */
	movq	$10, -8(%rsi)
	movabsq	$schedCont.C9, %r15
	movq	%r15, (%rsi)
	movq	%rsi, -80(%rbp)
	addq	$16, %rsi
	movq	$-1048576, %r15
	andq	%rsi, %r15
	movq	%rsi, 120(%r15)
	movq	$1, (%r15)
	movq	%rax, %rbx
	movq	%rcx, -88(%rbp)
	movq	%rdx, -96(%rbp)
	movq	%rdi, -104(%rbp)
	movq	%r8, -112(%rbp)
	movq	%r9, -120(%rbp)
	movq	%r10, %r12
	movq	%r11, %r13
	movq	$-1048576, %rcx
	andq	%rsi, %rcx
	movq	%rcx, %rdi
	movq	-80(%rbp), %rsi
	call	PromoteObj
	movq	%rax, %r14
	movq	%rbx, %rax
	movq	-88(%rbp), %rcx
	movq	-96(%rbp), %rdx
	movq	-104(%rbp), %rdi
	movq	-112(%rbp), %r8
	movq	-120(%rbp), %r9
	movq	%r12, %r10
	movq	%r13, %r11
	movq	120(%r15), %rsi
	movq	$3, (%r15)
	movq	-72(%rbp), %r15
	movq	%r14, 48(%r15)
	movq	$10, -8(%rsi)
	movl	$0, (%rsi)
	movq	%rsi, %rbx
	addq	$16, %rsi
	movq	$20, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	$1, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movq	%r12, (%rsi)
	movq	$1, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	$1289, -8(%rsi)
	movl	$-1, (%rsi)
	movq	$1, 8(%rsi)
	xorl	%r14d, %r14d
	incl	%r14d
	movl	%r14d, 16(%rsi)
	movq	%r13, 24(%rsi)
	movq	%rsi, %r15
	addq	$40, %rsi
	movq	$-1048576, %r14
	andq	%rsi, %r14
	movq	%rsi, 120(%r14)
	movq	$1, (%r14)
	movq	%rax, -80(%rbp)
	movq	%rcx, %rbx
	movq	%rdx, %r12
	movq	%rdi, -96(%rbp)
	movq	%r8, -88(%rbp)
	movq	%r9, %r13
	movq	%r10, -112(%rbp)
	movq	%r11, -104(%rbp)
	movq	$-1048576, %rcx
	andq	%rsi, %rcx
	movq	%rcx, %rdi
	movq	%r15, %rsi
	call	PromoteObj
	movq	%rax, %r15
	movq	-80(%rbp), %rax
	movq	%rbx, %rcx
	movq	%r12, %rdx
	movq	-96(%rbp), %rdi
	movq	-88(%rbp), %r8
	movq	%r13, %r9
	movq	-112(%rbp), %r10
	movq	-104(%rbp), %r11
	movq	120(%r14), %rsi
	movq	$3, (%r14)
	movq	-72(%rbp), %rbx
	movq	%r15, 32(%rbx)
	movq	$10, -8(%rsi)
	movabsq	$dummyK.CE, %r15
	movq	%r15, (%rsi)
	movq	%rsi, %r12
	addq	$16, %rsi
	movq	$-1048576, %rbx
	andq	%rsi, %rbx
	movq	%rsi, 120(%rbx)
	movq	$1, (%rbx)
	movq	%rax, %r13
	movq	%rcx, -96(%rbp)
	movq	%rdx, -104(%rbp)
	movq	%rdi, -112(%rbp)
	movq	%r8, %r14
	movq	%r9, %r15
	movq	%r10, -80(%rbp)
	movq	%r11, -88(%rbp)
	movq	$-1048576, %rcx
	andq	%rsi, %rcx
	movq	%rcx, %rdi
	movq	%r12, %rsi
	call	PromoteObj
	movq	%rax, %r12
	movq	%r13, %rax
	movq	-96(%rbp), %rcx
	movq	-104(%rbp), %rdx
	movq	-112(%rbp), %rdi
	movq	%r14, %r8
	movq	%r15, %r9
	movq	-80(%rbp), %r10
	movq	-88(%rbp), %r11
	movq	120(%rbx), %rsi
	movq	$3, (%rbx)
	movq	-72(%rbp), %r13
	movq	%r12, 56(%r13)
	movq	$519, -8(%rsi)
	movabsq	$letJoinK.D3, %rdx
	movq	%rdx, (%rsi)
	movq	-72(%rbp), %r14
	movq	%r14, 8(%rsi)
	movq	-64(%rbp), %r15
	movq	%r15, 16(%rsi)
	movq	%rsi, %rcx
	addq	$32, %rsi
	movq	-56(%rbp), %rdx
	movq	(%rdx), %rbx
	movq	(%rbx), %rdi
	movq	-72(%rbp), %r8
	movq	%rcx, %r9
	movq	$1, %r10
	jmp	mkSwitch.C4
doGCDB:
	movq	$1673, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%r12, 8(%rsi)
	movq	%r13, 16(%rsi)
	movq	%r14, 24(%rsi)
	movq	%rsi, %r10
	addq	$40, %rsi
	movq	%r10, %rdi
	movabsq	$retGCD9, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.DE:
	movq	%rdi, %rcx
	jmp	gcTestE0
	/* live= GP={%rcx} spilled=  */
retGCDF:
	movq	(%rdi), %rcx
gcTestE0:
	movq	%r11, %r10
	subq	%rsi, %r10
	jle	doGCE1
check.DC:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block check<B9B1> (ep<B558>) */
	movq	8(%rcx), %rdx
	movq	(%rdx), %rdi
	movq	24(%rcx), %r8
	movq	16(%rcx), %r9
	jmp	lp.DD
doGCE1:
	movq	$12, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%rsi, %rbx
	addq	$16, %rsi
	movq	%rbx, %rdi
	movabsq	$retGCDF, %r8
	jmp	ASM_InvokeGC
	.text
lp.DD:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTestE8
	/* live= GP={%rcx %rdx %rbx} spilled=  */
retGCE7:
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTestE8:
	movq	%r11, %r10
	subq	%rsi, %r10
	jle	doGCE9
check.E2:
	/* block check<B9B5> (ep<B547>,vps<B548>,retK<B549>) */
	cmpq	$1, %rdx
	je	LEA
L_trueE3:
then.E5:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block then<B551> (ep<B54E>,vps<B550>,retK<B54F>) */
	movq	$133, -8(%rsi)
	movq	%rbx, (%rsi)
	movabsq	$lp.DD, %r12
	movq	%r12, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	$1801, -8(%rsi)
	movabsq	$letJoinK.DE, %r14
	movq	%r14, (%rsi)
	movq	%r10, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	8(%rdx), %r15
	movq	%r15, 24(%rsi)
	movq	%rsi, %r13
	addq	$40, %rsi
	movq	8(%rbx), %rcx
	movq	(%rcx), %rdi
	movq	(%rdx), %rdx
	movq	(%rdx), %r8
	movq	%r13, %r9
	movq	(%rbx), %r10
	jmp	initVPFields.D8
doGCE9:
	movq	$28, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%rsi, %rbx
	addq	$32, %rsi
	movq	%rbx, %rdi
	movabsq	$retGCE7, %r8
	jmp	ASM_InvokeGC
LEA:
else.E4:
	/* Liveout:  GP={%rdi}  */
	/* block else<B566> (retK<B565>) */
	movq	%rcx, %rdi
	jmp	letJoinK.E6
	.text
retGCF3:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
	jmp	gcTestF4
L_trueEC:
then.EE:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block then<B5F0> (ep<B5EE>,retK<B5EF>) */
	pause
	movq	%rdx, %rdi
	movq	%rcx, %r8
spinLp.F2:
	movq	%r8, %rcx
	movq	%rdi, %rdx
gcTestF4:
	movq	%r11, %r15
	subq	%rsi, %r15
	jle	doGCF5
check.EB:
	/* block check<B9B8> (ep<B5E7>,retK<B5E8>) */
	movq	(%rdx), %r10
	leaq	(%r10), %rbx
	cmpl	$1, (%rbx)
	je	L_trueEC
else.ED:
	/* block else<B5F4> (ep<B5F2>,retK<B5F3>) */
	movq	$1, %r12
	movq	(%rdx), %r13
	lock
	xchgq	%r12, (%r13)
	cmpq	$1, %r12
	je	L_trueEF
else.F0:
	/* Liveout:  GP={%rdi}  */
	/* block else<B5FD> (retK<B5FC>) */
	movq	%rcx, %rdi
	jmp	letJoinK.D
L_trueEF:
then.F1:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block then<B5F9> (ep<B5F7>,retK<B5F8>) */
	movq	%rdx, %rdi
	movq	%rcx, %r8
	jmp	spinLp.F2
doGCF5:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	%r14, %rdi
	movabsq	$retGCF3, %r8
	jmp	ASM_InvokeGC
	/* live= GP={%rcx %rdx} spilled=  */
	jmp	retGCF3
	.text
k.F7:
	movq	%rax, %rcx
	movq	%rdi, %rdx
	jmp	gcTestF9
	/* live= GP={%rcx %rdx} spilled=  */
retGCF8:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTestF9:
	movq	%r11, %r15
	subq	%rsi, %r15
	jle	doGCFA
check.F6:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<B9BB> (ep<B595>,x<B591>) */
	movq	$-1048576, %rbx
	andq	%rsi, %rbx
	movq	$3, 8(%rbx)
	movq	$2827, -8(%rsi)
	movabsq	$letJoinK.D, %r12
	movq	%r12, (%rsi)
	movq	8(%rdx), %r13
	movq	%r13, 8(%rsi)
	movq	16(%rdx), %r14
	movq	%r14, 16(%rsi)
	movq	24(%rdx), %r15
	movq	%r15, 24(%rsi)
	movq	32(%rdx), %rcx
	movq	%rcx, 32(%rsi)
	movq	%rsi, %r10
	addq	$48, %rsi
	movq	$12, -8(%rsi)
	movq	8(%rdx), %rbx
	movq	%rbx, (%rsi)
	movq	%rsi, %rdx
	addq	$16, %rsi
	movq	$133, -8(%rsi)
	movq	%rdx, (%rsi)
	movabsq	$spinLp.F2, %r13
	movq	%r13, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	(%r12), %rdi
	movq	%r10, %r8
	jmp	spinLp.F2
doGCFA:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	%r14, %rdi
	movabsq	$retGCF8, %r8
	jmp	ASM_InvokeGC
	.text
dispatch.13:
	movq	%rdi, %rcx
	jmp	gcTestFD
	/* live= GP={%rcx} spilled=  */
retGCFC:
	movq	(%rdi), %rcx
gcTestFD:
	movq	%r11, %rdx
	subq	%rsi, %rdx
	jle	doGCFE
check.FB:
	/* Liveout:  GP={%rax %rdi}  */
	/* block check<B9BD> (ep<B58E>) */
	movq	$2827, -8(%rsi)
	movabsq	$k.F7, %r12
	movq	%r12, (%rsi)
	movq	8(%rcx), %r13
	movq	%r13, 8(%rsi)
	movq	16(%rcx), %r14
	movq	%r14, 16(%rsi)
	movq	24(%rcx), %r15
	movq	%r15, 24(%rsi)
	movq	%rcx, 32(%rsi)
	movq	%rsi, %r10
	addq	$48, %rsi
	movq	$20, -8(%rsi)
	movq	$1, (%rsi)
	movq	%r10, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	24(%rcx), %r10
	movq	40(%r10), %rdx
	movq	8(%rdx), %r12
	movq	24(%rcx), %r13
	movq	%r12, 40(%r13)
	movq	(%rdx), %rdx
	movq	(%rdx), %r14
	movq	%rbx, %rax
	movq	%rdx, %rdi
	jmp	*%r14
doGCFE:
	movq	$12, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%rsi, %r15
	addq	$16, %rsi
	movq	%r15, %rdi
	movabsq	$retGCFC, %r8
	jmp	ASM_InvokeGC
	.text
retGC107:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
	jmp	gcTest108
L_true100:
then.102:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block then<B65C> (ep<B65A>,retK<B65B>) */
	pause
	movq	%rdx, %rdi
	movq	%rcx, %r8
spinLp.106:
	movq	%r8, %rcx
	movq	%rdi, %rdx
gcTest108:
	movq	%r11, %r15
	subq	%rsi, %r15
	jle	doGC109
check.FF:
	/* block check<B9C0> (ep<B653>,retK<B654>) */
	movq	(%rdx), %r10
	leaq	(%r10), %rbx
	cmpl	$1, (%rbx)
	je	L_true100
else.101:
	/* block else<B660> (ep<B65E>,retK<B65F>) */
	movq	$1, %r12
	movq	(%rdx), %r13
	lock
	xchgq	%r12, (%r13)
	cmpq	$1, %r12
	je	L_true103
else.104:
	/* Liveout:  GP={%rdi}  */
	/* block else<B669> (retK<B668>) */
	movq	%rcx, %rdi
	jmp	letJoinK.19
L_true103:
then.105:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block then<B665> (ep<B663>,retK<B664>) */
	movq	%rdx, %rdi
	movq	%rcx, %r8
	jmp	spinLp.106
doGC109:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	%r14, %rdi
	movabsq	$retGC107, %r8
	jmp	ASM_InvokeGC
	/* live= GP={%rcx %rdx} spilled=  */
	jmp	retGC107
	.text
letJoinK.10B:
	movq	%rdi, %r14
	jmp	gcTest10D
	/* live= spilled= GP={%r~1}  */
retGC10C:
	movq	(%rdi), %r14
gcTest10D:
	movq	%r11, %rbx
	subq	%rsi, %rbx
	jle	doGC10E
	movq	%r14, -64(%rbp)
check.10A:
	/* Liveout:  GP={%rax %rdi}  */
	/* flushLoads */
	/* block check<B9C2> (ep<B688>) */
	movq	$-1048576, %r13
	andq	%rsi, %r13
	movq	%rsi, 120(%r13)
	movq	$1, (%r13)
	movq	%rax, -104(%rbp)
	movq	%rcx, -96(%rbp)
	movq	%rdx, %r14
	movq	%rdi, %r15
	movq	%r8, -80(%rbp)
	movq	%r9, -72(%rbp)
	movq	%r10, %rbx
	movq	%r11, %r12
	movq	$-1048576, %rcx
	andq	%rsi, %rcx
	movq	%rcx, %rdi
	movq	-64(%rbp), %r10
	movq	8(%r10), %rdx
	movq	16(%rdx), %rcx
	movq	%rcx, %rsi
	call	PromoteObj
	movq	%rax, -56(%rbp)
	movq	-104(%rbp), %rax
	movq	-96(%rbp), %rcx
	movq	%r14, %rdx
	movq	%r15, %rdi
	movq	-80(%rbp), %r8
	movq	-72(%rbp), %r9
	movq	%rbx, %r10
	movq	%r12, %r11
	movq	120(%r13), %rsi
	movq	$3, (%r13)
	movq	$-1048576, %rbx
	andq	%rsi, %rbx
	movq	%rsi, 120(%rbx)
	movq	$1, (%rbx)
	movq	%rax, %r12
	movq	%rcx, -72(%rbp)
	movq	%rdx, -80(%rbp)
	movq	%rdi, -88(%rbp)
	movq	%r8, -96(%rbp)
	movq	%r9, -104(%rbp)
	movq	%r10, %r13
	movq	%r11, %r14
	movq	$-1048576, %r10
	andq	%rsi, %r10
	movq	%r10, %rdi
	movq	-64(%rbp), %r15
	movq	8(%r15), %r15
	movq	24(%r15), %r10
	movq	%r10, %rsi
	call	PromoteObj
	movq	%rax, %r15
	movq	%r12, %rax
	movq	-72(%rbp), %rcx
	movq	-80(%rbp), %rdx
	movq	-88(%rbp), %rdi
	movq	-96(%rbp), %r8
	movq	-104(%rbp), %r9
	movq	%r13, %r10
	movq	%r14, %r11
	movq	120(%rbx), %rsi
	movq	$3, (%rbx)
	movq	-64(%rbp), %rbx
	movq	8(%rbx), %r13
	movq	-64(%rbp), %r12
	movq	8(%r12), %r14
	movq	-56(%rbp), %r13
	movq	%r13, (%r15)
	movq	$-1048576, %r15
	andq	%rsi, %r15
	movq	%rsi, 120(%r15)
	movq	$1, (%r15)
	movq	%rax, %rbx
	movq	%rcx, -56(%rbp)
	movq	%rdx, %r12
	movq	%rdi, %r13
	movq	%r8, -96(%rbp)
	movq	%r9, -72(%rbp)
	movq	%r10, -88(%rbp)
	movq	%r11, -80(%rbp)
	movq	$-1048576, %rcx
	andq	%rsi, %rcx
	movq	%rcx, %rdi
	movq	-64(%rbp), %r14
	movq	8(%r14), %rdx
	movq	8(%rdx), %rcx
	movq	%rcx, %rsi
	call	PromoteObj
	movq	%rax, %r14
	movq	%rbx, %rax
	movq	-56(%rbp), %rcx
	movq	%r12, %rdx
	movq	%r13, %rdi
	movq	-96(%rbp), %r8
	movq	-72(%rbp), %r9
	movq	-88(%rbp), %r10
	movq	-80(%rbp), %r11
	movq	120(%r15), %rsi
	movq	$3, (%r15)
	movq	-64(%rbp), %r15
	movq	8(%r15), %rcx
	movq	(%r14), %rax
	xorq	%rdx, %rdx
	lock
	cmpxchgq	%rdx, (%r14)
	movq	-64(%rbp), %rcx
	movq	16(%rcx), %r10
	movq	40(%r10), %rbx
	movq	8(%rbx), %r12
	movq	-64(%rbp), %rdx
	movq	16(%rdx), %r13
	movq	%r12, 40(%r13)
	movq	(%rbx), %r14
	movq	(%r14), %r15
	movq	$1, %rcx
	movq	%rcx, %rax
	movq	%r14, %rdi
	jmp	*%r15
doGC10E:
	movq	$12, -8(%rsi)
	movq	%r14, (%rsi)
	movq	%rsi, %rdx
	addq	$16, %rsi
	movq	%rdx, %rdi
	movabsq	$retGC10C, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.110:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest112
	/* live= GP={%rcx %rdx} spilled=  */
retGC111:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest112:
	movq	%r11, %rbx
	subq	%rsi, %rbx
	jle	doGC113
check.10F:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block check<B9C5> (ep<B681>,_t<B67E>) */
	movq	$20, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	32(%rdx), %r10
	movq	%r10, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	$263, -8(%rsi)
	movabsq	$letJoinK.10B, %r13
	movq	%r13, (%rsi)
	movq	16(%rdx), %r14
	movq	%r14, 8(%rsi)
	movq	24(%rdx), %r15
	movq	%r15, 16(%rsi)
	movq	%rsi, %r12
	addq	$32, %rsi
	movq	8(%rdx), %rcx
	movq	(%rcx), %rdi
	movq	%rbx, %r8
	movq	%r12, %r9
	jmp	set_D_ite.3D
doGC113:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	movq	%rdx, %rdi
	movabsq	$retGC111, %r8
	jmp	ASM_InvokeGC
	.text
terminate.115:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest117
	/* live= GP={%rcx %rdx} spilled=  */
retGC116:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest117:
	movq	%r11, %rbx
	subq	%rsi, %rbx
	jle	doGC118
check.114:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block check<B9C8> (ep<B679>,self<B675>) */
	movq	$2827, -8(%rsi)
	movabsq	$letJoinK.110, %r10
	movq	%r10, (%rsi)
	movq	24(%rdx), %r12
	movq	%r12, 8(%rsi)
	movq	32(%rdx), %r13
	movq	%r13, 16(%rsi)
	movq	%rcx, 24(%rsi)
	movq	32(%rdx), %r14
	movq	32(%r14), %r15
	movq	%r15, 32(%rsi)
	movq	%rsi, %rbx
	addq	$48, %rsi
	movq	16(%rdx), %rcx
	movq	(%rcx), %rdi
	movq	%rbx, %r8
	movq	8(%rdx), %r9
	jmp	get_D_ite.37
doGC118:
	movq	$133, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	movq	%rdx, %rdi
	movabsq	$retGC116, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.11E:
	movq	%rdi, %r12
gcTest120:
	movq	%r11, %r13
	subq	%rsi, %r13
	jle	doGC121
check.119:
	/* block check<B9CA> (ep<B6BE>) */
	movq	$-1048576, %r13
	andq	%rsi, %r13
	movq	%rsi, 120(%r13)
	movq	$1, (%r13)
	movq	%rax, -56(%rbp)
	movq	%rcx, -80(%rbp)
	movq	%rdx, %r15
	movq	%rdi, %rbx
	movq	%r8, -64(%rbp)
	movq	%r9, -72(%rbp)
	movq	%r10, -88(%rbp)
	movq	%r11, -96(%rbp)
	movq	$-1048576, %r14
	andq	%rsi, %r14
	movq	%r14, %rdi
	movq	8(%r12), %r14
	movq	8(%r14), %r10
	movq	%r10, %rsi
	call	PromoteObj
	movq	%rax, %r14
	movq	-56(%rbp), %rax
	movq	-80(%rbp), %rcx
	movq	%r15, %rdx
	movq	%rbx, %rdi
	movq	-64(%rbp), %r8
	movq	-72(%rbp), %r9
	movq	-88(%rbp), %r10
	movq	-96(%rbp), %r11
	movq	120(%r13), %rsi
	movq	$3, (%r13)
	movq	8(%r12), %rbx
	movq	(%r14), %rax
	movq	24(%r12), %r13
	lock
	cmpxchgq	%r13, (%r14)
	movq	$-1048576, %r13
	andq	%rsi, %r13
	movq	%rsi, 120(%r13)
	movq	$1, (%r13)
	movq	%rax, %r15
	movq	%rcx, -96(%rbp)
	movq	%rdx, %rbx
	movq	%rdi, -88(%rbp)
	movq	%r8, -80(%rbp)
	movq	%r9, -72(%rbp)
	movq	%r10, -64(%rbp)
	movq	%r11, -56(%rbp)
	movq	$-1048576, %r14
	andq	%rsi, %r14
	movq	%r14, %rdi
	movq	8(%r12), %rcx
	movq	(%rcx), %r14
	movq	%r14, %rsi
	call	PromoteObj
	movq	%rax, %r14
	movq	%r15, %rax
	movq	-96(%rbp), %rcx
	movq	%rbx, %rdx
	movq	-88(%rbp), %rdi
	movq	-80(%rbp), %r8
	movq	-72(%rbp), %r9
	movq	-64(%rbp), %r10
	movq	-56(%rbp), %r11
	movq	120(%r13), %rsi
	movq	$3, (%r13)
	movq	(%r14), %rdx
	cmpq	$1, %rdx
	jne	L122
S_case11A:
case.11B:
	/* Liveout:  GP={%rax %rdi}  */
	/* block case<B6CD> (ep<B6CC>) */
	movq	$20, -8(%rsi)
	movq	32(%r12), %r10
	movq	%r10, (%rsi)
	movq	24(%r12), %r13
	movq	40(%r13), %r14
	movq	%r14, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	24(%r12), %r15
	movq	%rbx, 40(%r15)
	movq	24(%r12), %rcx
	movq	$1, 8(%rcx)
	movq	40(%r12), %rdx
	movq	(%rdx), %rbx
	movq	$1, %r10
	movq	%r10, %rax
	movq	%rdx, %rdi
	jmp	*%rbx
doGC121:
	movq	$12, -8(%rsi)
	movq	%r12, (%rsi)
	movq	%rsi, %r12
	addq	$16, %rsi
	movq	%r12, %rdi
	movabsq	$retGC11F, %r8
	jmp	ASM_InvokeGC
	/* live= GP={%r12} spilled=  */
retGC11F:
	movq	(%rdi), %r12
	jmp	gcTest120
L122:
	cmpq	$3, %rdx
	jne	S_case11A
S_case11C:
case.11D:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block case<B6DA> (ep<B6D9>) */
	movq	16(%r12), %rdi
	movq	24(%r12), %r8
	jmp	terminate.115
	.text
letJoinK.124:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest126
	/* live= GP={%rcx %rdx} spilled=  */
retGC125:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest126:
	movq	%r11, %r14
	subq	%rsi, %r14
	jle	doGC127
check.123:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block check<B9CD> (ep<B6B4>,_t<B6AF>) */
	movq	$20, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	16(%rdx), %rbx
	movq	%rbx, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	$6925, -8(%rsi)
	movabsq	$letJoinK.11E, %r12
	movq	%r12, (%rsi)
	movq	24(%rdx), %r13
	movq	%r13, 8(%rsi)
	movq	32(%rdx), %r14
	movq	%r14, 16(%rsi)
	movq	40(%rdx), %r15
	movq	%r15, 24(%rsi)
	movq	48(%rdx), %rcx
	movq	%rcx, 32(%rsi)
	movq	56(%rdx), %r10
	movq	%r10, 40(%rsi)
	movq	%rsi, %r10
	addq	$56, %rsi
	movq	8(%rdx), %r12
	movq	(%r12), %rdi
	movq	%rbx, %r8
	movq	%r10, %r9
	jmp	set_D_ite.3D
doGC127:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	%r13, %rdi
	movabsq	$retGC125, %r8
	jmp	ASM_InvokeGC
	.text
dispatch.129:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest12B
	/* live= GP={%r10 %rcx %rdx %rbx} spilled=  */
retGC12A:
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest12B:
	movq	%r11, %r15
	subq	%rsi, %r15
	jle	doGC12C
check.128:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block check<B9D2> (ep<B6AC>,self<B6A6>,handler<B6A7>,k<B6A8>) */
	movq	$28433, -8(%rsi)
	movabsq	$letJoinK.124, %r12
	movq	%r12, (%rsi)
	movq	24(%rbx), %r13
	movq	%r13, 8(%rsi)
	movq	32(%rbx), %r14
	movq	%r14, 16(%rsi)
	movq	40(%rbx), %r15
	movq	%r15, 24(%rsi)
	movq	48(%rbx), %r12
	movq	%r12, 32(%rsi)
	movq	%rdx, 40(%rsi)
	movq	%rcx, 48(%rsi)
	movq	%r10, 56(%rsi)
	movq	%rsi, %r10
	addq	$72, %rsi
	movq	16(%rbx), %r13
	movq	(%r13), %rdi
	movq	%r10, %r8
	movq	8(%rbx), %r9
	jmp	get_D_ite.37
doGC12C:
	movq	$1673, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %r14
	addq	$40, %rsi
	movq	%r14, %rdi
	movabsq	$retGC12A, %r8
	jmp	ASM_InvokeGC
	.text
k.12E:
	movq	%rax, %rcx
	movq	%rdi, %rdx
	jmp	gcTest130
	/* live= GP={%rcx %rdx} spilled=  */
retGC12F:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest130:
	movq	%r11, %r12
	subq	%rsi, %r12
	jle	doGC131
check.12D:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block check<B9D5> (ep<B71F>,x<B71A>) */
	movq	$-1048576, %rbx
	andq	%rsi, %rbx
	movq	$3, 8(%rbx)
	movq	8(%rdx), %rdi
	movq	24(%rdx), %r8
	movq	16(%rdx), %r9
	movq	32(%rdx), %r10
	jmp	dispatch.129
doGC131:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	%r10, %rdi
	movabsq	$retGC12F, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.133:
	movq	%rdi, %r15
	jmp	gcTest135
	/* live= GP={%r15} spilled=  */
retGC134:
	movq	(%rdi), %r15
gcTest135:
	movq	%r11, %rbx
	subq	%rsi, %rbx
	jle	doGC136
check.132:
	/* Liveout:  GP={%rax %rdi}  */
	/* flushLoads */
	/* block check<B9D7> (ep<B70A>) */
	movq	$-1048576, %rbx
	andq	%rsi, %rbx
	movq	%rsi, 120(%rbx)
	movq	$1, (%rbx)
	movq	%rax, %r12
	movq	%rcx, %r13
	movq	%rdx, -80(%rbp)
	movq	%rdi, -88(%rbp)
	movq	%r8, -64(%rbp)
	movq	%r9, -72(%rbp)
	movq	%r10, %r14
	movq	%r11, -96(%rbp)
	movq	$-1048576, %rcx
	andq	%rsi, %rcx
	movq	%rcx, %rdi
	movq	8(%r15), %rcx
	movq	16(%rcx), %r10
	movq	%r10, %rsi
	call	PromoteObj
	movq	%rax, -56(%rbp)
	movq	%r12, %rax
	movq	%r13, %rcx
	movq	-80(%rbp), %rdx
	movq	-88(%rbp), %rdi
	movq	-64(%rbp), %r8
	movq	-72(%rbp), %r9
	movq	%r14, %r10
	movq	-96(%rbp), %r11
	movq	120(%rbx), %rsi
	movq	$3, (%rbx)
	movq	$-1048576, %rbx
	andq	%rsi, %rbx
	movq	%rsi, 120(%rbx)
	movq	$1, (%rbx)
	movq	%rax, -72(%rbp)
	movq	%rcx, -80(%rbp)
	movq	%rdx, -96(%rbp)
	movq	%rdi, -104(%rbp)
	movq	%r8, %r12
	movq	%r9, %r13
	movq	%r10, -88(%rbp)
	movq	%r11, -64(%rbp)
	movq	$-1048576, %r10
	andq	%rsi, %r10
	movq	%r10, %rdi
	movq	8(%r15), %r10
	movq	24(%r10), %rdx
	movq	%rdx, %rsi
	call	PromoteObj
	movq	%rax, %r14
	movq	-72(%rbp), %rax
	movq	-80(%rbp), %rcx
	movq	-96(%rbp), %rdx
	movq	-104(%rbp), %rdi
	movq	%r12, %r8
	movq	%r13, %r9
	movq	-88(%rbp), %r10
	movq	-64(%rbp), %r11
	movq	120(%rbx), %rsi
	movq	$3, (%rbx)
	movq	8(%r15), %rbx
	movq	8(%r15), %r12
	movq	-56(%rbp), %rbx
	movq	%rbx, (%r14)
	movq	$-1048576, %r14
	andq	%rsi, %r14
	movq	%rsi, 120(%r14)
	movq	$1, (%r14)
	movq	%rax, -56(%rbp)
	movq	%rcx, %rbx
	movq	%rdx, -64(%rbp)
	movq	%rdi, -80(%rbp)
	movq	%r8, %r12
	movq	%r9, -88(%rbp)
	movq	%r10, -72(%rbp)
	movq	%r11, -104(%rbp)
	movq	$-1048576, %rcx
	andq	%rsi, %rcx
	movq	%rcx, %rdi
	movq	8(%r15), %r13
	movq	8(%r13), %r13
	movq	%r13, %rsi
	call	PromoteObj
	movq	%rax, %r13
	movq	-56(%rbp), %rax
	movq	%rbx, %rcx
	movq	-64(%rbp), %rdx
	movq	-80(%rbp), %rdi
	movq	%r12, %r8
	movq	-88(%rbp), %r9
	movq	-72(%rbp), %r10
	movq	-104(%rbp), %r11
	movq	120(%r14), %rsi
	movq	$3, (%r14)
	movq	8(%r15), %rcx
	movq	(%r13), %rax
	xorq	%rdx, %rdx
	lock
	cmpxchgq	%rdx, (%r13)
	movq	$2827, -8(%rsi)
	movabsq	$k.12E, %r10
	movq	%r10, (%rsi)
	movq	16(%r15), %r12
	movq	%r12, 8(%rsi)
	movq	24(%r15), %r13
	movq	%r13, 16(%rsi)
	movq	32(%r15), %r14
	movq	%r14, 24(%rsi)
	movq	40(%r15), %rcx
	movq	%rcx, 32(%rsi)
	movq	%rsi, %rbx
	addq	$48, %rsi
	movq	$20, -8(%rsi)
	movq	$1, (%rsi)
	movq	%rbx, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	movq	32(%r15), %r10
	movq	40(%r10), %rbx
	movq	8(%rbx), %r12
	movq	32(%r15), %r13
	movq	%r12, 40(%r13)
	movq	(%rbx), %r14
	movq	(%r14), %r15
	movq	%rdx, %rax
	movq	%r14, %rdi
	jmp	*%r15
doGC136:
	movq	$12, -8(%rsi)
	movq	%r15, (%rsi)
	movq	%rsi, %rcx
	addq	$16, %rsi
	movq	%rcx, %rdi
	movabsq	$retGC134, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.138:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest13A
	/* live= GP={%rcx %rdx} spilled=  */
retGC139:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest13A:
	movq	%r11, %r14
	subq	%rsi, %r14
	jle	doGC13B
check.137:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block check<B9DA> (ep<B700>,_t<B6FC>) */
	movq	$20, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	56(%rdx), %rbx
	movq	%rbx, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	$5901, -8(%rsi)
	movabsq	$letJoinK.133, %r12
	movq	%r12, (%rsi)
	movq	16(%rdx), %r13
	movq	%r13, 8(%rsi)
	movq	24(%rdx), %r14
	movq	%r14, 16(%rsi)
	movq	32(%rdx), %r15
	movq	%r15, 24(%rsi)
	movq	40(%rdx), %rcx
	movq	%rcx, 32(%rsi)
	movq	48(%rdx), %r10
	movq	%r10, 40(%rsi)
	movq	%rsi, %r10
	addq	$56, %rsi
	movq	8(%rdx), %r12
	movq	(%r12), %rdi
	movq	%rbx, %r8
	movq	%r10, %r9
	jmp	set_D_ite.3D
doGC13B:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	%r13, %rdi
	movabsq	$retGC139, %r8
	jmp	ASM_InvokeGC
	.text
handler.143:
	movq	%rax, %rcx
	movq	%rdi, %rdx
gcTest145:
	movq	%r11, %r13
	subq	%rsi, %r13
	jle	doGC146
check.13C:
	/* block check<B9DD> (ep<B6E9>,s<B6E5>) */
	movq	$-1048576, %r12
	andq	%rsi, %r12
	cmpq	$1, %rcx
	jne	L_true13D
else.13E:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block else<B740> (ep<B73E>,self<B73F>) */
	movq	40(%rdx), %rdi
	movq	%r12, %r8
	jmp	terminate.115
doGC146:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	%r12, %rdi
	movabsq	$retGC144, %r8
	jmp	ASM_InvokeGC
	/* live= GP={%rcx %rdx} spilled=  */
retGC144:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
	jmp	gcTest145
L_true13D:
then.13F:
	/* block then<B6F1> (ep<B6EE>,s<B6F0>,self<B6EF>) */
	cmpq	$1, (%rcx)
	jne	L147
L_true140:
then.142:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block then<B6F8> (ep<B6F5>,self<B6F7>,s<B6F6>) */
	movq	$28433, -8(%rsi)
	movabsq	$letJoinK.138, %rbx
	movq	%rbx, (%rsi)
	movq	24(%rdx), %r10
	movq	%r10, 8(%rsi)
	movq	32(%rdx), %r13
	movq	%r13, 16(%rsi)
	movq	48(%rdx), %r14
	movq	%r14, 24(%rsi)
	movq	%rdx, 32(%rsi)
	movq	%r12, 40(%rsi)
	movq	8(%rcx), %r15
	movq	%r15, 48(%rsi)
	movq	32(%rdx), %rcx
	movq	32(%rcx), %rbx
	movq	%rbx, 56(%rsi)
	movq	%rsi, %r15
	addq	$72, %rsi
	movq	16(%rdx), %r10
	movq	(%r10), %rdi
	movq	%r15, %r8
	movq	8(%rdx), %r9
	jmp	get_D_ite.37
L147:
else.141:
	/* Liveout:  GP={%rax %rdi}  */
	/* block else<B739> (ep<B738>) */
	movq	$12, -8(%rsi)
	movabsq	$tagBD, %r13
	movq	%r13, (%rsi)
	movq	%rsi, %r10
	addq	$16, %rsi
	movq	8(%rdx), %rbx
	movq	(%rbx), %r14
	movq	%r10, %rax
	movq	%rbx, %rdi
	jmp	*%r14
	.text
wrappedK.149:
	movq	%rax, %rcx
	movq	%rdi, %rdx
	jmp	gcTest14B
	/* live= GP={%rcx %rdx} spilled=  */
retGC14A:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest14B:
	movq	%r11, %r12
	subq	%rsi, %r12
	jle	doGC14C
check.148:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block check<B9E0> (ep<B744>,x<B743>) */
	movq	$-1048576, %rbx
	andq	%rsi, %rbx
	movq	$3, 8(%rbx)
	movq	16(%rdx), %rdi
	movq	%rbx, %r8
	movq	24(%rdx), %r9
	movq	8(%rdx), %r10
	jmp	dispatch.129
doGC14C:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	%r10, %rdi
	movabsq	$retGC14A, %r8
	jmp	ASM_InvokeGC
	.text
schedulerLoop.15A:
	movq	%rax, %rbx
	movq	%rdi, %r13
	jmp	gcTest15C
	/* live= GP={%rbx %r13} spilled=  */
retGC15B:
	movq	8(%rdi), %rbx
	movq	(%rdi), %r13
gcTest15C:
	movq	%r11, %r14
	subq	%rsi, %r14
	jle	doGC15D
check.14D:
	/* block check<B9E3> (ep<B589>,s<B585>) */
	movq	$777, -8(%rsi)
	movabsq	$dispatch.13, %r12
	movq	%r12, (%rsi)
	movq	32(%r13), %r14
	movq	%r14, 8(%rsi)
	movq	%r13, 16(%rsi)
	movq	$-1048576, %r15
	andq	%rsi, %r15
	movq	%r15, 24(%rsi)
	movq	%rsi, %r12
	addq	$40, %rsi
	cmpq	$1, %rbx
	jne	L_true14E
else.14F:
	/* Liveout:  GP={%rdi}  */
	/* block else<B763> (dispatch<B762>) */
	movq	%r12, %rdi
	jmp	dispatch.13
L_true14E:
then.150:
	/* block then<B60D> (ep<B60A>,s<B60C>,dispatch<B60B>) */
	cmpq	$1, (%rbx)
	jne	L15E
L_true151:
then.153:
	/* block then<B614> (ep<B611>,dispatch<B613>,s<B612>) */
	movq	$-1048576, %r15
	andq	%rsi, %r15
	movq	32(%r15), %r14
	movq	8(%r14), %r15
	cmpq	$1, %r15
	jne	L_true154
else.155:
	/* Liveout:  GP={%rax %rdi}  */
	/* block else<B753> (ep<B752>) */
	movq	$133, -8(%rsi)
	movabsq	$str35, %rdx
	movq	%rdx, (%rsi)
	movl	$51, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movabsq	$tag36, %r10
	movq	%r10, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	8(%r13), %r12
	movq	(%r12), %r13
	movq	%rbx, %rax
	movq	%r12, %rdi
	jmp	*%r13
L_true154:
	movq	%r12, -56(%rbp)
	movq	8(%rbx), %rbx
then.156:
	/* block then<B61E> (ep<B61A>,dispatch<B61D>,k<B61C>,_t<B61B>) */
	movq	(%r15), %r14
	movq	8(%r14), %r12
	cmpq	$1, %r12
	jne	L_true157
	movq	%r13, -64(%rbp)
	jmp	letJoinK.158
L_true157:
	movq	%r14, -64(%rbp)
	movq	%rbx, -72(%rbp)
then.159:
	/* block then<B672> (ep<B66D>,dispatch<B671>,k<B670>,c<B66F>,ite<B66E>) */
	movq	(%r12), %rbx
	movq	$3851, -8(%rsi)
	movabsq	$terminate.115, %r14
	movq	%r14, (%rsi)
	movq	8(%r13), %r15
	movq	%r15, 8(%rsi)
	movq	16(%r13), %r14
	movq	%r14, 16(%rsi)
	movq	24(%r13), %r15
	movq	%r15, 24(%rsi)
	movq	%rbx, 32(%rsi)
	movq	%rsi, %r15
	addq	$48, %rsi
	movq	$16143, -8(%rsi)
	movabsq	$dispatch.129, %r14
	movq	%r14, (%rsi)
	movq	8(%r13), %r14
	movq	%r14, 8(%rsi)
	movq	16(%r13), %r14
	movq	%r14, 16(%rsi)
	movq	24(%r13), %r14
	movq	%r14, 24(%rsi)
	movq	%r12, 32(%rsi)
	movq	%rbx, 40(%rsi)
	movq	%r15, 48(%rsi)
	movq	%rsi, %r12
	addq	$64, %rsi
	movq	$60, -8(%rsi)
	movabsq	$handler.143, %r14
	movq	%r14, (%rsi)
	movq	8(%r13), %r14
	movq	%r14, 8(%rsi)
	movq	16(%r13), %r14
	movq	%r14, 16(%rsi)
	movq	24(%r13), %r14
	movq	%r14, 24(%rsi)
	movq	%rbx, 32(%rsi)
	movq	%r15, 40(%rsi)
	movq	%r12, 48(%rsi)
	movq	%rsi, %r15
	addq	$64, %rsi
	movq	$36, -8(%rsi)
	movabsq	$wrappedK.149, %rbx
	movq	%rbx, (%rsi)
	movq	-72(%rbp), %rbx
	movq	%rbx, 8(%rsi)
	movq	%r12, 16(%rsi)
	movq	%r15, 24(%rsi)
	movq	%rsi, %rbx
	addq	$40, %rsi
	movq	-64(%rbp), %r14
	movq	%r13, -64(%rbp)
letJoinK.158:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block letJoinK<B627> (ep<B623>,dispatch<B626>,ite<B625>,k<B624>) */
	movq	$20, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%r14, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movq	%r12, (%rsi)
	movq	$1, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	$-1048576, %r13
	andq	%rsi, %r13
	movq	%rsi, 120(%r13)
	movq	$1, (%r13)
	movq	%rax, -80(%rbp)
	movq	%rcx, -88(%rbp)
	movq	%rdx, -96(%rbp)
	movq	%rdi, -104(%rbp)
	movq	%r8, -72(%rbp)
	movq	%r9, %r15
	movq	%r10, %rbx
	movq	%r11, %r12
	movq	$-1048576, %rcx
	andq	%rsi, %rcx
	movq	%rcx, %rdi
	movq	%r14, %rsi
	call	PromoteObj
	movq	%rax, %r14
	movq	-80(%rbp), %rax
	movq	-88(%rbp), %rcx
	movq	-96(%rbp), %rdx
	movq	-104(%rbp), %rdi
	movq	-72(%rbp), %r8
	movq	%r15, %r9
	movq	%rbx, %r10
	movq	%r12, %r11
	movq	120(%r13), %rsi
	movq	$3, (%r13)
	movq	$1801, -8(%rsi)
	movabsq	$letJoinK.19, %r15
	movq	%r15, (%rsi)
	movq	-64(%rbp), %r10
	movq	32(%r10), %rcx
	movq	%rcx, 8(%rsi)
	movq	-56(%rbp), %r12
	movq	%r12, 16(%rsi)
	movq	%r14, 24(%rsi)
	movq	%rsi, %r13
	addq	$40, %rsi
	movq	$12, -8(%rsi)
	movq	-64(%rbp), %r14
	movq	32(%r14), %rbx
	movq	%rbx, (%rsi)
	movq	%rsi, %rdx
	addq	$16, %rsi
	movq	$133, -8(%rsi)
	movq	%rdx, (%rsi)
	movabsq	$spinLp.106, %r12
	movq	%r12, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	(%r10), %rdi
	movq	%r13, %r8
	jmp	spinLp.106
L15E:
else.152:
	/* Liveout:  GP={%rax %rdi}  */
	/* block else<B75C> (ep<B75B>) */
	movq	$12, -8(%rsi)
	movabsq	$tagBD, %rdx
	movq	%rdx, (%rsi)
	movq	%rsi, %rcx
	addq	$16, %rsi
	movq	8(%r13), %rbx
	movq	(%rbx), %r10
	movq	%rcx, %rax
	movq	%rbx, %rdi
	jmp	*%r10
doGC15D:
	movq	$20, -8(%rsi)
	movq	%r13, (%rsi)
	movq	%rbx, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	%r13, %rdi
	movabsq	$retGC15B, %r8
	jmp	ASM_InvokeGC
	.text
initK.160:
	movq	%rax, %rcx
	movq	%rdi, %rdx
	jmp	gcTest162
	/* live= GP={%rcx %rdx} spilled=  */
retGC161:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest162:
	movq	%r11, %r13
	subq	%rsi, %r13
	jle	doGC163
check.15F:
	/* Liveout:  GP={%rax %rdi}  */
	/* block check<B9E6> (ep<B766>,x<B765>) */
	movq	$1, %r10
	movq	8(%rdx), %rbx
	movq	%r10, %rax
	movq	%rbx, %rdi
	jmp	schedulerLoop.15A
doGC163:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	%r12, %rdi
	movabsq	$retGC161, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.21:
	movq	%rdi, %rcx
	jmp	gcTest166
	/* live= GP={%rcx} spilled=  */
retGC165:
	movq	(%rdi), %rcx
gcTest166:
	movq	%r11, %r13
	subq	%rsi, %r13
	jle	doGC167
check.164:
	/* Liveout:  GP={%rax %rdi}  */
	/* block check<B9E8> (ep<B790>) */
	movq	8(%rcx), %rdx
	movq	(%rdx), %rbx
	movq	$1, %r10
	movq	%r10, %rax
	movq	%rdx, %rdi
	jmp	*%rbx
doGC167:
	movq	$12, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%rsi, %r12
	addq	$16, %rsi
	movq	%r12, %rdi
	movabsq	$retGC165, %r8
	jmp	ASM_InvokeGC
	.text
k.26:
	movq	%rax, %rcx
	movq	%rdi, %rdx
	jmp	gcTest16A
	/* live= GP={%rcx %rdx} spilled=  */
retGC169:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest16A:
	movq	%r11, %r10
	subq	%rsi, %r10
	jle	doGC16B
check.168:
	/* Liveout:  GP={%rdi}  */
	/* block check<B9EB> (ep<B7A2>,x<B7A1>) */
	movq	8(%rdx), %rdi
	jmp	letJoinK.21
doGC16B:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	%rbx, %rdi
	movabsq	$retGC169, %r8
	jmp	ASM_InvokeGC
	.text
retGC174:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
	jmp	gcTest175
L_true16D:
then.16F:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block then<B7CA> (ep<B7C8>,retK<B7C9>) */
	pause
	movq	%rdx, %rdi
	movq	%rcx, %r8
spinLp.173:
	movq	%r8, %rcx
	movq	%rdi, %rdx
gcTest175:
	movq	%r11, %r15
	subq	%rsi, %r15
	jle	doGC176
check.16C:
	/* block check<B9EE> (ep<B7C1>,retK<B7C2>) */
	movq	(%rdx), %r10
	leaq	(%r10), %rbx
	cmpl	$1, (%rbx)
	je	L_true16D
else.16E:
	/* block else<B7CE> (ep<B7CC>,retK<B7CD>) */
	movq	$1, %r12
	movq	(%rdx), %r13
	lock
	xchgq	%r12, (%r13)
	cmpq	$1, %r12
	je	L_true170
else.171:
	/* Liveout:  GP={%rdi}  */
	/* block else<B7D7> (retK<B7D6>) */
	movq	%rcx, %rdi
	jmp	letJoinK.27
L_true170:
then.172:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block then<B7D3> (ep<B7D1>,retK<B7D2>) */
	movq	%rdx, %rdi
	movq	%rcx, %r8
	jmp	spinLp.173
doGC176:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	%r14, %rdi
	movabsq	$retGC174, %r8
	jmp	ASM_InvokeGC
	/* live= GP={%rcx %rdx} spilled=  */
	jmp	retGC174
	.text
spawnFn.178:
	movq	%r9, %r14
	movq	%r8, %r12
	movq	%rax, %r13
	movq	%rdi, %rbx
	jmp	gcTest17A
	/* live= GP={%r14 %r13} spilled= GP={%r~1 %r~1}  */
retGC179:
	movq	24(%rdi), %r14
	movq	16(%rdi), %r12
	movq	8(%rdi), %r13
	movq	(%rdi), %rbx
gcTest17A:
	movq	%r11, %r15
	subq	%rsi, %r15
	jle	doGC17B
	movq	%r12, -56(%rbp)
	movq	%rbx, -64(%rbp)
check.177:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<B9F3> (ep<B76E>,thd<B76F>,retK<B770>,exh<B771>) */
	movq	$-1048576, %r15
	andq	%rsi, %r15
	movq	$3, 8(%r15)
	movq	$20, -8(%rsi)
	movq	%r13, (%rsi)
	movq	$1, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	$-1048576, %rbx
	andq	%rsi, %rbx
	movq	%rsi, 120(%rbx)
	movq	$1, (%rbx)
	movq	%rax, %r13
	movq	%rcx, -80(%rbp)
	movq	%rdx, -96(%rbp)
	movq	%rdi, -104(%rbp)
	movq	%r8, -88(%rbp)
	movq	%r9, -112(%rbp)
	movq	%r10, %r14
	movq	%r11, -72(%rbp)
	movq	$-1048576, %rcx
	andq	%rsi, %rcx
	movq	%rcx, %rdi
	movq	%r12, %rsi
	call	PromoteObj
	movq	%rax, %r12
	movq	%r13, %rax
	movq	-80(%rbp), %rcx
	movq	-96(%rbp), %rdx
	movq	-104(%rbp), %rdi
	movq	-88(%rbp), %r8
	movq	-112(%rbp), %r9
	movq	%r14, %r10
	movq	-72(%rbp), %r11
	movq	120(%rbx), %rsi
	movq	$3, (%rbx)
	movq	$2827, -8(%rsi)
	movabsq	$letJoinK.27, %r13
	movq	%r13, (%rsi)
	movq	-64(%rbp), %rcx
	movq	(%rcx), %r14
	movq	%r14, 8(%rsi)
	movq	-56(%rbp), %rdx
	movq	%rdx, 16(%rsi)
	movq	%r15, 24(%rsi)
	movq	%r12, 32(%rsi)
	movq	%rsi, %r15
	addq	$48, %rsi
	movq	$12, -8(%rsi)
	movq	-64(%rbp), %rbx
	movq	(%rbx), %rdx
	movq	%rdx, (%rsi)
	movq	%rsi, %rcx
	addq	$16, %rsi
	movq	$133, -8(%rsi)
	movq	%rcx, (%rsi)
	movabsq	$spinLp.173, %r10
	movq	%r10, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	(%rbx), %rdi
	movq	%r15, %r8
	jmp	spinLp.173
doGC17B:
	movq	$36, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%r13, 8(%rsi)
	movq	%r12, 16(%rsi)
	movq	%r14, 24(%rsi)
	movq	%rsi, %r12
	addq	$40, %rsi
	movq	%r12, %rdi
	movabsq	$retGC179, %r8
	jmp	ASM_InvokeGC
	.text
removeFn.17D:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rax, %rbx
	movq	%rdi, %r10
	jmp	gcTest17F
	/* live= GP={%rcx %rdx %rbx %r10} spilled=  */
retGC17E:
	movq	24(%rdi), %rcx
	movq	16(%rdi), %rdx
	movq	8(%rdi), %rbx
	movq	(%rdi), %r10
gcTest17F:
	movq	%r11, %r15
	subq	%rsi, %r15
	jle	doGC180
check.17C:
	/* Liveout:  GP={%rax %rdi}  */
	/* block check<B9F8> (ep<B7DD>,thd<B7DE>,retK<B7DF>,exh<B7E0>) */
	movq	(%rdx), %r12
	movq	$1, %r13
	movq	%r13, %rax
	movq	%rdx, %rdi
	jmp	*%r12
doGC180:
	movq	$36, -8(%rsi)
	movq	%r10, (%rsi)
	movq	%rbx, 8(%rsi)
	movq	%rdx, 16(%rsi)
	movq	%rcx, 24(%rsi)
	movq	%rsi, %r14
	addq	$40, %rsi
	movq	%r14, %rdi
	movabsq	$retGC17E, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.2D:
	movq	%rdi, %rcx
	jmp	gcTest183
	/* live= GP={%rcx} spilled=  */
retGC182:
	movq	(%rdi), %rcx
gcTest183:
	movq	%r11, %r12
	subq	%rsi, %r12
	jle	doGC184
check.181:
	/* Liveout:  GP={%rdi}  */
	/* block check<B9FA> (ep<B811>) */
	movq	8(%rcx), %rdx
	movq	(%rdx), %rbx
	movq	%rdx, %rdi
	jmp	*%rbx
doGC184:
	movq	$12, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%rsi, %r10
	addq	$16, %rsi
	movq	%r10, %rdi
	movabsq	$retGC182, %r8
	jmp	ASM_InvokeGC
	.text
k.2B:
	movq	%rax, %rcx
	movq	%rdi, %rdx
	jmp	gcTest187
	/* live= GP={%rcx %rdx} spilled=  */
retGC186:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest187:
	movq	%r11, %r10
	subq	%rsi, %r10
	jle	doGC188
check.185:
	/* Liveout:  GP={%rdi}  */
	/* block check<B9FD> (ep<B822>,x<B821>) */
	movq	8(%rdx), %rdi
	jmp	letJoinK.2D
doGC188:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	%rbx, %rdi
	movabsq	$retGC186, %r8
	jmp	ASM_InvokeGC
	.text
retGC18C:
	movq	(%rdi), %r13
	jmp	gcTest18D
L_true18F:
then.18B:
	/* block then<B850> (ep<B9FE>) */
gcTest18D:
	movq	%r11, %r12
	subq	%rsi, %r12
	jle	doGC18E
check.189:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<B9FF> (ep<B84F>) */
	pause
	movq	%r13, %rdi
	movq	$1, %r8
lp.18A:
	movq	%rdi, %r13
	movq	8(%r13), %r14
	movq	312(%r14), %rbx
	movq	%rbx, -56(%rbp)
	movq	$28, -8(%rsi)
	movq	16(%r13), %r15
	movq	%r15, (%rsi)
	movq	(%r13), %rbx
	movq	%rbx, 8(%rsi)
	movq	-56(%rbp), %r12
	movq	%r12, 16(%rsi)
	movq	%rsi, %r15
	addq	$32, %rsi
	movq	$-1048576, %r14
	andq	%rsi, %r14
	movq	%rsi, 120(%r14)
	movq	$1, (%r14)
	movq	%rax, %rbx
	movq	%rcx, -64(%rbp)
	movq	%rdx, -72(%rbp)
	movq	%rdi, -80(%rbp)
	movq	%r8, -88(%rbp)
	movq	%r9, -96(%rbp)
	movq	%r10, -104(%rbp)
	movq	%r11, %r12
	movq	$-1048576, %rcx
	andq	%rsi, %rcx
	movq	%rcx, %rdi
	movq	%r15, %rsi
	call	PromoteObj
	movq	%rax, %r15
	movq	%rbx, %rax
	movq	-64(%rbp), %rcx
	movq	-72(%rbp), %rdx
	movq	-80(%rbp), %rdi
	movq	-88(%rbp), %r8
	movq	-96(%rbp), %r9
	movq	-104(%rbp), %r10
	movq	%r12, %r11
	movq	120(%r14), %rsi
	movq	$3, (%r14)
	movq	8(%r13), %rbx
	leaq	312(%rbx), %r14
	movq	-56(%rbp), %rax
	movq	8(%r13), %r12
	lock
	cmpxchgq	%r15, 312(%r12)
	movq	%rax, %r12
	cmpq	-56(%rbp), %r12
	jne	L_true18F
else.190:
	/* block else<B855> (ep<B854>) */
	movq	8(%r13), %r15
	movq	24(%r15), %r14
	cmpq	$1, %r14
	je	letJoinK.192
L196:
	cmpq	$3, %r14
	je	S_case193
S_case191:
	jmp	letJoinK.192
S_case193:
	movq	%r13, -56(%rbp)
case.194:
	/* block case<B85F> (ep<B85E>) */
	movq	$-1048576, %r12
	andq	%rsi, %r12
	movq	$1, (%r12)
	movq	%rax, %rbx
	movq	%rcx, %r13
	movq	%rdx, -80(%rbp)
	movq	%rsi, %r14
	movq	%rdi, %r15
	movq	%r8, -64(%rbp)
	movq	%r9, -96(%rbp)
	movq	%r10, -88(%rbp)
	movq	%r11, -72(%rbp)
	movq	-56(%rbp), %rcx
	movq	8(%rcx), %rcx
	movq	%rcx, %rdi
	call	VProcWake
	movq	%rbx, %rax
	movq	%r13, %rcx
	movq	-80(%rbp), %rdx
	movq	%r14, %rsi
	movq	%r15, %rdi
	movq	-64(%rbp), %r8
	movq	-96(%rbp), %r9
	movq	-88(%rbp), %r10
	movq	-72(%rbp), %r11
	movq	$3, (%r12)
	movq	-56(%rbp), %r13
letJoinK.192:
	/* Liveout:  GP={%rdi}  */
	/* block letJoinK<B859> (ep<B858>) */
	movq	24(%r13), %rdi
	jmp	letJoinK.2C
doGC18E:
	movq	$12, -8(%rsi)
	movq	%r13, (%rsi)
	movq	%rsi, %r10
	addq	$16, %rsi
	movq	%r10, %rdi
	movabsq	$retGC18C, %r8
	jmp	ASM_InvokeGC
	.text
spawnFn.19C:
	movq	%r9, %r15
	movq	%r8, %r14
	movq	%rdi, %r13
	movq	%r12, -56(%rbp)
	movq	%r10, %rbx
	jmp	gcTest19E
	/* live= GP={%r13} spilled= GP={%r~1 %r~1 %r~1 %r~1}  */
retGC19D:
	movq	32(%rdi), %r12
	movq	%r12, -56(%rbp)
	movq	24(%rdi), %rbx
	movq	16(%rdi), %r15
	movl	8(%rdi), %r14d
	movq	(%rdi), %r13
gcTest19E:
	movq	%r11, %r12
	subq	%rsi, %r12
	jle	doGC19F
	movq	%r14, -80(%rbp)
	movq	%rbx, -56(%rbp)
	movq	%r15, -64(%rbp)
check.197:
	/* block check<BA05> (ep<B7F8>,i<B7F9>,k<B7FA>,retK<B7FB>,exh<B7FC>) */
	movq	$-1048576, %r12
	andq	%rsi, %r12
	movq	$1, (%r12)
	movq	%rax, %r14
	movq	%rcx, %r15
	movq	%rdx, -88(%rbp)
	movq	%rsi, -96(%rbp)
	movq	%rdi, -104(%rbp)
	movq	%r8, -112(%rbp)
	movq	%r9, %rbx
	movq	%r10, -128(%rbp)
	movq	%r11, -120(%rbp)
	movslq	-80(%rbp), %r10
	movq	%r10, %rdi
	call	GetNthVProc
	movq	%rax, -72(%rbp)
	movq	%r14, %rax
	movq	%r15, %rcx
	movq	-88(%rbp), %rdx
	movq	-96(%rbp), %rsi
	movq	-104(%rbp), %rdi
	movq	-112(%rbp), %r8
	movq	%rbx, %r9
	movq	-128(%rbp), %r10
	movq	-120(%rbp), %r11
	movq	$3, (%r12)
	movq	$1289, -8(%rsi)
	movq	-80(%rbp), %r14
	movl	%r14d, (%rsi)
	movq	(%r13), %r14
	movq	8(%r14), %r15
	movq	%r15, 8(%rsi)
	movq	(%r13), %rcx
	movl	16(%rcx), %edx
	movl	%edx, 16(%rsi)
	movq	(%r13), %rbx
	movq	24(%rbx), %r10
	movq	%r10, 24(%rsi)
	movq	%rsi, %r12
	addq	$40, %rsi
	movq	$-1048576, %r14
	andq	%rsi, %r14
	movq	$3, 8(%r14)
	movq	$263, -8(%rsi)
	movabsq	$letJoinK.2C, %rcx
	movq	%rcx, (%rsi)
	movq	-56(%rbp), %r15
	movq	%r15, 8(%rsi)
	movq	%r14, 16(%rsi)
	movq	%rsi, %r15
	addq	$32, %rsi
	cmpq	-72(%rbp), %r14
	jne	L1A0
L_true199:
	movq	-64(%rbp), %r13
then.19B:
	/* Liveout:  GP={%rdi}  */
	/* block then<B834> (k<B833>,fls<B832>,vp<B831>,letJoinK<B830>) */
	movq	$28, -8(%rsi)
	movq	%r12, (%rsi)
	movq	%r13, 8(%rsi)
	movq	80(%r14), %r13
	movq	%r13, 16(%rsi)
	movq	%rsi, %r12
	addq	$32, %rsi
	movq	%r12, 80(%r14)
	movq	%r15, %rdi
	jmp	letJoinK.2C
doGC19F:
	movq	$3723, -8(%rsi)
	movq	%r13, (%rsi)
	movl	%r14d, 8(%rsi)
	movq	%r15, 16(%rsi)
	movq	%rbx, 24(%rsi)
	movq	-56(%rbp), %rbx
	movq	%rbx, 32(%rsi)
	movq	%rsi, %r14
	addq	$48, %rsi
	movq	%r14, %rdi
	movabsq	$retGC19D, %r8
	jmp	ASM_InvokeGC
L1A0:
else.19A:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block else<B83D> (k<B83C>,vp<B83B>,fls<B83A>,letJoinK<B839>) */
	movq	$1673, -8(%rsi)
	movq	-64(%rbp), %rcx
	movq	%rcx, (%rsi)
	movq	-72(%rbp), %rdx
	movq	%rdx, 8(%rsi)
	movq	%r12, 16(%rsi)
	movq	%r15, 24(%rsi)
	movq	%rsi, %rdx
	addq	$40, %rsi
	movq	$133, -8(%rsi)
	movq	%rdx, (%rsi)
	movabsq	$lp.18A, %r10
	movq	%r10, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	(%rbx), %rdi
	movq	$1, %r8
	jmp	lp.18A
	.text
letJoinK.1A2:
	movq	%rdi, %rcx
	jmp	gcTest1A4
	/* live= GP={%rcx} spilled=  */
retGC1A3:
	movq	(%rdi), %rcx
gcTest1A4:
	movq	%r11, %r12
	subq	%rsi, %r12
	jle	doGC1A5
check.1A1:
	/* Liveout:  GP={%rax %rdi}  */
	/* block check<BA07> (ep<B877>) */
	movq	$1, %rbx
	movq	8(%rcx), %rdx
	movq	%rbx, %rax
	movq	%rdx, %rdi
	jmp	initK.160
doGC1A5:
	movq	$12, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%rsi, %r10
	addq	$16, %rsi
	movq	%r10, %rdi
	movabsq	$retGC1A3, %r8
	jmp	ASM_InvokeGC
	.text
init.1A7:
	movq	%rax, %rcx
	movq	%rdi, %rdx
	jmp	gcTest1A9
	/* live= GP={%rcx %rdx} spilled=  */
retGC1A8:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest1A9:
	movq	%r11, %rbx
	subq	%rsi, %rbx
	jle	doGC1AA
check.1A6:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block check<BA0A> (ep<B86F>,x<B86C>) */
	movl	$1, %ebx
	movq	32(%rdx), %r10
	lock
	xaddl	%ebx, 8(%r10)
	movq	$261, -8(%rsi)
	movabsq	$letJoinK.1A2, %r13
	movq	%r13, (%rsi)
	movq	24(%rdx), %r14
	movq	%r14, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	16(%rdx), %r15
	movq	(%r15), %rdi
	movq	32(%rdx), %r8
	movq	%r12, %r9
	movq	8(%rdx), %r10
	jmp	wait.59
doGC1AA:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	%rcx, %rdi
	movabsq	$retGC1A8, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.1AD:
	movq	%rdi, %rcx
	jmp	gcTest1AF
	/* live= GP={%rcx} spilled=  */
retGC1AE:
	movq	(%rdi), %rcx
gcTest1AF:
	movq	%r11, %r12
	subq	%rsi, %r12
	jle	doGC1B0
check.1AB:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block check<BA0C> (ep<B896>) */
	movq	8(%rcx), %rdx
	movq	(%rdx), %rdi
	movl	16(%rcx), %ebx
	movq	%rbx, %r8
	incl	%r8d
	movq	24(%rcx), %r9
	movq	$1, %r10
	jmp	spawn.1AC
doGC1B0:
	movq	$12, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%rsi, %r10
	addq	$16, %rsi
	movq	%r10, %rdi
	movabsq	$retGC1AE, %r8
	jmp	ASM_InvokeGC
	.text
spawn.1AC:
	movq	%r9, %rdx
	movq	%r8, %rcx
	movq	%rdi, %rbx
	jmp	gcTest1B7
	/* live= GP={%r10 %rdx %rcx %rbx} spilled=  */
retGC1B6:
	movq	24(%rdi), %r10
	movq	16(%rdi), %rdx
	movl	8(%rdi), %ecx
	movq	(%rdi), %rbx
gcTest1B7:
	movq	%r11, %r12
	subq	%rsi, %r12
	jle	doGC1B8
check.1B1:
	/* block check<BA11> (ep<B884>,i<B885>,retK<B886>,exh<B887>) */
	cmpl	8(%rbx), %ecx
	jl	L1B9
L_true1B2:
then.1B4:
	/* Liveout:  GP={%rdi}  */
	/* block then<B88E> (retK<B88D>) */
	movq	%rdx, %rdi
	jmp	letJoinK.1B5
doGC1B8:
	movq	$1673, -8(%rsi)
	movq	%rbx, (%rsi)
	movl	%ecx, 8(%rsi)
	movq	%rdx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %r15
	addq	$40, %rsi
	movq	%r15, %rdi
	movabsq	$retGC1B6, %r8
	jmp	ASM_InvokeGC
L1B9:
else.1B3:
	/* Liveout:  GP={%r12 %r10 %r9 %r8 %rdi}  */
	/* block else<B893> (ep<B890>,i<B892>,retK<B891>) */
	movq	$133, -8(%rsi)
	movq	%rbx, (%rsi)
	movabsq	$spawn.1AC, %r12
	movq	%r12, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	$1289, -8(%rsi)
	movabsq	$letJoinK.1AD, %r13
	movq	%r13, (%rsi)
	movq	%r10, 8(%rsi)
	movl	%ecx, 16(%rsi)
	movq	%rdx, 24(%rsi)
	movq	%rsi, %r10
	addq	$40, %rsi
	movq	(%rbx), %r14
	movq	(%r14), %rdi
	movq	%rcx, %r8
	movq	16(%rbx), %r9
	movq	$1, %r12
	jmp	spawnFn.19C
	.text
letJoinK.1C4:
	movq	%rdi, %rcx
	jmp	gcTest1C6
retGC1C5:
	movq	(%rdi), %rcx
gcTest1C6:
	movq	%r11, %rdx
	subq	%rsi, %rdx
	jle	doGC1C7
check.1BA:
	/* block check<BA13> (ep<B8B2>) */
	movq	$-1048576, %r14
	andq	%rsi, %r14
	movq	32(%r14), %r15
	cmpq	$1, 8(%r15)
	je	L1C8
L_true1BB:
then.1BD:
	/* block then<B8DE> (ep<B8DD>) */
	movq	$-1048576, %rdx
	andq	%rsi, %rdx
	movq	32(%rdx), %rbx
	cmpq	$1, 8(%rbx)
	jne	letJoinK.1BE
L1C9:
else.1C3:
	/* Liveout:  GP={%rax %rdi}  */
	/* block else<B8E6> (ep<B8E5>) */
	movq	$133, -8(%rsi)
	movabsq	$str35, %r12
	movq	%r12, (%rsi)
	movl	$51, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movabsq	$tag36, %r13
	movq	%r13, (%rsi)
	movq	%r10, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	16(%rcx), %r14
	movq	(%r14), %rcx
	movq	%r15, %rax
	movq	%r14, %rdi
	jmp	*%rcx
L1C8:
else.1BC:
	/* block else<B8EF> (ep<B8EE>) */
	movq	$20, -8(%rsi)
	movq	$1, (%rsi)
	movq	$1, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	movq	$-1048576, %r12
	andq	%rsi, %r12
	movq	32(%r12), %rbx
	movq	$12, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rsi, %r10
	addq	$16, %rsi
	movq	$1289, -8(%rsi)
	movl	(%rbx), %r13d
	movl	%r13d, (%rsi)
	movq	%r10, 8(%rsi)
	movl	16(%rbx), %r14d
	movl	%r14d, 16(%rsi)
	movq	24(%rbx), %r15
	movq	%r15, 24(%rsi)
	movq	%rsi, %r12
	addq	$40, %rsi
	movq	$-1048576, %r13
	andq	%rsi, %r13
	movq	%r12, 32(%r13)
letJoinK.1BE:
	/* block letJoinK<B8BA> (ep<B8B9>) */
	movq	$-1048576, %r10
	andq	%rsi, %r10
	movq	32(%r10), %rdx
	movq	8(%rdx), %rbx
	cmpq	$1, %rbx
	jne	L_true1BF
else.1C0:
	/* Liveout:  GP={%rax %rdi}  */
	/* block else<B8D5> (ep<B8D4>) */
	movq	$133, -8(%rsi)
	movabsq	$str35, %rbx
	movq	%rbx, (%rsi)
	movl	$51, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movabsq	$tag36, %r10
	movq	%r10, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	16(%rcx), %r12
	movq	(%r12), %r14
	movq	%r13, %rax
	movq	%r12, %rdi
	jmp	*%r14
L_true1BF:
then.1C1:
	/* Liveout:  GP={%rax %rdi}  */
	/* block then<B8C1> (ep<B8BF>,_t<B8C0>) */
	movq	(%rbx), %r15
	movq	$20, -8(%rsi)
	movq	24(%rcx), %rbx
	movq	%rbx, (%rsi)
	movq	(%r15), %r10
	movq	%r10, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	8(%r15), %r13
	movq	%r13, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	$-1048576, %rdx
	andq	%rsi, %rdx
	movq	32(%rdx), %r14
	movq	$12, -8(%rsi)
	movq	%r12, (%rsi)
	movq	%rsi, %r15
	addq	$16, %rsi
	movq	$1289, -8(%rsi)
	movl	(%r14), %ebx
	movl	%ebx, (%rsi)
	movq	%r15, 8(%rsi)
	movl	16(%r14), %r10d
	movl	%r10d, 16(%rsi)
	movq	24(%r14), %r12
	movq	%r12, 24(%rsi)
	movq	%rsi, %rdx
	addq	$40, %rsi
	movq	$-1048576, %rbx
	andq	%rsi, %rbx
	movq	%rdx, 32(%rbx)
	movq	8(%rcx), %r10
	movq	(%r10), %r13
	movq	$1, %r14
	movq	%r14, %rax
	movq	%r10, %rdi
	jmp	*%r13
doGC1C7:
	movq	$12, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%rsi, %r15
	addq	$16, %rsi
	movq	%r15, %rdi
	movabsq	$retGC1C5, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.1B5:
	movq	%rdi, %rcx
	jmp	gcTest1CC
	/* live= GP={%rcx} spilled=  */
retGC1CB:
	movq	(%rdi), %rcx
gcTest1CC:
	movq	%r11, %rdx
	subq	%rsi, %rdx
	jle	doGC1CD
check.1CA:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block check<BA15> (ep<B8AC>) */
	movq	$1801, -8(%rsi)
	movabsq	$letJoinK.1C4, %rbx
	movq	%rbx, (%rsi)
	movq	8(%rcx), %r10
	movq	%r10, 8(%rsi)
	movq	16(%rcx), %r12
	movq	%r12, 16(%rsi)
	movq	32(%rcx), %r13
	movq	%r13, 24(%rsi)
	movq	%rsi, %rdx
	addq	$40, %rsi
	movq	24(%rcx), %r14
	movq	(%r14), %rdi
	movq	40(%rcx), %r8
	movq	%rdx, %r9
	movq	16(%rcx), %r10
	jmp	wait.59
doGC1CD:
	movq	$12, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%rsi, %r15
	addq	$16, %rsi
	movq	%r15, %rdi
	movabsq	$retGC1CB, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.1CF:
	movq	%rdi, %rcx
	jmp	gcTest1D1
	/* live= GP={%rcx} spilled=  */
retGC1D0:
	movq	(%rdi), %rcx
gcTest1D1:
	movq	%r11, %r12
	subq	%rsi, %r12
	jle	doGC1D2
check.1CE:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block check<BA17> (ep<B90C>) */
	movq	16(%rcx), %rdx
	movq	(%rdx), %rdi
	movl	24(%rcx), %ebx
	movq	%rbx, %r8
	incl	%r8d
	movq	32(%rcx), %r9
	movq	8(%rcx), %r10
	jmp	spawn.1AC
doGC1D2:
	movq	$12, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%rsi, %r10
	addq	$16, %rsi
	movq	%r10, %rdi
	movabsq	$retGC1D0, %r8
	jmp	ASM_InvokeGC
	.text
k.1D8:
	movq	%rax, %r13
	movq	%rdi, %r12
	jmp	gcTest1DA
	/* live= GP={%r13} spilled= GP={%r~1}  */
retGC1D9:
	movq	8(%rdi), %r13
	movq	(%rdi), %r12
gcTest1DA:
	movq	%r11, %r14
	subq	%rsi, %r14
	jle	doGC1DB
	movq	%r12, -88(%rbp)
check.1D3:
	/* block check<BA1A> (ep<B7EB>,x<B7E7>) */
	movq	$36, -8(%rsi)
	movq	-88(%rbp), %r15
	movq	56(%r15), %r14
	movq	%r14, (%rsi)
	movq	-88(%rbp), %rbx
	movq	32(%rbx), %r15
	movq	%r15, 8(%rsi)
	movq	-88(%rbp), %r12
	movq	40(%r12), %rbx
	movq	%rbx, 16(%rsi)
	movq	-88(%rbp), %r13
	movq	48(%r13), %r12
	movq	%r12, 24(%rsi)
	movq	%rsi, -56(%rbp)
	addq	$40, %rsi
	movq	$-1048576, %rbx
	andq	%rsi, %rbx
	movq	%rsi, 120(%rbx)
	movq	$1, (%rbx)
	movq	%rax, -80(%rbp)
	movq	%rcx, %r12
	movq	%rdx, %r13
	movq	%rdi, -64(%rbp)
	movq	%r8, -72(%rbp)
	movq	%r9, -96(%rbp)
	movq	%r10, %r14
	movq	%r11, %r15
	movq	$-1048576, %rcx
	andq	%rsi, %rcx
	movq	%rcx, %rdi
	movq	-56(%rbp), %rsi
	call	PromoteObj
	movq	%rax, -56(%rbp)
	movq	-80(%rbp), %rax
	movq	%r12, %rcx
	movq	%r13, %rdx
	movq	-64(%rbp), %rdi
	movq	-72(%rbp), %r8
	movq	-96(%rbp), %r9
	movq	%r14, %r10
	movq	%r15, %r11
	movq	120(%rbx), %rsi
	movq	$3, (%rbx)
	movq	$12, -8(%rsi)
	movq	$-1048576, %r14
	andq	%rsi, %r14
	movq	32(%r14), %r15
	movq	%r15, (%rsi)
	movq	%rsi, %r13
	addq	$16, %rsi
	movq	$133, -8(%rsi)
	movq	%r13, (%rsi)
	movabsq	$spawnFn.19C, %rbx
	movq	%rbx, 8(%rsi)
	movq	%rsi, -64(%rbp)
	addq	$24, %rsi
	movq	$-1048576, %rbx
	andq	%rsi, %rbx
	movq	$1, (%rbx)
	movq	%rax, -112(%rbp)
	movq	%rcx, -120(%rbp)
	movq	%rdx, -80(%rbp)
	movq	%rsi, %r12
	movq	%rdi, %r13
	movq	%r8, -104(%rbp)
	movq	%r9, -96(%rbp)
	movq	%r10, %r14
	movq	%r11, %r15
	call	GetNumVProcs
	movq	%rax, -72(%rbp)
	movq	-112(%rbp), %rax
	movq	-120(%rbp), %rcx
	movq	-80(%rbp), %rdx
	movq	%r12, %rsi
	movq	%r13, %rdi
	movq	-104(%rbp), %r8
	movq	-96(%rbp), %r9
	movq	%r14, %r10
	movq	%r15, %r11
	movq	$3, (%rbx)
	movq	$18, -8(%rsi)
	movq	-72(%rbp), %r14
	movl	%r14d, (%rsi)
	movl	$0, 8(%rsi)
	movq	%rsi, -80(%rbp)
	addq	$24, %rsi
	movq	$-1048576, %r12
	andq	%rsi, %r12
	movq	%rsi, 120(%r12)
	movq	$1, (%r12)
	movq	%rax, %r13
	movq	%rcx, %r14
	movq	%rdx, %r15
	movq	%rdi, -96(%rbp)
	movq	%r8, -104(%rbp)
	movq	%r9, -112(%rbp)
	movq	%r10, -120(%rbp)
	movq	%r11, %rbx
	movq	$-1048576, %r10
	andq	%rsi, %r10
	movq	%r10, %rdi
	movq	-80(%rbp), %rsi
	call	PromoteObj
	movq	%rax, -80(%rbp)
	movq	%r13, %rax
	movq	%r14, %rcx
	movq	%r15, %rdx
	movq	-96(%rbp), %rdi
	movq	-104(%rbp), %r8
	movq	-112(%rbp), %r9
	movq	-120(%rbp), %r10
	movq	%rbx, %r11
	movq	120(%r12), %rsi
	movq	$3, (%r12)
	movq	$44, -8(%rsi)
	movabsq	$init.1A7, %rcx
	movq	%rcx, (%rsi)
	movq	-88(%rbp), %r15
	movq	16(%r15), %rdx
	movq	%rdx, 8(%rsi)
	movq	-88(%rbp), %rcx
	movq	24(%rcx), %rbx
	movq	%rbx, 16(%rsi)
	movq	-56(%rbp), %rdx
	movq	(%rdx), %r10
	movq	%r10, 24(%rsi)
	movq	-80(%rbp), %rbx
	movq	%rbx, 32(%rsi)
	movq	%rsi, %rcx
	addq	$48, %rsi
	movq	$647, -8(%rsi)
	movq	-64(%rbp), %r10
	movq	%r10, (%rsi)
	movq	-72(%rbp), %r12
	movl	%r12d, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%rsi, %r12
	addq	$32, %rsi
	movq	$133, -8(%rsi)
	movq	%r12, (%rsi)
	movabsq	$spawn.1AC, %r13
	movq	%r13, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	xorl	%r15d, %r15d
	movq	$7949, -8(%rsi)
	movabsq	$letJoinK.1B5, %r14
	movq	%r14, (%rsi)
	movq	-88(%rbp), %r13
	movq	8(%r13), %rbx
	movq	%rbx, 8(%rsi)
	movq	-88(%rbp), %r14
	movq	16(%r14), %r10
	movq	%r10, 16(%rsi)
	movq	-88(%rbp), %rbx
	movq	24(%rbx), %r12
	movq	%r12, 24(%rsi)
	movq	-56(%rbp), %r10
	movq	%r10, 32(%rsi)
	movq	-80(%rbp), %r12
	movq	%r12, 40(%rsi)
	movq	%rsi, %r14
	addq	$56, %rsi
	cmpl	-72(%rbp), %r15d
	jl	L1DC
L_true1D5:
then.1D7:
	/* Liveout:  GP={%rdi}  */
	/* block then<B902> (letJoinK<B901>) */
	movq	%r14, %rdi
	jmp	letJoinK.1B5
doGC1DB:
	movq	$20, -8(%rsi)
	movq	%r12, (%rsi)
	movq	%r13, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	%r12, %rdi
	movabsq	$retGC1D9, %r8
	jmp	ASM_InvokeGC
L1DC:
	movq	-88(%rbp), %r13
else.1D6:
	/* Liveout:  GP={%r12 %r10 %r9 %r8 %rdi}  */
	/* block else<B90A> (ep<B904>,spawnFn<B909>,init<B908>,spawn<B907>,_t<B906>,letJoinK<B905>) */
	movq	$2827, -8(%rsi)
	movabsq	$letJoinK.1CF, %rbx
	movq	%rbx, (%rsi)
	movq	16(%r13), %r10
	movq	%r10, 8(%rsi)
	movq	%rdx, 16(%rsi)
	movl	%r15d, 24(%rsi)
	movq	%r14, 32(%rsi)
	movq	%rsi, %r10
	addq	$48, %rsi
	movq	-64(%rbp), %r14
	movq	(%r14), %rdi
	movq	%r15, %r8
	movq	%rcx, %r9
	movq	16(%r13), %r12
	jmp	spawnFn.19C
	.text
letJoinK.1DE:
	movq	%rdi, %rcx
	jmp	gcTest1E0
	/* live= GP={%rcx} spilled=  */
retGC1DF:
	movq	(%rdi), %rcx
gcTest1E0:
	movq	%r11, %rdx
	subq	%rsi, %rdx
	jle	doGC1E1
check.1DD:
	/* Liveout:  GP={%rax %rdi}  */
	/* block check<BA1C> (ep<B921>) */
	movq	$-1048576, %rdx
	andq	%rsi, %rdx
	movq	$3, 8(%rdx)
	movq	40(%rdx), %r10
	movq	8(%r10), %r12
	movq	%r12, 40(%rdx)
	movq	(%r10), %rbx
	movq	(%rbx), %r13
	movq	$1, %r14
	movq	%r14, %rax
	movq	%rbx, %rdi
	jmp	*%r13
doGC1E1:
	movq	$12, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%rsi, %r15
	addq	$16, %rsi
	movq	%r15, %rdi
	movabsq	$retGC1DF, %r8
	jmp	ASM_InvokeGC
	.text
k.1E3:
	movq	%rax, %rcx
	movq	%rdi, %rdx
	jmp	gcTest1E5
	/* live= GP={%rcx %rdx} spilled=  */
retGC1E4:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest1E5:
	movq	%r11, %r10
	subq	%rsi, %r10
	jle	doGC1E6
check.1E2:
	/* Liveout:  GP={%rdi}  */
	/* block check<BA1F> (ep<B937>,x<B936>) */
	movq	8(%rdx), %rdi
	jmp	letJoinK.1DE
doGC1E6:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	%rbx, %rdi
	movabsq	$retGC1E4, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.1EC:
	movq	%r8, %rbx
	movq	%rdi, %r13
gcTest1EE:
	movq	%r11, %r14
	subq	%rsi, %r14
	jle	doGC1EF
check.1E7:
	/* block check<BA22> (ep<B576>,act<B56F>) */
	movq	$20, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	48(%r13), %r14
	movq	40(%r14), %r15
	movq	%r15, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	48(%r13), %rbx
	movq	%r12, 40(%rbx)
	movq	48(%r13), %r12
	movq	$1, 8(%r12)
	movq	$775, -8(%rsi)
	movl	$0, (%rsi)
	movq	$1, 8(%rsi)
	movq	$1, 16(%rsi)
	movq	%rsi, %r15
	addq	$32, %rsi
	movq	$-1048576, %r14
	andq	%rsi, %r14
	movq	%rsi, 120(%r14)
	movq	$1, (%r14)
	movq	%rax, -80(%rbp)
	movq	%rcx, -96(%rbp)
	movq	%rdx, %rbx
	movq	%rdi, -56(%rbp)
	movq	%r8, -64(%rbp)
	movq	%r9, -88(%rbp)
	movq	%r10, -72(%rbp)
	movq	%r11, %r12
	movq	$-1048576, %rcx
	andq	%rsi, %rcx
	movq	%rcx, %rdi
	movq	%r15, %rsi
	call	PromoteObj
	movq	%rax, %r15
	movq	-80(%rbp), %rax
	movq	-96(%rbp), %rcx
	movq	%rbx, %rdx
	movq	-56(%rbp), %rdi
	movq	-64(%rbp), %r8
	movq	-88(%rbp), %r9
	movq	-72(%rbp), %r10
	movq	%r12, %r11
	movq	120(%r14), %rsi
	movq	$3, (%r14)
	movq	$44, -8(%rsi)
	movabsq	$schedulerLoop.15A, %rdx
	movq	%rdx, (%rsi)
	movq	16(%r13), %rbx
	movq	%rbx, 8(%rsi)
	movq	24(%r13), %r10
	movq	%r10, 16(%rsi)
	movq	32(%r13), %r12
	movq	%r12, 24(%rsi)
	movq	%r15, 32(%rsi)
	movq	%rsi, %rcx
	addq	$48, %rsi
	movq	$20, -8(%rsi)
	movabsq	$initK.160, %r14
	movq	%r14, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	$12, -8(%rsi)
	movq	%r15, (%rsi)
	movq	%rsi, %r15
	addq	$16, %rsi
	movq	$20, -8(%rsi)
	movq	%r15, (%rsi)
	movabsq	$spawnFn.178, %rcx
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	$18, -8(%rsi)
	movq	$1, (%rsi)
	movabsq	$removeFn.17D, %rbx
	movq	%rbx, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	movq	$68, -8(%rsi)
	movabsq	$k.1D8, %r12
	movq	%r12, (%rsi)
	movq	8(%r13), %r15
	movq	%r15, 8(%rsi)
	movq	16(%r13), %rcx
	movq	%rcx, 16(%rsi)
	movq	40(%r13), %r12
	movq	%r12, 24(%rsi)
	movq	%r10, 32(%rsi)
	movq	%rdx, 40(%rsi)
	movq	$1, 48(%rsi)
	movq	%r14, 56(%rsi)
	movq	%rsi, %r10
	addq	$72, %rsi
	movq	$-1048576, %r12
	andq	%rsi, %r12
	movq	$3, 8(%r12)
	movq	$28, -8(%rsi)
	movq	$-1048576, %r14
	andq	%rsi, %r14
	movq	32(%r14), %r15
	movq	%r15, (%rsi)
	movq	%r10, 8(%rsi)
	movq	80(%r12), %rcx
	movq	%rcx, 16(%rsi)
	movq	%rsi, %r13
	addq	$32, %rsi
	movq	%r13, 80(%r12)
	movq	$10, -8(%rsi)
	movabsq	$letJoinK.1DE, %rbx
	movq	%rbx, (%rsi)
	movq	%rsi, %rdx
	addq	$16, %rsi
	movq	16(%r12), %r13
	cmpq	$1, %r13
	jne	L1F0
S_case1E8:
case.1E9:
	/* Liveout:  GP={%rdi}  */
	/* block case<B92E> (vp<B92D>,letJoinK<B92C>) */
	movq	$1, 8(%r12)
	movq	%rdx, %rdi
	jmp	letJoinK.1DE
doGC1EF:
	movq	$20, -8(%rsi)
	movq	%r13, (%rsi)
	movq	%rbx, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	%r13, %rdi
	movabsq	$retGC1ED, %r8
	jmp	ASM_InvokeGC
	/* live= GP={%rbx %r13} spilled=  */
retGC1ED:
	movq	8(%rdi), %rbx
	movq	(%rdi), %r13
	jmp	gcTest1EE
L1F0:
	cmpq	$3, %r13
	jne	S_case1E8
S_case1EA:
case.1EB:
	/* Liveout:  GP={%rax %rdi}  */
	/* block case<B934> (vp<B933>,letJoinK<B932>) */
	movq	$1, 16(%r12)
	movq	$20, -8(%rsi)
	movabsq	$k.1E3, %r15
	movq	%r15, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movq	$1, (%rsi)
	movq	%r14, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	40(%r12), %rdx
	movq	8(%rdx), %rbx
	movq	%rbx, 40(%r12)
	movq	(%rdx), %r10
	movq	(%r10), %r12
	movq	%rcx, %rax
	movq	%r10, %rdi
	jmp	*%r12
	.text
letJoinK.E6:
	movq	%rdi, %rcx
	jmp	gcTest1F3
	/* live= GP={%rcx} spilled=  */
retGC1F2:
	movq	(%rdi), %rcx
gcTest1F3:
	movq	%r11, %r12
	subq	%rsi, %r12
	jle	doGC1F4
check.1F1:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block check<BA24> (ep<B56C>) */
	movq	$7951, -8(%rsi)
	movabsq	$letJoinK.1EC, %rbx
	movq	%rbx, (%rsi)
	movq	8(%rcx), %r10
	movq	%r10, 8(%rsi)
	movq	16(%rcx), %r12
	movq	%r12, 16(%rsi)
	movq	24(%rcx), %r13
	movq	%r13, 24(%rsi)
	movq	32(%rcx), %r14
	movq	%r14, 32(%rsi)
	movq	40(%rcx), %r15
	movq	%r15, 40(%rsi)
	movq	56(%rcx), %rdx
	movq	%rdx, 48(%rsi)
	movq	%rsi, %rdx
	addq	$64, %rsi
	movq	48(%rcx), %rbx
	movq	(%rbx), %rdi
	movq	56(%rcx), %r8
	movq	%rdx, %r9
	movq	16(%rcx), %r10
	jmp	mkSwitch.C4
doGC1F4:
	movq	$12, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%rsi, %r10
	addq	$16, %rsi
	movq	%r10, %rdi
	movabsq	$retGC1F2, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.1F6:
	movq	%rdi, %rcx
	jmp	gcTest1F8
	/* live= GP={%rcx} spilled=  */
retGC1F7:
	movq	(%rdi), %rcx
gcTest1F8:
	movq	%r11, %r10
	subq	%rsi, %r10
	jle	doGC1F9
check.1F5:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block check<BA26> (ep<B952>) */
	movq	8(%rcx), %rdx
	movq	(%rdx), %rdi
	movq	24(%rcx), %r8
	movq	16(%rcx), %r9
	jmp	lp.DD
doGC1F9:
	movq	$12, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%rsi, %rbx
	addq	$16, %rsi
	movq	%rbx, %rdi
	movabsq	$retGC1F7, %r8
	jmp	ASM_InvokeGC
	.text
main.200:
Main_init:
mantEntry:
	movq	%r9, %rbx
	movq	%r8, %r13
	movq	%rax, %r12
	movq	%rdi, %r15
gcTest204:
	movq	%r11, %r14
	subq	%rsi, %r14
	jg	L206
doGC205:
	movq	$36, -8(%rsi)
	movq	%r15, (%rsi)
	movq	%r12, 8(%rsi)
	movq	%r13, 16(%rsi)
	movq	%rbx, 24(%rsi)
	movq	%rsi, %r14
	addq	$40, %rsi
	movq	%r14, %rdi
	movabsq	$retGC203, %r8
	jmp	ASM_InvokeGC
L206:
	movq	%r13, -88(%rbp)
check.1FE:
	/* block check<BA2B> (dummyEP<B308>,argFormalWrap<B960>,retK<B30A>,_exh<B30B>) */
	movq	$18, -8(%rsi)
	movq	$1, (%rsi)
	movabsq	$get_D_ite.37, %r14
	movq	%r14, 8(%rsi)
	movq	%rsi, -64(%rbp)
	addq	$24, %rsi
	movq	$18, -8(%rsi)
	movq	$1, (%rsi)
	movabsq	$set_D_ite.3D, %r15
	movq	%r15, 8(%rsi)
	movq	%rsi, -72(%rbp)
	addq	$24, %rsi
	movq	$18, -8(%rsi)
	movq	$1, (%rsi)
	movabsq	$wait.59, %r12
	movq	%r12, 8(%rsi)
	movq	%rsi, -80(%rbp)
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movabsq	$switch.BE, %r14
	movq	%r14, (%rsi)
	movq	%rbx, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	$12, -8(%rsi)
	movq	%r13, (%rsi)
	movq	%rsi, %r15
	addq	$16, %rsi
	movq	$133, -8(%rsi)
	movq	%r15, (%rsi)
	movabsq	$mkSwitch.C4, %r12
	movq	%r12, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	$-1048576, %r12
	andq	%rsi, %r12
	movq	$3, 8(%r12)
	movq	$12, -8(%rsi)
	movq	%r14, (%rsi)
	movq	%rsi, %r13
	addq	$16, %rsi
	movq	$133, -8(%rsi)
	movq	%r13, (%rsi)
	movabsq	$initVPFields.D8, %r15
	movq	%r15, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%r15, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	$133, -8(%rsi)
	movq	%r13, (%rsi)
	movabsq	$lp.DD, %r13
	movq	%r13, 8(%rsi)
	movq	%rsi, -56(%rbp)
	addq	$24, %rsi
	movq	%r15, -96(%rbp)
	movq	%r12, -104(%rbp)
	movq	%r14, -112(%rbp)
	movq	$57235, -8(%rsi)
	movq	-88(%rbp), %r14
	movq	%r14, (%rsi)
	movq	%rbx, 8(%rsi)
	movq	-64(%rbp), %r15
	movq	%r15, 16(%rsi)
	movq	-72(%rbp), %rbx
	movq	%rbx, 24(%rsi)
	movq	-80(%rbp), %r12
	movq	%r12, 32(%rsi)
	movq	-112(%rbp), %r13
	movq	%r13, 40(%rsi)
	movq	-104(%rbp), %r14
	movq	%r14, 48(%rsi)
	movq	-96(%rbp), %r15
	movq	%r15, 56(%rsi)
	movq	-56(%rbp), %rbx
	movq	%rbx, 64(%rsi)
	movq	%rsi, %r14
	addq	$80, %rsi
	movq	$-1048576, %r15
	andq	%rsi, %r15
	movq	%r14, 96(%r15)
	movq	%rsi, 120(%r15)
	movq	%r11, 320(%r15)
	movq	$-1048576, %r13
	andq	%rsi, %r13
	movq	$1, (%r13)
	movq	%rax, -64(%rbp)
	movq	%rsi, -72(%rbp)
	movq	%rdi, -80(%rbp)
	movq	%r8, %r15
	movq	%r9, %rbx
	movq	%r11, %r12
	movq	$-1048576, %r14
	andq	%rsi, %r14
	movq	%r14, %rdi
	call	ListVProcs
	movq	%rax, -56(%rbp)
	movq	-64(%rbp), %rax
	movq	-72(%rbp), %rsi
	movq	-80(%rbp), %rdi
	movq	%r15, %r8
	movq	%rbx, %r9
	movq	%r12, %r11
	movq	$3, (%r13)
	movq	$-1048576, %r13
	andq	%rsi, %r13
	movq	120(%r13), %rsi
	movq	320(%r13), %r11
	movq	96(%r13), %rcx
	movq	64(%rcx), %r10
	movq	%r10, -64(%rbp)
	movq	96(%r13), %rdx
	movq	56(%rdx), %r12
	movq	%r12, -72(%rbp)
	movq	96(%r13), %rbx
	movq	48(%rbx), %r14
	movq	%r14, -80(%rbp)
	movq	96(%r13), %r10
	movq	40(%r10), %rcx
	movq	96(%r13), %r12
	movq	32(%r12), %r12
	movq	96(%r13), %r14
	movq	24(%r14), %rbx
	movq	96(%r13), %r15
	movq	16(%r15), %rdx
	movq	96(%r13), %r10
	movq	8(%r10), %r10
	movq	96(%r13), %r13
	movq	(%r13), %r15
	movq	-56(%rbp), %r14
allocCCall.1FA:
	/* block allocCCall<BA35> (vps<B569>,retK<BA2C>,_exh<BA2D>,get-ite<BA2E>,set-ite<BA2F>,wait<BA30>,mkSwitch<BA31>,vp<BA32>,initVPFields<BA33>,lp<BA34>) */
	movq	$16145, -8(%rsi)
	movabsq	$letJoinK.E6, %r13
	movq	%r13, (%rsi)
	movq	%r15, 8(%rsi)
	movq	%r10, 16(%rsi)
	movq	%rdx, 24(%rsi)
	movq	%rbx, 32(%rsi)
	movq	%r12, 40(%rsi)
	movq	%rcx, 48(%rsi)
	movq	-80(%rbp), %r13
	movq	%r13, 56(%rsi)
	movq	%rsi, %rcx
	addq	$72, %rsi
	cmpq	$1, %r14
	je	L207
L_true1FB:
then.1FD:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block then<B94D> (_exh<B94C>,initVPFields<B94B>,lp<B94A>,vps<B949>,letJoinK<B948>) */
	movq	$1801, -8(%rsi)
	movabsq	$letJoinK.1F6, %rdx
	movq	%rdx, (%rsi)
	movq	-64(%rbp), %r15
	movq	%r15, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	8(%r14), %rbx
	movq	%rbx, 24(%rsi)
	movq	%rsi, %r15
	addq	$40, %rsi
	movq	-72(%rbp), %rcx
	movq	(%rcx), %rdi
	movq	(%r14), %r12
	movq	(%r12), %r8
	movq	%r15, %r9
	jmp	initVPFields.D8
L207:
else.1FC:
	/* Liveout:  GP={%rdi}  */
	/* block else<B95E> (letJoinK<B95D>) */
	movq	%rcx, %rdi
	jmp	letJoinK.E6
	/* live= GP={%rbx %r12 %r15} spilled= GP={%r~1}  */
retGC203:
	movq	24(%rdi), %rbx
	movq	16(%rdi), %r13
	movq	8(%rdi), %r12
	movq	(%rdi), %r15
	jmp	gcTest204
	.global	mantEntry
	.global	Main_init
	.text
letJoinK.7:
	movq	%r8, %r12
	movq	%rdi, %rbx
	jmp	gcTest20F
	/* live= GP={%r12 %rbx} spilled=  */
retGC20E:
	movq	8(%rdi), %r12
	movq	(%rdi), %rbx
gcTest20F:
	movq	%r11, %r13
	subq	%rsi, %r13
	jle	doGC210
check.20B:
	/* block check<B97C> (ep<B39D>,item<B39A>) */
	cmpq	$1, %r12
	jne	L_true20C
else.209:
	/* block else<B3B4> (ep<B3B3>) */
	movq	$20, -8(%rsi)
	movabsq	$wakeupK.5F, %r12
	movq	%r12, (%rsi)
	movq	24(%rbx), %r13
	movq	%r13, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	16(%rbx), %r14
	movq	%r15, 64(%r14)
	movq	$-1048576, %r12
	andq	%rsi, %r12
	movq	$1, 96(%r12)
	movq	%rsi, 120(%r12)
	movq	%r11, 320(%r12)
	movq	$-1048576, %r14
	andq	%rsi, %r14
	movq	$1, (%r14)
	movq	%rax, %r12
	movq	%rsi, %r13
	movq	%rdi, %r15
	movq	%r8, -56(%rbp)
	movq	%r9, -64(%rbp)
	movq	%r11, -72(%rbp)
	movq	16(%rbx), %rbx
	movq	%rbx, %rdi
	call	SleepCont
	movq	%rax, %rbx
	movq	%r12, %rax
	movq	%r13, %rsi
	movq	%r15, %rdi
	movq	-56(%rbp), %r8
	movq	-64(%rbp), %r9
	movq	-72(%rbp), %r11
	movq	$3, (%r14)
	movq	$-1048576, %r13
	andq	%rsi, %r13
	movq	120(%r13), %rsi
	movq	320(%r13), %r11
allocCCall.208:
	/* Liveout:  GP={%rax %rdi}  */
	/* block allocCCall<BA37> (sleepK<B3BD>) */
	movq	(%rbx), %r13
	movq	$1, %r14
	movq	%r14, %rax
	movq	%rbx, %rdi
	jmp	*%r13
doGC210:
	movq	$20, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%r12, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	%r10, %rdi
	movabsq	$retGC20E, %r8
	jmp	ASM_InvokeGC
L_true20C:
then.20D:
	/* Liveout:  GP={%rax %rdi}  */
	/* block then<B3A3> (ep<B3A1>,item<B3A2>) */
	movq	(%r12), %r14
	movq	8(%r14), %r13
	movq	(%r14), %r15
	movq	16(%rbx), %rcx
	movq	%r15, 32(%rcx)
	movq	$20, -8(%rsi)
	movq	8(%rbx), %r10
	movq	%r10, (%rsi)
	movq	16(%rbx), %r12
	movq	40(%r12), %r14
	movq	%r14, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	movq	16(%rbx), %r15
	movq	%rdx, 40(%r15)
	movq	16(%rbx), %rcx
	movq	$1, 8(%rcx)
	movq	(%r13), %rdx
	movq	$1, %rbx
	movq	%rbx, %rax
	movq	%r13, %rdi
	jmp	*%rdx
	.text
	.section	.rodata
	.global	mantMagic
mantMagic:
	.long	3437656105
	.global	SequentialFlag
SequentialFlag:
	.long	0
	.align	8
str35:
	.asciz	"FLS.ite: nonexistant implicit threading environment"
	.align	8
tag36:
	.asciz	"Fail"
	.align	8
tagBD:
	.asciz	"Match"
