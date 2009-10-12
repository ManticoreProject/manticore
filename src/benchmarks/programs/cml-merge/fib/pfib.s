	.text
letJoinK.6:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	movq	%r8, %rdx
	movq	%rdi, %rcx
	movq	$775, -8(%rsi)
	movabsq	$letJoinK.7, %rbx
	movq	%rbx, (%rsi)
	movq	8(%rcx), %r10
	movq	%r10, 8(%rsi)
	movq	%rdx, 16(%rsi)
	movq	%rsi, %r10
	addq	$32, %rsi
	movq	$18, -8(%rsi)
	movq	$1, (%rsi)
	movabsq	$lp.8, %r13
	movq	%r13, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	(%r12), %rdi
	movq	16(%rcx), %r8
	movq	$1, %r9
	jmp	lp.8
	.text
letJoinK.7:
	/* Liveout:  GP={%r8 %rdi}  */
	movq	%r8, %rdx
	movq	%rdi, %rcx
	movq	$20, -8(%rsi)
	movq	16(%rcx), %r10
	movq	%r10, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	8(%rcx), %rdi
	movq	%rbx, %r8
	jmp	letJoinK.9
	.text
letJoinK.9:
	/* Liveout:  GP={%r12 %r10 %r9 %r8 %rdi}  */
	movq	%r8, %r14
	movq	%rdi, %r13
	movq	$10, -8(%rsi)
	movq	16(%r13), %rdx
	movq	%rdx, (%rsi)
	movq	%rsi, %rcx
	addq	$16, %rsi
	movq	$133, -8(%rsi)
	movq	%rcx, (%rsi)
	movabsq	$f1.A, %r10
	movq	%r10, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	$1801, -8(%rsi)
	movabsq	$letJoinK.B, %r12
	movq	%r12, (%rsi)
	movq	32(%r13), %r15
	movq	%r15, 8(%rsi)
	movq	40(%r13), %rcx
	movq	%rcx, 16(%rsi)
	movq	%r14, 24(%rsi)
	movq	%rsi, %r10
	addq	$40, %rsi
	movq	8(%r13), %rdx
	movq	(%rdx), %rdi
	movq	%rbx, %r8
	movq	(%r14), %r9
	movq	24(%r13), %r12
	jmp	app_D_w_uncurried.C
	.text
letJoinK.D:
	/* Liveout:  GP={%r8 %rdi}  */
	movq	%r8, %rdx
	movq	%rdi, %rcx
	movq	16(%rdx), %rbx
	movq	8(%rcx), %r10
	movq	%rbx, 80(%r10)
	movq	$12, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rsi, %r12
	addq	$16, %rsi
	movq	16(%rcx), %rdi
	movq	%r12, %r8
	jmp	letJoinK.E
	.text
letJoinK.F:
	/* Liveout:  GP={%r8 %rdi}  */
	movq	%rdi, %rcx
	movq	$261, -8(%rsi)
	movabsq	$letJoinK.10, %rbx
	movq	%rbx, (%rsi)
	movq	16(%rcx), %r10
	movq	%r10, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	movq	$10, -8(%rsi)
	movq	8(%rcx), %r13
	movq	%r13, (%rsi)
	movq	%rsi, %r12
	addq	$16, %rsi
	movq	$133, -8(%rsi)
	movq	%r12, (%rsi)
	movabsq	$preempt.11, %r15
	movq	%r15, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	(%r14), %rdi
	movq	%rdx, %r8
	jmp	preempt.11
	.text
letJoinK.10:
	/* Liveout:  GP={%rdi}  */
	movq	%rdi, %rcx
	movq	8(%rcx), %rdi
	jmp	letJoinK.12
	.text
letJoinK.13:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	movq	%rdi, %rcx
	movq	24(%rcx), %r10
	leaq	12(%r10), %rbx
	movq	24(%rcx), %r12
	leaq	12(%r12), %rdx
	movq	24(%rcx), %r13
	decl	12(%r13)
	movq	8(%rcx), %rdi
	movq	16(%rcx), %r8
	movq	40(%rcx), %r9
	movq	32(%rcx), %r10
	jmp	schedulerLoop.14
	.text
letJoinK.15:
	/* Liveout:  GP={%r8 %rdi}  */
	movq	%rdi, %r15
	movq	$10, -8(%rsi)
	movq	16(%r15), %rdx
	movq	%rdx, (%rsi)
	movq	%rsi, %rcx
	addq	$16, %rsi
	movq	$133, -8(%rsi)
	movq	%rcx, (%rsi)
	movabsq	$preempt.16, %rdx
	movq	%rdx, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	$777, -8(%rsi)
	movabsq	$letJoinK.17, %r10
	movq	%r10, (%rsi)
	movq	8(%r15), %r12
	movq	%r12, 8(%rsi)
	movq	24(%r15), %r13
	movq	%r13, 16(%rsi)
	movq	32(%r15), %r14
	movq	%r14, 24(%rsi)
	movq	%rsi, %rbx
	addq	$40, %rsi
	movq	(%rcx), %rdi
	movq	%rbx, %r8
	jmp	preempt.16
	.text
letJoinK.17:
	/* Liveout:  GP={%r8 %rdi}  */
	movq	%rdi, %rcx
	movq	$133, -8(%rsi)
	movq	16(%rcx), %rbx
	movq	%rbx, (%rsi)
	movq	24(%rcx), %r10
	movq	%r10, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	movq	$133, -8(%rsi)
	movq	%rdx, (%rsi)
	movabsq	$wait.18, %r13
	movq	%r13, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	(%r12), %rdi
	movq	8(%rcx), %r8
	jmp	wait.18
	.text
letJoinK.19:
	/* Liveout:  GP={%r8 %rdi}  */
	movq	%rdi, %rcx
	movq	$261, -8(%rsi)
	movabsq	$letJoinK.1A, %rbx
	movq	%rbx, (%rsi)
	movq	16(%rcx), %r10
	movq	%r10, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	movq	$10, -8(%rsi)
	movq	8(%rcx), %r13
	movq	%r13, (%rsi)
	movq	%rsi, %r12
	addq	$16, %rsi
	movq	$133, -8(%rsi)
	movq	%r12, (%rsi)
	movabsq	$preempt.1B, %r15
	movq	%r15, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	(%r14), %rdi
	movq	%rdx, %r8
	jmp	preempt.1B
	.text
letJoinK.1A:
	/* Liveout:  GP={%rdi}  */
	movq	%rdi, %rcx
	movq	8(%rcx), %rdi
	jmp	letJoinK.1C
	.text
letJoinK.26:
	movq	%r8, %rdx
	movq	%rdi, %rbx
	movq	8(%rbx), %r15
	movq	$1, %r10
	movq	(%rdx), %rcx
	shlq	%cl, %r10
	movq	(%r15), %r12
	subq	%r10, %r12
	xorq	%r10, %r10
	cmpq	%r10, %r12
	je	L_true27
else.1F:
	/* block else<CABB> (ep<CAB7>,lg<CABA>,_t<CAB9>,_lit<CAB8>) */
	cmpq	%r10, %r12
	jb	L_true20
else.1D:
	/* block else<CAC2> (ep<CAC0>,lg<CAC1>) */
	movq	$3, %r13
	jmp	letJoinK.1E
L_true27:
then.25:
	/* block then<CAB5> (ep<CAB3>,lg<CAB4>) */
	movq	$5, %r13
	jmp	letJoinK.1E
L_true20:
then.21:
	/* block then<CABE> (ep<CABC>,lg<CABD>) */
	movq	$1, %r13
letJoinK.1E:
	/* block letJoinK<CAA1> (ep<CA9E>,lg<CAA0>,_t<CA9F>) */
	cmpq	$3, %r13
	jne	L29
S_case22:
case.23:
	/* block case<CAAD> (ep<CAAB>,lg<CAAC>) */
	movq	$1, %r14
letJoinK.28:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block letJoinK<CAA5> (ep<CAA2>,lg<CAA4>,_t<CAA3>) */
	movq	$10, -8(%rsi)
	movq	(%rdx), %r15
	leaq	(%r15,%r14,1), %r13
	movq	%r13, (%rsi)
	movq	%rsi, %r12
	addq	$16, %rsi
	movq	16(%rbx), %rcx
	movq	(%rcx), %rdx
	movq	%rcx, %rdi
	movq	%r12, %r8
	jmp	*%rdx
L29:
default.24:
	/* block default<CAB1> (ep<CAAF>,lg<CAB0>) */
	xorq	%r14, %r14
	jmp	letJoinK.28
	.text
letJoinK.2C:
	movq	%rdi, %rcx
	cmpl	$2000, 16(%rcx)
	jg	L_true2D
else.2A:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block else<CE99> (ep<CE98>) */
	movq	8(%rcx), %rdx
	movq	(%rdx), %rdi
	movl	16(%rcx), %ebx
	movq	%rbx, %r8
	incl	%r8d
	movq	24(%rcx), %r9
	jmp	lp1.2B
L_true2D:
then.2E:
	/* Liveout:  GP={%rdi}  */
	/* block then<CE95> (ep<CE94>) */
	movq	24(%rcx), %rdi
	jmp	letJoinK.2F
	.text
letJoinK.33:
	movq	%r8, %rdx
	movq	%rdi, %rbx
	movq	$2827, -8(%rsi)
	movabsq	$letJoinK.34, %r13
	movq	%r13, (%rsi)
	movq	16(%rbx), %r14
	movq	%r14, 8(%rsi)
	movq	24(%rbx), %r15
	movq	%r15, 16(%rsi)
	movl	32(%rbx), %ecx
	movl	%ecx, 24(%rsi)
	movq	40(%rbx), %r10
	movq	%r10, 32(%rsi)
	movq	%rsi, %rcx
	addq	$48, %rsi
	cmpq	$1, %rdx
	je	L_true35
else.30:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block else<CEAF> (ep<CEAC>,landingPadItems<CEAE>,letJoinK<CEAD>) */
	movq	$12, -8(%rsi)
	movq	8(%rbx), %r12
	movq	80(%r12), %r13
	movq	%r13, (%rsi)
	movq	%rsi, %r10
	addq	$16, %rsi
	movq	$133, -8(%rsi)
	movq	%r10, (%rsi)
	movabsq	$append.31, %r15
	movq	%r15, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	$519, -8(%rsi)
	movabsq	$letJoinK.32, %r10
	movq	%r10, (%rsi)
	movq	8(%rbx), %r12
	movq	%r12, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%rsi, %rcx
	addq	$32, %rsi
	movq	(%r14), %rdi
	movq	%rdx, %r8
	movq	%rcx, %r9
	jmp	append.31
L_true35:
then.36:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block then<CEA9> (letJoinK<CEA8>) */
	movq	%rcx, %rdi
	movq	$1, %r8
	jmp	letJoinK.34
	.text
letJoinK.E:
	movq	%r8, %rdx
	movq	%rdi, %rbx
	cmpq	$1, %rdx
	jne	L_true3A
else.37:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block else<CF55> (ep<CF54>) */
	movq	$261, -8(%rsi)
	movabsq	$letJoinK.38, %r12
	movq	%r12, (%rsi)
	movq	32(%rbx), %r13
	movq	%r13, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	16(%rbx), %r14
	movq	(%r14), %rdi
	movq	%r10, %r8
	jmp	waitForWork.39
L_true3A:
then.3B:
	/* Liveout:  GP={%rax %rdi}  */
	/* block then<CF44> (ep<CF42>,item<CF43>) */
	movq	(%rdx), %r15
	movq	8(%r15), %rcx
	movq	(%r15), %rdx
	movq	8(%rbx), %r10
	movq	%rdx, 24(%r10)
	movq	$20, -8(%rsi)
	movq	24(%rbx), %r13
	movq	%r13, (%rsi)
	movq	8(%rbx), %r14
	movq	32(%r14), %r15
	movq	%r15, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	8(%rbx), %rdx
	movq	%r12, 32(%rdx)
	movq	8(%rbx), %rbx
	movq	$1, (%rbx)
	movq	(%rcx), %r10
	movq	$1, %r12
	movq	%r12, %rax
	movq	%rcx, %rdi
	jmp	*%r10
	.text
letJoinK.3F:
	movq	%r8, %rdx
	movq	%rdi, %rbx
	movq	$775, -8(%rsi)
	movabsq	$letJoinK.40, %r13
	movq	%r13, (%rsi)
	movq	16(%rbx), %r14
	movq	%r14, 8(%rsi)
	movq	24(%rbx), %r15
	movq	%r15, 16(%rsi)
	movq	%rsi, %rcx
	addq	$32, %rsi
	cmpq	$1, %rdx
	je	L_true41
else.3C:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block else<D008> (ep<D005>,landingPadItems<D007>,letJoinK<D006>) */
	movq	$12, -8(%rsi)
	movq	8(%rbx), %r12
	movq	80(%r12), %r13
	movq	%r13, (%rsi)
	movq	%rsi, %r10
	addq	$16, %rsi
	movq	$133, -8(%rsi)
	movq	%r10, (%rsi)
	movabsq	$append.3D, %r15
	movq	%r15, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	$519, -8(%rsi)
	movabsq	$letJoinK.3E, %r10
	movq	%r10, (%rsi)
	movq	8(%rbx), %r12
	movq	%r12, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%rsi, %rcx
	addq	$32, %rsi
	movq	(%r14), %rdi
	movq	%rdx, %r8
	movq	%rcx, %r9
	jmp	append.3D
L_true41:
then.42:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block then<D002> (letJoinK<D001>) */
	movq	%rcx, %rdi
	movq	$1, %r8
	jmp	letJoinK.40
	.text
letJoinK.4E:
	movq	%rdi, %rdx
	movq	8(%rdx), %r15
	movq	8(%r15), %rcx
	movq	8(%rdx), %rbx
	movq	16(%rbx), %rbx
	cmpq	$1, %rbx
	je	L_true4F
else.43:
	/* flushLoads */
	/* block else<D2CC> (ep<D2C9>,qHd<D2CB>,qTl<D2CA>) */
	movq	32(%rdx), %r14
	movq	%r14, 8(%rbx)
	jmp	letJoinK.44
L_true4F:
letJoinK.44:
	/* flushLoads */
	/* block letJoinK<D296> (ep<D294>,qHd<D295>) */
	movq	8(%rdx), %rbx
	movq	32(%rdx), %r10
	movq	%r10, 16(%rbx)
	cmpq	$1, %rcx
	je	L_true45
	jmp	letJoinK.46
L_true45:
then.47:
	/* flushLoads */
	/* block then<D2C1> (ep<D2C0>) */
	movq	8(%rdx), %r12
	movq	32(%rdx), %r13
	movq	%r13, 8(%r12)
letJoinK.46:
	/* flushLoads */
	/* block letJoinK<D29B> (ep<D29A>) */
	movq	8(%rdx), %r14
	movl	$0, (%r14)
	movq	$261, -8(%rsi)
	movabsq	$letJoinK.48, %r15
	movq	%r15, (%rsi)
	movq	16(%rdx), %rcx
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	24(%rdx), %r10
	movq	8(%r10), %rbx
	cmpq	$1, %rbx
	jne	L50
S_case49:
case.4A:
	/* Liveout:  GP={%rdi}  */
	/* block case<D2A9> (ep<D2A7>,letJoinK<D2A8>) */
	movq	24(%rdx), %r14
	movq	$1, (%r14)
	movq	%r13, %rdi
	jmp	letJoinK.48
L50:
	cmpq	$3, %rbx
	jne	S_case49
S_case4B:
case.4C:
	/* Liveout:  GP={%rax %rdi}  */
	/* block case<D2AF> (ep<D2AD>,letJoinK<D2AE>) */
	movq	24(%rdx), %r12
	movq	$1, 8(%r12)
	movq	$20, -8(%rsi)
	movabsq	$k.4D, %r14
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
	movq	32(%rcx), %r15
	movq	8(%r15), %rbx
	movq	24(%rdx), %r10
	movq	%rbx, 32(%r10)
	movq	(%r15), %r10
	movq	(%r10), %r13
	movq	%r12, %rax
	movq	%r10, %rdi
	jmp	*%r13
	.text
letJoinK.59:
	movq	%rdi, %rbx
	movq	16(%rbx), %r12
	movl	(%r12), %r13d
	cmpl	8(%rbx), %r13d
	jg	L5C
L_true5A:
then.58:
	/* flushLoads */
	/* block then<D3E7> (ep<D3E6>) */
	movq	16(%rbx), %r10
	movl	(%r10), %r10d
	incl	%r10d
	movq	16(%rbx), %rdx
	movl	%r10d, (%rdx)
	movq	$1, %r13
letJoinK.52:
	/* block letJoinK<D3C6> (ep<D3C4>,reset<D3C5>) */
	movq	$261, -8(%rsi)
	movabsq	$letJoinK.53, %r15
	movq	%r15, (%rsi)
	movq	24(%rbx), %rcx
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	cmpq	$1, %r13
	je	S_case54
	cmpq	$3, %r13
	je	S_case56
S_case54:
case.55:
	/* Liveout:  GP={%rdi}  */
	/* block case<D3D1> (letJoinK<D3D0>) */
	movq	%r12, %rdi
	jmp	letJoinK.53
L5C:
else.51:
	/* flushLoads */
	/* block else<D3EF> (ep<D3EE>) */
	movq	16(%rbx), %r14
	movl	$1, (%r14)
	movq	$3, %r13
	jmp	letJoinK.52
S_case56:
case.57:
	/* Liveout:  GP={%rax %rdi}  */
	/* block case<D3D5> (letJoinK<D3D4>) */
	movq	$20, -8(%rsi)
	movabsq	$k.5B, %r15
	movq	%r15, (%rsi)
	movq	%r12, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	$263, -8(%rsi)
	movq	$3, (%rsi)
	movq	%r14, 8(%rsi)
	movq	$300000000, 16(%rsi)
	movq	%rsi, %rdx
	addq	$32, %rsi
	movq	$3, (%r11)
	movq	32(%r11), %rcx
	movq	8(%rcx), %rbx
	movq	%rbx, 32(%r11)
	movq	(%rcx), %rcx
	movq	(%rcx), %r10
	movq	%rdx, %rax
	movq	%rcx, %rdi
	jmp	*%r10
	.text
letJoinK.62:
	movq	%rdi, %r10
	movq	8(%r10), %rbx
	movq	8(%rbx), %r12
	cmpq	$1, %r12
	je	L_true63
	movq	8(%r10), %r13
	movq	16(%r13), %r14
else.60:
	/* block else<D431> (ep<D42E>,qTl<D430>,qHd<D42F>) */
	movq	8(%r12), %r13
	cmpq	%r14, %r12
	je	L_true61
letJoinK.5E:
	/* flushLoads */
	/* block letJoinK<D437> (ep<D434>,qHd<D436>,qNext<D435>) */
	movq	8(%r10), %rcx
	movq	%r13, 8(%rcx)
	movq	$12, -8(%rsi)
	movq	(%r12), %rdx
	movq	%rdx, (%rsi)
	movq	%rsi, %rbx
	addq	$16, %rsi
	jmp	letJoinK.5F
L_true63:
then.64:
	/* block then<D42C> (ep<D42B>) */
	movq	$1, %rbx
letJoinK.5F:
	/* flushLoads */
	/* block letJoinK<D397> (ep<D395>,elt<D396>) */
	movq	8(%r10), %r14
	movl	$0, (%r14)
	cmpq	$1, %rbx
	jne	L_true65
else.66:
	/* Liveout:  GP={%rax %rdi}  */
	/* block else<D3A5> (ep<D3A4>) */
	movq	$2571, -8(%rsi)
	movabsq	$k.68, %rcx
	movq	%rcx, (%rsi)
	movl	16(%r10), %edx
	movl	%edx, 8(%rsi)
	movq	24(%r10), %rbx
	movq	%rbx, 16(%rsi)
	movq	32(%r10), %r12
	movq	%r12, 24(%rsi)
	movq	48(%r10), %r13
	movq	%r13, 32(%rsi)
	movq	%rsi, %r15
	addq	$48, %rsi
	movq	$20, -8(%rsi)
	movq	$1, (%rsi)
	movq	%r15, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	movq	32(%r10), %r15
	movq	32(%r15), %r14
	movq	8(%r14), %rcx
	movq	32(%r10), %rbx
	movq	%rcx, 32(%rbx)
	movq	(%r14), %rcx
	movq	(%rcx), %r10
	movq	%rdx, %rax
	movq	%rcx, %rdi
	jmp	*%r10
L_true65:
then.67:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block then<D39D> (ep<D39B>,elt<D39C>) */
	movq	(%rbx), %r12
	movq	40(%r10), %rdi
	movq	(%r12), %r8
	movq	8(%r12), %r9
	jmp	run.69
L_true61:
then.5D:
	/* flushLoads */
	/* block then<D43E> (ep<D43B>,qHd<D43D>,qNext<D43C>) */
	movq	8(%r10), %r15
	movq	$1, 16(%r15)
	jmp	letJoinK.5E
	.text
letJoinK.12:
	movq	%rdi, %rdx
	movq	$261, -8(%rsi)
	movabsq	$letJoinK.6C, %r14
	movq	%r14, (%rsi)
	movq	8(%rdx), %r15
	movq	%r15, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	16(%rdx), %r10
	movq	8(%r10), %rbx
	cmpq	$1, %rbx
	je	S_case6D
	cmpq	$3, %rbx
	je	S_case6F
S_case6D:
case.6E:
	/* Liveout:  GP={%rdi}  */
	/* block case<D4CA> (ep<D4C8>,letJoinK<D4C9>) */
	movq	16(%rdx), %r12
	movq	$1, (%r12)
	movq	%rcx, %rdi
	jmp	letJoinK.6C
S_case6F:
case.6A:
	/* Liveout:  GP={%rax %rdi}  */
	/* block case<D4D0> (ep<D4CE>,letJoinK<D4CF>) */
	movq	16(%rdx), %r12
	movq	$1, 8(%r12)
	movq	$20, -8(%rsi)
	movabsq	$k.6B, %r14
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
	movq	32(%rcx), %r15
	movq	8(%r15), %rbx
	movq	16(%rdx), %r12
	movq	%rbx, 32(%r12)
	movq	(%r15), %rbx
	movq	(%rbx), %r13
	movq	%r10, %rax
	movq	%rbx, %rdi
	jmp	*%r13
	.text
letJoinK.78:
	movq	%rdi, %rbx
	movq	16(%rbx), %r12
	movl	(%r12), %r13d
	cmpl	8(%rbx), %r13d
	jg	L7B
L_true79:
then.77:
	/* flushLoads */
	/* block then<DC4F> (ep<DC4E>) */
	movq	16(%rbx), %r10
	movl	(%r10), %r10d
	incl	%r10d
	movq	16(%rbx), %rdx
	movl	%r10d, (%rdx)
	movq	$1, %r13
letJoinK.71:
	/* block letJoinK<DC2F> (ep<DC2D>,reset<DC2E>) */
	movq	$261, -8(%rsi)
	movabsq	$letJoinK.72, %r15
	movq	%r15, (%rsi)
	movq	24(%rbx), %rcx
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	cmpq	$1, %r13
	je	S_case73
	cmpq	$3, %r13
	je	S_case75
S_case73:
case.74:
	/* Liveout:  GP={%rdi}  */
	/* block case<DC39> (letJoinK<DC38>) */
	movq	%r12, %rdi
	jmp	letJoinK.72
L7B:
else.70:
	/* flushLoads */
	/* block else<DC57> (ep<DC56>) */
	movq	16(%rbx), %r14
	movl	$1, (%r14)
	movq	$3, %r13
	jmp	letJoinK.71
S_case75:
case.76:
	/* Liveout:  GP={%rax %rdi}  */
	/* block case<DC3D> (letJoinK<DC3C>) */
	movq	$20, -8(%rsi)
	movabsq	$k.7A, %r15
	movq	%r15, (%rsi)
	movq	%r12, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	$263, -8(%rsi)
	movq	$3, (%rsi)
	movq	%r14, 8(%rsi)
	movq	$1000000, 16(%rsi)
	movq	%rsi, %rdx
	addq	$32, %rsi
	movq	$3, (%r11)
	movq	32(%r11), %rcx
	movq	8(%rcx), %rbx
	movq	%rbx, 32(%r11)
	movq	(%rcx), %rcx
	movq	(%rcx), %r10
	movq	%rdx, %rax
	movq	%rcx, %rdi
	jmp	*%r10
	.text
letJoinK.1C:
	movq	%rdi, %rdx
	movq	$261, -8(%rsi)
	movabsq	$letJoinK.7E, %r14
	movq	%r14, (%rsi)
	movq	8(%rdx), %r15
	movq	%r15, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	16(%rdx), %r10
	movq	8(%r10), %rbx
	cmpq	$1, %rbx
	je	S_case7F
	cmpq	$3, %rbx
	je	S_case81
S_case7F:
case.80:
	/* Liveout:  GP={%rdi}  */
	/* block case<DF3C> (ep<DF3A>,letJoinK<DF3B>) */
	movq	16(%rdx), %r12
	movq	$1, (%r12)
	movq	%rcx, %rdi
	jmp	letJoinK.7E
S_case81:
case.7C:
	/* Liveout:  GP={%rax %rdi}  */
	/* block case<DF42> (ep<DF40>,letJoinK<DF41>) */
	movq	16(%rdx), %r12
	movq	$1, 8(%r12)
	movq	$20, -8(%rsi)
	movabsq	$k.7D, %r14
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
	movq	32(%rcx), %r15
	movq	8(%r15), %rbx
	movq	16(%rdx), %r12
	movq	%rbx, 32(%r12)
	movq	(%r15), %rbx
	movq	(%rbx), %r13
	movq	%r10, %rax
	movq	%rbx, %rdi
	jmp	*%r13
	.text
retGC85:
	movq	24(%rdi), %rdx
	movq	16(%rdi), %rcx
	movq	8(%rdi), %r10
	movq	(%rdi), %rbx
gcTest86:
	movq	%r11, %r12
	movq	448(%r12), %r12
	subq	%rsi, %r12
	jle	doGC87
check.82:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block check<E1BA> (ep<CAD3>,retK<CAD6>,i<CAD5>,_t<CAD4>) */
	movq	$10, -8(%rsi)
	movq	(%rcx), %r13
	incq	%r13
	movq	%r13, (%rsi)
	movq	%rsi, %r14
	addq	$16, %rsi
	movq	%rbx, %rdi
	movq	%rdx, %r8
	shrq	$1, %r8
	movq	%r14, %r9
lp.83:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	cmpq	$1, %rdx
	jne	L8A
L_true88:
then.89:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block then<CAD1> (retK<CAD0>,i<CACF>) */
	movq	%r10, %rdi
	movq	%rcx, %r8
	jmp	letJoinK.26
L8A:
else.84:
	/* block else<CAD7> (ep<E1B6>,retK<E1B7>,i<E1B8>,_t<E1B9>) */
	jmp	gcTest86
doGC87:
	movq	$777, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%r10, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%rdx, 24(%rsi)
	movq	%rsi, %r15
	addq	$40, %rsi
	movq	%r15, %rdi
	movabsq	$retGC85, %r8
	jmp	_ASM_InvokeGC
	/* live= GP={%rdx %rcx %r10 %rbx} spilled=  */
	jmp	retGC85
	.text
ceilingLg.8C:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest8E
	/* live= GP={%rcx %rdx %rbx} spilled=  */
retGC8D:
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest8E:
	movq	%r11, %r10
	movq	448(%r10), %r10
	subq	%rsi, %r10
	jle	doGC8F
check.8B:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block check<E1BE> (ep<CA8D>,x<CA8E>,retK<CA8F>) */
	movq	$775, -8(%rsi)
	movabsq	$letJoinK.26, %r12
	movq	%r12, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%rsi, %r10
	addq	$32, %rsi
	movq	$18, -8(%rsi)
	movq	$1, (%rsi)
	movabsq	$lp.83, %r14
	movq	%r14, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	$10, -8(%rsi)
	movq	$0, (%rsi)
	movq	%rsi, %r15
	addq	$16, %rsi
	movq	(%r13), %rdi
	movq	(%rdx), %r8
	movq	%r15, %r9
	jmp	lp.83
doGC8F:
	movq	$28, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%rsi, %rcx
	addq	$32, %rsi
	movq	%rcx, %rdi
	movabsq	$retGC8D, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.91:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest93
	/* live= GP={%rcx %rdx} spilled=  */
retGC92:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest93:
	movq	%r11, %r14
	movq	448(%r14), %rbx
	subq	%rsi, %rbx
	jle	doGC94
check.90:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<E1C1> (ep<CB0E>,retval<CB0C>) */
	movq	8(%rdx), %r10
	movq	(%r10), %r12
	movq	%r10, %rdi
	movq	%rcx, %r8
	jmp	*%r12
doGC94:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	%r13, %rdi
	movabsq	$retGC92, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.97:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest99
	/* live= GP={%rcx %rdx} spilled=  */
retGC98:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest99:
	movq	%r11, %r12
	movq	448(%r12), %rbx
	subq	%rsi, %rbx
	jle	doGC9A
check.95:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block check<E1C4> (ep<CB2A>,_t<CB26>) */
	movq	8(%rdx), %r13
	movq	24(%rdx), %r14
	movl	(%r14), %r10d
	shlq	$3, %r10
	movq	%rcx, (%r13,%r10,1)
	movq	$10, -8(%rsi)
	movq	24(%rdx), %rcx
	movl	(%rcx), %r12d
	incl	%r12d
	movl	%r12d, (%rsi)
	movq	%rsi, %r15
	addq	$16, %rsi
	movq	16(%rdx), %rbx
	movq	(%rbx), %rdi
	movq	%r15, %r8
	movq	32(%rdx), %r9
	movq	40(%rdx), %r10
	jmp	tab.96
doGC9A:
	movq	$133, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	%r10, %rdi
	movabsq	$retGC98, %r8
	jmp	_ASM_InvokeGC
	.text
tab.96:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTestA0
	/* live= GP={%r10 %rcx %rdx %rbx} spilled=  */
retGC9F:
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTestA0:
	movq	%r11, %r12
	movq	448(%r12), %r12
	subq	%rsi, %r12
	jle	doGCA1
check.9B:
	/* block check<E1C9> (ep<CB17>,i<CB18>,retK<CB19>,_exh<CB1A>) */
	movq	(%rbx), %r13
	movl	(%rdx), %r14d
	cmpl	(%r13), %r14d
	jge	LA2
L_true9C:
then.9E:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block then<CB25> (ep<CB21>,i<CB24>,retK<CB23>,_exh<CB22>) */
	movq	$133, -8(%rsi)
	movq	%rbx, (%rsi)
	movabsq	$tab.96, %r12
	movq	%r12, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	$7949, -8(%rsi)
	movabsq	$letJoinK.97, %r14
	movq	%r14, (%rsi)
	movq	16(%rbx), %r12
	movq	%r12, 8(%rsi)
	movq	%r15, 16(%rsi)
	movq	%rdx, 24(%rsi)
	movq	%rcx, 32(%rsi)
	movq	%r10, 40(%rsi)
	movq	%rsi, %r13
	addq	$56, %rsi
	movq	8(%rbx), %r14
	movq	8(%r14), %r15
	movq	(%r14), %rdi
	movl	(%rdx), %r8d
	movq	%r13, %r9
	jmp	*%r15
doGCA1:
	movq	$36, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %rcx
	addq	$40, %rsi
	movq	%rcx, %rdi
	movabsq	$retGC9F, %r8
	jmp	_ASM_InvokeGC
LA2:
else.9D:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block else<CB40> (ep<CB3E>,retK<CB3F>) */
	movq	%rcx, %rdi
	movq	24(%rbx), %r8
	jmp	letJoinK.91
	.text
letJoinK.A5:
	movq	%r8, %rdx
	movq	%rdi, %r15
	jmp	gcTestA7
	/* live= GP={%rdx %r15} spilled=  */
retGCA6:
	movq	8(%rdi), %rdx
	movq	(%rdi), %r15
gcTestA7:
	movq	%r11, %r13
	movq	448(%r13), %r14
	subq	%rsi, %r14
	jle	doGCA8
check.A3:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block check<E1CC> (ep<CB03>,_t<CB02>) */
	movq	24(%r15), %rcx
	movl	(%rcx), %r14d
	movq	%rax, %r13
	movq	%rdi, %r12
	movq	%r8, %rbx
	movq	%r9, %rcx
	movq	%rcx, -56(%rbp)
	movq	%rsi, %r10
	movq	%r10, -72(%rbp)
	movq	%r11, %rcx
	movq	%rcx, -64(%rbp)
	movq	%r11, %r10
	movq	%r10, %rdi
	movslq	%r14d, %rcx
	movq	%rcx, %rsi
	call	_GlobalAllocWord64Array
	movq	%rax, %rcx
	movq	%r13, %rax
	movq	%r12, %rdi
	movq	%rbx, %r8
	movq	-56(%rbp), %r9
	movq	-72(%rbp), %rsi
	movq	-64(%rbp), %r11
	movq	$133, -8(%rsi)
	movq	%rcx, (%rsi)
	movl	%r14d, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	$261, -8(%rsi)
	movabsq	$letJoinK.91, %r12
	movq	%r12, (%rsi)
	movq	8(%r15), %rdx
	movq	%rdx, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	movq	$36, -8(%rsi)
	movq	24(%r15), %r12
	movq	%r12, (%rsi)
	movq	32(%r15), %r13
	movq	%r13, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %rbx
	addq	$40, %rsi
	movq	$133, -8(%rsi)
	movq	%rbx, (%rsi)
	movabsq	$tab.96, %rbx
	movq	%rbx, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	$10, -8(%rsi)
	movl	$1, (%rsi)
	movq	%rsi, %r10
	addq	$16, %rsi
	movq	(%r14), %rdi
	movq	%r10, %r8
	movq	%rdx, %r9
	movq	16(%r15), %r10
	jmp	tab.96
doGCA8:
	movq	$133, -8(%rsi)
	movq	%r15, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	%r12, %rdi
	movabsq	$retGCA6, %r8
	jmp	_ASM_InvokeGC
	.text
tabulate.AD:
	movq	%r9, %r15
	movq	%r8, %rdx
	movq	%rdi, %rcx
	jmp	gcTestAF
	/* live= GP={%r12 %r10 %r15 %rdx %rcx} spilled=  */
retGCAE:
	movq	32(%rdi), %r12
	movq	24(%rdi), %r10
	movq	16(%rdi), %r15
	movq	8(%rdi), %rdx
	movq	(%rdi), %rcx
gcTestAF:
	movq	%r11, %rbx
	movq	448(%rbx), %r13
	subq	%rsi, %r13
	jle	doGCB0
check.A9:
	/* block check<E1D2> (ep<CAE6>,n<CAE7>,f<CAE8>,retK<CAE9>,_exh<CAEA>) */
	cmpl	$0, (%rdx)
	jne	LB1
L_trueAA:
	movq	%r10, -56(%rbp)
then.AC:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block then<CAF2> (retK<CAF1>) */
	movq	%rax, %r15
	movq	%rdi, %rbx
	movq	%r8, %r12
	movq	%r9, %r13
	movq	%rsi, %r14
	movq	%r11, %rcx
	movq	%rcx, -64(%rbp)
	movq	%r11, %rcx
	movq	%rcx, %rdi
	xorl	%ecx, %ecx
	movslq	%ecx, %rdx
	movq	%rdx, %rsi
	xorq	%rdx, %rdx
	call	_GlobalAllocWord64Array
	movq	%rax, %r10
	movq	%r15, %rax
	movq	%rbx, %rdi
	movq	%r12, %r8
	movq	%r13, %r9
	movq	%r14, %rsi
	movq	-64(%rbp), %r11
	movq	$133, -8(%rsi)
	movq	%r10, (%rsi)
	movl	$0, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	movq	-56(%rbp), %rbx
	movq	(%rbx), %rbx
	movq	-56(%rbp), %rdi
	movq	%rdx, %r8
	jmp	*%rbx
doGCB0:
	movq	$44, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%r15, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%r12, 32(%rsi)
	movq	%rsi, %r10
	addq	$48, %rsi
	movq	%r10, %rdi
	movabsq	$retGCAE, %r8
	jmp	_ASM_InvokeGC
LB1:
else.AB:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block else<CB00> (retK<CAFF>,_exh<CAFE>,n<CAFD>,f<CAFC>) */
	movq	$3851, -8(%rsi)
	movabsq	$letJoinK.A5, %r13
	movq	%r13, (%rsi)
	movq	%r10, 8(%rsi)
	movq	%r12, 16(%rsi)
	movq	%rdx, 24(%rsi)
	movq	%r15, 32(%rsi)
	movq	%rsi, %rbx
	addq	$48, %rsi
	movq	8(%r15), %r14
	movq	(%r15), %rdi
	xorl	%r8d, %r8d
	movq	%rbx, %r9
	movq	%r12, %r10
	jmp	*%r14
	.text
letJoinK.B4:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTestB6
	/* live= GP={%rcx %rdx} spilled=  */
retGCB5:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTestB6:
	movq	%r11, %r13
	movq	448(%r13), %rbx
	subq	%rsi, %rbx
	jle	doGCB7
check.B2:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block check<E1D5> (ep<CB69>,unused<CB66>) */
	movq	8(%rdx), %r10
	movq	(%r10), %rdi
	movq	32(%rdx), %r8
	movq	16(%rdx), %r9
	movq	24(%rdx), %r10
	jmp	lp.B3
doGCB7:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	%r12, %rdi
	movabsq	$retGCB5, %r8
	jmp	_ASM_InvokeGC
	.text
lp.B3:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTestBD
	/* live= GP={%r10 %rcx %rdx %rbx} spilled=  */
retGCBC:
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTestBD:
	movq	%r11, %r12
	movq	448(%r12), %r12
	subq	%rsi, %r12
	jle	doGCBE
check.B8:
	/* block check<E1DA> (ep<CB56>,xs<CB57>,retK<CB58>,_exh<CB59>) */
	cmpq	$1, %rdx
	je	LBF
L_trueB9:
then.BB:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block then<CB62> (ep<CB5E>,xs<CB61>,retK<CB60>,_exh<CB5F>) */
	movq	$133, -8(%rsi)
	movq	%rbx, (%rsi)
	movabsq	$lp.B3, %r15
	movq	%r15, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	$3851, -8(%rsi)
	movabsq	$letJoinK.B4, %r12
	movq	%r12, (%rsi)
	movq	%r14, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	8(%rdx), %r13
	movq	%r13, 32(%rsi)
	movq	%rsi, %rcx
	addq	$48, %rsi
	movq	(%rbx), %r14
	movq	8(%r14), %r15
	movq	(%r14), %rdi
	movq	(%rdx), %r8
	movq	%rcx, %r9
	jmp	*%r15
doGCBE:
	movq	$36, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %rcx
	addq	$40, %rsi
	movq	%rcx, %rdi
	movabsq	$retGCBC, %r8
	jmp	_ASM_InvokeGC
LBF:
else.BA:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block else<CB77> (retK<CB76>) */
	movq	(%rcx), %r13
	movq	%rcx, %rdi
	movq	$1, %r8
	jmp	*%r13
	.text
app_D_w_uncurried.C:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTestC2
	/* live= GP={%r12 %r10 %rcx %rdx %rbx} spilled=  */
retGCC1:
	movq	32(%rdi), %r12
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTestC2:
	movq	%r11, %r13
	movq	448(%r13), %r13
	subq	%rsi, %r13
	jle	doGCC3
check.C0:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block check<E1E0> (ep<CB4C>,_arg<CB4D>,ls<CB4E>,retK<CB4F>,_exh<CB50>) */
	movq	$12, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rsi, %r14
	addq	$16, %rsi
	movq	$133, -8(%rsi)
	movq	%r14, (%rsi)
	movabsq	$lp.B3, %rdx
	movq	%rdx, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	(%r15), %rdi
	movq	%rcx, %r8
	movq	%r10, %r9
	movq	%r12, %r10
	jmp	lp.B3
doGCC3:
	movq	$44, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%r12, 32(%rsi)
	movq	%rsi, %rbx
	addq	$48, %rsi
	movq	%rbx, %rdi
	movabsq	$retGCC1, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.CB:
	movq	%r8, %rcx
	movq	%rdi, %rdx
gcTestCD:
	movq	%r11, %rbx
	movq	448(%rbx), %rbx
	subq	%rsi, %rbx
	jle	doGCCE
check.C4:
	/* block check<E1E3> (ep<CB9D>,_t<CB9A>) */
	cmpq	$1, %rcx
	je	S_caseC5
	cmpq	$3, %rcx
	je	S_caseC7
S_caseC5:
case.C6:
	/* block case<CBAA> (ep<CBA9>) */
	movq	32(%rdx), %r10
	jmp	letJoinK.C9
doGCCE:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	%r15, %rdi
	movabsq	$retGCCC, %r8
	jmp	_ASM_InvokeGC
S_caseC7:
case.C8:
	/* block case<CBAD> (ep<CBAC>) */
	movq	$20, -8(%rsi)
	movq	40(%rdx), %r12
	movq	%r12, (%rsi)
	movq	32(%rdx), %r13
	movq	%r13, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
letJoinK.C9:
	/* Liveout:  GP={%r12 %r10 %r9 %r8 %rdi}  */
	/* block letJoinK<CBA2> (ep<CBA0>,_t<CBA1>) */
	movq	8(%rdx), %r14
	movq	(%r14), %rdi
	movq	48(%rdx), %r8
	movq	%r10, %r9
	movq	16(%rdx), %r10
	movq	24(%rdx), %r12
	jmp	lp.CA
	/* live= GP={%rcx %rdx} spilled=  */
retGCCC:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
	jmp	gcTestCD
	.text
lp.D3:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTestD5
	/* live= GP={%r10 %rcx %rdx %rbx} spilled=  */
retGCD4:
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTestD5:
	movq	%r11, %r12
	movq	448(%r12), %r12
	subq	%rsi, %r12
	jle	doGCD6
check.CF:
	/* block check<E1E8> (ep<CBBC>,ls<CBBD>,acc<CBBE>,retK<CBBF>) */
	cmpq	$1, %rdx
	je	LD7
L_trueD0:
then.D2:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block then<CBC8> (ep<CBC4>,retK<CBC7>,ls<CBC6>,acc<CBC5>) */
	movq	$20, -8(%rsi)
	movq	(%rdx), %r15
	movq	%r15, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	%rbx, %rdi
	movq	8(%rdx), %r8
	movq	%r14, %r9
	jmp	lp.D3
doGCD6:
	movq	$36, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %rcx
	addq	$40, %rsi
	movq	%rcx, %rdi
	movabsq	$retGCD4, %r8
	jmp	_ASM_InvokeGC
LD7:
else.D1:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block else<CBD1> (retK<CBD0>,acc<CBCF>) */
	movq	(%r10), %r13
	movq	%r10, %rdi
	movq	%rcx, %r8
	jmp	*%r13
	.text
lp.CA:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTestDD
	/* live= GP={%r12 %r10 %rcx %rdx %rbx} spilled=  */
retGCDC:
	movq	32(%rdi), %r12
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTestDD:
	movq	%r11, %r13
	movq	448(%r13), %r13
	subq	%rsi, %r13
	jle	doGCDE
check.D8:
	/* block check<E1EE> (ep<CB88>,_anon_<CB89>,_anon_<CB8A>,retK<CB8B>,_exh<CB8C>) */
	cmpq	$1, %rdx
	je	LDF
L_trueD9:
then.DB:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block then<CB96> (ep<CB91>,retK<CB95>,_exh<CB94>,_anon_<CB93>,_anon_<CB92>) */
	movq	(%rdx), %r15
	movq	$133, -8(%rsi)
	movq	%rbx, (%rsi)
	movabsq	$lp.CA, %r14
	movq	%r14, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	$16143, -8(%rsi)
	movabsq	$letJoinK.CB, %r14
	movq	%r14, (%rsi)
	movq	%r13, 8(%rsi)
	movq	%r10, 16(%rsi)
	movq	%r12, 24(%rsi)
	movq	%rcx, 32(%rsi)
	movq	%r15, 40(%rsi)
	movq	8(%rdx), %rcx
	movq	%rcx, 48(%rsi)
	movq	%rsi, %rcx
	addq	$64, %rsi
	movq	(%rbx), %rdx
	movq	8(%rdx), %rbx
	movq	(%rdx), %rdi
	movq	(%r15), %r8
	movq	%rcx, %r9
	movq	%r12, %r10
	jmp	*%rbx
doGCDE:
	movq	$44, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%r12, 32(%rsi)
	movq	%rsi, %r10
	addq	$48, %rsi
	movq	%r10, %rdi
	movabsq	$retGCDC, %r8
	jmp	_ASM_InvokeGC
LDF:
else.DA:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block else<CBB9> (retK<CBB8>,_anon_<CBB7>) */
	movq	$18, -8(%rsi)
	movq	$1, (%rsi)
	movabsq	$lp.D3, %r15
	movq	%r15, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	(%r14), %rdi
	movq	%rcx, %r8
	movq	$1, %r9
	jmp	lp.D3
	.text
filter_D_w_uncurried.E1:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTestE3
	/* live= GP={%r12 %r10 %rcx %rdx %rbx} spilled=  */
retGCE2:
	movq	32(%rdi), %r12
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTestE3:
	movq	%r11, %r13
	movq	448(%r13), %r13
	subq	%rsi, %r13
	jle	doGCE4
check.E0:
	/* Liveout:  GP={%r12 %r10 %r9 %r8 %rdi}  */
	/* block check<E1F4> (ep<CB7E>,_arg<CB7F>,xs<CB80>,retK<CB81>,_exh<CB82>) */
	movq	$12, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rsi, %r14
	addq	$16, %rsi
	movq	$133, -8(%rsi)
	movq	%r14, (%rsi)
	movabsq	$lp.CA, %rdx
	movq	%rdx, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	(%r15), %rdi
	movq	%rcx, %r8
	movq	$1, %r9
	jmp	lp.CA
doGCE4:
	movq	$44, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%r12, 32(%rsi)
	movq	%rsi, %rbx
	addq	$48, %rsi
	movq	%rbx, %rdi
	movabsq	$retGCE2, %r8
	jmp	_ASM_InvokeGC
	.text
get_D_ite.EB:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTestED
	/* live= GP={%rcx %rdx %rbx} spilled=  */
retGCEC:
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTestED:
	movq	%r11, %r14
	movq	448(%r14), %r10
	subq	%rsi, %r10
	jle	doGCEE
check.E5:
	/* block check<E1F8> (ep<CBDB>,retK<CBDC>,exh<CBDD>) */
	movq	24(%r11), %r14
	movq	8(%r14), %r13
	cmpq	$1, %r13
	je	LEF
L_trueE6:
then.E8:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block then<CBE8> (retK<CBE7>,_t<CBE6>) */
	movq	(%rdx), %r12
	movq	%rdx, %rdi
	movq	(%r13), %r8
	jmp	*%r12
doGCEE:
	movq	$28, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%rsi, %r13
	addq	$32, %rsi
	movq	%r13, %rdi
	movabsq	$retGCEC, %r8
	jmp	_ASM_InvokeGC
LEF:
else.E7:
	/* Liveout:  GP={%rax %rdi}  */
	/* block else<CBEE> (exh<CBED>) */
	movq	$133, -8(%rsi)
	movabsq	$strE9, %rdx
	movq	%rdx, (%rsi)
	movl	$51, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movabsq	$tagEA, %rbx
	movq	%rbx, (%rsi)
	movq	%r15, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	(%rcx), %r10
	movq	%r12, %rax
	movq	%rcx, %rdi
	jmp	*%r10
	.text
set_D_ite.F1:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTestF3
	/* live= GP={%rcx %rdx %rbx} spilled=  */
retGCF2:
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTestF3:
	movq	%r11, %r13
	movq	448(%r13), %r10
	subq	%rsi, %r10
	jle	doGCF4
check.F0:
	/* Liveout:  GP={%rdi}  */
	/* block check<E1FC> (ep<CBF7>,ite<CBF8>,retK<CBF9>) */
	movq	24(%r11), %r12
	movq	$12, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rsi, %r13
	addq	$16, %rsi
	movq	$1289, -8(%rsi)
	movl	(%r12), %r15d
	movl	%r15d, (%rsi)
	movq	%r13, 8(%rsi)
	movl	16(%r12), %edx
	movl	%edx, 16(%rsi)
	movq	24(%r12), %rbx
	movq	%rbx, 24(%rsi)
	movq	%rsi, %r14
	addq	$40, %rsi
	movq	%r14, 24(%r11)
	movq	(%rcx), %r10
	movq	%rcx, %rdi
	jmp	*%r10
doGCF4:
	movq	$28, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%rsi, %r12
	addq	$32, %rsi
	movq	%r12, %rdi
	movabsq	$retGCF2, %r8
	jmp	_ASM_InvokeGC
	.text
anon.F6:
	movq	%r9, %rdx
	movq	%r8, %rcx
	movq	%rdi, %rbx
	jmp	gcTestF8
	/* live= GP={%r10 %rdx %rcx %rbx} spilled=  */
retGCF7:
	movq	24(%rdi), %r10
	movq	16(%rdi), %rdx
	movl	8(%rdi), %ecx
	movq	(%rdi), %rbx
gcTestF8:
	movq	%r11, %r12
	movq	448(%r12), %r12
	subq	%rsi, %r12
	jle	doGCF9
check.F5:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<E201> (ep<CC13>,_t<CC14>,retK<CC15>,_exh<CC16>) */
	movq	(%rdx), %r14
	movq	%rdx, %rdi
	movl	%ecx, %r13d
	movq	(%rbx), %r15
	movq	$64, %rcx
	subq	(%r15), %rcx
	movq	%r13, %r8
	shlq	%cl, %r8
	jmp	*%r14
doGCF9:
	movq	$1673, -8(%rsi)
	movq	%rbx, (%rsi)
	movl	%ecx, 8(%rsi)
	movq	%rdx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %rcx
	addq	$40, %rsi
	movq	%rcx, %rdi
	movabsq	$retGCF7, %r8
	jmp	_ASM_InvokeGC
	.text
new.FB:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTestFD
	/* live= GP={%rcx %rdx %rbx} spilled=  */
retGCFC:
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTestFD:
	movq	%r11, %r10
	movq	448(%r10), %r10
	subq	%rsi, %r10
	jle	doGCFE
check.FA:
	/* Liveout:  GP={%rdi}  */
	/* block check<E205> (ep<CC2F>,data<CC30>,retK<CC31>) */
	movq	$3, (%r11)
	movl	224(%r11), %r13d
	movq	%r13, %r14
	shlq	$3, %r14
	movq	(%rdx,%r14,1), %r12
	incq	%r12
	shlq	$3, %r13
	movq	%r12, (%rdx,%r13,1)
	movq	(%rcx), %r15
	movq	%rcx, %rdi
	jmp	*%r15
doGCFE:
	movq	$28, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%rsi, %rcx
	addq	$32, %rsi
	movq	%rcx, %rdi
	movabsq	$retGCFC, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.100:
	movq	%rdi, %r10
	jmp	gcTest102
	/* live= spilled= GP={%r~1}  */
retGC101:
	movq	(%rdi), %r10
gcTest102:
	movq	%r11, %rcx
	movq	448(%rcx), %rdx
	subq	%rsi, %rdx
	jle	doGC103
	movq	%r10, -64(%rbp)
check.FF:
	/* Liveout:  GP={%rax %rdi}  */
	/* flushLoads */
	/* block check<E207> (ep<CC59>) */
	movq	%r11, %r12
	movq	%rsi, 128(%r12)
	movq	%rax, %rbx
	movq	%rbx, -72(%rbp)
	movq	%rdi, %r13
	movq	%r8, %r14
	movq	%r9, %r15
	movq	%r11, %rbx
	movq	%r11, %r10
	movq	%r10, %rdi
	movq	-64(%rbp), %r10
	movq	8(%r10), %rdx
	movq	16(%rdx), %rcx
	movq	%rcx, %rsi
	call	_PromoteObj
	movq	%rax, %rcx
	movq	%rcx, -56(%rbp)
	movq	-72(%rbp), %rax
	movq	%r13, %rdi
	movq	%r14, %r8
	movq	%r15, %r9
	movq	%rbx, %r11
	movq	128(%r12), %rsi
	movq	%r11, %rbx
	movq	%rsi, 128(%rbx)
	movq	%rax, %rdx
	movq	%rdx, -72(%rbp)
	movq	%rdi, %r12
	movq	%r8, %r13
	movq	%r9, %r14
	movq	%r11, %r15
	movq	%r11, %rcx
	movq	%rcx, %rdi
	movq	-64(%rbp), %r10
	movq	8(%r10), %r10
	movq	24(%r10), %rdx
	movq	%rdx, %rsi
	call	_PromoteObj
	movq	%rax, %rcx
	movq	-72(%rbp), %rax
	movq	%r12, %rdi
	movq	%r13, %r8
	movq	%r14, %r9
	movq	%r15, %r11
	movq	128(%rbx), %rsi
	movq	-64(%rbp), %r12
	movq	8(%r12), %rdx
	movq	-64(%rbp), %r13
	movq	8(%r13), %rbx
	movq	-56(%rbp), %r14
	movq	%r14, (%rcx)
	movq	%r11, %r12
	movq	%rsi, 128(%r12)
	movq	%rax, %r15
	movq	%r15, -56(%rbp)
	movq	%rdi, %r13
	movq	%r8, %r14
	movq	%r9, %r15
	movq	%r11, %rbx
	movq	%r11, %r10
	movq	%r10, %rdi
	movq	-64(%rbp), %rcx
	movq	8(%rcx), %rdx
	movq	8(%rdx), %rcx
	movq	%rcx, %rsi
	call	_PromoteObj
	movq	%rax, %rcx
	movq	-56(%rbp), %rax
	movq	%r13, %rdi
	movq	%r14, %r8
	movq	%r15, %r9
	movq	%rbx, %r11
	movq	128(%r12), %rsi
	movq	-64(%rbp), %rdx
	movq	8(%rdx), %r12
	movq	(%rcx), %rax
	xorq	%rdx, %rdx
	lock
	cmpxchgq	%rdx, (%rcx)
	movq	$3, (%r11)
	movq	32(%r11), %rbx
	movq	8(%rbx), %r10
	movq	%r10, 32(%r11)
	movq	(%rbx), %r12
	movq	(%r12), %r13
	movq	$1, %r14
	movq	%r14, %rax
	movq	%r12, %rdi
	jmp	*%r13
doGC103:
	movq	$12, -8(%rsi)
	movq	%r10, (%rsi)
	movq	%rsi, %r15
	addq	$16, %rsi
	movq	%r15, %rdi
	movabsq	$retGC101, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.105:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest107
	/* live= GP={%rcx %rdx} spilled=  */
retGC106:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest107:
	movq	%r11, %r10
	movq	448(%r10), %rbx
	subq	%rsi, %rbx
	jle	doGC108
check.104:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block check<E20A> (ep<CC52>,ite<CC4F>) */
	movq	$20, -8(%rsi)
	movq	(%rcx), %r12
	movq	%r12, (%rsi)
	movq	24(%rdx), %r13
	movq	%r13, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	$261, -8(%rsi)
	movabsq	$letJoinK.100, %r15
	movq	%r15, (%rsi)
	movq	16(%rdx), %rcx
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	8(%rdx), %rdx
	movq	(%rdx), %rdi
	movq	%r10, %r8
	movq	%r14, %r9
	jmp	set_D_ite.F1
doGC108:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	%rbx, %rdi
	movabsq	$retGC106, %r8
	jmp	_ASM_InvokeGC
	.text
terminate.10A:
	movq	%rdi, %rcx
	jmp	gcTest10C
	/* live= GP={%rcx} spilled=  */
retGC10B:
	movq	(%rdi), %rcx
gcTest10C:
	movq	%r11, %r10
	movq	448(%r10), %rdx
	subq	%rsi, %rdx
	jle	doGC10D
check.109:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block check<E20C> (ep<CC4A>) */
	movq	$1801, -8(%rsi)
	movabsq	$letJoinK.105, %r10
	movq	%r10, (%rsi)
	movq	16(%rcx), %r12
	movq	%r12, 8(%rsi)
	movq	24(%rcx), %r13
	movq	%r13, 16(%rsi)
	movq	24(%rcx), %r14
	movq	32(%r14), %r15
	movq	%r15, 24(%rsi)
	movq	%rsi, %rbx
	addq	$40, %rsi
	movq	8(%rcx), %rdx
	movq	(%rdx), %rdi
	movq	%rbx, %r8
	movq	32(%rcx), %r9
	jmp	get_D_ite.EB
doGC10D:
	movq	$12, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%rsi, %rbx
	addq	$16, %rsi
	movq	%rbx, %rdi
	movabsq	$retGC10B, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.113:
	movq	%rdi, %r12
gcTest115:
	movq	%r11, %r13
	movq	448(%r13), %r14
	subq	%rsi, %r14
	jle	doGC116
check.10E:
	/* block check<E20E> (ep<CC91>) */
	movq	%r11, %r13
	movq	%rsi, 128(%r13)
	movq	%rax, %r14
	movq	%rdi, %r15
	movq	%r15, -56(%rbp)
	movq	%r8, %rcx
	movq	%rcx, -64(%rbp)
	movq	%r9, %r15
	movq	%r11, %rbx
	movq	%r11, %r10
	movq	%r10, %rdi
	movq	8(%r12), %rdx
	movq	8(%rdx), %rcx
	movq	%rcx, %rsi
	call	_PromoteObj
	movq	%rax, %rcx
	movq	%r14, %rax
	movq	-56(%rbp), %rdi
	movq	-64(%rbp), %r8
	movq	%r15, %r9
	movq	%rbx, %r11
	movq	128(%r13), %rsi
	movq	8(%r12), %r13
	movq	(%rcx), %rax
	movq	40(%r12), %rdx
	lock
	cmpxchgq	%rdx, (%rcx)
	movq	%r11, %rbx
	movq	%rsi, 128(%rbx)
	movq	%rax, %r13
	movq	%rdi, %r14
	movq	%r8, %r15
	movq	%r9, %rdx
	movq	%rdx, -64(%rbp)
	movq	%r11, %r10
	movq	%r10, -56(%rbp)
	movq	%r11, %rcx
	movq	%rcx, %rdi
	movq	8(%r12), %rcx
	movq	(%rcx), %rdx
	movq	%rdx, %rsi
	call	_PromoteObj
	movq	%rax, %r10
	movq	%r13, %rax
	movq	%r14, %rdi
	movq	%r15, %r8
	movq	-64(%rbp), %r9
	movq	-56(%rbp), %r11
	movq	128(%rbx), %rsi
	movq	(%r10), %rdx
	cmpq	$1, %rdx
	jne	L117
S_case10F:
case.110:
	/* Liveout:  GP={%rax %rdi}  */
	/* block case<CCA0> (ep<CC9F>) */
	movq	$20, -8(%rsi)
	movq	24(%r12), %r10
	movq	%r10, (%rsi)
	movq	40(%r12), %r13
	movq	32(%r13), %r14
	movq	%r14, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	40(%r12), %r15
	movq	%rbx, 32(%r15)
	movq	40(%r12), %rcx
	movq	$1, (%rcx)
	movq	32(%r12), %rdx
	movq	(%rdx), %rbx
	movq	$1, %r10
	movq	%r10, %rax
	movq	%rdx, %rdi
	jmp	*%rbx
doGC116:
	movq	$12, -8(%rsi)
	movq	%r12, (%rsi)
	movq	%rsi, %r12
	addq	$16, %rsi
	movq	%r12, %rdi
	movabsq	$retGC114, %r8
	jmp	_ASM_InvokeGC
	/* live= GP={%r12} spilled=  */
retGC114:
	movq	(%rdi), %r12
	jmp	gcTest115
L117:
	cmpq	$3, %rdx
	jne	S_case10F
S_case111:
case.112:
	/* Liveout:  GP={%rdi}  */
	/* block case<CCAD> (ep<CCAC>) */
	movq	16(%r12), %rdi
	jmp	terminate.10A
	.text
letJoinK.119:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest11B
	/* live= GP={%rcx %rdx} spilled=  */
retGC11A:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest11B:
	movq	%r11, %rbx
	movq	448(%rbx), %rbx
	subq	%rsi, %rbx
	jle	doGC11C
check.118:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block check<E211> (ep<CC86>,ite<CC82>) */
	movq	$20, -8(%rsi)
	movq	(%rcx), %r10
	movq	%r10, (%rsi)
	movq	56(%rdx), %r12
	movq	%r12, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	$3853, -8(%rsi)
	movabsq	$letJoinK.113, %r14
	movq	%r14, (%rsi)
	movq	16(%rdx), %r15
	movq	%r15, 8(%rsi)
	movq	24(%rdx), %rcx
	movq	%rcx, 16(%rsi)
	movq	32(%rdx), %rbx
	movq	%rbx, 24(%rsi)
	movq	40(%rdx), %r10
	movq	%r10, 32(%rsi)
	movq	48(%rdx), %r13
	movq	%r13, 40(%rsi)
	movq	%rsi, %r13
	addq	$56, %rsi
	movq	8(%rdx), %r14
	movq	(%r14), %rdi
	movq	%r12, %r8
	movq	%r13, %r9
	jmp	set_D_ite.F1
doGC11C:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	%r15, %rdi
	movabsq	$retGC11A, %r8
	jmp	_ASM_InvokeGC
	.text
dispatch.11E:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest120
	/* live= GP={%rcx %rdx %rbx} spilled=  */
retGC11F:
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest120:
	movq	%r11, %r10
	movq	448(%r10), %r10
	subq	%rsi, %r10
	jle	doGC121
check.11D:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block check<E215> (ep<CC7B>,act<CC77>,k<CC78>) */
	movq	$3, (%r11)
	movq	$12, -8(%rsi)
	movq	24(%rbx), %r13
	movq	%r13, (%rsi)
	movq	%rsi, %r12
	addq	$16, %rsi
	movq	$24337, -8(%rsi)
	movabsq	$letJoinK.119, %r15
	movq	%r15, (%rsi)
	movq	16(%rbx), %r10
	movq	%r10, 8(%rsi)
	movq	24(%rbx), %r13
	movq	%r13, 16(%rsi)
	movq	40(%rbx), %r14
	movq	%r14, 24(%rsi)
	movq	%rdx, 32(%rsi)
	movq	%rcx, 40(%rsi)
	movq	%r11, 48(%rsi)
	movq	%r12, 56(%rsi)
	movq	%rsi, %r14
	addq	$72, %rsi
	movq	8(%rbx), %r15
	movq	(%r15), %rdi
	movq	%r14, %r8
	movq	32(%rbx), %r9
	jmp	get_D_ite.EB
doGC121:
	movq	$28, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%rsi, %rcx
	addq	$32, %rsi
	movq	%rcx, %rdi
	movabsq	$retGC11F, %r8
	jmp	_ASM_InvokeGC
	.text
k.123:
	movq	%rax, %rcx
	movq	%rdi, %rdx
	jmp	gcTest125
	/* live= GP={%rcx %rdx} spilled=  */
retGC124:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest125:
	movq	%r11, %r12
	movq	448(%r12), %rbx
	subq	%rsi, %rbx
	jle	doGC126
check.122:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block check<E218> (ep<CCEC>,x<CCE8>) */
	movq	8(%rdx), %rdi
	movq	16(%rdx), %r8
	movq	24(%rdx), %r9
	jmp	dispatch.11E
doGC126:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	%r10, %rdi
	movabsq	$retGC124, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.128:
	movq	%rdi, %r13
	jmp	gcTest12A
	/* live= spilled= GP={%r~1}  */
retGC129:
	movq	(%rdi), %r13
gcTest12A:
	movq	%r11, %r14
	movq	448(%r14), %r15
	subq	%rsi, %r15
	jle	doGC12B
	movq	%r13, -72(%rbp)
check.127:
	/* Liveout:  GP={%rax %rdi}  */
	/* flushLoads */
	/* block check<E21A> (ep<CCD8>) */
	movq	%r11, %r14
	movq	%rsi, 128(%r14)
	movq	%rax, %rcx
	movq	%rcx, -64(%rbp)
	movq	%rdi, %r15
	movq	%r8, %rbx
	movq	%r9, %r12
	movq	%r11, %r13
	movq	%r11, %rcx
	movq	%rcx, %rdi
	movq	-72(%rbp), %rdx
	movq	8(%rdx), %r10
	movq	16(%r10), %rdx
	movq	%rdx, %rsi
	call	_PromoteObj
	movq	%rax, %r10
	movq	%r10, -56(%rbp)
	movq	-64(%rbp), %rax
	movq	%r15, %rdi
	movq	%rbx, %r8
	movq	%r12, %r9
	movq	%r13, %r11
	movq	128(%r14), %rsi
	movq	%r11, %rbx
	movq	%rsi, 128(%rbx)
	movq	%rax, %r12
	movq	%r12, -64(%rbp)
	movq	%rdi, %r12
	movq	%r8, %r13
	movq	%r9, %r14
	movq	%r11, %r15
	movq	%r11, %rcx
	movq	%rcx, %rdi
	movq	-72(%rbp), %rcx
	movq	8(%rcx), %r10
	movq	24(%r10), %rdx
	movq	%rdx, %rsi
	call	_PromoteObj
	movq	%rax, %r10
	movq	-64(%rbp), %rax
	movq	%r12, %rdi
	movq	%r13, %r8
	movq	%r14, %r9
	movq	%r15, %r11
	movq	128(%rbx), %rsi
	movq	-72(%rbp), %rdx
	movq	8(%rdx), %r14
	movq	-72(%rbp), %rbx
	movq	8(%rbx), %r15
	movq	-56(%rbp), %r12
	movq	%r12, (%r10)
	movq	%r11, %r14
	movq	%rsi, 128(%r14)
	movq	%rax, %r15
	movq	%rdi, %r13
	movq	%r13, -56(%rbp)
	movq	%r8, %rbx
	movq	%r9, %r12
	movq	%r11, %r13
	movq	%r11, %rcx
	movq	%rcx, %rdi
	movq	-72(%rbp), %rcx
	movq	8(%rcx), %r10
	movq	8(%r10), %rdx
	movq	%rdx, %rsi
	call	_PromoteObj
	movq	%rax, %rcx
	movq	%r15, %rax
	movq	-56(%rbp), %rdi
	movq	%rbx, %r8
	movq	%r12, %r9
	movq	%r13, %r11
	movq	128(%r14), %rsi
	movq	-72(%rbp), %rdx
	movq	8(%rdx), %rdx
	movq	(%rcx), %rax
	xorq	%rbx, %rbx
	lock
	cmpxchgq	%rbx, (%rcx)
	movq	$36, -8(%rsi)
	movabsq	$k.123, %r12
	movq	%r12, (%rsi)
	movq	-72(%rbp), %rbx
	movq	16(%rbx), %r13
	movq	%r13, 8(%rsi)
	movq	-72(%rbp), %r10
	movq	24(%r10), %r14
	movq	%r14, 16(%rsi)
	movq	-72(%rbp), %r12
	movq	32(%r12), %r15
	movq	%r15, 24(%rsi)
	movq	%rsi, %r10
	addq	$40, %rsi
	movq	$20, -8(%rsi)
	movq	$1, (%rsi)
	movq	%r10, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	$3, (%r11)
	movq	32(%r11), %rdx
	movq	8(%rdx), %rbx
	movq	%rbx, 32(%r11)
	movq	(%rdx), %r10
	movq	(%r10), %r12
	movq	%rcx, %rax
	movq	%r10, %rdi
	jmp	*%r12
doGC12B:
	movq	$12, -8(%rsi)
	movq	%r13, (%rsi)
	movq	%rsi, %r13
	addq	$16, %rsi
	movq	%r13, %rdi
	movabsq	$retGC129, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.12D:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest12F
	/* live= GP={%rcx %rdx} spilled=  */
retGC12E:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest12F:
	movq	%r11, %rbx
	movq	448(%rbx), %rbx
	subq	%rsi, %rbx
	jle	doGC130
check.12C:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block check<E21D> (ep<CCCE>,ite<CCCA>) */
	movq	$20, -8(%rsi)
	movq	(%rcx), %r10
	movq	%r10, (%rsi)
	movq	48(%rdx), %r12
	movq	%r12, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	$3851, -8(%rsi)
	movabsq	$letJoinK.128, %r14
	movq	%r14, (%rsi)
	movq	16(%rdx), %r15
	movq	%r15, 8(%rsi)
	movq	24(%rdx), %rcx
	movq	%rcx, 16(%rsi)
	movq	32(%rdx), %rbx
	movq	%rbx, 24(%rsi)
	movq	40(%rdx), %r10
	movq	%r10, 32(%rsi)
	movq	%rsi, %r13
	addq	$48, %rsi
	movq	8(%rdx), %r14
	movq	(%r14), %rdi
	movq	%r12, %r8
	movq	%r13, %r9
	jmp	set_D_ite.F1
doGC130:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	%r15, %rdi
	movabsq	$retGC12E, %r8
	jmp	_ASM_InvokeGC
	.text
act.139:
	movq	%rax, %rcx
	movq	%rdi, %rdx
gcTest13B:
	movq	%r11, %r12
	movq	448(%r12), %rbx
	subq	%rsi, %rbx
	jle	doGC13C
check.131:
	/* block check<E220> (ep<CCBA>,s<CCB7>) */
	cmpq	$1, %rcx
	jne	L_true132
else.133:
	/* Liveout:  GP={%rdi}  */
	/* block else<CD09> (ep<CD08>) */
	movq	40(%rdx), %rdi
	jmp	terminate.10A
doGC13C:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	%r10, %rdi
	movabsq	$retGC13A, %r8
	jmp	_ASM_InvokeGC
	/* live= GP={%rcx %rdx} spilled=  */
retGC13A:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
	jmp	gcTest13B
L_true132:
then.134:
	/* block then<CCC0> (ep<CCBE>,s<CCBF>) */
	cmpq	$1, (%rcx)
	jne	L13D
L_true135:
then.137:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block then<CCC6> (ep<CCC4>,s<CCC5>) */
	movq	$16143, -8(%rsi)
	movabsq	$letJoinK.12D, %rbx
	movq	%rbx, (%rsi)
	movq	16(%rdx), %r10
	movq	%r10, 8(%rsi)
	movq	24(%rdx), %r12
	movq	%r12, 16(%rsi)
	movq	48(%rdx), %r13
	movq	%r13, 24(%rsi)
	movq	%rdx, 32(%rsi)
	movq	8(%rcx), %r14
	movq	%r14, 40(%rsi)
	movq	24(%rdx), %r15
	movq	32(%r15), %rcx
	movq	%rcx, 48(%rsi)
	movq	%rsi, %r15
	addq	$64, %rsi
	movq	8(%rdx), %rbx
	movq	(%rbx), %rdi
	movq	%r15, %r8
	movq	32(%rdx), %r9
	jmp	get_D_ite.EB
L13D:
else.136:
	/* Liveout:  GP={%rax %rdi}  */
	/* block else<CD03> (ep<CD02>) */
	movq	$12, -8(%rsi)
	movabsq	$tag138, %r13
	movq	%r13, (%rsi)
	movq	%rsi, %r12
	addq	$16, %rsi
	movq	32(%rdx), %r10
	movq	(%r10), %r14
	movq	%r12, %rax
	movq	%r10, %rdi
	jmp	*%r14
	.text
wrappedK.13F:
	movq	%rax, %rcx
	movq	%rdi, %rdx
	jmp	gcTest141
	/* live= GP={%rcx %rdx} spilled=  */
retGC140:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest141:
	movq	%r11, %r12
	movq	448(%r12), %rbx
	subq	%rsi, %rbx
	jle	doGC142
check.13E:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block check<E223> (ep<CD0D>,x<CD0C>) */
	movq	16(%rdx), %rdi
	movq	24(%rdx), %r8
	movq	8(%rdx), %r9
	jmp	dispatch.11E
doGC142:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	%r10, %rdi
	movabsq	$retGC140, %r8
	jmp	_ASM_InvokeGC
	.text
wrap.144:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest146
	/* live= GP={%r12 %r10 %rcx %rdx %rbx} spilled=  */
retGC145:
	movq	32(%rdi), %r12
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest146:
	movq	%r11, %r13
	movq	448(%r13), %r13
	subq	%rsi, %r13
	jle	doGC147
check.143:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<E229> (ep<CC40>,c<CC41>,k<CC42>,retK<CC43>,exh<CC44>) */
	movq	$3851, -8(%rsi)
	movabsq	$terminate.10A, %r14
	movq	%r14, (%rsi)
	movq	(%rbx), %r15
	movq	%r15, 8(%rsi)
	movq	8(%rbx), %r13
	movq	%r13, 16(%rsi)
	movq	%rdx, 24(%rsi)
	movq	%r12, 32(%rsi)
	movq	%rsi, %r13
	addq	$48, %rsi
	movq	$7949, -8(%rsi)
	movabsq	$dispatch.11E, %r14
	movq	%r14, (%rsi)
	movq	(%rbx), %r15
	movq	%r15, 8(%rsi)
	movq	8(%rbx), %r14
	movq	%r14, 16(%rsi)
	movq	%rdx, 24(%rsi)
	movq	%r12, 32(%rsi)
	movq	%r13, 40(%rsi)
	movq	%rsi, %r14
	addq	$56, %rsi
	movq	$60, -8(%rsi)
	movabsq	$act.139, %r15
	movq	%r15, (%rsi)
	movq	(%rbx), %r15
	movq	%r15, 8(%rsi)
	movq	8(%rbx), %rbx
	movq	%rbx, 16(%rsi)
	movq	%rdx, 24(%rsi)
	movq	%r12, 32(%rsi)
	movq	%r13, 40(%rsi)
	movq	%r14, 48(%rsi)
	movq	%rsi, %r15
	addq	$64, %rsi
	movq	$36, -8(%rsi)
	movabsq	$wrappedK.13F, %r13
	movq	%r13, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%r14, 16(%rsi)
	movq	%r15, 24(%rsi)
	movq	%rsi, %r12
	addq	$40, %rsi
	movq	(%r10), %r14
	movq	%r10, %rdi
	movq	%r12, %r8
	jmp	*%r14
doGC147:
	movq	$44, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%r12, 32(%rsi)
	movq	%rsi, %r15
	addq	$48, %rsi
	movq	%r15, %rdi
	movabsq	$retGC145, %r8
	jmp	_ASM_InvokeGC
	.text
k.149:
	movq	%rax, %rcx
	movq	%rdi, %rdx
	jmp	gcTest14B
	/* live= GP={%rcx %rdx} spilled=  */
retGC14A:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest14B:
	movq	%r11, %r14
	movq	448(%r14), %rbx
	subq	%rsi, %rbx
	jle	doGC14C
check.148:
	/* Liveout:  GP={%rdi}  */
	/* block check<E22C> (ep<CD1E>,_wild<CD1D>) */
	movq	8(%rdx), %r10
	movq	(%r10), %r12
	movq	%r10, %rdi
	jmp	*%r12
doGC14C:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	%r13, %rdi
	movabsq	$retGC14A, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.14E:
	movq	%rdi, %rcx
	jmp	gcTest150
	/* live= GP={%rcx} spilled=  */
retGC14F:
	movq	(%rdi), %rcx
gcTest150:
	movq	%r11, %rdx
	movq	448(%rdx), %rdx
	subq	%rsi, %rdx
	jle	doGC151
check.14D:
	/* Liveout:  GP={%rax %rdi}  */
	/* block check<E22E> (ep<CD2A>) */
	movq	$3, (%r11)
	movq	32(%r11), %r10
	movq	8(%r10), %r12
	movq	%r12, 32(%r11)
	movq	(%r10), %rbx
	movq	(%rbx), %r13
	movq	$1, %r14
	movq	%r14, %rax
	movq	%rbx, %rdi
	jmp	*%r13
doGC151:
	movq	$12, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%rsi, %r15
	addq	$16, %rsi
	movq	%r15, %rdi
	movabsq	$retGC14F, %r8
	jmp	_ASM_InvokeGC
	.text
k.153:
	movq	%rax, %rcx
	movq	%rdi, %rdx
	jmp	gcTest155
	/* live= GP={%rcx %rdx} spilled=  */
retGC154:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest155:
	movq	%r11, %r12
	movq	448(%r12), %rbx
	subq	%rsi, %rbx
	jle	doGC156
check.152:
	/* Liveout:  GP={%rdi}  */
	/* block check<E231> (ep<CD40>,x<CD3F>) */
	movq	8(%rdx), %rdi
	jmp	letJoinK.14E
doGC156:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	%r10, %rdi
	movabsq	$retGC154, %r8
	jmp	_ASM_InvokeGC
	.text
migrate_D_to_D_top_D_level_D_sched.15C:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
gcTest15E:
	movq	%r11, %r10
	movq	448(%r10), %r10
	subq	%rsi, %r10
	jle	doGC15F
check.157:
	/* block check<E235> (ep<CD17>,retK<CD18>,exh<CD19>) */
	movq	$20, -8(%rsi)
	movabsq	$k.149, %rbx
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	movq	$3, (%r11)
	movq	$28, -8(%rsi)
	movq	24(%r11), %r12
	movq	%r12, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	88(%r11), %r13
	movq	%r13, 16(%rsi)
	movq	%rsi, %r10
	addq	$32, %rsi
	movq	%r10, 88(%r11)
	movq	$10, -8(%rsi)
	movabsq	$letJoinK.14E, %r14
	movq	%r14, (%rsi)
	movq	%rsi, %r14
	addq	$16, %rsi
	movq	8(%r11), %r15
	cmpq	$1, %r15
	jne	L160
S_case158:
	movq	%r11, %rcx
case.159:
	/* Liveout:  GP={%rdi}  */
	/* block case<CD37> (vp<CD36>,letJoinK<CD35>) */
	movq	$1, (%rcx)
	movq	%r14, %rdi
	jmp	letJoinK.14E
doGC15F:
	movq	$28, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%rsi, %r15
	addq	$32, %rsi
	movq	%r15, %rdi
	movabsq	$retGC15D, %r8
	jmp	_ASM_InvokeGC
	/* live= GP={%rcx %rdx %rbx} spilled=  */
retGC15D:
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
	jmp	gcTest15E
L160:
	cmpq	$3, %r15
	jne	S_case158
S_case15A:
	movq	%r11, %r15
case.15B:
	/* Liveout:  GP={%rax %rdi}  */
	/* block case<CD3D> (vp<CD3C>,letJoinK<CD3B>) */
	movq	$1, 8(%r15)
	movq	$20, -8(%rsi)
	movabsq	$k.153, %rdx
	movq	%rdx, (%rsi)
	movq	%r14, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movq	$1, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	32(%r15), %rbx
	movq	8(%rbx), %r10
	movq	%r10, 32(%r15)
	movq	(%rbx), %r12
	movq	(%r12), %r14
	movq	%r13, %rax
	movq	%r12, %rdi
	jmp	*%r14
	.text
current_D_work_D_group.169:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
gcTest16B:
	movq	%r11, %r12
	movq	448(%r12), %r12
	subq	%rsi, %r12
	jle	doGC16C
check.161:
	/* block check<E23A> (ep<CD4D>,_wild<CD4E>,retK<CD4F>,exh<CD50>) */
	movq	24(%r11), %rbx
	movq	8(%rbx), %rdx
	cmpq	$1, %rdx
	jne	L_true162
else.163:
	/* Liveout:  GP={%rax %rdi}  */
	/* block else<CD76> (exh<CD75>) */
	movq	$133, -8(%rsi)
	movabsq	$strE9, %r13
	movq	%r13, (%rsi)
	movl	$51, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movabsq	$tagEA, %r14
	movq	%r14, (%rsi)
	movq	%r12, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	(%r10), %rcx
	movq	%r15, %rax
	movq	%r10, %rdi
	jmp	*%rcx
doGC16C:
	movq	$36, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %rbx
	addq	$40, %rsi
	movq	%rbx, %rdi
	movabsq	$retGC16A, %r8
	jmp	_ASM_InvokeGC
	/* live= GP={%r10 %rcx %rdx %rbx} spilled=  */
retGC16A:
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
	jmp	gcTest16B
L_true162:
then.164:
	/* block then<CD5C> (retK<CD5B>,exh<CD5A>,_t<CD59>) */
	movq	(%rdx), %rdx
	movq	(%rdx), %r14
	cmpq	$1, %r14
	je	L16D
L_true165:
then.167:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block then<CD64> (retK<CD63>,stk<CD62>) */
	movq	(%r14), %r15
	movq	(%rcx), %rdx
	movq	%rcx, %rdi
	movq	8(%r15), %r8
	movq	24(%r15), %r9
	movq	32(%r15), %r10
	jmp	*%rdx
L16D:
else.166:
	/* Liveout:  GP={%rax %rdi}  */
	/* block else<CD6D> (exh<CD6C>) */
	movq	$133, -8(%rsi)
	movabsq	$str168, %r12
	movq	%r12, (%rsi)
	movl	$58, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movabsq	$tagEA, %r13
	movq	%r13, (%rsi)
	movq	%rbx, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	(%r10), %r14
	movq	%r13, %rax
	movq	%r10, %rdi
	jmp	*%r14
	.text
	/* live= GP={%r10 %rdx %r14 %r12 %rcx} spilled=  */
retGC174:
	movq	32(%rdi), %r10
	movq	24(%rdi), %rdx
	movq	16(%rdi), %r14
	movq	8(%rdi), %r15
	movq	(%rdi), %rcx
	jmp	gcTest175
L17B:
	movq	%r15, %r12
check.16E:
	/* block check<E240> (ep<CDA5>,retK<CDA9>,l<CDA8>,trueList<CDA7>,falseList<CDA6>) */
	movq	(%r14), %r13
	movq	8(%r14), %rbx
	movq	16(%r13), %r14
	movq	(%r14), %r15
	cmpq	(%rcx), %r15
	jl	L_true16F
else.170:
	/* Liveout:  GP={%r12 %r10 %r9 %r8 %rdi}  */
	/* block else<CDC1> (ep<CDBB>,retK<CDC0>,trueList<CDBF>,falseList<CDBE>,_anon_<CDBD>,_anon_<CDBC>) */
	movq	$20, -8(%rsi)
	movq	%r13, (%rsi)
	movq	%r10, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	%rcx, %rdi
	movq	%rbx, %r8
	movq	%rdx, %r9
loop.172:
	movq	%r12, %r15
	movq	%r9, %rdx
	movq	%r8, %r14
	movq	%rdi, %rcx
	cmpq	$1, %r14
	je	L17A
L_true177:
then.173:
	/* block then<CDAA> (ep<E23B>,retK<E23C>,l<E23D>,trueList<E23E>,falseList<E23F>) */
gcTest175:
	movq	%r11, %r12
	movq	448(%r12), %rbx
	subq	%rsi, %rbx
	jg	L17B
doGC176:
	movq	$44, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%r15, 8(%rsi)
	movq	%r14, 16(%rsi)
	movq	%rdx, 24(%rsi)
	movq	%r10, 32(%rsi)
	movq	%rsi, %rbx
	addq	$48, %rsi
	movq	%rbx, %rdi
	movabsq	$retGC174, %r8
	jmp	_ASM_InvokeGC
L17A:
else.178:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block else<CDC8> (retK<CDC7>,trueList<CDC6>,falseList<CDC5>) */
	movq	$775, -8(%rsi)
	movabsq	$letJoinK.6, %r13
	movq	%r13, (%rsi)
	movq	%r15, 8(%rsi)
	movq	%r10, 16(%rsi)
	movq	%rsi, %r10
	addq	$32, %rsi
	movq	$18, -8(%rsi)
	movq	$1, (%rsi)
	movabsq	$lp.179, %r15
	movq	%r15, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	(%r14), %rdi
	movq	%rdx, %r8
	movq	$1, %r9
	jmp	lp.179
L_true16F:
then.171:
	/* Liveout:  GP={%r12 %r10 %r9 %r8 %rdi}  */
	/* block then<CDB8> (ep<CDB2>,retK<CDB7>,trueList<CDB6>,falseList<CDB5>,_anon_<CDB4>,_anon_<CDB3>) */
	movq	$20, -8(%rsi)
	movq	%r13, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	movq	%rcx, %rdi
	movq	%rbx, %r8
	movq	%rdx, %r9
	jmp	loop.172
	.text
lp.8:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest181
	/* live= GP={%r10 %rcx %rdx %rbx} spilled=  */
retGC180:
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest181:
	movq	%r11, %r12
	movq	448(%r12), %r12
	subq	%rsi, %r12
	jle	doGC182
check.17C:
	/* block check<E245> (ep<CDD8>,ls<CDD9>,acc<CDDA>,retK<CDDB>) */
	cmpq	$1, %rdx
	je	L183
L_true17D:
then.17F:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block then<CDE4> (ep<CDE0>,retK<CDE3>,ls<CDE2>,acc<CDE1>) */
	movq	$20, -8(%rsi)
	movq	(%rdx), %r14
	movq	%r14, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	%rbx, %rdi
	movq	8(%rdx), %r8
	movq	%r13, %r9
	jmp	lp.8
doGC182:
	movq	$36, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %r15
	addq	$40, %rsi
	movq	%r15, %rdi
	movabsq	$retGC180, %r8
	jmp	_ASM_InvokeGC
L183:
else.17E:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block else<CDED> (retK<CDEC>,acc<CDEB>) */
	movq	%r10, %rdi
	movq	%rcx, %r8
	jmp	letJoinK.7
	.text
lp.179:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest189
	/* live= GP={%r10 %rcx %rdx %rbx} spilled=  */
retGC188:
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest189:
	movq	%r11, %r12
	movq	448(%r12), %r12
	subq	%rsi, %r12
	jle	doGC18A
check.184:
	/* block check<E24A> (ep<CDF5>,ls<CDF6>,acc<CDF7>,retK<CDF8>) */
	cmpq	$1, %rdx
	je	L18B
L_true185:
then.187:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block then<CE01> (ep<CDFD>,retK<CE00>,ls<CDFF>,acc<CDFE>) */
	movq	$20, -8(%rsi)
	movq	(%rdx), %r14
	movq	%r14, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	%rbx, %rdi
	movq	8(%rdx), %r8
	movq	%r13, %r9
	jmp	lp.179
doGC18A:
	movq	$36, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %r15
	addq	$40, %rsi
	movq	%r15, %rdi
	movabsq	$retGC188, %r8
	jmp	_ASM_InvokeGC
L18B:
else.186:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block else<CE0A> (retK<CE09>,acc<CE08>) */
	movq	%r10, %rdi
	movq	%rcx, %r8
	jmp	letJoinK.6
	.text
f1.A:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest18E
	/* live= GP={%r10 %rcx %rdx %rbx} spilled=  */
retGC18D:
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest18E:
	movq	%r11, %r14
	movq	448(%r14), %r12
	subq	%rsi, %r12
	jle	doGC18F
check.18C:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<E24F> (ep<CE1D>,x<CE1E>,retK<CE1F>,exh<CE20>) */
	movq	$28, -8(%rsi)
	movq	(%rdx), %r14
	movq	%r14, (%rsi)
	movq	8(%rdx), %r15
	movq	%r15, 8(%rsi)
	movq	(%rbx), %rdx
	movq	88(%rdx), %r10
	movq	%r10, 16(%rsi)
	movq	%rsi, %r13
	addq	$32, %rsi
	movq	(%rbx), %r12
	movq	%r13, 88(%r12)
	movq	%rcx, %rdi
	movq	$1, %r8
	jmp	letJoinK.B4
doGC18F:
	movq	$36, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %r13
	addq	$40, %rsi
	movq	%r13, %rdi
	movabsq	$retGC18D, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.B:
	movq	%r8, %r14
	movq	%rdi, %r13
	jmp	gcTest195
	/* live= GP={%r14 %r13} spilled=  */
retGC194:
	movq	8(%rdi), %r14
	movq	(%rdi), %r13
gcTest195:
	movq	%r11, %rcx
	movq	448(%rcx), %rdx
	subq	%rsi, %rdx
	jle	doGC196
check.190:
	/* flushLoads */
	/* block check<E252> (ep<CE30>,_wild<CE2D>) */
	movq	%r11, %r15
	movq	%rsi, 128(%r15)
	movq	%rax, %rbx
	movq	%rdi, %r10
	movq	%r10, -56(%rbp)
	movq	%r8, %r12
	movq	%r9, %r14
	movq	%r11, %rcx
	movq	%rcx, -64(%rbp)
	movq	%r11, %rcx
	movq	%rcx, %rdi
	movq	24(%r13), %r10
	movq	8(%r10), %rdx
	movq	%rdx, %rsi
	call	_PromoteObj
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	-56(%rbp), %rdi
	movq	%r12, %r8
	movq	%r14, %r9
	movq	-64(%rbp), %r11
	movq	128(%r15), %rsi
	movq	24(%r13), %rdx
	movq	8(%r13), %r14
	movq	%rcx, (%r14)
	movq	24(%r13), %rbx
	cmpq	$1, (%rbx)
	je	L197
L_true191:
then.193:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block then<CE3B> (ep<CE3A>) */
	movq	16(%r13), %r13
	movq	(%r13), %r14
	movq	%r13, %rdi
	movq	$3, %r8
	jmp	*%r14
doGC196:
	movq	$20, -8(%rsi)
	movq	%r13, (%rsi)
	movq	%r14, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	%r15, %rdi
	movabsq	$retGC194, %r8
	jmp	_ASM_InvokeGC
L197:
else.192:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block else<CE40> (ep<CE3F>) */
	movq	16(%r13), %r10
	movq	(%r10), %r12
	movq	%r10, %rdi
	movq	$1, %r8
	jmp	*%r12
	.text
wakeupSleepingThreads.19A:
	movq	%r8, %r14
	movq	%rdi, %r13
	jmp	gcTest19C
	/* live= GP={%r13} spilled= GP={%r~1}  */
retGC19B:
	movq	8(%rdi), %r14
	movq	(%rdi), %r13
gcTest19C:
	movq	%r11, %r15
	movq	448(%r15), %rcx
	subq	%rsi, %rcx
	jle	doGC19D
	movq	%r14, -56(%rbp)
check.198:
	/* Liveout:  GP={%r12 %r10 %r9 %r8 %rdi}  */
	/* flushLoads */
	/* block check<E255> (ep<CD90>,retK<CD91>) */
	movq	%rax, %rdx
	movq	%rdx, -72(%rbp)
	movq	%rdi, %r15
	movq	%r8, %rbx
	movq	%r9, %r12
	movq	%rsi, %r14
	movq	%r11, %r10
	movq	%r10, -64(%rbp)
	call	_M_GetTime
	movq	%rax, %rcx
	movq	-72(%rbp), %rax
	movq	%r15, %rdi
	movq	%rbx, %r8
	movq	%r12, %r9
	movq	%r14, %rsi
	movq	-64(%rbp), %r11
	movq	24(%r13), %rbx
	movq	%rcx, (%rbx)
	movq	$10, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%rsi, %r10
	addq	$16, %rsi
	movq	$133, -8(%rsi)
	movq	%r10, (%rsi)
	movabsq	$loop.172, %r12
	movq	%r12, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	$7437, -8(%rsi)
	movabsq	$letJoinK.9, %r14
	movq	%r14, (%rsi)
	movq	(%r13), %r15
	movq	%r15, 8(%rsi)
	movq	8(%r13), %rcx
	movq	%rcx, 16(%rsi)
	movq	16(%r13), %rdx
	movq	%rdx, 24(%rsi)
	movq	32(%r13), %rbx
	movq	%rbx, 32(%rsi)
	movq	-56(%rbp), %r12
	movq	%r12, 40(%rsi)
	movq	%rsi, %r12
	addq	$56, %rsi
	movq	(%r10), %rdi
	movq	32(%r13), %r10
	movq	(%r10), %r8
	movq	$1, %r9
	movq	$1, %r10
	jmp	loop.172
doGC19D:
	movq	$20, -8(%rsi)
	movq	%r13, (%rsi)
	movq	%r14, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	%r12, %rdi
	movabsq	$retGC19B, %r8
	jmp	_ASM_InvokeGC
	.text
workIsAvailable.19F:
	movq	%rdi, %rcx
	jmp	gcTest1A1
	/* live= GP={%rcx} spilled=  */
retGC1A0:
	movq	(%rdi), %rcx
gcTest1A1:
	movq	%r11, %r10
	movq	448(%r10), %rdx
	subq	%rsi, %rdx
	jle	doGC1A2
check.19E:
	/* Liveout:  GP={%rdi}  */
	/* block check<E257> (ep<CE51>) */
	movq	8(%rcx), %rdi
	jmp	letJoinK.38
doGC1A2:
	movq	$12, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%rsi, %rbx
	addq	$16, %rsi
	movq	%rbx, %rdi
	movabsq	$retGC1A0, %r8
	jmp	_ASM_InvokeGC
	.text
lp2.1A7:
	movq	%r9, %rdx
	movq	%r8, %rcx
	movq	%rdi, %rbx
	jmp	gcTest1A9
	/* live= GP={%rdx %rcx %rbx} spilled=  */
retGC1A8:
	movq	16(%rdi), %rdx
	movl	8(%rdi), %ecx
	movq	(%rdi), %rbx
gcTest1A9:
	movq	%r11, %r13
	movq	448(%r13), %r10
	subq	%rsi, %r10
	jle	doGC1AA
check.1A3:
	/* block check<E25B> (ep<CE79>,j<CE7A>,retK<CE7B>) */
	cmpl	$500, %ecx
	jle	L1AB
L_true1A4:
then.1A6:
	/* Liveout:  GP={%rdi}  */
	/* block then<CE82> (retK<CE81>) */
	movq	%rdx, %rdi
	jmp	letJoinK.2C
doGC1AA:
	movq	$519, -8(%rsi)
	movq	%rbx, (%rsi)
	movl	%ecx, 8(%rsi)
	movq	%rdx, 16(%rsi)
	movq	%rsi, %r12
	addq	$32, %rsi
	movq	%r12, %rdi
	movabsq	$retGC1A8, %r8
	jmp	_ASM_InvokeGC
L1AB:
else.1A5:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block else<CE87> (ep<CE84>,j<CE86>,retK<CE85>) */
	movq	%rbx, %rdi
	movq	%rcx, %r8
	incl	%r8d
	movq	%rdx, %r9
	jmp	lp2.1A7
	.text
letJoinK.34:
	movq	%r8, %rcx
	movq	%rdi, %rdx
gcTest1B2:
	movq	%r11, %r10
	movq	448(%r10), %rbx
	subq	%rsi, %rbx
	jle	doGC1B3
check.1AC:
	/* block check<E25E> (ep<CE72>,w<CE6D>) */
	cmpq	$1, %rcx
	jne	L1B4
S_case1AD:
case.1AE:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block case<CE76> (ep<CE75>) */
	pause
	movq	$18, -8(%rsi)
	movq	$1, (%rsi)
	movabsq	$lp2.1A7, %r12
	movq	%r12, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	$1289, -8(%rsi)
	movabsq	$letJoinK.2C, %r14
	movq	%r14, (%rsi)
	movq	16(%rdx), %r15
	movq	%r15, 8(%rsi)
	movl	24(%rdx), %ecx
	movl	%ecx, 16(%rsi)
	movq	32(%rdx), %rdx
	movq	%rdx, 24(%rsi)
	movq	%rsi, %r13
	addq	$40, %rsi
	movq	(%r10), %rdi
	xorl	%r8d, %r8d
	movq	%r13, %r9
	jmp	lp2.1A7
doGC1B3:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	%rbx, %rdi
	movabsq	$retGC1B1, %r8
	jmp	_ASM_InvokeGC
	/* live= GP={%rcx %rdx} spilled=  */
retGC1B1:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
	jmp	gcTest1B2
L1B4:
	cmpq	$3, %rcx
	jne	S_case1AD
S_case1AF:
case.1B0:
	/* Liveout:  GP={%rdi}  */
	/* block case<CEA4> (ep<CEA3>) */
	movq	8(%rdx), %rdi
	jmp	workIsAvailable.19F
	.text
letJoinK.1B6:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest1B8
	/* live= GP={%rcx %rdx} spilled=  */
retGC1B7:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest1B8:
	movq	%r11, %r10
	movq	448(%r10), %rbx
	subq	%rsi, %rbx
	jle	doGC1B9
check.1B5:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<E261> (ep<CEC7>,rest<CEC6>) */
	movq	$28, -8(%rsi)
	movq	8(%rdx), %r12
	movq	(%r12), %r13
	movq	%r13, (%rsi)
	movq	8(%rdx), %r14
	movq	8(%r14), %r15
	movq	%r15, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%rsi, %r10
	addq	$32, %rsi
	movq	16(%rdx), %rcx
	movq	(%rcx), %rdx
	movq	%rcx, %rdi
	movq	%r10, %r8
	jmp	*%rdx
doGC1B9:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	%rbx, %rdi
	movabsq	$retGC1B7, %r8
	jmp	_ASM_InvokeGC
	.text
append.31:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest1BF
	/* live= GP={%rcx %rdx %rbx} spilled=  */
retGC1BE:
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest1BF:
	movq	%r11, %r10
	movq	448(%r10), %r10
	subq	%rsi, %r10
	jle	doGC1C0
check.1BA:
	/* block check<E265> (ep<CEB4>,queue1<CEB5>,retK<CEB6>) */
	cmpq	$1, %rdx
	jne	L1C1
L_true1BB:
then.1BD:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block then<CEBE> (ep<CEBC>,retK<CEBD>) */
	movq	(%rcx), %r14
	movq	%rcx, %rdi
	movq	(%rbx), %r8
	jmp	*%r14
doGC1C0:
	movq	$28, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%rsi, %r15
	addq	$32, %rsi
	movq	%r15, %rdi
	movabsq	$retGC1BE, %r8
	jmp	_ASM_InvokeGC
L1C1:
else.1BC:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block else<CEC4> (ep<CEC1>,queue1<CEC3>,retK<CEC2>) */
	movq	$775, -8(%rsi)
	movabsq	$letJoinK.1B6, %r13
	movq	%r13, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%rsi, %r12
	addq	$32, %rsi
	movq	%rbx, %rdi
	movq	16(%rdx), %r8
	movq	%r12, %r9
	jmp	append.31
	.text
letJoinK.32:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest1C4
	/* live= GP={%rcx %rdx} spilled=  */
retGC1C3:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest1C4:
	movq	%r11, %r13
	movq	448(%r13), %rbx
	subq	%rsi, %rbx
	jle	doGC1C5
check.1C2:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<E268> (ep<CED5>,newHd<CED3>) */
	movq	8(%rdx), %r10
	movq	%rcx, 80(%r10)
	movq	16(%rdx), %rdi
	movq	$3, %r8
	jmp	letJoinK.34
doGC1C5:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	%r12, %rdi
	movabsq	$retGC1C3, %r8
	jmp	_ASM_InvokeGC
	.text
retGC1C9:
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest1CA:
	movq	%r11, %r13
	movq	448(%r13), %r10
	subq	%rsi, %r10
	jle	doGC1CB
check.1C6:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<E26B> (ep<CEFC>,retK<CEFD>) */
	pause
	movq	%rbx, %rdi
	movq	%rdx, %r8
lp.1C7:
	movq	%r8, %rdx
	movq	%rdi, %rbx
	movq	(%rbx), %r15
	movq	384(%r15), %r14
	movq	(%rbx), %r10
	leaq	384(%r10), %rcx
	movq	%r14, %rax
	movq	$1, %r12
	movq	(%rbx), %r13
	lock
	cmpxchgq	%r12, 384(%r13)
	movq	%rax, %rcx
	cmpq	%rcx, %r14
	jne	L1CE
L_true1CC:
then.1CD:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block then<CEFA> (retK<CEF9>,x<CEF8>) */
	movq	%rdx, %rdi
	movq	%rcx, %r8
	jmp	letJoinK.33
L1CE:
else.1C8:
	/* block else<CEFE> (ep<E269>,retK<E26A>) */
	jmp	gcTest1CA
doGC1CB:
	movq	$20, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	%r12, %rdi
	movabsq	$retGC1C9, %r8
	jmp	_ASM_InvokeGC
	/* live= GP={%rdx %rbx} spilled=  */
	jmp	retGC1C9
	.text
lp1.2B:
	movq	%r9, %rdx
	movq	%r8, %rcx
	movq	%rdi, %rbx
	jmp	gcTest1D4
	/* live= GP={%rdx %rcx %rbx} spilled=  */
retGC1D3:
	movq	16(%rdi), %rdx
	movl	8(%rdi), %ecx
	movq	(%rdi), %rbx
gcTest1D4:
	movq	%r11, %r10
	movq	448(%r10), %r10
	subq	%rsi, %r10
	jle	doGC1D5
check.1CF:
	/* block check<E26F> (ep<CE59>,i<CE5A>,retK<CE5B>) */
	movq	$133, -8(%rsi)
	movq	%rbx, (%rsi)
	movabsq	$lp1.2B, %r14
	movq	%r14, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	$5645, -8(%rsi)
	movabsq	$letJoinK.33, %r15
	movq	%r15, (%rsi)
	movq	(%rbx), %r10
	movq	%r10, 8(%rsi)
	movq	8(%rbx), %r12
	movq	%r12, 16(%rsi)
	movq	%r13, 24(%rsi)
	movl	%ecx, 32(%rsi)
	movq	%rdx, 40(%rsi)
	movq	%rsi, %r12
	addq	$56, %rsi
	movq	(%rbx), %r13
	cmpq	$1, 384(%r13)
	jne	L1D6
L_true1D0:
then.1D2:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block then<CEE0> (letJoinK<CEDF>) */
	movq	%r12, %rdi
	movq	$1, %r8
	jmp	letJoinK.33
doGC1D5:
	movq	$647, -8(%rsi)
	movq	%rbx, (%rsi)
	movl	%ecx, 8(%rsi)
	movq	%rdx, 16(%rsi)
	movq	%rsi, %rbx
	addq	$32, %rsi
	movq	%rbx, %rdi
	movabsq	$retGC1D3, %r8
	jmp	_ASM_InvokeGC
L1D6:
else.1D1:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block else<CEE6> (ep<CEE4>,letJoinK<CEE5>) */
	movq	$10, -8(%rsi)
	movq	(%rbx), %r15
	movq	%r15, (%rsi)
	movq	%rsi, %r14
	addq	$16, %rsi
	movq	$133, -8(%rsi)
	movq	%r14, (%rsi)
	movabsq	$lp.1C7, %rdx
	movq	%rdx, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	(%rcx), %rdi
	movq	%r12, %r8
	jmp	lp.1C7
	.text
wakeupK.1D8:
	movq	%rax, %rcx
	movq	%rdi, %rdx
	jmp	gcTest1DA
	/* live= GP={%rcx %rdx} spilled=  */
retGC1D9:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest1DA:
	movq	%r11, %r13
	movq	448(%r13), %rbx
	subq	%rsi, %rbx
	jle	doGC1DB
check.1D7:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<E272> (ep<CF18>,b<CF15>) */
	movq	8(%rdx), %r10
	movq	(%r10), %rdi
	movq	16(%rdx), %r8
	jmp	waitForWork.39
doGC1DB:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	%r12, %rdi
	movabsq	$retGC1D9, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.1E2:
	movq	%r8, %rcx
	movq	%rdi, %r10
gcTest1E4:
	movq	%r11, %r12
	movq	448(%r12), %r13
	subq	%rsi, %r13
	jle	doGC1E5
check.1DC:
	/* block check<E275> (ep<CF0F>,w<CF0A>) */
	cmpq	$1, %rcx
	jne	L1E6
S_case1DD:
case.1DE:
	/* Liveout:  GP={%rax %rdi}  */
	/* block case<CF13> (ep<CF12>) */
	movq	$28, -8(%rsi)
	movabsq	$wakeupK.1D8, %r12
	movq	%r12, (%rsi)
	movq	16(%r10), %r13
	movq	%r13, 8(%rsi)
	movq	24(%r10), %r14
	movq	%r14, 16(%rsi)
	movq	%rsi, %rdx
	addq	$32, %rsi
	movq	8(%r10), %r15
	movq	%rdx, 56(%r15)
	movq	%r11, %r14
	movq	%r14, -56(%rbp)
	movq	%rsi, 128(%r14)
	movq	%rax, %r15
	movq	%rdi, %r14
	movq	%r8, %r13
	movq	%r9, %r12
	movq	%r11, %rbx
	movq	8(%r10), %rcx
	movq	%rcx, %rdi
	call	_SleepCont
	movq	%rax, %rcx
	movq	%r15, %rax
	movq	%r14, %rdi
	movq	%r13, %r8
	movq	%r12, %r9
	movq	%rbx, %r11
	movq	-56(%rbp), %rdx
	movq	128(%rdx), %rsi
	movq	$10, -8(%rsi)
	movq	$1000000, (%rsi)
	movq	%rsi, %rdx
	addq	$16, %rsi
	movq	(%rcx), %rbx
	movq	%rdx, %rax
	movq	%rcx, %rdi
	jmp	*%rbx
doGC1E5:
	movq	$20, -8(%rsi)
	movq	%r10, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	%r10, %rdi
	movabsq	$retGC1E3, %r8
	jmp	_ASM_InvokeGC
	/* live= GP={%rcx %r10} spilled=  */
retGC1E3:
	movq	8(%rdi), %rcx
	movq	(%rdi), %r10
	jmp	gcTest1E4
L1E6:
	cmpq	$3, %rcx
	jne	S_case1DD
S_case1DF:
case.1E0:
	/* Liveout:  GP={%rdi}  */
	/* block case<CF25> (ep<CF24>) */
	movq	32(%r10), %rdi
	jmp	workIsAvailable.19F
	.text
letJoinK.2F:
	movq	%rdi, %rcx
	jmp	gcTest1E9
	/* live= GP={%rcx} spilled=  */
retGC1E8:
	movq	(%rdi), %rcx
gcTest1E9:
	movq	%r11, %rbx
	movq	448(%rbx), %rdx
	subq	%rsi, %rdx
	jle	doGC1EA
check.1E7:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<E277> (ep<CF07>) */
	movq	$3595, -8(%rsi)
	movabsq	$letJoinK.1E2, %r10
	movq	%r10, (%rsi)
	movq	8(%rcx), %r12
	movq	%r12, 8(%rsi)
	movq	24(%rcx), %r13
	movq	%r13, 16(%rsi)
	movq	32(%rcx), %r14
	movq	%r14, 24(%rsi)
	movq	40(%rcx), %r15
	movq	%r15, 32(%rsi)
	movq	%rsi, %rbx
	addq	$48, %rsi
	movq	16(%rcx), %rcx
	movq	(%rcx), %rdi
	movq	%rbx, %r8
	jmp	wakeupSleepingThreads.19A
doGC1EA:
	movq	$12, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%rsi, %rdx
	addq	$16, %rsi
	movq	%rdx, %rdi
	movabsq	$retGC1E8, %r8
	jmp	_ASM_InvokeGC
	.text
waitForWork.39:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest1ED
	/* live= GP={%rcx %rdx} spilled=  */
retGC1EC:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest1ED:
	movq	%r11, %r10
	movq	448(%r10), %rbx
	subq	%rsi, %rbx
	jle	doGC1EE
check.1EB:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block check<E27A> (ep<CE4C>,retK<CE4D>) */
	movq	$261, -8(%rsi)
	movabsq	$workIsAvailable.19F, %r10
	movq	%r10, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	$261, -8(%rsi)
	movq	(%rdx), %r13
	movq	%r13, (%rsi)
	movq	%r10, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	$133, -8(%rsi)
	movq	%r12, (%rsi)
	movabsq	$lp1.2B, %r14
	movq	%r14, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	$133, -8(%rsi)
	movq	%rdx, (%rsi)
	movabsq	$waitForWork.39, %rbx
	movq	%rbx, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	$7693, -8(%rsi)
	movabsq	$letJoinK.2F, %r12
	movq	%r12, (%rsi)
	movq	(%rdx), %r14
	movq	%r14, 8(%rsi)
	movq	8(%rdx), %rdx
	movq	%rdx, 16(%rsi)
	movq	%r15, 24(%rsi)
	movq	%rcx, 32(%rsi)
	movq	%r10, 40(%rsi)
	movq	%rsi, %r10
	addq	$56, %rsi
	movq	(%r13), %rdi
	xorl	%r8d, %r8d
	movq	%r10, %r9
	jmp	lp1.2B
doGC1EE:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	%rbx, %rdi
	movabsq	$retGC1EC, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.38:
	movq	%rdi, %rcx
	jmp	gcTest1F2
	/* live= GP={%rcx} spilled=  */
retGC1F1:
	movq	(%rdi), %rcx
gcTest1F2:
	movq	%r11, %r10
	movq	448(%r10), %rdx
	subq	%rsi, %rdx
	jle	doGC1F3
check.1EF:
	/* Liveout:  GP={%rdi}  */
	/* block check<E27C> (ep<CF57>) */
	movq	8(%rcx), %rdi
	jmp	dispatch.1F0
doGC1F3:
	movq	$12, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%rsi, %rbx
	addq	$16, %rsi
	movq	%rbx, %rdi
	movabsq	$retGC1F1, %r8
	jmp	_ASM_InvokeGC
	.text
retGC1F7:
	movq	24(%rdi), %r12
	movq	16(%rdi), %r13
	movq	8(%rdi), %r10
	movq	(%rdi), %rcx
gcTest1F8:
	movq	%r11, %rdx
	movq	448(%rdx), %rdx
	subq	%rsi, %rdx
	jle	doGC1F9
check.1F4:
	/* Liveout:  GP={%r13 %r12 %r10 %r9 %r8 %rdi}  */
	/* block check<E281> (ep<CF97>,rest<CF9A>,retK<CF99>,acc<CF98>) */
	movq	%rcx, %rdi
	movq	(%r10), %r8
	movq	8(%r10), %r9
	movq	16(%r10), %r10
revQueue.1F5:
	movq	%r9, %r14
	movq	%r8, %rbx
	movq	%rdi, %rcx
	movq	$28, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%r14, 8(%rsi)
	movq	%r12, 16(%rsi)
	movq	%rsi, %r12
	addq	$32, %rsi
	cmpq	$1, %r10
	jne	L1FC
L_true1FA:
then.1FB:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block then<CF95> (retK<CF94>,acc<CF93>) */
	movq	%r13, %rdi
	movq	%r12, %r8
	jmp	letJoinK.D
L1FC:
else.1F6:
	/* block else<CF9B> (ep<E27D>,rest<E27E>,retK<E27F>,acc<E280>) */
	jmp	gcTest1F8
doGC1F9:
	movq	$36, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%r10, 8(%rsi)
	movq	%r13, 16(%rsi)
	movq	%r12, 24(%rsi)
	movq	%rsi, %r15
	addq	$40, %rsi
	movq	%r15, %rdi
	movabsq	$retGC1F7, %r8
	jmp	_ASM_InvokeGC
	/* live= GP={%r12 %r13 %r10 %rcx} spilled=  */
	jmp	retGC1F7
	.text
dispatch.1F0:
	movq	%rdi, %rcx
	jmp	gcTest208
	/* live= GP={%rcx} spilled=  */
retGC207:
	movq	(%rdi), %rcx
gcTest208:
	movq	%r11, %r13
	movq	448(%r13), %rdx
	subq	%rsi, %rdx
	jle	doGC209
check.1FD:
	/* block check<E283> (ep<CF33>) */
	movq	8(%rcx), %r15
	movq	80(%r15), %r14
	movq	$3595, -8(%rsi)
	movabsq	$letJoinK.E, %rdx
	movq	%rdx, (%rsi)
	movq	8(%rcx), %rbx
	movq	%rbx, 8(%rsi)
	movq	16(%rcx), %r10
	movq	%r10, 16(%rsi)
	movq	24(%rcx), %r12
	movq	%r12, 24(%rsi)
	movq	%rcx, 32(%rsi)
	movq	%rsi, %r13
	addq	$48, %rsi
	cmpq	$1, %r14
	je	L20A
L_true1FE:
then.200:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block then<CF62> (ep<CF5F>,hd<CF61>,letJoinK<CF60>) */
	movq	16(%r14), %rdx
	movq	8(%rcx), %rbx
	movq	%rdx, 80(%rbx)
	movq	$12, -8(%rsi)
	movq	%r14, (%rsi)
	movq	%rsi, %r10
	addq	$16, %rsi
	movq	%r13, %rdi
	movq	%r10, %r8
	jmp	letJoinK.E
L20A:
else.1FF:
	/* block else<CF69> (ep<CF67>,letJoinK<CF68>) */
	movq	8(%rcx), %r14
	movq	88(%r14), %r12
	cmpq	$1, %r12
	je	L20B
L_true201:
then.203:
	/* block then<CF71> (ep<CF6E>,letJoinK<CF70>,tl<CF6F>) */
	movq	8(%rcx), %r15
	movq	$1, 88(%r15)
	movq	16(%r12), %rbx
	movq	$519, -8(%rsi)
	movabsq	$letJoinK.D, %rdx
	movq	%rdx, (%rsi)
	movq	8(%rcx), %r10
	movq	%r10, 8(%rsi)
	movq	%r13, 16(%rsi)
	movq	%rsi, %r13
	addq	$32, %rsi
	movq	$18, -8(%rsi)
	movq	$1, (%rsi)
	movabsq	$revQueue.1F5, %r14
	movq	%r14, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	$28, -8(%rsi)
	movq	(%r12), %r15
	movq	%r15, (%rsi)
	movq	8(%r12), %rcx
	movq	%rcx, 8(%rsi)
	movq	$1, 16(%rsi)
	movq	%rsi, %r12
	addq	$32, %rsi
	cmpq	$1, %rbx
	je	L_true204
else.205:
	/* Liveout:  GP={%r13 %r12 %r10 %r9 %r8 %rdi}  */
	/* block else<CFB0> (_t<CFAF>,revQueue<CFAE>,acc<CFAD>,letJoinK<CFAC>) */
	movq	(%r10), %rdi
	movq	(%rbx), %r8
	movq	8(%rbx), %r9
	movq	16(%rbx), %r10
	jmp	revQueue.1F5
L20B:
else.202:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block else<CFB9> (letJoinK<CFB8>) */
	movq	%r13, %rdi
	movq	$1, %r8
	jmp	letJoinK.E
L_true204:
then.206:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block then<CFA9> (acc<CFA8>,letJoinK<CFA7>) */
	movq	%r13, %rdi
	movq	%r12, %r8
	jmp	letJoinK.D
doGC209:
	movq	$12, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%rsi, %r12
	addq	$16, %rsi
	movq	%r12, %rdi
	movabsq	$retGC207, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.20D:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest20F
	/* live= GP={%rcx %rdx} spilled=  */
retGC20E:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest20F:
	movq	%r11, %r12
	movq	448(%r12), %rbx
	subq	%rsi, %rbx
	jle	doGC210
check.20C:
	/* Liveout:  GP={%rdi}  */
	/* block check<E286> (ep<CFF8>,_wild<CFF6>) */
	movq	8(%rdx), %rdi
	jmp	dispatch.1F0
doGC210:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	%r10, %rdi
	movabsq	$retGC20E, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.40:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest213
	/* live= GP={%rcx %rdx} spilled=  */
retGC212:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest213:
	movq	%r11, %rbx
	movq	448(%rbx), %rbx
	subq	%rsi, %rbx
	jle	doGC214
check.211:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<E289> (ep<CFF3>,_wild<CFF0>) */
	movq	$261, -8(%rsi)
	movabsq	$letJoinK.20D, %r12
	movq	%r12, (%rsi)
	movq	16(%rdx), %r13
	movq	%r13, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	8(%rdx), %r14
	movq	(%r14), %rdi
	movq	%r10, %r8
	jmp	wakeupSleepingThreads.19A
doGC214:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	%r15, %rdi
	movabsq	$retGC212, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.216:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest218
	/* live= GP={%rcx %rdx} spilled=  */
retGC217:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest218:
	movq	%r11, %r10
	movq	448(%r10), %rbx
	subq	%rsi, %rbx
	jle	doGC219
check.215:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<E28C> (ep<D020>,rest<D01F>) */
	movq	$28, -8(%rsi)
	movq	8(%rdx), %r12
	movq	(%r12), %r13
	movq	%r13, (%rsi)
	movq	8(%rdx), %r14
	movq	8(%r14), %r15
	movq	%r15, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%rsi, %r10
	addq	$32, %rsi
	movq	16(%rdx), %rcx
	movq	(%rcx), %rdx
	movq	%rcx, %rdi
	movq	%r10, %r8
	jmp	*%rdx
doGC219:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	%rbx, %rdi
	movabsq	$retGC217, %r8
	jmp	_ASM_InvokeGC
	.text
retGC21C:
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rbx
	movq	(%rdi), %rdx
gcTest21D:
	movq	%r11, %r15
	movq	448(%r15), %r10
	subq	%rsi, %r10
	jle	doGC21E
check.21A:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block check<E290> (ep<D01A>,queue1<D01C>,retK<D01B>) */
	movq	$775, -8(%rsi)
	movabsq	$letJoinK.216, %r13
	movq	%r13, (%rsi)
	movq	%rbx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%rsi, %r12
	addq	$32, %rsi
	movq	%rdx, %rdi
	movq	16(%rbx), %r8
	movq	%r12, %r9
append.3D:
	movq	%r9, %rcx
	movq	%r8, %rbx
	movq	%rdi, %rdx
	cmpq	$1, %rbx
	jne	L221
L_true21F:
then.220:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block then<D017> (ep<D015>,retK<D016>) */
	movq	(%rcx), %rbx
	movq	%rcx, %rdi
	movq	(%rdx), %r8
	jmp	*%rbx
L221:
else.21B:
	/* block else<D01D> (ep<E28D>,queue1<E28E>,retK<E28F>) */
	jmp	gcTest21D
doGC21E:
	movq	$28, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rbx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%rsi, %r14
	addq	$32, %rsi
	movq	%r14, %rdi
	movabsq	$retGC21C, %r8
	jmp	_ASM_InvokeGC
	/* live= GP={%rcx %rbx %rdx} spilled=  */
	jmp	retGC21C
	.text
letJoinK.3E:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest224
	/* live= GP={%rcx %rdx} spilled=  */
retGC223:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest224:
	movq	%r11, %r13
	movq	448(%r13), %rbx
	subq	%rsi, %rbx
	jle	doGC225
check.222:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<E293> (ep<D02E>,newHd<D02C>) */
	movq	8(%rdx), %r10
	movq	%rcx, 80(%r10)
	movq	16(%rdx), %rdi
	movq	$1, %r8
	jmp	letJoinK.40
doGC225:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	%r12, %rdi
	movabsq	$retGC223, %r8
	jmp	_ASM_InvokeGC
	.text
retGC229:
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest22A:
	movq	%r11, %r13
	movq	448(%r13), %r10
	subq	%rsi, %r10
	jle	doGC22B
check.226:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<E296> (ep<D055>,retK<D056>) */
	pause
	movq	%rbx, %rdi
	movq	%rdx, %r8
lp.227:
	movq	%r8, %rdx
	movq	%rdi, %rbx
	movq	(%rbx), %r15
	movq	384(%r15), %r14
	movq	(%rbx), %r10
	leaq	384(%r10), %rcx
	movq	%r14, %rax
	movq	$1, %r12
	movq	(%rbx), %r13
	lock
	cmpxchgq	%r12, 384(%r13)
	movq	%rax, %rcx
	cmpq	%rcx, %r14
	jne	L22E
L_true22C:
then.22D:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block then<D053> (retK<D052>,x<D051>) */
	movq	%rdx, %rdi
	movq	%rcx, %r8
	jmp	letJoinK.3F
L22E:
else.228:
	/* block else<D057> (ep<E294>,retK<E295>) */
	jmp	gcTest22A
doGC22B:
	movq	$20, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	%r12, %rdi
	movabsq	$retGC229, %r8
	jmp	_ASM_InvokeGC
	/* live= GP={%rdx %rbx} spilled=  */
	jmp	retGC229
	.text
switch.23B:
	movq	%rax, %r13
	movq	%rdi, %r15
	jmp	gcTest23D
	/* live= GP={%r13 %r15} spilled=  */
retGC23C:
	movq	8(%rdi), %r13
	movq	(%rdi), %r15
gcTest23D:
	movq	%r11, %r12
	movq	448(%r12), %r14
	subq	%rsi, %r14
	jle	doGC23E
check.22F:
	/* block check<E299> (ep<CF2E>,s<CF2D>) */
	movq	$1545, -8(%rsi)
	movabsq	$dispatch.1F0, %rdx
	movq	%rdx, (%rsi)
	movq	8(%r15), %rbx
	movq	%rbx, 8(%rsi)
	movq	48(%r15), %r10
	movq	%r10, 16(%rsi)
	movq	%r15, 24(%rsi)
	movq	%rsi, %r14
	addq	$40, %rsi
	cmpq	$1, %r13
	je	L23F
L_true230:
then.232:
	/* block then<CFC0> (ep<CFBD>,s<CFBF>,dispatch<CFBE>) */
	movq	(%r13), %r12
	cmpq	$3, %r12
	je	S_case233
	cmpq	$1, %r12
	je	S_case235
default.237:
	/* Liveout:  GP={%rax %rdi}  */
	/* block default<D05C> (ep<D05B>) */
	movq	$12, -8(%rsi)
	movabsq	$tag138, %r14
	movq	%r14, (%rsi)
	movq	%rsi, %r13
	addq	$16, %rsi
	movq	16(%r15), %r15
	movq	(%r15), %rcx
	movq	%r13, %rax
	movq	%r15, %rdi
	jmp	*%rcx
S_case235:
case.236:
	/* block case<CFDB> (ep<CFD8>,s<CFDA>,dispatch<CFD9>) */
	movq	$28, -8(%rsi)
	movq	8(%r15), %rbx
	movq	24(%rbx), %r10
	movq	%r10, (%rsi)
	movq	8(%r13), %r12
	movq	%r12, 8(%rsi)
	movq	8(%r15), %rcx
	movq	88(%rcx), %rdx
	movq	%rdx, 16(%rsi)
	movq	%rsi, %rdx
	addq	$32, %rsi
	movq	8(%r15), %rbx
	movq	%rdx, 88(%rbx)
	movq	$1545, -8(%rsi)
	movabsq	$letJoinK.3F, %r12
	movq	%r12, (%rsi)
	movq	8(%r15), %r13
	movq	%r13, 8(%rsi)
	movq	40(%r15), %rcx
	movq	%rcx, 16(%rsi)
	movq	%r14, 24(%rsi)
	movq	%rsi, %r10
	addq	$40, %rsi
	movq	8(%r15), %rdx
	cmpq	$1, 384(%rdx)
	je	L_true238
else.239:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block else<D03F> (ep<D03D>,letJoinK<D03E>) */
	movq	$10, -8(%rsi)
	movq	8(%r15), %r12
	movq	%r12, (%rsi)
	movq	%rsi, %rbx
	addq	$16, %rsi
	movq	$133, -8(%rsi)
	movq	%rbx, (%rsi)
	movabsq	$lp.227, %r14
	movq	%r14, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	(%r13), %rdi
	movq	%r10, %r8
	jmp	lp.227
L_true238:
then.23A:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block then<D039> (letJoinK<D038>) */
	movq	%r10, %rdi
	movq	$1, %r8
	jmp	letJoinK.3F
L23F:
else.231:
	/* Liveout:  GP={%rdi}  */
	/* block else<D063> (dispatch<D062>) */
	movq	%r14, %rdi
	jmp	dispatch.1F0
doGC23E:
	movq	$20, -8(%rsi)
	movq	%r15, (%rsi)
	movq	%r13, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	%r10, %rdi
	movabsq	$retGC23C, %r8
	jmp	_ASM_InvokeGC
S_case233:
	movq	%r14, -56(%rbp)
	movq	%r13, -64(%rbp)
case.234:
	/* Liveout:  GP={%rdi}  */
	/* flushLoads */
	/* block case<CFC6> (ep<CFC3>,s<CFC5>,dispatch<CFC4>) */
	movq	$10, -8(%rsi)
	movq	24(%r15), %r10
	movq	(%r10), %rbx
	movq	-64(%rbp), %rcx
	movq	16(%rcx), %r12
	leaq	(%rbx,%r12,1), %rdx
	movq	%rdx, (%rsi)
	movq	%rsi, %rcx
	addq	$16, %rsi
	movq	$28, -8(%rsi)
	movq	8(%r15), %r14
	movq	24(%r14), %rdx
	movq	%rdx, (%rsi)
	movq	-64(%rbp), %rdx
	movq	8(%rdx), %rbx
	movq	%rbx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%rsi, %r13
	addq	$32, %rsi
	movq	$20, -8(%rsi)
	movq	%r13, (%rsi)
	movq	32(%r15), %r10
	movq	(%r10), %r12
	movq	%r12, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	%r11, %rbx
	movq	%rsi, 128(%rbx)
	movq	%rax, %r12
	movq	%rdi, %r13
	movq	%r8, %r10
	movq	%r10, -80(%rbp)
	movq	%r9, %r14
	movq	%r14, -72(%rbp)
	movq	%r11, %r14
	movq	%r11, %rdx
	movq	%rdx, %rdi
	movq	%rcx, %rsi
	call	_PromoteObj
	movq	%rax, %rcx
	movq	%r12, %rax
	movq	%r13, %rdi
	movq	-80(%rbp), %r8
	movq	-72(%rbp), %r9
	movq	%r14, %r11
	movq	128(%rbx), %rsi
	movq	-64(%rbp), %rdx
	movq	8(%r15), %r12
	movq	24(%r15), %r14
	movq	(%r14), %r13
	movq	-64(%rbp), %rbx
	movq	16(%rbx), %rdx
	leaq	(%r13,%rdx,1), %rdx
	movq	32(%r15), %rbx
	movq	32(%r15), %r10
	movq	%rcx, (%r10)
	movq	-56(%rbp), %rdi
	jmp	dispatch.1F0
	.text
mkSwitch.241:
	movq	%r9, %r12
	movq	%r8, %rbx
	movq	%rdi, %rdx
	jmp	gcTest243
	/* live= spilled= GP={%r~1 %r~1 %r~1 %r~1}  */
retGC242:
	movq	24(%rdi), %r10
	movq	16(%rdi), %r12
	movq	8(%rdi), %rbx
	movq	(%rdi), %rdx
gcTest243:
	movq	%r11, %r13
	movq	448(%r13), %r14
	subq	%rsi, %r14
	jle	doGC244
	movq	%r10, -64(%rbp)
	movq	%r12, -56(%rbp)
	movq	%rbx, -88(%rbp)
	movq	%rdx, -80(%rbp)
check.240:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<E29E> (ep<CD80>,self<CD81>,retK<CD82>,exh<CD83>) */
	movq	$10, -8(%rsi)
	movq	$0, (%rsi)
	movq	%rsi, %rcx
	addq	$16, %rsi
	movq	%r11, %r13
	movq	%rsi, 128(%r13)
	movq	%rax, %r15
	movq	%r15, -96(%rbp)
	movq	%rdi, %r12
	movq	%r8, %rbx
	movq	%r9, %r15
	movq	%r11, %r14
	movq	%r11, %rdx
	movq	%rdx, %rdi
	movq	%rcx, %rsi
	call	_PromoteObj
	movq	%rax, %rcx
	movq	%rcx, -72(%rbp)
	movq	-96(%rbp), %rax
	movq	%r12, %rdi
	movq	%rbx, %r8
	movq	%r15, %r9
	movq	%r14, %r11
	movq	128(%r13), %rsi
	movq	$12, -8(%rsi)
	movq	$1, (%rsi)
	movq	%rsi, %rcx
	addq	$16, %rsi
	movq	%r11, %r15
	movq	%rsi, 128(%r15)
	movq	%rax, %rdx
	movq	%rdx, -96(%rbp)
	movq	%rdi, %r14
	movq	%r8, %r13
	movq	%r9, %r12
	movq	%r11, %rbx
	movq	%r11, %r10
	movq	%r10, %rdi
	movq	%rcx, %rsi
	call	_PromoteObj
	movq	%rax, %rcx
	movq	-96(%rbp), %rax
	movq	%r14, %rdi
	movq	%r13, %r8
	movq	%r12, %r9
	movq	%rbx, %r11
	movq	128(%r15), %rsi
	movq	$3723, -8(%rsi)
	movq	-80(%rbp), %rbx
	movq	(%rbx), %r13
	movq	%r13, (%rsi)
	movq	-88(%rbp), %r10
	movq	%r10, 8(%rsi)
	movq	-64(%rbp), %r12
	movq	%r12, 16(%rsi)
	movq	-72(%rbp), %r13
	movq	%r13, 24(%rsi)
	movq	%rcx, 32(%rsi)
	movq	%rsi, %r12
	addq	$48, %rsi
	movq	$133, -8(%rsi)
	movq	%r12, (%rsi)
	movabsq	$wakeupSleepingThreads.19A, %rdx
	movq	%rdx, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	movq	$261, -8(%rsi)
	movq	-88(%rbp), %r14
	movq	%r14, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	$133, -8(%rsi)
	movq	%rbx, (%rsi)
	movabsq	$waitForWork.39, %r12
	movq	%r12, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	$15887, -8(%rsi)
	movabsq	$switch.23B, %r14
	movq	%r14, (%rsi)
	movq	-88(%rbp), %r15
	movq	%r15, 8(%rsi)
	movq	-64(%rbp), %rbx
	movq	%rbx, 16(%rsi)
	movq	-72(%rbp), %r12
	movq	%r12, 24(%rsi)
	movq	%rcx, 32(%rsi)
	movq	%rdx, 40(%rsi)
	movq	%r10, 48(%rsi)
	movq	%rsi, %r13
	addq	$64, %rsi
	movq	-56(%rbp), %r14
	movq	(%r14), %r15
	movq	-56(%rbp), %rdi
	movq	%r13, %r8
	jmp	*%r15
doGC244:
	movq	$1673, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rbx, 8(%rsi)
	movq	%r12, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %rcx
	addq	$40, %rsi
	movq	%rcx, %rdi
	movabsq	$retGC242, %r8
	jmp	_ASM_InvokeGC
	.text
schedCont.246:
	movq	%rax, %rcx
	movq	%rdi, %rdx
	jmp	gcTest248
	/* live= GP={%rcx %rdx} spilled=  */
retGC247:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest248:
	movq	%r11, %rbx
	movq	448(%rbx), %rbx
	subq	%rsi, %rbx
	jle	doGC249
check.245:
	/* Liveout:  GP={%rax %rdi}  */
	/* block check<E2A1> (ep<D076>,k<D075>) */
	movq	$20, -8(%rsi)
	movq	$1, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	$3, (%r11)
	movq	32(%r11), %r13
	movq	8(%r13), %r14
	movq	%r14, 32(%r11)
	movq	(%r13), %r10
	movq	(%r10), %r15
	movq	%r12, %rax
	movq	%r10, %rdi
	jmp	*%r15
doGC249:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	%rcx, %rdi
	movabsq	$retGC247, %r8
	jmp	_ASM_InvokeGC
	.text
dummyK.24B:
	movq	%rax, %rcx
	movq	%rdi, %rdx
	jmp	gcTest24D
	/* live= GP={%rcx %rdx} spilled=  */
retGC24C:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest24D:
	movq	%r11, %rbx
	movq	448(%rbx), %rbx
	subq	%rsi, %rbx
	jle	doGC24E
check.24A:
	/* Liveout:  GP={%rax %rdi}  */
	/* block check<E2A4> (ep<D090>,x<D08F>) */
	movq	$3, (%r11)
	movq	32(%r11), %r12
	movq	8(%r12), %r13
	movq	%r13, 32(%r11)
	movq	(%r12), %r10
	movq	(%r10), %r14
	movq	$1, %r15
	movq	%r15, %rax
	movq	%r10, %rdi
	jmp	*%r14
doGC24E:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	%rcx, %rdi
	movabsq	$retGC24C, %r8
	jmp	_ASM_InvokeGC
	.text
retGC252:
	movq	8(%rdi), %r10
	movq	(%rdi), %rcx
gcTest253:
	movq	%r11, %r13
	movq	448(%r13), %rdx
	subq	%rsi, %rdx
	jle	doGC254
check.24F:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<E2A7> (ep<D0B6>,retK<D0B7>) */
	pause
	movq	%rcx, %rdi
	movq	%r10, %r8
wait.250:
	movq	%r8, %r10
	movq	%rdi, %rcx
	movq	(%rcx), %rbx
	cmpl	$0, (%rbx)
	jne	L258
L_true255:
	movq	%r10, -56(%rbp)
then.256:
	/* Liveout:  GP={%rdi}  */
	/* block then<D0B2> (ep<D0B0>,retK<D0B1>) */
	movq	%rax, %r13
	movq	%rdi, %r12
	movq	%r8, %rbx
	movq	%r9, %r15
	movq	%rsi, %r14
	movq	%r11, %rdx
	movq	%rdx, -64(%rbp)
	movq	8(%rcx), %r10
	movq	%r10, %rdi
	call	_VProcExit
	movq	%r13, %rax
	movq	%r12, %rdi
	movq	%rbx, %r8
	movq	%r15, %r9
	movq	%r14, %rsi
	movq	-64(%rbp), %r11
	movq	-56(%rbp), %rbx
	movq	(%rbx), %r13
	movq	-56(%rbp), %rdi
	jmp	*%r13
L258:
else.251:
	/* block else<D0B8> (ep<E2A5>,retK<E2A6>) */
	jmp	gcTest253
doGC254:
	movq	$20, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%r10, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	%r12, %rdi
	movabsq	$retGC252, %r8
	jmp	_ASM_InvokeGC
	/* live= GP={%r10 %rcx} spilled=  */
	jmp	retGC252
	.text
shutdownCont.25A:
	movq	%rax, %rcx
	movq	%rdi, %rdx
	jmp	gcTest25C
	/* live= GP={%rcx %rdx} spilled=  */
retGC25B:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest25C:
	movq	%r11, %r12
	movq	448(%r12), %rbx
	subq	%rsi, %rbx
	jle	doGC25D
check.259:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<E2AA> (ep<D09D>,_wild<D09B>) */
	movl	$-1, %r10d
	movq	8(%rdx), %r12
	lock
	xaddl	%r10d, (%r12)
	movq	$133, -8(%rsi)
	movq	8(%rdx), %r14
	movq	%r14, (%rsi)
	movq	16(%rdx), %r15
	movq	%r15, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	$133, -8(%rsi)
	movq	%r13, (%rsi)
	movabsq	$wait.250, %rbx
	movq	%rbx, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	(%rcx), %rdi
	movq	24(%rdx), %r8
	jmp	wait.250
doGC25D:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	%r10, %rdi
	movabsq	$retGC25B, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.25F:
	movq	%r8, %rdx
	movq	%rdi, %r14
	jmp	gcTest261
	/* live= GP={%rdx %r14} spilled=  */
retGC260:
	movq	8(%rdi), %rdx
	movq	(%rdi), %r14
gcTest261:
	movq	%r11, %rcx
	movq	448(%rcx), %rbx
	subq	%rsi, %rbx
	jle	doGC262
check.25E:
	/* Liveout:  GP={%rdi}  */
	/* block check<E2AD> (ep<D0BF>,act<D0BE>) */
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	8(%r14), %rbx
	movq	32(%rbx), %r10
	movq	%r10, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	%r11, %r13
	movq	%rsi, 128(%r13)
	movq	%rax, %r10
	movq	%r10, -56(%rbp)
	movq	%rdi, %r12
	movq	%r8, %rbx
	movq	%r9, %r15
	movq	%r11, %rdx
	movq	%rdx, -64(%rbp)
	movq	%r11, %rdx
	movq	%rdx, %rdi
	movq	%rcx, %rsi
	call	_PromoteObj
	movq	%rax, %r10
	movq	-56(%rbp), %rax
	movq	%r12, %rdi
	movq	%rbx, %r8
	movq	%r15, %r9
	movq	-64(%rbp), %r11
	movq	128(%r13), %rsi
	movq	8(%r14), %r12
	movq	%r10, 32(%r12)
	movq	16(%r14), %r13
	movq	(%r13), %r14
	movq	%r13, %rdi
	jmp	*%r14
doGC262:
	movq	$20, -8(%rsi)
	movq	%r14, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	%r15, %rdi
	movabsq	$retGC260, %r8
	jmp	_ASM_InvokeGC
	.text
initVPFields.264:
	movq	%r9, %r12
	movq	%r8, %rbx
	movq	%rdi, %rdx
	jmp	gcTest266
	/* live= spilled= GP={%r~1 %r~1 %r~1 %r~1}  */
retGC265:
	movq	24(%rdi), %r10
	movq	16(%rdi), %r12
	movq	8(%rdi), %rbx
	movq	(%rdi), %rdx
gcTest266:
	movq	%r11, %rcx
	movq	448(%rcx), %r13
	subq	%rsi, %r13
	jle	doGC267
	movq	%r10, -56(%rbp)
	movq	%r12, -72(%rbp)
	movq	%rbx, -80(%rbp)
	movq	%rdx, -64(%rbp)
check.263:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block check<E2B2> (ep<D06E>,vp<D06F>,retK<D070>,exh<D071>) */
	movq	$10, -8(%rsi)
	movabsq	$schedCont.246, %r13
	movq	%r13, (%rsi)
	movq	%rsi, %rcx
	addq	$16, %rsi
	movq	%r11, %r13
	movq	%rsi, 128(%r13)
	movq	%rax, %r14
	movq	%r14, -88(%rbp)
	movq	%rdi, %r12
	movq	%r8, %rbx
	movq	%r9, %r15
	movq	%r11, %r14
	movq	%r11, %rdx
	movq	%rdx, %rdi
	movq	%rcx, %rsi
	call	_PromoteObj
	movq	%rax, %r10
	movq	-88(%rbp), %rax
	movq	%r12, %rdi
	movq	%rbx, %r8
	movq	%r15, %r9
	movq	%r14, %r11
	movq	128(%r13), %rsi
	movq	-80(%rbp), %r15
	movq	%r10, 40(%r15)
	movq	$10, -8(%rsi)
	movl	$0, (%rsi)
	movq	%rsi, %r12
	addq	$16, %rsi
	movq	$20, -8(%rsi)
	movq	%r12, (%rsi)
	movq	$1, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movq	%r13, (%rsi)
	movq	$1, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	$1289, -8(%rsi)
	movl	$-1, (%rsi)
	movq	$1, 8(%rsi)
	xorl	%r15d, %r15d
	incl	%r15d
	movl	%r15d, 16(%rsi)
	movq	%r14, 24(%rsi)
	movq	%rsi, %rcx
	addq	$40, %rsi
	movq	%r11, %r15
	movq	%rsi, 128(%r15)
	movq	%rax, %rdx
	movq	%rdx, -88(%rbp)
	movq	%rdi, %r14
	movq	%r8, %r13
	movq	%r9, %r12
	movq	%r11, %rbx
	movq	%r11, %rdx
	movq	%rdx, %rdi
	movq	%rcx, %rsi
	call	_PromoteObj
	movq	%rax, %r10
	movq	-88(%rbp), %rax
	movq	%r14, %rdi
	movq	%r13, %r8
	movq	%r12, %r9
	movq	%rbx, %r11
	movq	128(%r15), %rsi
	movq	-80(%rbp), %rbx
	movq	%r10, 24(%rbx)
	movq	$10, -8(%rsi)
	movabsq	$dummyK.24B, %r12
	movq	%r12, (%rsi)
	movq	%rsi, %rcx
	addq	$16, %rsi
	movq	%r11, %r15
	movq	%rsi, 128(%r15)
	movq	%rax, %r14
	movq	%rdi, %r13
	movq	%r8, %r12
	movq	%r9, %r10
	movq	%r10, -88(%rbp)
	movq	%r11, %rbx
	movq	%r11, %rdx
	movq	%rdx, %rdi
	movq	%rcx, %rsi
	call	_PromoteObj
	movq	%rax, %r10
	movq	%r14, %rax
	movq	%r13, %rdi
	movq	%r12, %r8
	movq	-88(%rbp), %r9
	movq	%rbx, %r11
	movq	128(%r15), %rsi
	movq	-80(%rbp), %r12
	movq	%r10, 48(%r12)
	movq	$1289, -8(%rsi)
	movabsq	$shutdownCont.25A, %r12
	movq	%r12, (%rsi)
	movq	-64(%rbp), %r13
	movq	8(%r13), %r13
	movq	%r13, 8(%rsi)
	movq	-80(%rbp), %r14
	movq	%r14, 16(%rsi)
	movq	-72(%rbp), %r15
	movq	%r15, 24(%rsi)
	movq	%rsi, %rcx
	addq	$40, %rsi
	movq	%r11, %r15
	movq	%rsi, 128(%r15)
	movq	%rax, %r14
	movq	%rdi, %rdx
	movq	%rdx, -88(%rbp)
	movq	%r8, %r13
	movq	%r9, %r12
	movq	%r11, %rbx
	movq	%r11, %rdx
	movq	%rdx, %rdi
	movq	%rcx, %rsi
	call	_PromoteObj
	movq	%rax, %r10
	movq	%r14, %rax
	movq	-88(%rbp), %rdi
	movq	%r13, %r8
	movq	%r12, %r9
	movq	%rbx, %r11
	movq	128(%r15), %rsi
	movq	-80(%rbp), %rbx
	movq	%r10, 64(%rbx)
	movq	$519, -8(%rsi)
	movabsq	$letJoinK.25F, %r13
	movq	%r13, (%rsi)
	movq	-80(%rbp), %r10
	movq	%r10, 8(%rsi)
	movq	-72(%rbp), %r12
	movq	%r12, 16(%rsi)
	movq	%rsi, %r12
	addq	$32, %rsi
	movq	-64(%rbp), %r13
	movq	(%r13), %r14
	movq	(%r14), %rdi
	movq	-80(%rbp), %r8
	movq	%r12, %r9
	movq	-56(%rbp), %r10
	jmp	mkSwitch.241
doGC267:
	movq	$1673, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rbx, 8(%rsi)
	movq	%r12, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %r15
	addq	$40, %rsi
	movq	%r15, %rdi
	movabsq	$retGC265, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.26A:
	movq	%rdi, %rcx
	jmp	gcTest26C
	/* live= GP={%rcx} spilled=  */
retGC26B:
	movq	(%rdi), %rcx
gcTest26C:
	movq	%r11, %r12
	movq	448(%r12), %rdx
	subq	%rsi, %rdx
	jle	doGC26D
check.268:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block check<E2B4> (ep<D0E1>) */
	movq	8(%rcx), %rbx
	movq	(%rbx), %rdi
	movq	24(%rcx), %r8
	movq	16(%rcx), %r9
	jmp	lp.269
doGC26D:
	movq	$12, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%rsi, %r10
	addq	$16, %rsi
	movq	%r10, %rdi
	movabsq	$retGC26B, %r8
	jmp	_ASM_InvokeGC
	.text
lp.269:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest274
	/* live= GP={%rcx %rdx %rbx} spilled=  */
retGC273:
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest274:
	movq	%r11, %r14
	movq	448(%r14), %r10
	subq	%rsi, %r10
	jle	doGC275
check.26E:
	/* block check<E2B8> (ep<D0D0>,vps<D0D1>,retK<D0D2>) */
	cmpq	$1, %rdx
	je	L276
L_true26F:
then.271:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block then<D0DA> (ep<D0D7>,vps<D0D9>,retK<D0D8>) */
	movq	$133, -8(%rsi)
	movq	%rbx, (%rsi)
	movabsq	$lp.269, %r13
	movq	%r13, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	$1801, -8(%rsi)
	movabsq	$letJoinK.26A, %r15
	movq	%r15, (%rsi)
	movq	%r12, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	8(%rdx), %rcx
	movq	%rcx, 24(%rsi)
	movq	%rsi, %r14
	addq	$40, %rsi
	movq	8(%rbx), %r10
	movq	(%r10), %rdi
	movq	(%rdx), %r12
	movq	(%r12), %r8
	movq	%r14, %r9
	movq	(%rbx), %r10
	jmp	initVPFields.264
doGC275:
	movq	$28, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%rsi, %r13
	addq	$32, %rsi
	movq	%r13, %rdi
	movabsq	$retGC273, %r8
	jmp	_ASM_InvokeGC
L276:
else.270:
	/* Liveout:  GP={%rdi}  */
	/* block else<D0EF> (retK<D0EE>) */
	movq	%rcx, %rdi
	jmp	letJoinK.272
	.text
anon.278:
	movq	%r9, %rdx
	movq	%r8, %rcx
	movq	%rdi, %rbx
	jmp	gcTest27A
	/* live= GP={%r10 %rdx %rcx %rbx} spilled=  */
retGC279:
	movq	24(%rdi), %r10
	movq	16(%rdi), %rdx
	movl	8(%rdi), %ecx
	movq	(%rdi), %rbx
gcTest27A:
	movq	%r11, %r12
	movq	448(%r12), %r12
	subq	%rsi, %r12
	jle	doGC27B
check.277:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<E2BD> (ep<D12F>,_t<D130>,retK<D131>,_exh<D132>) */
	movq	(%rdx), %r14
	movq	%rdx, %rdi
	movl	%ecx, %r13d
	movq	(%rbx), %r15
	movq	$64, %rcx
	subq	(%r15), %rcx
	movq	%r13, %r8
	shlq	%cl, %r8
	jmp	*%r14
doGC27B:
	movq	$1673, -8(%rsi)
	movq	%rbx, (%rsi)
	movl	%ecx, 8(%rsi)
	movq	%rdx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %rcx
	addq	$40, %rsi
	movq	%rcx, %rdi
	movabsq	$retGC279, %r8
	jmp	_ASM_InvokeGC
	.text
new.27D:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest27F
	/* live= GP={%rcx %rdx %rbx} spilled=  */
retGC27E:
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest27F:
	movq	%r11, %r10
	movq	448(%r10), %r10
	subq	%rsi, %r10
	jle	doGC280
check.27C:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<E2C1> (ep<D14D>,data<D14E>,retK<D14F>) */
	movq	$3, (%r11)
	movl	224(%r11), %r13d
	movq	%r13, %r14
	shlq	$3, %r14
	movq	(%rdx,%r14,1), %r12
	incq	%r12
	shlq	$3, %r13
	movq	%r12, (%rdx,%r13,1)
	movq	(%rcx), %r15
	movq	%rcx, %rdi
	movq	%r12, %r8
	jmp	*%r15
doGC280:
	movq	$28, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%rsi, %rcx
	addq	$32, %rsi
	movq	%rcx, %rdi
	movabsq	$retGC27E, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.282:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest284
	/* live= GP={%rcx %rdx} spilled=  */
retGC283:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest284:
	movq	%r11, %rbx
	movq	448(%rbx), %rbx
	subq	%rsi, %rbx
	jle	doGC285
check.281:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<E2C4> (ep<D180>,rest<D17F>) */
	movq	$20, -8(%rsi)
	movq	16(%rdx), %r12
	movq	%r12, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	8(%rdx), %r13
	movq	(%r13), %r14
	movq	%r13, %rdi
	movq	%r10, %r8
	jmp	*%r14
doGC285:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	%r15, %rdi
	movabsq	$retGC283, %r8
	jmp	_ASM_InvokeGC
	.text
lp.292:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest294
	/* live= GP={%rcx %rdx} spilled=  */
retGC293:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest294:
	movq	%r11, %rbx
	movq	448(%rbx), %rbx
	subq	%rsi, %rbx
	jle	doGC295
check.286:
	/* block check<E2C7> (ep<D165>,retK<D166>) */
	movq	(%rdx), %rbx
	movq	(%rdx), %r10
	movl	4(%rbx), %r12d
	cmpl	(%r10), %r12d
	jne	L296
L_true287:
then.289:
	/* block then<D18F> (ep<D18D>,retK<D18E>) */
	movq	$1, %r10
	jmp	letJoinK.28E
doGC295:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	%rcx, %rdi
	movabsq	$retGC293, %r8
	jmp	_ASM_InvokeGC
L296:
else.288:
	/* block else<D193> (ep<D191>,retK<D192>) */
	movq	(%rdx), %r13
	movl	(%r13), %r14d
	movq	(%rdx), %r15
	movq	%r14, %r10
	shll	$3, %r10d
	movslq	%r10d, %rbx
	movq	16(%r15,%rbx,1), %r12
	movq	(%rdx), %r15
	leaq	(%r15), %r13
	movq	(%rdx), %rbx
	shll	$3, %r14d
	movslq	%r14d, %r10
	movq	$1, 16(%rbx,%r10,1)
	movq	(%rdx), %r13
	movl	(%r13), %r13d
	movq	(%rdx), %r14
	movl	8(%r14), %r15d
	decl	%r15d
	cmpl	%r15d, %r13d
	jl	L297
L_true28A:
then.28C:
	/* block then<D1C2> (ep<D1BF>,retK<D1C1>,elt<D1C0>) */
	xorl	%r13d, %r13d
letJoinK.28D:
	/* block letJoinK<D1BA> (ep<D1B6>,retK<D1B9>,elt<D1B8>,oldR<D1B7>) */
	movq	(%rdx), %rbx
	leaq	(%rbx), %r15
	movq	(%rdx), %r10
	movl	%r13d, (%r10)
	movq	$12, -8(%rsi)
	movq	%r12, (%rsi)
	movq	%rsi, %r10
	addq	$16, %rsi
letJoinK.28E:
	/* block letJoinK<D177> (ep<D174>,retK<D176>,thd<D175>) */
	cmpq	$1, %r10
	jne	L_true28F
else.290:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block else<D18A> (retK<D189>) */
	movq	(%rcx), %r12
	movq	%rcx, %rdi
	movq	$1, %r8
	jmp	*%r12
L297:
else.28B:
	/* block else<D1C8> (ep<D1C4>,retK<D1C7>,elt<D1C6>,_t<D1C5>) */
	incl	%r13d
	jmp	letJoinK.28D
L_true28F:
then.291:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block then<D17C> (ep<D179>,retK<D17B>,thd<D17A>) */
	movq	$775, -8(%rsi)
	movabsq	$letJoinK.282, %r14
	movq	%r14, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	(%r10), %r15
	movq	%r15, 16(%rsi)
	movq	%rsi, %r13
	addq	$32, %rsi
	movq	%rdx, %rdi
	movq	%r13, %r8
	jmp	lp.292
	.text
to_D_list_D_from_D_atomic.299:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest29B
	/* live= GP={%r10 %rcx %rdx %rbx} spilled=  */
retGC29A:
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest29B:
	movq	%r11, %r12
	movq	448(%r12), %r12
	subq	%rsi, %r12
	jle	doGC29C
check.298:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<E2CC> (ep<D15C>,self<D15D>,deque<D15E>,retK<D15F>) */
	movq	$12, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%rsi, %r13
	addq	$16, %rsi
	movq	$133, -8(%rsi)
	movq	%r13, (%rsi)
	movabsq	$lp.292, %r15
	movq	%r15, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	(%r14), %rdi
	movq	%r10, %r8
	jmp	lp.292
doGC29C:
	movq	$1545, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %rcx
	addq	$40, %rsi
	movq	%rcx, %rdi
	movabsq	$retGC29A, %r8
	jmp	_ASM_InvokeGC
	.text
isMuggable.2A5:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
gcTest2A7:
	movq	%r11, %r12
	movq	448(%r12), %r12
	subq	%rsi, %r12
	jle	doGC2A8
check.29D:
	/* block check<E2D1> (ep<D1DA>,deque<D1DB>,retK<D1DC>,exh<D1DD>) */
	movl	12(%rdx), %r14d
	xorl	%r13d, %r13d
	movl	4(%rdx), %ebx
	cmpl	(%rdx), %ebx
	jne	L2A9
L_true29E:
then.2A0:
	/* block then<D203> (retK<D202>,_t<D201>,_t<D200>) */
	movq	$1, %r15
letJoinK.2A1:
	/* block letJoinK<D1F4> (retK<D1F3>,_t<D1F2>,_t<D1F1>,isNotEmpty<D1F0>) */
	cmpl	%r13d, %r14d
	je	L_true2A2
else.2A3:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block else<D1FC> (retK<D1FB>) */
	movq	%rcx, %rdi
	movq	$1, %r8
	jmp	letJoinK.CB
doGC2A8:
	movq	$36, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %r10
	addq	$40, %rsi
	movq	%r10, %rdi
	movabsq	$retGC2A6, %r8
	jmp	_ASM_InvokeGC
	/* live= GP={%r10 %rcx %rdx %rbx} spilled=  */
retGC2A6:
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
	jmp	gcTest2A7
L2A9:
else.29F:
	/* block else<D209> (retK<D208>,_t<D207>,_t<D206>) */
	movq	$3, %r15
	jmp	letJoinK.2A1
L_true2A2:
then.2A4:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block then<D1F8> (retK<D1F7>,isNotEmpty<D1F6>) */
	movq	%rcx, %rdi
	movq	%r15, %r8
	jmp	letJoinK.CB
	.text
exh.2AB:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest2AD
	/* live= GP={%rcx %rdx} spilled=  */
retGC2AC:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest2AD:
	movq	%r11, %r14
	movq	448(%r14), %rbx
	subq	%rsi, %rbx
	jle	doGC2AE
check.2AA:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<E2D4> (ep<D20E>,_wild<D20D>) */
	movq	8(%rdx), %r10
	movq	(%r10), %r12
	movq	%r10, %rdi
	movq	$1, %r8
	jmp	*%r12
doGC2AE:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	%r13, %rdi
	movabsq	$retGC2AC, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.2B0:
	movq	%rdi, %rcx
	jmp	gcTest2B2
	/* live= GP={%rcx} spilled=  */
retGC2B1:
	movq	(%rdi), %rcx
gcTest2B2:
	movq	%r11, %r13
	movq	448(%r13), %rdx
	subq	%rsi, %rdx
	jle	doGC2B3
check.2AF:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<E2D6> (ep<D223>) */
	movq	8(%rcx), %rbx
	movq	(%rbx), %r10
	movq	%rbx, %rdi
	movq	16(%rcx), %r8
	jmp	*%r10
doGC2B3:
	movq	$12, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%rsi, %r12
	addq	$16, %rsi
	movq	%r12, %rdi
	movabsq	$retGC2B1, %r8
	jmp	_ASM_InvokeGC
	.text
exh.2B5:
	movq	%rax, %rcx
	movq	%rdi, %rdx
	jmp	gcTest2B7
	/* live= GP={%rcx %rdx} spilled=  */
retGC2B6:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest2B7:
	movq	%r11, %r12
	movq	448(%r12), %rbx
	subq	%rsi, %rbx
	jle	doGC2B8
check.2B4:
	/* Liveout:  GP={%rdi}  */
	/* block check<E2D9> (ep<D22A>,_wild<D229>) */
	movq	8(%rdx), %rdi
	jmp	letJoinK.2B0
doGC2B8:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	%r10, %rdi
	movabsq	$retGC2B6, %r8
	jmp	_ASM_InvokeGC
	.text
f1.2BA:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest2BC
	/* live= GP={%r10 %rcx %rdx %rbx} spilled=  */
retGC2BB:
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest2BC:
	movq	%r11, %r15
	movq	448(%r15), %r12
	subq	%rsi, %r12
	jle	doGC2BD
check.2B9:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<E2DE> (ep<D231>,x<D232>,retK<D233>,exh<D234>) */
	movq	(%rdx), %r13
	decl	12(%r13)
	movq	%rcx, %rdi
	movq	$1, %r8
	jmp	letJoinK.B4
doGC2BD:
	movq	$36, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %r14
	addq	$40, %rsi
	movq	%r14, %rdi
	movabsq	$retGC2BB, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.2BF:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest2C1
	/* live= GP={%rcx %rdx} spilled=  */
retGC2C0:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest2C1:
	movq	%r11, %r12
	movq	448(%r12), %rbx
	subq	%rsi, %rbx
	jle	doGC2C2
check.2BE:
	/* Liveout:  GP={%rdi}  */
	/* block check<E2E1> (ep<D248>,_wild<D247>) */
	movq	8(%rdx), %rdi
	jmp	letJoinK.2B0
doGC2C2:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	%r10, %rdi
	movabsq	$retGC2C0, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.2C4:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest2C6
	/* live= GP={%rcx %rdx} spilled=  */
retGC2C5:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest2C6:
	movq	%r11, %r15
	movq	448(%r15), %rbx
	subq	%rsi, %rbx
	jle	doGC2C7
check.2C3:
	/* Liveout:  GP={%r12 %r10 %r9 %r8 %rdi}  */
	/* block check<E2E4> (ep<D21F>,threads<D21B>) */
	movq	$775, -8(%rsi)
	movabsq	$letJoinK.2B0, %r12
	movq	%r12, (%rsi)
	movq	16(%rdx), %r13
	movq	%r13, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%rsi, %r10
	addq	$32, %rsi
	movq	$20, -8(%rsi)
	movabsq	$exh.2B5, %r14
	movq	%r14, (%rsi)
	movq	%r10, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	$18, -8(%rsi)
	movq	$1, (%rsi)
	movabsq	$f1.2BA, %rcx
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	$261, -8(%rsi)
	movabsq	$letJoinK.2BF, %rbx
	movq	%rbx, (%rsi)
	movq	%r10, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	8(%rdx), %r13
	movq	(%r13), %rdi
	movq	%r15, %r8
	movq	24(%rdx), %r9
	jmp	app_D_w_uncurried.C
doGC2C7:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	%r14, %rdi
	movabsq	$retGC2C5, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.2CC:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest2CE
	/* live= GP={%rcx %rdx} spilled=  */
retGC2CD:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest2CE:
	movq	%r11, %rbx
	movq	448(%rbx), %rbx
	subq	%rsi, %rbx
	jle	doGC2CF
check.2C8:
	/* block check<E2E7> (ep<D217>,muggable<D214>) */
	movq	$1801, -8(%rsi)
	movabsq	$letJoinK.2C4, %r10
	movq	%r10, (%rsi)
	movq	8(%rdx), %r12
	movq	%r12, 8(%rsi)
	movq	32(%rdx), %r13
	movq	%r13, 16(%rsi)
	movq	40(%rdx), %r14
	movq	%r14, 24(%rsi)
	movq	%rsi, %r10
	addq	$40, %rsi
	cmpq	$1, %rcx
	je	L2D0
L_true2C9:
then.2CB:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block then<D254> (ep<D251>,muggable<D253>,letJoinK<D252>) */
	movq	16(%rdx), %r15
	movq	(%r15), %rdi
	movq	24(%rdx), %r8
	movq	(%rcx), %rcx
	movq	(%rcx), %r9
	jmp	to_D_list_D_from_D_atomic.299
doGC2CF:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	movq	%rdx, %rdi
	movabsq	$retGC2CD, %r8
	jmp	_ASM_InvokeGC
L2D0:
else.2CA:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block else<D25E> (letJoinK<D25D>) */
	movq	%r10, %rdi
	movq	$1, %r8
	jmp	letJoinK.2C4
	.text
mug_D_from_D_atomic.2D3:
	movq	%r9, %rcx
	movq	%r8, %rbx
	movq	%rdi, %rdx
	jmp	gcTest2D5
	/* live= GP={%rcx} spilled= GP={%r~1 %r~1 %r~1}  */
retGC2D4:
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rbx
	movq	(%rdi), %rdx
gcTest2D5:
	movq	%r11, %r12
	movq	448(%r12), %r13
	subq	%rsi, %r13
	jle	doGC2D6
	movq	%r10, -64(%rbp)
	movq	%rbx, -72(%rbp)
	movq	%rdx, -80(%rbp)
check.2D1:
	/* Liveout:  GP={%r12 %r10 %r9 %r8 %rdi}  */
	/* block check<E2EC> (ep<D1D1>,self<D1D2>,workGroupId<D1D3>,retK<D1D4>) */
	movq	$18, -8(%rsi)
	movq	$1, (%rsi)
	movabsq	$isMuggable.2A5, %r10
	movq	%r10, 8(%rsi)
	movq	%rsi, %r14
	movq	%r14, -56(%rbp)
	addq	$24, %rsi
	movq	%r11, %rbx
	movq	%rsi, 128(%rbx)
	movq	%rax, %r12
	movq	%rdi, %r13
	movq	%r8, %r14
	movq	%r9, %r15
	movq	%r15, -88(%rbp)
	movq	%r11, %r15
	movq	-72(%rbp), %rdi
	movq	%rcx, %rsi
	call	_M_LocalDeques
	movq	%rax, %rcx
	movq	%r12, %rax
	movq	%r13, %rdi
	movq	%r14, %r8
	movq	-88(%rbp), %r9
	movq	%r15, %r11
	movq	128(%rbx), %rsi
	movq	$261, -8(%rsi)
	movabsq	$exh.2AB, %rdx
	movq	%rdx, (%rsi)
	movq	-64(%rbp), %rdx
	movq	%rdx, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	$6925, -8(%rsi)
	movabsq	$letJoinK.2CC, %rbx
	movq	%rbx, (%rsi)
	movq	-80(%rbp), %rbx
	movq	(%rbx), %r10
	movq	%r10, 8(%rsi)
	movq	-80(%rbp), %r10
	movq	16(%r10), %r13
	movq	%r13, 16(%rsi)
	movq	-72(%rbp), %r13
	movq	%r13, 24(%rsi)
	movq	-64(%rbp), %r14
	movq	%r14, 32(%rsi)
	movq	%rcx, 40(%rsi)
	movq	%rsi, %r10
	addq	$56, %rsi
	movq	-80(%rbp), %r15
	movq	8(%r15), %r14
	movq	(%r14), %rdi
	movq	-56(%rbp), %r8
	movq	%rcx, %r9
	jmp	filter_D_w_uncurried.E1
doGC2D6:
	movq	$1161, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rbx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %r15
	addq	$40, %rsi
	movq	%r15, %rdi
	movabsq	$retGC2D4, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.48:
	movq	%rdi, %rcx
	jmp	gcTest2D9
	/* live= GP={%rcx} spilled=  */
retGC2D8:
	movq	(%rdi), %rcx
gcTest2D9:
	movq	%r11, %r14
	movq	448(%r14), %rdx
	subq	%rsi, %rdx
	jle	doGC2DA
check.2D7:
	/* Liveout:  GP={%rax %rdi}  */
	/* block check<E2EE> (ep<D2A1>) */
	movq	8(%rcx), %rbx
	movq	(%rbx), %r10
	movq	$1, %r12
	movq	%r12, %rax
	movq	%rbx, %rdi
	jmp	*%r10
doGC2DA:
	movq	$12, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%rsi, %r13
	addq	$16, %rsi
	movq	%r13, %rdi
	movabsq	$retGC2D8, %r8
	jmp	_ASM_InvokeGC
	.text
k.4D:
	movq	%rax, %rcx
	movq	%rdi, %rdx
	jmp	gcTest2DD
	/* live= GP={%rcx %rdx} spilled=  */
retGC2DC:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest2DD:
	movq	%r11, %r12
	movq	448(%r12), %rbx
	subq	%rsi, %rbx
	jle	doGC2DE
check.2DB:
	/* Liveout:  GP={%rdi}  */
	/* block check<E2F1> (ep<D2B3>,x<D2B2>) */
	movq	8(%rdx), %rdi
	jmp	letJoinK.48
doGC2DE:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	%r10, %rdi
	movabsq	$retGC2DC, %r8
	jmp	_ASM_InvokeGC
	.text
retGC2E7:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
	jmp	gcTest2E8
L_true2E0:
then.2E2:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block then<D2DB> (ep<D2D9>,retK<D2DA>) */
	pause
	movq	%rdx, %rdi
	movq	%rcx, %r8
spinLp.2E6:
	movq	%r8, %rcx
	movq	%rdi, %rdx
gcTest2E8:
	movq	%r11, %rbx
	movq	448(%rbx), %rbx
	subq	%rsi, %rbx
	jle	doGC2E9
check.2DF:
	/* block check<E2F4> (ep<D2D2>,retK<D2D3>) */
	movq	(%rdx), %r12
	leaq	(%r12), %r10
	cmpl	$1, (%r10)
	je	L_true2E0
else.2E1:
	/* block else<D2DF> (ep<D2DD>,retK<D2DE>) */
	movq	$1, %r13
	movq	(%rdx), %r14
	lock
	xchgq	%r13, (%r14)
	cmpq	$1, %r13
	je	L_true2E3
else.2E4:
	/* Liveout:  GP={%rdi}  */
	/* block else<D2E8> (retK<D2E7>) */
	movq	%rcx, %rdi
	jmp	letJoinK.4E
L_true2E3:
then.2E5:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block then<D2E4> (ep<D2E2>,retK<D2E3>) */
	movq	%rdx, %rdi
	movq	%rcx, %r8
	jmp	spinLp.2E6
doGC2E9:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	%r15, %rdi
	movabsq	$retGC2E7, %r8
	jmp	_ASM_InvokeGC
	/* live= GP={%rcx %rdx} spilled=  */
	jmp	retGC2E7
	.text
spawnFn.2EB:
	movq	%r9, %r12
	movq	%r8, %r10
	movq	%rax, %rbx
	movq	%rdi, %rdx
	jmp	gcTest2ED
	/* live= GP={%r12 %rbx} spilled= GP={%r~1 %r~1}  */
retGC2EC:
	movq	24(%rdi), %r12
	movq	16(%rdi), %r10
	movq	8(%rdi), %rbx
	movq	(%rdi), %rdx
gcTest2ED:
	movq	%r11, %r13
	movq	448(%r13), %r14
	subq	%rsi, %r14
	jle	doGC2EE
	movq	%r10, -56(%rbp)
	movq	%rdx, -64(%rbp)
check.2EA:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<E2F9> (ep<D27F>,thd<D280>,retK<D281>,exh<D282>) */
	movq	$3, (%r11)
	movq	$20, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	$1, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	%r11, %r15
	movq	%rsi, 128(%r15)
	movq	%rax, %r14
	movq	%rdi, %r13
	movq	%r8, %rdx
	movq	%rdx, -72(%rbp)
	movq	%r9, %r12
	movq	%r11, %rbx
	movq	%r11, %rdx
	movq	%rdx, %rdi
	movq	%rcx, %rsi
	call	_PromoteObj
	movq	%rax, %rcx
	movq	%r14, %rax
	movq	%r13, %rdi
	movq	-72(%rbp), %r8
	movq	%r12, %r9
	movq	%rbx, %r11
	movq	128(%r15), %rsi
	movq	$2827, -8(%rsi)
	movabsq	$letJoinK.4E, %r10
	movq	%r10, (%rsi)
	movq	-64(%rbp), %rdx
	movq	(%rdx), %r12
	movq	%r12, 8(%rsi)
	movq	-56(%rbp), %rbx
	movq	%rbx, 16(%rsi)
	movq	%r11, 24(%rsi)
	movq	%rcx, 32(%rsi)
	movq	%rsi, %rbx
	addq	$48, %rsi
	movq	$12, -8(%rsi)
	movq	-64(%rbp), %r10
	movq	(%r10), %r14
	movq	%r14, (%rsi)
	movq	%rsi, %r13
	addq	$16, %rsi
	movq	$133, -8(%rsi)
	movq	%r13, (%rsi)
	movabsq	$spinLp.2E6, %rcx
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	(%r15), %rdi
	movq	%rbx, %r8
	jmp	spinLp.2E6
doGC2EE:
	movq	$36, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rbx, 8(%rsi)
	movq	%r10, 16(%rsi)
	movq	%r12, 24(%rsi)
	movq	%rsi, %rdx
	addq	$40, %rsi
	movq	%rdx, %rdi
	movabsq	$retGC2EC, %r8
	jmp	_ASM_InvokeGC
	.text
removeFn.2F0:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rax, %rbx
	movq	%rdi, %r10
	jmp	gcTest2F2
	/* live= GP={%rcx %rdx %rbx %r10} spilled=  */
retGC2F1:
	movq	24(%rdi), %rcx
	movq	16(%rdi), %rdx
	movq	8(%rdi), %rbx
	movq	(%rdi), %r10
gcTest2F2:
	movq	%r11, %r12
	movq	448(%r12), %r12
	subq	%rsi, %r12
	jle	doGC2F3
check.2EF:
	/* Liveout:  GP={%rax %rdi}  */
	/* block check<E2FE> (ep<D2EE>,thd<D2EF>,retK<D2F0>,exh<D2F1>) */
	movq	(%rdx), %r13
	movq	$1, %r14
	movq	%r14, %rax
	movq	%rdx, %rdi
	jmp	*%r13
doGC2F3:
	movq	$36, -8(%rsi)
	movq	%r10, (%rsi)
	movq	%rbx, 8(%rsi)
	movq	%rdx, 16(%rsi)
	movq	%rcx, 24(%rsi)
	movq	%rsi, %r15
	addq	$40, %rsi
	movq	%r15, %rdi
	movabsq	$retGC2F1, %r8
	jmp	_ASM_InvokeGC
	.text
act.2F6:
	movq	%rax, %rcx
	movq	%rdi, %rdx
	jmp	gcTest2F8
	/* live= GP={%rcx %rdx} spilled=  */
retGC2F7:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest2F8:
	movq	%r11, %r12
	movq	448(%r12), %rbx
	subq	%rsi, %rbx
	jle	doGC2F9
check.2F4:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block check<E301> (ep<D36B>,s<D368>) */
	movq	8(%rdx), %rdi
	movq	16(%rdx), %r8
	movq	%rcx, %r9
	jmp	schedulerLoop.2F5
doGC2F9:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	%r10, %rdi
	movabsq	$retGC2F7, %r8
	jmp	_ASM_InvokeGC
	.text
run.69:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest2FC
	/* live= GP={%rcx %rdx %rbx} spilled=  */
retGC2FB:
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest2FC:
	movq	%r11, %r10
	movq	448(%r10), %r10
	subq	%rsi, %r10
	jle	doGC2FD
check.2FA:
	/* Liveout:  GP={%rax %rdi}  */
	/* block check<E305> (ep<D365>,_t<D363>,_t<D364>) */
	movq	$263, -8(%rsi)
	movabsq	$act.2F6, %r12
	movq	%r12, (%rsi)
	movq	8(%rbx), %r13
	movq	%r13, 8(%rsi)
	movq	16(%rbx), %r14
	movq	%r14, 16(%rsi)
	movq	%rsi, %r13
	addq	$32, %rsi
	movq	24(%r11), %r15
	movq	$12, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%rsi, %rcx
	addq	$16, %rsi
	movq	$1289, -8(%rsi)
	movl	(%r15), %r12d
	movl	%r12d, (%rsi)
	movq	%rcx, 8(%rsi)
	movl	16(%r15), %r14d
	movl	%r14d, 16(%rsi)
	movq	24(%r15), %r15
	movq	%r15, 24(%rsi)
	movq	%rsi, %r10
	addq	$40, %rsi
	movq	%r10, 24(%r11)
	movq	$20, -8(%rsi)
	movq	%r13, (%rsi)
	movq	16(%rbx), %r10
	movq	32(%r10), %r12
	movq	%r12, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	16(%rbx), %r13
	movq	%rcx, 32(%r13)
	movq	16(%rbx), %r14
	movq	$1, (%r14)
	movq	(%rdx), %r15
	movq	$1, %rcx
	movq	%rcx, %rax
	movq	%rdx, %rdi
	jmp	*%r15
doGC2FD:
	movq	$28, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%rsi, %rdx
	addq	$32, %rsi
	movq	%rdx, %rdi
	movabsq	$retGC2FB, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.53:
	movq	%rdi, %rcx
	jmp	gcTest301
	/* live= GP={%rcx} spilled=  */
retGC300:
	movq	(%rdi), %rcx
gcTest301:
	movq	%r11, %r10
	movq	448(%r10), %rdx
	subq	%rsi, %rdx
	jle	doGC302
check.2FE:
	/* Liveout:  GP={%rdi}  */
	/* block check<E307> (ep<D3C8>) */
	movq	$3, (%r11)
	movq	8(%rcx), %rdi
	jmp	dispatch.2FF
doGC302:
	movq	$12, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%rsi, %rbx
	addq	$16, %rsi
	movq	%rbx, %rdi
	movabsq	$retGC300, %r8
	jmp	_ASM_InvokeGC
	.text
k.5B:
	movq	%rax, %rcx
	movq	%rdi, %rdx
	jmp	gcTest305
	/* live= GP={%rcx %rdx} spilled=  */
retGC304:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest305:
	movq	%r11, %r12
	movq	448(%r12), %rbx
	subq	%rsi, %rbx
	jle	doGC306
check.303:
	/* Liveout:  GP={%rdi}  */
	/* block check<E30A> (ep<D3D8>,x<D3D7>) */
	movq	8(%rdx), %rdi
	jmp	letJoinK.53
doGC306:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	%r10, %rdi
	movabsq	$retGC304, %r8
	jmp	_ASM_InvokeGC
	.text
retGC30A:
	movq	16(%rdi), %rcx
	movl	8(%rdi), %edx
	movq	(%rdi), %rbx
gcTest30B:
	movq	%r11, %r13
	movq	448(%r13), %r10
	subq	%rsi, %r10
	jle	doGC30C
check.307:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block check<E30E> (ep<D400>,i<D402>,retK<D401>) */
	movq	%rbx, %rdi
	movq	%rdx, %r8
	incl	%r8d
	movq	%rcx, %r9
spin.308:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	cmpl	(%rbx), %edx
	jge	L30F
L_true30D:
then.30E:
	/* Liveout:  GP={%rdi}  */
	/* block then<D3FE> (retK<D3FD>) */
	movq	%rcx, %rdi
	jmp	letJoinK.59
L30F:
else.309:
	/* block else<D403> (ep<E30B>,i<E30C>,retK<E30D>) */
	jmp	gcTest30B
doGC30C:
	movq	$647, -8(%rsi)
	movq	%rbx, (%rsi)
	movl	%edx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%rsi, %r12
	addq	$32, %rsi
	movq	%r12, %rdi
	movabsq	$retGC30A, %r8
	jmp	_ASM_InvokeGC
	/* live= GP={%rcx %rdx %rbx} spilled=  */
	jmp	retGC30A
	.text
letJoinK.311:
	movq	%rdi, %rcx
	jmp	gcTest313
	/* live= GP={%rcx} spilled=  */
retGC312:
	movq	(%rdi), %rcx
gcTest313:
	movq	%r11, %r15
	movq	448(%r15), %rdx
	subq	%rsi, %rdx
	jle	doGC314
check.310:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block check<E310> (ep<D3B5>) */
	movq	$1545, -8(%rsi)
	movabsq	$letJoinK.59, %r10
	movq	%r10, (%rsi)
	movl	8(%rcx), %r12d
	movl	%r12d, 8(%rsi)
	movq	16(%rcx), %r13
	movq	%r13, 16(%rsi)
	movq	24(%rcx), %r14
	movq	%r14, 24(%rsi)
	movq	%rsi, %rbx
	addq	$40, %rsi
	movq	$10, -8(%rsi)
	movq	16(%rcx), %r10
	movl	$1, %edx
	movl	(%r10), %ecx
	shll	%cl, %edx
	movl	%edx, (%rsi)
	movq	%rsi, %r15
	addq	$16, %rsi
	movq	$133, -8(%rsi)
	movq	%r15, (%rsi)
	movabsq	$spin.308, %r13
	movq	%r13, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	(%r12), %rdi
	xorl	%r8d, %r8d
	movq	%rbx, %r9
	jmp	spin.308
doGC314:
	movq	$12, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%rsi, %r14
	addq	$16, %rsi
	movq	%r14, %rdi
	movabsq	$retGC312, %r8
	jmp	_ASM_InvokeGC
	.text
k.316:
	movq	%rax, %rcx
	movq	%rdi, %rdx
	jmp	gcTest318
	/* live= GP={%rcx %rdx} spilled=  */
retGC317:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest318:
	movq	%r11, %r12
	movq	448(%r12), %rbx
	subq	%rsi, %rbx
	jle	doGC319
check.315:
	/* Liveout:  GP={%rdi}  */
	/* block check<E313> (ep<D416>,x<D415>) */
	movq	8(%rdx), %rdi
	jmp	letJoinK.311
doGC319:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	%r10, %rdi
	movabsq	$retGC317, %r8
	jmp	_ASM_InvokeGC
	.text
k.68:
	movq	%rax, %rcx
	movq	%rdi, %rdx
gcTest320:
	movq	%r11, %rbx
	movq	448(%rbx), %rbx
	subq	%rsi, %rbx
	jle	doGC321
check.31A:
	/* block check<E316> (ep<D3AB>,x<D3A6>) */
	movq	$3, (%r11)
	movq	$1545, -8(%rsi)
	movabsq	$letJoinK.311, %r14
	movq	%r14, (%rsi)
	movl	8(%rdx), %r15d
	movl	%r15d, 8(%rsi)
	movq	16(%rdx), %rcx
	movq	%rcx, 16(%rsi)
	movq	32(%rdx), %rbx
	movq	%rbx, 24(%rsi)
	movq	%rsi, %r13
	addq	$40, %rsi
	movq	24(%rdx), %r12
	movq	8(%r12), %r10
	cmpq	$1, %r10
	jne	L322
S_case31B:
case.31C:
	/* Liveout:  GP={%rdi}  */
	/* block case<D40C> (ep<D40A>,letJoinK<D40B>) */
	movq	24(%rdx), %rcx
	movq	$1, (%rcx)
	movq	%r13, %rdi
	jmp	letJoinK.311
doGC321:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	movq	%rdx, %rdi
	movabsq	$retGC31F, %r8
	jmp	_ASM_InvokeGC
	/* live= GP={%rcx %rdx} spilled=  */
retGC31F:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
	jmp	gcTest320
L322:
	cmpq	$3, %r10
	jne	S_case31B
S_case31D:
case.31E:
	/* Liveout:  GP={%rax %rdi}  */
	/* block case<D412> (ep<D410>,letJoinK<D411>) */
	movq	24(%rdx), %r14
	movq	$1, 8(%r14)
	movq	$20, -8(%rsi)
	movabsq	$k.316, %rcx
	movq	%rcx, (%rsi)
	movq	%r13, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movq	$1, (%rsi)
	movq	%r15, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	24(%rdx), %r10
	movq	32(%r10), %rbx
	movq	8(%rbx), %r13
	movq	24(%rdx), %r14
	movq	%r13, 32(%r14)
	movq	(%rbx), %r10
	movq	(%r10), %r15
	movq	%r12, %rax
	movq	%r10, %rdi
	jmp	*%r15
	.text
retGC32B:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
	jmp	gcTest32C
L_true324:
then.326:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block then<D452> (ep<D450>,retK<D451>) */
	pause
	movq	%rdx, %rdi
	movq	%rcx, %r8
spinLp.32A:
	movq	%r8, %rcx
	movq	%rdi, %rdx
gcTest32C:
	movq	%r11, %rbx
	movq	448(%rbx), %rbx
	subq	%rsi, %rbx
	jle	doGC32D
check.323:
	/* block check<E319> (ep<D449>,retK<D44A>) */
	movq	(%rdx), %r12
	leaq	(%r12), %r10
	cmpl	$1, (%r10)
	je	L_true324
else.325:
	/* block else<D456> (ep<D454>,retK<D455>) */
	movq	$1, %r13
	movq	(%rdx), %r14
	lock
	xchgq	%r13, (%r14)
	cmpq	$1, %r13
	je	L_true327
else.328:
	/* Liveout:  GP={%rdi}  */
	/* block else<D45F> (retK<D45E>) */
	movq	%rcx, %rdi
	jmp	letJoinK.62
L_true327:
then.329:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block then<D45B> (ep<D459>,retK<D45A>) */
	movq	%rdx, %rdi
	movq	%rcx, %r8
	jmp	spinLp.32A
doGC32D:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	%r15, %rdi
	movabsq	$retGC32B, %r8
	jmp	_ASM_InvokeGC
	/* live= GP={%rcx %rdx} spilled=  */
	jmp	retGC32B
	.text
dispatch.2FF:
	movq	%rdi, %rcx
	jmp	gcTest330
	/* live= GP={%rcx} spilled=  */
retGC32F:
	movq	(%rdi), %rcx
gcTest330:
	movq	%r11, %rdx
	movq	448(%rdx), %rdx
	subq	%rsi, %rdx
	jle	doGC331
check.32E:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<E31B> (ep<D385>) */
	movq	$13583, -8(%rsi)
	movabsq	$letJoinK.62, %r10
	movq	%r10, (%rsi)
	movq	8(%rcx), %r12
	movq	%r12, 8(%rsi)
	movl	16(%rcx), %r13d
	movl	%r13d, 16(%rsi)
	movq	24(%rcx), %r14
	movq	%r14, 24(%rsi)
	movq	32(%rcx), %r15
	movq	%r15, 32(%rsi)
	movq	40(%rcx), %rdx
	movq	%rdx, 40(%rsi)
	movq	%rcx, 48(%rsi)
	movq	%rsi, %rbx
	addq	$64, %rsi
	movq	$12, -8(%rsi)
	movq	8(%rcx), %r12
	movq	%r12, (%rsi)
	movq	%rsi, %r10
	addq	$16, %rsi
	movq	$133, -8(%rsi)
	movq	%r10, (%rsi)
	movabsq	$spinLp.32A, %r14
	movq	%r14, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	(%r13), %rdi
	movq	%rbx, %r8
	jmp	spinLp.32A
doGC331:
	movq	$12, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%rsi, %r15
	addq	$16, %rsi
	movq	%r15, %rdi
	movabsq	$retGC32F, %r8
	jmp	_ASM_InvokeGC
	.text
k.333:
	movq	%rax, %rcx
	movq	%rdi, %rdx
	jmp	gcTest335
	/* live= GP={%rcx %rdx} spilled=  */
retGC334:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest335:
	movq	%r11, %r12
	movq	448(%r12), %rbx
	subq	%rsi, %rbx
	jle	doGC336
check.332:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block check<E31E> (ep<D483>,x<D480>) */
	movq	$3, (%r11)
	movq	8(%rdx), %rdi
	movq	24(%rdx), %r8
	movq	16(%rdx), %r9
	jmp	run.69
doGC336:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	%r10, %rdi
	movabsq	$retGC334, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.338:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest33A
	/* live= GP={%rcx %rdx} spilled=  */
retGC339:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest33A:
	movq	%r11, %rbx
	movq	448(%rbx), %rbx
	subq	%rsi, %rbx
	jle	doGC33B
check.337:
	/* Liveout:  GP={%rax %rdi}  */
	/* block check<E321> (ep<D47D>,k<D47A>) */
	movq	$36, -8(%rsi)
	movabsq	$k.333, %r14
	movq	%r14, (%rsi)
	movq	16(%rdx), %r15
	movq	%r15, 8(%rsi)
	movq	24(%rdx), %rbx
	movq	%rbx, 16(%rsi)
	movq	%rcx, 24(%rsi)
	movq	%rsi, %r13
	addq	$40, %rsi
	movq	$20, -8(%rsi)
	movq	$1, (%rsi)
	movq	%r13, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	8(%rdx), %r13
	movq	32(%r13), %r10
	movq	8(%r10), %r14
	movq	8(%rdx), %r15
	movq	%r14, 32(%r15)
	movq	(%r10), %r10
	movq	(%r10), %rcx
	movq	%r12, %rax
	movq	%r10, %rdi
	jmp	*%rcx
doGC33B:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	movq	%rdx, %rdi
	movabsq	$retGC339, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.33D:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest33F
	/* live= GP={%rcx %rdx} spilled=  */
retGC33E:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest33F:
	movq	%r11, %r12
	movq	448(%r12), %rbx
	subq	%rsi, %rbx
	jle	doGC340
check.33C:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<E324> (ep<D49B>,k<D49A>) */
	movq	8(%rdx), %rdi
	movq	%rcx, %r8
	jmp	letJoinK.338
doGC340:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	%r10, %rdi
	movabsq	$retGC33E, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.345:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest347
	/* live= GP={%rcx %rdx} spilled=  */
retGC346:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest347:
	movq	%r11, %r12
	movq	448(%r12), %rbx
	subq	%rsi, %rbx
	jle	doGC348
check.341:
	/* block check<E327> (ep<D475>,ite<D472>) */
	movq	8(%rcx), %r12
	movq	$1545, -8(%rsi)
	movabsq	$letJoinK.338, %r13
	movq	%r13, (%rsi)
	movq	24(%rdx), %r14
	movq	%r14, 8(%rsi)
	movq	32(%rdx), %r15
	movq	%r15, 16(%rsi)
	movq	%rcx, 24(%rsi)
	movq	%rsi, %r10
	addq	$40, %rsi
	cmpq	$1, %r12
	je	L349
L_true342:
then.344:
	/* Liveout:  GP={%r12 %r10 %r9 %r8 %rdi}  */
	/* block then<D497> (ep<D494>,c<D496>,letJoinK<D495>) */
	movq	$261, -8(%rsi)
	movabsq	$letJoinK.33D, %rcx
	movq	%rcx, (%rsi)
	movq	%r10, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	16(%rdx), %rbx
	movq	(%rbx), %rdi
	movq	(%r12), %r8
	movq	40(%rdx), %r9
	movq	8(%rdx), %r12
	jmp	wrap.144
doGC348:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	%r10, %rdi
	movabsq	$retGC346, %r8
	jmp	_ASM_InvokeGC
L349:
else.343:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block else<D4A7> (ep<D4A5>,letJoinK<D4A6>) */
	movq	%r10, %rdi
	movq	40(%rdx), %r8
	jmp	letJoinK.338
	.text
schedulerLoop.2F5:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
gcTest352:
	movq	%r11, %r10
	movq	448(%r10), %r10
	subq	%rsi, %r10
	jle	doGC353
check.34A:
	/* block check<E32B> (ep<D360>,self<D35A>,s<D35B>) */
	movq	$263, -8(%rsi)
	movabsq	$run.69, %r10
	movq	%r10, (%rsi)
	movq	%rbx, 8(%rsi)
	movq	%rdx, 16(%rsi)
	movq	%rsi, %r14
	addq	$32, %rsi
	movq	$5389, -8(%rsi)
	movabsq	$dispatch.2FF, %r12
	movq	%r12, (%rsi)
	movq	32(%rbx), %r13
	movq	%r13, 8(%rsi)
	movl	40(%rbx), %r15d
	movl	%r15d, 16(%rsi)
	movq	48(%rbx), %r10
	movq	%r10, 24(%rsi)
	movq	%rdx, 32(%rsi)
	movq	%r14, 40(%rsi)
	movq	%rsi, %r15
	addq	$56, %rsi
	cmpq	$1, %rcx
	jne	L_true34B
else.34C:
	/* Liveout:  GP={%rdi}  */
	/* block else<D4B6> (dispatch<D4B5>) */
	movq	%r15, %rdi
	jmp	dispatch.2FF
doGC353:
	movq	$647, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%rsi, %rdx
	addq	$32, %rsi
	movq	%rdx, %rdi
	movabsq	$retGC351, %r8
	jmp	_ASM_InvokeGC
	/* live= GP={%rcx %rdx %rbx} spilled=  */
retGC351:
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
	jmp	gcTest352
L_true34B:
then.34D:
	/* block then<D468> (ep<D464>,self<D467>,s<D466>,run<D465>) */
	cmpq	$1, (%rcx)
	jne	L354
L_true34E:
then.350:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block then<D470> (ep<D46C>,self<D46F>,run<D46E>,s<D46D>) */
	movq	$6925, -8(%rsi)
	movabsq	$letJoinK.345, %r10
	movq	%r10, (%rsi)
	movq	8(%rbx), %r12
	movq	%r12, 8(%rsi)
	movq	24(%rbx), %r13
	movq	%r13, 16(%rsi)
	movq	%rdx, 24(%rsi)
	movq	%r14, 32(%rsi)
	movq	8(%rcx), %r14
	movq	%r14, 40(%rsi)
	movq	%rsi, %r15
	addq	$56, %rsi
	movq	16(%rbx), %rcx
	movq	(%rcx), %rdi
	movq	%r15, %r8
	movq	8(%rbx), %r9
	jmp	get_D_ite.EB
L354:
else.34F:
	/* Liveout:  GP={%rax %rdi}  */
	/* block else<D4AF> (ep<D4AE>) */
	movq	$12, -8(%rsi)
	movabsq	$tag138, %r12
	movq	%r12, (%rsi)
	movq	%rsi, %r13
	addq	$16, %rsi
	movq	8(%rbx), %r12
	movq	(%r12), %r14
	movq	%r13, %rax
	movq	%r12, %rdi
	jmp	*%r14
	.text
initWorker_P_.356:
	movq	%rax, %rdx
	movq	%rdi, %rbx
	jmp	gcTest358
	/* live= GP={%rdx %rbx} spilled=  */
retGC357:
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest358:
	movq	%r11, %r14
	movq	448(%r14), %r15
	subq	%rsi, %r15
	jle	doGC359
check.355:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block check<E32E> (ep<D351>,_wild<D34C>) */
	movq	$3, (%r11)
	movq	$10, -8(%rsi)
	movl	$1, (%rsi)
	movq	%rsi, %rcx
	addq	$16, %rsi
	movq	%r11, %r15
	movq	%rsi, 128(%r15)
	movq	%rax, %rdx
	movq	%rdx, -56(%rbp)
	movq	%rdi, %r14
	movq	%r8, %r13
	movq	%r9, %r12
	movq	%r11, %r10
	movq	%r10, -64(%rbp)
	movq	%r11, %r10
	movq	%r10, %rdi
	movq	%rcx, %rsi
	call	_PromoteObj
	movq	%rax, %rcx
	movq	-56(%rbp), %rax
	movq	%r14, %rdi
	movq	%r13, %r8
	movq	%r12, %r9
	movq	-64(%rbp), %r11
	movq	128(%r15), %rsi
	movq	$12047, -8(%rsi)
	movabsq	$schedulerLoop.2F5, %r13
	movq	%r13, (%rsi)
	movq	8(%rbx), %r14
	movq	%r14, 8(%rsi)
	movq	16(%rbx), %rdx
	movq	%rdx, 16(%rsi)
	movq	24(%rbx), %r10
	movq	%r10, 24(%rsi)
	movq	32(%rbx), %r12
	movq	%r12, 32(%rsi)
	movl	$15, 40(%rsi)
	movq	%rcx, 48(%rsi)
	movq	%rsi, %r12
	addq	$64, %rsi
	movq	%r12, %rdi
	movq	%r11, %r8
	movq	$1, %r9
	jmp	schedulerLoop.2F5
doGC359:
	movq	$20, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	%r13, %rdi
	movabsq	$retGC357, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.6C:
	movq	%rdi, %rcx
	jmp	gcTest35C
	/* live= GP={%rcx} spilled=  */
retGC35B:
	movq	(%rdi), %rcx
gcTest35C:
	movq	%r11, %r13
	movq	448(%r13), %rdx
	subq	%rsi, %rdx
	jle	doGC35D
check.35A:
	/* Liveout:  GP={%rdi}  */
	/* block check<E330> (ep<D4C3>) */
	movq	8(%rcx), %rbx
	movq	(%rbx), %r10
	movq	%rbx, %rdi
	jmp	*%r10
doGC35D:
	movq	$12, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%rsi, %r12
	addq	$16, %rsi
	movq	%r12, %rdi
	movabsq	$retGC35B, %r8
	jmp	_ASM_InvokeGC
	.text
k.6B:
	movq	%rax, %rcx
	movq	%rdi, %rdx
	jmp	gcTest360
	/* live= GP={%rcx %rdx} spilled=  */
retGC35F:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest360:
	movq	%r11, %r12
	movq	448(%r12), %rbx
	subq	%rsi, %rbx
	jle	doGC361
check.35E:
	/* Liveout:  GP={%rdi}  */
	/* block check<E333> (ep<D4D4>,x<D4D3>) */
	movq	8(%rdx), %rdi
	jmp	letJoinK.6C
doGC361:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	%r10, %rdi
	movabsq	$retGC35F, %r8
	jmp	_ASM_InvokeGC
	.text
retGC365:
	movq	8(%rdi), %rdx
	movq	(%rdi), %r12
	jmp	gcTest366
L_true368:
	movq	-56(%rbp), %rdx
then.364:
	/* block then<D504> (ep<E334>,retK<E335>) */
gcTest366:
	movq	%r11, %r13
	movq	448(%r13), %r14
	subq	%rsi, %r14
	jle	doGC367
check.362:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<E336> (ep<D502>,retK<D503>) */
	pause
	movq	%r12, %rdi
	movq	%rdx, %r8
lp.363:
	movq	%r8, %rbx
	movq	%rbx, -56(%rbp)
	movq	%rdi, %r12
	movq	(%r12), %r15
	movq	384(%r15), %r10
	movq	%r10, -64(%rbp)
	movq	$28, -8(%rsi)
	movq	8(%r12), %rcx
	movq	%rcx, (%rsi)
	movq	16(%r12), %rdx
	movq	%rdx, 8(%rsi)
	movq	-64(%rbp), %r13
	movq	%r13, 16(%rsi)
	movq	%rsi, %rcx
	addq	$32, %rsi
	movq	%r11, %r15
	movq	%rsi, 128(%r15)
	movq	%rax, %r14
	movq	%r14, -72(%rbp)
	movq	%rdi, %rdx
	movq	%rdx, -80(%rbp)
	movq	%r8, %r14
	movq	%r9, %r13
	movq	%r11, %rbx
	movq	%r11, %r10
	movq	%r10, %rdi
	movq	%rcx, %rsi
	call	_PromoteObj
	movq	%rax, %rcx
	movq	-72(%rbp), %rax
	movq	-80(%rbp), %rdi
	movq	%r14, %r8
	movq	%r13, %r9
	movq	%rbx, %r11
	movq	128(%r15), %rsi
	movq	(%r12), %rbx
	leaq	384(%rbx), %rdx
	movq	-64(%rbp), %rax
	movq	(%r12), %r10
	lock
	cmpxchgq	%rcx, 384(%r10)
	movq	%rax, %r13
	cmpq	-64(%rbp), %r13
	jne	L_true368
	movq	-56(%rbp), %rbx
	movq	%r12, %rcx
else.369:
	/* block else<D508> (ep<D506>,retK<D507>) */
	movq	(%rcx), %r13
	movq	16(%r13), %r12
	cmpq	$1, %r12
	je	letJoinK.36B
L36F:
	cmpq	$3, %r12
	je	S_case36C
S_case36A:
	jmp	letJoinK.36B
S_case36C:
	movq	%rbx, -56(%rbp)
case.36D:
	/* block case<D514> (ep<D512>,retK<D513>) */
	movq	%rax, %r12
	movq	%rdi, %rbx
	movq	%r8, %r15
	movq	%r9, %r14
	movq	%rsi, %r13
	movq	%r11, %r10
	movq	%r10, -64(%rbp)
	movq	(%rcx), %rcx
	movq	%rcx, %rdi
	call	_VProcWake
	movq	%r12, %rax
	movq	%rbx, %rdi
	movq	%r15, %r8
	movq	%r14, %r9
	movq	%r13, %rsi
	movq	-64(%rbp), %r11
	movq	-56(%rbp), %rbx
letJoinK.36B:
	/* Liveout:  GP={%rdi}  */
	/* block letJoinK<D50D> (retK<D50C>) */
	movq	%rbx, %rdi
	jmp	letJoinK.F
doGC367:
	movq	$20, -8(%rsi)
	movq	%r12, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	%r10, %rdi
	movabsq	$retGC365, %r8
	jmp	_ASM_InvokeGC
	.text
preempt.11:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest375
	/* live= GP={%rcx %rdx} spilled=  */
retGC374:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest375:
	movq	%r11, %r14
	movq	448(%r14), %rbx
	subq	%rsi, %rbx
	jle	doGC376
check.370:
	/* block check<E339> (ep<D524>,retK<D525>) */
	movq	(%rdx), %r12
	movq	448(%r12), %r10
	movq	(%rdx), %r15
	leaq	448(%r15), %r14
	movq	%r10, %rax
	xorq	%rbx, %rbx
	movq	(%rdx), %r12
	lock
	cmpxchgq	%rbx, 448(%r12)
	movq	%rax, %r13
	cmpq	%r10, %r13
	je	L377
L_true371:
then.373:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block then<D532> (ep<D530>,retK<D531>) */
	pause
	movq	%rdx, %rdi
	movq	%rcx, %r8
	jmp	preempt.11
doGC376:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	%r13, %rdi
	movabsq	$retGC374, %r8
	jmp	_ASM_InvokeGC
L377:
else.372:
	/* Liveout:  GP={%rdi}  */
	/* block else<D536> (retK<D535>) */
	movq	%rcx, %rdi
	jmp	letJoinK.10
	.text
letJoinK.37C:
	movq	%rdi, %rcx
	jmp	gcTest37E
	/* live= GP={%rcx} spilled=  */
retGC37D:
	movq	(%rdi), %rcx
gcTest37E:
	movq	%r11, %rdx
	movq	448(%rdx), %rdx
	subq	%rsi, %rdx
	jle	doGC37F
check.378:
	/* block check<E33B> (ep<D340>) */
	movq	$1289, -8(%rsi)
	movq	48(%rcx), %r14
	movl	224(%r14), %r15d
	movl	%r15d, (%rsi)
	movq	32(%rcx), %rdx
	movq	8(%rdx), %rbx
	movq	%rbx, 8(%rsi)
	movq	32(%rcx), %r10
	movl	16(%r10), %r12d
	movl	%r12d, 16(%rsi)
	movq	32(%rcx), %r13
	movq	24(%r13), %r14
	movq	%r14, 24(%rsi)
	movq	%rsi, %r12
	addq	$40, %rsi
	movq	$44, -8(%rsi)
	movabsq	$initWorker_P_.356, %r15
	movq	%r15, (%rsi)
	movq	8(%rcx), %rdx
	movq	%rdx, 8(%rsi)
	movq	16(%rcx), %rbx
	movq	%rbx, 16(%rsi)
	movq	24(%rcx), %r10
	movq	%r10, 24(%rsi)
	movq	40(%rcx), %r13
	movq	%r13, 32(%rsi)
	movq	%rsi, %r10
	addq	$48, %rsi
	movq	$3, (%r11)
	movq	$263, -8(%rsi)
	movabsq	$letJoinK.12, %r14
	movq	%r14, (%rsi)
	movq	56(%rcx), %r15
	movq	%r15, 8(%rsi)
	movq	%r11, 16(%rsi)
	movq	%rsi, %rbx
	addq	$32, %rsi
	cmpq	48(%rcx), %r11
	jne	L380
L_true379:
	movq	%r11, %r13
then.37B:
	/* Liveout:  GP={%rdi}  */
	/* block then<D4E7> (fls<D4E6>,initWorker'<D4E5>,vp<D4E4>,letJoinK<D4E3>) */
	movq	$28, -8(%rsi)
	movq	%r12, (%rsi)
	movq	%r10, 8(%rsi)
	movq	88(%r13), %r14
	movq	%r14, 16(%rsi)
	movq	%rsi, %r12
	addq	$32, %rsi
	movq	%r12, 88(%r13)
	movq	%rbx, %rdi
	jmp	letJoinK.12
doGC37F:
	movq	$12, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%rsi, %r15
	addq	$16, %rsi
	movq	%r15, %rdi
	movabsq	$retGC37D, %r8
	jmp	_ASM_InvokeGC
L380:
else.37A:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block else<D4EF> (ep<D4EB>,fls<D4EE>,initWorker'<D4ED>,letJoinK<D4EC>) */
	movq	$775, -8(%rsi)
	movq	48(%rcx), %r13
	movq	%r13, (%rsi)
	movq	%r12, 8(%rsi)
	movq	%r10, 16(%rsi)
	movq	%rsi, %rdx
	addq	$32, %rsi
	movq	$133, -8(%rsi)
	movq	%rdx, (%rsi)
	movabsq	$lp.363, %r15
	movq	%r15, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	$519, -8(%rsi)
	movabsq	$letJoinK.F, %rdx
	movq	%rdx, (%rsi)
	movq	48(%rcx), %r10
	movq	%r10, 8(%rsi)
	movq	%rbx, 16(%rsi)
	movq	%rsi, %rcx
	addq	$32, %rsi
	movq	(%r14), %rdi
	movq	%rcx, %r8
	jmp	lp.363
	.text
spawnWorker.382:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest384
	/* live= GP={%r10 %rcx %rdx %rbx} spilled=  */
retGC383:
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest384:
	movq	%r11, %r12
	movq	448(%r12), %r12
	subq	%rsi, %r12
	jle	doGC385
check.381:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block check<E340> (ep<D334>,dst<D335>,retK<D336>,exh<D337>) */
	movq	$24337, -8(%rsi)
	movabsq	$letJoinK.37C, %r14
	movq	%r14, (%rsi)
	movq	(%rbx), %r15
	movq	%r15, 8(%rsi)
	movq	8(%rbx), %r10
	movq	%r10, 16(%rsi)
	movq	32(%rbx), %r12
	movq	%r12, 24(%rsi)
	movq	40(%rbx), %r13
	movq	%r13, 32(%rsi)
	movq	48(%rbx), %r14
	movq	%r14, 40(%rsi)
	movq	%rdx, 48(%rsi)
	movq	%rcx, 56(%rsi)
	movq	%rsi, %r13
	addq	$72, %rsi
	movq	24(%rbx), %r15
	movq	(%r15), %rdi
	movq	16(%rbx), %rcx
	movq	(%rcx), %r8
	movq	%r13, %r9
	jmp	new.FB
doGC385:
	movq	$1673, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %rdx
	addq	$40, %rsi
	movq	%rdx, %rdi
	movabsq	$retGC383, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.388:
	movq	%rdi, %rcx
	jmp	gcTest38A
	/* live= GP={%rcx} spilled=  */
retGC389:
	movq	(%rdi), %rcx
gcTest38A:
	movq	%r11, %r12
	movq	448(%r12), %rdx
	subq	%rsi, %rdx
	jle	doGC38B
check.386:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block check<E342> (ep<D555>) */
	movq	8(%rcx), %rbx
	movq	(%rbx), %rdi
	movq	24(%rcx), %r8
	movq	16(%rcx), %r9
	jmp	lp.387
doGC38B:
	movq	$12, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%rsi, %r10
	addq	$16, %rsi
	movq	%r10, %rdi
	movabsq	$retGC389, %r8
	jmp	_ASM_InvokeGC
	.text
lp.387:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest392
	/* live= GP={%rcx %rdx %rbx} spilled=  */
retGC391:
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest392:
	movq	%r11, %r14
	movq	448(%r14), %r10
	subq	%rsi, %r10
	jle	doGC393
check.38C:
	/* block check<E346> (ep<D544>,vps<D545>,retK<D546>) */
	cmpq	$1, %rdx
	je	L394
L_true38D:
then.38F:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block then<D54E> (ep<D54B>,vps<D54D>,retK<D54C>) */
	movq	$133, -8(%rsi)
	movq	%rbx, (%rsi)
	movabsq	$lp.387, %r13
	movq	%r13, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	$1801, -8(%rsi)
	movabsq	$letJoinK.388, %r15
	movq	%r15, (%rsi)
	movq	%r12, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	8(%rdx), %rcx
	movq	%rcx, 24(%rsi)
	movq	%rsi, %r14
	addq	$40, %rsi
	movq	8(%rbx), %r10
	movq	(%r10), %rdi
	movq	(%rdx), %r12
	movq	(%r12), %r8
	movq	%r14, %r9
	movq	(%rbx), %r10
	jmp	spawnWorker.382
doGC393:
	movq	$28, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%rsi, %r13
	addq	$32, %rsi
	movq	%r13, %rdi
	movabsq	$retGC391, %r8
	jmp	_ASM_InvokeGC
L394:
else.38E:
	/* Liveout:  GP={%rdi}  */
	/* block else<D563> (retK<D562>) */
	movq	%rcx, %rdi
	jmp	letJoinK.390
	.text
letJoinK.396:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest398
	/* live= GP={%rcx %rdx %rbx} spilled=  */
retGC397:
	movl	16(%rdi), %ecx
	movl	8(%rdi), %edx
	movq	(%rdi), %rbx
gcTest398:
	movq	%r11, %r10
	movq	448(%r10), %r10
	subq	%rsi, %r10
	jle	doGC399
check.395:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<E34A> (ep<D5CA>,_t<D5C8>,_t<D5C9>) */
	movq	$10, -8(%rsi)
	leal	(%rdx,%rcx,1), %r13d
	movl	%r13d, (%rsi)
	movq	%rsi, %r12
	addq	$16, %rsi
	movq	8(%rbx), %r14
	movq	(%r14), %r15
	movq	%r14, %rdi
	movq	%r12, %r8
	jmp	*%r15
doGC399:
	movq	$135, -8(%rsi)
	movq	%rbx, (%rsi)
	movl	%edx, 8(%rsi)
	movl	%ecx, 16(%rsi)
	movq	%rsi, %rcx
	addq	$32, %rsi
	movq	%rcx, %rdi
	movabsq	$retGC397, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.39E:
	movq	%r8, %rcx
	movq	%rdi, %rbx
	jmp	gcTest3A0
	/* live= GP={%rcx} spilled= GP={%r~1}  */
retGC39F:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rbx
gcTest3A0:
	movq	%r11, %r15
	movq	448(%r15), %rdx
	subq	%rsi, %rdx
	jle	doGC3A1
	movq	%rbx, -56(%rbp)
check.39A:
	/* flushLoads */
	/* block check<E34D> (ep<D5E1>,v_1<D5DE>) */
	movq	%r11, %rbx
	movq	%rsi, 128(%rbx)
	movq	%rax, %r10
	movq	%r10, -64(%rbp)
	movq	%rdi, %r12
	movq	%r8, %r13
	movq	%r9, %r14
	movq	%r11, %r15
	movq	%r11, %r10
	movq	%r10, %rdi
	movq	%rcx, %rsi
	call	_PromoteObj
	movq	%rax, %rcx
	movq	-64(%rbp), %rax
	movq	%r12, %rdi
	movq	%r13, %r8
	movq	%r14, %r9
	movq	%r15, %r11
	movq	128(%rbx), %rsi
	movq	-56(%rbp), %r12
	movq	24(%r12), %rdx
	movq	%rcx, 8(%rdx)
	movq	%r11, %rbx
	movq	%rsi, 128(%rbx)
	movq	%rax, %r12
	movq	%rdi, %r13
	movq	%r8, %r14
	movq	%r14, -64(%rbp)
	movq	%r9, %r14
	movq	%r11, %r15
	movq	%r11, %rcx
	movq	%rcx, %rdi
	movq	-56(%rbp), %rcx
	movq	8(%rcx), %rdx
	movq	%rdx, %rsi
	call	_PromoteObj
	movq	%rax, %rdx
	movq	%r12, %rax
	movq	%r13, %rdi
	movq	-64(%rbp), %r8
	movq	%r14, %r9
	movq	%r15, %r11
	movq	128(%rbx), %rsi
	movq	-56(%rbp), %rbx
	movl	$1, %ebx
	lock
	xaddl	%ebx, (%rdx)
	cmpl	$2, %ebx
	jne	L3A2
L_true39B:
	movq	-56(%rbp), %rdx
then.39D:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block then<D5ED> (ep<D5EC>) */
	movq	16(%rdx), %rdi
	movq	24(%rdx), %r10
	movq	(%r10), %rbx
	movl	(%rbx), %r8d
	movq	24(%rdx), %r13
	movq	8(%r13), %r12
	movl	(%r12), %r9d
	jmp	letJoinK.396
doGC3A1:
	movq	$20, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	%r14, %rdi
	movabsq	$retGC39F, %r8
	jmp	_ASM_InvokeGC
L3A2:
else.39C:
	/* Liveout:  GP={%rax %rdi}  */
	/* block else<D5F7> () */
	movq	$3, (%r11)
	movq	32(%r11), %r12
	movq	8(%r12), %r13
	movq	%r13, 32(%r11)
	movq	(%r12), %r14
	movq	(%r14), %r15
	movq	$1, %rcx
	movq	%rcx, %rax
	movq	%r14, %rdi
	jmp	*%r15
	.text
slowClone_1.3A5:
	movq	%rax, %r15
	movq	%rdi, %r14
	jmp	gcTest3A7
	/* live= GP={%r15 %r14} spilled=  */
retGC3A6:
	movq	8(%rdi), %r15
	movq	(%rdi), %r14
gcTest3A7:
	movq	%r11, %rbx
	movq	448(%rbx), %r10
	subq	%rsi, %r10
	jle	doGC3A8
check.3A3:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block check<E350> (ep<D5D4>,_unit<D5D1>) */
	movq	%r11, %rbx
	movq	%rsi, 128(%rbx)
	movq	%rax, %r12
	movq	%rdi, %r13
	movq	%r13, -56(%rbp)
	movq	%r8, %r13
	movq	%r9, %r15
	movq	%r11, %rcx
	movq	%rcx, -64(%rbp)
	movq	%r11, %rcx
	movq	%rcx, %rdi
	movq	32(%r14), %rdx
	movq	%rdx, %rsi
	call	_PromoteObj
	movq	%rax, %rcx
	movq	%r12, %rax
	movq	-56(%rbp), %rdi
	movq	%r13, %r8
	movq	%r15, %r9
	movq	-64(%rbp), %r11
	movq	128(%rbx), %rsi
	movq	$10, -8(%rsi)
	movq	16(%r14), %r10
	movl	(%r10), %edx
	subl	$2, %edx
	movl	%edx, (%rsi)
	movq	%rsi, %rbx
	addq	$16, %rsi
	movq	$1801, -8(%rsi)
	movabsq	$letJoinK.39E, %r12
	movq	%r12, (%rsi)
	movq	40(%r14), %r13
	movq	%r13, 8(%rsi)
	movq	48(%r14), %r15
	movq	%r15, 16(%rsi)
	movq	%rcx, 24(%rsi)
	movq	%rsi, %r10
	addq	$40, %rsi
	movq	8(%r14), %rcx
	movq	(%rcx), %rdi
	movq	%rbx, %r8
	movq	%r10, %r9
	movq	24(%r14), %r10
	jmp	pFib.3A4
doGC3A8:
	movq	$20, -8(%rsi)
	movq	%r14, (%rsi)
	movq	%r15, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	movq	%rdx, %rdi
	movabsq	$retGC3A6, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.3AA:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest3AC
	/* live= GP={%rcx %rdx} spilled=  */
retGC3AB:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest3AC:
	movq	%r11, %r13
	movq	448(%r13), %rbx
	subq	%rsi, %rbx
	jle	doGC3AD
check.3A9:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block check<E353> (ep<D679>,v_1<D676>) */
	movq	8(%rdx), %rdi
	movq	16(%rdx), %r10
	movl	(%r10), %r8d
	movl	(%rcx), %r9d
	jmp	letJoinK.396
doGC3AD:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	%r12, %rdi
	movabsq	$retGC3AB, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.3B6:
	movq	%rax, %r15
	movq	%rdi, %r14
gcTest3B8:
	movq	%r11, %r10
	movq	448(%r10), %r12
	subq	%rsi, %r12
	jle	doGC3B9
check.3AE:
	/* block check<E356> (ep<D64E>,notStolen_1<D646>) */
	cmpq	$1, %r15
	je	S_case3AF
	cmpq	$3, %r15
	je	S_case3B1
S_case3AF:
	movq	%r14, -64(%rbp)
case.3B0:
	/* flushLoads */
	/* block case<D652> (ep<D651>) */
	movq	%r11, %rbx
	movq	%rsi, 128(%rbx)
	movq	%rax, %r13
	movq	%r13, -72(%rbp)
	movq	%rdi, %r12
	movq	%r8, %r13
	movq	%r9, %r14
	movq	%r11, %r15
	movq	%r11, %rcx
	movq	%rcx, %rdi
	movq	-64(%rbp), %rcx
	movq	32(%rcx), %rdx
	movq	%rdx, %rsi
	call	_PromoteObj
	movq	%rax, %rdx
	movq	%rdx, -56(%rbp)
	movq	-72(%rbp), %rax
	movq	%r12, %rdi
	movq	%r13, %r8
	movq	%r14, %r9
	movq	%r15, %r11
	movq	128(%rbx), %rsi
	movq	%r11, %rbx
	movq	%rsi, 128(%rbx)
	movq	%rax, %r10
	movq	%r10, -72(%rbp)
	movq	%rdi, %r12
	movq	%r8, %r13
	movq	%r9, %r14
	movq	%r11, %r15
	movq	%r11, %rcx
	movq	%rcx, %rdi
	movq	-64(%rbp), %rcx
	movq	56(%rcx), %rdx
	movq	%rdx, %rsi
	call	_PromoteObj
	movq	%rax, %rdx
	movq	-72(%rbp), %rax
	movq	%r12, %rdi
	movq	%r13, %r8
	movq	%r14, %r9
	movq	%r15, %r11
	movq	128(%rbx), %rsi
	movq	-64(%rbp), %r10
	movq	-56(%rbp), %r12
	movq	%rdx, (%r12)
	movq	%r11, %r12
	movq	%rsi, 128(%r12)
	movq	%rax, %r13
	movq	%rdi, %r14
	movq	%r8, %r15
	movq	%r9, %rbx
	movq	%r11, %rcx
	movq	%rcx, -72(%rbp)
	movq	%r11, %r10
	movq	%r10, %rdi
	movq	-64(%rbp), %rdx
	movq	40(%rdx), %rcx
	movq	%rcx, %rsi
	call	_PromoteObj
	movq	%rax, %r10
	movq	%r13, %rax
	movq	%r14, %rdi
	movq	%r15, %r8
	movq	%rbx, %r9
	movq	-72(%rbp), %r11
	movq	128(%r12), %rsi
	movq	-64(%rbp), %r12
	movl	$1, %edx
	lock
	xaddl	%edx, (%r10)
	cmpl	$2, %edx
	jne	L3BA
L_true3B3:
	movq	-56(%rbp), %r14
	movq	-64(%rbp), %r13
then.3B5:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block then<D65F> (ep<D65D>,r<D65E>) */
	movq	48(%r13), %rdi
	movq	(%r14), %rcx
	movl	(%rcx), %r8d
	movq	8(%r14), %rdx
	movl	(%rdx), %r9d
	jmp	letJoinK.396
doGC3B9:
	movq	$20, -8(%rsi)
	movq	%r14, (%rsi)
	movq	%r15, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	%rbx, %rdi
	movabsq	$retGC3B7, %r8
	jmp	_ASM_InvokeGC
	/* live= GP={%r15 %r14} spilled=  */
retGC3B7:
	movq	8(%rdi), %r15
	movq	(%rdi), %r14
	jmp	gcTest3B8
S_case3B1:
case.3B2:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block case<D670> (ep<D66F>) */
	movq	$10, -8(%rsi)
	movq	16(%r14), %rdx
	movl	(%rdx), %ebx
	subl	$2, %ebx
	movl	%ebx, (%rsi)
	movq	%rsi, %rcx
	addq	$16, %rsi
	movq	$775, -8(%rsi)
	movabsq	$letJoinK.3AA, %r12
	movq	%r12, (%rsi)
	movq	48(%r14), %r13
	movq	%r13, 8(%rsi)
	movq	56(%r14), %r15
	movq	%r15, 16(%rsi)
	movq	%rsi, %r10
	addq	$32, %rsi
	movq	8(%r14), %rdx
	movq	(%rdx), %rdi
	movq	%rcx, %r8
	movq	%r10, %r9
	movq	24(%r14), %r10
	jmp	pFib.3A4
L3BA:
else.3B4:
	/* Liveout:  GP={%rax %rdi}  */
	/* block else<D667> () */
	movq	$3, (%r11)
	movq	32(%r11), %r10
	movq	8(%r10), %r12
	movq	%r12, 32(%r11)
	movq	(%r10), %r13
	movq	(%r13), %r14
	movq	$1, %r15
	movq	%r15, %rax
	movq	%r13, %rdi
	jmp	*%r14
	.text
letJoinK.3BC:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest3BE
	/* live= GP={%r10 %rcx %rdx %rbx} spilled=  */
retGC3BD:
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest3BE:
	movq	%r11, %r14
	movq	448(%r14), %r12
	subq	%rsi, %r12
	jle	doGC3BF
check.3BB:
	/* Liveout:  GP={%r9 %r8 %rax %rdi}  */
	/* block check<E35B> (ep<D643>,unused<D639>,removeFn<D63A>,unused<D63B>) */
	movq	$68, -8(%rsi)
	movabsq	$letJoinK.3B6, %r14
	movq	%r14, (%rsi)
	movq	8(%rbx), %r15
	movq	%r15, 8(%rsi)
	movq	16(%rbx), %rdx
	movq	%rdx, 16(%rsi)
	movq	24(%rbx), %r10
	movq	%r10, 24(%rsi)
	movq	32(%rbx), %r12
	movq	%r12, 32(%rsi)
	movq	40(%rbx), %r13
	movq	%r13, 40(%rsi)
	movq	48(%rbx), %r14
	movq	%r14, 48(%rsi)
	movq	64(%rbx), %r15
	movq	%r15, 56(%rsi)
	movq	%rsi, %r13
	addq	$72, %rsi
	movq	8(%rcx), %rdx
	movq	24(%rbx), %r12
	movq	56(%rbx), %r10
	movq	(%rcx), %rbx
	movq	%r12, %r9
	movq	%r13, %r8
	movq	%r10, %rax
	movq	%rbx, %rdi
	jmp	*%rdx
doGC3BF:
	movq	$36, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %r13
	addq	$40, %rsi
	movq	%r13, %rdi
	movabsq	$retGC3BD, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.3C1:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest3C3
	/* live= GP={%rcx %rdx} spilled=  */
retGC3C2:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest3C3:
	movq	%r11, %rbx
	movq	448(%rbx), %rbx
	subq	%rsi, %rbx
	jle	doGC3C4
check.3C0:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block check<E35E> (ep<D636>,v_0<D62D>) */
	movq	$65299, -8(%rsi)
	movabsq	$letJoinK.3BC, %r12
	movq	%r12, (%rsi)
	movq	16(%rdx), %r13
	movq	%r13, 8(%rsi)
	movq	24(%rdx), %r14
	movq	%r14, 16(%rsi)
	movq	32(%rdx), %r15
	movq	%r15, 24(%rsi)
	movq	40(%rdx), %rbx
	movq	%rbx, 32(%rsi)
	movq	48(%rdx), %r10
	movq	%r10, 40(%rsi)
	movq	56(%rdx), %r12
	movq	%r12, 48(%rsi)
	movq	64(%rdx), %r13
	movq	%r13, 56(%rsi)
	movq	%rcx, 64(%rsi)
	movq	%rsi, %r10
	addq	$80, %rsi
	movq	8(%rdx), %r14
	movq	(%r14), %rdi
	movq	$1, %r8
	movq	%r10, %r9
	movq	32(%rdx), %r10
	jmp	current_D_work_D_group.169
doGC3C4:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	%r15, %rdi
	movabsq	$retGC3C2, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.3C6:
	movq	%rax, %rcx
	movq	%rdi, %rdx
	jmp	gcTest3C8
	/* live= GP={%rcx %rdx} spilled=  */
retGC3C7:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest3C8:
	movq	%r11, %r12
	movq	448(%r12), %rbx
	subq	%rsi, %rbx
	jle	doGC3C9
check.3C5:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block check<E361> (ep<D625>,_wild<D61C>) */
	movq	$10, -8(%rsi)
	movq	24(%rdx), %r12
	movl	(%r12), %r10d
	decl	%r10d
	movl	%r10d, (%rsi)
	movq	%rsi, %r12
	addq	$16, %rsi
	movq	$65299, -8(%rsi)
	movabsq	$letJoinK.3C1, %r14
	movq	%r14, (%rsi)
	movq	8(%rdx), %r15
	movq	%r15, 8(%rsi)
	movq	16(%rdx), %rcx
	movq	%rcx, 16(%rsi)
	movq	24(%rdx), %rbx
	movq	%rbx, 24(%rsi)
	movq	32(%rdx), %r10
	movq	%r10, 32(%rsi)
	movq	40(%rdx), %r13
	movq	%r13, 40(%rsi)
	movq	48(%rdx), %r14
	movq	%r14, 48(%rsi)
	movq	56(%rdx), %r15
	movq	%r15, 56(%rsi)
	movq	64(%rdx), %rcx
	movq	%rcx, 64(%rsi)
	movq	%rsi, %r13
	addq	$80, %rsi
	movq	16(%rdx), %rbx
	movq	(%rbx), %rdi
	movq	%r12, %r8
	movq	%r13, %r9
	movq	32(%rdx), %r10
	jmp	pFib.3A4
doGC3C9:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	%r10, %rdi
	movabsq	$retGC3C7, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.3CB:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest3CD
	/* live= GP={%r10 %rcx %rdx %rbx} spilled=  */
retGC3CC:
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest3CD:
	movq	%r11, %r12
	movq	448(%r12), %r12
	subq	%rsi, %r12
	jle	doGC3CE
check.3CA:
	/* Liveout:  GP={%r9 %r8 %rax %rdi}  */
	/* block check<E366> (ep<D619>,spawnFn<D60F>,unused<D610>,unused<D611>) */
	movq	$76, -8(%rsi)
	movabsq	$letJoinK.3C6, %r14
	movq	%r14, (%rsi)
	movq	8(%rbx), %r15
	movq	%r15, 8(%rsi)
	movq	16(%rbx), %rcx
	movq	%rcx, 16(%rsi)
	movq	24(%rbx), %r10
	movq	%r10, 24(%rsi)
	movq	32(%rbx), %r12
	movq	%r12, 32(%rsi)
	movq	40(%rbx), %r13
	movq	%r13, 40(%rsi)
	movq	48(%rbx), %r14
	movq	%r14, 48(%rsi)
	movq	56(%rbx), %r15
	movq	%r15, 56(%rsi)
	movq	64(%rbx), %rcx
	movq	%rcx, 64(%rsi)
	movq	%rsi, %r13
	addq	$80, %rsi
	movq	8(%rdx), %r10
	movq	32(%rbx), %r15
	movq	64(%rbx), %r14
	movq	(%rdx), %r12
	movq	%r15, %r9
	movq	%r13, %r8
	movq	%r14, %rax
	movq	%r12, %rdi
	jmp	*%r10
doGC3CE:
	movq	$36, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %rcx
	addq	$40, %rsi
	movq	%rcx, %rdi
	movabsq	$retGC3CC, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.3D0:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest3D2
	/* live= GP={%rcx %rdx} spilled=  */
retGC3D1:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest3D2:
	movq	%r11, %r12
	movq	448(%r12), %rbx
	subq	%rsi, %rbx
	jle	doGC3D3
check.3CF:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block check<E369> (ep<D607>,ite<D603>) */
	movq	$20, -8(%rsi)
	movq	(%rcx), %r12
	movq	%r12, (%rsi)
	movq	$1, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movq	64(%rdx), %r13
	movq	%r13, (%rsi)
	movq	%r10, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	$65299, -8(%rsi)
	movabsq	$letJoinK.3CB, %r15
	movq	%r15, (%rsi)
	movq	8(%rdx), %rcx
	movq	%rcx, 8(%rsi)
	movq	16(%rdx), %rbx
	movq	%rbx, 16(%rsi)
	movq	24(%rdx), %r10
	movq	%r10, 24(%rsi)
	movq	32(%rdx), %r12
	movq	%r12, 32(%rsi)
	movq	40(%rdx), %r13
	movq	%r13, 40(%rsi)
	movq	48(%rdx), %r15
	movq	%r15, 48(%rsi)
	movq	56(%rdx), %rcx
	movq	%rcx, 56(%rsi)
	movq	%r14, 64(%rsi)
	movq	%rsi, %r14
	addq	$80, %rsi
	movq	8(%rdx), %rbx
	movq	(%rbx), %rdi
	movq	$1, %r8
	movq	%r14, %r9
	movq	32(%rdx), %r10
	jmp	current_D_work_D_group.169
doGC3D3:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	%r10, %rdi
	movabsq	$retGC3D1, %r8
	jmp	_ASM_InvokeGC
	.text
pFib.3A4:
	movq	%r9, %rcx
	movq	%r8, %rbx
	movq	%rdi, %rdx
gcTest3DC:
	movq	%r11, %r14
	movq	448(%r14), %r15
	subq	%rsi, %r15
	jle	doGC3DD
check.3D4:
	/* block check<E36E> (ep<D59E>,n<D59F>,retK<D5A0>,_exh<D5A1>) */
	movl	(%rbx), %r12d
	cmpl	$0, %r12d
	je	L_true3D5
else.3D6:
	/* block else<D5B2> (ep<D5AD>,n<D5B1>,retK<D5B0>,_exh<D5AF>,_raw<D5AE>) */
	cmpl	$1, %r12d
	je	L_true3D8
	movq	%rbx, -56(%rbp)
else.3D9:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block else<D5BE> (ep<D5BA>,n<D5BD>,retK<D5BC>,_exh<D5BB>) */
	movq	$20, -8(%rsi)
	movq	$1, (%rsi)
	movq	$1, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	$10, -8(%rsi)
	movl	$1, (%rsi)
	movq	%rsi, %r12
	addq	$16, %rsi
	movq	$261, -8(%rsi)
	movabsq	$letJoinK.396, %r13
	movq	%r13, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	$133, -8(%rsi)
	movq	%rdx, (%rsi)
	movabsq	$pFib.3A4, %r15
	movq	%r15, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	$60, -8(%rsi)
	movabsq	$slowClone_1.3A5, %r13
	movq	%r13, (%rsi)
	movq	%r14, 8(%rsi)
	movq	-56(%rbp), %r13
	movq	%r13, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rbx, 32(%rsi)
	movq	%r12, 40(%rsi)
	movq	%rcx, 48(%rsi)
	movq	%rsi, %r14
	addq	$64, %rsi
	movq	$133, -8(%rsi)
	movq	%rdx, (%rsi)
	movabsq	$pFib.3A4, %r13
	movq	%r13, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	$65299, -8(%rsi)
	movabsq	$letJoinK.3D0, %r15
	movq	%r15, (%rsi)
	movq	8(%rdx), %r15
	movq	%r15, 8(%rsi)
	movq	%r13, 16(%rsi)
	movq	-56(%rbp), %r15
	movq	%r15, 24(%rsi)
	movq	%r10, 32(%rsi)
	movq	%rbx, 40(%rsi)
	movq	%r12, 48(%rsi)
	movq	%rcx, 56(%rsi)
	movq	%r14, 64(%rsi)
	movq	%rsi, %r14
	addq	$80, %rsi
	movq	(%rdx), %rcx
	movq	(%rcx), %rdi
	movq	%r14, %r8
	movq	%r10, %r9
	jmp	get_D_ite.EB
L_true3D8:
then.3DA:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block then<D5B6> (retK<D5B5>) */
	movq	$10, -8(%rsi)
	movl	$1, (%rsi)
	movq	%rsi, %rdx
	addq	$16, %rsi
	movq	(%rcx), %rbx
	movq	%rcx, %rdi
	movq	%rdx, %r8
	jmp	*%rbx
doGC3DD:
	movq	$36, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rbx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %r13
	addq	$40, %rsi
	movq	%r13, %rdi
	movabsq	$retGC3DB, %r8
	jmp	_ASM_InvokeGC
	/* live= GP={%r10 %rcx %rbx %rdx} spilled=  */
retGC3DB:
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rbx
	movq	(%rdi), %rdx
	jmp	gcTest3DC
L_true3D5:
then.3D7:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block then<D5A9> (retK<D5A8>) */
	movq	$10, -8(%rsi)
	movl	$0, (%rsi)
	movq	%rsi, %r10
	addq	$16, %rsi
	movq	(%rcx), %r12
	movq	%rcx, %rdi
	movq	%r10, %r8
	jmp	*%r12
	.text
pickVictim.3ED:
	movq	%r9, %rdx
	movq	%r8, %rcx
	movq	%rdi, %r15
	jmp	gcTest3EF
	/* live= spilled= GP={%r~1 %r~1 %r~1}  */
retGC3EE:
	movq	16(%rdi), %rdx
	movq	8(%rdi), %rcx
	movq	(%rdi), %r15
gcTest3EF:
	movq	%r11, %r10
	movq	448(%r10), %r12
	subq	%rsi, %r12
	jg	L3F1
doGC3F0:
	movq	$519, -8(%rsi)
	movq	%r15, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rdx, 16(%rsi)
	movq	%rsi, %rbx
	addq	$32, %rsi
	movq	%rbx, %rdi
	movabsq	$retGC3EE, %r8
	jmp	_ASM_InvokeGC
L3F1:
	movq	%rdx, -88(%rbp)
	movq	%rcx, -80(%rbp)
	movq	%r15, -72(%rbp)
check.3DE:
	/* block check<E376> (ep<D6C0>,self<D6C1>,retK<D6C2>) */
	movq	%rax, %r12
	movq	%rdi, %r13
	movq	%r8, %r14
	movq	%r9, %r15
	movq	%r15, -56(%rbp)
	movq	%rsi, %r15
	movq	%r11, %rbx
	call	_GetNumVProcs
	movq	%rax, %r10
	movq	%r12, %rax
	movq	%r13, %rdi
	movq	%r14, %r8
	movq	-56(%rbp), %r9
	movq	%r15, %rsi
	movq	%rbx, %r11
	cmpl	$1, %r10d
	jle	L_true3E0
	movq	%r10, %rcx
else.3E1:
	/* block else<D6D2> (ep<D6CE>,self<D6D1>,retK<D6D0>,n<D6CF>) */
	movq	%rax, %rdx
	movq	%rdx, -56(%rbp)
	movq	%rdi, %rbx
	movq	%rbx, -64(%rbp)
	movq	%r8, %r15
	movq	%r9, %r14
	movq	%rsi, %r13
	movq	%r11, %r12
	xorl	%ebx, %ebx
	movslq	%ebx, %r10
	movq	%r10, %rdi
	movslq	%ecx, %rcx
	movq	%rcx, %rsi
	call	_M_RandomInt
	movq	%rax, %r10
	movq	-56(%rbp), %rax
	movq	-64(%rbp), %rdi
	movq	%r15, %r8
	movq	%r14, %r9
	movq	%r13, %rsi
	movq	%r12, %r11
	movq	%rax, %rbx
	movq	%rdi, %r15
	movq	%r8, %r14
	movq	%r9, %r13
	movq	%rsi, %r12
	movq	%r12, -56(%rbp)
	movq	%r11, %r12
	movslq	%r10d, %r10
	movq	%r10, %rdi
	call	_GetNthVProc
	movq	%rax, %rdx
	movq	%rbx, %rax
	movq	%r15, %rdi
	movq	%r14, %r8
	movq	%r13, %r9
	movq	-56(%rbp), %rsi
	movq	%r12, %r11
	cmpq	-80(%rbp), %rdx
	je	L_true3E5
	movq	-88(%rbp), %rbx
else.3E6:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block else<D6E0> (retK<D6DF>,vp<D6DE>) */
	movq	$10, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rsi, %r13
	addq	$16, %rsi
	movq	$12, -8(%rsi)
	movq	%r13, (%rsi)
	movq	%rsi, %r14
	addq	$16, %rsi
	movq	%rbx, %rdi
	movq	%r14, %r8
	jmp	letJoinK.3E8
L_true3E5:
	movq	-88(%rbp), %r13
	movq	-80(%rbp), %r12
	movq	-72(%rbp), %r10
then.3E7:
	/* block then<D6DB> (ep<E36F>,self<E370>,retK<E371>) */
gcTest3EB:
	movq	%r11, %rcx
	movq	448(%rcx), %rdx
	subq	%rsi, %rdx
	jle	doGC3EC
check.3E9:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block check<E372> (ep<D6D8>,self<D6DA>,retK<D6D9>) */
	movq	%r10, %rdi
	movq	%r12, %r8
	movq	%r13, %r9
	jmp	pickVictim.3ED
doGC3EC:
	movq	$519, -8(%rsi)
	movq	%r10, (%rsi)
	movq	%r12, 8(%rsi)
	movq	%r13, 16(%rsi)
	movq	%rsi, %r15
	addq	$32, %rsi
	movq	%r15, %rdi
	movabsq	$retGC3EA, %r8
	jmp	_ASM_InvokeGC
L_true3E0:
	movq	-88(%rbp), %r14
then.3E2:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block then<D6CB> (retK<D6CA>) */
	movq	%r14, %rdi
	movq	$1, %r8
	jmp	letJoinK.3E8
	/* live= GP={%r13 %r12 %r10} spilled=  */
retGC3EA:
	movq	16(%rdi), %r13
	movq	8(%rdi), %r12
	movq	(%rdi), %r10
	jmp	gcTest3EB
	.text
k.3F4:
	movq	%rax, %rcx
	movq	%rdi, %rdx
	jmp	gcTest3F6
	/* live= GP={%rcx %rdx} spilled=  */
retGC3F5:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest3F6:
	movq	%r11, %r13
	movq	448(%r13), %rbx
	subq	%rsi, %rbx
	jle	doGC3F7
check.3F2:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block check<E379> (ep<D769>,x<D766>) */
	movq	$3, (%r11)
	movq	8(%rdx), %r10
	movq	(%r10), %rdi
	movq	%r11, %r8
	movq	16(%rdx), %r9
	jmp	lp.3F3
doGC3F7:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	%r12, %rdi
	movabsq	$retGC3F5, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.3FA:
	movq	%rdi, %rcx
	jmp	gcTest3FC
	/* live= GP={%rcx} spilled=  */
retGC3FB:
	movq	(%rdi), %rcx
gcTest3FC:
	movq	%r11, %r10
	movq	448(%r10), %rdx
	subq	%rsi, %rdx
	jle	doGC3FD
check.3F8:
	/* Liveout:  GP={%rdi}  */
	/* block check<E37B> (ep<D7A2>) */
	movq	8(%rcx), %rdi
	jmp	letJoinK.3F9
doGC3FD:
	movq	$12, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%rsi, %rbx
	addq	$16, %rsi
	movq	%rbx, %rdi
	movabsq	$retGC3FB, %r8
	jmp	_ASM_InvokeGC
	.text
k.3FF:
	movq	%rax, %rcx
	movq	%rdi, %rdx
	jmp	gcTest401
	/* live= GP={%rcx %rdx} spilled=  */
retGC400:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest401:
	movq	%r11, %r12
	movq	448(%r12), %rbx
	subq	%rsi, %rbx
	jle	doGC402
check.3FE:
	/* Liveout:  GP={%rdi}  */
	/* block check<E37E> (ep<D7B3>,x<D7B2>) */
	movq	8(%rdx), %rdi
	jmp	letJoinK.3FA
doGC402:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	%r10, %rdi
	movabsq	$retGC400, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.413:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
gcTest415:
	movq	%r11, %r13
	movq	448(%r13), %r12
	subq	%rsi, %r12
	jle	doGC416
check.403:
	/* block check<E383> (ep<D748>,unused<D742>,unused<D743>,_t<D744>) */
	movq	(%r10), %r12
	movq	24(%rbx), %r13
	movl	224(%r13), %r10d
	shlq	$3, %r10
	movq	(%r12,%r10,1), %r10
	movl	(%r10), %r14d
	cmpl	4(%r10), %r14d
	jle	L_true404
else.405:
	/* block else<D7DA> (ep<D7D8>,_cast_x<D7D9>) */
	movl	8(%r10), %edx
	subl	(%r10), %edx
	movl	4(%r10), %r15d
	leal	(%rdx,%r15,1), %r15d
letJoinK.407:
	/* block letJoinK<D75C> (ep<D759>,_cast_x<D75B>,size<D75A>) */
	movl	8(%r10), %ecx
	decl	%ecx
	cmpl	%ecx, %r15d
	jl	L417
L_true408:
then.40A:
	/* Liveout:  GP={%rax %rdi}  */
	/* block then<D765> (ep<D764>) */
	movq	$28, -8(%rsi)
	movabsq	$k.3F4, %r12
	movq	%r12, (%rsi)
	movq	16(%rbx), %r13
	movq	%r13, 8(%rsi)
	movq	32(%rbx), %r14
	movq	%r14, 16(%rsi)
	movq	%rsi, %r10
	addq	$32, %rsi
	movq	$20, -8(%rsi)
	movq	$1, (%rsi)
	movq	%r10, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	24(%rbx), %rcx
	movq	32(%rcx), %r15
	movq	8(%r15), %rdx
	movq	24(%rbx), %rbx
	movq	%rdx, 32(%rbx)
	movq	(%r15), %r13
	movq	(%r13), %r10
	movq	%r14, %rax
	movq	%r13, %rdi
	jmp	*%r10
doGC416:
	movq	$36, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %r12
	addq	$40, %rsi
	movq	%r12, %rdi
	movabsq	$retGC414, %r8
	jmp	_ASM_InvokeGC
L417:
else.409:
	/* block else<D77C> (ep<D77A>,_cast_x<D77B>) */
	movl	4(%r10), %edx
	movl	4(%r10), %r13d
	movl	8(%r10), %r14d
	decl	%r14d
	cmpl	%r14d, %r13d
	jge	L_true40B
else.40C:
	/* block else<D7C9> (ep<D7C5>,_cast_x<D7C8>,new<D7C7>,_t<D7C6>) */
	incl	%r13d
	jmp	letJoinK.40E
L_true40B:
then.40D:
	/* block then<D7C3> (ep<D7C0>,_cast_x<D7C2>,new<D7C1>) */
	xorl	%r13d, %r13d
letJoinK.40E:
	/* block letJoinK<D792> (ep<D78E>,_cast_x<D791>,new<D790>,newR<D78F>) */
	movl	%r13d, 4(%r10)
	movq	8(%rbx), %rcx
	shll	$3, %edx
	movslq	%edx, %rdx
	movq	%rcx, 16(%r10,%rdx,1)
	movq	$261, -8(%rsi)
	movabsq	$letJoinK.3FA, %r10
	movq	%r10, (%rsi)
	movq	32(%rbx), %r12
	movq	%r12, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	24(%rbx), %r14
	movq	8(%r14), %r13
	cmpq	$1, %r13
	je	S_case40F
	cmpq	$3, %r13
	jne	S_case40F
S_case411:
case.412:
	/* Liveout:  GP={%rax %rdi}  */
	/* block case<D7AF> (ep<D7AD>,letJoinK<D7AE>) */
	movq	24(%rbx), %r15
	movq	$1, 8(%r15)
	movq	$20, -8(%rsi)
	movabsq	$k.3FF, %rdx
	movq	%rdx, (%rsi)
	movq	%r12, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movq	$1, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	24(%rbx), %r12
	movq	32(%r12), %r10
	movq	8(%r10), %r13
	movq	24(%rbx), %r14
	movq	%r13, 32(%r14)
	movq	(%r10), %r15
	movq	(%r15), %rdx
	movq	%rcx, %rax
	movq	%r15, %rdi
	jmp	*%rdx
S_case40F:
case.410:
	/* Liveout:  GP={%rdi}  */
	/* block case<D7A9> (ep<D7A7>,letJoinK<D7A8>) */
	movq	24(%rbx), %rbx
	movq	$1, (%rbx)
	movq	%r12, %rdi
	jmp	letJoinK.3FA
L_true404:
then.406:
	/* block then<D7CE> (ep<D7CC>,_cast_x<D7CD>) */
	movl	4(%r10), %r15d
	subl	(%r10), %r15d
	jmp	letJoinK.407
	/* live= GP={%r10 %rcx %rdx %rbx} spilled=  */
retGC414:
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
	jmp	gcTest415
	.text
lp.3F3:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest41A
	/* live= GP={%rcx %rdx %rbx} spilled=  */
retGC419:
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest41A:
	movq	%r11, %r14
	movq	448(%r14), %r10
	subq	%rsi, %r10
	jle	doGC41B
check.418:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block check<E387> (ep<D73C>,self<D73D>,retK<D73E>) */
	movq	$133, -8(%rsi)
	movq	%rbx, (%rsi)
	movabsq	$lp.3F3, %r13
	movq	%r13, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	$2827, -8(%rsi)
	movabsq	$letJoinK.413, %r15
	movq	%r15, (%rsi)
	movq	8(%rbx), %r10
	movq	%r10, 8(%rsi)
	movq	%r12, 16(%rsi)
	movq	%rdx, 24(%rsi)
	movq	%rcx, 32(%rsi)
	movq	%rsi, %r14
	addq	$48, %rsi
	movq	(%rbx), %r12
	movq	(%r12), %rdi
	movq	$1, %r8
	movq	%r14, %r9
	movq	16(%rbx), %r10
	jmp	current_D_work_D_group.169
doGC41B:
	movq	$647, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%rsi, %r13
	addq	$32, %rsi
	movq	%r13, %rdi
	movabsq	$retGC419, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.3F9:
	movq	%rdi, %rcx
	jmp	gcTest41E
	/* live= GP={%rcx} spilled=  */
retGC41D:
	movq	(%rdi), %rcx
gcTest41E:
	movq	%r11, %r14
	movq	448(%r14), %rdx
	subq	%rsi, %rdx
	jle	doGC41F
check.41C:
	/* Liveout:  GP={%rax %rdi}  */
	/* block check<E389> (ep<D7F1>) */
	movq	8(%rcx), %rbx
	movq	(%rbx), %r10
	movq	$1, %r12
	movq	%r12, %rax
	movq	%rbx, %rdi
	jmp	*%r10
doGC41F:
	movq	$12, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%rsi, %r13
	addq	$16, %rsi
	movq	%r13, %rdi
	movabsq	$retGC41D, %r8
	jmp	_ASM_InvokeGC
	.text
spawnFn.421:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rax, %rbx
	movq	%rdi, %r10
	jmp	gcTest423
	/* live= GP={%rcx %rdx %rbx %r10} spilled=  */
retGC422:
	movq	24(%rdi), %rcx
	movq	16(%rdi), %rdx
	movq	8(%rdi), %rbx
	movq	(%rdi), %r10
gcTest423:
	movq	%r11, %r12
	movq	448(%r12), %r12
	subq	%rsi, %r12
	jle	doGC424
check.420:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block check<E38E> (ep<D732>,thd<D733>,retK<D734>,exh<D735>) */
	movq	$28, -8(%rsi)
	movq	(%r10), %r14
	movq	%r14, (%rsi)
	movq	%rbx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%rsi, %r13
	addq	$32, %rsi
	movq	$133, -8(%rsi)
	movq	%r13, (%rsi)
	movabsq	$lp.3F3, %rcx
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	$3, (%r11)
	movq	$261, -8(%rsi)
	movabsq	$letJoinK.3F9, %rbx
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	movq	(%r15), %rdi
	movq	%r11, %r8
	movq	%rdx, %r9
	jmp	lp.3F3
doGC424:
	movq	$36, -8(%rsi)
	movq	%r10, (%rsi)
	movq	%rbx, 8(%rsi)
	movq	%rdx, 16(%rsi)
	movq	%rcx, 24(%rsi)
	movq	%rsi, %r10
	addq	$40, %rsi
	movq	%r10, %rdi
	movabsq	$retGC422, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.426:
	movq	%rdi, %rcx
	jmp	gcTest428
	/* live= GP={%rcx} spilled=  */
retGC427:
	movq	(%rdi), %rcx
gcTest428:
	movq	%r11, %r14
	movq	448(%r14), %rdx
	subq	%rsi, %rdx
	jle	doGC429
check.425:
	/* Liveout:  GP={%rax %rdi}  */
	/* block check<E390> (ep<D845>) */
	movq	8(%rcx), %rbx
	movq	(%rbx), %r10
	movq	$1, %r12
	movq	%r12, %rax
	movq	%rbx, %rdi
	jmp	*%r10
doGC429:
	movq	$12, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%rsi, %r13
	addq	$16, %rsi
	movq	%r13, %rdi
	movabsq	$retGC427, %r8
	jmp	_ASM_InvokeGC
	.text
k.42B:
	movq	%rax, %rcx
	movq	%rdi, %rdx
	jmp	gcTest42D
	/* live= GP={%rcx %rdx} spilled=  */
retGC42C:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest42D:
	movq	%r11, %r12
	movq	448(%r12), %rbx
	subq	%rsi, %rbx
	jle	doGC42E
check.42A:
	/* Liveout:  GP={%rdi}  */
	/* block check<E393> (ep<D857>,x<D856>) */
	movq	8(%rdx), %rdi
	jmp	letJoinK.426
doGC42E:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	%r10, %rdi
	movabsq	$retGC42C, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.43C:
	movq	%r8, %rdx
	movq	%rdi, %r14
	jmp	gcTest43E
	/* live= GP={%rdx %r14} spilled=  */
retGC43D:
	movq	8(%rdi), %rdx
	movq	(%rdi), %r14
gcTest43E:
	movq	%r11, %r15
	movq	448(%r15), %rcx
	subq	%rsi, %rcx
	jle	doGC43F
check.42F:
	/* block check<E396> (ep<D806>,ite<D805>) */
	movq	(%rdx), %rcx
	cmpq	$1, %rcx
	jne	L_true430
else.431:
	/* Liveout:  GP={%rax %rdi}  */
	/* block else<D871> (ep<D870>) */
	movq	$133, -8(%rsi)
	movabsq	$str168, %r10
	movq	%r10, (%rsi)
	movl	$58, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movabsq	$tagEA, %r13
	movq	%r13, (%rsi)
	movq	%rbx, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	24(%r14), %r14
	movq	(%r14), %r15
	movq	%r12, %rax
	movq	%r14, %rdi
	jmp	*%r15
doGC43F:
	movq	$20, -8(%rsi)
	movq	%r14, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	%rcx, %rdi
	movabsq	$retGC43D, %r8
	jmp	_ASM_InvokeGC
L_true430:
	movq	%r14, -56(%rbp)
then.432:
	/* block then<D80D> (ep<D80B>,stk<D80C>) */
	movq	%rax, %r15
	movq	%rdi, %rdx
	movq	%rdx, -64(%rbp)
	movq	%r8, %rbx
	movq	%r9, %r12
	movq	%rsi, %r13
	movq	%r11, %r14
	movq	-56(%rbp), %r10
	movq	32(%r10), %rdx
	movq	%rdx, %rdi
	movq	(%rcx), %rcx
	movq	(%rcx), %r10
	movq	%r10, %rsi
	movl	$1, %edx
	movslq	%edx, %rdx
	call	_M_DequeAlloc
	movq	%rax, %rcx
	movq	%r15, %rax
	movq	-64(%rbp), %rdi
	movq	%rbx, %r8
	movq	%r12, %r9
	movq	%r13, %rsi
	movq	%r14, %r11
	movl	4(%rcx), %ebx
	movl	4(%rcx), %r10d
	movl	8(%rcx), %edx
	decl	%edx
	cmpl	%edx, %r10d
	jl	L440
L_true434:
	movq	-56(%rbp), %r15
then.436:
	/* block then<D867> (ep<D864>,deque<D866>,new<D865>) */
	xorl	%r10d, %r10d
	jmp	letJoinK.437
L440:
	movq	-56(%rbp), %r15
else.435:
	/* block else<D86D> (ep<D869>,deque<D86C>,new<D86B>,_t<D86A>) */
	incl	%r10d
letJoinK.437:
	/* block letJoinK<D82A> (ep<D826>,deque<D829>,new<D828>,newR<D827>) */
	movl	%r10d, 4(%rcx)
	movq	8(%r15), %r10
	shll	$3, %ebx
	movslq	%ebx, %r12
	movq	%r10, 16(%rcx,%r12,1)
	decl	12(%rcx)
	movq	$261, -8(%rsi)
	movabsq	$letJoinK.426, %r14
	movq	%r14, (%rsi)
	movq	16(%r15), %rcx
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	32(%r15), %rbx
	movq	8(%rbx), %rdx
	cmpq	$1, %rdx
	je	S_case438
	cmpq	$3, %rdx
	je	S_case43A
S_case438:
case.439:
	/* Liveout:  GP={%rdi}  */
	/* block case<D84D> (ep<D84B>,letJoinK<D84C>) */
	movq	32(%r15), %r15
	movq	$1, (%r15)
	movq	%r13, %rdi
	jmp	letJoinK.426
S_case43A:
case.43B:
	/* Liveout:  GP={%rax %rdi}  */
	/* block case<D853> (ep<D851>,letJoinK<D852>) */
	movq	32(%r15), %r10
	movq	$1, 8(%r10)
	movq	$20, -8(%rsi)
	movabsq	$k.42B, %r14
	movq	%r14, (%rsi)
	movq	%r13, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movq	$1, (%rsi)
	movq	%r12, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	32(%r15), %rbx
	movq	32(%rbx), %rdx
	movq	8(%rdx), %r10
	movq	32(%r15), %r12
	movq	%r10, 32(%r12)
	movq	(%rdx), %r13
	movq	(%r13), %r14
	movq	%rcx, %rax
	movq	%r13, %rdi
	jmp	*%r14
	.text
resumeFn.442:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rax, %rbx
	movq	%rdi, %r10
	jmp	gcTest444
	/* live= GP={%rcx %rdx %rbx %r10} spilled=  */
retGC443:
	movq	24(%rdi), %rcx
	movq	16(%rdi), %rdx
	movq	8(%rdi), %rbx
	movq	(%rdi), %r10
gcTest444:
	movq	%r11, %r12
	movq	448(%r12), %r12
	subq	%rsi, %r12
	jle	doGC445
check.441:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block check<E39B> (ep<D7FC>,thd<D7FD>,retK<D7FE>,exh<D7FF>) */
	movq	$3, (%r11)
	movq	$1803, -8(%rsi)
	movabsq	$letJoinK.43C, %r14
	movq	%r14, (%rsi)
	movq	%rbx, 8(%rsi)
	movq	%rdx, 16(%rsi)
	movq	%rcx, 24(%rsi)
	movq	%r11, 32(%rsi)
	movq	%rsi, %r13
	addq	$48, %rsi
	movq	(%r10), %r15
	movq	(%r15), %rdi
	movq	%r13, %r8
	movq	%rcx, %r9
	jmp	get_D_ite.EB
doGC445:
	movq	$36, -8(%rsi)
	movq	%r10, (%rsi)
	movq	%rbx, 8(%rsi)
	movq	%rdx, 16(%rsi)
	movq	%rcx, 24(%rsi)
	movq	%rsi, %rcx
	addq	$40, %rsi
	movq	%rcx, %rdi
	movabsq	$retGC443, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.44A:
	movq	%rdi, %rcx
	jmp	gcTest44C
	/* live= GP={%rcx} spilled=  */
retGC44B:
	movq	(%rdi), %rcx
gcTest44C:
	movq	%r11, %rdx
	movq	448(%rdx), %rdx
	subq	%rsi, %rdx
	jle	doGC44D
check.446:
	/* block check<E39D> (ep<D8A2>) */
	cmpq	$1, 16(%rcx)
	je	L44E
L_true447:
then.449:
	/* Liveout:  GP={%rax %rdi}  */
	/* block then<D8A8> (ep<D8A7>) */
	movq	8(%rcx), %rbx
	movq	(%rbx), %r14
	movq	$3, %r15
	movq	%r15, %rax
	movq	%rbx, %rdi
	jmp	*%r14
doGC44D:
	movq	$12, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%rsi, %rcx
	addq	$16, %rsi
	movq	%rcx, %rdi
	movabsq	$retGC44B, %r8
	jmp	_ASM_InvokeGC
L44E:
else.448:
	/* Liveout:  GP={%rax %rdi}  */
	/* block else<D8AD> (ep<D8AC>) */
	movq	8(%rcx), %r10
	movq	(%r10), %r12
	movq	$1, %r13
	movq	%r13, %rax
	movq	%r10, %rdi
	jmp	*%r12
	.text
k.450:
	movq	%rax, %rcx
	movq	%rdi, %rdx
	jmp	gcTest452
	/* live= GP={%rcx %rdx} spilled=  */
retGC451:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest452:
	movq	%r11, %r12
	movq	448(%r12), %rbx
	subq	%rsi, %rbx
	jle	doGC453
check.44F:
	/* Liveout:  GP={%rdi}  */
	/* block check<E3A0> (ep<D8BD>,x<D8BC>) */
	movq	8(%rdx), %rdi
	jmp	letJoinK.44A
doGC453:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	%r10, %rdi
	movabsq	$retGC451, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.461:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest463
	/* live= GP={%r10 %rcx %rdx %rbx} spilled=  */
retGC462:
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest463:
	movq	%r11, %r12
	movq	448(%r12), %r12
	subq	%rsi, %r12
	jle	doGC464
check.454:
	/* block check<E3A5> (ep<D88B>,unused<D888>,unused<D889>,_t<D88A>) */
	movq	(%r10), %r13
	movq	16(%rbx), %r14
	movl	224(%r14), %r12d
	shlq	$3, %r12
	movq	(%r13,%r12,1), %rcx
	movl	4(%rcx), %r15d
	cmpl	(%rcx), %r15d
	jne	L465
L_true455:
then.457:
	/* block then<D8CB> (ep<D8CA>) */
	movq	$1, %r10
	jmp	letJoinK.45C
L465:
else.456:
	/* block else<D8CF> (ep<D8CD>,_cast_x<D8CE>) */
	movl	4(%rcx), %edx
	cmpl	$0, %edx
	jg	L466
L_true458:
	movl	8(%rcx), %edx
then.45A:
	/* block then<D8F7> (ep<D8F4>,_cast_x<D8F6>,_t<D8F5>) */
	decl	%edx
	jmp	letJoinK.45B
L466:
else.459:
	/* block else<D8FD> (ep<D8FA>,_cast_x<D8FC>,_t<D8FB>) */
	decl	%edx
letJoinK.45B:
	/* block letJoinK<D8DE> (ep<D8DB>,_cast_x<D8DD>,newL<D8DC>) */
	movq	%rdx, %r13
	shll	$3, %r13d
	movslq	%r13d, %r12
	movq	16(%rcx,%r12,1), %r10
	movq	%rdx, %r15
	shll	$3, %r15d
	movslq	%r15d, %r14
	movq	$1, 16(%rcx,%r14,1)
	movl	%edx, 4(%rcx)
	movq	$12, -8(%rsi)
	movq	%r10, (%rsi)
	movq	%rsi, %r10
	addq	$16, %rsi
letJoinK.45C:
	/* block letJoinK<D89E> (ep<D89C>,k<D89D>) */
	movq	$775, -8(%rsi)
	movabsq	$letJoinK.44A, %rcx
	movq	%rcx, (%rsi)
	movq	8(%rbx), %rdx
	movq	%rdx, 8(%rsi)
	movq	%r10, 16(%rsi)
	movq	%rsi, %r15
	addq	$32, %rsi
	movq	16(%rbx), %r12
	movq	8(%r12), %r10
	cmpq	$1, %r10
	je	S_case45D
	cmpq	$3, %r10
	je	S_case45F
S_case45D:
case.45E:
	/* Liveout:  GP={%rdi}  */
	/* block case<D8B3> (ep<D8B1>,letJoinK<D8B2>) */
	movq	16(%rbx), %rcx
	movq	$1, (%rcx)
	movq	%r15, %rdi
	jmp	letJoinK.44A
S_case45F:
case.460:
	/* Liveout:  GP={%rax %rdi}  */
	/* block case<D8B9> (ep<D8B7>,letJoinK<D8B8>) */
	movq	16(%rbx), %r13
	movq	$1, 8(%r13)
	movq	$20, -8(%rsi)
	movabsq	$k.450, %rcx
	movq	%rcx, (%rsi)
	movq	%r15, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movq	$1, (%rsi)
	movq	%r14, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	16(%rbx), %r10
	movq	32(%r10), %rdx
	movq	8(%rdx), %r12
	movq	16(%rbx), %r13
	movq	%r12, 32(%r13)
	movq	(%rdx), %r13
	movq	(%r13), %r15
	movq	%r14, %rax
	movq	%r13, %rdi
	jmp	*%r15
doGC464:
	movq	$36, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %rdx
	addq	$40, %rsi
	movq	%rdx, %rdi
	movabsq	$retGC462, %r8
	jmp	_ASM_InvokeGC
	.text
removeFn.468:
	movq	%r9, %r10
	movq	%r8, %rcx
	movq	%rax, %rdx
	movq	%rdi, %rbx
	jmp	gcTest46A
	/* live= GP={%r10 %rcx %rdx %rbx} spilled=  */
retGC469:
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest46A:
	movq	%r11, %r12
	movq	448(%r12), %r12
	subq	%rsi, %r12
	jle	doGC46B
check.467:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block check<E3AA> (ep<D87F>,_wild<D880>,retK<D881>,exh<D882>) */
	movq	$3, (%r11)
	movq	$263, -8(%rsi)
	movabsq	$letJoinK.461, %r14
	movq	%r14, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%r11, 16(%rsi)
	movq	%rsi, %r13
	addq	$32, %rsi
	movq	(%rbx), %r15
	movq	(%r15), %rdi
	movq	$1, %r8
	movq	%r13, %r9
	jmp	current_D_work_D_group.169
doGC46B:
	movq	$36, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %rcx
	addq	$40, %rsi
	movq	%rcx, %rdi
	movabsq	$retGC469, %r8
	jmp	_ASM_InvokeGC
	.text
retGC476:
	movq	16(%rdi), %r14
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
	jmp	gcTest477
L_true47C:
then.47D:
	/* block then<DA63> (ep<DA60>,retK<DA62>,elt<DA61>) */
	xorl	%r10d, %r10d
letJoinK.479:
	/* block letJoinK<DA5B> (ep<DA57>,retK<DA5A>,elt<DA59>,oldR<DA58>) */
	movq	(%rdx), %r13
	leaq	(%r13), %r12
	movq	(%rdx), %r14
	movl	%r10d, (%r14)
	movq	$12, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rsi, %r14
	addq	$16, %rsi
letJoinK.475:
	/* block letJoinK<D9EC> (ep<E3AB>,retK<E3AC>,elt<E3AD>) */
gcTest477:
	movq	%r11, %rbx
	movq	448(%rbx), %r15
	subq	%rsi, %r15
	jle	doGC478
check.46C:
	/* block check<E3AE> (ep<D9E9>,retK<D9EB>,elt<D9EA>) */
	cmpq	$1, %r14
	je	L480
L_true46D:
then.46F:
	/* block then<D9F1> (ep<D9EE>,retK<D9F0>,elt<D9EF>) */
	movq	(%r14), %r10
	movq	8(%rdx), %r14
	movl	4(%r14), %ebx
	movq	8(%rdx), %r15
	movl	4(%r15), %r12d
	movq	8(%rdx), %r13
	movl	8(%r13), %r13d
	decl	%r13d
	cmpl	%r13d, %r12d
	jge	L_true470
else.471:
	/* block else<DA27> (ep<DA22>,retK<DA26>,elt<DA25>,new<DA24>,_t<DA23>) */
	incl	%r12d
letJoinK.473:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block letJoinK<DA0D> (ep<DA08>,retK<DA0C>,elt<DA0B>,new<DA0A>,newR<DA09>) */
	movq	8(%rdx), %r15
	leaq	4(%r15), %r14
	movq	8(%rdx), %r13
	movl	%r12d, 4(%r13)
	movq	8(%rdx), %r14
	shll	$3, %ebx
	movslq	%ebx, %r15
	movq	%r10, 16(%r14,%r15,1)
	movq	%rdx, %rdi
	movq	%rcx, %r8
copy.474:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	movq	(%rdx), %r15
	movq	(%rdx), %rbx
	movl	4(%r15), %r10d
	cmpl	(%rbx), %r10d
	je	L_true47E
else.47B:
	/* block else<DA34> (ep<DA32>,retK<DA33>) */
	movq	(%rdx), %r15
	movl	(%r15), %r12d
	movq	(%rdx), %rbx
	movq	%r12, %r13
	shll	$3, %r13d
	movslq	%r13d, %r10
	movq	16(%rbx,%r10,1), %rbx
	movq	(%rdx), %r15
	leaq	(%r15), %r14
	movq	(%rdx), %r10
	shll	$3, %r12d
	movslq	%r12d, %r12
	movq	$1, 16(%r10,%r12,1)
	movq	(%rdx), %r13
	movl	(%r13), %r10d
	movq	(%rdx), %r14
	movl	8(%r14), %r13d
	decl	%r13d
	cmpl	%r13d, %r10d
	jge	L_true47C
else.47A:
	/* block else<DA69> (ep<DA65>,retK<DA68>,elt<DA67>,_t<DA66>) */
	incl	%r10d
	jmp	letJoinK.479
L_true470:
then.472:
	/* block then<DA20> (ep<DA1C>,retK<DA1F>,elt<DA1E>,new<DA1D>) */
	xorl	%r12d, %r12d
	jmp	letJoinK.473
L480:
else.46E:
	/* Liveout:  GP={%rdi}  */
	/* block else<DA2C> (retK<DA2B>) */
	movq	%rcx, %rdi
	jmp	letJoinK.13
L_true47E:
then.47F:
	/* block then<DA30> (ep<DA2E>,retK<DA2F>) */
	movq	$1, %r14
	jmp	letJoinK.475
doGC478:
	movq	$28, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%r14, 16(%rsi)
	movq	%rsi, %rcx
	addq	$32, %rsi
	movq	%rcx, %rdi
	movabsq	$retGC476, %r8
	jmp	_ASM_InvokeGC
	/* live= GP={%r14 %rcx %rdx} spilled=  */
	jmp	retGC476
	.text
k.482:
	movq	%rax, %rcx
	movq	%rdi, %rdx
	jmp	gcTest484
	/* live= GP={%rcx %rdx} spilled=  */
retGC483:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest484:
	movq	%r11, %r12
	movq	448(%r12), %rbx
	subq	%rsi, %rbx
	jle	doGC485
check.481:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block check<E3B1> (ep<DAC3>,x<DABF>) */
	movq	$3, (%r11)
	movq	8(%rdx), %rdi
	movq	16(%rdx), %r8
	movq	24(%rdx), %r9
	movq	$1, %r10
	jmp	schedulerLoop.14
doGC485:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	%r10, %rdi
	movabsq	$retGC483, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.48B:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest48D
	/* live= GP={%rcx %rdx} spilled=  */
retGC48C:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest48D:
	movq	%r11, %r14
	movq	448(%r14), %rbx
	subq	%rsi, %rbx
	jle	doGC48E
check.486:
	/* block check<E3B4> (ep<DA94>,k<DA90>) */
	movq	$20, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	32(%rdx), %rbx
	movq	%rbx, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	24(%rdx), %r10
	movl	4(%r10), %r10d
	movq	24(%rdx), %r13
	movl	4(%r13), %r15d
	movq	24(%rdx), %r14
	movl	8(%r14), %ecx
	decl	%ecx
	cmpl	%ecx, %r15d
	jge	L_true487
else.488:
	/* block else<DADE> (ep<DADA>,_t<DADD>,new<DADC>,_t<DADB>) */
	incl	%r15d
	jmp	letJoinK.48A
doGC48E:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	%r13, %rdi
	movabsq	$retGC48C, %r8
	jmp	_ASM_InvokeGC
L_true487:
then.489:
	/* block then<DAD8> (ep<DAD5>,_t<DAD7>,new<DAD6>) */
	xorl	%r15d, %r15d
letJoinK.48A:
	/* Liveout:  GP={%rax %rdi}  */
	/* block letJoinK<DAB1> (ep<DAAD>,_t<DAB0>,new<DAAF>,newR<DAAE>) */
	movq	24(%rdx), %rbx
	leaq	4(%rbx), %rcx
	movq	24(%rdx), %r13
	movl	%r15d, 4(%r13)
	movq	24(%rdx), %r14
	shll	$3, %r10d
	movslq	%r10d, %r15
	movq	%r12, 16(%r14,%r15,1)
	movq	$1289, -8(%rsi)
	movabsq	$k.482, %rbx
	movq	%rbx, (%rsi)
	movq	8(%rdx), %r10
	movq	%r10, 8(%rsi)
	movq	16(%rdx), %r12
	movq	%r12, 16(%rsi)
	movq	24(%rdx), %r13
	movq	%r13, 24(%rsi)
	movq	%rsi, %rcx
	addq	$40, %rsi
	movq	$20, -8(%rsi)
	movq	$1, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	16(%rdx), %rcx
	movq	32(%rcx), %r15
	movq	8(%r15), %rbx
	movq	16(%rdx), %r10
	movq	%rbx, 32(%r10)
	movq	(%r15), %r13
	movq	(%r13), %r12
	movq	%r14, %rax
	movq	%r13, %rdi
	jmp	*%r12
	.text
letJoinK.490:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest492
	/* live= GP={%rcx %rdx} spilled=  */
retGC491:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest492:
	movq	%r11, %r12
	movq	448(%r12), %rbx
	subq	%rsi, %rbx
	jle	doGC493
check.48F:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<E3B7> (ep<DAE8>,k<DAE7>) */
	movq	8(%rdx), %rdi
	movq	%rcx, %r8
	jmp	letJoinK.48B
doGC493:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	%r10, %rdi
	movabsq	$retGC491, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.498:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest49A
	/* live= GP={%rcx %rdx} spilled=  */
retGC499:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest49A:
	movq	%r11, %rbx
	movq	448(%rbx), %rbx
	subq	%rsi, %rbx
	jle	doGC49B
check.494:
	/* block check<E3BA> (ep<DA8B>,ite<DA88>) */
	movq	8(%rcx), %r12
	movq	$3339, -8(%rsi)
	movabsq	$letJoinK.48B, %r13
	movq	%r13, (%rsi)
	movq	24(%rdx), %r14
	movq	%r14, 8(%rsi)
	movq	32(%rdx), %r15
	movq	%r15, 16(%rsi)
	movq	40(%rdx), %rbx
	movq	%rbx, 24(%rsi)
	movq	%rcx, 32(%rsi)
	movq	%rsi, %r10
	addq	$48, %rsi
	cmpq	$1, %r12
	je	L49C
L_true495:
then.497:
	/* Liveout:  GP={%r12 %r10 %r9 %r8 %rdi}  */
	/* block then<DAE4> (ep<DAE1>,c<DAE3>,letJoinK<DAE2>) */
	movq	$261, -8(%rsi)
	movabsq	$letJoinK.490, %r13
	movq	%r13, (%rsi)
	movq	%r10, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	16(%rdx), %r14
	movq	(%r14), %rdi
	movq	(%r12), %r8
	movq	48(%rdx), %r9
	movq	8(%rdx), %r12
	jmp	wrap.144
doGC49B:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	%r15, %rdi
	movabsq	$retGC499, %r8
	jmp	_ASM_InvokeGC
L49C:
else.496:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block else<DAF4> (ep<DAF2>,letJoinK<DAF3>) */
	movq	%r10, %rdi
	movq	48(%rdx), %r8
	jmp	letJoinK.48B
	.text
letJoinK.49E:
	movq	%rdi, %r15
	jmp	gcTest4A0
	/* live= GP={%r15} spilled=  */
retGC49F:
	movq	(%rdi), %r15
gcTest4A0:
	movq	%r11, %r10
	movq	448(%r10), %r12
	subq	%rsi, %r12
	jle	doGC4A1
check.49D:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block check<E3BC> (ep<DB3A>) */
	movq	%r11, %rbx
	movq	%rsi, 128(%rbx)
	movq	%rax, %r12
	movq	%rdi, %r13
	movq	%r13, -56(%rbp)
	movq	%r8, %r14
	movq	%r14, -64(%rbp)
	movq	%r9, %r13
	movq	%r11, %r14
	movq	%r11, %rcx
	movq	%rcx, %rdi
	movq	16(%r15), %rdx
	movq	%rdx, %rsi
	call	_PromoteObj
	movq	%rax, %rcx
	movq	%r12, %rax
	movq	-56(%rbp), %rdi
	movq	-64(%rbp), %r8
	movq	%r13, %r9
	movq	%r14, %r11
	movq	128(%rbx), %rsi
	movq	48(%r15), %rbx
	movq	(%rbx), %rdx
	movq	32(%r15), %r12
	movl	224(%r12), %r10d
	shlq	$3, %r10
	movq	%rcx, (%rdx,%r10,1)
	movq	%r11, %r13
	movq	%rsi, 128(%r13)
	movq	%rax, %r14
	movq	%rdi, %rbx
	movq	%r8, %r12
	movq	%r9, %rcx
	movq	%rcx, -64(%rbp)
	movq	%r11, %rdx
	movq	%rdx, -56(%rbp)
	movq	%r11, %rcx
	movq	%rcx, %rdi
	movq	$1, %rdx
	movq	%rdx, %rsi
	call	_PromoteObj
	movq	%rax, %rcx
	movq	%r14, %rax
	movq	%rbx, %rdi
	movq	%r12, %r8
	movq	-64(%rbp), %r9
	movq	-56(%rbp), %r11
	movq	128(%r13), %rsi
	movq	40(%r15), %r10
	movq	(%r10), %rbx
	movl	24(%r15), %edx
	shlq	$3, %rdx
	movq	%rcx, (%rbx,%rdx,1)
	movq	8(%r15), %rdi
	movq	32(%r15), %r8
	movq	16(%r15), %r9
	movq	$1, %r10
	jmp	schedulerLoop.14
doGC4A1:
	movq	$12, -8(%rsi)
	movq	%r15, (%rsi)
	movq	%rsi, %rbx
	addq	$16, %rsi
	movq	%rbx, %rdi
	movabsq	$retGC49F, %r8
	jmp	_ASM_InvokeGC
	.text
exh.4A3:
	movq	%rax, %rcx
	movq	%rdi, %rdx
	jmp	gcTest4A5
	/* live= GP={%rcx %rdx} spilled=  */
retGC4A4:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest4A5:
	movq	%r11, %r12
	movq	448(%r12), %rbx
	subq	%rsi, %rbx
	jle	doGC4A6
check.4A2:
	/* Liveout:  GP={%rdi}  */
	/* block check<E3BF> (ep<DB50>,_wild<DB4F>) */
	movq	8(%rdx), %rdi
	jmp	letJoinK.49E
doGC4A6:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	%r10, %rdi
	movabsq	$retGC4A4, %r8
	jmp	_ASM_InvokeGC
	.text
f1.4AC:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest4AE
	/* live= GP={%r10 %rcx %rdx %rbx} spilled=  */
retGC4AD:
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest4AE:
	movq	%r11, %r13
	movq	448(%r13), %r12
	subq	%rsi, %r12
	jle	doGC4AF
check.4A7:
	/* block check<E3C4> (ep<DB58>,x<DB59>,retK<DB5A>,exh<DB5B>) */
	movq	(%rbx), %r10
	movl	4(%r10), %r13d
	movq	(%rbx), %r12
	movl	4(%r12), %r14d
	movq	(%rbx), %r15
	movl	8(%r15), %r15d
	decl	%r15d
	cmpl	%r15d, %r14d
	jge	L_true4A8
else.4A9:
	/* block else<DB94> (ep<DB8F>,retK<DB93>,_cast_x<DB92>,new<DB91>,_t<DB90>) */
	incl	%r14d
	jmp	letJoinK.4AB
doGC4AF:
	movq	$36, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %r12
	addq	$40, %rsi
	movq	%r12, %rdi
	movabsq	$retGC4AD, %r8
	jmp	_ASM_InvokeGC
L_true4A8:
then.4AA:
	/* block then<DB8D> (ep<DB89>,retK<DB8C>,_cast_x<DB8B>,new<DB8A>) */
	xorl	%r14d, %r14d
letJoinK.4AB:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block letJoinK<DB79> (ep<DB74>,retK<DB78>,_cast_x<DB77>,new<DB76>,newR<DB75>) */
	movq	(%rbx), %r12
	leaq	4(%r12), %r10
	movq	(%rbx), %r15
	movl	%r14d, 4(%r15)
	movq	(%rbx), %rbx
	shll	$3, %r13d
	movslq	%r13d, %r10
	movq	%rdx, 16(%rbx,%r10,1)
	movq	%rcx, %rdi
	movq	$1, %r8
	jmp	letJoinK.B4
	.text
letJoinK.4B1:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest4B3
	/* live= GP={%rcx %rdx} spilled=  */
retGC4B2:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest4B3:
	movq	%r11, %r12
	movq	448(%r12), %rbx
	subq	%rsi, %rbx
	jle	doGC4B4
check.4B0:
	/* Liveout:  GP={%rdi}  */
	/* block check<E3C7> (ep<DB98>,_wild<DB97>) */
	movq	8(%rdx), %rdi
	jmp	letJoinK.49E
doGC4B4:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	%r10, %rdi
	movabsq	$retGC4B2, %r8
	jmp	_ASM_InvokeGC
	.text
foundWork.4B6:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest4B8
	/* live= GP={%rcx %rdx %rbx} spilled=  */
retGC4B7:
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest4B8:
	movq	%r11, %r10
	movq	448(%r10), %r10
	subq	%rsi, %r10
	jle	doGC4B9
check.4B5:
	/* Liveout:  GP={%r12 %r10 %r9 %r8 %rdi}  */
	/* block check<E3CB> (ep<DB32>,self<DB2D>,thds<DB2E>) */
	movq	$13071, -8(%rsi)
	movabsq	$letJoinK.49E, %r12
	movq	%r12, (%rsi)
	movq	16(%rbx), %r13
	movq	%r13, 8(%rsi)
	movq	24(%rbx), %r14
	movq	%r14, 16(%rsi)
	movl	32(%rbx), %r15d
	movl	%r15d, 24(%rsi)
	movq	%rdx, 32(%rsi)
	movq	40(%rbx), %rdx
	movq	%rdx, 40(%rsi)
	movq	48(%rbx), %r10
	movq	%r10, 48(%rsi)
	movq	%rsi, %rdx
	addq	$64, %rsi
	movq	$20, -8(%rsi)
	movabsq	$exh.4A3, %r12
	movq	%r12, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	$12, -8(%rsi)
	movq	24(%rbx), %r14
	movq	%r14, (%rsi)
	movq	%rsi, %r13
	addq	$16, %rsi
	movq	$133, -8(%rsi)
	movq	%r13, (%rsi)
	movabsq	$f1.4AC, %r10
	movq	%r10, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	$261, -8(%rsi)
	movabsq	$letJoinK.4B1, %r13
	movq	%r13, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	8(%rbx), %r14
	movq	(%r14), %rdi
	movq	%r15, %r8
	movq	%rcx, %r9
	jmp	app_D_w_uncurried.C
doGC4B9:
	movq	$647, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%rsi, %r15
	addq	$32, %rsi
	movq	%r15, %rdi
	movabsq	$retGC4B7, %r8
	jmp	_ASM_InvokeGC
	.text
spin.4BE:
	movq	%r9, %rdx
	movq	%r8, %rcx
	movq	%rdi, %rbx
	jmp	gcTest4C0
	/* live= GP={%rdx %rcx %rbx} spilled=  */
retGC4BF:
	movq	16(%rdi), %rdx
	movl	8(%rdi), %ecx
	movq	(%rdi), %rbx
gcTest4C0:
	movq	%r11, %r13
	movq	448(%r13), %r10
	subq	%rsi, %r10
	jle	doGC4C1
check.4BA:
	/* block check<E3CF> (ep<DC12>,i<DC13>,retK<DC14>) */
	cmpl	(%rbx), %ecx
	jge	L4C2
L_true4BB:
then.4BD:
	/* Liveout:  GP={%rdi}  */
	/* block then<DC1B> (retK<DC1A>) */
	movq	%rdx, %rdi
	jmp	letJoinK.78
doGC4C1:
	movq	$647, -8(%rsi)
	movq	%rbx, (%rsi)
	movl	%ecx, 8(%rsi)
	movq	%rdx, 16(%rsi)
	movq	%rsi, %r12
	addq	$32, %rsi
	movq	%r12, %rdi
	movabsq	$retGC4BF, %r8
	jmp	_ASM_InvokeGC
L4C2:
else.4BC:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block else<DC20> (ep<DC1D>,i<DC1F>,retK<DC1E>) */
	movq	%rbx, %rdi
	movq	%rcx, %r8
	incl	%r8d
	movq	%rdx, %r9
	jmp	spin.4BE
	.text
letJoinK.72:
	movq	%rdi, %rcx
	jmp	gcTest4C6
	/* live= GP={%rcx} spilled=  */
retGC4C5:
	movq	(%rdi), %rcx
gcTest4C6:
	movq	%r11, %r10
	movq	448(%r10), %rdx
	subq	%rsi, %rdx
	jle	doGC4C7
check.4C3:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<E3D1> (ep<DC31>) */
	movq	8(%rcx), %rdi
	xorl	%r8d, %r8d
	jmp	findRemoteWorkLp.4C4
doGC4C7:
	movq	$12, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%rsi, %rbx
	addq	$16, %rsi
	movq	%rbx, %rdi
	movabsq	$retGC4C5, %r8
	jmp	_ASM_InvokeGC
	.text
k.7A:
	movq	%rax, %rcx
	movq	%rdi, %rdx
	jmp	gcTest4CA
	/* live= GP={%rcx %rdx} spilled=  */
retGC4C9:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest4CA:
	movq	%r11, %r12
	movq	448(%r12), %rbx
	subq	%rsi, %rbx
	jle	doGC4CB
check.4C8:
	/* Liveout:  GP={%rdi}  */
	/* block check<E3D4> (ep<DC40>,x<DC3F>) */
	movq	8(%rdx), %rdi
	jmp	letJoinK.72
doGC4CB:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	%r10, %rdi
	movabsq	$retGC4C9, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.4CD:
	movq	%rdi, %rdx
	jmp	gcTest4CF
	/* live= GP={%rdx} spilled=  */
retGC4CE:
	movq	(%rdi), %rdx
gcTest4CF:
	movq	%r11, %r15
	movq	448(%r15), %rbx
	subq	%rsi, %rbx
	jle	doGC4D0
check.4CC:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block check<E3D6> (ep<DC09>) */
	movq	$10, -8(%rsi)
	movq	16(%rdx), %r13
	movl	$1, %r12d
	movl	(%r13), %ecx
	shll	%cl, %r12d
	movl	%r12d, (%rsi)
	movq	%rsi, %r10
	addq	$16, %rsi
	movq	$133, -8(%rsi)
	movq	%r10, (%rsi)
	movabsq	$spin.4BE, %r15
	movq	%r15, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	$1545, -8(%rsi)
	movabsq	$letJoinK.78, %rbx
	movq	%rbx, (%rsi)
	movl	8(%rdx), %r10d
	movl	%r10d, 8(%rsi)
	movq	16(%rdx), %r12
	movq	%r12, 16(%rsi)
	movq	24(%rdx), %r13
	movq	%r13, 24(%rsi)
	movq	%rsi, %rcx
	addq	$40, %rsi
	movq	(%r14), %rdi
	xorl	%r8d, %r8d
	movq	%rcx, %r9
	jmp	spin.4BE
doGC4D0:
	movq	$12, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rsi, %r14
	addq	$16, %rsi
	movq	%r14, %rdi
	movabsq	$retGC4CE, %r8
	jmp	_ASM_InvokeGC
	.text
k.4D2:
	movq	%rax, %rcx
	movq	%rdi, %rdx
	jmp	gcTest4D4
	/* live= GP={%rcx %rdx} spilled=  */
retGC4D3:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest4D4:
	movq	%r11, %r12
	movq	448(%r12), %rbx
	subq	%rsi, %rbx
	jle	doGC4D5
check.4D1:
	/* Liveout:  GP={%rdi}  */
	/* block check<E3D9> (ep<DC69>,x<DC68>) */
	movq	8(%rdx), %rdi
	jmp	letJoinK.4CD
doGC4D5:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	%r10, %rdi
	movabsq	$retGC4D3, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.4E1:
	movq	%r8, %rcx
	movq	%rdi, %r15
	jmp	gcTest4E3
	/* live= GP={%rcx %r15} spilled=  */
retGC4E2:
	movq	8(%rdi), %rcx
	movq	(%rdi), %r15
gcTest4E3:
	movq	%r11, %rdx
	movq	448(%rdx), %rbx
	subq	%rsi, %rbx
	jle	doGC4E4
check.4D6:
	/* block check<E3DC> (ep<DBF3>,stolenThds<DBEC>) */
	cmpq	$1, %rcx
	je	L4E5
L_true4D7:
then.4D9:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block then<DBF9> (ep<DBF7>,stolenThds<DBF8>) */
	movq	8(%r15), %rdi
	movq	48(%r15), %r8
	movq	%rcx, %r9
	jmp	foundWork.4B6
doGC4E4:
	movq	$20, -8(%rsi)
	movq	%r15, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	%rcx, %rdi
	movabsq	$retGC4E2, %r8
	jmp	_ASM_InvokeGC
L4E5:
else.4D8:
	/* block else<DBFE> (ep<DBFD>) */
	movq	%rax, %r10
	movq	%r10, -56(%rbp)
	movq	%rdi, %rbx
	movq	%r8, %r12
	movq	%r9, %r13
	movq	%rsi, %r14
	movq	%r11, %rcx
	movq	%rcx, -64(%rbp)
	call	_GetNumVProcs
	movq	%rax, %rcx
	movq	-56(%rbp), %rax
	movq	%rbx, %rdi
	movq	%r12, %r8
	movq	%r13, %r9
	movq	%r14, %rsi
	movq	-64(%rbp), %r11
	cmpl	%ecx, 40(%r15)
	jle	L4E6
L_true4DA:
then.4DC:
	/* block then<DC03> (ep<DC02>) */
	movq	$1545, -8(%rsi)
	movabsq	$letJoinK.4CD, %r12
	movq	%r12, (%rsi)
	movl	16(%r15), %r13d
	movl	%r13d, 8(%rsi)
	movq	24(%r15), %r14
	movq	%r14, 16(%rsi)
	movq	32(%r15), %rcx
	movq	%rcx, 24(%rsi)
	movq	%rsi, %r10
	addq	$40, %rsi
	movq	48(%r15), %rbx
	movq	8(%rbx), %rdx
	cmpq	$1, %rdx
	je	S_case4DD
	cmpq	$3, %rdx
	je	S_case4DF
S_case4DD:
case.4DE:
	/* Liveout:  GP={%rdi}  */
	/* block case<DC5F> (ep<DC5D>,letJoinK<DC5E>) */
	movq	48(%r15), %r15
	movq	$1, (%r15)
	movq	%r10, %rdi
	jmp	letJoinK.4CD
L4E6:
else.4DB:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block else<DC77> (ep<DC76>) */
	movq	32(%r15), %rdi
	movl	40(%r15), %ebx
	movq	%rbx, %r8
	incl	%r8d
	jmp	findRemoteWorkLp.4C4
S_case4DF:
case.4E0:
	/* Liveout:  GP={%rax %rdi}  */
	/* block case<DC65> (ep<DC63>,letJoinK<DC64>) */
	movq	48(%r15), %r12
	movq	$1, 8(%r12)
	movq	$20, -8(%rsi)
	movabsq	$k.4D2, %r14
	movq	%r14, (%rsi)
	movq	%r10, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movq	$1, (%rsi)
	movq	%r13, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	48(%r15), %rbx
	movq	32(%rbx), %rdx
	movq	8(%rdx), %r10
	movq	48(%r15), %r12
	movq	%r10, 32(%r12)
	movq	(%rdx), %r13
	movq	(%r13), %r14
	movq	%rcx, %rax
	movq	%r13, %rdi
	jmp	*%r14
	.text
canSteal.4EF:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
gcTest4F1:
	movq	%r11, %r12
	movq	448(%r12), %r12
	subq	%rsi, %r12
	jle	doGC4F2
check.4E7:
	/* block check<E3E1> (ep<DCC6>,_t<DCC7>,retK<DCC8>,exh<DCC9>) */
	movl	(%rdx), %r15d
	cmpl	4(%rdx), %r15d
	jg	L4F3
L_true4E8:
then.4EA:
	/* block then<DCE7> (retK<DCE6>,_t<DCE5>) */
	movl	4(%rdx), %r13d
	subl	(%rdx), %r13d
letJoinK.4EB:
	/* block letJoinK<DCD8> (retK<DCD7>,size<DCD6>) */
	cmpl	$1, %r13d
	jg	L_true4EC
else.4ED:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block else<DCE1> (retK<DCE0>) */
	movq	%rcx, %rdi
	movq	$1, %r8
	jmp	letJoinK.CB
doGC4F2:
	movq	$36, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %rbx
	addq	$40, %rsi
	movq	%rbx, %rdi
	movabsq	$retGC4F0, %r8
	jmp	_ASM_InvokeGC
	/* live= GP={%r10 %rcx %rdx %rbx} spilled=  */
retGC4F0:
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
	jmp	gcTest4F1
L4F3:
else.4E9:
	/* block else<DCF4> (retK<DCF3>,_t<DCF2>) */
	movl	8(%rdx), %r14d
	subl	(%rdx), %r14d
	movl	4(%rdx), %edx
	leal	(%r14,%rdx,1), %r13d
	jmp	letJoinK.4EB
L_true4EC:
then.4EE:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block then<DCDC> (retK<DCDB>) */
	movq	%rcx, %rdi
	movq	$3, %r8
	jmp	letJoinK.CB
	.text
letJoinK.4F9:
	movq	%r8, %rbx
	movq	%rdi, %rdx
	jmp	gcTest4FB
	/* live= GP={%rbx %rdx} spilled=  */
retGC4FA:
	movq	8(%rdi), %rbx
	movq	(%rdi), %rdx
gcTest4FB:
	movq	%r11, %r13
	movq	448(%r13), %r14
	subq	%rsi, %r14
	jle	doGC4FC
check.4F4:
	/* block check<E3E4> (ep<DD0A>,stolenThread<DD08>) */
	cmpq	$1, %rbx
	jne	L_true4F5
	movq	%rdx, -56(%rbp)
else.4F6:
	/* flushLoads */
	/* block else<DD23> (ep<DD22>) */
	movq	$12, -8(%rsi)
	movq	$1, (%rsi)
	movq	%rsi, %rcx
	addq	$16, %rsi
	movq	%r11, %r15
	movq	%rsi, 128(%r15)
	movq	%rax, %r14
	movq	%rdi, %rdx
	movq	%rdx, -64(%rbp)
	movq	%r8, %r13
	movq	%r9, %r12
	movq	%r11, %rbx
	movq	%r11, %r10
	movq	%r10, %rdi
	movq	%rcx, %rsi
	call	_PromoteObj
	movq	%rax, %rcx
	movq	%r14, %rax
	movq	-64(%rbp), %rdi
	movq	%r13, %r8
	movq	%r12, %r9
	movq	%rbx, %r11
	movq	128(%r15), %rsi
	movq	-56(%rbp), %rbx
	movq	8(%rbx), %rdx
	movq	%rcx, (%rdx)
	jmp	letJoinK.4F8
doGC4FC:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rbx, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	%r12, %rdi
	movabsq	$retGC4FA, %r8
	jmp	_ASM_InvokeGC
L_true4F5:
	movq	%rbx, -64(%rbp)
	movq	%rdx, -56(%rbp)
then.4F7:
	/* flushLoads */
	/* block then<DD19> (ep<DD17>,stolenThread<DD18>) */
	movq	$20, -8(%rsi)
	movq	-64(%rbp), %r10
	movq	(%r10), %rcx
	movq	%rcx, (%rsi)
	movq	$1, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	$12, -8(%rsi)
	movq	%r15, (%rsi)
	movq	%rsi, %rcx
	addq	$16, %rsi
	movq	%r11, %r15
	movq	%rsi, 128(%r15)
	movq	%rax, %r14
	movq	%rdi, %r13
	movq	%r8, %r12
	movq	%r12, -72(%rbp)
	movq	%r9, %r12
	movq	%r11, %rbx
	movq	%r11, %rdx
	movq	%rdx, %rdi
	movq	%rcx, %rsi
	call	_PromoteObj
	movq	%rax, %rcx
	movq	%r14, %rax
	movq	%r13, %rdi
	movq	-72(%rbp), %r8
	movq	%r12, %r9
	movq	%rbx, %r11
	movq	128(%r15), %rsi
	movq	-64(%rbp), %rdx
	movq	-56(%rbp), %rbx
	movq	8(%rbx), %r10
	movq	%rcx, (%r10)
letJoinK.4F8:
	/* Liveout:  GP={%rax %rdi}  */
	/* block letJoinK<DD0F> () */
	movq	$3, (%r11)
	movq	32(%r11), %rbx
	movq	8(%rbx), %r10
	movq	%r10, 32(%r11)
	movq	(%rbx), %r12
	movq	(%r12), %r13
	movq	$1, %r14
	movq	%r14, %rax
	movq	%r12, %rdi
	jmp	*%r13
	.text
exh.4FE:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest500
	/* live= GP={%rcx %rdx} spilled=  */
retGC4FF:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest500:
	movq	%r11, %r12
	movq	448(%r12), %rbx
	subq	%rsi, %rbx
	jle	doGC501
check.4FD:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<E3E7> (ep<DD2A>,_wild<DD29>) */
	movq	8(%rdx), %rdi
	movq	$1, %r8
	jmp	letJoinK.4F9
doGC501:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	%r10, %rdi
	movabsq	$retGC4FF, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.503:
	movq	%rdi, %rcx
	jmp	gcTest505
	/* live= GP={%rcx} spilled=  */
retGC504:
	movq	(%rdi), %rcx
gcTest505:
	movq	%r11, %r10
	movq	448(%r10), %rdx
	subq	%rsi, %rdx
	jle	doGC506
check.502:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<E3E9> (ep<DD3A>) */
	movq	8(%rcx), %rdi
	movq	16(%rcx), %r8
	jmp	letJoinK.4F9
doGC506:
	movq	$12, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%rsi, %rbx
	addq	$16, %rsi
	movq	%rbx, %rdi
	movabsq	$retGC504, %r8
	jmp	_ASM_InvokeGC
	.text
exh.508:
	movq	%rax, %rcx
	movq	%rdi, %rdx
	jmp	gcTest50A
	/* live= GP={%rcx %rdx} spilled=  */
retGC509:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest50A:
	movq	%r11, %r12
	movq	448(%r12), %rbx
	subq	%rsi, %rbx
	jle	doGC50B
check.507:
	/* Liveout:  GP={%rdi}  */
	/* block check<E3EC> (ep<DD41>,_wild<DD40>) */
	movq	8(%rdx), %rdi
	jmp	letJoinK.503
doGC50B:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	%r10, %rdi
	movabsq	$retGC509, %r8
	jmp	_ASM_InvokeGC
	.text
f1.50D:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest50F
	/* live= GP={%r10 %rcx %rdx %rbx} spilled=  */
retGC50E:
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest50F:
	movq	%r11, %r15
	movq	448(%r15), %r12
	subq	%rsi, %r12
	jle	doGC510
check.50C:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<E3F1> (ep<DD48>,x<DD49>,retK<DD4A>,exh<DD4B>) */
	movq	(%rdx), %r13
	decl	12(%r13)
	movq	%rcx, %rdi
	movq	$1, %r8
	jmp	letJoinK.B4
doGC510:
	movq	$36, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %r14
	addq	$40, %rsi
	movq	%r14, %rdi
	movabsq	$retGC50E, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.512:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest514
	/* live= GP={%rcx %rdx} spilled=  */
retGC513:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest514:
	movq	%r11, %r12
	movq	448(%r12), %rbx
	subq	%rsi, %rbx
	jle	doGC515
check.511:
	/* Liveout:  GP={%rdi}  */
	/* block check<E3F4> (ep<DD5F>,_wild<DD5E>) */
	movq	8(%rdx), %rdi
	jmp	letJoinK.503
doGC515:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	%r10, %rdi
	movabsq	$retGC513, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.522:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest524
	/* live= GP={%rcx %rdx} spilled=  */
retGC523:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest524:
	movq	%r11, %rbx
	movq	448(%rbx), %rbx
	subq	%rsi, %rbx
	jle	doGC525
check.516:
	/* block check<E3F7> (ep<DD32>,victimDeques<DD30>) */
	cmpq	$1, %rcx
	je	L526
L_true517:
then.519:
	/* block then<DD6A> (ep<DD68>,victimDeques<DD69>) */
	movq	(%rcx), %r15
	movq	(%r15), %r10
	movl	4(%r10), %ecx
	cmpl	(%r10), %ecx
	je	L_true520
else.51D:
	/* block else<DD7B> (ep<DD79>,_t<DD7A>) */
	movl	(%r10), %r14d
	movq	%r14, %r13
	shll	$3, %r13d
	movslq	%r13d, %r12
	movq	16(%r10,%r12,1), %r12
	shll	$3, %r14d
	movslq	%r14d, %r14
	movq	$1, 16(%r10,%r14,1)
	movl	(%r10), %r13d
	movl	8(%r10), %r15d
	decl	%r15d
	cmpl	%r15d, %r13d
	jl	L527
L_true51E:
then.51F:
	/* block then<DDA4> (ep<DDA1>,_t<DDA3>,elt<DDA2>) */
	xorl	%r13d, %r13d
	jmp	letJoinK.51B
L527:
else.51C:
	/* block else<DDAA> (ep<DDA6>,_t<DDA9>,elt<DDA8>,_t<DDA7>) */
	incl	%r13d
letJoinK.51B:
	/* block letJoinK<DD9D> (ep<DD99>,_t<DD9C>,elt<DD9B>,oldR<DD9A>) */
	movl	%r13d, (%r10)
	movq	$12, -8(%rsi)
	movq	%r12, (%rsi)
	movq	%rsi, %rcx
	addq	$16, %rsi
	jmp	letJoinK.51A
L526:
else.518:
	/* block else<DDAE> (ep<DDAD>) */
	movq	$1, %rcx
letJoinK.51A:
	/* Liveout:  GP={%r12 %r10 %r9 %r8 %rdi}  */
	/* block letJoinK<DD38> (ep<DD36>,thd<DD37>) */
	movq	$775, -8(%rsi)
	movabsq	$letJoinK.503, %r10
	movq	%r10, (%rsi)
	movq	24(%rdx), %r12
	movq	%r12, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%rsi, %rbx
	addq	$32, %rsi
	movq	$20, -8(%rsi)
	movabsq	$exh.508, %r13
	movq	%r13, (%rsi)
	movq	%rbx, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	$18, -8(%rsi)
	movq	$1, (%rsi)
	movabsq	$f1.50D, %r15
	movq	%r15, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	$261, -8(%rsi)
	movabsq	$letJoinK.512, %rcx
	movq	%rcx, (%rsi)
	movq	%rbx, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	8(%rdx), %rbx
	movq	(%rbx), %rdi
	movq	%r14, %r8
	movq	16(%rdx), %r9
	jmp	app_D_w_uncurried.C
doGC525:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	movq	%rdx, %rdi
	movabsq	$retGC523, %r8
	jmp	_ASM_InvokeGC
L_true520:
then.521:
	/* block then<DD77> (ep<DD76>) */
	movq	$1, %rcx
	jmp	letJoinK.51A
	.text
letJoinK.52C:
	movq	%r8, %r10
	movq	%rdi, %rbx
	jmp	gcTest52E
	/* live= GP={%r10 %rbx} spilled=  */
retGC52D:
	movq	8(%rdi), %r10
	movq	(%rdi), %rbx
gcTest52E:
	movq	%r11, %rcx
	movq	448(%rcx), %rdx
	subq	%rsi, %rdx
	jle	doGC52F
check.528:
	/* block check<E3FA> (ep<DCB0>,muggedThreads<DCAB>) */
	cmpq	$1, %r10
	je	L530
L_true529:
	movq	%rbx, -56(%rbp)
then.52B:
	/* Liveout:  GP={%rax %rdi}  */
	/* flushLoads */
	/* block then<DCB6> (ep<DCB4>,muggedThreads<DCB5>) */
	movq	$12, -8(%rsi)
	movq	%r10, (%rsi)
	movq	%rsi, %rcx
	addq	$16, %rsi
	movq	%r11, %r15
	movq	%rsi, 128(%r15)
	movq	%rax, %r14
	movq	%rdi, %r13
	movq	%r8, %r12
	movq	%r9, %rbx
	movq	%r11, %rdx
	movq	%rdx, -64(%rbp)
	movq	%r11, %r10
	movq	%r10, %rdi
	movq	%rcx, %rsi
	call	_PromoteObj
	movq	%rax, %rcx
	movq	%r14, %rax
	movq	%r13, %rdi
	movq	%r12, %r8
	movq	%rbx, %r9
	movq	-64(%rbp), %r11
	movq	128(%r15), %rsi
	movq	-56(%rbp), %rbx
	movq	32(%rbx), %rdx
	movq	%rcx, (%rdx)
	movq	$3, (%r11)
	movq	32(%r11), %rbx
	movq	8(%rbx), %r10
	movq	%r10, 32(%r11)
	movq	(%rbx), %r12
	movq	(%r12), %r13
	movq	$1, %r14
	movq	%r14, %rax
	movq	%r12, %rdi
	jmp	*%r13
doGC52F:
	movq	$20, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%r10, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	%r15, %rdi
	movabsq	$retGC52D, %r8
	jmp	_ASM_InvokeGC
L530:
else.52A:
	/* Liveout:  GP={%r12 %r10 %r9 %r8 %rdi}  */
	/* block else<DCC3> (ep<DCC2>) */
	movq	$18, -8(%rsi)
	movq	$1, (%rsi)
	movabsq	$canSteal.4EF, %r12
	movq	%r12, 8(%rsi)
	movq	%rsi, %r10
	movq	%r10, -56(%rbp)
	addq	$24, %rsi
	movq	%r11, %r13
	movq	%rsi, 128(%r13)
	movq	%rax, %r14
	movq	%rdi, %r15
	movq	%r8, %r12
	movq	%r12, -64(%rbp)
	movq	%r9, %rcx
	movq	%rcx, -72(%rbp)
	movq	%r11, %r12
	movq	40(%rbx), %rcx
	movq	%rcx, %rdi
	movq	24(%rbx), %rdx
	movq	%rdx, %rsi
	call	_M_LocalDeques
	movq	%rax, %rcx
	movq	%r14, %rax
	movq	%r15, %rdi
	movq	-64(%rbp), %r8
	movq	-72(%rbp), %r9
	movq	%r12, %r11
	movq	128(%r13), %rsi
	movq	$261, -8(%rsi)
	movabsq	$letJoinK.4F9, %r10
	movq	%r10, (%rsi)
	movq	32(%rbx), %r12
	movq	%r12, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	movq	$261, -8(%rsi)
	movabsq	$exh.4FE, %r13
	movq	%r13, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	$1801, -8(%rsi)
	movabsq	$letJoinK.522, %r14
	movq	%r14, (%rsi)
	movq	8(%rbx), %r15
	movq	%r15, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%rdx, 24(%rsi)
	movq	%rsi, %r10
	addq	$40, %rsi
	movq	16(%rbx), %rdx
	movq	(%rdx), %rdi
	movq	-56(%rbp), %r8
	movq	%rcx, %r9
	jmp	filter_D_w_uncurried.E1
	.text
thief.532:
	movq	%rax, %rcx
	movq	%rdi, %rdx
	jmp	gcTest534
	/* live= GP={%rcx %rdx} spilled=  */
retGC533:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest534:
	movq	%r11, %rbx
	movq	448(%rbx), %rbx
	subq	%rsi, %rbx
	jle	doGC535
check.531:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block check<E3FD> (ep<DCA6>,_wild<DCA1>) */
	movq	$3, (%r11)
	movq	$2829, -8(%rsi)
	movabsq	$letJoinK.52C, %r10
	movq	%r10, (%rsi)
	movq	8(%rdx), %r12
	movq	%r12, 8(%rsi)
	movq	16(%rdx), %r13
	movq	%r13, 16(%rsi)
	movq	32(%rdx), %r14
	movq	%r14, 24(%rsi)
	movq	40(%rdx), %r15
	movq	%r15, 32(%rsi)
	movq	%r11, 40(%rsi)
	movq	%rsi, %r10
	addq	$56, %rsi
	movq	24(%rdx), %rcx
	movq	(%rcx), %rdi
	movq	%r11, %r8
	movq	32(%rdx), %r9
	jmp	mug_D_from_D_atomic.2D3
doGC535:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	movq	%rdx, %rdi
	movabsq	$retGC533, %r8
	jmp	_ASM_InvokeGC
	.text
L_true537:
	movq	-56(%rbp), %rbx
then.539:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block then<DDCC> (ep<DDCA>,retK<DDCB>) */
	pause
	movq	%r13, %rdi
	movq	%rbx, %r8
lp.53E:
	movq	%r8, %r10
	movq	%rdi, %r13
gcTest540:
	movq	%r11, %r12
	movq	448(%r12), %r14
	subq	%rsi, %r14
	jle	doGC541
	movq	%r10, -56(%rbp)
check.536:
	/* block check<E400> (ep<DDBB>,retK<DDBC>) */
	movq	(%r13), %r12
	movq	384(%r12), %r15
	movq	%r15, -64(%rbp)
	movq	$28, -8(%rsi)
	movq	16(%r13), %r14
	movq	%r14, (%rsi)
	movq	8(%r13), %r15
	movq	%r15, 8(%rsi)
	movq	-64(%rbp), %rcx
	movq	%rcx, 16(%rsi)
	movq	%rsi, %rcx
	addq	$32, %rsi
	movq	%r11, %r15
	movq	%rsi, 128(%r15)
	movq	%rax, %rdx
	movq	%rdx, -72(%rbp)
	movq	%rdi, %rbx
	movq	%rbx, -80(%rbp)
	movq	%r8, %r14
	movq	%r9, %r12
	movq	%r11, %rbx
	movq	%r11, %rdx
	movq	%rdx, %rdi
	movq	%rcx, %rsi
	call	_PromoteObj
	movq	%rax, %rcx
	movq	-72(%rbp), %rax
	movq	-80(%rbp), %rdi
	movq	%r14, %r8
	movq	%r12, %r9
	movq	%rbx, %r11
	movq	128(%r15), %rsi
	movq	(%r13), %rbx
	leaq	384(%rbx), %rdx
	movq	-64(%rbp), %rax
	movq	(%r13), %r10
	lock
	cmpxchgq	%rcx, 384(%r10)
	movq	%rax, %rbx
	cmpq	-64(%rbp), %rbx
	jne	L_true537
	movq	-56(%rbp), %rdx
	movq	%r13, %rcx
else.538:
	/* block else<DDD0> (ep<DDCE>,retK<DDCF>) */
	movq	(%rcx), %r13
	movq	16(%r13), %r12
	cmpq	$1, %r12
	je	letJoinK.53B
L542:
	cmpq	$3, %r12
	je	S_case53C
S_case53A:
	jmp	letJoinK.53B
S_case53C:
	movq	%rdx, -56(%rbp)
case.53D:
	/* block case<DDDC> (ep<DDDA>,retK<DDDB>) */
	movq	%rax, %r13
	movq	%rdi, %r12
	movq	%r8, %rbx
	movq	%r9, %r15
	movq	%rsi, %r14
	movq	%r11, %r10
	movq	%r10, -64(%rbp)
	movq	(%rcx), %rcx
	movq	%rcx, %rdi
	call	_VProcWake
	movq	%r13, %rax
	movq	%r12, %rdi
	movq	%rbx, %r8
	movq	%r15, %r9
	movq	%r14, %rsi
	movq	-64(%rbp), %r11
	movq	-56(%rbp), %rdx
letJoinK.53B:
	/* Liveout:  GP={%rdi}  */
	/* block letJoinK<DDD5> (retK<DDD4>) */
	movq	%rdx, %rdi
	jmp	letJoinK.15
doGC541:
	movq	$20, -8(%rsi)
	movq	%r13, (%rsi)
	movq	%r10, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	%rbx, %rdi
	movabsq	$retGC53F, %r8
	jmp	_ASM_InvokeGC
retGC53F:
	movq	8(%rdi), %r10
	movq	(%rdi), %r13
	jmp	gcTest540
	.text
preempt.16:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest548
	/* live= GP={%rcx %rdx} spilled=  */
retGC547:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest548:
	movq	%r11, %r14
	movq	448(%r14), %rbx
	subq	%rsi, %rbx
	jle	doGC549
check.543:
	/* block check<E403> (ep<DDE7>,retK<DDE8>) */
	movq	(%rdx), %r12
	movq	448(%r12), %r10
	movq	(%rdx), %r15
	leaq	448(%r15), %r14
	movq	%r10, %rax
	xorq	%rbx, %rbx
	movq	(%rdx), %r12
	lock
	cmpxchgq	%rbx, 448(%r12)
	movq	%rax, %r13
	cmpq	%r10, %r13
	je	L54A
L_true544:
then.546:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block then<DDF5> (ep<DDF3>,retK<DDF4>) */
	pause
	movq	%rdx, %rdi
	movq	%rcx, %r8
	jmp	preempt.16
doGC549:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	%r13, %rdi
	movabsq	$retGC547, %r8
	jmp	_ASM_InvokeGC
L54A:
else.545:
	/* Liveout:  GP={%rdi}  */
	/* block else<DDF9> (retK<DDF8>) */
	movq	%rcx, %rdi
	jmp	letJoinK.17
	.text
k.54C:
	movq	%rax, %rcx
	movq	%rdi, %rdx
	jmp	gcTest54E
	/* live= GP={%rcx %rdx} spilled=  */
retGC54D:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest54E:
	movq	%r11, %r13
	movq	448(%r13), %rbx
	subq	%rsi, %rbx
	jle	doGC54F
check.54B:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<E406> (ep<DE1A>,x<DE17>) */
	movq	$3, (%r11)
	movq	8(%rdx), %r10
	movq	(%r10), %rdi
	movq	16(%rdx), %r8
	jmp	wait.18
doGC54F:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	%r12, %rdi
	movabsq	$retGC54D, %r8
	jmp	_ASM_InvokeGC
	.text
wait.18:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest555
	/* live= GP={%rcx %rdx} spilled=  */
retGC554:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest555:
	movq	%r11, %r10
	movq	448(%r10), %rbx
	subq	%rsi, %rbx
	jle	doGC556
check.550:
	/* block check<E409> (ep<DE05>,retK<DE06>) */
	movq	(%rdx), %r14
	movq	(%r14), %r13
	cmpq	$1, %r13
	je	L557
L_true551:
then.553:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block then<DE10> (retK<DE0F>,_t<DE0E>) */
	movq	%rcx, %rdi
	movq	(%r13), %r8
	jmp	letJoinK.4E1
doGC556:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	%rbx, %rdi
	movabsq	$retGC554, %r8
	jmp	_ASM_InvokeGC
L557:
else.552:
	/* Liveout:  GP={%rax %rdi}  */
	/* block else<DE16> (ep<DE14>,retK<DE15>) */
	pause
	movq	$133, -8(%rsi)
	movq	%rdx, (%rsi)
	movabsq	$wait.18, %rbx
	movq	%rbx, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	$28, -8(%rsi)
	movabsq	$k.54C, %r12
	movq	%r12, (%rsi)
	movq	%r15, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%rsi, %r10
	addq	$32, %rsi
	movq	$20, -8(%rsi)
	movq	$1, (%rsi)
	movq	%r10, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	8(%rdx), %r14
	movq	32(%r14), %r13
	movq	8(%r13), %r15
	movq	8(%rdx), %rcx
	movq	%r15, 32(%rcx)
	movq	(%r13), %r10
	movq	(%r10), %rdx
	movq	%r12, %rax
	movq	%r10, %rdi
	jmp	*%rdx
	.text
letJoinK.3E8:
	movq	%r8, %rdx
	movq	%rdi, %r14
gcTest561:
	movq	%r11, %rcx
	movq	448(%rcx), %rbx
	subq	%rsi, %rbx
	jle	doGC562
check.558:
	/* block check<E40C> (ep<DC8C>,victimVP<DC85>) */
	cmpq	$1, %rdx
	jne	L_true559
else.55A:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block else<DE38> (ep<DE37>) */
	movq	40(%r14), %rdi
	movq	$1, %r8
	jmp	letJoinK.4E1
doGC562:
	movq	$20, -8(%rsi)
	movq	%r14, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	%r15, %rdi
	movabsq	$retGC560, %r8
	jmp	_ASM_InvokeGC
	/* live= GP={%rdx %r14} spilled=  */
retGC560:
	movq	8(%rdi), %rdx
	movq	(%rdi), %r14
	jmp	gcTest561
L_true559:
then.55B:
	/* block then<DC92> (ep<DC90>,victimVP<DC91>) */
	movq	(%rdx), %r10
	movq	(%r10), %rbx
	movq	56(%r14), %r15
	movq	(%r15), %r13
	movl	224(%rbx), %ecx
	shlq	$3, %rcx
	movq	(%r13,%rcx,1), %r12
	cmpq	$1, %r12
	jne	L563
S_case55C:
	movq	%rbx, -56(%rbp)
case.55D:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block case<DC9D> (ep<DC9B>,victimVP<DC9C>) */
	movq	$12, -8(%rsi)
	movq	$1, (%rsi)
	movq	%rsi, %rcx
	addq	$16, %rsi
	movq	%r11, %r15
	movq	%rsi, 128(%r15)
	movq	%rax, %rbx
	movq	%rdi, %r12
	movq	%r8, %r10
	movq	%r10, -64(%rbp)
	movq	%r9, %r13
	movq	%r13, -72(%rbp)
	movq	%r11, %r13
	movq	%r11, %rdx
	movq	%rdx, %rdi
	movq	%rcx, %rsi
	call	_PromoteObj
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%r12, %rdi
	movq	-64(%rbp), %r8
	movq	-72(%rbp), %r9
	movq	%r13, %r11
	movq	128(%r15), %rsi
	movq	$5901, -8(%rsi)
	movabsq	$thief.532, %r10
	movq	%r10, (%rsi)
	movq	8(%r14), %rdx
	movq	%rdx, 8(%rsi)
	movq	16(%r14), %rbx
	movq	%rbx, 16(%rsi)
	movq	24(%r14), %r10
	movq	%r10, 24(%rsi)
	movq	32(%r14), %r12
	movq	%r12, 32(%rsi)
	movq	%rcx, 40(%rsi)
	movq	%rsi, %rbx
	addq	$56, %rsi
	movq	$775, -8(%rsi)
	movq	-56(%rbp), %r15
	movq	%r15, (%rsi)
	movq	%rbx, 8(%rsi)
	movq	24(%r11), %r15
	movq	%r15, 16(%rsi)
	movq	%rsi, %r13
	addq	$32, %rsi
	movq	$133, -8(%rsi)
	movq	%r13, (%rsi)
	movabsq	$lp.53E, %rbx
	movq	%rbx, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	movq	$1291, -8(%rsi)
	movabsq	$letJoinK.15, %r12
	movq	%r12, (%rsi)
	movq	40(%r14), %r13
	movq	%r13, 8(%rsi)
	movq	-56(%rbp), %rbx
	movq	%rbx, 16(%rsi)
	movq	%rcx, 24(%rsi)
	movq	48(%r14), %r14
	movq	%r14, 32(%rsi)
	movq	%rsi, %r10
	addq	$48, %rsi
	movq	(%rdx), %rdi
	movq	%r10, %r8
	jmp	lp.53E
L563:
	cmpq	$3, %r12
	jne	S_case55C
S_case55E:
case.55F:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block case<DE33> (ep<DE32>) */
	movq	40(%r14), %rdi
	movq	$1, %r8
	jmp	letJoinK.4E1
	.text
letJoinK.568:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest56A
	/* live= GP={%rcx %rdx} spilled=  */
retGC569:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest56A:
	movq	%r11, %rbx
	movq	448(%rbx), %rbx
	subq	%rsi, %rbx
	jle	doGC56B
check.564:
	/* block check<E40F> (ep<DBE8>,muggedThreads<DBDC>) */
	movq	$3343, -8(%rsi)
	movabsq	$letJoinK.4E1, %r12
	movq	%r12, (%rsi)
	movq	48(%rdx), %r13
	movq	%r13, 8(%rsi)
	movl	56(%rdx), %r14d
	movl	%r14d, 16(%rsi)
	movq	64(%rdx), %r15
	movq	%r15, 24(%rsi)
	movq	72(%rdx), %rbx
	movq	%rbx, 32(%rsi)
	movl	80(%rdx), %r10d
	movl	%r10d, 40(%rsi)
	movq	88(%rdx), %r12
	movq	%r12, 48(%rsi)
	movq	%rsi, %r10
	addq	$64, %rsi
	cmpq	$1, %rcx
	je	L56C
L_true565:
then.567:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block then<DC80> (muggedThreads<DC7F>,letJoinK<DC7E>) */
	movq	%r10, %rdi
	movq	%rcx, %r8
	jmp	letJoinK.4E1
doGC56B:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	%rcx, %rdi
	movabsq	$retGC569, %r8
	jmp	_ASM_InvokeGC
L56C:
else.566:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block else<DC84> (ep<DC82>,letJoinK<DC83>) */
	movq	$22289, -8(%rsi)
	movabsq	$letJoinK.3E8, %r14
	movq	%r14, (%rsi)
	movq	8(%rdx), %r15
	movq	%r15, 8(%rsi)
	movq	16(%rdx), %rcx
	movq	%rcx, 16(%rsi)
	movq	24(%rdx), %rbx
	movq	%rbx, 24(%rsi)
	movq	32(%rdx), %r12
	movq	%r12, 32(%rsi)
	movq	%r10, 40(%rsi)
	movq	88(%rdx), %r13
	movq	%r13, 48(%rsi)
	movq	96(%rdx), %r14
	movq	%r14, 56(%rsi)
	movq	%rsi, %r13
	addq	$72, %rsi
	movq	40(%rdx), %r15
	movq	(%r15), %rdi
	movq	88(%rdx), %r8
	movq	%r13, %r9
	jmp	pickVictim.3ED
	.text
k.579:
	movq	%rax, %rcx
	movq	%rdi, %rdx
	jmp	gcTest57B
	/* live= GP={%rcx %rdx} spilled=  */
retGC57A:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest57B:
	movq	%r11, %rbx
	movq	448(%rbx), %rbx
	subq	%rsi, %rbx
	jle	doGC57C
check.56D:
	/* block check<E412> (ep<DBBB>,x<DBB0>) */
	movq	$3, (%r11)
	movq	48(%rdx), %r15
	movq	48(%rdx), %rcx
	movl	4(%r15), %ebx
	cmpl	(%rcx), %ebx
	jne	L57D
L_true56E:
	movq	%r11, %r12
then.570:
	/* block then<DE46> (ep<DE44>,vp<DE45>) */
	movq	$1, %r13
	jmp	letJoinK.575
doGC57C:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	movq	%rdx, %rdi
	movabsq	$retGC57A, %r8
	jmp	_ASM_InvokeGC
L57D:
	movq	%r11, %r12
else.56F:
	/* block else<DE4A> (ep<DE48>,vp<DE49>) */
	movq	48(%rdx), %r10
	movl	4(%r10), %r14d
	cmpl	$0, %r14d
	jg	L57E
L_true571:
	movq	48(%rdx), %r13
	movl	8(%r13), %r14d
then.573:
	/* block then<DE77> (ep<DE74>,_t<DE76>,vp<DE75>) */
	decl	%r14d
letJoinK.574:
	/* block letJoinK<DE5B> (ep<DE58>,vp<DE5A>,newL<DE59>) */
	movq	48(%rdx), %r15
	movq	%r14, %rbx
	shll	$3, %ebx
	movslq	%ebx, %rcx
	movq	16(%r15,%rcx,1), %rbx
	movq	48(%rdx), %r10
	movq	%r14, %r15
	shll	$3, %r15d
	movslq	%r15d, %r13
	movq	$1, 16(%r10,%r13,1)
	movq	48(%rdx), %r10
	leaq	4(%r10), %rcx
	movq	48(%rdx), %r13
	movl	%r14d, 4(%r13)
	movq	$12, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rsi, %r13
	addq	$16, %rsi
letJoinK.575:
	/* block letJoinK<DBCD> (ep<DBCA>,vp<DBCC>,thd<DBCB>) */
	cmpq	$1, %r13
	jne	L_true576
else.577:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block else<DBDB> (ep<DBD9>,vp<DBDA>) */
	movq	$636699, -8(%rsi)
	movabsq	$letJoinK.568, %r14
	movq	%r14, (%rsi)
	movq	8(%rdx), %r15
	movq	%r15, 8(%rsi)
	movq	16(%rdx), %rcx
	movq	%rcx, 16(%rsi)
	movq	24(%rdx), %rbx
	movq	%rbx, 24(%rsi)
	movq	32(%rdx), %r10
	movq	%r10, 32(%rsi)
	movq	40(%rdx), %r13
	movq	%r13, 40(%rsi)
	movq	56(%rdx), %r14
	movq	%r14, 48(%rsi)
	movl	64(%rdx), %r15d
	movl	%r15d, 56(%rsi)
	movq	72(%rdx), %rcx
	movq	%rcx, 64(%rsi)
	movq	80(%rdx), %rbx
	movq	%rbx, 72(%rsi)
	movl	88(%rdx), %r10d
	movl	%r10d, 80(%rsi)
	movq	%r12, 88(%rsi)
	movq	96(%rdx), %r13
	movq	%r13, 96(%rsi)
	movq	%rsi, %r10
	addq	$112, %rsi
	movq	24(%rdx), %r14
	movq	(%r14), %rdi
	movq	%r12, %r8
	movq	32(%rdx), %r9
	jmp	mug_D_from_D_atomic.2D3
L57E:
else.572:
	/* block else<DE7D> (ep<DE7A>,_t<DE7C>,vp<DE7B>) */
	decl	%r14d
	jmp	letJoinK.574
L_true576:
then.578:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block then<DBD2> (ep<DBCF>,thd<DBD1>,vp<DBD0>) */
	movq	$20, -8(%rsi)
	movq	(%r13), %rcx
	movq	%rcx, (%rsi)
	movq	$1, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	56(%rdx), %rdi
	movq	%r12, %r8
	movq	%r15, %r9
	jmp	foundWork.4B6
	.text
findRemoteWorkLp.4C4:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest581
	/* live= GP={%rcx %rdx} spilled=  */
retGC580:
	movl	8(%rdi), %ecx
	movq	(%rdi), %rdx
gcTest581:
	movq	%r11, %rbx
	movq	448(%rbx), %rbx
	subq	%rsi, %rbx
	jle	doGC582
check.57F:
	/* Liveout:  GP={%rax %rdi}  */
	/* block check<E415> (ep<DBAB>,nTries<DBA4>) */
	movq	$3, (%r11)
	movq	$751387, -8(%rsi)
	movabsq	$k.579, %r14
	movq	%r14, (%rsi)
	movq	8(%rdx), %r15
	movq	%r15, 8(%rsi)
	movq	16(%rdx), %rbx
	movq	%rbx, 16(%rsi)
	movq	24(%rdx), %r10
	movq	%r10, 24(%rsi)
	movq	32(%rdx), %r12
	movq	%r12, 32(%rsi)
	movq	40(%rdx), %r13
	movq	%r13, 40(%rsi)
	movq	48(%rdx), %r14
	movq	%r14, 48(%rsi)
	movq	56(%rdx), %r15
	movq	%r15, 56(%rsi)
	movl	64(%rdx), %ebx
	movl	%ebx, 64(%rsi)
	movq	72(%rdx), %r10
	movq	%r10, 72(%rsi)
	movq	%rdx, 80(%rsi)
	movl	%ecx, 88(%rsi)
	movq	80(%rdx), %r12
	movq	%r12, 96(%rsi)
	movq	%rsi, %r13
	addq	$112, %rsi
	movq	$20, -8(%rsi)
	movq	$1, (%rsi)
	movq	%r13, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	32(%r11), %r13
	movq	8(%r13), %r14
	movq	%r14, 32(%r11)
	movq	(%r13), %r10
	movq	(%r10), %r15
	movq	%r12, %rax
	movq	%r10, %rdi
	jmp	*%r15
doGC582:
	movq	$133, -8(%rsi)
	movq	%rdx, (%rsi)
	movl	%ecx, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	%rcx, %rdi
	movabsq	$retGC580, %r8
	jmp	_ASM_InvokeGC
	.text
act.584:
	movq	%rax, %r10
	movq	%rdi, %rcx
	jmp	gcTest586
	/* live= GP={%r10 %rcx} spilled=  */
retGC585:
	movq	8(%rdi), %r10
	movq	(%rdi), %rcx
gcTest586:
	movq	%r11, %r12
	movq	448(%r12), %rdx
	subq	%rsi, %rdx
	jle	doGC587
check.583:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block check<E418> (ep<DE9E>,sign<DE9D>) */
	movq	8(%rcx), %rdi
	movq	16(%rcx), %r8
	movq	24(%rcx), %r9
	jmp	schedulerLoop.14
doGC587:
	movq	$20, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%r10, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	%rbx, %rdi
	movabsq	$retGC585, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.589:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest58B
	/* live= GP={%rcx %rdx} spilled=  */
retGC58A:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest58B:
	movq	%r11, %rbx
	movq	448(%rbx), %rbx
	subq	%rsi, %rbx
	jle	doGC58C
check.588:
	/* Liveout:  GP={%rax %rdi}  */
	/* block check<E41B> (ep<DF10>,_wild<DF0F>) */
	movq	16(%rdx), %r14
	leaq	12(%r14), %r13
	movq	16(%rdx), %r15
	leaq	12(%r15), %r12
	movq	16(%rdx), %rcx
	decl	12(%rcx)
	movq	8(%rdx), %r10
	movq	32(%r10), %rbx
	movq	8(%rbx), %r12
	movq	8(%rdx), %r13
	movq	%r12, 32(%r13)
	movq	(%rbx), %r10
	movq	(%r10), %r14
	movq	$1, %r15
	movq	%r15, %rax
	movq	%r10, %rdi
	jmp	*%r14
doGC58C:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	%rcx, %rdi
	movabsq	$retGC58A, %r8
	jmp	_ASM_InvokeGC
	.text
schedulerLoop.14:
	movq	%r9, %r15
	movq	%r8, %r14
	movq	%rdi, %r13
gcTest5AC:
	movq	%r11, %rcx
	movq	448(%rcx), %rdx
	subq	%rsi, %rdx
	jle	doGC5AD
check.58D:
	/* block check<E420> (ep<D997>,self<D988>,deque<D989>,sign<D98A>) */
	movq	72(%r13), %rdx
	movq	(%rdx), %rcx
	cmpq	$1, %rcx
	jne	L5AE
S_case58E:
	movl	224(%r14), %r12d
case.58F:
	/* block case<D9A2> (ep<D99D>,self<D9A1>,deque<D9A0>,sign<D99F>,id<D99E>) */
	cmpq	$1, %r10
	jne	L_true592
	movq	%r12, -72(%rbp)
	movq	%r15, -80(%rbp)
	movq	%r14, -64(%rbp)
else.593:
	/* block else<DB2C> (ep<DB28>,self<DB2B>,deque<DB2A>,id<DB29>) */
	movq	$14095, -8(%rsi)
	movabsq	$foundWork.4B6, %r14
	movq	%r14, (%rsi)
	movq	16(%r13), %r15
	movq	%r15, 8(%rsi)
	movq	%r13, 16(%rsi)
	movq	-80(%rbp), %r14
	movq	%r14, 24(%rsi)
	movq	-72(%rbp), %r15
	movl	%r15d, 32(%rsi)
	movq	88(%r13), %rcx
	movq	%rcx, 40(%rsi)
	movq	96(%r13), %rdx
	movq	%rdx, 48(%rsi)
	movq	%rsi, %rcx
	movq	%rcx, -56(%rbp)
	addq	$64, %rsi
	movq	$10, -8(%rsi)
	movl	$1, (%rsi)
	movq	%rsi, %r10
	addq	$16, %rsi
	movq	%r11, %rbx
	movq	%rsi, 128(%rbx)
	movq	%rax, %rdx
	movq	%rdx, -88(%rbp)
	movq	%rdi, %r12
	movq	%r12, -96(%rbp)
	movq	%r8, %r15
	movq	%r9, %r14
	movq	%r11, %r12
	movq	%r11, %rcx
	movq	%rcx, %rdi
	movq	%r10, %rsi
	call	_PromoteObj
	movq	%rax, %r10
	movq	-88(%rbp), %rax
	movq	-96(%rbp), %rdi
	movq	%r15, %r8
	movq	%r14, %r9
	movq	%r12, %r11
	movq	128(%rbx), %rsi
	movq	$227095, -8(%rsi)
	movabsq	$findRemoteWorkLp.4C4, %rdx
	movq	%rdx, (%rsi)
	movq	16(%r13), %rbx
	movq	%rbx, 8(%rsi)
	movq	24(%r13), %r12
	movq	%r12, 16(%rsi)
	movq	56(%r13), %r14
	movq	%r14, 24(%rsi)
	movq	64(%r13), %r15
	movq	%r15, 32(%rsi)
	movq	80(%r13), %rcx
	movq	%rcx, 40(%rsi)
	movq	-80(%rbp), %r14
	movq	%r14, 48(%rsi)
	movq	-56(%rbp), %r15
	movq	%r15, 56(%rsi)
	movl	$20, 64(%rsi)
	movq	%r10, 72(%rsi)
	movq	88(%r13), %rdx
	movq	%rdx, 80(%rsi)
	movq	%rsi, %r14
	addq	$96, %rsi
	movq	-80(%rbp), %rcx
	movl	4(%rcx), %ebx
	cmpl	(%rcx), %ebx
	je	L_true595
	movq	-72(%rbp), %r12
	movq	-80(%rbp), %r15
	movq	-64(%rbp), %rcx
	movq	%r13, %rdx
else.596:
	/* block else<DED1> (ep<DECC>,self<DED0>,deque<DECF>,id<DECE>,findRemoteWorkLp<DECD>) */
	movl	4(%r15), %ebx
	cmpl	$0, %ebx
	jle	L_true598
	movq	%rbx, %r10
	movq	%r14, %rbx
else.599:
	/* block else<DF08> (ep<DF02>,self<DF07>,deque<DF06>,id<DF05>,findRemoteWorkLp<DF04>,_t<DF03>) */
	movq	%rcx, %r13
	movq	%rdx, %rcx
	decl	%r10d
	jmp	letJoinK.59B
doGC5AD:
	movq	$1673, -8(%rsi)
	movq	%r13, (%rsi)
	movq	%r14, 8(%rsi)
	movq	%r15, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %r13
	addq	$40, %rsi
	movq	%r13, %rdi
	movabsq	$retGC5AB, %r8
	jmp	_ASM_InvokeGC
	/* live= GP={%r10 %r15 %r14 %r13} spilled=  */
retGC5AB:
	movq	24(%rdi), %r10
	movq	16(%rdi), %r15
	movq	8(%rdi), %r14
	movq	(%rdi), %r13
	jmp	gcTest5AC
L_true592:
then.594:
	/* block then<D9A8> (ep<D9A4>,self<D9A7>,deque<D9A6>,sign<D9A5>) */
	cmpq	$1, (%r10)
	je	L_true5A0
else.5A1:
	/* Liveout:  GP={%rax %rdi}  */
	/* block else<DB20> (ep<DB1F>) */
	movq	$133, -8(%rsi)
	movabsq	$str5A3, %r15
	movq	%r15, (%rsi)
	movl	$43, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movabsq	$tagEA, %rdx
	movq	%rdx, (%rsi)
	movq	%r14, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	8(%r13), %rbx
	movq	(%rbx), %r10
	movq	%rcx, %rax
	movq	%rbx, %rdi
	jmp	*%r10
L_true5A0:
then.5A2:
	/* block then<D9B1> (ep<D9AC>,self<D9B0>,deque<D9AF>,sign<D9AE>,sign<D9AD>) */
	movq	8(%r10), %rdx
	movl	(%r15), %r12d
	cmpl	4(%r15), %r12d
	jle	L_true5A4
else.5A5:
	/* block else<DB0F> (ep<DB0A>,self<DB0E>,deque<DB0D>,sign<DB0C>,k<DB0B>) */
	movl	8(%r15), %ecx
	subl	(%r15), %ecx
	movl	4(%r15), %ebx
	leal	(%rcx,%rbx,1), %ecx
	jmp	letJoinK.5A7
L_true5A4:
then.5A6:
	/* block then<DB00> (ep<DAFB>,self<DAFF>,deque<DAFE>,sign<DAFD>,k<DAFC>) */
	movl	4(%r15), %ecx
	subl	(%r15), %ecx
letJoinK.5A7:
	/* block letJoinK<D9C1> (ep<D9BB>,self<D9C0>,deque<D9BF>,sign<D9BE>,k<D9BD>,size<D9BC>) */
	movl	8(%r15), %r12d
	decl	%r12d
	cmpl	%r12d, %ecx
	jge	L_true5A8
else.5A9:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block else<DA87> (ep<DA83>,self<DA86>,deque<DA85>,k<DA84>) */
	movq	$14095, -8(%rsi)
	movabsq	$letJoinK.498, %rbx
	movq	%rbx, (%rsi)
	movq	8(%r13), %r10
	movq	%r10, 8(%rsi)
	movq	40(%r13), %r12
	movq	%r12, 16(%rsi)
	movq	%r13, 24(%rsi)
	movq	%r14, 32(%rsi)
	movq	%r15, 40(%rsi)
	movq	%rdx, 48(%rsi)
	movq	%rsi, %rdx
	addq	$64, %rsi
	movq	32(%r13), %r14
	movq	(%r14), %rdi
	movq	%rdx, %r8
	movq	8(%r13), %r9
	jmp	get_D_ite.EB
L_true5A8:
	movq	%r10, -56(%rbp)
	movq	%r14, -80(%rbp)
	movq	%r13, -72(%rbp)
then.5AA:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block then<D9CD> (ep<D9C9>,self<D9CC>,deque<D9CB>,sign<D9CA>) */
	movq	%rax, %rbx
	movq	%rdi, %r12
	movq	%r8, %r10
	movq	%r10, -64(%rbp)
	movq	%r9, %r13
	movq	%r13, -88(%rbp)
	movq	%rsi, %r13
	movq	%r11, %r14
	movq	-80(%rbp), %rdi
	movq	-72(%rbp), %rcx
	movq	64(%rcx), %rcx
	movq	%rcx, %rsi
	movl	8(%r15), %ecx
	shll	$1, %ecx
	movslq	%ecx, %rdx
	call	_M_DequeAlloc
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%r12, %rdi
	movq	-64(%rbp), %r8
	movq	-88(%rbp), %r9
	movq	%r13, %rsi
	movq	%r14, %r11
	movq	$20, -8(%rsi)
	movq	%r15, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	$133, -8(%rsi)
	movq	%rbx, (%rsi)
	movabsq	$copy.474, %rdx
	movq	%rdx, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	$7437, -8(%rsi)
	movabsq	$letJoinK.13, %r12
	movq	%r12, (%rsi)
	movq	-72(%rbp), %rdx
	movq	%rdx, 8(%rsi)
	movq	-80(%rbp), %rbx
	movq	%rbx, 16(%rsi)
	movq	%r15, 24(%rsi)
	movq	-56(%rbp), %r12
	movq	%r12, 32(%rsi)
	movq	%rcx, 40(%rsi)
	movq	%rsi, %rbx
	addq	$56, %rsi
	movq	(%r10), %rdi
	movq	%rbx, %r8
	jmp	copy.474
L_true595:
	movq	%r13, -56(%rbp)
then.597:
	/* block then<DECA> (ep<DEC5>,self<DEC9>,deque<DEC8>,id<DEC7>,findRemoteWorkLp<DEC6>) */
	movq	%r14, %rbx
	movq	-80(%rbp), %rdx
	movq	$1, %r10
letJoinK.59C:
	/* block letJoinK<DE94> (ep<DE8E>,self<DE93>,deque<DE92>,id<DE91>,findRemoteWorkLp<DE90>,thd<DE8F>) */
	cmpq	$1, %r10
	je	L5AF
L_true59D:
then.59F:
	/* Liveout:  GP={%rax %rdi}  */
	/* block then<DE9A> (ep<DE96>,self<DE99>,deque<DE98>,thd<DE97>) */
	movq	(%r10), %r10
	movq	%r10, -80(%rbp)
	movq	$1289, -8(%rsi)
	movabsq	$act.584, %rbx
	movq	%rbx, (%rsi)
	movq	-56(%rbp), %r12
	movq	%r12, 8(%rsi)
	movq	-64(%rbp), %r13
	movq	%r13, 16(%rsi)
	movq	%rdx, 24(%rsi)
	movq	%rsi, %r14
	movq	%r14, -72(%rbp)
	addq	$40, %rsi
	movq	%r11, %rbx
	movq	%rsi, 128(%rbx)
	movq	%rax, %r12
	movq	%rdi, %r13
	movq	%r8, %r15
	movq	%r15, -88(%rbp)
	movq	%r9, %r14
	movq	%r11, %r15
	movq	%r11, %r10
	movq	%r10, %rdi
	movq	%rdx, %rsi
	call	_PromoteObj
	movq	%rax, %rcx
	movq	%r12, %rax
	movq	%r13, %rdi
	movq	-88(%rbp), %r8
	movq	%r14, %r9
	movq	%r15, %r11
	movq	128(%rbx), %rsi
	movq	-56(%rbp), %rdx
	movq	96(%rdx), %r13
	movq	(%r13), %r12
	movq	-64(%rbp), %rbx
	movl	224(%rbx), %edx
	shlq	$3, %rdx
	movq	%rcx, (%r12,%rdx,1)
	movq	24(%r11), %rbx
	movq	$12, -8(%rsi)
	movq	-80(%rbp), %r10
	movq	8(%r10), %r12
	movq	%r12, (%rsi)
	movq	%rsi, %r10
	addq	$16, %rsi
	movq	$1289, -8(%rsi)
	movl	(%rbx), %r14d
	movl	%r14d, (%rsi)
	movq	%r10, 8(%rsi)
	movl	16(%rbx), %r15d
	movl	%r15d, 16(%rsi)
	movq	24(%rbx), %rcx
	movq	%rcx, 24(%rsi)
	movq	%rsi, %r13
	addq	$40, %rsi
	movq	%r13, 24(%r11)
	movq	-80(%rbp), %r12
	movq	(%r12), %rdx
	movq	$20, -8(%rsi)
	movq	-72(%rbp), %r13
	movq	%r13, (%rsi)
	movq	-64(%rbp), %r14
	movq	32(%r14), %r10
	movq	%r10, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	-64(%rbp), %r15
	movq	%rbx, 32(%r15)
	movq	$1, (%r15)
	movq	(%rdx), %r12
	movq	$1, %r13
	movq	%r13, %rax
	movq	%rdx, %rdi
	jmp	*%r12
L5AF:
	movq	%rbx, -64(%rbp)
else.59E:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block else<DEBD> (ep<DEBA>,id<DEBC>,findRemoteWorkLp<DEBB>) */
	movq	%r11, %r15
	movq	%rsi, 128(%r15)
	movq	%rax, %rdx
	movq	%rdx, -96(%rbp)
	movq	%rdi, %rbx
	movq	%r8, %r12
	movq	%r9, %r13
	movq	%r11, %r14
	movq	%r11, %rcx
	movq	%rcx, %rdi
	movq	$3, %rdx
	movq	%rdx, %rsi
	call	_PromoteObj
	movq	%rax, %rcx
	movq	-96(%rbp), %rax
	movq	%rbx, %rdi
	movq	%r12, %r8
	movq	%r13, %r9
	movq	%r14, %r11
	movq	128(%r15), %rsi
	movq	-56(%rbp), %rbx
	movq	88(%rbx), %r10
	movq	(%r10), %rbx
	movq	-72(%rbp), %rdx
	shlq	$3, %rdx
	movq	%rcx, (%rbx,%rdx,1)
	movq	-64(%rbp), %rdi
	xorl	%r8d, %r8d
	jmp	findRemoteWorkLp.4C4
L5AE:
	cmpq	$3, %rcx
	jne	S_case58E
S_case590:
case.591:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block case<DF0E> (ep<DF0B>,self<DF0D>,deque<DF0C>) */
	movq	$519, -8(%rsi)
	movabsq	$letJoinK.589, %rbx
	movq	%rbx, (%rsi)
	movq	%r14, 8(%rsi)
	movq	%r15, 16(%rsi)
	movq	%rsi, %r10
	addq	$32, %rsi
	movq	48(%r13), %r12
	movq	(%r12), %rdi
	movq	%r14, %r8
	movq	%r15, %r9
	jmp	to_D_list_D_from_D_atomic.299
L_true598:
	movq	%r12, %r13
	movq	%r14, %rbx
	movl	8(%r15), %r14d
then.59A:
	/* block then<DEFF> (ep<DEF9>,self<DEFE>,deque<DEFD>,id<DEFC>,findRemoteWorkLp<DEFB>,_t<DEFA>) */
	movq	%r13, %r12
	movq	%rcx, %r13
	movq	%rdx, %rcx
	movq	%r14, %r10
	decl	%r10d
letJoinK.59B:
	/* block letJoinK<DEE3> (ep<DEDD>,self<DEE2>,deque<DEE1>,id<DEE0>,findRemoteWorkLp<DEDF>,newL<DEDE>) */
	movq	%r10, %rdx
	shll	$3, %edx
	movslq	%edx, %r14
	movq	16(%r15,%r14,1), %rdx
	movq	%r10, %r14
	shll	$3, %r14d
	movslq	%r14d, %r14
	movq	$1, 16(%r15,%r14,1)
	movl	%r10d, 4(%r15)
	movq	$12, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rsi, %r10
	addq	$16, %rsi
	movq	%r12, -72(%rbp)
	movq	%r15, %rdx
	movq	%r13, -64(%rbp)
	movq	%rcx, -56(%rbp)
	jmp	letJoinK.59C
	.text
initWorker_P_.5B1:
	movq	%rax, %r15
	movq	%rdi, %r12
	jmp	gcTest5B3
	/* live= GP={%r15 %r12} spilled=  */
retGC5B2:
	movq	8(%rdi), %r15
	movq	(%rdi), %r12
gcTest5B3:
	movq	%r11, %r13
	movq	448(%r13), %r14
	subq	%rsi, %r14
	jle	doGC5B4
check.5B0:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block check<E423> (ep<D97A>,_wild<D96D>) */
	movq	$3, (%r11)
	movq	%rax, %r15
	movq	%r15, -72(%rbp)
	movq	%rdi, %rbx
	movq	%r8, %r13
	movq	%r9, %r14
	movq	%rsi, %r15
	movq	%r11, %rcx
	movq	%rcx, -64(%rbp)
	movq	%r11, %rcx
	movq	%rcx, %rdi
	movq	64(%r12), %rdx
	movq	%rdx, %rsi
	movl	$128, %r10d
	movslq	%r10d, %rdx
	call	_M_DequeAlloc
	movq	%rax, %rdx
	movq	%rdx, -56(%rbp)
	movq	-72(%rbp), %rax
	movq	%rbx, %rdi
	movq	%r13, %r8
	movq	%r14, %r9
	movq	%r15, %rsi
	movq	-64(%rbp), %r11
	movq	%r11, %r13
	movq	%rsi, 128(%r13)
	movq	%rax, %rbx
	movq	%rbx, -64(%rbp)
	movq	%rdi, %r10
	movq	%r10, -72(%rbp)
	movq	%r8, %r14
	movq	%r9, %r15
	movq	%r11, %rbx
	movq	%r11, %r10
	movq	%r10, %rdi
	movq	-56(%rbp), %rsi
	call	_PromoteObj
	movq	%rax, %rcx
	movq	-64(%rbp), %rax
	movq	-72(%rbp), %rdi
	movq	%r14, %r8
	movq	%r15, %r9
	movq	%rbx, %r11
	movq	128(%r13), %rsi
	movq	96(%r12), %rdx
	movq	(%rdx), %r14
	movl	224(%r11), %ebx
	shlq	$3, %rbx
	movq	%rcx, (%r14,%rbx,1)
	movq	$1015579, -8(%rsi)
	movabsq	$schedulerLoop.14, %r13
	movq	%r13, (%rsi)
	movq	8(%r12), %r14
	movq	%r14, 8(%rsi)
	movq	16(%r12), %r15
	movq	%r15, 16(%rsi)
	movq	24(%r12), %rcx
	movq	%rcx, 24(%rsi)
	movq	32(%r12), %rdx
	movq	%rdx, 32(%rsi)
	movq	40(%r12), %rbx
	movq	%rbx, 40(%rsi)
	movq	48(%r12), %r10
	movq	%r10, 48(%rsi)
	movq	56(%r12), %r13
	movq	%r13, 56(%rsi)
	movq	64(%r12), %r14
	movq	%r14, 64(%rsi)
	movq	72(%r12), %r15
	movq	%r15, 72(%rsi)
	movq	80(%r12), %rcx
	movq	%rcx, 80(%rsi)
	movq	88(%r12), %rdx
	movq	%rdx, 88(%rsi)
	movq	96(%r12), %rbx
	movq	%rbx, 96(%rsi)
	movq	%rsi, %r10
	addq	$112, %rsi
	movq	%r10, %rdi
	movq	%r11, %r8
	movq	-56(%rbp), %r9
	movq	$1, %r10
	jmp	schedulerLoop.14
doGC5B4:
	movq	$20, -8(%rsi)
	movq	%r12, (%rsi)
	movq	%r15, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	%r10, %rdi
	movabsq	$retGC5B2, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.7E:
	movq	%rdi, %rcx
	jmp	gcTest5B7
	/* live= GP={%rcx} spilled=  */
retGC5B6:
	movq	(%rdi), %rcx
gcTest5B7:
	movq	%r11, %r13
	movq	448(%r13), %rdx
	subq	%rsi, %rdx
	jle	doGC5B8
check.5B5:
	/* Liveout:  GP={%rdi}  */
	/* block check<E425> (ep<DF35>) */
	movq	8(%rcx), %rbx
	movq	(%rbx), %r10
	movq	%rbx, %rdi
	jmp	*%r10
doGC5B8:
	movq	$12, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%rsi, %r12
	addq	$16, %rsi
	movq	%r12, %rdi
	movabsq	$retGC5B6, %r8
	jmp	_ASM_InvokeGC
	.text
k.7D:
	movq	%rax, %rcx
	movq	%rdi, %rdx
	jmp	gcTest5BB
	/* live= GP={%rcx %rdx} spilled=  */
retGC5BA:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest5BB:
	movq	%r11, %r12
	movq	448(%r12), %rbx
	subq	%rsi, %rbx
	jle	doGC5BC
check.5B9:
	/* Liveout:  GP={%rdi}  */
	/* block check<E428> (ep<DF46>,x<DF45>) */
	movq	8(%rdx), %rdi
	jmp	letJoinK.7E
doGC5BC:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	%r10, %rdi
	movabsq	$retGC5BA, %r8
	jmp	_ASM_InvokeGC
	.text
retGC5C0:
	movq	8(%rdi), %rdx
	movq	(%rdi), %r12
	jmp	gcTest5C1
L_true5C3:
	movq	-56(%rbp), %rdx
then.5BF:
	/* block then<DF76> (ep<E429>,retK<E42A>) */
gcTest5C1:
	movq	%r11, %r13
	movq	448(%r13), %r14
	subq	%rsi, %r14
	jle	doGC5C2
check.5BD:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<E42B> (ep<DF74>,retK<DF75>) */
	pause
	movq	%r12, %rdi
	movq	%rdx, %r8
lp.5BE:
	movq	%r8, %rbx
	movq	%rbx, -56(%rbp)
	movq	%rdi, %r12
	movq	(%r12), %r15
	movq	384(%r15), %r10
	movq	%r10, -64(%rbp)
	movq	$28, -8(%rsi)
	movq	8(%r12), %rcx
	movq	%rcx, (%rsi)
	movq	16(%r12), %rdx
	movq	%rdx, 8(%rsi)
	movq	-64(%rbp), %r13
	movq	%r13, 16(%rsi)
	movq	%rsi, %rcx
	addq	$32, %rsi
	movq	%r11, %r15
	movq	%rsi, 128(%r15)
	movq	%rax, %r14
	movq	%r14, -72(%rbp)
	movq	%rdi, %rdx
	movq	%rdx, -80(%rbp)
	movq	%r8, %r14
	movq	%r9, %r13
	movq	%r11, %rbx
	movq	%r11, %r10
	movq	%r10, %rdi
	movq	%rcx, %rsi
	call	_PromoteObj
	movq	%rax, %rcx
	movq	-72(%rbp), %rax
	movq	-80(%rbp), %rdi
	movq	%r14, %r8
	movq	%r13, %r9
	movq	%rbx, %r11
	movq	128(%r15), %rsi
	movq	(%r12), %rbx
	leaq	384(%rbx), %rdx
	movq	-64(%rbp), %rax
	movq	(%r12), %r10
	lock
	cmpxchgq	%rcx, 384(%r10)
	movq	%rax, %r13
	cmpq	-64(%rbp), %r13
	jne	L_true5C3
	movq	-56(%rbp), %rbx
	movq	%r12, %rcx
else.5C4:
	/* block else<DF7A> (ep<DF78>,retK<DF79>) */
	movq	(%rcx), %r13
	movq	16(%r13), %r12
	cmpq	$1, %r12
	je	letJoinK.5C6
L5C9:
	cmpq	$3, %r12
	je	S_case5C7
S_case5C5:
	jmp	letJoinK.5C6
S_case5C7:
	movq	%rbx, -56(%rbp)
case.5C8:
	/* block case<DF86> (ep<DF84>,retK<DF85>) */
	movq	%rax, %r12
	movq	%rdi, %rbx
	movq	%r8, %r15
	movq	%r9, %r14
	movq	%rsi, %r13
	movq	%r11, %r10
	movq	%r10, -64(%rbp)
	movq	(%rcx), %rcx
	movq	%rcx, %rdi
	call	_VProcWake
	movq	%r12, %rax
	movq	%rbx, %rdi
	movq	%r15, %r8
	movq	%r14, %r9
	movq	%r13, %rsi
	movq	-64(%rbp), %r11
	movq	-56(%rbp), %rbx
letJoinK.5C6:
	/* Liveout:  GP={%rdi}  */
	/* block letJoinK<DF7F> (retK<DF7E>) */
	movq	%rbx, %rdi
	jmp	letJoinK.19
doGC5C2:
	movq	$20, -8(%rsi)
	movq	%r12, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	%r10, %rdi
	movabsq	$retGC5C0, %r8
	jmp	_ASM_InvokeGC
	.text
preempt.1B:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest5CF
	/* live= GP={%rcx %rdx} spilled=  */
retGC5CE:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest5CF:
	movq	%r11, %r14
	movq	448(%r14), %rbx
	subq	%rsi, %rbx
	jle	doGC5D0
check.5CA:
	/* block check<E42E> (ep<DF96>,retK<DF97>) */
	movq	(%rdx), %r12
	movq	448(%r12), %r10
	movq	(%rdx), %r15
	leaq	448(%r15), %r14
	movq	%r10, %rax
	xorq	%rbx, %rbx
	movq	(%rdx), %r12
	lock
	cmpxchgq	%rbx, 448(%r12)
	movq	%rax, %r13
	cmpq	%r10, %r13
	je	L5D1
L_true5CB:
then.5CD:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block then<DFA4> (ep<DFA2>,retK<DFA3>) */
	pause
	movq	%rdx, %rdi
	movq	%rcx, %r8
	jmp	preempt.1B
doGC5D0:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	%r13, %rdi
	movabsq	$retGC5CE, %r8
	jmp	_ASM_InvokeGC
L5D1:
else.5CC:
	/* Liveout:  GP={%rdi}  */
	/* block else<DFA8> (retK<DFA7>) */
	movq	%rcx, %rdi
	jmp	letJoinK.1A
	.text
letJoinK.5D6:
	movq	%rdi, %rcx
	jmp	gcTest5D8
	/* live= GP={%rcx} spilled=  */
retGC5D7:
	movq	(%rdi), %rcx
gcTest5D8:
	movq	%r11, %rdx
	movq	448(%rdx), %rdx
	subq	%rsi, %rdx
	jle	doGC5D9
check.5D2:
	/* block check<E430> (ep<D961>) */
	movq	$1289, -8(%rsi)
	movq	96(%rcx), %r14
	movl	224(%r14), %r15d
	movl	%r15d, (%rsi)
	movq	64(%rcx), %rdx
	movq	8(%rdx), %rbx
	movq	%rbx, 8(%rsi)
	movq	64(%rcx), %r10
	movl	16(%r10), %r12d
	movl	%r12d, 16(%rsi)
	movq	64(%rcx), %r13
	movq	24(%r13), %r14
	movq	%r14, 24(%rsi)
	movq	%rsi, %r12
	addq	$40, %rsi
	movq	$1015579, -8(%rsi)
	movabsq	$initWorker_P_.5B1, %r15
	movq	%r15, (%rsi)
	movq	8(%rcx), %rdx
	movq	%rdx, 8(%rsi)
	movq	16(%rcx), %rbx
	movq	%rbx, 16(%rsi)
	movq	24(%rcx), %r10
	movq	%r10, 24(%rsi)
	movq	32(%rcx), %r13
	movq	%r13, 32(%rsi)
	movq	40(%rcx), %r14
	movq	%r14, 40(%rsi)
	movq	48(%rcx), %r15
	movq	%r15, 48(%rsi)
	movq	56(%rcx), %rdx
	movq	%rdx, 56(%rsi)
	movq	72(%rcx), %rbx
	movq	%rbx, 64(%rsi)
	movq	80(%rcx), %r10
	movq	%r10, 72(%rsi)
	movq	88(%rcx), %r13
	movq	%r13, 80(%rsi)
	movq	112(%rcx), %r14
	movq	%r14, 88(%rsi)
	movq	120(%rcx), %r15
	movq	%r15, 96(%rsi)
	movq	%rsi, %r10
	addq	$112, %rsi
	movq	$3, (%r11)
	movq	$263, -8(%rsi)
	movabsq	$letJoinK.1C, %rdx
	movq	%rdx, (%rsi)
	movq	104(%rcx), %rbx
	movq	%rbx, 8(%rsi)
	movq	%r11, 16(%rsi)
	movq	%rsi, %rbx
	addq	$32, %rsi
	cmpq	96(%rcx), %r11
	jne	L5DA
L_true5D3:
	movq	%r11, %r13
then.5D5:
	/* Liveout:  GP={%rdi}  */
	/* block then<DF59> (fls<DF58>,initWorker'<DF57>,vp<DF56>,letJoinK<DF55>) */
	movq	$28, -8(%rsi)
	movq	%r12, (%rsi)
	movq	%r10, 8(%rsi)
	movq	88(%r13), %r14
	movq	%r14, 16(%rsi)
	movq	%rsi, %r12
	addq	$32, %rsi
	movq	%r12, 88(%r13)
	movq	%rbx, %rdi
	jmp	letJoinK.1C
doGC5D9:
	movq	$12, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%rsi, %r15
	addq	$16, %rsi
	movq	%r15, %rdi
	movabsq	$retGC5D7, %r8
	jmp	_ASM_InvokeGC
L5DA:
else.5D4:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block else<DF61> (ep<DF5D>,fls<DF60>,initWorker'<DF5F>,letJoinK<DF5E>) */
	movq	$775, -8(%rsi)
	movq	96(%rcx), %r13
	movq	%r13, (%rsi)
	movq	%r12, 8(%rsi)
	movq	%r10, 16(%rsi)
	movq	%rsi, %r10
	addq	$32, %rsi
	movq	$133, -8(%rsi)
	movq	%r10, (%rsi)
	movabsq	$lp.5BE, %r15
	movq	%r15, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	$519, -8(%rsi)
	movabsq	$letJoinK.19, %rdx
	movq	%rdx, (%rsi)
	movq	96(%rcx), %r10
	movq	%r10, 8(%rsi)
	movq	%rbx, 16(%rsi)
	movq	%rsi, %rcx
	addq	$32, %rsi
	movq	(%r14), %rdi
	movq	%rcx, %r8
	jmp	lp.5BE
	.text
spawnWorker.5DC:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest5DE
	/* live= GP={%r10 %rcx %rdx %rbx} spilled=  */
retGC5DD:
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest5DE:
	movq	%r11, %r14
	movq	448(%r14), %r12
	subq	%rsi, %r12
	jle	doGC5DF
check.5DB:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block check<E435> (ep<D94D>,dst<D94E>,retK<D94F>,exh<D950>) */
	movq	$7798561, -8(%rsi)
	movabsq	$letJoinK.5D6, %r14
	movq	%r14, (%rsi)
	movq	(%rbx), %r15
	movq	%r15, 8(%rsi)
	movq	8(%rbx), %r10
	movq	%r10, 16(%rsi)
	movq	16(%rbx), %r12
	movq	%r12, 24(%rsi)
	movq	24(%rbx), %r13
	movq	%r13, 32(%rsi)
	movq	48(%rbx), %r14
	movq	%r14, 40(%rsi)
	movq	56(%rbx), %r15
	movq	%r15, 48(%rsi)
	movq	64(%rbx), %r10
	movq	%r10, 56(%rsi)
	movq	72(%rbx), %r12
	movq	%r12, 64(%rsi)
	movq	80(%rbx), %r13
	movq	%r13, 72(%rsi)
	movq	88(%rbx), %r14
	movq	%r14, 80(%rsi)
	movq	96(%rbx), %r15
	movq	%r15, 88(%rsi)
	movq	%rdx, 96(%rsi)
	movq	%rcx, 104(%rsi)
	movq	104(%rbx), %rcx
	movq	%rcx, 112(%rsi)
	movq	112(%rbx), %rdx
	movq	%rdx, 120(%rsi)
	movq	%rsi, %r13
	addq	$136, %rsi
	movq	40(%rbx), %r10
	movq	(%r10), %rdi
	movq	32(%rbx), %r12
	movq	(%r12), %r8
	movq	%r13, %r9
	jmp	new.FB
doGC5DF:
	movq	$1673, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %r13
	addq	$40, %rsi
	movq	%r13, %rdi
	movabsq	$retGC5DD, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.5E2:
	movq	%rdi, %rcx
	jmp	gcTest5E4
	/* live= GP={%rcx} spilled=  */
retGC5E3:
	movq	(%rdi), %rcx
gcTest5E4:
	movq	%r11, %r12
	movq	448(%r12), %rdx
	subq	%rsi, %rdx
	jle	doGC5E5
check.5E0:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block check<E437> (ep<DFC7>) */
	movq	8(%rcx), %rbx
	movq	(%rbx), %rdi
	movq	24(%rcx), %r8
	movq	16(%rcx), %r9
	jmp	lp.5E1
doGC5E5:
	movq	$12, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%rsi, %r10
	addq	$16, %rsi
	movq	%r10, %rdi
	movabsq	$retGC5E3, %r8
	jmp	_ASM_InvokeGC
	.text
lp.5E1:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest5EC
	/* live= GP={%rcx %rdx %rbx} spilled=  */
retGC5EB:
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest5EC:
	movq	%r11, %r14
	movq	448(%r14), %r10
	subq	%rsi, %r10
	jle	doGC5ED
check.5E6:
	/* block check<E43B> (ep<DFB6>,vps<DFB7>,retK<DFB8>) */
	cmpq	$1, %rdx
	je	L5EE
L_true5E7:
then.5E9:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block then<DFC0> (ep<DFBD>,vps<DFBF>,retK<DFBE>) */
	movq	$133, -8(%rsi)
	movq	%rbx, (%rsi)
	movabsq	$lp.5E1, %r13
	movq	%r13, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	$1801, -8(%rsi)
	movabsq	$letJoinK.5E2, %r15
	movq	%r15, (%rsi)
	movq	%r12, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	8(%rdx), %rcx
	movq	%rcx, 24(%rsi)
	movq	%rsi, %r14
	addq	$40, %rsi
	movq	8(%rbx), %r10
	movq	(%r10), %rdi
	movq	(%rdx), %r12
	movq	(%r12), %r8
	movq	%r14, %r9
	movq	(%rbx), %r10
	jmp	spawnWorker.5DC
doGC5ED:
	movq	$28, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%rsi, %r13
	addq	$32, %rsi
	movq	%r13, %rdi
	movabsq	$retGC5EB, %r8
	jmp	_ASM_InvokeGC
L5EE:
else.5E8:
	/* Liveout:  GP={%rdi}  */
	/* block else<DFD5> (retK<DFD4>) */
	movq	%rcx, %rdi
	jmp	letJoinK.5EA
	.text
letJoinK.604:
	movq	%rdi, %rdx
	jmp	gcTest606
	/* live= spilled= GP={%r~1}  */
retGC605:
	movq	(%rdi), %rdx
gcTest606:
	movq	%r11, %rcx
	movq	448(%rcx), %rbx
	subq	%rsi, %rbx
	jle	doGC607
	movq	%rdx, -88(%rbp)
check.5EF:
	/* block check<E43D> (ep<E034>) */
	movq	%rax, %rbx
	movq	%rdi, %r12
	movq	%r8, %r13
	movq	%r9, %r14
	movq	%rsi, %r10
	movq	%r10, -56(%rbp)
	movq	%r11, %r15
	call	_M_GetTime
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%r12, %rdi
	movq	%r13, %r8
	movq	%r14, %r9
	movq	-56(%rbp), %rsi
	movq	%r15, %r11
	movq	-88(%rbp), %r12
	subq	16(%r12), %rcx
	movq	$10, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%rsi, %r13
	movq	%r13, -72(%rbp)
	addq	$16, %rsi
	cmpq	$0, %rcx
	jge	L608
L_true5F0:
then.5F2:
	/* block then<E08B> (ep<E089>,_t<E08A>) */
	movq	$133, -8(%rsi)
	movabsq	$str5F8, %r15
	movq	%r15, (%rsi)
	movl	$1, 8(%rsi)
	movq	%rsi, %rdx
	movq	%rdx, -64(%rbp)
	addq	$24, %rsi
	movq	$10, -8(%rsi)
	negq	%rcx
	movq	%rcx, (%rsi)
	movq	%rsi, %rbx
	movq	%rbx, -72(%rbp)
	addq	$16, %rsi
	jmp	letJoinK.5F4
L608:
else.5F1:
	/* block else<E093> (ep<E091>,res<E092>) */
	movq	$133, -8(%rsi)
	movabsq	$str5F3, %r10
	movq	%r10, (%rsi)
	movl	$0, 8(%rsi)
	movq	%rsi, %r14
	movq	%r14, -64(%rbp)
	addq	$24, %rsi
letJoinK.5F4:
	/* block letJoinK<E040> (ep<E03D>,sign<E03E>,t<E03F>) */
	movq	-72(%rbp), %r15
	movq	(%r15), %rax
	cdq
	movq	$1000, %r13
	idivq	%r13
	movq	%rax, %r12
	movq	%r12, %rax
	cdq
	movq	$1000, %r14
	idivq	%r14
	cmpq	$10, %rdx
	jl	L_true5F5
else.5F6:
	/* block else<E06F> (ep<E06B>,sign<E06E>,t<E06D>,_t<E06C>) */
	cmpq	$100, %rdx
	jl	L_true5F9
else.5FA:
	/* block else<E081> (ep<E07D>,sign<E080>,t<E07F>,_t<E07E>) */
	movq	$133, -8(%rsi)
	movabsq	$str5FC, %rcx
	movq	%rcx, (%rsi)
	movl	$1, 8(%rsi)
	movq	%rsi, %r10
	movq	%r10, -56(%rbp)
	addq	$24, %rsi
	movq	%r11, %rbx
	movq	%rsi, 128(%rbx)
	movq	%rax, %r12
	movq	%r12, -80(%rbp)
	movq	%rdi, %r12
	movq	%r8, %r13
	movq	%r9, %r14
	movq	%r11, %r15
	movq	%rdx, %rdi
	call	_M_LongToString
	movq	%rax, %rcx
	movq	-80(%rbp), %rax
	movq	%r12, %rdi
	movq	%r13, %r8
	movq	%r14, %r9
	movq	%r15, %r11
	movq	128(%rbx), %rsi
	movq	%r11, %r15
	movq	%rsi, 128(%r15)
	movq	%rax, %r13
	movq	%r13, -80(%rbp)
	movq	%rdi, %r14
	movq	%r8, %r13
	movq	%r9, %r12
	movq	%r11, %rbx
	movq	-56(%rbp), %rdi
	movq	%rcx, %r10
	movq	%r10, %rsi
	call	_M_StringConcat2
	movq	%rax, %rcx
	movq	%rcx, -56(%rbp)
	movq	-80(%rbp), %rax
	movq	%r14, %rdi
	movq	%r13, %r8
	movq	%r12, %r9
	movq	%rbx, %r11
	movq	128(%r15), %rsi
letJoinK.5FF:
	/* Liveout:  GP={%rax %rdi}  */
	/* block letJoinK<E04B> (ep<E047>,sign<E04A>,t<E049>,frac<E048>) */
	movq	%r11, %rbx
	movq	%rsi, 128(%rbx)
	movq	%rax, %rdx
	movq	%rdx, -80(%rbp)
	movq	%rdi, %r12
	movq	%r8, %r13
	movq	%r9, %r14
	movq	%r11, %r15
	movq	-72(%rbp), %r10
	movq	(%r10), %rax
	cdq
	movq	$1000000, %r10
	idivq	%r10
	movq	%rax, %rdx
	movq	%rdx, %rdi
	call	_M_LongToString
	movq	%rax, %rcx
	movq	-80(%rbp), %rax
	movq	%r12, %rdi
	movq	%r13, %r8
	movq	%r14, %r9
	movq	%r15, %r11
	movq	128(%rbx), %rsi
	movq	%r11, %rbx
	movq	%rsi, 128(%rbx)
	movq	%rax, %r12
	movq	%rdi, %r13
	movq	%r8, %r14
	movq	%r9, %r15
	movq	%r15, -72(%rbp)
	movq	%r11, %r15
	movq	-64(%rbp), %rdi
	movq	%rcx, %rsi
	call	_M_StringConcat2
	movq	%rax, %rcx
	movq	%r12, %rax
	movq	%r13, %rdi
	movq	%r14, %r8
	movq	-72(%rbp), %r9
	movq	%r15, %r11
	movq	128(%rbx), %rsi
	movq	%r11, %r15
	movq	%rsi, 128(%r15)
	movq	%rax, %rdx
	movq	%rdx, -80(%rbp)
	movq	%rdi, %r14
	movq	%r8, %r13
	movq	%r9, %r12
	movq	%r11, %rbx
	movq	%rcx, %rdx
	movq	%rdx, %rdi
	movq	-56(%rbp), %rsi
	call	_M_StringConcat2
	movq	%rax, %rdx
	movq	-80(%rbp), %rax
	movq	%r14, %rdi
	movq	%r13, %r8
	movq	%r12, %r9
	movq	%rbx, %r11
	movq	128(%r15), %rsi
	movq	$133, -8(%rsi)
	movabsq	$str600, %r12
	movq	%r12, (%rsi)
	movl	$1, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	%r11, %rbx
	movq	%rsi, 128(%rbx)
	movq	%rax, %r12
	movq	%rdi, %r13
	movq	%r8, %r10
	movq	%r10, -56(%rbp)
	movq	%r9, %r14
	movq	%r11, %r15
	movq	%rdx, %r10
	movq	%r10, %rdi
	movq	%rcx, %rsi
	call	_M_StringConcat2
	movq	%rax, %rcx
	movq	%r12, %rax
	movq	%r13, %rdi
	movq	-56(%rbp), %r8
	movq	%r14, %r9
	movq	%r15, %r11
	movq	128(%rbx), %rsi
	movq	%rax, %r15
	movq	%rdi, %r14
	movq	%r8, %r13
	movq	%r9, %r12
	movq	%rsi, %rbx
	movq	%r11, %rdx
	movq	%rdx, -56(%rbp)
	movq	(%rcx), %rcx
	movq	%rcx, %rdi
	call	_M_Print
	movq	%r15, %rax
	movq	%r14, %rdi
	movq	%r13, %r8
	movq	%r12, %r9
	movq	%rbx, %rsi
	movq	-56(%rbp), %r11
	movq	-88(%rbp), %rbx
	movq	8(%rbx), %rbx
	movq	(%rbx), %r10
	movq	$1, %r12
	movq	%r12, %rax
	movq	%rbx, %rdi
	jmp	*%r10
L_true5F5:
then.5F7:
	/* block then<E063> (ep<E05F>,sign<E062>,t<E061>,_t<E060>) */
	movq	$133, -8(%rsi)
	movabsq	$str603, %r14
	movq	%r14, (%rsi)
	movl	$3, 8(%rsi)
	movq	%rsi, %r12
	movq	%r12, -56(%rbp)
	addq	$24, %rsi
	movq	%r11, %rbx
	movq	%rsi, 128(%rbx)
	movq	%rax, %r13
	movq	%r13, -80(%rbp)
	movq	%rdi, %r12
	movq	%r8, %r13
	movq	%r9, %r14
	movq	%r11, %r15
	movq	%rdx, %rdi
	call	_M_LongToString
	movq	%rax, %rcx
	movq	-80(%rbp), %rax
	movq	%r12, %rdi
	movq	%r13, %r8
	movq	%r14, %r9
	movq	%r15, %r11
	movq	128(%rbx), %rsi
	movq	%r11, %r15
	movq	%rsi, 128(%r15)
	movq	%rax, %r14
	movq	%r14, -80(%rbp)
	movq	%rdi, %r14
	movq	%r8, %r13
	movq	%r9, %r12
	movq	%r11, %rbx
	movq	-56(%rbp), %rdi
	movq	%rcx, %rdx
	movq	%rdx, %rsi
	call	_M_StringConcat2
	movq	%rax, %rcx
	movq	%rcx, -56(%rbp)
	movq	-80(%rbp), %rax
	movq	%r14, %rdi
	movq	%r13, %r8
	movq	%r12, %r9
	movq	%rbx, %r11
	movq	128(%r15), %rsi
	jmp	letJoinK.5FF
L_true5F9:
then.5FB:
	/* block then<E075> (ep<E071>,sign<E074>,t<E073>,_t<E072>) */
	movq	$133, -8(%rsi)
	movabsq	$str602, %r13
	movq	%r13, (%rsi)
	movl	$2, 8(%rsi)
	movq	%rsi, %r10
	movq	%r10, -56(%rbp)
	addq	$24, %rsi
	movq	%r11, %rbx
	movq	%rsi, 128(%rbx)
	movq	%rax, %r12
	movq	%rdi, %r13
	movq	%r13, -80(%rbp)
	movq	%r8, %r13
	movq	%r9, %r14
	movq	%r11, %r15
	movq	%rdx, %rdi
	call	_M_LongToString
	movq	%rax, %rcx
	movq	%r12, %rax
	movq	-80(%rbp), %rdi
	movq	%r13, %r8
	movq	%r14, %r9
	movq	%r15, %r11
	movq	128(%rbx), %rsi
	movq	%r11, %r15
	movq	%rsi, 128(%r15)
	movq	%rax, %r14
	movq	%rdi, %r13
	movq	%r8, %r12
	movq	%r9, %rdx
	movq	%rdx, -80(%rbp)
	movq	%r11, %rbx
	movq	-56(%rbp), %rdi
	movq	%rcx, %rsi
	call	_M_StringConcat2
	movq	%rax, %r10
	movq	%r10, -56(%rbp)
	movq	%r14, %rax
	movq	%r13, %rdi
	movq	%r12, %r8
	movq	-80(%rbp), %r9
	movq	%rbx, %r11
	movq	128(%r15), %rsi
	jmp	letJoinK.5FF
doGC607:
	movq	$12, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rsi, %r15
	addq	$16, %rsi
	movq	%r15, %rdi
	movabsq	$retGC605, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.611:
	movq	%r8, %rcx
	movq	%rdi, %rdx
gcTest613:
	movq	%r11, %rbx
	movq	448(%rbx), %rbx
	subq	%rsi, %rbx
	jle	doGC614
check.609:
	/* block check<E440> (ep<E021>,x<E01C>) */
	movq	24(%r11), %rbx
	movq	8(%rbx), %r15
	cmpq	$1, %r15
	je	L615
L_true60A:
then.60C:
	/* block then<E02A> (ep<E028>,_t<E029>) */
	movq	(%r15), %r10
	movq	(%r10), %r12
	cmpq	$1, %r12
	jne	L_true60D
else.60E:
	/* block else<E0AE> (ep<E0AD>) */
letJoinK.610:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block letJoinK<E031> (ep<E02F>,_wild<E030>) */
	movq	$263, -8(%rsi)
	movabsq	$letJoinK.604, %rbx
	movq	%rbx, (%rsi)
	movq	8(%rdx), %r10
	movq	%r10, 8(%rsi)
	movq	32(%rdx), %r12
	movq	%r12, 16(%rsi)
	movq	%rsi, %rcx
	addq	$32, %rsi
	movq	24(%rdx), %r13
	movq	(%r13), %rdi
	movq	%rcx, %r8
	movq	16(%rdx), %r9
	jmp	migrate_D_to_D_top_D_level_D_sched.15C
doGC614:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	%rcx, %rdi
	movabsq	$retGC612, %r8
	jmp	_ASM_InvokeGC
	/* live= GP={%rcx %rdx} spilled=  */
retGC612:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
	jmp	gcTest613
L615:
else.60B:
	/* Liveout:  GP={%rax %rdi}  */
	/* block else<E0B1> (ep<E0B0>) */
	movq	$133, -8(%rsi)
	movabsq	$strE9, %r12
	movq	%r12, (%rsi)
	movl	$51, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movabsq	$tagEA, %r13
	movq	%r13, (%rsi)
	movq	%r10, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	16(%rdx), %r13
	movq	(%r13), %r15
	movq	%r14, %rax
	movq	%r13, %rdi
	jmp	*%r15
L_true60D:
then.60F:
	/* block then<E09E> (ep<E09B>,stk<E09D>,ite<E09C>) */
	movq	$20, -8(%rsi)
	movq	8(%r12), %r15
	movq	%r15, (%rsi)
	movq	8(%r10), %rcx
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	24(%r11), %rbx
	movq	$12, -8(%rsi)
	movq	%r14, (%rsi)
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
	movq	%r12, 24(%r11)
	jmp	letJoinK.610
	.text
k.617:
	movq	%rax, %rcx
	movq	%rdi, %rdx
	jmp	gcTest619
	/* live= GP={%rcx %rdx} spilled=  */
retGC618:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest619:
	movq	%r11, %r12
	movq	448(%r12), %rbx
	subq	%rsi, %rbx
	jle	doGC61A
check.616:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block check<E443> (ep<E019>,x<E012>) */
	movq	$1803, -8(%rsi)
	movabsq	$letJoinK.611, %r12
	movq	%r12, (%rsi)
	movq	8(%rdx), %r13
	movq	%r13, 8(%rsi)
	movq	16(%rdx), %r14
	movq	%r14, 16(%rsi)
	movq	24(%rdx), %r15
	movq	%r15, 24(%rsi)
	movq	48(%rdx), %rcx
	movq	%rcx, 32(%rsi)
	movq	%rsi, %r10
	addq	$48, %rsi
	movq	32(%rdx), %rbx
	movq	(%rbx), %rdi
	movq	40(%rdx), %r8
	movq	%r10, %r9
	movq	16(%rdx), %r10
	jmp	pFib.3A4
doGC61A:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	%r10, %rdi
	movabsq	$retGC618, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.61C:
	movq	%rax, %rcx
	movq	%rdi, %rdx
	jmp	gcTest61E
	/* live= GP={%rcx %rdx} spilled=  */
retGC61D:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest61E:
	movq	%r11, %rbx
	movq	448(%rbx), %rbx
	subq	%rsi, %rbx
	jle	doGC61F
check.61B:
	/* Liveout:  GP={%rax %rdi}  */
	/* block check<E446> (ep<E0D1>,_wild<E0D0>) */
	movq	$3, (%r11)
	movq	32(%r11), %r12
	movq	8(%r12), %r13
	movq	%r13, 32(%r11)
	movq	(%r12), %r10
	movq	(%r10), %r14
	movq	$1, %r15
	movq	%r15, %rax
	movq	%r10, %rdi
	jmp	*%r14
doGC61F:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	%rcx, %rdi
	movabsq	$retGC61D, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.621:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest623
	/* live= GP={%r10 %rcx %rdx %rbx} spilled=  */
retGC622:
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest623:
	movq	%r11, %r13
	movq	448(%r13), %r12
	subq	%rsi, %r12
	jle	doGC624
check.620:
	/* Liveout:  GP={%r9 %r8 %rax %rdi}  */
	/* block check<E44B> (ep<E0CD>,spawnFn<E0C9>,unused<E0CA>,unused<E0CB>) */
	movq	$10, -8(%rsi)
	movabsq	$letJoinK.61C, %r14
	movq	%r14, (%rsi)
	movq	%rsi, %r13
	addq	$16, %rsi
	movq	8(%rdx), %r15
	movq	8(%rbx), %r10
	movq	16(%rbx), %rbx
	movq	(%rdx), %rcx
	movq	%r10, %r9
	movq	%r13, %r8
	movq	%rbx, %rax
	movq	%rcx, %rdi
	jmp	*%r15
doGC624:
	movq	$36, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %r12
	addq	$40, %rsi
	movq	%r12, %rdi
	movabsq	$retGC622, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.626:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest628
	/* live= GP={%rcx %rdx} spilled=  */
retGC627:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest628:
	movq	%r11, %r13
	movq	448(%r13), %rbx
	subq	%rsi, %rbx
	jle	doGC629
check.625:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block check<E44E> (ep<E0C1>,ite<E0BE>) */
	movq	$20, -8(%rsi)
	movq	(%rcx), %r12
	movq	%r12, (%rsi)
	movq	$1, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movq	24(%rdx), %r14
	movq	%r14, (%rsi)
	movq	%r10, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	$775, -8(%rsi)
	movabsq	$letJoinK.621, %rcx
	movq	%rcx, (%rsi)
	movq	8(%rdx), %rbx
	movq	%rbx, 8(%rsi)
	movq	%r13, 16(%rsi)
	movq	%rsi, %r15
	addq	$32, %rsi
	movq	16(%rdx), %r10
	movq	(%r10), %rdi
	movq	$1, %r8
	movq	%r15, %r9
	movq	8(%rdx), %r10
	jmp	current_D_work_D_group.169
doGC629:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	%r12, %rdi
	movabsq	$retGC627, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.62B:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest62D
	/* live= GP={%rcx %rdx} spilled=  */
retGC62C:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest62D:
	movq	%r11, %r15
	movq	448(%r15), %rbx
	subq	%rsi, %rbx
	jle	doGC62E
check.62A:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block check<E451> (ep<E002>,ite<DFF8>) */
	movq	$20, -8(%rsi)
	movq	72(%rdx), %r12
	movq	%r12, (%rsi)
	movq	(%rcx), %r13
	movq	%r13, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movq	%r10, (%rsi)
	movq	8(%rcx), %r15
	movq	%r15, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	24(%r11), %rcx
	movq	$12, -8(%rsi)
	movq	%r14, (%rsi)
	movq	%rsi, %rbx
	addq	$16, %rsi
	movq	$1289, -8(%rsi)
	movl	(%rcx), %r12d
	movl	%r12d, (%rsi)
	movq	%rbx, 8(%rsi)
	movl	16(%rcx), %r13d
	movl	%r13d, 16(%rsi)
	movq	24(%rcx), %r14
	movq	%r14, 24(%rsi)
	movq	%rsi, %r10
	addq	$40, %rsi
	movq	%r10, 24(%r11)
	movq	$7951, -8(%rsi)
	movabsq	$k.617, %rcx
	movq	%rcx, (%rsi)
	movq	8(%rdx), %rbx
	movq	%rbx, 8(%rsi)
	movq	16(%rdx), %r10
	movq	%r10, 16(%rsi)
	movq	32(%rdx), %r12
	movq	%r12, 24(%rsi)
	movq	48(%rdx), %r13
	movq	%r13, 32(%rsi)
	movq	56(%rdx), %r14
	movq	%r14, 40(%rsi)
	movq	64(%rdx), %r15
	movq	%r15, 48(%rsi)
	movq	%rsi, %r15
	addq	$64, %rsi
	movq	$1801, -8(%rsi)
	movabsq	$letJoinK.626, %rbx
	movq	%rbx, (%rsi)
	movq	16(%rdx), %r10
	movq	%r10, 8(%rsi)
	movq	40(%rdx), %r12
	movq	%r12, 16(%rsi)
	movq	%r15, 24(%rsi)
	movq	%rsi, %rcx
	addq	$40, %rsi
	movq	24(%rdx), %r13
	movq	(%r13), %rdi
	movq	%rcx, %r8
	movq	16(%rdx), %r9
	jmp	get_D_ite.EB
doGC62E:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	%r14, %rdi
	movabsq	$retGC62C, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.633:
	movq	%rdi, %rcx
	jmp	gcTest635
	/* live= GP={%rcx} spilled=  */
retGC634:
	movq	(%rdi), %rcx
gcTest635:
	movq	%r11, %rdx
	movq	448(%rdx), %rdx
	subq	%rsi, %rdx
	jle	doGC636
check.62F:
	/* block check<E453> (ep<DFEF>) */
	movq	24(%r11), %rbx
	cmpq	$1, 8(%rbx)
	je	L637
L_true630:
letJoinK.632:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block letJoinK<DFF7> (ep<DFF6>) */
	movq	$98069, -8(%rsi)
	movabsq	$letJoinK.62B, %r12
	movq	%r12, (%rsi)
	movq	8(%rcx), %r13
	movq	%r13, 8(%rsi)
	movq	16(%rcx), %r14
	movq	%r14, 16(%rsi)
	movq	24(%rcx), %r15
	movq	%r15, 24(%rsi)
	movq	32(%rcx), %rdx
	movq	%rdx, 32(%rsi)
	movq	40(%rcx), %rbx
	movq	%rbx, 40(%rsi)
	movq	48(%rcx), %r10
	movq	%r10, 48(%rsi)
	movq	56(%rcx), %r12
	movq	%r12, 56(%rsi)
	movq	64(%rcx), %r13
	movq	%r13, 64(%rsi)
	movq	72(%rcx), %r14
	movq	%r14, 72(%rsi)
	movq	%rsi, %r10
	addq	$88, %rsi
	movq	24(%rcx), %r15
	movq	(%r15), %rdi
	movq	%r10, %r8
	movq	16(%rcx), %r9
	jmp	get_D_ite.EB
doGC636:
	movq	$12, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%rsi, %rcx
	addq	$16, %rsi
	movq	%rcx, %rdi
	movabsq	$retGC634, %r8
	jmp	_ASM_InvokeGC
L637:
else.631:
	/* block else<E0EF> (ep<E0EE>) */
	movq	$20, -8(%rsi)
	movq	$1, (%rsi)
	movq	$1, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	24(%r11), %r12
	movq	$12, -8(%rsi)
	movq	%r10, (%rsi)
	movq	%rsi, %r13
	addq	$16, %rsi
	movq	$1289, -8(%rsi)
	movl	(%r12), %r15d
	movl	%r15d, (%rsi)
	movq	%r13, 8(%rsi)
	movl	16(%r12), %edx
	movl	%edx, 16(%rsi)
	movq	24(%r12), %rbx
	movq	%rbx, 24(%rsi)
	movq	%rsi, %r14
	addq	$40, %rsi
	movq	%r14, 24(%r11)
	jmp	letJoinK.632
	.text
letJoinK.5EA:
	movq	%rdi, %rcx
	jmp	gcTest63A
	/* live= GP={%rcx} spilled=  */
retGC639:
	movq	(%rdi), %rcx
gcTest63A:
	movq	%r11, %rdx
	movq	448(%rdx), %rdx
	subq	%rsi, %rdx
	jle	doGC63B
check.638:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block check<E455> (ep<DFE3>) */
	movq	$98069, -8(%rsi)
	movabsq	$letJoinK.633, %r10
	movq	%r10, (%rsi)
	movq	8(%rcx), %r12
	movq	%r12, 8(%rsi)
	movq	16(%rcx), %r13
	movq	%r13, 16(%rsi)
	movq	24(%rcx), %r14
	movq	%r14, 24(%rsi)
	movq	32(%rcx), %r15
	movq	%r15, 32(%rsi)
	movq	40(%rcx), %rdx
	movq	%rdx, 40(%rsi)
	movq	48(%rcx), %rbx
	movq	%rbx, 48(%rsi)
	movq	56(%rcx), %r10
	movq	%r10, 56(%rsi)
	movq	64(%rcx), %r12
	movq	%r12, 64(%rsi)
	movq	72(%rcx), %r13
	movq	%r13, 72(%rsi)
	movq	%rsi, %rbx
	addq	$88, %rsi
	movq	32(%rcx), %r14
	movq	(%r14), %rdi
	movq	%rbx, %r8
	movq	16(%rcx), %r9
	jmp	migrate_D_to_D_top_D_level_D_sched.15C
doGC63B:
	movq	$12, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%rsi, %r15
	addq	$16, %rsi
	movq	%r15, %rdi
	movabsq	$retGC639, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.63D:
	movq	%rdi, %rcx
	jmp	gcTest63F
	/* live= GP={%rcx} spilled=  */
retGC63E:
	movq	(%rdi), %rcx
gcTest63F:
	movq	%r11, %r12
	movq	448(%r12), %rdx
	subq	%rsi, %rdx
	jle	doGC640
check.63C:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block check<E457> (ep<E109>) */
	movq	8(%rcx), %rbx
	movq	(%rbx), %rdi
	movq	24(%rcx), %r8
	movq	16(%rcx), %r9
	jmp	lp.5E1
doGC640:
	movq	$12, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%rsi, %r10
	addq	$16, %rsi
	movq	%r10, %rdi
	movabsq	$retGC63E, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.64B:
	movq	%rdi, %rbx
gcTest64D:
	movq	%r11, %r15
	movq	448(%r15), %rcx
	subq	%rsi, %rcx
	jle	doGC64E
	movq	%rbx, -64(%rbp)
check.641:
	/* block check<E459> (ep<D919>) */
	movq	%rax, %r12
	movq	%rdi, %r13
	movq	%r8, %r14
	movq	%r9, %r15
	movq	%rsi, %rbx
	movq	%r11, %rdx
	movq	%rdx, -72(%rbp)
	call	_GetNumVProcs
	movq	%rax, %r10
	movq	%r12, %rax
	movq	%r13, %rdi
	movq	%r14, %r8
	movq	%r15, %r9
	movq	%rbx, %rsi
	movq	-72(%rbp), %r11
	cmpl	$0, %r10d
	jl	L_true642
	movq	%r10, -56(%rbp)
	movq	-64(%rbp), %r13
	movq	$1, %rcx
else.643:
	/* block else<D92C> (ep<D929>,n<D92B>,con_false<D92A>) */
	movq	%r11, %r15
	movq	%rsi, 128(%r15)
	movq	%rax, %r14
	movq	%rdi, %r12
	movq	%r8, %rbx
	movq	%r9, %r10
	movq	%r10, -64(%rbp)
	movq	%r11, %rdx
	movq	%rdx, -72(%rbp)
	movq	%r11, %rdx
	movq	%rdx, %rdi
	movq	%rcx, %rsi
	call	_PromoteObj
	movq	%rax, %rdx
	movq	%r14, %rax
	movq	%r12, %rdi
	movq	%rbx, %r8
	movq	-64(%rbp), %r9
	movq	-72(%rbp), %r11
	movq	128(%r15), %rsi
	movq	%rax, %rbx
	movq	%rbx, -64(%rbp)
	movq	%rdi, %r10
	movq	%r10, -72(%rbp)
	movq	%r8, %r15
	movq	%r9, %r14
	movq	%rsi, %r12
	movq	%r11, %rbx
	movq	%r11, %r10
	movq	%r10, %rdi
	movslq	-56(%rbp), %rcx
	movq	%rcx, %rsi
	call	_M_NewArray
	movq	%rax, %rdx
	movq	-64(%rbp), %rax
	movq	-72(%rbp), %rdi
	movq	%r15, %r8
	movq	%r14, %r9
	movq	%r12, %rsi
	movq	%rbx, %r11
	movq	$133, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	-56(%rbp), %r12
	movl	%r12d, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	$16143, -8(%rsi)
	movq	136(%r13), %r12
	movq	%r12, (%rsi)
	movq	160(%r13), %r14
	movq	%r14, 8(%rsi)
	movq	168(%r13), %r15
	movq	%r15, 16(%rsi)
	movq	176(%r13), %rcx
	movq	%rcx, 24(%rsi)
	movq	192(%r13), %rdx
	movq	%rdx, 32(%rsi)
	movq	%r10, 40(%rsi)
	movq	144(%r13), %rbx
	movq	%rbx, 48(%rsi)
	movq	%rsi, %rcx
	addq	$64, %rsi
	movq	%r11, %r15
	movq	%rsi, 128(%r15)
	movq	%rax, %r14
	movq	%rdi, %rdx
	movq	%rdx, -64(%rbp)
	movq	%r8, %rbx
	movq	%rbx, -72(%rbp)
	movq	%r9, %r12
	movq	%r11, %rbx
	movq	%r11, %r10
	movq	%r10, %rdi
	movq	%rcx, %rsi
	call	_PromoteObj
	movq	%rax, %r10
	movq	%r10, -56(%rbp)
	movq	%r14, %rax
	movq	-64(%rbp), %rdi
	movq	-72(%rbp), %r8
	movq	%r12, %r9
	movq	%rbx, %r11
	movq	128(%r15), %rsi
	movq	$4063135, -8(%rsi)
	movq	16(%r13), %r14
	movq	%r14, (%rsi)
	movq	24(%r13), %r15
	movq	%r15, 8(%rsi)
	movq	32(%r13), %rcx
	movq	%rcx, 16(%rsi)
	movq	40(%r13), %rdx
	movq	%rdx, 24(%rsi)
	movq	48(%r13), %rbx
	movq	%rbx, 32(%rsi)
	movq	56(%r13), %r10
	movq	%r10, 40(%rsi)
	movq	64(%r13), %r12
	movq	%r12, 48(%rsi)
	movq	88(%r13), %r14
	movq	%r14, 56(%rsi)
	movq	96(%r13), %r15
	movq	%r15, 64(%rsi)
	movq	128(%r13), %rcx
	movq	%rcx, 72(%rsi)
	movq	136(%r13), %rdx
	movq	%rdx, 80(%rsi)
	movq	144(%r13), %rbx
	movq	%rbx, 88(%rsi)
	movq	152(%r13), %r10
	movq	%r10, 96(%rsi)
	movq	184(%r13), %r12
	movq	%r12, 104(%rsi)
	movq	192(%r13), %r14
	movq	%r14, 112(%rsi)
	movq	%rsi, %r12
	addq	$128, %rsi
	movq	$133, -8(%rsi)
	movq	%r12, (%rsi)
	movabsq	$spawnWorker.5DC, %r15
	movq	%r15, 8(%rsi)
	movq	%rsi, %r12
	movq	%r12, -72(%rbp)
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movq	16(%r13), %rdx
	movq	%rdx, (%rsi)
	movq	-72(%rbp), %r14
	movq	%r14, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	$133, -8(%rsi)
	movq	%rcx, (%rsi)
	movabsq	$lp.5E1, %rbx
	movq	%rbx, 8(%rsi)
	movq	%rsi, %r15
	movq	%r15, -64(%rbp)
	addq	$24, %rsi
	movq	%r11, %r14
	movq	%rsi, 128(%r14)
	movq	%rax, %rcx
	movq	%rcx, -88(%rbp)
	movq	%rdi, %r15
	movq	%r8, %rbx
	movq	%r9, %r12
	movq	%r11, %rdx
	movq	%rdx, -80(%rbp)
	movq	%r11, %r10
	movq	%r10, %rdi
	call	_ListVProcs
	movq	%rax, %rcx
	movq	-88(%rbp), %rax
	movq	%r15, %rdi
	movq	%rbx, %r8
	movq	%r12, %r9
	movq	-80(%rbp), %r11
	movq	128(%r14), %rsi
	movq	$98069, -8(%rsi)
	movabsq	$letJoinK.5EA, %r14
	movq	%r14, (%rsi)
	movq	8(%r13), %r15
	movq	%r15, 8(%rsi)
	movq	16(%r13), %rdx
	movq	%rdx, 16(%rsi)
	movq	40(%r13), %rbx
	movq	%rbx, 24(%rsi)
	movq	72(%r13), %r10
	movq	%r10, 32(%rsi)
	movq	80(%r13), %r12
	movq	%r12, 40(%rsi)
	movq	104(%r13), %r14
	movq	%r14, 48(%rsi)
	movq	112(%r13), %r15
	movq	%r15, 56(%rsi)
	movq	120(%r13), %rdx
	movq	%rdx, 64(%rsi)
	movq	-56(%rbp), %rbx
	movq	%rbx, 72(%rsi)
	movq	%rsi, %r12
	addq	$88, %rsi
	cmpq	$1, %rcx
	jne	L_true647
else.648:
	/* Liveout:  GP={%rdi}  */
	/* block else<E116> (letJoinK<E115>) */
	movq	%r12, %rdi
	jmp	letJoinK.5EA
L_true647:
then.649:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block then<E104> (ep<E0FF>,spawnWorker<E103>,lp<E102>,vps<E101>,letJoinK<E100>) */
	movq	$1801, -8(%rsi)
	movabsq	$letJoinK.63D, %r10
	movq	%r10, (%rsi)
	movq	-64(%rbp), %r10
	movq	%r10, 8(%rsi)
	movq	%r12, 16(%rsi)
	movq	8(%rcx), %r12
	movq	%r12, 24(%rsi)
	movq	%rsi, %rbx
	addq	$40, %rsi
	movq	-72(%rbp), %r12
	movq	(%r12), %rdi
	movq	(%rcx), %r14
	movq	(%r14), %r8
	movq	%rbx, %r9
	movq	16(%r13), %r10
	jmp	spawnWorker.5DC
doGC64E:
	movq	$12, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rsi, %r14
	addq	$16, %rsi
	movq	%r14, %rdi
	movabsq	$retGC64C, %r8
	jmp	_ASM_InvokeGC
	/* live= spilled= GP={%r~1}  */
retGC64C:
	movq	(%rdi), %rbx
	jmp	gcTest64D
L_true642:
	movq	-64(%rbp), %rdx
then.644:
	/* Liveout:  GP={%rax %rdi}  */
	/* block then<D921> (ep<D920>) */
	movq	$133, -8(%rsi)
	movabsq	$str64A, %rcx
	movq	%rcx, (%rsi)
	movl	$29, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movabsq	$tagEA, %r10
	movq	%r10, (%rsi)
	movq	%r15, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	16(%rdx), %r12
	movq	(%r12), %r13
	movq	%rbx, %rax
	movq	%r12, %rdi
	jmp	*%r13
	.text
letJoinK.650:
	movq	%rdi, %rcx
	jmp	gcTest652
	/* live= GP={%rcx} spilled=  */
retGC651:
	movq	(%rdi), %rcx
gcTest652:
	movq	%r11, %r10
	movq	448(%r10), %rdx
	subq	%rsi, %rdx
	jle	doGC653
check.64F:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block check<E45B> (ep<D72C>) */
	movq	$12, -8(%rsi)
	movq	80(%rcx), %r10
	movq	%r10, (%rsi)
	movq	%rsi, %rbx
	addq	$16, %rsi
	movq	$20, -8(%rsi)
	movq	%rbx, (%rsi)
	movabsq	$spawnFn.421, %r12
	movq	%r12, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	$12, -8(%rsi)
	movq	40(%rcx), %r14
	movq	%r14, (%rsi)
	movq	%rsi, %r13
	addq	$16, %rsi
	movq	$20, -8(%rsi)
	movq	%r13, (%rsi)
	movabsq	$resumeFn.442, %r15
	movq	%r15, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	$12, -8(%rsi)
	movq	80(%rcx), %rbx
	movq	%rbx, (%rsi)
	movq	%rsi, %rdx
	addq	$16, %rsi
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movabsq	$removeFn.468, %r10
	movq	%r10, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	$4273995571, %r13
	movq	%r13, -8(%rsi)
	movabsq	$letJoinK.64B, %r14
	movq	%r14, (%rsi)
	movq	8(%rcx), %rdx
	movq	%rdx, 8(%rsi)
	movq	16(%rcx), %rbx
	movq	%rbx, 16(%rsi)
	movq	24(%rcx), %r13
	movq	%r13, 24(%rsi)
	movq	32(%rcx), %r14
	movq	%r14, 32(%rsi)
	movq	40(%rcx), %rdx
	movq	%rdx, 40(%rsi)
	movq	48(%rcx), %rbx
	movq	%rbx, 48(%rsi)
	movq	56(%rcx), %r13
	movq	%r13, 56(%rsi)
	movq	64(%rcx), %r14
	movq	%r14, 64(%rsi)
	movq	72(%rcx), %rdx
	movq	%rdx, 72(%rsi)
	movq	80(%rcx), %rbx
	movq	%rbx, 80(%rsi)
	movq	88(%rcx), %r13
	movq	%r13, 88(%rsi)
	movq	96(%rcx), %r14
	movq	%r14, 96(%rsi)
	movq	104(%rcx), %rdx
	movq	%rdx, 104(%rsi)
	movq	112(%rcx), %rbx
	movq	%rbx, 112(%rsi)
	movq	120(%rcx), %r13
	movq	%r13, 120(%rsi)
	movq	128(%rcx), %r14
	movq	%r14, 128(%rsi)
	movq	136(%rcx), %rdx
	movq	%rdx, 136(%rsi)
	movq	144(%rcx), %rbx
	movq	%rbx, 144(%rsi)
	movq	152(%rcx), %r13
	movq	%r13, 152(%rsi)
	movq	%r12, 160(%rsi)
	movq	%r15, 168(%rsi)
	movq	%r10, 176(%rsi)
	movq	160(%rcx), %r14
	movq	%r14, 184(%rsi)
	movq	168(%rcx), %r15
	movq	%r15, 192(%rsi)
	movq	%rsi, %r12
	addq	$208, %rsi
	movq	72(%rcx), %rdx
	movq	(%rdx), %rdi
	movq	%r12, %r8
	movq	16(%rcx), %r9
	jmp	migrate_D_to_D_top_D_level_D_sched.15C
doGC653:
	movq	$12, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%rsi, %rbx
	addq	$16, %rsi
	movq	%rbx, %rdi
	movabsq	$retGC651, %r8
	jmp	_ASM_InvokeGC
	.text
k.655:
	movq	%rax, %rcx
	movq	%rdi, %rdx
	jmp	gcTest657
	/* live= GP={%rcx %rdx} spilled=  */
retGC656:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest657:
	movq	%r11, %r12
	movq	448(%r12), %rbx
	subq	%rsi, %rbx
	jle	doGC658
check.654:
	/* Liveout:  GP={%rdi}  */
	/* block check<E45E> (ep<E128>,x<E127>) */
	movq	8(%rdx), %rdi
	jmp	letJoinK.650
doGC658:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	%r10, %rdi
	movabsq	$retGC656, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.664:
	movq	%r8, %r14
	movq	%rdi, %r13
	jmp	gcTest666
	/* live= spilled= GP={%r~1 %r~1}  */
retGC665:
	movq	8(%rdi), %r14
	movq	(%rdi), %r13
gcTest666:
	movq	%r11, %rdx
	movq	448(%rdx), %rbx
	subq	%rsi, %rbx
	jle	doGC667
	movq	%r14, -72(%rbp)
	movq	%r13, -80(%rbp)
check.659:
	/* block check<E461> (ep<D6B8>,_t<D6AB>) */
	movq	$10, -8(%rsi)
	movq	$1, (%rsi)
	movq	%rsi, %rcx
	addq	$16, %rsi
	movq	%r11, %r15
	movq	%rsi, 128(%r15)
	movq	%rax, %r10
	movq	%r10, -88(%rbp)
	movq	%rdi, %r14
	movq	%r8, %r13
	movq	%r9, %r12
	movq	%r11, %rbx
	movq	%r11, %rdx
	movq	%rdx, %rdi
	movq	%rcx, %rsi
	call	_PromoteObj
	movq	%rax, %rcx
	movq	%rcx, -64(%rbp)
	movq	-88(%rbp), %rax
	movq	%r14, %rdi
	movq	%r13, %r8
	movq	%r12, %r9
	movq	%rbx, %r11
	movq	128(%r15), %rsi
	movq	$18, -8(%rsi)
	movq	$1, (%rsi)
	movabsq	$pickVictim.3ED, %rbx
	movq	%rbx, 8(%rsi)
	movq	%rsi, %rdx
	movq	%rdx, -88(%rbp)
	addq	$24, %rsi
	movq	%rax, %r12
	movq	%rdi, %r13
	movq	%r8, %r14
	movq	%r9, %r15
	movq	%rsi, %rbx
	movq	%r11, %r10
	movq	%r10, -96(%rbp)
	call	_GetNumVProcs
	movq	%rax, %r10
	movq	%r12, %rax
	movq	%r13, %rdi
	movq	%r14, %r8
	movq	%r15, %r9
	movq	%rbx, %rsi
	movq	-96(%rbp), %r11
	cmpl	$0, %r10d
	jge	L668
L_true65A:
	movq	-80(%rbp), %r12
then.65C:
	/* Liveout:  GP={%rax %rdi}  */
	/* block then<D6E9> (ep<D6E8>) */
	movq	$133, -8(%rsi)
	movabsq	$str64A, %rbx
	movq	%rbx, (%rsi)
	movl	$29, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movabsq	$tagEA, %r13
	movq	%r13, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	16(%r12), %r14
	movq	(%r14), %r15
	movq	%r10, %rax
	movq	%r14, %rdi
	jmp	*%r15
doGC667:
	movq	$133, -8(%rsi)
	movq	%r13, (%rsi)
	movq	%r14, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	%rcx, %rdi
	movabsq	$retGC665, %r8
	jmp	_ASM_InvokeGC
L668:
	movq	%r10, -96(%rbp)
	movq	$1, %rcx
else.65B:
	/* block else<D6F7> (ep<D6F1>,_t<D6F6>,terminated<D6F5>,pickVictim<D6F4>,n<D6F3>,_t<D6F2>) */
	movq	%r11, %r14
	movq	%rsi, 128(%r14)
	movq	%rax, %r12
	movq	%r12, -56(%rbp)
	movq	%rdi, %r13
	movq	%r8, %r12
	movq	%r9, %rbx
	movq	%r11, %r15
	movq	%r11, %rdx
	movq	%rdx, %rdi
	movq	%rcx, %rsi
	call	_PromoteObj
	movq	%rax, %rdx
	movq	-56(%rbp), %rax
	movq	%r13, %rdi
	movq	%r12, %r8
	movq	%rbx, %r9
	movq	%r15, %r11
	movq	128(%r14), %rsi
	movq	%rax, %r13
	movq	%r13, -56(%rbp)
	movq	%rdi, %r15
	movq	%r8, %r14
	movq	%r9, %r13
	movq	%rsi, %r12
	movq	%r11, %rbx
	movq	%r11, %r10
	movq	%r10, %rdi
	movslq	-96(%rbp), %rcx
	movq	%rcx, %rsi
	call	_M_NewArray
	movq	%rax, %rdx
	movq	-56(%rbp), %rax
	movq	%r15, %rdi
	movq	%r14, %r8
	movq	%r13, %r9
	movq	%r12, %rsi
	movq	%rbx, %r11
	movq	$133, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	-96(%rbp), %r14
	movl	%r14d, 8(%rsi)
	movq	%rsi, %r15
	movq	%r15, -56(%rbp)
	addq	$24, %rsi
	cmpl	$0, -96(%rbp)
	jge	L669
L_true65D:
	movq	-80(%rbp), %r10
then.65F:
	/* Liveout:  GP={%rax %rdi}  */
	/* block then<D702> (ep<D701>) */
	movq	$133, -8(%rsi)
	movabsq	$str64A, %r12
	movq	%r12, (%rsi)
	movl	$29, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movabsq	$tagEA, %r14
	movq	%r14, (%rsi)
	movq	%rbx, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	16(%r10), %r15
	movq	(%r15), %rcx
	movq	%r13, %rax
	movq	%r15, %rdi
	jmp	*%rcx
L669:
	movq	-80(%rbp), %r15
	movq	$1, %rcx
else.65E:
	/* block else<D711> (ep<D70A>,_t<D710>,terminated<D70F>,pickVictim<D70E>,n<D70D>,con_false<D70C>,_cast_t<D70B>) */
	movq	%r11, %r14
	movq	%rsi, 128(%r14)
	movq	%rax, %rdx
	movq	%rdx, -104(%rbp)
	movq	%rdi, %r13
	movq	%r8, %rbx
	movq	%rbx, -80(%rbp)
	movq	%r9, %r12
	movq	%r11, %rbx
	movq	%r11, %r10
	movq	%r10, %rdi
	movq	%rcx, %rsi
	call	_PromoteObj
	movq	%rax, %rdx
	movq	-104(%rbp), %rax
	movq	%r13, %rdi
	movq	-80(%rbp), %r8
	movq	%r12, %r9
	movq	%rbx, %r11
	movq	128(%r14), %rsi
	movq	%rax, %r10
	movq	%r10, -104(%rbp)
	movq	%rdi, %r14
	movq	%r8, %r13
	movq	%r9, %r12
	movq	%rsi, %rcx
	movq	%rcx, -80(%rbp)
	movq	%r11, %rbx
	movq	%r11, %rcx
	movq	%rcx, %rdi
	movslq	-96(%rbp), %r10
	movq	%r10, %rsi
	call	_M_NewArray
	movq	%rax, %rcx
	movq	-104(%rbp), %rax
	movq	%r14, %rdi
	movq	%r13, %r8
	movq	%r12, %r9
	movq	-80(%rbp), %rsi
	movq	%rbx, %r11
	movq	$133, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	-96(%rbp), %rdx
	movl	%edx, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	$3, (%r11)
	movq	$515899181, -8(%rsi)
	movabsq	$letJoinK.650, %r12
	movq	%r12, (%rsi)
	movq	8(%r15), %r13
	movq	%r13, 8(%rsi)
	movq	16(%r15), %r14
	movq	%r14, 16(%rsi)
	movq	24(%r15), %rcx
	movq	%rcx, 24(%rsi)
	movq	32(%r15), %rdx
	movq	%rdx, 32(%rsi)
	movq	40(%r15), %rbx
	movq	%rbx, 40(%rsi)
	movq	48(%r15), %r12
	movq	%r12, 48(%rsi)
	movq	56(%r15), %r13
	movq	%r13, 56(%rsi)
	movq	64(%r15), %r14
	movq	%r14, 64(%rsi)
	movq	72(%r15), %rcx
	movq	%rcx, 72(%rsi)
	movq	80(%r15), %rdx
	movq	%rdx, 80(%rsi)
	movq	88(%r15), %rbx
	movq	%rbx, 88(%rsi)
	movq	96(%r15), %r12
	movq	%r12, 96(%rsi)
	movq	104(%r15), %r13
	movq	%r13, 104(%rsi)
	movq	112(%r15), %r14
	movq	%r14, 112(%rsi)
	movq	120(%r15), %rcx
	movq	%rcx, 120(%rsi)
	movq	128(%r15), %rdx
	movq	%rdx, 128(%rsi)
	movq	-72(%rbp), %rbx
	movq	%rbx, 136(%rsi)
	movq	-64(%rbp), %r12
	movq	%r12, 144(%rsi)
	movq	-88(%rbp), %r13
	movq	%r13, 152(%rsi)
	movq	%r10, 160(%rsi)
	movq	-56(%rbp), %r14
	movq	%r14, 168(%rsi)
	movq	%rsi, %r10
	addq	$184, %rsi
	movq	8(%r11), %rbx
	cmpq	$1, %rbx
	jne	L66A
S_case660:
	movq	%r11, %rbx
case.661:
	/* Liveout:  GP={%rdi}  */
	/* block case<E11F> (vp<E11E>,letJoinK<E11D>) */
	movq	$1, (%rbx)
	movq	%r10, %rdi
	jmp	letJoinK.650
L66A:
	cmpq	$3, %rbx
	jne	S_case660
S_case662:
	movq	%r11, %rdx
case.663:
	/* Liveout:  GP={%rax %rdi}  */
	/* block case<E125> (vp<E124>,letJoinK<E123>) */
	movq	$1, 8(%rdx)
	movq	$20, -8(%rsi)
	movabsq	$k.655, %r12
	movq	%r12, (%rsi)
	movq	%r10, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movq	$1, (%rsi)
	movq	%r10, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	32(%rdx), %r14
	movq	8(%r14), %r15
	movq	%r15, 32(%rdx)
	movq	(%r14), %rcx
	movq	(%rcx), %rdx
	movq	%r13, %rax
	movq	%rcx, %rdi
	jmp	*%rdx
	.text
letJoinK.390:
	movq	%rdi, %rdx
	jmp	gcTest67B
	/* live= GP={%rdx} spilled=  */
retGC67A:
	movq	(%rdi), %rdx
gcTest67B:
	movq	%r11, %r10
	movq	448(%r10), %r12
	subq	%rsi, %r12
	jle	doGC67C
check.66B:
	/* block check<E463> (ep<D577>) */
	movq	24(%r11), %rbx
	cmpq	$1, 8(%rbx)
	je	L67D
L_true66C:
letJoinK.66E:
	/* block letJoinK<D57F> (ep<D57E>) */
	movq	24(%r11), %r12
	movq	8(%r12), %r10
	cmpq	$1, %r10
	jne	L_true66F
else.670:
	/* Liveout:  GP={%rax %rdi}  */
	/* block else<E152> (ep<E151>) */
	movq	$133, -8(%rsi)
	movabsq	$strE9, %r14
	movq	%r14, (%rsi)
	movl	$51, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movabsq	$tagEA, %rcx
	movq	%rcx, (%rsi)
	movq	%r13, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	16(%rdx), %rdx
	movq	(%rdx), %rbx
	movq	%r15, %rax
	movq	%rdx, %rdi
	jmp	*%rbx
L67D:
else.66D:
	/* block else<E15D> (ep<E15C>) */
	movq	$20, -8(%rsi)
	movq	$1, (%rsi)
	movq	$1, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	24(%r11), %r12
	movq	$12, -8(%rsi)
	movq	%r10, (%rsi)
	movq	%rsi, %r13
	addq	$16, %rsi
	movq	$1289, -8(%rsi)
	movl	(%r12), %r15d
	movl	%r15d, (%rsi)
	movq	%r13, 8(%rsi)
	movl	16(%r12), %ecx
	movl	%ecx, 16(%rsi)
	movq	24(%r12), %rbx
	movq	%rbx, 24(%rsi)
	movq	%rsi, %r14
	addq	$40, %rsi
	movq	%r14, 24(%r11)
	jmp	letJoinK.66E
doGC67C:
	movq	$12, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rsi, %rbx
	addq	$16, %rsi
	movq	%rbx, %rdi
	movabsq	$retGC67A, %r8
	jmp	_ASM_InvokeGC
L_true66F:
	movq	%rdx, -64(%rbp)
then.671:
	/* block then<D586> (ep<D584>,_t<D585>) */
	movq	(%r10), %r10
	movq	$20, -8(%rsi)
	movq	-64(%rbp), %r13
	movq	120(%r13), %r13
	movq	%r13, (%rsi)
	movq	(%r10), %r14
	movq	%r14, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movq	%r12, (%rsi)
	movq	8(%r10), %rcx
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	24(%r11), %rdx
	movq	$12, -8(%rsi)
	movq	%r15, (%rsi)
	movq	%rsi, %rbx
	addq	$16, %rsi
	movq	$1289, -8(%rsi)
	movl	(%rdx), %r12d
	movl	%r12d, (%rsi)
	movq	%rbx, 8(%rsi)
	movl	16(%rdx), %r13d
	movl	%r13d, 16(%rsi)
	movq	24(%rdx), %r14
	movq	%r14, 24(%rsi)
	movq	%rsi, %r10
	addq	$40, %rsi
	movq	%r10, 24(%r11)
	movq	%r11, %r15
	movq	%rsi, 128(%r15)
	movq	%rax, %r14
	movq	%r14, -56(%rbp)
	movq	%rdi, %rbx
	movq	%r8, %r12
	movq	%r9, %r13
	movq	%r11, %r14
	movq	%r11, %rcx
	movq	%rcx, %rdi
	movq	$1, %rdx
	movq	%rdx, %rsi
	call	_AllocVector
	movq	-56(%rbp), %rax
	movq	%rbx, %rdi
	movq	%r12, %r8
	movq	%r13, %r9
	movq	%r14, %r11
	movq	128(%r15), %rsi
	movq	$20, -8(%rsi)
	movq	-64(%rbp), %r15
	movq	40(%r15), %r13
	movq	%r13, (%rsi)
	movq	-64(%rbp), %rcx
	movq	80(%rcx), %r14
	movq	%r14, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	$133, -8(%rsi)
	movq	%r12, (%rsi)
	movabsq	$pFib.3A4, %r15
	movq	%r15, 8(%rsi)
	movq	%rsi, %rdx
	movq	%rdx, -56(%rbp)
	addq	$24, %rsi
	movq	$10, -8(%rsi)
	movl	$30, (%rsi)
	movq	%rsi, %rbx
	movq	%rbx, -72(%rbp)
	addq	$16, %rsi
	movq	%r11, %rbx
	movq	%rsi, 128(%rbx)
	movq	%rax, %r12
	movq	%rdi, %r13
	movq	%r8, %r10
	movq	%r10, -80(%rbp)
	movq	%r9, %r14
	movq	%r11, %r15
	call	_M_Arguments
	movq	%rax, %rcx
	movq	%r12, %rax
	movq	%r13, %rdi
	movq	-80(%rbp), %r8
	movq	%r14, %r9
	movq	%r15, %r11
	movq	128(%rbx), %rsi
	cmpq	$1, %rcx
	je	L67E
L_true674:
then.676:
	/* block then<E13C> (ep<E138>,pFib<E13B>,_wlit<E13A>,args<E139>) */
	movq	%r11, %r15
	movq	%rsi, 128(%r15)
	movq	%rax, %r12
	movq	%r12, -80(%rbp)
	movq	%rdi, %r14
	movq	%r8, %r13
	movq	%r9, %r12
	movq	%r11, %rbx
	movq	(%rcx), %rcx
	movq	%rcx, %rdi
	call	_M_IntFromString
	movq	%rax, %rdx
	movq	-80(%rbp), %rax
	movq	%r14, %rdi
	movq	%r13, %r8
	movq	%r12, %r9
	movq	%rbx, %r11
	movq	128(%r15), %rsi
	cmpq	$1, %rdx
	jne	L_true678
	movq	-64(%rbp), %r12
	jmp	letJoinK.675
L67E:
	movq	-64(%rbp), %r12
letJoinK.675:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block letJoinK<D6A6> (ep<D6A3>,pFib<D6A5>,n<D6A4>) */
	movq	%rax, %r14
	movq	%rdi, %r15
	movq	%r15, -64(%rbp)
	movq	%r8, %r15
	movq	%r9, %rcx
	movq	%rcx, -80(%rbp)
	movq	%rsi, %rbx
	movq	%r11, %r13
	call	_M_GetTime
	movq	%rax, %rcx
	movq	%r14, %rax
	movq	-64(%rbp), %rdi
	movq	%r15, %r8
	movq	-80(%rbp), %r9
	movq	%rbx, %rsi
	movq	%r13, %r11
	movq	$12582691, -8(%rsi)
	movabsq	$letJoinK.664, %r10
	movq	%r10, (%rsi)
	movq	8(%r12), %r13
	movq	%r13, 8(%rsi)
	movq	16(%r12), %rdx
	movq	%rdx, 16(%rsi)
	movq	24(%r12), %rbx
	movq	%rbx, 24(%rsi)
	movq	32(%r12), %r10
	movq	%r10, 32(%rsi)
	movq	40(%r12), %r13
	movq	%r13, 40(%rsi)
	movq	48(%r12), %r14
	movq	%r14, 48(%rsi)
	movq	56(%r12), %r15
	movq	%r15, 56(%rsi)
	movq	64(%r12), %rdx
	movq	%rdx, 64(%rsi)
	movq	72(%r12), %rbx
	movq	%rbx, 72(%rsi)
	movq	80(%r12), %r10
	movq	%r10, 80(%rsi)
	movq	104(%r12), %r13
	movq	%r13, 88(%rsi)
	movq	112(%r12), %r14
	movq	%r14, 96(%rsi)
	movq	-56(%rbp), %rdx
	movq	%rdx, 104(%rsi)
	movq	-72(%rbp), %rbx
	movq	%rbx, 112(%rsi)
	movq	%rcx, 120(%rsi)
	movq	24(%r11), %r15
	movq	%r15, 128(%rsi)
	movq	%rsi, %rbx
	addq	$144, %rsi
	movq	96(%r12), %rcx
	movq	(%rcx), %rdi
	movq	88(%r12), %rdx
	movq	(%rdx), %r8
	movq	%rbx, %r9
	jmp	new.27D
L_true678:
then.679:
	/* block then<E145> (ep<E142>,pFib<E144>,res<E143>) */
	movq	-64(%rbp), %r12
	movq	(%rdx), %r13
	movq	%r13, -72(%rbp)
	jmp	letJoinK.675
	.text
letJoinK.680:
	movq	%rdi, %rcx
	jmp	gcTest682
	/* live= GP={%rcx} spilled=  */
retGC681:
	movq	(%rdi), %rcx
gcTest682:
	movq	%r11, %r12
	movq	448(%r12), %rdx
	subq	%rsi, %rdx
	jle	doGC683
check.67F:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block check<E465> (ep<E173>) */
	movq	8(%rcx), %rbx
	movq	(%rbx), %rdi
	movq	24(%rcx), %r8
	movq	16(%rcx), %r9
	jmp	lp.387
doGC683:
	movq	$12, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%rsi, %r10
	addq	$16, %rsi
	movq	%r10, %rdi
	movabsq	$retGC681, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.68B:
	movq	%rdi, %rbx
gcTest68D:
	movq	%r11, %r15
	movq	448(%r15), %rcx
	subq	%rsi, %rcx
	jle	doGC68E
	movq	%rbx, -64(%rbp)
check.684:
	/* block check<E467> (ep<D308>) */
	movq	%rax, %r12
	movq	%rdi, %r13
	movq	%r8, %r14
	movq	%r9, %r15
	movq	%rsi, %rbx
	movq	%r11, %rdx
	movq	%rdx, -72(%rbp)
	call	_GetNumVProcs
	movq	%rax, %r10
	movq	%r12, %rax
	movq	%r13, %rdi
	movq	%r14, %r8
	movq	%r15, %r9
	movq	%rbx, %rsi
	movq	-72(%rbp), %r11
	cmpl	$0, %r10d
	jl	L_true685
	movq	%r10, -56(%rbp)
	movq	-64(%rbp), %r13
	movq	$1, %rcx
else.686:
	/* block else<D31B> (ep<D318>,n<D31A>,con_false<D319>) */
	movq	%r11, %r15
	movq	%rsi, 128(%r15)
	movq	%rax, %r14
	movq	%rdi, %r12
	movq	%r8, %rbx
	movq	%r9, %r10
	movq	%r10, -64(%rbp)
	movq	%r11, %rdx
	movq	%rdx, -72(%rbp)
	movq	%r11, %rdx
	movq	%rdx, %rdi
	movq	%rcx, %rsi
	call	_PromoteObj
	movq	%rax, %rdx
	movq	%r14, %rax
	movq	%r12, %rdi
	movq	%rbx, %r8
	movq	-64(%rbp), %r9
	movq	-72(%rbp), %r11
	movq	128(%r15), %rsi
	movq	%rax, %rbx
	movq	%rbx, -64(%rbp)
	movq	%rdi, %r10
	movq	%r10, -72(%rbp)
	movq	%r8, %r15
	movq	%r9, %r14
	movq	%rsi, %r12
	movq	%r11, %rbx
	movq	%r11, %r10
	movq	%r10, %rdi
	movslq	-56(%rbp), %rcx
	movq	%rcx, %rsi
	call	_M_NewArray
	movq	%rax, %rdx
	movq	-64(%rbp), %rax
	movq	-72(%rbp), %rdi
	movq	%r15, %r8
	movq	%r14, %r9
	movq	%r12, %rsi
	movq	%rbx, %r11
	movq	$133, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	-56(%rbp), %r12
	movl	%r12d, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	$16143, -8(%rsi)
	movq	136(%r13), %r12
	movq	%r12, (%rsi)
	movq	152(%r13), %r14
	movq	%r14, 8(%rsi)
	movq	152(%r13), %r15
	movq	%r15, 16(%rsi)
	movq	160(%r13), %rcx
	movq	%rcx, 24(%rsi)
	movq	168(%r13), %rdx
	movq	%rdx, 32(%rsi)
	movq	%r10, 40(%rsi)
	movq	144(%r13), %rbx
	movq	%rbx, 48(%rsi)
	movq	%rsi, %rcx
	addq	$64, %rsi
	movq	%r11, %r15
	movq	%rsi, 128(%r15)
	movq	%rax, %r14
	movq	%rdi, %rdx
	movq	%rdx, -64(%rbp)
	movq	%r8, %rbx
	movq	%rbx, -72(%rbp)
	movq	%r9, %r12
	movq	%r11, %rbx
	movq	%r11, %r10
	movq	%r10, %rdi
	movq	%rcx, %rsi
	call	_PromoteObj
	movq	%rax, %r10
	movq	%r10, -56(%rbp)
	movq	%r14, %rax
	movq	-64(%rbp), %rdi
	movq	-72(%rbp), %r8
	movq	%r12, %r9
	movq	%rbx, %r11
	movq	128(%r15), %rsi
	movq	$60, -8(%rsi)
	movq	16(%r13), %r14
	movq	%r14, (%rsi)
	movq	40(%r13), %r15
	movq	%r15, 8(%rsi)
	movq	48(%r13), %rcx
	movq	%rcx, 16(%rsi)
	movq	56(%r13), %rdx
	movq	%rdx, 24(%rsi)
	movq	64(%r13), %rbx
	movq	%rbx, 32(%rsi)
	movq	120(%r13), %r10
	movq	%r10, 40(%rsi)
	movq	128(%r13), %r12
	movq	%r12, 48(%rsi)
	movq	%rsi, %r12
	addq	$64, %rsi
	movq	$133, -8(%rsi)
	movq	%r12, (%rsi)
	movabsq	$spawnWorker.382, %r14
	movq	%r14, 8(%rsi)
	movq	%rsi, %r12
	movq	%r12, -72(%rbp)
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movq	16(%r13), %rcx
	movq	%rcx, (%rsi)
	movq	-72(%rbp), %r14
	movq	%r14, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	$133, -8(%rsi)
	movq	%r15, (%rsi)
	movabsq	$lp.387, %rdx
	movq	%rdx, 8(%rsi)
	movq	%rsi, %r15
	movq	%r15, -64(%rbp)
	addq	$24, %rsi
	movq	%r11, %r14
	movq	%rsi, 128(%r14)
	movq	%rax, %rcx
	movq	%rcx, -88(%rbp)
	movq	%rdi, %r15
	movq	%r8, %rbx
	movq	%r9, %r12
	movq	%r11, %rdx
	movq	%rdx, -80(%rbp)
	movq	%r11, %r10
	movq	%r10, %rdi
	call	_ListVProcs
	movq	%rax, %rcx
	movq	-88(%rbp), %rax
	movq	%r15, %rdi
	movq	%rbx, %r8
	movq	%r12, %r9
	movq	-80(%rbp), %r11
	movq	128(%r14), %rsi
	movq	$8388385, -8(%rsi)
	movabsq	$letJoinK.390, %r14
	movq	%r14, (%rsi)
	movq	8(%r13), %r15
	movq	%r15, 8(%rsi)
	movq	16(%r13), %rdx
	movq	%rdx, 16(%rsi)
	movq	24(%r13), %rbx
	movq	%rbx, 24(%rsi)
	movq	32(%r13), %r10
	movq	%r10, 32(%rsi)
	movq	40(%r13), %r12
	movq	%r12, 40(%rsi)
	movq	48(%r13), %r14
	movq	%r14, 48(%rsi)
	movq	56(%r13), %r15
	movq	%r15, 56(%rsi)
	movq	64(%r13), %rdx
	movq	%rdx, 64(%rsi)
	movq	72(%r13), %rbx
	movq	%rbx, 72(%rsi)
	movq	80(%r13), %r10
	movq	%r10, 80(%rsi)
	movq	88(%r13), %r12
	movq	%r12, 88(%rsi)
	movq	96(%r13), %r14
	movq	%r14, 96(%rsi)
	movq	104(%r13), %r15
	movq	%r15, 104(%rsi)
	movq	112(%r13), %rdx
	movq	%rdx, 112(%rsi)
	movq	-56(%rbp), %rbx
	movq	%rbx, 120(%rsi)
	movq	%rsi, %r12
	addq	$136, %rsi
	cmpq	$1, %rcx
	jne	L_true688
else.689:
	/* Liveout:  GP={%rdi}  */
	/* block else<E180> (letJoinK<E17F>) */
	movq	%r12, %rdi
	jmp	letJoinK.390
L_true688:
then.68A:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block then<E16E> (ep<E169>,spawnWorker<E16D>,lp<E16C>,vps<E16B>,letJoinK<E16A>) */
	movq	$1801, -8(%rsi)
	movabsq	$letJoinK.680, %r10
	movq	%r10, (%rsi)
	movq	-64(%rbp), %r10
	movq	%r10, 8(%rsi)
	movq	%r12, 16(%rsi)
	movq	8(%rcx), %r12
	movq	%r12, 24(%rsi)
	movq	%rsi, %rbx
	addq	$40, %rsi
	movq	-72(%rbp), %r12
	movq	(%r12), %rdi
	movq	(%rcx), %r14
	movq	(%r14), %r8
	movq	%rbx, %r9
	movq	16(%r13), %r10
	jmp	spawnWorker.382
doGC68E:
	movq	$12, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rsi, %r14
	addq	$16, %rsi
	movq	%r14, %rdi
	movabsq	$retGC68C, %r8
	jmp	_ASM_InvokeGC
	/* live= spilled= GP={%r~1}  */
retGC68C:
	movq	(%rdi), %rbx
	jmp	gcTest68D
L_true685:
	movq	-64(%rbp), %rdx
then.687:
	/* Liveout:  GP={%rax %rdi}  */
	/* block then<D310> (ep<D30F>) */
	movq	$133, -8(%rsi)
	movabsq	$str64A, %rcx
	movq	%rcx, (%rsi)
	movl	$29, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movabsq	$tagEA, %r10
	movq	%r10, (%rsi)
	movq	%r15, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	16(%rdx), %r12
	movq	(%r12), %r13
	movq	%rbx, %rax
	movq	%r12, %rdi
	jmp	*%r13
	.text
letJoinK.690:
	movq	%r8, %rdx
	movq	%rdi, %r14
	jmp	gcTest692
	/* live= GP={%r14} spilled= GP={%r~1}  */
retGC691:
	movq	8(%rdi), %rdx
	movq	(%rdi), %r14
gcTest692:
	movq	%r11, %r15
	movq	448(%r15), %rcx
	subq	%rsi, %rcx
	jle	doGC693
	movq	%rdx, -56(%rbp)
check.68F:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block check<E46A> (ep<D276>,_t<D26B>) */
	movq	$10, -8(%rsi)
	movq	$1, (%rsi)
	movq	%rsi, %rcx
	addq	$16, %rsi
	movq	%r11, %r13
	movq	%rsi, 128(%r13)
	movq	%rax, %r12
	movq	%rdi, %rbx
	movq	%r8, %rdx
	movq	%rdx, -64(%rbp)
	movq	%r9, %r10
	movq	%r10, -72(%rbp)
	movq	%r11, %r15
	movq	%r11, %r10
	movq	%r10, %rdi
	movq	%rcx, %rsi
	call	_PromoteObj
	movq	%rax, %rcx
	movq	%r12, %rax
	movq	%rbx, %rdi
	movq	-64(%rbp), %r8
	movq	-72(%rbp), %r9
	movq	%r15, %r11
	movq	128(%r13), %rsi
	movq	$12, -8(%rsi)
	movq	128(%r14), %r13
	movq	%r13, (%rsi)
	movq	%rsi, %r12
	addq	$16, %rsi
	movq	$20, -8(%rsi)
	movq	%r12, (%rsi)
	movabsq	$spawnFn.2EB, %rdx
	movq	%rdx, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	$18, -8(%rsi)
	movq	$1, (%rsi)
	movabsq	$removeFn.2F0, %rbx
	movq	%rbx, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	$520093485, -8(%rsi)
	movabsq	$letJoinK.68B, %r12
	movq	%r12, (%rsi)
	movq	8(%r14), %r13
	movq	%r13, 8(%rsi)
	movq	16(%r14), %r15
	movq	%r15, 16(%rsi)
	movq	24(%r14), %rdx
	movq	%rdx, 24(%rsi)
	movq	32(%r14), %r12
	movq	%r12, 32(%rsi)
	movq	40(%r14), %r13
	movq	%r13, 40(%rsi)
	movq	48(%r14), %r15
	movq	%r15, 48(%rsi)
	movq	56(%r14), %rdx
	movq	%rdx, 56(%rsi)
	movq	64(%r14), %r12
	movq	%r12, 64(%rsi)
	movq	72(%r14), %r13
	movq	%r13, 72(%rsi)
	movq	80(%r14), %r15
	movq	%r15, 80(%rsi)
	movq	88(%r14), %rdx
	movq	%rdx, 88(%rsi)
	movq	96(%r14), %r12
	movq	%r12, 96(%rsi)
	movq	104(%r14), %r13
	movq	%r13, 104(%rsi)
	movq	112(%r14), %r15
	movq	%r15, 112(%rsi)
	movq	120(%r14), %rdx
	movq	%rdx, 120(%rsi)
	movq	128(%r14), %r12
	movq	%r12, 128(%rsi)
	movq	-56(%rbp), %r12
	movq	%r12, 136(%rsi)
	movq	%rcx, 144(%rsi)
	movq	%r10, 152(%rsi)
	movq	%rbx, 160(%rsi)
	movq	$1, 168(%rsi)
	movq	%rsi, %r10
	addq	$184, %rsi
	movq	72(%r14), %r13
	movq	(%r13), %rdi
	movq	%r10, %r8
	movq	16(%r14), %r9
	jmp	migrate_D_to_D_top_D_level_D_sched.15C
doGC693:
	movq	$133, -8(%rsi)
	movq	%r14, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	%r14, %rdi
	movabsq	$retGC691, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.695:
	movq	%r8, %r13
	movq	%rdi, %r12
	jmp	gcTest697
	/* live= GP={%r12} spilled= GP={%r~1}  */
retGC696:
	movq	8(%rdi), %r13
	movq	(%rdi), %r12
gcTest697:
	movq	%r11, %r14
	movq	448(%r14), %r15
	subq	%rsi, %r15
	jle	doGC698
	movq	%r13, -80(%rbp)
check.694:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block check<E46D> (ep<D148>,cntArr<D13D>) */
	movq	$18, -8(%rsi)
	movq	$1, (%rsi)
	movabsq	$new.27D, %r14
	movq	%r14, 8(%rsi)
	movq	%rsi, %rcx
	movq	%rcx, -64(%rbp)
	addq	$24, %rsi
	movq	$18, -8(%rsi)
	movq	$1, (%rsi)
	movabsq	$to_D_list_D_from_D_atomic.299, %r15
	movq	%r15, 8(%rsi)
	movq	%rsi, %rdx
	movq	%rdx, -72(%rbp)
	addq	$24, %rsi
	movq	$28, -8(%rsi)
	movq	24(%r12), %rdx
	movq	%rdx, (%rsi)
	movq	32(%r12), %rbx
	movq	%rbx, 8(%rsi)
	movq	-72(%rbp), %rbx
	movq	%rbx, 16(%rsi)
	movq	%rsi, %rcx
	addq	$32, %rsi
	movq	$133, -8(%rsi)
	movq	%rcx, (%rsi)
	movabsq	$mug_D_from_D_atomic.2D3, %r10
	movq	%r10, 8(%rsi)
	movq	%rsi, %r10
	movq	%r10, -56(%rbp)
	addq	$24, %rsi
	movq	$775, -8(%rsi)
	movl	$0, (%rsi)
	movq	$1, 8(%rsi)
	movq	$1, 16(%rsi)
	movq	%rsi, %r10
	addq	$32, %rsi
	movq	%r11, %rbx
	movq	%rsi, 128(%rbx)
	movq	%rax, %r13
	movq	%r13, -96(%rbp)
	movq	%rdi, %r15
	movq	%r8, %r14
	movq	%r14, -88(%rbp)
	movq	%r9, %r14
	movq	%r11, %r13
	movq	%r11, %rcx
	movq	%rcx, %rdi
	movq	%r10, %rsi
	call	_PromoteObj
	movq	%rax, %rcx
	movq	-96(%rbp), %rax
	movq	%r15, %rdi
	movq	-88(%rbp), %r8
	movq	%r14, %r9
	movq	%r13, %r11
	movq	128(%rbx), %rsi
	movq	$16776995, -8(%rsi)
	movabsq	$letJoinK.690, %rbx
	movq	%rbx, (%rsi)
	movq	8(%r12), %r10
	movq	%r10, 8(%rsi)
	movq	16(%r12), %rdx
	movq	%rdx, 16(%rsi)
	movq	24(%r12), %rbx
	movq	%rbx, 24(%rsi)
	movq	32(%r12), %r10
	movq	%r10, 32(%rsi)
	movq	40(%r12), %r13
	movq	%r13, 40(%rsi)
	movq	48(%r12), %r14
	movq	%r14, 48(%rsi)
	movq	56(%r12), %r15
	movq	%r15, 56(%rsi)
	movq	64(%r12), %rdx
	movq	%rdx, 64(%rsi)
	movq	72(%r12), %rbx
	movq	%rbx, 72(%rsi)
	movq	80(%r12), %r10
	movq	%r10, 80(%rsi)
	movq	-80(%rbp), %r15
	movq	%r15, 88(%rsi)
	movq	-64(%rbp), %rdx
	movq	%rdx, 96(%rsi)
	movq	-72(%rbp), %rbx
	movq	%rbx, 104(%rsi)
	movq	-56(%rbp), %r10
	movq	%r10, 112(%rsi)
	movq	24(%r11), %r12
	movq	%r12, 120(%rsi)
	movq	%rcx, 128(%rsi)
	movq	%rsi, %rdx
	addq	$144, %rsi
	movq	-64(%rbp), %r12
	movq	(%r12), %rdi
	movq	-80(%rbp), %r13
	movq	(%r13), %r8
	movq	%rdx, %r9
	jmp	new.27D
doGC698:
	movq	$20, -8(%rsi)
	movq	%r12, (%rsi)
	movq	%r13, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	%r13, %rdi
	movabsq	$retGC696, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.69A:
	movq	%r8, %r13
	movq	%rdi, %r12
	jmp	gcTest69C
	/* live= GP={%r12} spilled= GP={%r~1}  */
retGC69B:
	movq	8(%rdi), %r13
	movq	(%rdi), %r12
gcTest69C:
	movq	%r11, %rcx
	movq	448(%rcx), %rdx
	subq	%rsi, %rdx
	jle	doGC69D
	movq	%r13, -56(%rbp)
check.699:
	/* Liveout:  GP={%r12 %r10 %r9 %r8 %rdi}  */
	/* block check<E470> (ep<D127>,nVProcLg<D11B>) */
	movq	%rax, %rbx
	movq	%rbx, -72(%rbp)
	movq	%rdi, %r14
	movq	%r8, %r15
	movq	%r9, %rbx
	movq	%rsi, %r13
	movq	%r11, %r10
	movq	%r10, -64(%rbp)
	call	_GetNumVProcs
	movq	%rax, %rcx
	movq	-72(%rbp), %rax
	movq	%r14, %rdi
	movq	%r15, %r8
	movq	%rbx, %r9
	movq	%r13, %rsi
	movq	-64(%rbp), %r11
	movq	$10, -8(%rsi)
	movl	%ecx, (%rsi)
	movq	%rsi, %rcx
	addq	$16, %rsi
	movq	$12, -8(%rsi)
	movq	-56(%rbp), %r13
	movq	%r13, (%rsi)
	movq	%rsi, %rbx
	addq	$16, %rsi
	movq	$133, -8(%rsi)
	movq	%rbx, (%rsi)
	movabsq	$anon.278, %r10
	movq	%r10, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	$261911, -8(%rsi)
	movabsq	$letJoinK.695, %r13
	movq	%r13, (%rsi)
	movq	8(%r12), %r14
	movq	%r14, 8(%rsi)
	movq	16(%r12), %r15
	movq	%r15, 16(%rsi)
	movq	32(%r12), %rdx
	movq	%rdx, 24(%rsi)
	movq	40(%r12), %r10
	movq	%r10, 32(%rsi)
	movq	48(%r12), %r13
	movq	%r13, 40(%rsi)
	movq	56(%r12), %r14
	movq	%r14, 48(%rsi)
	movq	64(%r12), %r15
	movq	%r15, 56(%rsi)
	movq	72(%r12), %rdx
	movq	%rdx, 64(%rsi)
	movq	80(%r12), %r10
	movq	%r10, 72(%rsi)
	movq	88(%r12), %r13
	movq	%r13, 80(%rsi)
	movq	%rsi, %r10
	addq	$96, %rsi
	movq	24(%r12), %r14
	movq	(%r14), %rdi
	movq	%rcx, %r8
	movq	%rbx, %r9
	movq	16(%r12), %r12
	jmp	tabulate.AD
doGC69D:
	movq	$20, -8(%rsi)
	movq	%r12, (%rsi)
	movq	%r13, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	%r15, %rdi
	movabsq	$retGC69B, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.69F:
	movq	%r8, %r10
	movq	%rdi, %rbx
	jmp	gcTest6A1
	/* live= GP={%r10 %rbx} spilled=  */
retGC6A0:
	movq	8(%rdi), %r10
	movq	(%rdi), %rbx
gcTest6A1:
	movq	%r11, %r15
	movq	448(%r15), %rcx
	subq	%rsi, %rcx
	jle	doGC6A2
check.69E:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block check<E473> (ep<D10D>,act<D0FF>) */
	movq	$20, -8(%rsi)
	movq	%r10, (%rsi)
	movq	104(%rbx), %r13
	movq	32(%r13), %r14
	movq	%r14, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	104(%rbx), %r15
	movq	%r12, 32(%r15)
	movq	104(%rbx), %rcx
	movq	$1, (%rcx)
	movq	%rax, %rdx
	movq	%rdx, -56(%rbp)
	movq	%rdi, %r12
	movq	%r8, %r13
	movq	%r9, %r14
	movq	%rsi, %r15
	movq	%r11, %r10
	movq	%r10, -64(%rbp)
	call	_GetNumVProcs
	movq	%rax, %rcx
	movq	-56(%rbp), %rax
	movq	%r12, %rdi
	movq	%r13, %r8
	movq	%r14, %r9
	movq	%r15, %rsi
	movq	-64(%rbp), %r11
	movq	$10, -8(%rsi)
	movl	%ecx, %r10d
	movq	%r10, (%rsi)
	movq	%rsi, %r10
	addq	$16, %rsi
	movq	$524057, -8(%rsi)
	movabsq	$letJoinK.69A, %r13
	movq	%r13, (%rsi)
	movq	8(%rbx), %r14
	movq	%r14, 8(%rsi)
	movq	16(%rbx), %r15
	movq	%r15, 16(%rsi)
	movq	32(%rbx), %rcx
	movq	%rcx, 24(%rsi)
	movq	40(%rbx), %rdx
	movq	%rdx, 32(%rsi)
	movq	48(%rbx), %r12
	movq	%r12, 40(%rsi)
	movq	56(%rbx), %r13
	movq	%r13, 48(%rsi)
	movq	64(%rbx), %r14
	movq	%r14, 56(%rsi)
	movq	72(%rbx), %r15
	movq	%r15, 64(%rsi)
	movq	80(%rbx), %rcx
	movq	%rcx, 72(%rsi)
	movq	88(%rbx), %rdx
	movq	%rdx, 80(%rsi)
	movq	96(%rbx), %r12
	movq	%r12, 88(%rsi)
	movq	%rsi, %r12
	addq	$104, %rsi
	movq	24(%rbx), %r13
	movq	(%r13), %rdi
	movq	%r10, %r8
	movq	%r12, %r9
	jmp	ceilingLg.8C
doGC6A2:
	movq	$20, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%r10, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	%r14, %rdi
	movabsq	$retGC6A0, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.272:
	movq	%rdi, %rcx
	jmp	gcTest6A5
	/* live= GP={%rcx} spilled=  */
retGC6A4:
	movq	(%rdi), %rcx
gcTest6A5:
	movq	%r11, %r13
	movq	448(%r13), %rdx
	subq	%rsi, %rdx
	jle	doGC6A6
check.6A3:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block check<E475> (ep<D0FC>) */
	movq	$1048349, -8(%rsi)
	movabsq	$letJoinK.69F, %r10
	movq	%r10, (%rsi)
	movq	8(%rcx), %r12
	movq	%r12, 8(%rsi)
	movq	16(%rcx), %r13
	movq	%r13, 16(%rsi)
	movq	24(%rcx), %r14
	movq	%r14, 24(%rsi)
	movq	32(%rcx), %r15
	movq	%r15, 32(%rsi)
	movq	40(%rcx), %rdx
	movq	%rdx, 40(%rsi)
	movq	48(%rcx), %rbx
	movq	%rbx, 48(%rsi)
	movq	56(%rcx), %r10
	movq	%r10, 56(%rsi)
	movq	64(%rcx), %r12
	movq	%r12, 64(%rsi)
	movq	72(%rcx), %r13
	movq	%r13, 72(%rsi)
	movq	80(%rcx), %r14
	movq	%r14, 80(%rsi)
	movq	88(%rcx), %r15
	movq	%r15, 88(%rsi)
	movq	96(%rcx), %rdx
	movq	%rdx, 96(%rsi)
	movq	112(%rcx), %rbx
	movq	%rbx, 104(%rsi)
	movq	%rsi, %rbx
	addq	$120, %rsi
	movq	104(%rcx), %r10
	movq	(%r10), %rdi
	movq	112(%rcx), %r8
	movq	%rbx, %r9
	movq	16(%rcx), %r10
	jmp	mkSwitch.241
doGC6A6:
	movq	$12, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%rsi, %r12
	addq	$16, %rsi
	movq	%r12, %rdi
	movabsq	$retGC6A4, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.6A8:
	movq	%rdi, %rcx
	jmp	gcTest6AA
	/* live= GP={%rcx} spilled=  */
retGC6A9:
	movq	(%rdi), %rcx
gcTest6AA:
	movq	%r11, %r12
	movq	448(%r12), %rdx
	subq	%rsi, %rdx
	jle	doGC6AB
check.6A7:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block check<E477> (ep<E19F>) */
	movq	8(%rcx), %rbx
	movq	(%rbx), %rdi
	movq	24(%rcx), %r8
	movq	16(%rcx), %r9
	jmp	lp.269
doGC6AB:
	movq	$12, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%rsi, %r10
	addq	$16, %rsi
	movq	%r10, %rdi
	movabsq	$retGC6A9, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.6B0:
	movq	%r8, %rbx
	movq	%rdi, %rdx
	jmp	gcTest6B2
	/* live= spilled= GP={%r~1 %r~1}  */
retGC6B1:
	movq	8(%rdi), %rbx
	movq	(%rdi), %rdx
gcTest6B2:
	movq	%r11, %rcx
	movq	448(%rcx), %r10
	subq	%rsi, %r10
	jle	doGC6B3
	movq	%rbx, -88(%rbp)
	movq	%rdx, -120(%rbp)
check.6AC:
	/* block check<E47A> (ep<CC2A>,cntArr<CC21>) */
	movq	$18, -8(%rsi)
	movq	$1, (%rsi)
	movabsq	$new.FB, %r10
	movq	%r10, 8(%rsi)
	movq	%rsi, %r12
	movq	%r12, -56(%rbp)
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movq	-120(%rbp), %r13
	movq	56(%r13), %r13
	movq	%r13, (%rsi)
	movq	-120(%rbp), %r14
	movq	64(%r14), %r14
	movq	%r14, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	$133, -8(%rsi)
	movq	%r12, (%rsi)
	movabsq	$wrap.144, %r15
	movq	%r15, 8(%rsi)
	movq	%rsi, %r15
	movq	%r15, -64(%rbp)
	addq	$24, %rsi
	movq	$18, -8(%rsi)
	movq	$1, (%rsi)
	movabsq	$migrate_D_to_D_top_D_level_D_sched.15C, %rcx
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rcx
	movq	%rcx, -72(%rbp)
	addq	$24, %rsi
	movq	$18, -8(%rsi)
	movq	$1, (%rsi)
	movabsq	$current_D_work_D_group.169, %rdx
	movq	%rdx, 8(%rsi)
	movq	%rsi, %rdx
	movq	%rdx, -80(%rbp)
	addq	$24, %rsi
	movq	$12, -8(%rsi)
	movq	-120(%rbp), %rbx
	movq	40(%rbx), %r10
	movq	%r10, (%rsi)
	movq	%rsi, %rbx
	addq	$16, %rsi
	movq	$133, -8(%rsi)
	movq	%rbx, (%rsi)
	movabsq	$mkSwitch.241, %r12
	movq	%r12, 8(%rsi)
	movq	%rsi, %r10
	movq	%r10, -96(%rbp)
	addq	$24, %rsi
	movq	$3, (%r11)
	movq	%rax, %r13
	movq	%rdi, %r12
	movq	%r12, -104(%rbp)
	movq	%r8, %r14
	movq	%r9, %r15
	movq	%rsi, %rbx
	movq	%r11, %r12
	call	_GetNumVProcs
	movq	%rax, %rcx
	movq	%r13, %rax
	movq	-104(%rbp), %rdi
	movq	%r14, %r8
	movq	%r15, %r9
	movq	%rbx, %rsi
	movq	%r12, %r11
	movq	$10, -8(%rsi)
	movl	%ecx, (%rsi)
	movq	%rsi, %rcx
	addq	$16, %rsi
	movq	%r11, %r15
	movq	%rsi, 128(%r15)
	movq	%rax, %r13
	movq	%r13, -104(%rbp)
	movq	%rdi, %r14
	movq	%r8, %r13
	movq	%r9, %r12
	movq	%r11, %rbx
	movq	%r11, %r10
	movq	%r10, %rdi
	movq	%rcx, %rsi
	call	_PromoteObj
	movq	%rax, %rcx
	movq	-104(%rbp), %rax
	movq	%r14, %rdi
	movq	%r13, %r8
	movq	%r12, %r9
	movq	%rbx, %r11
	movq	128(%r15), %rsi
	movq	$20, -8(%rsi)
	movq	-96(%rbp), %r14
	movq	%r14, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	movq	$133, -8(%rsi)
	movq	%rdx, (%rsi)
	movabsq	$initVPFields.264, %rbx
	movq	%rbx, 8(%rsi)
	movq	%rsi, %r15
	movq	%r15, -112(%rbp)
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movq	-120(%rbp), %rcx
	movq	16(%rcx), %r12
	movq	%r12, (%rsi)
	movq	-112(%rbp), %rdx
	movq	%rdx, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	$133, -8(%rsi)
	movq	%r10, (%rsi)
	movabsq	$lp.269, %r13
	movq	%r13, 8(%rsi)
	movq	%rsi, %rbx
	movq	%rbx, -104(%rbp)
	addq	$24, %rsi
	movq	%r11, %rbx
	movq	%rsi, 128(%rbx)
	movq	%rax, %r12
	movq	%rdi, %r13
	movq	%r8, %r14
	movq	%r9, %r15
	movq	%r11, %r10
	movq	%r10, -128(%rbp)
	movq	%r11, %rcx
	movq	%rcx, %rdi
	call	_ListVProcs
	movq	%rax, %rcx
	movq	%r12, %rax
	movq	%r13, %rdi
	movq	%r14, %r8
	movq	%r15, %r9
	movq	-128(%rbp), %r11
	movq	128(%rbx), %rsi
	movq	$2096927, -8(%rsi)
	movabsq	$letJoinK.272, %rbx
	movq	%rbx, (%rsi)
	movq	-120(%rbp), %r12
	movq	8(%r12), %r10
	movq	%r10, 8(%rsi)
	movq	-120(%rbp), %r13
	movq	16(%r13), %r12
	movq	%r12, 16(%rsi)
	movq	-120(%rbp), %r14
	movq	24(%r14), %r13
	movq	%r13, 24(%rsi)
	movq	-120(%rbp), %r15
	movq	32(%r15), %r14
	movq	%r14, 32(%rsi)
	movq	-120(%rbp), %rdx
	movq	40(%rdx), %r15
	movq	%r15, 40(%rsi)
	movq	-120(%rbp), %rbx
	movq	48(%rbx), %rdx
	movq	%rdx, 48(%rsi)
	movq	-120(%rbp), %r10
	movq	56(%r10), %rbx
	movq	%rbx, 56(%rsi)
	movq	-88(%rbp), %r12
	movq	%r12, 64(%rsi)
	movq	-56(%rbp), %r13
	movq	%r13, 72(%rsi)
	movq	-64(%rbp), %r14
	movq	%r14, 80(%rsi)
	movq	-72(%rbp), %r15
	movq	%r15, 88(%rsi)
	movq	-80(%rbp), %rdx
	movq	%rdx, 96(%rsi)
	movq	-96(%rbp), %rbx
	movq	%rbx, 104(%rsi)
	movq	%r11, 112(%rsi)
	movq	%rsi, %rdx
	addq	$128, %rsi
	cmpq	$1, %rcx
	je	L6B4
L_true6AD:
then.6AF:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block then<E19A> (ep<E195>,initVPFields<E199>,lp<E198>,vps<E197>,letJoinK<E196>) */
	movq	$1801, -8(%rsi)
	movabsq	$letJoinK.6A8, %r12
	movq	%r12, (%rsi)
	movq	-104(%rbp), %r10
	movq	%r10, 8(%rsi)
	movq	%rdx, 16(%rsi)
	movq	8(%rcx), %r13
	movq	%r13, 24(%rsi)
	movq	%rsi, %r10
	addq	$40, %rsi
	movq	-112(%rbp), %r12
	movq	(%r12), %rdi
	movq	(%rcx), %r14
	movq	(%r14), %r8
	movq	%r10, %r9
	movq	-120(%rbp), %r13
	movq	16(%r13), %r10
	jmp	initVPFields.264
doGC6B3:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rbx, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	%r15, %rdi
	movabsq	$retGC6B1, %r8
	jmp	_ASM_InvokeGC
L6B4:
else.6AE:
	/* Liveout:  GP={%rdi}  */
	/* block else<E1AC> (letJoinK<E1AB>) */
	movq	%rdx, %rdi
	jmp	letJoinK.272
	.text
letJoinK.6B6:
	movq	%r8, %r10
	movq	%rdi, %rbx
	jmp	gcTest6B8
	/* live= GP={%rbx} spilled= GP={%r~1}  */
retGC6B7:
	movq	8(%rdi), %r10
	movq	(%rdi), %rbx
gcTest6B8:
	movq	%r11, %r15
	movq	448(%r15), %rcx
	subq	%rsi, %rcx
	jle	doGC6B9
	movq	%r10, -56(%rbp)
check.6B5:
	/* Liveout:  GP={%r12 %r10 %r9 %r8 %rdi}  */
	/* block check<E47D> (ep<CC0B>,nVProcLg<CC0A>) */
	movq	%rax, %rdx
	movq	%rdx, -72(%rbp)
	movq	%rdi, %r12
	movq	%r8, %r13
	movq	%r9, %r14
	movq	%rsi, %r15
	movq	%r11, %r10
	movq	%r10, -64(%rbp)
	call	_GetNumVProcs
	movq	%rax, %rcx
	movq	-72(%rbp), %rax
	movq	%r12, %rdi
	movq	%r13, %r8
	movq	%r14, %r9
	movq	%r15, %rsi
	movq	-64(%rbp), %r11
	movq	$10, -8(%rsi)
	movl	%ecx, (%rsi)
	movq	%rsi, %rcx
	addq	$16, %rsi
	movq	$12, -8(%rsi)
	movq	-56(%rbp), %r12
	movq	%r12, (%rsi)
	movq	%rsi, %r10
	addq	$16, %rsi
	movq	$133, -8(%rsi)
	movq	%r10, (%rsi)
	movabsq	$anon.F6, %r12
	movq	%r12, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	$65299, -8(%rsi)
	movabsq	$letJoinK.6B0, %r13
	movq	%r13, (%rsi)
	movq	8(%rbx), %r14
	movq	%r14, 8(%rsi)
	movq	16(%rbx), %r15
	movq	%r15, 16(%rsi)
	movq	24(%rbx), %rdx
	movq	%rdx, 24(%rsi)
	movq	32(%rbx), %r13
	movq	%r13, 32(%rsi)
	movq	40(%rbx), %r14
	movq	%r14, 40(%rsi)
	movq	48(%rbx), %r15
	movq	%r15, 48(%rsi)
	movq	56(%rbx), %rdx
	movq	%rdx, 56(%rsi)
	movq	64(%rbx), %r10
	movq	%r10, 64(%rsi)
	movq	%rsi, %r10
	addq	$80, %rsi
	movq	32(%rbx), %r13
	movq	(%r13), %rdi
	movq	%rcx, %r8
	movq	%r12, %r9
	movq	16(%rbx), %r12
	jmp	tabulate.AD
doGC6B9:
	movq	$20, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%r10, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	%r14, %rdi
	movabsq	$retGC6B7, %r8
	jmp	_ASM_InvokeGC
	.text
main.6BB:
_Main_init:
_mantEntry:
	movq	%r9, %r13
	movq	%r8, %r12
	movq	%rax, %r10
	movq	%rdi, %rbx
	jmp	gcTest6BF
	/* live= GP={%r10 %rbx} spilled= GP={%r~1 %r~1}  */
retGC6BE:
	movq	24(%rdi), %r13
	movq	16(%rdi), %r12
	movq	8(%rdi), %r10
	movq	(%rdi), %rbx
gcTest6BF:
	movq	%r11, %rcx
	movq	448(%rcx), %rdx
	subq	%rsi, %rdx
	jg	L6C1
doGC6C0:
	movq	$36, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%r10, 8(%rsi)
	movq	%r12, 16(%rsi)
	movq	%r13, 24(%rsi)
	movq	%rsi, %r15
	addq	$40, %rsi
	movq	%r15, %rdi
	movabsq	$retGC6BE, %r8
	jmp	_ASM_InvokeGC
L6C1:
	movq	%r13, -88(%rbp)
	movq	%r12, -104(%rbp)
check.6BA:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block check<E482> (dummyEP<CA87>,argFormalWrap<E1B4>,retK<CA89>,_exh<CA8A>) */
	movq	$18, -8(%rsi)
	movq	$1, (%rsi)
	movabsq	$ceilingLg.8C, %r14
	movq	%r14, 8(%rsi)
	movq	%rsi, %rbx
	movq	%rbx, -96(%rbp)
	addq	$24, %rsi
	movq	$18, -8(%rsi)
	movq	$1, (%rsi)
	movabsq	$tabulate.AD, %r15
	movq	%r15, 8(%rsi)
	movq	%rsi, %r10
	movq	%r10, -64(%rbp)
	addq	$24, %rsi
	movq	$18, -8(%rsi)
	movq	$1, (%rsi)
	movabsq	$app_D_w_uncurried.C, %rcx
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r12
	movq	%r12, -72(%rbp)
	addq	$24, %rsi
	movq	$18, -8(%rsi)
	movq	$1, (%rsi)
	movabsq	$filter_D_w_uncurried.E1, %rdx
	movq	%rdx, 8(%rsi)
	movq	%rsi, %r13
	movq	%r13, -80(%rbp)
	addq	$24, %rsi
	movq	$18, -8(%rsi)
	movq	$1, (%rsi)
	movabsq	$get_D_ite.EB, %rbx
	movq	%rbx, 8(%rsi)
	movq	%rsi, %r14
	movq	%r14, -112(%rbp)
	addq	$24, %rsi
	movq	$18, -8(%rsi)
	movq	$1, (%rsi)
	movabsq	$set_D_ite.F1, %r10
	movq	%r10, 8(%rsi)
	movq	%rsi, %r15
	movq	%r15, -120(%rbp)
	addq	$24, %rsi
	movq	%rax, %r12
	movq	%rdi, %rcx
	movq	%rcx, -56(%rbp)
	movq	%r8, %r13
	movq	%r9, %r14
	movq	%rsi, %r15
	movq	%r11, %rbx
	call	_GetNumVProcs
	movq	%rax, %r10
	movq	%r12, %rax
	movq	-56(%rbp), %rdi
	movq	%r13, %r8
	movq	%r14, %r9
	movq	%r15, %rsi
	movq	%rbx, %r11
	movq	$10, -8(%rsi)
	movl	%r10d, %r12d
	movq	%r12, (%rsi)
	movq	%rsi, %r15
	addq	$16, %rsi
	movq	$65299, -8(%rsi)
	movabsq	$letJoinK.6B6, %r14
	movq	%r14, (%rsi)
	movq	-104(%rbp), %rdx
	movq	%rdx, 8(%rsi)
	movq	-88(%rbp), %rbx
	movq	%rbx, 16(%rsi)
	movq	-96(%rbp), %r10
	movq	%r10, 24(%rsi)
	movq	-64(%rbp), %r12
	movq	%r12, 32(%rsi)
	movq	-72(%rbp), %r13
	movq	%r13, 40(%rsi)
	movq	-80(%rbp), %rcx
	movq	%rcx, 48(%rsi)
	movq	-112(%rbp), %rdx
	movq	%rdx, 56(%rsi)
	movq	-120(%rbp), %rbx
	movq	%rbx, 64(%rsi)
	movq	%rsi, %r13
	addq	$80, %rsi
	movq	-96(%rbp), %r10
	movq	(%r10), %rdi
	movq	%r15, %r8
	movq	%r13, %r9
	jmp	ceilingLg.8C
	.globl	_mantEntry
	.globl	_Main_init
	.text
	.const_data
	.globl	_mantMagic
_mantMagic:
	.long	38464718
	.globl	_SequentialFlag
_SequentialFlag:
	.long	0
	.align	8
str5F3:
	.asciz	""
	.align	8
strE9:
	.asciz	"FLS.ite: nonexistant implicit threading environment"
	.align	8
str5A3:
	.asciz	"WorkStealing.@designated-worker: impossible"
	.align	8
str168:
	.asciz	"ImplicitThread.@current-work-group: empty work-group stack"
	.align	8
str64A:
	.asciz	"Array64.@array: negative size"
	.align	8
str600:
	.asciz	"\n"
	.align	8
str5F8:
	.asciz	"-"
	.align	8
str603:
	.asciz	".00"
	.align	8
str602:
	.asciz	".0"
	.align	8
str5FC:
	.asciz	"."
	.align	8
tagEA:
	.asciz	"Fail"
	.align	8
tag138:
	.asciz	"Match"
