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
	/* block else<ECB7> (ep<ECB4>,qTl<ECB6>,qHd<ECB5>) */
	movq	8(%r13), %r14
	cmpq	%r15, %r13
	je	L_trueC
letJoinK.9:
	/* flushLoads */
	/* block letJoinK<ECBD> (ep<ECBA>,qHd<ECBC>,qNext<ECBB>) */
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
	/* block then<ECB2> (ep<ECB1>) */
	movq	$1, %r10
letJoinK.A:
	/* flushLoads */
	/* block letJoinK<EC90> (ep<EC8E>,elt<EC8F>) */
	movq	8(%r12), %r15
	movl	$0, (%r15)
	cmpq	$1, %r10
	jne	L_true10
else.11:
	/* Liveout:  GP={%rdi}  */
	/* block else<ECAE> (ep<ECAD>) */
	movq	32(%r12), %rdi
	jmp	dispatch.13
L_true10:
then.12:
	/* Liveout:  GP={%rax %rdi}  */
	/* block then<EC96> (ep<EC94>,elt<EC95>) */
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
	/* block then<ECC4> (ep<ECC1>,qHd<ECC3>,qNext<ECC2>) */
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
	/* block else<ED37> (ep<ED34>,qHd<ED36>,qTl<ED35>) */
	movq	24(%rdx), %r10
	movq	%r10, 8(%rbx)
	jmp	letJoinK.15
L_true1A:
letJoinK.15:
	/* flushLoads */
	/* block letJoinK<ED21> (ep<ED1F>,qHd<ED20>) */
	movq	8(%rdx), %r13
	movq	24(%rdx), %r14
	movq	%r14, 16(%r13)
	cmpq	$1, %rcx
	jne	letJoinK.17
L_true16:
then.18:
	/* flushLoads */
	/* block then<ED2C> (ep<ED2B>) */
	movq	8(%rdx), %r15
	movq	24(%rdx), %rcx
	movq	%rcx, 8(%r15)
letJoinK.17:
	/* Liveout:  GP={%rdi}  */
	/* flushLoads */
	/* block letJoinK<ED26> (ep<ED25>) */
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
	/* block else<EDD6> (ep<EDD3>,qHd<EDD5>,qTl<EDD4>) */
	movq	32(%rdx), %r14
	movq	%r14, 8(%rbx)
	jmp	letJoinK.1D
L_true28:
letJoinK.1D:
	/* flushLoads */
	/* block letJoinK<EDA0> (ep<ED9E>,qHd<ED9F>) */
	movq	8(%rdx), %rbx
	movq	32(%rdx), %r10
	movq	%r10, 16(%rbx)
	cmpq	$1, %rcx
	je	L_true1E
	jmp	letJoinK.1F
L_true1E:
then.20:
	/* flushLoads */
	/* block then<EDCB> (ep<EDCA>) */
	movq	8(%rdx), %r12
	movq	32(%rdx), %r13
	movq	%r13, 8(%r12)
letJoinK.1F:
	/* flushLoads */
	/* block letJoinK<EDA5> (ep<EDA4>) */
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
	/* block case<EDB3> (ep<EDB1>,letJoinK<EDB2>) */
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
	/* block case<EDB9> (ep<EDB7>,letJoinK<EDB8>) */
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
	/* block case<EE37> (ep<EE35>,letJoinK<EE36>) */
	movq	16(%rdx), %r12
	movq	$1, 8(%r12)
	movq	%rcx, %rdi
	jmp	letJoinK.2D
S_case30:
case.2A:
	/* Liveout:  GP={%rax %rdi}  */
	/* block case<EE3D> (ep<EE3B>,letJoinK<EE3C>) */
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
letJoinK.32:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest34
	/* live= GP={%rcx %rdx} spilled=  */
retGC33:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest34:
	movq	%r11, %r12
	subq	%rsi, %r12
	jle	doGC35
check.31:
	/* Liveout:  GP={%r12 %r10 %r9 %r8 %rdi} FP={%xmm3 %xmm2}  */
	/* block check<10AC3> (ep<E82B>,_t<E829>) */
	movq	$20, -8(%rsi)
	movq	32(%rdx), %r10
	movq	%r10, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	8(%rdx), %r12
	movq	8(%r12), %r15
	movq	(%r12), %rdi
	movq	(%rbx), %r8
	movq	(%rbx), %r14
	movq	(%r14), %r13
	/* %xmm2.d := mem.d[(mem.64[(mem.64[%r702.64 +.64 0]) +.64 0]) +.64 0] */
	movsd	 (%r13), %xmm2
	movq	8(%rbx), %r9
	movq	8(%rbx), %rbx
	movq	(%rbx), %rcx
	/* %xmm3.d := mem.d[(mem.64[(mem.64[%r702.64 +.64 8]) +.64 0]) +.64 0] */
	movsd	 (%rcx), %xmm3
	movq	16(%rdx), %r10
	movq	24(%rdx), %r12
	jmp	*%r15
doGC35:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	%r10, %rdi
	movabsq	$retGC33, %r8
	jmp	ASM_InvokeGC
	.text
retGC39:
	movq	32(%rdi), %rcx
	movq	24(%rdi), %rdx
	movq	16(%rdi), %r12
	movq	8(%rdi), %r10
	movq	(%rdi), %rbx
gcTest3A:
	movq	%r11, %r13
	subq	%rsi, %r13
	jle	doGC3B
check.36:
	/* Liveout:  GP={%r12 %r10 %r9 %r8 %rdi}  */
	/* block check<10AC9> (ep<E820>,retK<E824>,_exh<E823>,xs<E822>,acc<E821>) */
	movq	$3851, -8(%rsi)
	movabsq	$letJoinK.32, %r13
	movq	%r13, (%rsi)
	movq	(%rbx), %r14
	movq	%r14, 8(%rsi)
	movq	%r10, 16(%rsi)
	movq	%r12, 24(%rsi)
	movq	(%rdx), %r15
	movq	%r15, 32(%rsi)
	movq	%rsi, %r10
	addq	$48, %rsi
	movq	%rbx, %rdi
	movq	8(%rdx), %r8
	movq	%rcx, %r9
lp.37:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	cmpq	$1, %rdx
	je	L3E
L_true3C:
then.38:
	/* block then<E825> (ep<10AC4>,retK<10AC5>,_exh<10AC6>,xs<10AC7>,acc<10AC8>) */
	jmp	gcTest3A
L3E:
else.3D:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block else<E842> (retK<E841>,acc<E840>) */
	movq	(%r10), %r12
	movq	%r10, %rdi
	movq	%rcx, %r8
	jmp	*%r12
doGC3B:
	movq	$44, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%r10, 8(%rsi)
	movq	%r12, 16(%rsi)
	movq	%rdx, 24(%rsi)
	movq	%rcx, 32(%rsi)
	movq	%rsi, %rcx
	addq	$48, %rsi
	movq	%rcx, %rdi
	movabsq	$retGC39, %r8
	jmp	ASM_InvokeGC
	/* live= GP={%rcx %rdx %r12 %r10 %rbx} spilled=  */
	jmp	retGC39
	.text
foldr_uncurried.40:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest42
	/* live= GP={%r13 %r12 %r14 %r10 %rdx %rbx} spilled=  */
retGC41:
	movq	40(%rdi), %r13
	movq	32(%rdi), %r12
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest42:
	movq	%r11, %r14
	subq	%rsi, %r14
	jle	doGC43
	movq	%r10, %r14
	movq	%rcx, %r10
check.3F:
	/* Liveout:  GP={%r12 %r10 %r9 %r8 %rdi}  */
	/* block check<10AD0> (ep<E80C>,f<E80D>,id<E80E>,xs<E80F>,retK<E810>,_exh<E811>) */
	movq	$12, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rsi, %r15
	addq	$16, %rsi
	movq	$133, -8(%rsi)
	movq	%r15, (%rsi)
	movabsq	$lp.37, %rdx
	movq	%rdx, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	(%rcx), %rdi
	movq	%r14, %r8
	movq	%r10, %r9
	movq	%r12, %r10
	movq	%r13, %r12
	jmp	lp.37
doGC43:
	movq	$52, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%r12, 32(%rsi)
	movq	%r13, 40(%rsi)
	movq	%rsi, %rbx
	addq	$56, %rsi
	movq	%rbx, %rdi
	movabsq	$retGC41, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.46:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest48
	/* live= GP={%rcx %rdx} spilled=  */
retGC47:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest48:
	movq	%r11, %r14
	subq	%rsi, %r14
	jle	doGC49
check.44:
	/* Liveout:  GP={%r12 %r10 %r9 %r8 %rdi}  */
	/* block check<10AD3> (ep<E867>,_t<E864>) */
	movq	$20, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	32(%rdx), %r10
	movq	%r10, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	8(%rdx), %r12
	movq	(%r12), %rdi
	movq	40(%rdx), %r8
	movq	%rbx, %r9
	movq	16(%rdx), %r10
	movq	24(%rdx), %r12
	jmp	lp.45
doGC49:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	%r13, %rdi
	movabsq	$retGC47, %r8
	jmp	ASM_InvokeGC
	.text
lp.4E:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest50
	/* live= GP={%r10 %rcx %rdx %rbx} spilled=  */
retGC4F:
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest50:
	movq	%r11, %r12
	subq	%rsi, %r12
	jle	doGC51
check.4A:
	/* block check<10AD8> (ep<E87C>,ls<E87D>,acc<E87E>,retK<E87F>) */
	cmpq	$1, %rdx
	je	L52
L_true4B:
then.4D:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block then<E888> (ep<E884>,retK<E887>,ls<E886>,acc<E885>) */
	movq	$20, -8(%rsi)
	movq	(%rdx), %r14
	movq	%r14, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	%rbx, %rdi
	movq	8(%rdx), %r8
	movq	%r13, %r9
	jmp	lp.4E
doGC51:
	movq	$36, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %r15
	addq	$40, %rsi
	movq	%r15, %rdi
	movabsq	$retGC4F, %r8
	jmp	ASM_InvokeGC
L52:
else.4C:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block else<E891> (retK<E890>,acc<E88F>) */
	movq	(%r10), %r12
	movq	%r10, %rdi
	movq	%rcx, %r8
	jmp	*%r12
	.text
lp.45:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest58
	/* live= GP={%r12 %r10 %rcx %rdx %rbx} spilled=  */
retGC57:
	movq	32(%rdi), %r12
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest58:
	movq	%r11, %r13
	subq	%rsi, %r13
	jle	doGC59
check.53:
	/* block check<10ADE> (ep<E852>,ls<E853>,acc<E854>,retK<E855>,_exh<E856>) */
	cmpq	$1, %rdx
	je	L5A
L_true54:
then.56:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block then<E860> (ep<E85B>,retK<E85F>,_exh<E85E>,ls<E85D>,acc<E85C>) */
	movq	$133, -8(%rsi)
	movq	%rbx, (%rsi)
	movabsq	$lp.45, %r15
	movq	%r15, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	$7949, -8(%rsi)
	movabsq	$letJoinK.46, %r13
	movq	%r13, (%rsi)
	movq	%r14, 8(%rsi)
	movq	%r10, 16(%rsi)
	movq	%r12, 24(%rsi)
	movq	%rcx, 32(%rsi)
	movq	8(%rdx), %r14
	movq	%r14, 40(%rsi)
	movq	%rsi, %rcx
	addq	$56, %rsi
	movq	(%rbx), %r15
	movq	8(%r15), %rbx
	movq	(%r15), %rdi
	movq	(%rdx), %r8
	movq	%rcx, %r9
	movq	%r12, %r10
	jmp	*%rbx
doGC59:
	movq	$44, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%r12, 32(%rsi)
	movq	%rsi, %r10
	addq	$48, %rsi
	movq	%r10, %rdi
	movabsq	$retGC57, %r8
	jmp	ASM_InvokeGC
L5A:
else.55:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block else<E879> (retK<E878>,acc<E877>) */
	movq	$18, -8(%rsi)
	movq	$1, (%rsi)
	movabsq	$lp.4E, %r13
	movq	%r13, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	(%r12), %rdi
	movq	%rcx, %r8
	movq	$1, %r9
	jmp	lp.4E
	.text
map_uncurried.5C:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest5E
	/* live= GP={%r12 %r10 %rcx %rdx %rbx} spilled=  */
retGC5D:
	movq	32(%rdi), %r12
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest5E:
	movq	%r11, %r13
	subq	%rsi, %r13
	jle	doGC5F
check.5B:
	/* Liveout:  GP={%r12 %r10 %r9 %r8 %rdi}  */
	/* block check<10AE4> (ep<E848>,f<E849>,ls<E84A>,retK<E84B>,_exh<E84C>) */
	movq	$12, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rsi, %r13
	addq	$16, %rsi
	movq	$133, -8(%rsi)
	movq	%r13, (%rsi)
	movabsq	$lp.45, %r15
	movq	%r15, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	(%r14), %rdi
	movq	%rcx, %r8
	movq	$1, %r9
	jmp	lp.45
doGC5F:
	movq	$44, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%r12, 32(%rsi)
	movq	%rsi, %rcx
	addq	$48, %rsi
	movq	%rcx, %rdi
	movabsq	$retGC5D, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.62:
	movq	%r8, %r14
	movq	%rdi, %r15
	jmp	gcTest64
	/* live= GP={%r14 %r15} spilled=  */
retGC63:
	movq	8(%rdi), %r14
	movq	(%rdi), %r15
gcTest64:
	movq	%r11, %rbx
	subq	%rsi, %rbx
	jle	doGC65
check.60:
	/* Liveout:  GP={%r12 %r10 %r9 %r8 %rdi}  */
	/* block check<10AE7> (ep<E8EB>,_t<E8E7>) */
	movq	$-1048576, %rbx
	andq	%rsi, %rbx
	movq	%rsi, 120(%rbx)
	movq	$1, (%rbx)
	movq	%rax, -72(%rbp)
	movq	%rcx, -96(%rbp)
	movq	%rdx, %r12
	movq	%rdi, -56(%rbp)
	movq	%r8, -64(%rbp)
	movq	%r9, -80(%rbp)
	movq	%r10, -88(%rbp)
	movq	%r11, %r13
	movq	$-1048576, %rcx
	andq	%rsi, %rcx
	movq	%rcx, %rdi
	movq	%r14, %rsi
	call	PromoteObj
	movq	%rax, %r14
	movq	-72(%rbp), %rax
	movq	-96(%rbp), %rcx
	movq	%r12, %rdx
	movq	-56(%rbp), %rdi
	movq	-64(%rbp), %r8
	movq	-80(%rbp), %r9
	movq	-88(%rbp), %r10
	movq	%r13, %r11
	movq	120(%rbx), %rsi
	movq	$3, (%rbx)
	movq	40(%r15), %rdx
	movq	(%rdx), %rcx
	movq	16(%r15), %r10
	movl	(%r10), %ebx
	shlq	$3, %rbx
	movq	%r14, (%rcx,%rbx,1)
	movq	16(%r15), %r12
	movl	(%r12), %r13d
	incl	%r13d
	movq	$10, -8(%rsi)
	movl	%r13d, (%rsi)
	movq	%rsi, %r14
	addq	$16, %rsi
	movq	8(%r15), %rcx
	movq	(%rcx), %rdi
	movq	%r14, %r8
	movq	%r13, %r9
	movq	24(%r15), %r10
	movq	32(%r15), %r12
	jmp	tab.61
doGC65:
	movq	$20, -8(%rsi)
	movq	%r15, (%rsi)
	movq	%r14, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	movq	%rdx, %rdi
	movabsq	$retGC63, %r8
	jmp	ASM_InvokeGC
	.text
tab.61:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest6B
	/* live= GP={%r12 %r10 %rcx %rdx %rbx} spilled=  */
retGC6A:
	movq	32(%rdi), %r12
	movq	24(%rdi), %r10
	movl	16(%rdi), %ecx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest6B:
	movq	%r11, %r13
	subq	%rsi, %r13
	jle	doGC6C
check.66:
	/* block check<10AED> (ep<E8D9>,i<E8DA>,_t<E8DB>,retK<E8DC>,_exh<E8DD>) */
	cmpl	8(%rbx), %ecx
	jge	L6D
L_true67:
then.69:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block then<E8E6> (ep<E8E2>,i<E8E5>,retK<E8E4>,_exh<E8E3>) */
	movq	$133, -8(%rsi)
	movq	%rbx, (%rsi)
	movabsq	$tab.61, %r14
	movq	%r14, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	$7949, -8(%rsi)
	movabsq	$letJoinK.62, %rcx
	movq	%rcx, (%rsi)
	movq	%r13, 8(%rsi)
	movq	%rdx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%r12, 32(%rsi)
	movq	16(%rbx), %r10
	movq	%r10, 40(%rsi)
	movq	%rsi, %r15
	addq	$56, %rsi
	movq	(%rbx), %r13
	movq	8(%r13), %r14
	movq	(%r13), %rdi
	movq	%rdx, %r8
	movq	%r15, %r9
	movq	%r12, %r10
	jmp	*%r14
doGC6C:
	movq	$3467, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movl	%ecx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%r12, 32(%rsi)
	movq	%rsi, %r15
	addq	$48, %rsi
	movq	%r15, %rdi
	movabsq	$retGC6A, %r8
	jmp	ASM_InvokeGC
L6D:
else.68:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block else<E903> (ep<E901>,retK<E902>) */
	movq	(%r10), %r14
	movq	%r10, %rdi
	movq	16(%rbx), %r8
	movq	16(%rbx), %r15
	movq	(%r15), %r9
	movq	16(%rbx), %rcx
	movl	8(%rcx), %r10d
	jmp	*%r14
	.text
letJoinK.75:
	movq	%r8, %r15
	movq	%rdi, %r13
	jmp	gcTest77
	/* live= GP={%r15 %r13} spilled=  */
retGC76:
	movq	8(%rdi), %r15
	movq	(%rdi), %r13
gcTest77:
	movq	%r11, %r14
	subq	%rsi, %r14
	jle	doGC78
check.6E:
	/* block check<10AF0> (ep<E8BB>,_t<E8BA>) */
	movq	24(%r13), %r12
	movl	(%r12), %ebx
	cmpl	$0, %ebx
	jge	L79
L_true6F:
then.71:
	/* Liveout:  GP={%rax %rdi}  */
	/* block then<E8C2> (ep<E8C1>) */
	movq	$133, -8(%rsi)
	movabsq	$str73, %r14
	movq	%r14, (%rsi)
	movl	$29, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movabsq	$tag74, %rdx
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	16(%r13), %rbx
	movq	(%rbx), %r10
	movq	%r15, %rax
	movq	%rbx, %rdi
	jmp	*%r10
doGC78:
	movq	$20, -8(%rsi)
	movq	%r13, (%rsi)
	movq	%r15, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	%r12, %rdi
	movabsq	$retGC76, %r8
	jmp	ASM_InvokeGC
L79:
	movq	%rbx, -56(%rbp)
	movq	%r15, -72(%rbp)
	movq	%r13, -64(%rbp)
else.70:
	/* Liveout:  GP={%r12 %r10 %r9 %r8 %rdi}  */
	/* block else<E8CD> (ep<E8CA>,_t<E8CC>,_t<E8CB>) */
	movq	$-1048576, %r13
	andq	%rsi, %r13
	movq	%rsi, 120(%r13)
	movq	$1, (%r13)
	movq	%rax, -104(%rbp)
	movq	%rcx, %r14
	movq	%rdx, -88(%rbp)
	movq	%rdi, -96(%rbp)
	movq	%r8, %r15
	movq	%r9, -80(%rbp)
	movq	%r10, %rbx
	movq	%r11, %r12
	movq	$-1048576, %rcx
	andq	%rsi, %rcx
	movq	%rcx, %rdi
	movq	-72(%rbp), %rsi
	call	PromoteObj
	movq	%rax, -72(%rbp)
	movq	-104(%rbp), %rax
	movq	%r14, %rcx
	movq	-88(%rbp), %rdx
	movq	-96(%rbp), %rdi
	movq	%r15, %r8
	movq	-80(%rbp), %r9
	movq	%rbx, %r10
	movq	%r12, %r11
	movq	120(%r13), %rsi
	movq	$3, (%r13)
	movq	$-1048576, %rbx
	andq	%rsi, %rbx
	movq	$1, (%rbx)
	movq	%rax, -80(%rbp)
	movq	%rcx, -96(%rbp)
	movq	%rdx, -104(%rbp)
	movq	%rsi, -112(%rbp)
	movq	%rdi, -120(%rbp)
	movq	%r8, %r12
	movq	%r9, -88(%rbp)
	movq	%r10, %r13
	movq	%r11, %r14
	movq	$-1048576, %r10
	andq	%rsi, %r10
	movq	%r10, %rdi
	movslq	-56(%rbp), %r15
	movq	%r15, %rsi
	movq	-72(%rbp), %rdx
	call	M_NewArray
	movq	%rax, %r15
	movq	-80(%rbp), %rax
	movq	-96(%rbp), %rcx
	movq	-104(%rbp), %rdx
	movq	-112(%rbp), %rsi
	movq	-120(%rbp), %rdi
	movq	%r12, %r8
	movq	-88(%rbp), %r9
	movq	%r13, %r10
	movq	%r14, %r11
	movq	$3, (%rbx)
	movq	$133, -8(%rsi)
	movq	%r15, (%rsi)
	movq	-56(%rbp), %r15
	movl	%r15d, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	$647, -8(%rsi)
	movq	-64(%rbp), %rdx
	movq	32(%rdx), %rbx
	movq	%rbx, (%rsi)
	movq	-64(%rbp), %rbx
	movl	40(%rbx), %r10d
	movl	%r10d, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%rsi, %rdx
	addq	$32, %rsi
	movq	$133, -8(%rsi)
	movq	%rdx, (%rsi)
	movabsq	$tab.61, %r13
	movq	%r13, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movl	$1, %r14d
	movq	$10, -8(%rsi)
	movl	%r14d, (%rsi)
	movq	%rsi, %r15
	addq	$16, %rsi
	movq	(%r12), %rdi
	movq	%r15, %r8
	movq	%r14, %r9
	movq	-64(%rbp), %r12
	movq	8(%r12), %r10
	movq	16(%r12), %r12
	jmp	tab.61
	.text
tabulate.7E:
	movq	%r13, -56(%rbp)
	movq	%r12, -64(%rbp)
	movq	%r9, %rbx
	movq	%r8, %r14
	movq	%rdi, %r13
	movq	%r10, %r12
	jmp	gcTest80
	/* live= GP={%r12 %rbx %r14 %r13} spilled= GP={%r~1 %r~1}  */
retGC7F:
	movq	40(%rdi), %r12
	movq	%r12, -56(%rbp)
	movq	32(%rdi), %r13
	movq	%r13, -64(%rbp)
	movq	24(%rdi), %r12
	movl	16(%rdi), %ebx
	movq	8(%rdi), %r14
	movq	(%rdi), %r13
gcTest80:
	movq	%r11, %r15
	subq	%rsi, %r15
	jle	doGC81
check.7A:
	/* block check<10AF7> (ep<E89B>,n<E89C>,a0<E89D>,f<E89E>,retK<E89F>,_exh<E8A0>) */
	cmpl	$0, %ebx
	jne	L82
L_true7B:
then.7D:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block then<E8A7> (retK<E8A6>) */
	movq	$-1048576, %r15
	andq	%rsi, %r15
	movq	$1, (%r15)
	movq	%rax, %rbx
	movq	%rcx, %r12
	movq	%rdx, -56(%rbp)
	movq	%rsi, -72(%rbp)
	movq	%rdi, -80(%rbp)
	movq	%r8, -88(%rbp)
	movq	%r9, -96(%rbp)
	movq	%r10, -104(%rbp)
	movq	%r11, %r13
	movq	$-1048576, %rcx
	andq	%rsi, %rcx
	movq	%rcx, %rdi
	xorl	%edx, %edx
	movslq	%edx, %rcx
	movq	%rcx, %rsi
	movq	$1, %rdx
	call	M_NewArray
	movq	%rax, %r14
	movq	%rbx, %rax
	movq	%r12, %rcx
	movq	-56(%rbp), %rdx
	movq	-72(%rbp), %rsi
	movq	-80(%rbp), %rdi
	movq	-88(%rbp), %r8
	movq	-96(%rbp), %r9
	movq	-104(%rbp), %r10
	movq	%r13, %r11
	movq	$3, (%r15)
	xorl	%r10d, %r10d
	movq	$133, -8(%rsi)
	movq	%r14, (%rsi)
	movl	%r10d, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	-64(%rbp), %rcx
	movq	(%rcx), %rcx
	movq	-64(%rbp), %rdi
	movq	%r15, %r8
	movq	%r14, %r9
	jmp	*%rcx
doGC81:
	movq	$7437, -8(%rsi)
	movq	%r13, (%rsi)
	movq	%r14, 8(%rsi)
	movl	%ebx, 16(%rsi)
	movq	%r12, 24(%rsi)
	movq	-64(%rbp), %rdx
	movq	%rdx, 32(%rsi)
	movq	-56(%rbp), %rbx
	movq	%rbx, 40(%rsi)
	movq	%rsi, %rdx
	addq	$56, %rsi
	movq	%rdx, %rdi
	movabsq	$retGC7F, %r8
	jmp	ASM_InvokeGC
L82:
	movq	-56(%rbp), %r10
	movq	-64(%rbp), %r15
else.7C:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block else<E8B7> (retK<E8B6>,_exh<E8B5>,n<E8B4>,f<E8B3>,a0<E8B2>) */
	movq	$10, -8(%rsi)
	movl	$0, (%rsi)
	movq	%rsi, %r13
	addq	$16, %rsi
	movq	$3853, -8(%rsi)
	movabsq	$letJoinK.75, %rdx
	movq	%rdx, (%rsi)
	movq	%r15, 8(%rsi)
	movq	%r10, 16(%rsi)
	movq	%r14, 24(%rsi)
	movq	%r12, 32(%rsi)
	movl	%ebx, 40(%rsi)
	movq	%rsi, %rcx
	addq	$56, %rsi
	movq	8(%r12), %r14
	movq	(%r12), %rdi
	movq	%r13, %r8
	movq	%rcx, %r9
	jmp	*%r14
	.text
get_D_ite.88:
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
	movq	%r11, %r10
	subq	%rsi, %r10
	jle	doGC8B
check.83:
	/* block check<10AFB> (ep<E914>,retK<E915>,exh<E916>) */
	movq	$-1048576, %r13
	andq	%rsi, %r13
	movq	32(%r13), %r14
	movq	8(%r14), %r12
	cmpq	$1, %r12
	je	L8C
L_true84:
then.86:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block then<E921> (retK<E920>,_t<E91F>) */
	movq	(%rdx), %r13
	movq	%rdx, %rdi
	movq	(%r12), %r14
	movq	(%r14), %r8
	jmp	*%r13
doGC8B:
	movq	$28, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%rsi, %r15
	addq	$32, %rsi
	movq	%r15, %rdi
	movabsq	$retGC89, %r8
	jmp	ASM_InvokeGC
L8C:
else.85:
	/* Liveout:  GP={%rax %rdi}  */
	/* block else<E928> (exh<E927>) */
	movq	$133, -8(%rsi)
	movabsq	$str87, %rdx
	movq	%rdx, (%rsi)
	movl	$51, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movabsq	$tag74, %rbx
	movq	%rbx, (%rsi)
	movq	%r15, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	(%rcx), %r12
	movq	%r10, %rax
	movq	%rcx, %rdi
	jmp	*%r12
	.text
set_D_ite.8E:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest90
	/* live= GP={%rcx %rdx %rbx} spilled=  */
retGC8F:
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest90:
	movq	%r11, %r14
	subq	%rsi, %r14
	jle	doGC91
check.8D:
	/* Liveout:  GP={%rdi}  */
	/* block check<10AFF> (ep<E931>,ite<E932>,retK<E933>) */
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
doGC91:
	movq	$28, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%rsi, %r13
	addq	$32, %rsi
	movq	%r13, %rdi
	movabsq	$retGC8F, %r8
	jmp	ASM_InvokeGC
	.text
k.93:
	movq	%rax, %rcx
	movq	%rdi, %rdx
	jmp	gcTest95
	/* live= GP={%rcx %rdx} spilled=  */
retGC94:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest95:
	movq	%r11, %r13
	subq	%rsi, %r13
	jle	doGC96
check.92:
	/* Liveout:  GP={%rdi}  */
	/* block check<10B02> (ep<E967>,x<E966>) */
	movq	8(%rdx), %rbx
	movq	(%rbx), %r10
	movq	%rbx, %rdi
	jmp	*%r10
doGC96:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	%r12, %rdi
	movabsq	$retGC94, %r8
	jmp	ASM_InvokeGC
	.text
k.99:
	movq	%rax, %rcx
	movq	%rdi, %rdx
	jmp	gcTest9B
	/* live= GP={%rcx %rdx} spilled=  */
retGC9A:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest9B:
	movq	%r11, %r13
	subq	%rsi, %r13
	jle	doGC9C
check.97:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<10B05> (ep<E97A>,x<E977>) */
	movq	$-1048576, %rbx
	andq	%rsi, %rbx
	movq	$3, 8(%rbx)
	pause
	movq	8(%rdx), %r10
	movq	(%r10), %rdi
	movq	16(%rdx), %r8
	jmp	barrierSpin.98
doGC9C:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	%r12, %rdi
	movabsq	$retGC9A, %r8
	jmp	ASM_InvokeGC
	.text
barrierSpin.98:
	movq	%r8, %rcx
	movq	%rdi, %rdx
gcTestA6:
	movq	%r11, %rbx
	subq	%rsi, %rbx
	jle	doGCA7
check.9D:
	/* block check<10B08> (ep<E94D>,retK<E94E>) */
	movq	(%rdx), %r14
	movq	(%rdx), %r15
	movl	(%r14), %ebx
	cmpl	8(%r15), %ebx
	je	L_true9E
else.9F:
	/* Liveout:  GP={%rax %rdi}  */
	/* block else<E976> (ep<E974>,retK<E975>) */
	movq	$133, -8(%rsi)
	movq	%rdx, (%rsi)
	movabsq	$barrierSpin.98, %r12
	movq	%r12, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	$28, -8(%rsi)
	movabsq	$k.99, %r14
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
doGCA7:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	%r15, %rdi
	movabsq	$retGCA5, %r8
	jmp	ASM_InvokeGC
	/* live= GP={%rcx %rdx} spilled=  */
retGCA5:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
	jmp	gcTestA6
L_true9E:
then.A0:
	/* block then<E958> (ep<E956>,retK<E957>) */
	movq	8(%rdx), %rbx
	movq	16(%rbx), %r15
	cmpq	$1, %r15
	jne	LA8
S_caseA1:
case.A2:
	/* Liveout:  GP={%rdi}  */
	/* block case<E95D> (ep<E95B>,retK<E95C>) */
	movq	8(%rdx), %r13
	movq	$1, 8(%r13)
	movq	(%rcx), %r14
	movq	%rcx, %rdi
	jmp	*%r14
LA8:
	cmpq	$3, %r15
	jne	S_caseA1
S_caseA3:
case.A4:
	/* Liveout:  GP={%rax %rdi}  */
	/* block case<E963> (ep<E961>,retK<E962>) */
	movq	8(%rdx), %r10
	movq	$1, 16(%r10)
	movq	$20, -8(%rsi)
	movabsq	$k.93, %r13
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
wait.AA:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTestAC
	/* live= GP={%r10 %rcx %rdx %rbx} spilled=  */
retGCAB:
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTestAC:
	movq	%r11, %r12
	subq	%rsi, %r12
	jle	doGCAD
check.A9:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<10B0D> (ep<E942>,b<E943>,retK<E944>,exh<E945>) */
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
	movabsq	$barrierSpin.98, %r14
	movq	%r14, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	(%r13), %rdi
	movq	%rcx, %r8
	jmp	barrierSpin.98
doGCAD:
	movq	$36, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %r15
	addq	$40, %rsi
	movq	%r15, %rdi
	movabsq	$retGCAB, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.AF:
	movq	%rdi, %r14
	jmp	gcTestB1
	/* live= spilled= GP={%r~1}  */
retGCB0:
	movq	(%rdi), %r14
gcTestB1:
	movq	%r11, %rbx
	subq	%rsi, %rbx
	jle	doGCB2
	movq	%r14, -64(%rbp)
check.AE:
	/* Liveout:  GP={%rax %rdi}  */
	/* flushLoads */
	/* block check<10B0F> (ep<E9A9>) */
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
doGCB2:
	movq	$12, -8(%rsi)
	movq	%r14, (%rsi)
	movq	%rsi, %rdx
	addq	$16, %rsi
	movq	%rdx, %rdi
	movabsq	$retGCB0, %r8
	jmp	ASM_InvokeGC
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
	movq	%r11, %rbx
	subq	%rsi, %rbx
	jle	doGCB7
check.B3:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block check<10B12> (ep<E9A2>,_t<E99F>) */
	movq	$20, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	32(%rdx), %r10
	movq	%r10, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	$263, -8(%rsi)
	movabsq	$letJoinK.AF, %r13
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
	jmp	set_D_ite.8E
doGCB7:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	movq	%rdx, %rdi
	movabsq	$retGCB5, %r8
	jmp	ASM_InvokeGC
	.text
terminate.B9:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTestBB
	/* live= GP={%rcx %rdx} spilled=  */
retGCBA:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTestBB:
	movq	%r11, %rbx
	subq	%rsi, %rbx
	jle	doGCBC
check.B8:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block check<10B15> (ep<E99A>,self<E997>) */
	movq	$2827, -8(%rsi)
	movabsq	$letJoinK.B4, %r10
	movq	%r10, (%rsi)
	movq	16(%rdx), %r12
	movq	%r12, 8(%rsi)
	movq	24(%rdx), %r13
	movq	%r13, 16(%rsi)
	movq	%rcx, 24(%rsi)
	movq	24(%rdx), %r14
	movq	32(%r14), %r15
	movq	%r15, 32(%rsi)
	movq	%rsi, %rbx
	addq	$48, %rsi
	movq	8(%rdx), %rcx
	movq	(%rcx), %rdi
	movq	%rbx, %r8
	movq	32(%rdx), %r9
	jmp	get_D_ite.88
doGCBC:
	movq	$133, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	movq	%rdx, %rdi
	movabsq	$retGCBA, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.C2:
	movq	%rdi, %r12
gcTestC4:
	movq	%r11, %r13
	subq	%rsi, %r13
	jle	doGCC5
check.BD:
	/* block check<10B17> (ep<E9DF>) */
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
	jne	LC6
S_caseBE:
case.BF:
	/* Liveout:  GP={%rax %rdi}  */
	/* block case<E9EE> (ep<E9ED>) */
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
doGCC5:
	movq	$12, -8(%rsi)
	movq	%r12, (%rsi)
	movq	%rsi, %r12
	addq	$16, %rsi
	movq	%r12, %rdi
	movabsq	$retGCC3, %r8
	jmp	ASM_InvokeGC
	/* live= GP={%r12} spilled=  */
retGCC3:
	movq	(%rdi), %r12
	jmp	gcTestC4
LC6:
	cmpq	$3, %rdx
	jne	S_caseBE
S_caseC0:
case.C1:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block case<E9FB> (ep<E9FA>) */
	movq	16(%r12), %rdi
	movq	24(%r12), %r8
	jmp	terminate.B9
	.text
letJoinK.C8:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTestCA
	/* live= GP={%rcx %rdx} spilled=  */
retGCC9:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTestCA:
	movq	%r11, %r14
	subq	%rsi, %r14
	jle	doGCCB
check.C7:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block check<10B1A> (ep<E9D5>,_t<E9D1>) */
	movq	$20, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	56(%rdx), %rbx
	movq	%rbx, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	$6925, -8(%rsi)
	movabsq	$letJoinK.C2, %r12
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
	jmp	set_D_ite.8E
doGCCB:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	%r13, %rdi
	movabsq	$retGCC9, %r8
	jmp	ASM_InvokeGC
	.text
dispatch.CD:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTestCF
	/* live= GP={%r10 %rcx %rdx %rbx} spilled=  */
retGCCE:
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTestCF:
	movq	%r11, %r12
	subq	%rsi, %r12
	jle	doGCD0
check.CC:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block check<10B1F> (ep<E9CC>,self<E9C7>,handler<E9C8>,k<E9C9>) */
	movq	$12, -8(%rsi)
	movq	24(%rbx), %r12
	movq	%r12, (%rsi)
	movq	%rsi, %r13
	addq	$16, %rsi
	movq	$30481, -8(%rsi)
	movabsq	$letJoinK.C8, %r14
	movq	%r14, (%rsi)
	movq	16(%rbx), %r15
	movq	%r15, 8(%rsi)
	movq	24(%rbx), %r12
	movq	%r12, 16(%rsi)
	movq	40(%rbx), %r14
	movq	%r14, 24(%rsi)
	movq	%rdx, 32(%rsi)
	movq	%rcx, 40(%rsi)
	movq	%r10, 48(%rsi)
	movq	%r13, 56(%rsi)
	movq	%rsi, %r13
	addq	$72, %rsi
	movq	8(%rbx), %r15
	movq	(%r15), %rdi
	movq	%r13, %r8
	movq	32(%rbx), %r9
	jmp	get_D_ite.88
doGCD0:
	movq	$1673, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %rcx
	addq	$40, %rsi
	movq	%rcx, %rdi
	movabsq	$retGCCE, %r8
	jmp	ASM_InvokeGC
	.text
k.D2:
	movq	%rax, %rcx
	movq	%rdi, %rdx
	jmp	gcTestD4
	/* live= GP={%rcx %rdx} spilled=  */
retGCD3:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTestD4:
	movq	%r11, %r12
	subq	%rsi, %r12
	jle	doGCD5
check.D1:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block check<10B22> (ep<EA3F>,x<EA3A>) */
	movq	$-1048576, %rbx
	andq	%rsi, %rbx
	movq	$3, 8(%rbx)
	movq	8(%rdx), %rdi
	movq	24(%rdx), %r8
	movq	16(%rdx), %r9
	movq	32(%rdx), %r10
	jmp	dispatch.CD
doGCD5:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	%r10, %rdi
	movabsq	$retGCD3, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.D7:
	movq	%rdi, %r12
	jmp	gcTestD9
	/* live= GP={%r12} spilled=  */
retGCD8:
	movq	(%rdi), %r12
gcTestD9:
	movq	%r11, %rbx
	subq	%rsi, %rbx
	jle	doGCDA
check.D6:
	/* Liveout:  GP={%rax %rdi}  */
	/* flushLoads */
	/* block check<10B24> (ep<EA2A>) */
	movq	$-1048576, %r13
	andq	%rsi, %r13
	movq	%rsi, 120(%r13)
	movq	$1, (%r13)
	movq	%rax, -80(%rbp)
	movq	%rcx, -104(%rbp)
	movq	%rdx, %r14
	movq	%rdi, %r15
	movq	%r8, -64(%rbp)
	movq	%r9, -72(%rbp)
	movq	%r10, %rbx
	movq	%r11, -96(%rbp)
	movq	$-1048576, %r10
	andq	%rsi, %r10
	movq	%r10, %rdi
	movq	8(%r12), %rcx
	movq	16(%rcx), %r10
	movq	%r10, %rsi
	call	PromoteObj
	movq	%rax, -56(%rbp)
	movq	-80(%rbp), %rax
	movq	-104(%rbp), %rcx
	movq	%r14, %rdx
	movq	%r15, %rdi
	movq	-64(%rbp), %r8
	movq	-72(%rbp), %r9
	movq	%rbx, %r10
	movq	-96(%rbp), %r11
	movq	120(%r13), %rsi
	movq	$3, (%r13)
	movq	$-1048576, %r13
	andq	%rsi, %r13
	movq	%rsi, 120(%r13)
	movq	$1, (%r13)
	movq	%rax, %r15
	movq	%rcx, -64(%rbp)
	movq	%rdx, -72(%rbp)
	movq	%rdi, -80(%rbp)
	movq	%r8, -88(%rbp)
	movq	%r9, -96(%rbp)
	movq	%r10, -104(%rbp)
	movq	%r11, %rbx
	movq	$-1048576, %r14
	andq	%rsi, %r14
	movq	%r14, %rdi
	movq	8(%r12), %r10
	movq	24(%r10), %rdx
	movq	%rdx, %rsi
	call	PromoteObj
	movq	%rax, %r14
	movq	%r15, %rax
	movq	-64(%rbp), %rcx
	movq	-72(%rbp), %rdx
	movq	-80(%rbp), %rdi
	movq	-88(%rbp), %r8
	movq	-96(%rbp), %r9
	movq	-104(%rbp), %r10
	movq	%rbx, %r11
	movq	120(%r13), %rsi
	movq	$3, (%r13)
	movq	8(%r12), %r13
	movq	8(%r12), %r15
	movq	-56(%rbp), %r13
	movq	%r13, (%r14)
	movq	$-1048576, %r13
	andq	%rsi, %r13
	movq	%rsi, 120(%r13)
	movq	$1, (%r13)
	movq	%rax, %r15
	movq	%rcx, -104(%rbp)
	movq	%rdx, %rbx
	movq	%rdi, -56(%rbp)
	movq	%r8, -96(%rbp)
	movq	%r9, -80(%rbp)
	movq	%r10, -72(%rbp)
	movq	%r11, -88(%rbp)
	movq	$-1048576, %r14
	andq	%rsi, %r14
	movq	%r14, %rdi
	movq	8(%r12), %rdx
	movq	8(%rdx), %rcx
	movq	%rcx, %rsi
	call	PromoteObj
	movq	%rax, %r14
	movq	%r15, %rax
	movq	-104(%rbp), %rcx
	movq	%rbx, %rdx
	movq	-56(%rbp), %rdi
	movq	-96(%rbp), %r8
	movq	-80(%rbp), %r9
	movq	-72(%rbp), %r10
	movq	-88(%rbp), %r11
	movq	120(%r13), %rsi
	movq	$3, (%r13)
	movq	8(%r12), %rcx
	movq	(%r14), %rax
	xorq	%rdx, %rdx
	lock
	cmpxchgq	%rdx, (%r14)
	movq	$2827, -8(%rsi)
	movabsq	$k.D2, %r10
	movq	%r10, (%rsi)
	movq	16(%r12), %r13
	movq	%r13, 8(%rsi)
	movq	24(%r12), %r14
	movq	%r14, 16(%rsi)
	movq	32(%r12), %r15
	movq	%r15, 24(%rsi)
	movq	40(%r12), %rcx
	movq	%rcx, 32(%rsi)
	movq	%rsi, %rbx
	addq	$48, %rsi
	movq	$20, -8(%rsi)
	movq	$1, (%rsi)
	movq	%rbx, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	movq	32(%r12), %r10
	movq	40(%r10), %rbx
	movq	8(%rbx), %r13
	movq	32(%r12), %r14
	movq	%r13, 40(%r14)
	movq	(%rbx), %r15
	movq	(%r15), %rcx
	movq	%rdx, %rax
	movq	%r15, %rdi
	jmp	*%rcx
doGCDA:
	movq	$12, -8(%rsi)
	movq	%r12, (%rsi)
	movq	%rsi, %rdx
	addq	$16, %rsi
	movq	%rdx, %rdi
	movabsq	$retGCD8, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.DC:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTestDE
	/* live= GP={%rcx %rdx} spilled=  */
retGCDD:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTestDE:
	movq	%r11, %r14
	subq	%rsi, %r14
	jle	doGCDF
check.DB:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block check<10B27> (ep<EA20>,_t<EA1C>) */
	movq	$20, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	56(%rdx), %rbx
	movq	%rbx, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	$5901, -8(%rsi)
	movabsq	$letJoinK.D7, %r12
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
	jmp	set_D_ite.8E
doGCDF:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	%r13, %rdi
	movabsq	$retGCDD, %r8
	jmp	ASM_InvokeGC
	.text
handler.E8:
	movq	%rax, %rcx
	movq	%rdi, %rdx
gcTestEA:
	movq	%r11, %r13
	subq	%rsi, %r13
	jle	doGCEB
check.E0:
	/* block check<10B2A> (ep<EA09>,s<EA06>) */
	movq	$-1048576, %r12
	andq	%rsi, %r12
	cmpq	$1, %rcx
	jne	L_trueE1
else.E2:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block else<EA60> (ep<EA5E>,self<EA5F>) */
	movq	40(%rdx), %rdi
	movq	%r12, %r8
	jmp	terminate.B9
doGCEB:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	%r12, %rdi
	movabsq	$retGCE9, %r8
	jmp	ASM_InvokeGC
	/* live= GP={%rcx %rdx} spilled=  */
retGCE9:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
	jmp	gcTestEA
L_trueE1:
then.E3:
	/* block then<EA11> (ep<EA0E>,s<EA10>,self<EA0F>) */
	cmpq	$1, (%rcx)
	jne	LEC
L_trueE4:
then.E6:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block then<EA18> (ep<EA15>,self<EA17>,s<EA16>) */
	movq	$28433, -8(%rsi)
	movabsq	$letJoinK.DC, %rbx
	movq	%rbx, (%rsi)
	movq	16(%rdx), %r10
	movq	%r10, 8(%rsi)
	movq	24(%rdx), %r13
	movq	%r13, 16(%rsi)
	movq	48(%rdx), %r14
	movq	%r14, 24(%rsi)
	movq	%rdx, 32(%rsi)
	movq	%r12, 40(%rsi)
	movq	8(%rcx), %r15
	movq	%r15, 48(%rsi)
	movq	24(%rdx), %rcx
	movq	32(%rcx), %rbx
	movq	%rbx, 56(%rsi)
	movq	%rsi, %r15
	addq	$72, %rsi
	movq	8(%rdx), %r10
	movq	(%r10), %rdi
	movq	%r15, %r8
	movq	32(%rdx), %r9
	jmp	get_D_ite.88
LEC:
else.E5:
	/* Liveout:  GP={%rax %rdi}  */
	/* block else<EA59> (ep<EA58>) */
	movq	$12, -8(%rsi)
	movabsq	$tagE7, %r13
	movq	%r13, (%rsi)
	movq	%rsi, %r10
	addq	$16, %rsi
	movq	32(%rdx), %rbx
	movq	(%rbx), %r14
	movq	%r10, %rax
	movq	%rbx, %rdi
	jmp	*%r14
	.text
wrappedK.EE:
	movq	%rax, %rcx
	movq	%rdi, %rdx
	jmp	gcTestF0
	/* live= GP={%rcx %rdx} spilled=  */
retGCEF:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTestF0:
	movq	%r11, %r12
	subq	%rsi, %r12
	jle	doGCF1
check.ED:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block check<10B2D> (ep<EA64>,x<EA63>) */
	movq	$-1048576, %rbx
	andq	%rsi, %rbx
	movq	$3, 8(%rbx)
	movq	16(%rdx), %rdi
	movq	%rbx, %r8
	movq	24(%rdx), %r9
	movq	8(%rdx), %r10
	jmp	dispatch.CD
doGCF1:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	%r10, %rdi
	movabsq	$retGCEF, %r8
	jmp	ASM_InvokeGC
	.text
wrap.F3:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTestF5
	/* live= GP={%r12 %r10 %rcx %rdx %rbx} spilled=  */
retGCF4:
	movq	32(%rdi), %r12
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTestF5:
	movq	%r11, %r13
	subq	%rsi, %r13
	jle	doGCF6
check.F2:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<10B33> (ep<E98F>,c<E990>,k<E991>,retK<E992>,exh<E993>) */
	movq	$3851, -8(%rsi)
	movabsq	$terminate.B9, %r13
	movq	%r13, (%rsi)
	movq	(%rbx), %r14
	movq	%r14, 8(%rsi)
	movq	8(%rbx), %r15
	movq	%r15, 16(%rsi)
	movq	%rdx, 24(%rsi)
	movq	%r12, 32(%rsi)
	movq	%rsi, %r15
	addq	$48, %rsi
	movq	$7949, -8(%rsi)
	movabsq	$dispatch.CD, %r13
	movq	%r13, (%rsi)
	movq	(%rbx), %r14
	movq	%r14, 8(%rsi)
	movq	8(%rbx), %r13
	movq	%r13, 16(%rsi)
	movq	%rdx, 24(%rsi)
	movq	%r12, 32(%rsi)
	movq	%r15, 40(%rsi)
	movq	%rsi, %r13
	addq	$56, %rsi
	movq	$60, -8(%rsi)
	movabsq	$handler.E8, %r14
	movq	%r14, (%rsi)
	movq	(%rbx), %r14
	movq	%r14, 8(%rsi)
	movq	8(%rbx), %rbx
	movq	%rbx, 16(%rsi)
	movq	%rdx, 24(%rsi)
	movq	%r12, 32(%rsi)
	movq	%r15, 40(%rsi)
	movq	%r13, 48(%rsi)
	movq	%rsi, %r14
	addq	$64, %rsi
	movq	$36, -8(%rsi)
	movabsq	$wrappedK.EE, %r15
	movq	%r15, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%r13, 16(%rsi)
	movq	%r14, 24(%rsi)
	movq	%rsi, %r12
	addq	$40, %rsi
	movq	(%r10), %rcx
	movq	%r10, %rdi
	movq	%r12, %r8
	jmp	*%rcx
doGCF6:
	movq	$44, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%r12, 32(%rsi)
	movq	%rsi, %rdx
	addq	$48, %rsi
	movq	%rdx, %rdi
	movabsq	$retGCF4, %r8
	jmp	ASM_InvokeGC
	.text
wakeupK.F9:
	movq	%rax, %rcx
	movq	%rdi, %rdx
	jmp	gcTestFB
	/* live= GP={%rcx %rdx} spilled=  */
retGCFA:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTestFB:
	movq	%r11, %r10
	subq	%rsi, %r10
	jle	doGCFC
check.F7:
	/* Liveout:  GP={%rdi}  */
	/* block check<10B36> (ep<EA9E>,x<EA9C>) */
	movq	8(%rdx), %rdi
	jmp	dispatch.F8
doGCFC:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	%rbx, %rdi
	movabsq	$retGCFA, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.FE:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest100
	/* live= GP={%rcx %rdx} spilled=  */
retGCFF:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest100:
	movq	%r11, %rbx
	subq	%rsi, %rbx
	jle	doGC101
check.FD:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<10B3C> (ep<EAC5>,rest<EAC4>) */
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
doGC101:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	movq	%rdx, %rdi
	movabsq	$retGCFF, %r8
	jmp	ASM_InvokeGC
	.text
append.106:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest108
	/* live= GP={%rcx %rdx %rbx} spilled=  */
retGC107:
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest108:
	movq	%r11, %r15
	subq	%rsi, %r15
	jle	doGC109
check.102:
	/* block check<10B40> (ep<EAB2>,queue1<EAB3>,retK<EAB4>) */
	cmpq	$1, %rdx
	jne	L10A
L_true103:
then.105:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block then<EABC> (ep<EABA>,retK<EABB>) */
	movq	(%rcx), %r13
	movq	%rcx, %rdi
	movq	(%rbx), %r8
	jmp	*%r13
doGC109:
	movq	$28, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%rsi, %r14
	addq	$32, %rsi
	movq	%r14, %rdi
	movabsq	$retGC107, %r8
	jmp	ASM_InvokeGC
L10A:
else.104:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block else<EAC2> (ep<EABF>,queue1<EAC1>,retK<EAC0>) */
	movq	$775, -8(%rsi)
	movabsq	$letJoinK.FE, %r12
	movq	%r12, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%rsi, %r10
	addq	$32, %rsi
	movq	%rbx, %rdi
	movq	16(%rdx), %r8
	movq	%r10, %r9
	jmp	append.106
	.text
letJoinK.10C:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest10E
	/* live= GP={%rcx %rdx} spilled=  */
retGC10D:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest10E:
	movq	%r11, %r10
	subq	%rsi, %r10
	jle	doGC10F
check.10B:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<10B43> (ep<EAD3>,newHd<EAD1>) */
	movq	8(%rdx), %r12
	movq	%rcx, 72(%r12)
	movq	$12, -8(%rsi)
	movq	16(%rdx), %r14
	movq	%r14, (%rsi)
	movq	%rsi, %r13
	addq	$16, %rsi
	movq	24(%rdx), %rdi
	movq	%r13, %r8
	jmp	letJoinK.7
doGC10F:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	%rbx, %rdi
	movabsq	$retGC10D, %r8
	jmp	ASM_InvokeGC
	.text
revQueue.114:
	movq	%r12, %rbx
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %r12
	jmp	gcTest116
	/* live= GP={%r13 %rbx %r10 %r15 %r14 %rcx} spilled=  */
retGC115:
	movq	40(%rdi), %r13
	movq	32(%rdi), %rbx
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %r12
gcTest116:
	movq	%r11, %r14
	subq	%rsi, %r14
	jle	doGC117
	movq	%rcx, %r15
	movq	%rdx, %r14
	movq	%r12, %rcx
check.110:
	/* block check<10B4A> (ep<EB0B>,fls<EB0C>,k<EB0D>,rest<EB0E>,acc<EB0F>,retK<EB10>) */
	movq	$28, -8(%rsi)
	movq	%r14, (%rsi)
	movq	%r15, 8(%rsi)
	movq	%rbx, 16(%rsi)
	movq	%rsi, %r12
	addq	$32, %rsi
	cmpq	$1, %r10
	jne	L118
L_true111:
then.113:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block then<EB1B> (acc<EB1A>,retK<EB19>,acc<EB18>) */
	movq	%r13, %rdi
	movq	%r12, %r8
	movq	%rbx, %r9
	jmp	letJoinK.6
doGC117:
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
	movabsq	$retGC115, %r8
	jmp	ASM_InvokeGC
L118:
else.112:
	/* Liveout:  GP={%r13 %r12 %r10 %r9 %r8 %rdi}  */
	/* block else<EB21> (ep<EB1D>,rest<EB20>,retK<EB1F>,acc<EB1E>) */
	movq	%rcx, %rdi
	movq	(%r10), %r8
	movq	8(%r10), %r9
	movq	16(%r10), %r10
	jmp	revQueue.114
	.text
letJoinK.11A:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest11C
	/* live= GP={%rcx %rdx} spilled=  */
retGC11B:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest11C:
	movq	%r11, %rbx
	subq	%rsi, %rbx
	jle	doGC11D
check.119:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<10B4D> (ep<EB6A>,rest<EB69>) */
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
doGC11D:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	movq	%rdx, %rdi
	movabsq	$retGC11B, %r8
	jmp	ASM_InvokeGC
	.text
append.122:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest124
	/* live= GP={%rcx %rdx %rbx} spilled=  */
retGC123:
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest124:
	movq	%r11, %r15
	subq	%rsi, %r15
	jle	doGC125
check.11E:
	/* block check<10B51> (ep<EB57>,queue1<EB58>,retK<EB59>) */
	cmpq	$1, %rdx
	jne	L126
L_true11F:
then.121:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block then<EB61> (ep<EB5F>,retK<EB60>) */
	movq	(%rcx), %r13
	movq	%rcx, %rdi
	movq	(%rbx), %r8
	jmp	*%r13
doGC125:
	movq	$28, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%rsi, %r14
	addq	$32, %rsi
	movq	%r14, %rdi
	movabsq	$retGC123, %r8
	jmp	ASM_InvokeGC
L126:
else.120:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block else<EB67> (ep<EB64>,queue1<EB66>,retK<EB65>) */
	movq	$775, -8(%rsi)
	movabsq	$letJoinK.11A, %r12
	movq	%r12, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%rsi, %r10
	addq	$32, %rsi
	movq	%rbx, %rdi
	movq	16(%rdx), %r8
	movq	%r10, %r9
	jmp	append.122
	.text
letJoinK.128:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest12A
	/* live= GP={%rcx %rdx} spilled=  */
retGC129:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest12A:
	movq	%r11, %r14
	subq	%rsi, %r14
	jle	doGC12B
check.127:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<10B54> (ep<EB78>,newHd<EB76>) */
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
doGC12B:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	%r13, %rdi
	movabsq	$retGC129, %r8
	jmp	ASM_InvokeGC
	.text
dispatch.F8:
	movq	%rdi, %rcx
gcTest149:
	movq	%r11, %rdx
	subq	%rsi, %rdx
	jle	doGC14A
check.12C:
	/* block check<10B56> (ep<EA73>) */
	movq	16(%rcx), %rbx
	movq	72(%rbx), %r13
	movq	16(%rcx), %r10
	movq	80(%r10), %r14
	movq	16(%rcx), %r12
	movq	312(%r12), %rdx
	cmpq	$1, %rdx
	jne	L14B
L_true12D:
then.12F:
	/* block then<EBA9> (ep<EBA6>,hd<EBA8>,tl<EBA7>) */
	movq	$1, %r15
	jmp	letJoinK.132
doGC14A:
	movq	$12, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%rsi, %r15
	addq	$16, %rsi
	movq	%r15, %rdi
	movabsq	$retGC148, %r8
	jmp	ASM_InvokeGC
	/* live= GP={%rcx} spilled=  */
retGC148:
	movq	(%rdi), %rcx
	jmp	gcTest149
L14B:
else.12E:
	/* block else<EBB0> (ep<EBAC>,hd<EBAF>,tl<EBAE>,ldgPadOrig<EBAD>) */
	movq	16(%rcx), %rbx
	leaq	312(%rbx), %r15
	movq	%rdx, %rax
	movq	$1, %r10
	movq	16(%rcx), %r12
	lock
	cmpxchgq	%r10, 312(%r12)
	movq	%rax, %r15
	cmpq	%rdx, %r15
	jne	L14C
L_true130:
letJoinK.132:
	/* block letJoinK<EA7E> (ep<EA7A>,hd<EA7D>,tl<EA7C>,landingPadItems<EA7B>) */
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
	jne	L_true133
else.134:
	/* block else<EAE1> (ep<EADD>,hd<EAE0>,tl<EADF>,letJoinK<EADE>) */
	cmpq	$1, %r13
	jne	L_true136
else.137:
	/* block else<EAF0> (ep<EAEC>,hd<EAEF>,tl<EAEE>,letJoinK<EAED>) */
	cmpq	$1, %r14
	jne	L_true139
else.13A:
	/* block else<EB41> (ep<EB3E>,hd<EB40>,letJoinK<EB3F>) */
	movq	16(%rcx), %r14
	movq	312(%r14), %r15
	cmpq	$1, %r15
	jne	L14D
L_true13C:
then.13E:
	/* block then<EB89> (ep<EB86>,hd<EB88>,letJoinK<EB87>) */
	movq	$1, %r14
letJoinK.141:
	/* block letJoinK<EB46> (ep<EB42>,hd<EB45>,letJoinK<EB44>,landingPadItems<EB43>) */
	cmpq	$1, %r14
	je	L_true142
else.143:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block else<EB52> (ep<EB4E>,hd<EB51>,letJoinK<EB50>,landingPadItems<EB4F>) */
	movq	$12, -8(%rsi)
	movq	%r13, (%rsi)
	movq	%rsi, %r15
	addq	$16, %rsi
	movq	$133, -8(%rsi)
	movq	%r15, (%rsi)
	movabsq	$append.122, %rbx
	movq	%rbx, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	movq	$1545, -8(%rsi)
	movabsq	$letJoinK.128, %r13
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
	jmp	append.122
L_true142:
then.144:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block then<EB4B> (letJoinK<EB4A>) */
	movq	%r12, %rdi
	movq	$1, %r8
	jmp	letJoinK.7
L_true133:
then.135:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block then<EAAD> (ep<EAA9>,hd<EAAC>,landingPadItems<EAAB>,letJoinK<EAAA>) */
	movq	$12, -8(%rsi)
	movq	%r13, (%rsi)
	movq	%rsi, %rdx
	addq	$16, %rsi
	movq	$133, -8(%rsi)
	movq	%rdx, (%rsi)
	movabsq	$append.106, %r10
	movq	%r10, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	$1545, -8(%rsi)
	movabsq	$letJoinK.10C, %r13
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
	jmp	append.106
L_true136:
then.138:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block then<EAE7> (ep<EAE4>,hd<EAE6>,letJoinK<EAE5>) */
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
L14D:
else.13D:
	/* block else<EB90> (ep<EB8C>,hd<EB8F>,letJoinK<EB8E>,ldgPadOrig<EB8D>) */
	movq	16(%rcx), %rbx
	leaq	312(%rbx), %rdx
	movq	%r15, %rax
	movq	$1, %r10
	movq	16(%rcx), %r14
	lock
	cmpxchgq	%r10, 312(%r14)
	movq	%rax, %r14
	cmpq	%r15, %r14
	jne	L14E
L_true13F:
	jmp	letJoinK.141
L14E:
else.140:
	/* block else<EB9F> (ep<EB9C>,hd<EB9E>,letJoinK<EB9D>) */
	movq	$1, %r14
	jmp	letJoinK.141
L_true139:
then.13B:
	/* block then<EAF6> (ep<EAF3>,tl<EAF5>,letJoinK<EAF4>) */
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
	movabsq	$revQueue.114, %r15
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
	jne	L14F
L_true145:
then.147:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block then<EB30> (_t<EB2F>,acc<EB2E>,letJoinK<EB2D>) */
	movq	%r13, %rdi
	movq	%r12, %r8
	movq	%rdx, %r9
	jmp	letJoinK.6
L14F:
else.146:
	/* Liveout:  GP={%r13 %r12 %r10 %r9 %r8 %rdi}  */
	/* block else<EB37> (_t<EB36>,revQueue<EB35>,acc<EB34>,letJoinK<EB33>) */
	movq	(%r10), %rdi
	movq	(%rbx), %r8
	movq	8(%rbx), %r9
	movq	16(%rbx), %r10
	jmp	revQueue.114
L14C:
else.131:
	/* block else<EBBF> (ep<EBBC>,hd<EBBE>,tl<EBBD>) */
	movq	$1, %r15
	jmp	letJoinK.132
	.text
switch.157:
	movq	%rax, %rcx
	movq	%rdi, %rdx
gcTest159:
	movq	%r11, %rbx
	subq	%rsi, %rbx
	jle	doGC15A
check.150:
	/* block check<10B59> (ep<EA6F>,s<EA6E>) */
	movq	$-1048576, %r13
	andq	%rsi, %r13
	movq	$263, -8(%rsi)
	movabsq	$dispatch.F8, %r14
	movq	%r14, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%r13, 16(%rsi)
	movq	%rsi, %r12
	addq	$32, %rsi
	cmpq	$1, %rcx
	jne	L_true151
else.152:
	/* Liveout:  GP={%rdi}  */
	/* block else<EBDD> (dispatch<EBDC>) */
	movq	%r12, %rdi
	jmp	dispatch.F8
doGC15A:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	%r15, %rdi
	movabsq	$retGC158, %r8
	jmp	ASM_InvokeGC
	/* live= GP={%rcx %rdx} spilled=  */
retGC158:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
	jmp	gcTest159
L_true151:
then.153:
	/* block then<EBC7> (ep<EBC3>,s<EBC6>,self<EBC5>,dispatch<EBC4>) */
	cmpq	$1, (%rcx)
	jne	L15B
L_true154:
then.156:
	/* Liveout:  GP={%rdi}  */
	/* block then<EBCF> (self<EBCE>,dispatch<EBCD>,s<EBCC>) */
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
	jmp	dispatch.F8
L15B:
else.155:
	/* Liveout:  GP={%rax %rdi}  */
	/* block else<EBD6> (ep<EBD5>) */
	movq	$12, -8(%rsi)
	movabsq	$tagE7, %r15
	movq	%r15, (%rsi)
	movq	%rsi, %r10
	addq	$16, %rsi
	movq	8(%rdx), %rbx
	movq	(%rbx), %rcx
	movq	%r10, %rax
	movq	%rbx, %rdi
	jmp	*%rcx
	.text
mkSwitch.15D:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest15F
	/* live= GP={%r10 %rcx %rdx %rbx} spilled=  */
retGC15E:
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest15F:
	movq	%r11, %r13
	subq	%rsi, %r13
	jle	doGC160
check.15C:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<10B5E> (ep<EBE1>,_wild<EBE2>,retK<EBE3>,exh<EBE4>) */
	movq	(%rcx), %r10
	movq	%rcx, %rdi
	movq	(%rbx), %r8
	jmp	*%r10
doGC160:
	movq	$1673, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %r12
	addq	$40, %rsi
	movq	%r12, %rdi
	movabsq	$retGC15E, %r8
	jmp	ASM_InvokeGC
	.text
schedCont.162:
	movq	%rax, %rcx
	movq	%rdi, %rdx
	jmp	gcTest164
	/* live= GP={%rcx %rdx} spilled=  */
retGC163:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest164:
	movq	%r11, %rbx
	subq	%rsi, %rbx
	jle	doGC165
check.161:
	/* Liveout:  GP={%rax %rdi}  */
	/* block check<10B61> (ep<EBF6>,k<EBF5>) */
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
doGC165:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	%rcx, %rdi
	movabsq	$retGC163, %r8
	jmp	ASM_InvokeGC
	.text
dummyK.167:
	movq	%rax, %rcx
	movq	%rdi, %rdx
	jmp	gcTest169
	/* live= GP={%rcx %rdx} spilled=  */
retGC168:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest169:
	movq	%r11, %rbx
	subq	%rsi, %rbx
	jle	doGC16A
check.166:
	/* Liveout:  GP={%rax %rdi}  */
	/* block check<10B64> (ep<EC10>,x<EC0F>) */
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
doGC16A:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	%rcx, %rdi
	movabsq	$retGC168, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.16C:
	movq	%r8, %r13
	movq	%rdi, %r12
	jmp	gcTest16E
	/* live= GP={%r13 %r12} spilled=  */
retGC16D:
	movq	8(%rdi), %r13
	movq	(%rdi), %r12
gcTest16E:
	movq	%r11, %r14
	subq	%rsi, %r14
	jle	doGC16F
check.16B:
	/* Liveout:  GP={%rdi}  */
	/* block check<10B67> (ep<EC1C>,act<EC1B>) */
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
doGC16F:
	movq	$20, -8(%rsi)
	movq	%r12, (%rsi)
	movq	%r13, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	%rbx, %rdi
	movabsq	$retGC16D, %r8
	jmp	ASM_InvokeGC
	.text
initVPFields.171:
	movq	%r10, %r14
	movq	%r9, %r13
	movq	%r8, %r12
	movq	%rdi, %rbx
	jmp	gcTest173
	/* live= GP={%r14} spilled= GP={%r~1 %r~1 %r~1}  */
retGC172:
	movq	24(%rdi), %r14
	movq	16(%rdi), %r13
	movq	8(%rdi), %r12
	movq	(%rdi), %rbx
gcTest173:
	movq	%r11, %r15
	subq	%rsi, %r15
	jle	doGC174
	movq	%r13, -64(%rbp)
	movq	%r12, -72(%rbp)
	movq	%rbx, -56(%rbp)
check.170:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block check<10B6C> (ep<EBEE>,vp<EBEF>,retK<EBF0>,exh<EBF1>) */
	movq	$10, -8(%rsi)
	movabsq	$schedCont.162, %r15
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
	movabsq	$dummyK.167, %r15
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
	movabsq	$letJoinK.16C, %rdx
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
	jmp	mkSwitch.15D
doGC174:
	movq	$1673, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%r12, 8(%rsi)
	movq	%r13, 16(%rsi)
	movq	%r14, 24(%rsi)
	movq	%rsi, %r10
	addq	$40, %rsi
	movq	%r10, %rdi
	movabsq	$retGC172, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.177:
	movq	%rdi, %rcx
	jmp	gcTest179
	/* live= GP={%rcx} spilled=  */
retGC178:
	movq	(%rdi), %rcx
gcTest179:
	movq	%r11, %r10
	subq	%rsi, %r10
	jle	doGC17A
check.175:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block check<10B6E> (ep<EC3F>) */
	movq	8(%rcx), %rdx
	movq	(%rdx), %rdi
	movq	24(%rcx), %r8
	movq	16(%rcx), %r9
	jmp	lp.176
doGC17A:
	movq	$12, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%rsi, %rbx
	addq	$16, %rsi
	movq	%rbx, %rdi
	movabsq	$retGC178, %r8
	jmp	ASM_InvokeGC
	.text
lp.176:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest181
	/* live= GP={%rcx %rdx %rbx} spilled=  */
retGC180:
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest181:
	movq	%r11, %r10
	subq	%rsi, %r10
	jle	doGC182
check.17B:
	/* block check<10B72> (ep<EC2E>,vps<EC2F>,retK<EC30>) */
	cmpq	$1, %rdx
	je	L183
L_true17C:
then.17E:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block then<EC38> (ep<EC35>,vps<EC37>,retK<EC36>) */
	movq	$133, -8(%rsi)
	movq	%rbx, (%rsi)
	movabsq	$lp.176, %r12
	movq	%r12, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	$1801, -8(%rsi)
	movabsq	$letJoinK.177, %r14
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
	jmp	initVPFields.171
doGC182:
	movq	$28, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%rsi, %rbx
	addq	$32, %rsi
	movq	%rbx, %rdi
	movabsq	$retGC180, %r8
	jmp	ASM_InvokeGC
L183:
else.17D:
	/* Liveout:  GP={%rdi}  */
	/* block else<EC4D> (retK<EC4C>) */
	movq	%rcx, %rdi
	jmp	letJoinK.17F
	.text
retGC18C:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
	jmp	gcTest18D
L_true185:
then.187:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block then<ECD8> (ep<ECD6>,retK<ECD7>) */
	pause
	movq	%rdx, %rdi
	movq	%rcx, %r8
spinLp.18B:
	movq	%r8, %rcx
	movq	%rdi, %rdx
gcTest18D:
	movq	%r11, %r15
	subq	%rsi, %r15
	jle	doGC18E
check.184:
	/* block check<10B75> (ep<ECCF>,retK<ECD0>) */
	movq	(%rdx), %r10
	leaq	(%r10), %rbx
	cmpl	$1, (%rbx)
	je	L_true185
else.186:
	/* block else<ECDC> (ep<ECDA>,retK<ECDB>) */
	movq	$1, %r12
	movq	(%rdx), %r13
	lock
	xchgq	%r12, (%r13)
	cmpq	$1, %r12
	je	L_true188
else.189:
	/* Liveout:  GP={%rdi}  */
	/* block else<ECE5> (retK<ECE4>) */
	movq	%rcx, %rdi
	jmp	letJoinK.D
L_true188:
then.18A:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block then<ECE1> (ep<ECDF>,retK<ECE0>) */
	movq	%rdx, %rdi
	movq	%rcx, %r8
	jmp	spinLp.18B
doGC18E:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	%r14, %rdi
	movabsq	$retGC18C, %r8
	jmp	ASM_InvokeGC
	/* live= GP={%rcx %rdx} spilled=  */
	jmp	retGC18C
	.text
k.190:
	movq	%rax, %rcx
	movq	%rdi, %rdx
	jmp	gcTest192
	/* live= GP={%rcx %rdx} spilled=  */
retGC191:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest192:
	movq	%r11, %r15
	subq	%rsi, %r15
	jle	doGC193
check.18F:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<10B78> (ep<EC7D>,x<EC79>) */
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
	movabsq	$spinLp.18B, %r13
	movq	%r13, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	(%r12), %rdi
	movq	%r10, %r8
	jmp	spinLp.18B
doGC193:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	%r14, %rdi
	movabsq	$retGC191, %r8
	jmp	ASM_InvokeGC
	.text
dispatch.13:
	movq	%rdi, %rcx
	jmp	gcTest196
	/* live= GP={%rcx} spilled=  */
retGC195:
	movq	(%rdi), %rcx
gcTest196:
	movq	%r11, %rdx
	subq	%rsi, %rdx
	jle	doGC197
check.194:
	/* Liveout:  GP={%rax %rdi}  */
	/* block check<10B7A> (ep<EC76>) */
	movq	$2827, -8(%rsi)
	movabsq	$k.190, %r12
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
doGC197:
	movq	$12, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%rsi, %r15
	addq	$16, %rsi
	movq	%r15, %rdi
	movabsq	$retGC195, %r8
	jmp	ASM_InvokeGC
	.text
retGC1A0:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
	jmp	gcTest1A1
L_true199:
then.19B:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block then<ED46> (ep<ED44>,retK<ED45>) */
	pause
	movq	%rdx, %rdi
	movq	%rcx, %r8
spinLp.19F:
	movq	%r8, %rcx
	movq	%rdi, %rdx
gcTest1A1:
	movq	%r11, %r15
	subq	%rsi, %r15
	jle	doGC1A2
check.198:
	/* block check<10B7D> (ep<ED3D>,retK<ED3E>) */
	movq	(%rdx), %r10
	leaq	(%r10), %rbx
	cmpl	$1, (%rbx)
	je	L_true199
else.19A:
	/* block else<ED4A> (ep<ED48>,retK<ED49>) */
	movq	$1, %r12
	movq	(%rdx), %r13
	lock
	xchgq	%r12, (%r13)
	cmpq	$1, %r12
	je	L_true19C
else.19D:
	/* Liveout:  GP={%rdi}  */
	/* block else<ED53> (retK<ED52>) */
	movq	%rcx, %rdi
	jmp	letJoinK.19
L_true19C:
then.19E:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block then<ED4F> (ep<ED4D>,retK<ED4E>) */
	movq	%rdx, %rdi
	movq	%rcx, %r8
	jmp	spinLp.19F
doGC1A2:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	%r14, %rdi
	movabsq	$retGC1A0, %r8
	jmp	ASM_InvokeGC
	/* live= GP={%rcx %rdx} spilled=  */
	jmp	retGC1A0
	.text
letJoinK.1A4:
	movq	%r8, %r13
	movq	%rdi, %r12
	jmp	gcTest1A6
	/* live= GP={%r13 %r12} spilled=  */
retGC1A5:
	movq	8(%rdi), %r13
	movq	(%rdi), %r12
gcTest1A6:
	movq	%r11, %r15
	subq	%rsi, %r15
	jle	doGC1A7
check.1A3:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<10B80> (ep<ED0D>,k<ED0B>) */
	movq	$20, -8(%rsi)
	movq	%r13, (%rsi)
	movq	24(%r12), %r15
	movq	%r15, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movq	%r14, (%rsi)
	movq	$1, 8(%rsi)
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
	movq	$1801, -8(%rsi)
	movabsq	$letJoinK.19, %r15
	movq	%r15, (%rsi)
	movq	8(%r12), %rcx
	movq	%rcx, 8(%rsi)
	movq	16(%r12), %rdx
	movq	%rdx, 16(%rsi)
	movq	%r14, 24(%rsi)
	movq	%rsi, %rcx
	addq	$40, %rsi
	movq	$12, -8(%rsi)
	movq	8(%r12), %r10
	movq	%r10, (%rsi)
	movq	%rsi, %rbx
	addq	$16, %rsi
	movq	$133, -8(%rsi)
	movq	%rbx, (%rsi)
	movabsq	$spinLp.19F, %r13
	movq	%r13, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	(%r12), %rdi
	movq	%rcx, %r8
	jmp	spinLp.19F
doGC1A7:
	movq	$20, -8(%rsi)
	movq	%r12, (%rsi)
	movq	%r13, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	%r14, %rdi
	movabsq	$retGC1A5, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.1A9:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest1AB
	/* live= GP={%rcx %rdx} spilled=  */
retGC1AA:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest1AB:
	movq	%r11, %r10
	subq	%rsi, %r10
	jle	doGC1AC
check.1A8:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<10B83> (ep<ED5F>,k<ED5E>) */
	movq	8(%rdx), %rdi
	movq	%rcx, %r8
	jmp	letJoinK.1A4
doGC1AC:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	%rbx, %rdi
	movabsq	$retGC1AA, %r8
	jmp	ASM_InvokeGC
	.text
schedulerLoop.1BA:
	movq	%rax, %rcx
	movq	%rdi, %rdx
	jmp	gcTest1BC
	/* live= GP={%rcx %rdx} spilled=  */
retGC1BB:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest1BC:
	movq	%r11, %r12
	subq	%rsi, %r12
	jle	doGC1BD
check.1AD:
	/* block check<10B86> (ep<EC71>,s<EC6E>) */
	movq	$777, -8(%rsi)
	movabsq	$dispatch.13, %rbx
	movq	%rbx, (%rsi)
	movq	24(%rdx), %r10
	movq	%r10, 8(%rsi)
	movq	%rdx, 16(%rsi)
	movq	$-1048576, %r15
	andq	%rsi, %r15
	movq	%r15, 24(%rsi)
	movq	%rsi, %r10
	addq	$40, %rsi
	cmpq	$1, %rcx
	jne	L_true1AE
else.1AF:
	/* Liveout:  GP={%rdi}  */
	/* block else<ED7E> (dispatch<ED7D>) */
	movq	%r10, %rdi
	jmp	dispatch.13
L_true1AE:
then.1B0:
	/* block then<ECF5> (ep<ECF2>,s<ECF4>,dispatch<ECF3>) */
	cmpq	$1, (%rcx)
	jne	L1BE
L_true1B1:
then.1B3:
	/* block then<ECFC> (ep<ECF9>,dispatch<ECFB>,s<ECFA>) */
	movq	$-1048576, %r12
	andq	%rsi, %r12
	movq	32(%r12), %rbx
	movq	8(%rbx), %rbx
	cmpq	$1, %rbx
	jne	L_true1B4
else.1B5:
	/* Liveout:  GP={%rax %rdi}  */
	/* block else<ED6E> (ep<ED6D>) */
	movq	$133, -8(%rsi)
	movabsq	$str87, %r12
	movq	%r12, (%rsi)
	movl	$51, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movabsq	$tag74, %r13
	movq	%r13, (%rsi)
	movq	%r10, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	8(%rdx), %r14
	movq	(%r14), %rcx
	movq	%r15, %rax
	movq	%r14, %rdi
	jmp	*%rcx
L_true1B4:
	movq	8(%rcx), %r13
then.1B6:
	/* block then<ED06> (ep<ED02>,dispatch<ED05>,k<ED04>,_t<ED03>) */
	movq	(%rbx), %rbx
	movq	8(%rbx), %r12
	movq	$1801, -8(%rsi)
	movabsq	$letJoinK.1A4, %r14
	movq	%r14, (%rsi)
	movq	24(%rdx), %r15
	movq	%r15, 8(%rsi)
	movq	%r10, 16(%rsi)
	movq	%rbx, 24(%rsi)
	movq	%rsi, %rbx
	addq	$40, %rsi
	cmpq	$1, %r12
	jne	L_true1B7
else.1B8:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block else<ED6B> (k<ED6A>,letJoinK<ED69>) */
	movq	%rbx, %rdi
	movq	%r13, %r8
	jmp	letJoinK.1A4
L_true1B7:
then.1B9:
	/* Liveout:  GP={%r12 %r10 %r9 %r8 %rdi}  */
	/* block then<ED5B> (ep<ED57>,k<ED5A>,c<ED59>,letJoinK<ED58>) */
	movq	$261, -8(%rsi)
	movabsq	$letJoinK.1A9, %rcx
	movq	%rcx, (%rsi)
	movq	%rbx, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	16(%rdx), %rbx
	movq	(%rbx), %rdi
	movq	(%r12), %r8
	movq	%r13, %r9
	movq	8(%rdx), %r12
	jmp	wrap.F3
L1BE:
else.1B2:
	/* Liveout:  GP={%rax %rdi}  */
	/* block else<ED77> (ep<ED76>) */
	movq	$12, -8(%rsi)
	movabsq	$tagE7, %r12
	movq	%r12, (%rsi)
	movq	%rsi, %r14
	addq	$16, %rsi
	movq	8(%rdx), %r13
	movq	(%r13), %r15
	movq	%r14, %rax
	movq	%r13, %rdi
	jmp	*%r15
doGC1BD:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	%r10, %rdi
	movabsq	$retGC1BB, %r8
	jmp	ASM_InvokeGC
	.text
initK.1C0:
	movq	%rax, %rcx
	movq	%rdi, %rdx
	jmp	gcTest1C2
	/* live= GP={%rcx %rdx} spilled=  */
retGC1C1:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest1C2:
	movq	%r11, %r13
	subq	%rsi, %r13
	jle	doGC1C3
check.1BF:
	/* Liveout:  GP={%rax %rdi}  */
	/* block check<10B89> (ep<ED81>,x<ED80>) */
	movq	$1, %r10
	movq	8(%rdx), %rbx
	movq	%r10, %rax
	movq	%rbx, %rdi
	jmp	schedulerLoop.1BA
doGC1C3:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	%r12, %rdi
	movabsq	$retGC1C1, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.21:
	movq	%rdi, %rcx
	jmp	gcTest1C6
	/* live= GP={%rcx} spilled=  */
retGC1C5:
	movq	(%rdi), %rcx
gcTest1C6:
	movq	%r11, %r13
	subq	%rsi, %r13
	jle	doGC1C7
check.1C4:
	/* Liveout:  GP={%rax %rdi}  */
	/* block check<10B8B> (ep<EDAB>) */
	movq	8(%rcx), %rdx
	movq	(%rdx), %rbx
	movq	$1, %r10
	movq	%r10, %rax
	movq	%rdx, %rdi
	jmp	*%rbx
doGC1C7:
	movq	$12, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%rsi, %r12
	addq	$16, %rsi
	movq	%r12, %rdi
	movabsq	$retGC1C5, %r8
	jmp	ASM_InvokeGC
	.text
k.26:
	movq	%rax, %rcx
	movq	%rdi, %rdx
	jmp	gcTest1CA
	/* live= GP={%rcx %rdx} spilled=  */
retGC1C9:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest1CA:
	movq	%r11, %r10
	subq	%rsi, %r10
	jle	doGC1CB
check.1C8:
	/* Liveout:  GP={%rdi}  */
	/* block check<10B8E> (ep<EDBD>,x<EDBC>) */
	movq	8(%rdx), %rdi
	jmp	letJoinK.21
doGC1CB:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	%rbx, %rdi
	movabsq	$retGC1C9, %r8
	jmp	ASM_InvokeGC
	.text
retGC1D4:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
	jmp	gcTest1D5
L_true1CD:
then.1CF:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block then<EDE5> (ep<EDE3>,retK<EDE4>) */
	pause
	movq	%rdx, %rdi
	movq	%rcx, %r8
spinLp.1D3:
	movq	%r8, %rcx
	movq	%rdi, %rdx
gcTest1D5:
	movq	%r11, %r15
	subq	%rsi, %r15
	jle	doGC1D6
check.1CC:
	/* block check<10B91> (ep<EDDC>,retK<EDDD>) */
	movq	(%rdx), %r10
	leaq	(%r10), %rbx
	cmpl	$1, (%rbx)
	je	L_true1CD
else.1CE:
	/* block else<EDE9> (ep<EDE7>,retK<EDE8>) */
	movq	$1, %r12
	movq	(%rdx), %r13
	lock
	xchgq	%r12, (%r13)
	cmpq	$1, %r12
	je	L_true1D0
else.1D1:
	/* Liveout:  GP={%rdi}  */
	/* block else<EDF2> (retK<EDF1>) */
	movq	%rcx, %rdi
	jmp	letJoinK.27
L_true1D0:
then.1D2:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block then<EDEE> (ep<EDEC>,retK<EDED>) */
	movq	%rdx, %rdi
	movq	%rcx, %r8
	jmp	spinLp.1D3
doGC1D6:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	%r14, %rdi
	movabsq	$retGC1D4, %r8
	jmp	ASM_InvokeGC
	/* live= GP={%rcx %rdx} spilled=  */
	jmp	retGC1D4
	.text
spawnFn.1D8:
	movq	%r9, %r14
	movq	%r8, %r12
	movq	%rax, %r13
	movq	%rdi, %rbx
	jmp	gcTest1DA
	/* live= GP={%r14 %r13} spilled= GP={%r~1 %r~1}  */
retGC1D9:
	movq	24(%rdi), %r14
	movq	16(%rdi), %r12
	movq	8(%rdi), %r13
	movq	(%rdi), %rbx
gcTest1DA:
	movq	%r11, %r15
	subq	%rsi, %r15
	jle	doGC1DB
	movq	%r12, -56(%rbp)
	movq	%rbx, -64(%rbp)
check.1D7:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<10B96> (ep<ED89>,thd<ED8A>,retK<ED8B>,exh<ED8C>) */
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
	movabsq	$spinLp.1D3, %r10
	movq	%r10, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	(%rbx), %rdi
	movq	%r15, %r8
	jmp	spinLp.1D3
doGC1DB:
	movq	$36, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%r13, 8(%rsi)
	movq	%r12, 16(%rsi)
	movq	%r14, 24(%rsi)
	movq	%rsi, %r12
	addq	$40, %rsi
	movq	%r12, %rdi
	movabsq	$retGC1D9, %r8
	jmp	ASM_InvokeGC
	.text
removeFn.1DD:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rax, %rbx
	movq	%rdi, %r10
	jmp	gcTest1DF
	/* live= GP={%rcx %rdx %rbx %r10} spilled=  */
retGC1DE:
	movq	24(%rdi), %rcx
	movq	16(%rdi), %rdx
	movq	8(%rdi), %rbx
	movq	(%rdi), %r10
gcTest1DF:
	movq	%r11, %r15
	subq	%rsi, %r15
	jle	doGC1E0
check.1DC:
	/* Liveout:  GP={%rax %rdi}  */
	/* block check<10B9B> (ep<EDF8>,thd<EDF9>,retK<EDFA>,exh<EDFB>) */
	movq	(%rdx), %r12
	movq	$1, %r13
	movq	%r13, %rax
	movq	%rdx, %rdi
	jmp	*%r12
doGC1E0:
	movq	$36, -8(%rsi)
	movq	%r10, (%rsi)
	movq	%rbx, 8(%rsi)
	movq	%rdx, 16(%rsi)
	movq	%rcx, 24(%rsi)
	movq	%rsi, %r14
	addq	$40, %rsi
	movq	%r14, %rdi
	movabsq	$retGC1DE, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.2D:
	movq	%rdi, %rcx
	jmp	gcTest1E3
	/* live= GP={%rcx} spilled=  */
retGC1E2:
	movq	(%rdi), %rcx
gcTest1E3:
	movq	%r11, %r12
	subq	%rsi, %r12
	jle	doGC1E4
check.1E1:
	/* Liveout:  GP={%rdi}  */
	/* block check<10B9D> (ep<EE30>) */
	movq	8(%rcx), %rdx
	movq	(%rdx), %rbx
	movq	%rdx, %rdi
	jmp	*%rbx
doGC1E4:
	movq	$12, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%rsi, %r10
	addq	$16, %rsi
	movq	%r10, %rdi
	movabsq	$retGC1E2, %r8
	jmp	ASM_InvokeGC
	.text
k.2B:
	movq	%rax, %rcx
	movq	%rdi, %rdx
	jmp	gcTest1E7
	/* live= GP={%rcx %rdx} spilled=  */
retGC1E6:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest1E7:
	movq	%r11, %r10
	subq	%rsi, %r10
	jle	doGC1E8
check.1E5:
	/* Liveout:  GP={%rdi}  */
	/* block check<10BA0> (ep<EE41>,x<EE40>) */
	movq	8(%rdx), %rdi
	jmp	letJoinK.2D
doGC1E8:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	%rbx, %rdi
	movabsq	$retGC1E6, %r8
	jmp	ASM_InvokeGC
	.text
retGC1EC:
	movq	(%rdi), %r13
	jmp	gcTest1ED
L_true1EF:
then.1EB:
	/* block then<EE6F> (ep<10BA1>) */
gcTest1ED:
	movq	%r11, %r12
	subq	%rsi, %r12
	jle	doGC1EE
check.1E9:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<10BA2> (ep<EE6E>) */
	pause
	movq	%r13, %rdi
	movq	$1, %r8
lp.1EA:
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
	jne	L_true1EF
else.1F0:
	/* block else<EE74> (ep<EE73>) */
	movq	8(%r13), %r15
	movq	24(%r15), %r14
	cmpq	$1, %r14
	je	letJoinK.1F2
L1F6:
	cmpq	$3, %r14
	je	S_case1F3
S_case1F1:
	jmp	letJoinK.1F2
S_case1F3:
	movq	%r13, -56(%rbp)
case.1F4:
	/* block case<EE7E> (ep<EE7D>) */
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
letJoinK.1F2:
	/* Liveout:  GP={%rdi}  */
	/* block letJoinK<EE78> (ep<EE77>) */
	movq	24(%r13), %rdi
	jmp	letJoinK.2C
doGC1EE:
	movq	$12, -8(%rsi)
	movq	%r13, (%rsi)
	movq	%rsi, %r10
	addq	$16, %rsi
	movq	%r10, %rdi
	movabsq	$retGC1EC, %r8
	jmp	ASM_InvokeGC
	.text
spawnFn.1FC:
	movq	%r9, %r15
	movq	%r8, %r14
	movq	%rdi, %r13
	movq	%r12, -56(%rbp)
	movq	%r10, %rbx
	jmp	gcTest1FE
	/* live= GP={%r13} spilled= GP={%r~1 %r~1 %r~1 %r~1}  */
retGC1FD:
	movq	32(%rdi), %r12
	movq	%r12, -56(%rbp)
	movq	24(%rdi), %rbx
	movq	16(%rdi), %r15
	movl	8(%rdi), %r14d
	movq	(%rdi), %r13
gcTest1FE:
	movq	%r11, %r12
	subq	%rsi, %r12
	jle	doGC1FF
	movq	%r14, -80(%rbp)
	movq	%rbx, -56(%rbp)
	movq	%r15, -64(%rbp)
check.1F7:
	/* block check<10BA8> (ep<EE17>,i<EE18>,k<EE19>,retK<EE1A>,exh<EE1B>) */
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
	jne	L200
L_true1F9:
	movq	-64(%rbp), %r13
then.1FB:
	/* Liveout:  GP={%rdi}  */
	/* block then<EE53> (k<EE52>,fls<EE51>,vp<EE50>,letJoinK<EE4F>) */
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
doGC1FF:
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
	movabsq	$retGC1FD, %r8
	jmp	ASM_InvokeGC
L200:
else.1FA:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block else<EE5C> (k<EE5B>,vp<EE5A>,fls<EE59>,letJoinK<EE58>) */
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
	movabsq	$lp.1EA, %r10
	movq	%r10, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	(%rbx), %rdi
	movq	$1, %r8
	jmp	lp.1EA
	.text
letJoinK.202:
	movq	%rdi, %rcx
	jmp	gcTest204
	/* live= GP={%rcx} spilled=  */
retGC203:
	movq	(%rdi), %rcx
gcTest204:
	movq	%r11, %r12
	subq	%rsi, %r12
	jle	doGC205
check.201:
	/* Liveout:  GP={%rax %rdi}  */
	/* block check<10BAA> (ep<EE96>) */
	movq	$1, %rbx
	movq	8(%rcx), %rdx
	movq	%rbx, %rax
	movq	%rdx, %rdi
	jmp	initK.1C0
doGC205:
	movq	$12, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%rsi, %r10
	addq	$16, %rsi
	movq	%r10, %rdi
	movabsq	$retGC203, %r8
	jmp	ASM_InvokeGC
	.text
init.207:
	movq	%rax, %rcx
	movq	%rdi, %rdx
	jmp	gcTest209
	/* live= GP={%rcx %rdx} spilled=  */
retGC208:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest209:
	movq	%r11, %rbx
	subq	%rsi, %rbx
	jle	doGC20A
check.206:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block check<10BAD> (ep<EE8E>,x<EE8B>) */
	movl	$1, %ebx
	movq	32(%rdx), %r10
	lock
	xaddl	%ebx, 8(%r10)
	movq	$261, -8(%rsi)
	movabsq	$letJoinK.202, %r13
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
	jmp	wait.AA
doGC20A:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	%rcx, %rdi
	movabsq	$retGC208, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.20D:
	movq	%rdi, %rcx
	jmp	gcTest20F
	/* live= GP={%rcx} spilled=  */
retGC20E:
	movq	(%rdi), %rcx
gcTest20F:
	movq	%r11, %r12
	subq	%rsi, %r12
	jle	doGC210
check.20B:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block check<10BAF> (ep<EEB5>) */
	movq	8(%rcx), %rdx
	movq	(%rdx), %rdi
	movl	16(%rcx), %ebx
	movq	%rbx, %r8
	incl	%r8d
	movq	24(%rcx), %r9
	movq	$1, %r10
	jmp	spawn.20C
doGC210:
	movq	$12, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%rsi, %r10
	addq	$16, %rsi
	movq	%r10, %rdi
	movabsq	$retGC20E, %r8
	jmp	ASM_InvokeGC
	.text
spawn.20C:
	movq	%r9, %rdx
	movq	%r8, %rcx
	movq	%rdi, %rbx
	jmp	gcTest217
	/* live= GP={%r10 %rdx %rcx %rbx} spilled=  */
retGC216:
	movq	24(%rdi), %r10
	movq	16(%rdi), %rdx
	movl	8(%rdi), %ecx
	movq	(%rdi), %rbx
gcTest217:
	movq	%r11, %r12
	subq	%rsi, %r12
	jle	doGC218
check.211:
	/* block check<10BB4> (ep<EEA3>,i<EEA4>,retK<EEA5>,exh<EEA6>) */
	cmpl	8(%rbx), %ecx
	jl	L219
L_true212:
then.214:
	/* Liveout:  GP={%rdi}  */
	/* block then<EEAD> (retK<EEAC>) */
	movq	%rdx, %rdi
	jmp	letJoinK.215
doGC218:
	movq	$1673, -8(%rsi)
	movq	%rbx, (%rsi)
	movl	%ecx, 8(%rsi)
	movq	%rdx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %r15
	addq	$40, %rsi
	movq	%r15, %rdi
	movabsq	$retGC216, %r8
	jmp	ASM_InvokeGC
L219:
else.213:
	/* Liveout:  GP={%r12 %r10 %r9 %r8 %rdi}  */
	/* block else<EEB2> (ep<EEAF>,i<EEB1>,retK<EEB0>) */
	movq	$133, -8(%rsi)
	movq	%rbx, (%rsi)
	movabsq	$spawn.20C, %r12
	movq	%r12, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	$1289, -8(%rsi)
	movabsq	$letJoinK.20D, %r13
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
	jmp	spawnFn.1FC
	.text
anon.21C:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest21E
	/* live= GP={%r10 %rcx %rdx %rbx} spilled=  */
retGC21D:
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest21E:
	movq	%r11, %r12
	subq	%rsi, %r12
	jle	doGC21F
check.21A:
	/* Liveout:  GP={%rax %rdi}  */
	/* block check<10BB9> (ep<EEFC>,param<EEFD>,retK<EEFE>,_exh<EEFF>) */
	movq	$133, -8(%rsi)
	movabsq	$str21B, %r14
	movq	%r14, (%rsi)
	movl	$30, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movabsq	$tag74, %r15
	movq	%r15, (%rsi)
	movq	%r13, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	(%r10), %rcx
	movq	%r12, %rax
	movq	%r10, %rdi
	jmp	*%rcx
doGC21F:
	movq	$36, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %rdx
	addq	$40, %rsi
	movq	%rdx, %rdi
	movabsq	$retGC21D, %r8
	jmp	ASM_InvokeGC
	.text
subInBounds.236:
	movq	%r12, %rdx
	movq	%r10, %r14
	movq	%r9, %r15
	movq	%r8, %rbx
	movq	%rdi, %r12
	jmp	gcTest238
	/* live= GP={%rdx %r14 %r15 %rbx %r12} spilled=  */
retGC237:
	movq	32(%rdi), %rdx
	movl	24(%rdi), %r14d
	movq	16(%rdi), %r15
	movq	8(%rdi), %rbx
	movq	(%rdi), %r12
gcTest238:
	movq	%r11, %r13
	subq	%rsi, %r13
	jg	L23A
doGC239:
	movq	$2827, -8(%rsi)
	movq	%r12, (%rsi)
	movq	%rbx, 8(%rsi)
	movq	%r15, 16(%rsi)
	movl	%r14d, 24(%rsi)
	movq	%rdx, 32(%rsi)
	movq	%rsi, %r12
	addq	$48, %rsi
	movq	%r12, %rdi
	movabsq	$retGC237, %r8
	jmp	ASM_InvokeGC
L23A:
check.220:
	/* block check<10BC7> (ep<EF1B>,r<EF1C>,i<EF1D>,_t<EF1E>,retK<EF1F>) */
	movq	(%rbx), %r10
	cmpq	$1, %r10
	je	S_case221
	cmpq	$3, %r10
	je	S_case223
S_case221:
	movq	%r15, %r10
	movq	%rdx, %rcx
	movq	%r12, %r13
case.222:
	/* block case<EF2A> (ep<EF25>,retK<EF29>,r<EF28>,i<EF27>,_t<EF26>) */
	movq	24(%rbx), %rdx
	movq	32(%rbx), %r12
	movq	(%rdx), %r15
	cmpq	$1, %r15
	jne	L23B
S_case225:
	movq	%r12, %r15
case.226:
	/* block case<EF66> (ep<EF60>,retK<EF65>,i<EF64>,_anon_<EF63>,_anon_<EF62>,_t<EF61>) */
	movq	%r14, %rbx
	movq	%r15, %r12
	movq	%r13, %r14
	movl	16(%rdx), %r13d
	jmp	letJoinK.229
S_case223:
case.224:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block case<EF76> (retK<EF75>,r<EF74>,_t<EF73>) */
	movq	(%rdx), %r12
	movq	%rdx, %rdi
	movq	16(%rbx), %r13
	shlq	$3, %r14
	movq	(%r13,%r14,1), %r8
	jmp	*%r12
L23B:
	cmpq	$3, %r15
	jne	S_case225
S_case227:
	movq	%r14, %rbx
	movq	%r13, %r14
case.228:
	/* block case<EF6F> (ep<EF69>,retK<EF6E>,i<EF6D>,_anon_<EF6C>,_anon_<EF6B>,_t<EF6A>) */
	movl	8(%rdx), %r13d
letJoinK.229:
	/* block letJoinK<EF37> (ep<10BBA>,retK<10BBB>,i<10BBC>,_anon_<10BBD>,_anon_<10BBE>,_t<10BBF>,_t<10BC0>) */
	movq	%rbx, %r15
	movq	%r12, %rbx
	movq	%rdx, %r12
	movq	%r10, %rdx
	movq	%rcx, %r10
gcTest22C:
	movq	%r11, %rcx
	subq	%rsi, %rcx
	jle	doGC22D
	movq	%rbx, %rcx
	movq	%rdx, %rbx
	movq	%r10, %rdx
check.22A:
	/* block check<10BC1> (ep<EF30>,retK<EF36>,i<EF35>,_anon_<EF34>,_anon_<EF33>,_t<EF32>,_t<EF31>) */
	cmpl	%r13d, %r15d
	jge	L23C
L_true22E:
	movq	%r12, %rcx
	movq	%rbx, %r15
	movq	%rdx, %r12
then.230:
	/* Liveout:  GP={%r12 %r10 %r9 %r8 %rdi}  */
	/* block then<EF3C> (ep<EF38>,retK<EF3B>,i<EF3A>,_anon_<EF39>) */
	movq	%r14, %rdi
	movq	%rcx, %r8
	movq	%r15, %r9
	movl	(%r15), %r10d
	jmp	subInBounds.236
doGC22D:
	movq	$3855, -8(%rsi)
	movq	%r14, (%rsi)
	movq	%r10, 8(%rsi)
	movq	%rdx, 16(%rsi)
	movq	%r12, 24(%rsi)
	movq	%rbx, 32(%rsi)
	movl	%r15d, 40(%rsi)
	movl	%r13d, 48(%rsi)
	movq	%rsi, %rcx
	addq	$64, %rsi
	movq	%rcx, %rdi
	movabsq	$retGC22B, %r8
	jmp	ASM_InvokeGC
L23C:
	movq	%r15, %r13
	movq	%rcx, %rbx
	movq	%r12, %r10
	movq	%rdx, %r15
else.22F:
	/* block else<EF44> (ep<EF3F>,retK<EF43>,_anon_<EF42>,_anon_<EF41>,_t<EF40>) */
	movq	(%r10), %rdx
	cmpq	$1, %rdx
	jne	L23D
S_case231:
case.232:
	/* block case<EF55> (ep<EF50>,retK<EF54>,_anon_<EF53>,_anon_<EF52>,_t<EF51>) */
	movl	16(%r10), %ecx
	jmp	letJoinK.235
L23D:
	cmpq	$3, %rdx
	jne	S_case231
S_case233:
case.234:
	/* block case<EF5D> (ep<EF58>,retK<EF5C>,_anon_<EF5B>,_anon_<EF5A>,_t<EF59>) */
	movl	8(%r10), %ecx
letJoinK.235:
	/* Liveout:  GP={%r12 %r10 %r9 %r8 %rdi}  */
	/* block letJoinK<EF4C> (ep<EF47>,retK<EF4B>,_anon_<EF4A>,_t<EF49>,_t<EF48>) */
	subl	%ecx, %r13d
	movq	$10, -8(%rsi)
	movl	%r13d, (%rsi)
	movq	%rsi, %r10
	addq	$16, %rsi
	movq	%r14, %rdi
	movq	%rbx, %r8
	movq	%r10, %r9
	movq	%r13, %r10
	movq	%r15, %r12
	jmp	subInBounds.236
	/* live= GP={%r13 %r15 %rcx %r12 %rbx %rdx %r14} spilled=  */
retGC22B:
	movl	48(%rdi), %r13d
	movl	40(%rdi), %r15d
	movq	32(%rdi), %rbx
	movq	24(%rdi), %r12
	movq	16(%rdi), %rdx
	movq	8(%rdi), %r10
	movq	(%rdi), %r14
	jmp	gcTest22C
	.text
leftmostLeaf.248:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest24A
	/* live= GP={%r10 %rcx %rdx %rbx} spilled=  */
retGC249:
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest24A:
	movq	%r11, %r15
	subq	%rsi, %r15
	jg	L24C
doGC24B:
	movq	$36, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %r14
	addq	$40, %rsi
	movq	%r14, %rdi
	movabsq	$retGC249, %r8
	jmp	ASM_InvokeGC
L24C:
check.23E:
	/* block check<10BD0> (ep<EF7D>,r<EF7E>,retK<EF7F>,_exh<EF80>) */
	movq	(%rdx), %r10
	cmpq	$1, %r10
	jne	L24D
S_case23F:
case.240:
	/* block case<EF89> (ep<10BC8>,r<10BC9>,retK<10BCA>) */
gcTest246:
	movq	%r11, %r13
	subq	%rsi, %r13
	jg	L24E
doGC247:
	movq	$28, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%rsi, %r12
	addq	$32, %rsi
	movq	%r12, %rdi
	movabsq	$retGC245, %r8
	jmp	ASM_InvokeGC
L24E:
check.244:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block check<10BCB> (ep<EF86>,r<EF88>,retK<EF87>) */
	movq	%rbx, %rdi
	movq	24(%rdx), %r8
	movq	%rcx, %r9
	movq	$1, %r10
	jmp	leftmostLeaf.248
L24D:
	cmpq	$3, %r10
	jne	S_case23F
S_case241:
case.242:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block case<EF92> (r<EF91>,retK<EF90>) */
	movq	%rcx, %rdi
	movq	%rdx, %r8
	jmp	letJoinK.243
	/* live= GP={%rcx %rdx %rbx} spilled=  */
retGC245:
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
	jmp	gcTest246
	.text
rightmostLeaf.259:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest25B
	/* live= GP={%r10 %rcx %rdx %rbx} spilled=  */
retGC25A:
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest25B:
	movq	%r11, %r15
	subq	%rsi, %r15
	jg	L25D
doGC25C:
	movq	$36, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %r14
	addq	$40, %rsi
	movq	%r14, %rdi
	movabsq	$retGC25A, %r8
	jmp	ASM_InvokeGC
L25D:
check.24F:
	/* block check<10BD9> (ep<EF96>,r<EF97>,retK<EF98>,_exh<EF99>) */
	movq	(%rdx), %r10
	cmpq	$1, %r10
	jne	L25E
S_case250:
case.251:
	/* block case<EFA2> (ep<10BD1>,r<10BD2>,retK<10BD3>) */
gcTest257:
	movq	%r11, %r13
	subq	%rsi, %r13
	jg	L25F
doGC258:
	movq	$28, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%rsi, %r12
	addq	$32, %rsi
	movq	%r12, %rdi
	movabsq	$retGC256, %r8
	jmp	ASM_InvokeGC
L25F:
check.255:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block check<10BD4> (ep<EF9F>,r<EFA1>,retK<EFA0>) */
	movq	%rbx, %rdi
	movq	32(%rdx), %r8
	movq	%rcx, %r9
	movq	$1, %r10
	jmp	rightmostLeaf.259
L25E:
	cmpq	$3, %r10
	jne	S_case250
S_case252:
case.253:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block case<EFAB> (r<EFAA>,retK<EFA9>) */
	movq	%rcx, %rdi
	movq	%rdx, %r8
	jmp	letJoinK.254
	/* live= GP={%rcx %rdx %rbx} spilled=  */
retGC256:
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
	jmp	gcTest257
	.text
f_P_.262:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest264
	/* live= GP={%r10 %rcx %rdx %rbx} spilled=  */
retGC263:
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest264:
	movq	%r11, %r15
	subq	%rsi, %r15
	jle	doGC265
check.260:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block check<10BDE> (ep<EFE0>,n<EFE1>,retK<EFE2>,_exh<EFE3>) */
	movq	(%rbx), %r10
	movq	(%r10), %rdi
	movl	(%rdx), %r12d
	movl	8(%rbx), %r13d
	leal	(%r12,%r13,1), %r8d
	movq	%rcx, %r9
	jmp	gen.261
doGC265:
	movq	$36, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %r14
	addq	$40, %rsi
	movq	%r14, %rdi
	movabsq	$retGC263, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.267:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest269
	/* live= GP={%r10 %rcx %rdx %rbx} spilled=  */
retGC268:
	movl	24(%rdi), %r10d
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest269:
	movq	%r11, %r12
	subq	%rsi, %r12
	jle	doGC26A
check.266:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<10BE3> (ep<EFF4>,_t<EFF1>,_t<EFF2>,_t<EFF3>) */
	movq	$521, -8(%rsi)
	movq	$3, (%rsi)
	movl	16(%rbx), %r12d
	movl	%r12d, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movl	%r10d, 24(%rsi)
	movq	%rsi, %r10
	addq	$40, %rsi
	movq	8(%rbx), %r13
	movq	(%r13), %r14
	movq	%r13, %rdi
	movq	%r10, %r8
	jmp	*%r14
doGC26A:
	movq	$905, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movl	%r10d, 24(%rsi)
	movq	%rsi, %r15
	addq	$40, %rsi
	movq	%r15, %rdi
	movabsq	$retGC268, %r8
	jmp	ASM_InvokeGC
	.text
elt.26F:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest271
	/* live= GP={%r10 %rcx %rdx %rbx} spilled=  */
retGC270:
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest271:
	movq	%r11, %r15
	subq	%rsi, %r15
	jle	doGC272
check.26B:
	/* block check<10BE8> (ep<F092>,i<F093>,retK<F094>,_exh<F095>) */
	movl	(%rdx), %r13d
	cmpl	8(%rbx), %r13d
	jge	L273
L_true26C:
then.26E:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block then<F09E> (ep<F09B>,i<F09D>,retK<F09C>) */
	movq	(%rcx), %r12
	movq	%rcx, %rdi
	movq	(%rbx), %r13
	movl	(%rdx), %r10d
	shlq	$3, %r10
	movq	(%r13,%r10,1), %r8
	jmp	*%r12
doGC272:
	movq	$36, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %r14
	addq	$40, %rsi
	movq	%r14, %rdi
	movabsq	$retGC270, %r8
	jmp	ASM_InvokeGC
L273:
else.26D:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block else<F0A6> (ep<F0A3>,i<F0A5>,retK<F0A4>) */
	movq	(%rcx), %r14
	movq	%rcx, %rdi
	movq	16(%rbx), %r15
	movl	(%rdx), %r12d
	subl	8(%rbx), %r12d
	shlq	$3, %r12
	movq	(%r15,%r12,1), %r8
	jmp	*%r14
	.text
letJoinK.275:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest277
	/* live= GP={%r10 %rcx %rdx %rbx} spilled=  */
retGC276:
	movl	24(%rdi), %r10d
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest277:
	movq	%r11, %r12
	subq	%rsi, %r12
	jle	doGC278
check.274:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<10BED> (ep<F0B4>,_t<F0B1>,_t<F0B2>,_t<F0B3>) */
	movq	$521, -8(%rsi)
	movq	$3, (%rsi)
	movl	16(%rbx), %r12d
	movl	%r12d, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movl	%r10d, 24(%rsi)
	movq	%rsi, %r10
	addq	$40, %rsi
	movq	8(%rbx), %r13
	movq	(%r13), %r14
	movq	%r13, %rdi
	movq	%r10, %r8
	jmp	*%r14
doGC278:
	movq	$905, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movl	%r10d, 24(%rsi)
	movq	%rsi, %r15
	addq	$40, %rsi
	movq	%r15, %rdi
	movabsq	$retGC276, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.27A:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest27C
	/* live= GP={%rcx %rdx} spilled=  */
retGC27B:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest27C:
	movq	%r11, %rbx
	subq	%rsi, %rbx
	jle	doGC27D
check.279:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<10BF0> (ep<F0D1>,_t<F0D0>) */
	movq	$3083, -8(%rsi)
	movq	$1, (%rsi)
	movl	16(%rdx), %r10d
	movl	%r10d, 8(%rsi)
	movl	32(%rdx), %r12d
	movl	%r12d, 16(%rsi)
	movq	%rcx, 24(%rsi)
	movq	24(%rdx), %r13
	movq	%r13, 32(%rsi)
	movq	%rsi, %rbx
	addq	$48, %rsi
	movq	8(%rdx), %r14
	movq	(%r14), %r15
	movq	%r14, %rdi
	movq	%rbx, %r8
	jmp	*%r15
doGC27D:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	%rcx, %rdi
	movabsq	$retGC27B, %r8
	jmp	ASM_InvokeGC
	.text
L287:
	cmpq	$1, %r10
	jne	S_case27F
S_case281:
case.282:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block case<F0C8> (ep<F0C4>,r<F0C7>,retK<F0C6>,_exh<F0C5>) */
	movq	$1291, -8(%rsi)
	movabsq	$letJoinK.27A, %r14
	movq	%r14, (%rsi)
	movq	%rcx, 8(%rsi)
	movl	8(%rdx), %r15d
	movl	%r15d, 16(%rsi)
	movq	32(%rdx), %rcx
	movq	%rcx, 24(%rsi)
	movl	16(%rdx), %r12d
	movl	16(%rbx), %r14d
	leal	(%r12,%r14,1), %r10d
	movl	%r10d, 32(%rsi)
	movq	%rsi, %r12
	addq	$48, %rsi
	movq	%rbx, %rdi
	movq	24(%rdx), %r8
	movq	%r12, %r9
	movq	%r13, %r10
go.283:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
gcTest285:
	movq	%r11, %r13
	subq	%rsi, %r13
	jle	doGC286
	movq	%r10, %r13
check.27E:
	/* block check<10BF5> (ep<F07A>,r<F07B>,retK<F07C>,_exh<F07D>) */
	movq	(%rdx), %r10
	cmpq	$3, %r10
	jne	L287
S_case27F:
case.280:
	/* Liveout:  GP={%r13 %r12 %r10 %r9 %r8 %rdi}  */
	/* block case<F087> (ep<F083>,r<F086>,retK<F085>,_exh<F084>) */
	movq	$647, -8(%rsi)
	movq	8(%rbx), %r10
	movq	%r10, (%rsi)
	movl	16(%rbx), %r12d
	movl	%r12d, 8(%rsi)
	movq	16(%rdx), %r14
	movq	%r14, 16(%rsi)
	movq	%rsi, %r15
	addq	$32, %rsi
	movq	$133, -8(%rsi)
	movq	%r15, (%rsi)
	movabsq	$elt.26F, %r15
	movq	%r15, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	$10, -8(%rsi)
	movl	16(%rbx), %r14d
	movl	24(%rdx), %r10d
	leal	(%r14,%r10,1), %r12d
	movl	%r12d, (%rsi)
	movq	%rsi, %r10
	addq	$16, %rsi
	movq	$20, -8(%rsi)
	movq	%r10, (%rsi)
	movq	%r15, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	$263, -8(%rsi)
	movabsq	$letJoinK.275, %r12
	movq	%r12, (%rsi)
	movq	%rcx, 8(%rsi)
	movl	8(%rdx), %ecx
	movl	16(%rbx), %edx
	leal	(%rcx,%rdx,1), %r14d
	movl	%r14d, 16(%rsi)
	movq	%rsi, %r12
	addq	$32, %rsi
	movq	(%rbx), %rbx
	movq	(%rbx), %rdi
	movq	(%r15), %r8
	movq	(%r15), %r10
	movl	(%r10), %r9d
	movq	8(%r15), %r10
	jmp	tabulate.7E
doGC286:
	movq	$36, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %r12
	addq	$40, %rsi
	movq	%r12, %rdi
	movabsq	$retGC284, %r8
	jmp	ASM_InvokeGC
	/* live= GP={%r13 %rcx %rdx %rbx} spilled=  */
retGC284:
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
	jmp	gcTest285
	.text
letJoinK.289:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest28B
	/* live= GP={%rcx %rdx} spilled=  */
retGC28A:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest28B:
	movq	%r11, %rbx
	subq	%rsi, %rbx
	jle	doGC28C
check.288:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<10BF8> (ep<F0E0>,_t<F0DC>) */
	movq	$3083, -8(%rsi)
	movq	$1, (%rsi)
	movl	16(%rdx), %r10d
	movl	%r10d, 8(%rsi)
	movl	32(%rdx), %r12d
	movl	%r12d, 16(%rsi)
	movq	%rcx, 24(%rsi)
	movq	24(%rdx), %r13
	movq	%r13, 32(%rsi)
	movq	%rsi, %rbx
	addq	$48, %rsi
	movq	8(%rdx), %r14
	movq	(%r14), %r15
	movq	%r14, %rdi
	movq	%rbx, %r8
	jmp	*%r15
doGC28C:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	%rcx, %rdi
	movabsq	$retGC28A, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.243:
	movq	%r8, %rcx
	movq	%rdi, %rdx
gcTest297:
	movq	%r11, %rbx
	subq	%rsi, %rbx
	jle	doGC298
check.28D:
	/* block check<10BFB> (ep<F065>,lmost<F060>) */
	movq	(%rcx), %r12
	cmpq	$1, %r12
	jne	L299
S_case28E:
case.28F:
	/* block case<F0FE> (ep<F0FC>,lmost<F0FD>) */
	movl	16(%rcx), %r10d
letJoinK.292:
	/* block letJoinK<F06C> (ep<F06A>,_t<F06B>) */
	movl	40(%rdx), %r14d
	leal	(%r14,%r10,1), %r13d
	cmpl	16(%rdx), %r13d
	jle	L_true293
else.294:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block else<F0EF> (ep<F0EE>) */
	movq	$3083, -8(%rsi)
	movq	$1, (%rsi)
	movl	64(%rdx), %ebx
	incl	%ebx
	movl	%ebx, 8(%rsi)
	movl	40(%rdx), %ebx
	movl	72(%rdx), %r10d
	leal	(%rbx,%r10,1), %ecx
	movl	%ecx, 16(%rsi)
	movq	96(%rdx), %r12
	movq	%r12, 24(%rsi)
	movq	104(%rdx), %r13
	movq	%r13, 32(%rsi)
	movq	%rsi, %r15
	addq	$48, %rsi
	movq	24(%rdx), %r14
	movq	(%r14), %rcx
	movq	%r14, %rdi
	movq	%r15, %r8
	jmp	*%rcx
L_true293:
then.295:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block then<F071> (ep<F070>) */
	movq	$391, -8(%rsi)
	movq	8(%rdx), %r10
	movq	%r10, (%rsi)
	movq	48(%rdx), %r12
	movq	%r12, 8(%rsi)
	movl	56(%rdx), %r13d
	movl	%r13d, 16(%rsi)
	movq	%rsi, %rbx
	addq	$32, %rsi
	movq	$133, -8(%rsi)
	movq	%rbx, (%rsi)
	movabsq	$go.283, %r14
	movq	%r14, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	$1291, -8(%rsi)
	movabsq	$letJoinK.289, %rcx
	movq	%rcx, (%rsi)
	movq	24(%rdx), %rbx
	movq	%rbx, 8(%rsi)
	movl	64(%rdx), %r10d
	movl	%r10d, 16(%rsi)
	movq	88(%rdx), %r12
	movq	%r12, 24(%rsi)
	movl	40(%rdx), %r15d
	movl	72(%rdx), %ecx
	leal	(%r15,%rcx,1), %r13d
	movl	%r13d, 32(%rsi)
	movq	%rsi, %r15
	addq	$48, %rsi
	movq	(%r14), %rdi
	movq	80(%rdx), %r8
	movq	%r15, %r9
	movq	32(%rdx), %r10
	jmp	go.283
doGC298:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	movq	%rdx, %rdi
	movabsq	$retGC296, %r8
	jmp	ASM_InvokeGC
	/* live= GP={%rcx %rdx} spilled=  */
retGC296:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
	jmp	gcTest297
L299:
	cmpq	$3, %r12
	jne	S_case28E
S_case290:
case.291:
	/* block case<F103> (ep<F101>,lmost<F102>) */
	movl	8(%rcx), %r10d
	jmp	letJoinK.292
	.text
elt.29E:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest2A0
	/* live= GP={%r10 %rcx %rdx %rbx} spilled=  */
retGC29F:
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest2A0:
	movq	%r11, %r15
	subq	%rsi, %r15
	jle	doGC2A1
check.29A:
	/* block check<10C00> (ep<F122>,i<F123>,retK<F124>,_exh<F125>) */
	movl	(%rdx), %r13d
	cmpl	8(%rbx), %r13d
	jge	L2A2
L_true29B:
then.29D:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block then<F12E> (ep<F12B>,i<F12D>,retK<F12C>) */
	movq	(%rcx), %r12
	movq	%rcx, %rdi
	movq	(%rbx), %r13
	movl	(%rdx), %r10d
	shlq	$3, %r10
	movq	(%r13,%r10,1), %r8
	jmp	*%r12
doGC2A1:
	movq	$36, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %r14
	addq	$40, %rsi
	movq	%r14, %rdi
	movabsq	$retGC29F, %r8
	jmp	ASM_InvokeGC
L2A2:
else.29C:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block else<F136> (ep<F133>,i<F135>,retK<F134>) */
	movq	(%rcx), %r14
	movq	%rcx, %rdi
	movq	16(%rbx), %r15
	movl	(%rdx), %r12d
	subl	8(%rbx), %r12d
	shlq	$3, %r12
	movq	(%r15,%r12,1), %r8
	jmp	*%r14
	.text
letJoinK.2A4:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest2A6
	/* live= GP={%r10 %rcx %rdx %rbx} spilled=  */
retGC2A5:
	movl	24(%rdi), %r10d
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest2A6:
	movq	%r11, %r12
	subq	%rsi, %r12
	jle	doGC2A7
check.2A3:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<10C05> (ep<F144>,_t<F140>,_t<F141>,_t<F142>) */
	movq	$521, -8(%rsi)
	movq	$3, (%rsi)
	movl	16(%rbx), %r12d
	movl	%r12d, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movl	%r10d, 24(%rsi)
	movq	%rsi, %r10
	addq	$40, %rsi
	movq	8(%rbx), %r13
	movq	(%r13), %r14
	movq	%r13, %rdi
	movq	%r10, %r8
	jmp	*%r14
doGC2A7:
	movq	$905, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movl	%r10d, 24(%rsi)
	movq	%rsi, %r15
	addq	$40, %rsi
	movq	%r15, %rdi
	movabsq	$retGC2A5, %r8
	jmp	ASM_InvokeGC
	.text
elt.2AC:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest2AE
	/* live= GP={%r10 %rcx %rdx %rbx} spilled=  */
retGC2AD:
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest2AE:
	movq	%r11, %r15
	subq	%rsi, %r15
	jle	doGC2AF
check.2A8:
	/* block check<10C0A> (ep<F172>,i<F173>,retK<F174>,_exh<F175>) */
	movl	(%rdx), %r13d
	cmpl	8(%rbx), %r13d
	jge	L2B0
L_true2A9:
then.2AB:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block then<F17E> (ep<F17B>,i<F17D>,retK<F17C>) */
	movq	(%rcx), %r12
	movq	%rcx, %rdi
	movq	(%rbx), %r13
	movl	(%rdx), %r10d
	shlq	$3, %r10
	movq	(%r13,%r10,1), %r8
	jmp	*%r12
doGC2AF:
	movq	$36, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %r14
	addq	$40, %rsi
	movq	%r14, %rdi
	movabsq	$retGC2AD, %r8
	jmp	ASM_InvokeGC
L2B0:
else.2AA:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block else<F186> (ep<F183>,i<F185>,retK<F184>) */
	movq	(%rcx), %r14
	movq	%rcx, %rdi
	movq	16(%rbx), %r15
	movl	(%rdx), %r12d
	subl	8(%rbx), %r12d
	shlq	$3, %r12
	movq	(%r15,%r12,1), %r8
	jmp	*%r14
	.text
letJoinK.2B2:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest2B4
	/* live= GP={%r10 %rcx %rdx %rbx} spilled=  */
retGC2B3:
	movl	24(%rdi), %r10d
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest2B4:
	movq	%r11, %r12
	subq	%rsi, %r12
	jle	doGC2B5
check.2B1:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<10C0F> (ep<F1AB>,s2'<F1A3>,_t<F1A4>,_t<F1A5>) */
	movq	$521, -8(%rsi)
	movq	$3, (%rsi)
	movl	8(%rbx), %r13d
	movl	%r13d, 8(%rsi)
	movq	48(%rbx), %r14
	movq	%r14, 16(%rsi)
	movl	56(%rbx), %r15d
	movl	%r15d, 24(%rsi)
	movq	%rsi, %r13
	addq	$40, %rsi
	movq	$521, -8(%rsi)
	movq	$3, (%rsi)
	movl	32(%rbx), %r12d
	subl	40(%rbx), %r12d
	movl	%r12d, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movl	%r10d, 24(%rsi)
	movq	%rsi, %rcx
	addq	$40, %rsi
	movq	$3083, -8(%rsi)
	movq	$1, (%rsi)
	movl	$1, 8(%rsi)
	movl	24(%rbx), %r14d
	movl	32(%rbx), %r15d
	leal	(%r14,%r15,1), %r10d
	movl	%r10d, 16(%rsi)
	movq	%r13, 24(%rsi)
	movq	%rcx, 32(%rsi)
	movq	%rsi, %rdx
	addq	$48, %rsi
	movq	16(%rbx), %rcx
	movq	(%rcx), %rbx
	movq	%rcx, %rdi
	movq	%rdx, %r8
	jmp	*%rbx
doGC2B5:
	movq	$905, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movl	%r10d, 24(%rsi)
	movq	%rsi, %r10
	addq	$40, %rsi
	movq	%r10, %rdi
	movabsq	$retGC2B3, %r8
	jmp	ASM_InvokeGC
	.text
anon.2B7:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest2B9
	/* live= GP={%r10 %rcx %rdx %rbx} spilled=  */
retGC2B8:
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest2B9:
	movq	%r11, %r13
	subq	%rsi, %r13
	jle	doGC2BA
check.2B6:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<10C14> (ep<F1D4>,i<F1D5>,retK<F1D6>,_exh<F1D7>) */
	movq	(%rcx), %r12
	movq	%rcx, %rdi
	movq	(%rbx), %r14
	movl	(%rdx), %r15d
	movl	8(%rbx), %r13d
	leal	(%r15,%r13,1), %r10d
	shlq	$3, %r10
	movq	(%r14,%r10,1), %r8
	jmp	*%r12
doGC2BA:
	movq	$36, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %r12
	addq	$40, %rsi
	movq	%r12, %rdi
	movabsq	$retGC2B8, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.2BF:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest2C1
	/* live= GP={%r10 %rcx %rdx %rbx} spilled=  */
retGC2C0:
	movl	24(%rdi), %r10d
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest2C1:
	movq	%r11, %r12
	subq	%rsi, %r12
	jle	doGC2C2
check.2BB:
	/* block check<10C19> (ep<F1A0>,s1'<F191>,unused<F192>,unused<F193>) */
	movq	$8721, -8(%rsi)
	movabsq	$letJoinK.2B2, %r12
	movq	%r12, (%rsi)
	movl	24(%rbx), %r13d
	movl	%r13d, 8(%rsi)
	movq	48(%rbx), %r14
	movq	%r14, 16(%rsi)
	movl	64(%rbx), %r15d
	movl	%r15d, 24(%rsi)
	movl	72(%rbx), %edx
	movl	%edx, 32(%rsi)
	movl	96(%rbx), %r12d
	movl	%r12d, 40(%rsi)
	movq	%rcx, 48(%rsi)
	movl	%r10d, 56(%rsi)
	movq	%rsi, %r12
	addq	$72, %rsi
	movl	96(%rbx), %r13d
	cmpl	88(%rbx), %r13d
	jl	L2C3
L_true2BC:
then.2BE:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block then<F1C4> (ep<F1C2>,letJoinK<F1C3>) */
	movq	(%r12), %rdx
	movq	%r12, %rdi
	movq	16(%rbx), %r8
	movq	32(%rbx), %r9
	movl	40(%rbx), %r10d
	jmp	*%rdx
doGC2C2:
	movq	$905, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movl	%r10d, 24(%rsi)
	movq	%rsi, %rbx
	addq	$40, %rsi
	movq	%rbx, %rdi
	movabsq	$retGC2C0, %r8
	jmp	ASM_InvokeGC
L2C3:
else.2BD:
	/* Liveout:  GP={%r13 %r12 %r10 %r9 %r8 %rdi}  */
	/* block else<F1CB> (ep<F1C9>,letJoinK<F1CA>) */
	movq	$10, -8(%rsi)
	movl	88(%rbx), %r10d
	subl	96(%rbx), %r10d
	movl	%r10d, (%rsi)
	movq	%rsi, %r14
	addq	$16, %rsi
	movq	$133, -8(%rsi)
	movq	80(%rbx), %rcx
	movq	%rcx, (%rsi)
	movl	96(%rbx), %edx
	movl	%edx, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	$133, -8(%rsi)
	movq	%r15, (%rsi)
	movabsq	$anon.2B7, %r13
	movq	%r13, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movq	%r14, (%rsi)
	movq	%r10, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	8(%rbx), %r15
	movq	(%r15), %rdi
	movq	(%r14), %r8
	movq	(%r14), %rcx
	movl	(%rcx), %r9d
	movq	8(%r14), %r10
	movq	56(%rbx), %r13
	jmp	tabulate.7E
	.text
letJoinK.2C5:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest2C7
	/* live= GP={%r10 %rcx %rdx %rbx} spilled=  */
retGC2C6:
	movl	24(%rdi), %r10d
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest2C7:
	movq	%r11, %r12
	subq	%rsi, %r12
	jle	doGC2C8
check.2C4:
	/* Liveout:  GP={%r13 %r12 %r10 %r9 %r8 %rdi}  */
	/* block check<10C1E> (ep<F16B>,_t<F161>,unused<F162>,_t<F163>) */
	movq	$647, -8(%rsi)
	movq	72(%rbx), %r13
	movq	%r13, (%rsi)
	movl	80(%rbx), %r14d
	movl	%r14d, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%rsi, %r12
	addq	$32, %rsi
	movq	$133, -8(%rsi)
	movq	%r12, (%rsi)
	movabsq	$elt.2AC, %rcx
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	$10, -8(%rsi)
	movl	80(%rbx), %r12d
	leal	(%r12,%r10,1), %r10d
	movl	%r10d, (%rsi)
	movq	%rsi, %rdx
	addq	$16, %rsi
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%r15, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	$158491, -8(%rsi)
	movabsq	$letJoinK.2BF, %r13
	movq	%r13, (%rsi)
	movq	8(%rbx), %r14
	movq	%r14, 8(%rsi)
	movq	16(%rbx), %r15
	movq	%r15, 16(%rsi)
	movl	24(%rbx), %ecx
	movl	%ecx, 24(%rsi)
	movq	32(%rbx), %rdx
	movq	%rdx, 32(%rsi)
	movl	40(%rbx), %r12d
	movl	%r12d, 40(%rsi)
	movq	48(%rbx), %r13
	movq	%r13, 48(%rsi)
	movq	56(%rbx), %r14
	movq	%r14, 56(%rsi)
	movl	64(%rbx), %r15d
	movl	%r15d, 64(%rsi)
	movl	88(%rbx), %ecx
	movl	%ecx, 72(%rsi)
	movq	96(%rbx), %rdx
	movq	%rdx, 80(%rsi)
	movl	104(%rbx), %r12d
	movl	%r12d, 88(%rsi)
	movl	112(%rbx), %r13d
	movl	%r13d, 96(%rsi)
	movq	%rsi, %r12
	addq	$112, %rsi
	movq	8(%rbx), %r14
	movq	(%r14), %rdi
	movq	(%r10), %r8
	movq	(%r10), %r15
	movl	(%r15), %r9d
	movq	8(%r10), %r10
	movq	56(%rbx), %r13
	jmp	tabulate.7E
doGC2C8:
	movq	$905, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movl	%r10d, 24(%rsi)
	movq	%rsi, %rcx
	addq	$40, %rsi
	movq	%rcx, %rdi
	movabsq	$retGC2C6, %r8
	jmp	ASM_InvokeGC
	.text
anon.2CA:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest2CC
	/* live= GP={%r10 %rcx %rdx %rbx} spilled=  */
retGC2CB:
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest2CC:
	movq	%r11, %r15
	subq	%rsi, %r15
	jle	doGC2CD
check.2C9:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<10C23> (ep<F202>,i<F203>,retK<F204>,_exh<F205>) */
	movq	(%rcx), %r12
	movq	%rcx, %rdi
	movq	(%rbx), %r13
	movl	(%rdx), %r10d
	shlq	$3, %r10
	movq	(%r13,%r10,1), %r8
	jmp	*%r12
doGC2CD:
	movq	$36, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %r14
	addq	$40, %rsi
	movq	%r14, %rdi
	movabsq	$retGC2CB, %r8
	jmp	ASM_InvokeGC
	.text
elt.2D2:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest2D4
	/* live= GP={%r10 %rcx %rdx %rbx} spilled=  */
retGC2D3:
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest2D4:
	movq	%r11, %r15
	subq	%rsi, %r15
	jle	doGC2D5
check.2CE:
	/* block check<10C28> (ep<F266>,i<F267>,retK<F268>,_exh<F269>) */
	movl	(%rdx), %r13d
	cmpl	16(%rbx), %r13d
	jge	L2D6
L_true2CF:
then.2D1:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block then<F272> (ep<F26F>,i<F271>,retK<F270>) */
	movq	(%rcx), %r12
	movq	%rcx, %rdi
	movq	8(%rbx), %r13
	movl	(%rdx), %r10d
	shlq	$3, %r10
	movq	(%r13,%r10,1), %r8
	jmp	*%r12
doGC2D5:
	movq	$36, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %r14
	addq	$40, %rsi
	movq	%r14, %rdi
	movabsq	$retGC2D3, %r8
	jmp	ASM_InvokeGC
L2D6:
else.2D0:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block else<F27A> (ep<F277>,i<F279>,retK<F278>) */
	movq	(%rcx), %r14
	movq	%rcx, %rdi
	movq	(%rbx), %r15
	movl	(%rdx), %r12d
	subl	16(%rbx), %r12d
	shlq	$3, %r12
	movq	(%r15,%r12,1), %r8
	jmp	*%r14
	.text
letJoinK.2D8:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest2DA
	/* live= GP={%r10 %rcx %rdx %rbx} spilled=  */
retGC2D9:
	movl	24(%rdi), %r10d
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest2DA:
	movq	%r11, %r12
	subq	%rsi, %r12
	jle	doGC2DB
check.2D7:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<10C2D> (ep<F288>,_t<F285>,_t<F286>,_t<F287>) */
	movq	$521, -8(%rsi)
	movq	$3, (%rsi)
	movl	16(%rbx), %r12d
	movl	%r12d, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movl	%r10d, 24(%rsi)
	movq	%rsi, %r10
	addq	$40, %rsi
	movq	8(%rbx), %r13
	movq	(%r13), %r14
	movq	%r13, %rdi
	movq	%r10, %r8
	jmp	*%r14
doGC2DB:
	movq	$905, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movl	%r10d, 24(%rsi)
	movq	%rsi, %r15
	addq	$40, %rsi
	movq	%r15, %rdi
	movabsq	$retGC2D9, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.2DD:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest2DF
	/* live= GP={%rcx %rdx} spilled=  */
retGC2DE:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest2DF:
	movq	%r11, %rbx
	subq	%rsi, %rbx
	jle	doGC2E0
check.2DC:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<10C30> (ep<F2A5>,_t<F2A4>) */
	movq	$3083, -8(%rsi)
	movq	$1, (%rsi)
	movl	16(%rdx), %r10d
	movl	%r10d, 8(%rsi)
	movl	32(%rdx), %r12d
	movl	%r12d, 16(%rsi)
	movq	24(%rdx), %r13
	movq	%r13, 24(%rsi)
	movq	%rcx, 32(%rsi)
	movq	%rsi, %rbx
	addq	$48, %rsi
	movq	8(%rdx), %r14
	movq	(%r14), %r15
	movq	%r14, %rdi
	movq	%rbx, %r8
	jmp	*%r15
doGC2E0:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	%rcx, %rdi
	movabsq	$retGC2DE, %r8
	jmp	ASM_InvokeGC
	.text
L2EA:
	cmpq	$1, %r10
	jne	S_case2E2
S_case2E4:
case.2E5:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block case<F29C> (ep<F298>,r<F29B>,retK<F29A>,_exh<F299>) */
	movq	$1291, -8(%rsi)
	movabsq	$letJoinK.2DD, %r14
	movq	%r14, (%rsi)
	movq	%rcx, 8(%rsi)
	movl	8(%rdx), %r15d
	movl	%r15d, 16(%rsi)
	movq	24(%rdx), %rcx
	movq	%rcx, 24(%rsi)
	movl	16(%rdx), %r12d
	movl	16(%rbx), %r14d
	leal	(%r12,%r14,1), %r10d
	movl	%r10d, 32(%rsi)
	movq	%rsi, %r12
	addq	$48, %rsi
	movq	%rbx, %rdi
	movq	32(%rdx), %r8
	movq	%r12, %r9
	movq	%r13, %r10
go.2E6:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
gcTest2E8:
	movq	%r11, %r13
	subq	%rsi, %r13
	jle	doGC2E9
	movq	%r10, %r13
check.2E1:
	/* block check<10C35> (ep<F24F>,r<F250>,retK<F251>,_exh<F252>) */
	movq	(%rdx), %r10
	cmpq	$3, %r10
	jne	L2EA
S_case2E2:
case.2E3:
	/* Liveout:  GP={%r13 %r12 %r10 %r9 %r8 %rdi}  */
	/* block case<F25C> (ep<F258>,r<F25B>,retK<F25A>,_exh<F259>) */
	movl	24(%rdx), %r14d
	movq	$391, -8(%rsi)
	movq	8(%rbx), %r10
	movq	%r10, (%rsi)
	movq	16(%rdx), %r12
	movq	%r12, 8(%rsi)
	movl	%r14d, 16(%rsi)
	movq	%rsi, %r15
	addq	$32, %rsi
	movq	$133, -8(%rsi)
	movq	%r15, (%rsi)
	movabsq	$elt.2D2, %r15
	movq	%r15, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	$10, -8(%rsi)
	movl	16(%rbx), %r10d
	leal	(%r14,%r10,1), %r12d
	movl	%r12d, (%rsi)
	movq	%rsi, %r10
	addq	$16, %rsi
	movq	$20, -8(%rsi)
	movq	%r10, (%rsi)
	movq	%r15, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	$263, -8(%rsi)
	movabsq	$letJoinK.2D8, %r12
	movq	%r12, (%rsi)
	movq	%rcx, 8(%rsi)
	movl	8(%rdx), %ecx
	movl	16(%rbx), %edx
	leal	(%rcx,%rdx,1), %r14d
	movl	%r14d, 16(%rsi)
	movq	%rsi, %r12
	addq	$32, %rsi
	movq	(%rbx), %rbx
	movq	(%rbx), %rdi
	movq	(%r15), %r8
	movq	(%r15), %r10
	movl	(%r10), %r9d
	movq	8(%r15), %r10
	jmp	tabulate.7E
doGC2E9:
	movq	$36, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %r12
	addq	$40, %rsi
	movq	%r12, %rdi
	movabsq	$retGC2E7, %r8
	jmp	ASM_InvokeGC
	/* live= GP={%r13 %rcx %rdx %rbx} spilled=  */
retGC2E7:
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
	jmp	gcTest2E8
	.text
letJoinK.2EC:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest2EE
	/* live= GP={%rcx %rdx} spilled=  */
retGC2ED:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest2EE:
	movq	%r11, %rbx
	subq	%rsi, %rbx
	jle	doGC2EF
check.2EB:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<10C38> (ep<F2B4>,_t<F2B0>) */
	movq	$3083, -8(%rsi)
	movq	$1, (%rsi)
	movl	16(%rdx), %r10d
	movl	%r10d, 8(%rsi)
	movl	32(%rdx), %r12d
	movl	%r12d, 16(%rsi)
	movq	24(%rdx), %r13
	movq	%r13, 24(%rsi)
	movq	%rcx, 32(%rsi)
	movq	%rsi, %rbx
	addq	$48, %rsi
	movq	8(%rdx), %r14
	movq	(%r14), %r15
	movq	%r14, %rdi
	movq	%rbx, %r8
	jmp	*%r15
doGC2EF:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	%rcx, %rdi
	movabsq	$retGC2ED, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.254:
	movq	%r8, %rcx
	movq	%rdi, %rdx
gcTest2FA:
	movq	%r11, %rbx
	subq	%rsi, %rbx
	jle	doGC2FB
check.2F0:
	/* block check<10C3B> (ep<F23A>,rmost<F235>) */
	movq	(%rcx), %r12
	cmpq	$1, %r12
	jne	L2FC
S_case2F1:
case.2F2:
	/* block case<F2D2> (ep<F2D0>,rmost<F2D1>) */
	movl	16(%rcx), %r10d
letJoinK.2F5:
	/* block letJoinK<F241> (ep<F23F>,_t<F240>) */
	movl	56(%rdx), %r14d
	leal	(%r10,%r14,1), %r13d
	cmpl	16(%rdx), %r13d
	jle	L_true2F6
else.2F7:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block else<F2C3> (ep<F2C2>) */
	movq	$3083, -8(%rsi)
	movq	$1, (%rsi)
	movl	40(%rdx), %ebx
	incl	%ebx
	movl	%ebx, 8(%rsi)
	movl	48(%rdx), %ebx
	movl	56(%rdx), %r10d
	leal	(%rbx,%r10,1), %ecx
	movl	%ecx, 16(%rsi)
	movq	96(%rdx), %r12
	movq	%r12, 24(%rsi)
	movq	104(%rdx), %r13
	movq	%r13, 32(%rsi)
	movq	%rsi, %r15
	addq	$48, %rsi
	movq	24(%rdx), %r14
	movq	(%r14), %rcx
	movq	%r14, %rdi
	movq	%r15, %r8
	jmp	*%rcx
L_true2F6:
then.2F8:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block then<F246> (ep<F245>) */
	movq	$391, -8(%rsi)
	movq	8(%rdx), %r10
	movq	%r10, (%rsi)
	movq	64(%rdx), %r12
	movq	%r12, 8(%rsi)
	movl	72(%rdx), %r13d
	movl	%r13d, 16(%rsi)
	movq	%rsi, %rbx
	addq	$32, %rsi
	movq	$133, -8(%rsi)
	movq	%rbx, (%rsi)
	movabsq	$go.2E6, %r14
	movq	%r14, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	$1291, -8(%rsi)
	movabsq	$letJoinK.2EC, %rcx
	movq	%rcx, (%rsi)
	movq	24(%rdx), %rbx
	movq	%rbx, 8(%rsi)
	movl	40(%rdx), %r10d
	movl	%r10d, 16(%rsi)
	movq	80(%rdx), %r12
	movq	%r12, 24(%rsi)
	movl	48(%rdx), %r15d
	movl	56(%rdx), %ecx
	leal	(%r15,%rcx,1), %r13d
	movl	%r13d, 32(%rsi)
	movq	%rsi, %r15
	addq	$48, %rsi
	movq	(%r14), %rdi
	movq	88(%rdx), %r8
	movq	%r15, %r9
	movq	32(%rdx), %r10
	jmp	go.2E6
doGC2FB:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	movq	%rdx, %rdi
	movabsq	$retGC2F9, %r8
	jmp	ASM_InvokeGC
	/* live= GP={%rcx %rdx} spilled=  */
retGC2F9:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
	jmp	gcTest2FA
L2FC:
	cmpq	$3, %r12
	jne	S_case2F1
S_case2F3:
case.2F4:
	/* block case<F2D7> (ep<F2D5>,rmost<F2D6>) */
	movl	8(%rcx), %r10d
	jmp	letJoinK.2F5
	.text
letJoinK.344:
	movq	%r9, %rdx
	movq	%r8, %r13
	movq	%rdi, %rcx
	jmp	gcTest346
	/* live= GP={%r10 %rdx %r13 %rcx} spilled=  */
retGC345:
	movq	24(%rdi), %r10
	movq	16(%rdi), %rdx
	movq	8(%rdi), %r13
	movq	(%rdi), %rcx
gcTest346:
	movq	%r11, %r14
	subq	%rsi, %r14
	jle	doGC347
check.2FD:
	/* block check<10C40> (ep<F024>,_t<F01A>,r1<F01B>,r2<F01C>) */
	movq	(%rdx), %r14
	cmpq	$1, %r14
	je	S_case2FE
	cmpq	$3, %r14
	je	S_case300
S_case2FE:
case.2FF:
	/* block case<F394> (ep<F390>,_t<F393>,r1<F392>,r2<F391>) */
	cmpl	$0, 16(%rdx)
	je	L_true30B
else.30A:
	/* block else<F3A2> (ep<F39E>,_t<F3A1>,r1<F3A0>,r2<F39F>) */
	movq	$1, %r15
	jmp	letJoinK.305
doGC347:
	movq	$36, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%r13, 8(%rsi)
	movq	%rdx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %r13
	addq	$40, %rsi
	movq	%r13, %rdi
	movabsq	$retGC345, %r8
	jmp	ASM_InvokeGC
S_case300:
case.301:
	/* block case<F3A8> (ep<F3A4>,_t<F3A7>,r1<F3A6>,r2<F3A5>) */
	cmpl	$0, 8(%rdx)
	je	L_true302
else.303:
	/* block else<F3B6> (ep<F3B2>,_t<F3B5>,r1<F3B4>,r2<F3B3>) */
	movq	$1, %r15
letJoinK.305:
	/* block letJoinK<F02E> (ep<F029>,_t<F02D>,r1<F02C>,r2<F02B>,_t<F02A>) */
	cmpq	$1, %r15
	jne	L348
S_case306:
case.307:
	/* block case<F033> (ep<F02F>,_t<F032>,r1<F031>,r2<F030>) */
	movq	(%r10), %rbx
	cmpq	$1, %rbx
	jne	L349
S_case30D:
case.30E:
	/* block case<F367> (ep<F363>,_t<F366>,r1<F365>,r2<F364>) */
	cmpl	$0, 16(%r10)
	je	L_true31A
else.319:
	/* block else<F375> (ep<F371>,_t<F374>,r1<F373>,r2<F372>) */
	movq	$1, %rbx
	jmp	letJoinK.314
L_true31A:
then.31B:
	/* block then<F36F> (ep<F36B>,_t<F36E>,r1<F36D>,r2<F36C>) */
	movq	$3, %rbx
	jmp	letJoinK.314
L349:
	cmpq	$3, %rbx
	jne	S_case30D
S_case30F:
case.310:
	/* block case<F37B> (ep<F377>,_t<F37A>,r1<F379>,r2<F378>) */
	cmpl	$0, 8(%r10)
	jne	L34A
L_true311:
then.313:
	/* block then<F383> (ep<F37F>,_t<F382>,r1<F381>,r2<F380>) */
	movq	$3, %rbx
letJoinK.314:
	/* block letJoinK<F03B> (ep<F036>,_t<F03A>,r1<F039>,r2<F038>,_t<F037>) */
	cmpq	$1, %rbx
	je	S_case315
	cmpq	$3, %rbx
	je	S_case317
S_case315:
case.316:
	/* block case<F040> (ep<F03C>,_t<F03F>,r1<F03E>,r2<F03D>) */
	movq	(%r13), %r14
	movq	8(%r13), %r12
	movq	(%r14), %r13
	cmpq	$3, %r13
	je	S_case31C
	cmpq	$1, %r13
	je	S_case31E
S_case31C:
case.31D:
	/* block case<F048> (ep<F045>,_anon_<F047>,_anon_<F046>) */
	movl	8(%r14), %r15d
	movq	16(%r14), %r13
	movl	24(%r14), %r14d
	movq	(%r12), %rbx
	cmpq	$1, %rbx
	je	S_case33A
	cmpq	$3, %rbx
	jne	S_case33A
S_case33C:
	movq	%r13, -80(%rbp)
case.33D:
	/* block case<F10F> (ep<F10A>,_anon_<F10E>,_t<F10D>,_t<F10C>,_t<F10B>) */
	movl	8(%r12), %ebx
	movq	16(%r12), %rdx
	movl	24(%r12), %r10d
	movq	$133, -8(%rsi)
	movq	%rdx, (%rsi)
	movl	%r10d, 8(%rsi)
	movq	%rsi, -56(%rbp)
	addq	$24, %rsi
	leal	(%r15,%rbx,1), %r12d
	cmpl	24(%rcx), %r12d
	jg	L34B
L_true33E:
	movq	-80(%rbp), %r12
then.340:
	/* Liveout:  GP={%r13 %r12 %r10 %r9 %r8 %rdi}  */
	/* block then<F11E> (ep<F117>,_t<F11D>,_t<F11C>,_t<F11B>,_t<F11A>,_t<F119>,_t<F118>) */
	movq	$647, -8(%rsi)
	movq	%r12, (%rsi)
	movl	%r14d, 8(%rsi)
	movq	%rdx, 16(%rsi)
	movq	%rsi, %rdx
	addq	$32, %rsi
	movq	$133, -8(%rsi)
	movq	%rdx, (%rsi)
	movabsq	$elt.29E, %r13
	movq	%r13, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	$10, -8(%rsi)
	leal	(%r14,%r10,1), %edx
	movl	%edx, (%rsi)
	movq	%rsi, %r14
	addq	$16, %rsi
	movq	$20, -8(%rsi)
	movq	%r14, (%rsi)
	movq	%r12, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	$263, -8(%rsi)
	movabsq	$letJoinK.2A4, %r10
	movq	%r10, (%rsi)
	movq	64(%rcx), %r12
	movq	%r12, 8(%rsi)
	leal	(%r15,%rbx,1), %r13d
	movl	%r13d, 16(%rsi)
	movq	%rsi, %r12
	addq	$32, %rsi
	movq	8(%rcx), %r15
	movq	(%r15), %rdi
	movq	(%r14), %r8
	movq	(%r14), %rdx
	movl	(%rdx), %r9d
	movq	8(%r14), %r10
	movq	72(%rcx), %r13
	jmp	tabulate.7E
S_case317:
case.318:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block case<F360> (ep<F35E>,r1<F35F>) */
	movq	64(%rcx), %r10
	movq	(%r10), %r12
	movq	%r10, %rdi
	movq	%rdx, %r8
	jmp	*%r12
S_case31E:
case.31F:
	/* block case<F21C> (ep<F217>,r1<F21B>,r2<F21A>,_anon_<F219>,_anon_<F218>) */
	cmpq	$3, (%r12)
	je	L_true320
else.321:
	/* block else<F2E1> (ep<F2DE>,r1<F2E0>,r2<F2DF>) */
	movl	$1, %r14d
	movq	(%rdx), %r15
	cmpq	$1, %r15
	je	S_case323
	cmpq	$3, %r15
	je	S_case325
S_case323:
case.324:
	/* block case<F353> (ep<F34F>,r1<F352>,r2<F351>,_lit<F350>) */
	movq	$10, -8(%rsi)
	movl	8(%rdx), %r13d
	movl	%r13d, (%rsi)
	movq	%rsi, %rbx
	addq	$16, %rsi
letJoinK.327:
	/* block letJoinK<F2EA> (ep<F2E5>,r1<F2E9>,r2<F2E8>,_lit<F2E7>,_t<F2E6>) */
	movq	(%r10), %r12
	cmpq	$1, %r12
	je	S_case328
	cmpq	$3, %r12
	jne	S_case328
S_case32A:
case.32B:
	/* block case<F34D> (ep<F348>,r1<F34C>,r2<F34B>,_lit<F34A>,_t<F349>) */
	xorl	%r15d, %r15d
letJoinK.32C:
	/* block letJoinK<F2F3> (ep<F2ED>,r1<F2F2>,r2<F2F1>,_lit<F2F0>,_t<F2EF>,_t<F2EE>) */
	cmpl	%r15d, (%rbx)
	jge	L34C
L_true32D:
letJoinK.32F:
	/* block letJoinK<F2FA> (ep<F2F5>,r1<F2F9>,r2<F2F8>,_lit<F2F7>,_t<F2F6>) */
	leal	(%r14,%r15,1), %r15d
	movq	(%rdx), %r14
	cmpq	$1, %r14
	jne	L34D
S_case330:
case.331:
	/* block case<F327> (ep<F323>,r1<F326>,r2<F325>,_t<F324>) */
	movq	$10, -8(%rsi)
	movl	16(%rdx), %r13d
	movl	%r13d, (%rsi)
	movq	%rsi, %r13
	addq	$16, %rsi
letJoinK.334:
	/* block letJoinK<F303> (ep<F2FE>,r1<F302>,r2<F301>,_t<F300>,_t<F2FF>) */
	movq	(%r10), %r12
	cmpq	$1, %r12
	je	S_case335
	cmpq	$3, %r12
	je	S_case337
S_case335:
case.336:
	/* block case<F318> (ep<F313>,r1<F317>,r2<F316>,_t<F315>,_t<F314>) */
	movl	16(%r10), %r12d
letJoinK.339:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block letJoinK<F30C> (ep<F306>,r1<F30B>,r2<F30A>,_t<F309>,_t<F308>,_t<F307>) */
	movq	$3083, -8(%rsi)
	movq	$1, (%rsi)
	movl	%r15d, 8(%rsi)
	movl	(%r13), %ebx
	leal	(%rbx,%r12,1), %r15d
	movl	%r15d, 16(%rsi)
	movq	%rdx, 24(%rsi)
	movq	%r10, 32(%rsi)
	movq	%rsi, %r14
	addq	$48, %rsi
	movq	64(%rcx), %r10
	movq	(%r10), %r12
	movq	%r10, %rdi
	movq	%r14, %r8
	jmp	*%r12
L_true320:
	movq	32(%r14), %r15
	movq	24(%r14), %r13
	movq	%r13, -72(%rbp)
	movl	16(%r14), %edx
	movl	8(%r14), %ebx
then.322:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block then<F22B> (ep<F225>,_t<F22A>,_t<F229>,_t<F228>,_t<F227>,_anon_<F226>) */
	movl	8(%r12), %r10d
	movq	16(%r12), %r13
	movl	24(%r12), %r12d
	movq	$3083, -8(%rsi)
	movq	$1, (%rsi)
	movl	%ebx, 8(%rsi)
	movl	%edx, 16(%rsi)
	movq	-72(%rbp), %r14
	movq	%r14, 24(%rsi)
	movq	%r15, 32(%rsi)
	movq	%rsi, -64(%rbp)
	addq	$48, %rsi
	movq	$521, -8(%rsi)
	movq	$3, (%rsi)
	movl	%r10d, 8(%rsi)
	movq	%r13, 16(%rsi)
	movl	%r12d, 24(%rsi)
	movq	%rsi, -56(%rbp)
	addq	$40, %rsi
	movq	$2002205, -8(%rsi)
	movabsq	$letJoinK.254, %r14
	movq	%r14, (%rsi)
	movq	8(%rcx), %r14
	movq	%r14, 8(%rsi)
	movl	24(%rcx), %r14d
	movl	%r14d, 16(%rsi)
	movq	64(%rcx), %r14
	movq	%r14, 24(%rsi)
	movq	72(%rcx), %r14
	movq	%r14, 32(%rsi)
	movl	%ebx, 40(%rsi)
	movl	%edx, 48(%rsi)
	movl	%r10d, 56(%rsi)
	movq	%r13, 64(%rsi)
	movl	%r12d, 72(%rsi)
	movq	-72(%rbp), %rdx
	movq	%rdx, 80(%rsi)
	movq	%r15, 88(%rsi)
	movq	-64(%rbp), %rbx
	movq	%rbx, 96(%rsi)
	movq	-56(%rbp), %r10
	movq	%r10, 104(%rsi)
	movq	%rsi, %r13
	addq	$120, %rsi
	movq	56(%rcx), %rdx
	movq	(%rdx), %rdi
	movq	%r15, %r8
	movq	%r13, %r9
	movq	72(%rcx), %r10
	jmp	rightmostLeaf.259
S_case325:
case.326:
	/* block case<F35B> (ep<F357>,r1<F35A>,r2<F359>,_lit<F358>) */
	movq	$10, -8(%rsi)
	movl	$0, (%rsi)
	movq	%rsi, %rbx
	addq	$16, %rsi
	jmp	letJoinK.327
L34C:
else.32E:
	/* block else<F33E> (ep<F339>,r1<F33D>,r2<F33C>,_lit<F33B>,_t<F33A>) */
	movl	(%rbx), %r15d
	jmp	letJoinK.32F
S_case337:
case.338:
	/* block case<F320> (ep<F31B>,r1<F31F>,r2<F31E>,_t<F31D>,_t<F31C>) */
	movl	8(%r10), %r12d
	jmp	letJoinK.339
S_case328:
case.329:
	/* block case<F345> (ep<F340>,r1<F344>,r2<F343>,_lit<F342>,_t<F341>) */
	movl	8(%r10), %r15d
	jmp	letJoinK.32C
L34D:
	cmpq	$3, %r14
	jne	S_case330
S_case332:
case.333:
	/* block case<F32F> (ep<F32B>,r1<F32E>,r2<F32D>,_t<F32C>) */
	movq	$10, -8(%rsi)
	movl	8(%rdx), %ebx
	movl	%ebx, (%rsi)
	movq	%rsi, %r13
	addq	$16, %rsi
	jmp	letJoinK.334
S_case33A:
	movq	%r14, -64(%rbp)
	movq	%r15, -72(%rbp)
case.33B:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block case<F054> (ep<F04F>,_anon_<F053>,_t<F052>,_t<F051>,_t<F050>) */
	movl	8(%r12), %r14d
	movl	16(%r12), %edx
	movq	24(%r12), %r15
	movq	32(%r12), %rbx
	movq	$521, -8(%rsi)
	movq	$3, (%rsi)
	movq	-72(%rbp), %r10
	movl	%r10d, 8(%rsi)
	movq	%r13, 16(%rsi)
	movq	-64(%rbp), %r12
	movl	%r12d, 24(%rsi)
	movq	%rsi, -56(%rbp)
	addq	$40, %rsi
	movq	$3083, -8(%rsi)
	movq	$1, (%rsi)
	movl	%r14d, 8(%rsi)
	movl	%edx, 16(%rsi)
	movq	%r15, 24(%rsi)
	movq	%rbx, 32(%rsi)
	movq	%rsi, %r12
	addq	$48, %rsi
	movq	$1977629, -8(%rsi)
	movabsq	$letJoinK.243, %r10
	movq	%r10, (%rsi)
	movq	8(%rcx), %r10
	movq	%r10, 8(%rsi)
	movl	24(%rcx), %r10d
	movl	%r10d, 16(%rsi)
	movq	64(%rcx), %r10
	movq	%r10, 24(%rsi)
	movq	72(%rcx), %r10
	movq	%r10, 32(%rsi)
	movq	-72(%rbp), %r10
	movl	%r10d, 40(%rsi)
	movq	%r13, 48(%rsi)
	movq	-64(%rbp), %r13
	movl	%r13d, 56(%rsi)
	movl	%r14d, 64(%rsi)
	movl	%edx, 72(%rsi)
	movq	%r15, 80(%rsi)
	movq	%rbx, 88(%rsi)
	movq	-56(%rbp), %r14
	movq	%r14, 96(%rsi)
	movq	%r12, 104(%rsi)
	movq	%rsi, %rbx
	addq	$120, %rsi
	movq	48(%rcx), %r12
	movq	(%r12), %rdi
	movq	%r15, %r8
	movq	%rbx, %r9
	movq	72(%rcx), %r10
	jmp	leftmostLeaf.248
L34B:
	movq	%rbx, -64(%rbp)
	movq	%r15, -72(%rbp)
else.33F:
	/* block else<F15D> (ep<F155>,_t<F15C>,_t<F15B>,_t<F15A>,_t<F159>,_t<F158>,_t<F157>,_t<F156>) */
	movl	24(%rcx), %r13d
	subl	%r14d, %r13d
	movq	$10, -8(%rsi)
	movl	%r13d, (%rsi)
	movq	%rsi, %rbx
	addq	$16, %rsi
	movq	$617247, -8(%rsi)
	movabsq	$letJoinK.2C5, %r15
	movq	%r15, (%rsi)
	movq	8(%rcx), %r12
	movq	%r12, 8(%rsi)
	movq	16(%rcx), %r15
	movq	%r15, 16(%rsi)
	movl	24(%rcx), %r12d
	movl	%r12d, 24(%rsi)
	movq	32(%rcx), %r15
	movq	%r15, 32(%rsi)
	movl	40(%rcx), %r12d
	movl	%r12d, 40(%rsi)
	movq	64(%rcx), %r15
	movq	%r15, 48(%rsi)
	movq	72(%rcx), %r12
	movq	%r12, 56(%rsi)
	movq	-72(%rbp), %r15
	movl	%r15d, 64(%rsi)
	movq	-80(%rbp), %r12
	movq	%r12, 72(%rsi)
	movl	%r14d, 80(%rsi)
	movq	-64(%rbp), %r14
	movl	%r14d, 88(%rsi)
	movq	%rdx, 96(%rsi)
	movl	%r10d, 104(%rsi)
	movl	%r13d, 112(%rsi)
	movq	%rsi, %r12
	addq	$128, %rsi
	cmpl	%r10d, %r13d
	jl	L34E
L_true341:
	movq	-56(%rbp), %r14
then.343:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block then<F1F9> (_t<F1F8>,_t<F1F7>,_t<F1F6>,letJoinK<F1F5>) */
	movq	(%r12), %r13
	movq	%r12, %rdi
	movq	%r14, %r8
	movq	%rdx, %r9
	jmp	*%r13
L34E:
else.342:
	/* Liveout:  GP={%r13 %r12 %r10 %r9 %r8 %rdi}  */
	/* block else<F1FF> (ep<F1FB>,_t<F1FE>,res<F1FD>,letJoinK<F1FC>) */
	movq	$12, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rsi, %r13
	addq	$16, %rsi
	movq	$133, -8(%rsi)
	movq	%r13, (%rsi)
	movabsq	$anon.2CA, %r15
	movq	%r15, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%r14, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	movq	8(%rcx), %rbx
	movq	(%rbx), %rdi
	movq	(%rdx), %r8
	movq	(%rdx), %r10
	movl	(%r10), %r9d
	movq	8(%rdx), %r10
	movq	72(%rcx), %r13
	jmp	tabulate.7E
L34A:
else.312:
	/* block else<F389> (ep<F385>,_t<F388>,r1<F387>,r2<F386>) */
	movq	$1, %rbx
	jmp	letJoinK.314
L348:
	cmpq	$3, %r15
	jne	S_case306
S_case308:
case.309:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block case<F38D> (ep<F38B>,r2<F38C>) */
	movq	64(%rcx), %r15
	movq	(%r15), %rcx
	movq	%r15, %rdi
	movq	%r10, %r8
	jmp	*%rcx
L_true302:
then.304:
	/* block then<F3B0> (ep<F3AC>,_t<F3AF>,r1<F3AE>,r2<F3AD>) */
	movq	$3, %r15
	jmp	letJoinK.305
L_true30B:
then.30C:
	/* block then<F39C> (ep<F398>,_t<F39B>,r1<F39A>,r2<F399>) */
	movq	$3, %r15
	jmp	letJoinK.305
	.text
letJoinK.353:
	movq	%r8, %rbx
	movq	%rdi, %r12
	jmp	gcTest355
	/* live= GP={%rbx} spilled= GP={%r~1}  */
retGC354:
	movq	8(%rdi), %rbx
	movq	(%rdi), %r12
gcTest355:
	movq	%r11, %r13
	subq	%rsi, %r13
	jle	doGC356
	movq	%r12, -56(%rbp)
check.34F:
	/* flushLoads */
	/* block check<10C43> (ep<F3C3>,v_1<F3C0>) */
	movq	$-1048576, %r12
	andq	%rsi, %r12
	movq	%rsi, 120(%r12)
	movq	$1, (%r12)
	movq	%rax, -80(%rbp)
	movq	%rcx, %r13
	movq	%rdx, %r14
	movq	%rdi, -64(%rbp)
	movq	%r8, -72(%rbp)
	movq	%r9, -88(%rbp)
	movq	%r10, -96(%rbp)
	movq	%r11, %r15
	movq	$-1048576, %rcx
	andq	%rsi, %rcx
	movq	%rcx, %rdi
	movq	%rbx, %rsi
	call	PromoteObj
	movq	%rax, %rbx
	movq	-80(%rbp), %rax
	movq	%r13, %rcx
	movq	%r14, %rdx
	movq	-64(%rbp), %rdi
	movq	-72(%rbp), %r8
	movq	-88(%rbp), %r9
	movq	-96(%rbp), %r10
	movq	%r15, %r11
	movq	120(%r12), %rsi
	movq	$3, (%r12)
	movq	-56(%rbp), %r14
	movq	24(%r14), %r12
	movq	%rbx, 8(%r12)
	movq	$-1048576, %rbx
	andq	%rsi, %rbx
	movq	%rsi, 120(%rbx)
	movq	$1, (%rbx)
	movq	%rax, -72(%rbp)
	movq	%rcx, -80(%rbp)
	movq	%rdx, -88(%rbp)
	movq	%rdi, %r12
	movq	%r8, -64(%rbp)
	movq	%r9, -96(%rbp)
	movq	%r10, %r13
	movq	%r11, %r14
	movq	$-1048576, %r10
	andq	%rsi, %r10
	movq	%r10, %rdi
	movq	-56(%rbp), %r15
	movq	8(%r15), %r15
	movq	%r15, %rsi
	call	PromoteObj
	movq	%rax, %r15
	movq	-72(%rbp), %rax
	movq	-80(%rbp), %rcx
	movq	-88(%rbp), %rdx
	movq	%r12, %rdi
	movq	-64(%rbp), %r8
	movq	-96(%rbp), %r9
	movq	%r13, %r10
	movq	%r14, %r11
	movq	120(%rbx), %rsi
	movq	$3, (%rbx)
	movq	-56(%rbp), %rcx
	movl	$1, %ecx
	lock
	xaddl	%ecx, (%r15)
	cmpl	$2, %ecx
	jne	L357
L_true350:
	movq	-56(%rbp), %r15
then.352:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block then<F3CF> (ep<F3CE>) */
	movq	16(%r15), %rdi
	movq	24(%r15), %r8
	movq	24(%r15), %rcx
	movq	(%rcx), %r9
	movq	24(%r15), %rdx
	movq	8(%rdx), %r10
	jmp	letJoinK.344
doGC356:
	movq	$20, -8(%rsi)
	movq	%r12, (%rsi)
	movq	%rbx, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	%rbx, %rdi
	movabsq	$retGC354, %r8
	jmp	ASM_InvokeGC
L357:
else.351:
	/* Liveout:  GP={%rax %rdi}  */
	/* block else<F3D8> () */
	movq	$-1048576, %rbx
	andq	%rsi, %rbx
	movq	$3, 8(%rbx)
	movq	40(%rbx), %r10
	movq	8(%r10), %r12
	movq	%r12, 40(%rbx)
	movq	(%r10), %r13
	movq	(%r13), %r14
	movq	$1, %r15
	movq	%r15, %rax
	movq	%r13, %rdi
	jmp	*%r14
	.text
slowClone_1.35A:
	movq	%rax, %r14
	movq	%rdi, %r15
	jmp	gcTest35C
	/* live= GP={%r14 %r15} spilled=  */
retGC35B:
	movq	8(%rdi), %r14
	movq	(%rdi), %r15
gcTest35C:
	movq	%r11, %rbx
	subq	%rsi, %rbx
	jle	doGC35D
check.358:
	/* Liveout:  GP={%r15 %r14 %r13 %r12 %r10 %r9 %r8 %rdi}  */
	/* block check<10C46> (ep<F3BB>,_unit<F3B8>) */
	movq	$-1048576, %rbx
	andq	%rsi, %rbx
	movq	%rsi, 120(%rbx)
	movq	$1, (%rbx)
	movq	%rax, -56(%rbp)
	movq	%rcx, -96(%rbp)
	movq	%rdx, %r12
	movq	%rdi, %r13
	movq	%r8, -64(%rbp)
	movq	%r9, -88(%rbp)
	movq	%r10, -80(%rbp)
	movq	%r11, -72(%rbp)
	movq	$-1048576, %r10
	andq	%rsi, %r10
	movq	%r10, %rdi
	movq	56(%r15), %rcx
	movq	%rcx, %rsi
	call	PromoteObj
	movq	%rax, %r14
	movq	-56(%rbp), %rax
	movq	-96(%rbp), %rcx
	movq	%r12, %rdx
	movq	%r13, %rdi
	movq	-64(%rbp), %r8
	movq	-88(%rbp), %r9
	movq	-80(%rbp), %r10
	movq	-72(%rbp), %r11
	movq	120(%rbx), %rsi
	movq	$3, (%rbx)
	movq	$1801, -8(%rsi)
	movabsq	$letJoinK.353, %rcx
	movq	%rcx, (%rsi)
	movq	64(%r15), %rdx
	movq	%rdx, 8(%rsi)
	movq	72(%r15), %rbx
	movq	%rbx, 16(%rsi)
	movq	%r14, 24(%rsi)
	movq	%rsi, %r14
	addq	$40, %rsi
	movq	8(%r15), %r10
	movq	(%r10), %rdi
	movq	48(%r15), %r8
	movl	40(%r15), %r9d
	movq	24(%r15), %r10
	movq	24(%r15), %r12
	movl	(%r12), %r12d
	movq	32(%r15), %r13
	movq	16(%r15), %r15
	jmp	tabFromToP.359
doGC35D:
	movq	$20, -8(%rsi)
	movq	%r15, (%rsi)
	movq	%r14, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	%r13, %rdi
	movabsq	$retGC35B, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.35F:
	movq	%r8, %r10
	movq	%rdi, %rcx
	jmp	gcTest361
	/* live= GP={%r10 %rcx} spilled=  */
retGC360:
	movq	8(%rdi), %r10
	movq	(%rdi), %rcx
gcTest361:
	movq	%r11, %r12
	subq	%rsi, %r12
	jle	doGC362
check.35E:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block check<10C49> (ep<F47B>,v_1<F478>) */
	movq	$20, -8(%rsi)
	movq	16(%rcx), %rbx
	movq	%rbx, (%rsi)
	movq	%r10, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	movq	8(%rcx), %rdi
	movq	%rdx, %r8
	movq	16(%rcx), %r9
	jmp	letJoinK.344
doGC362:
	movq	$20, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%r10, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	%r10, %rdi
	movabsq	$retGC360, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.36B:
	movq	%rax, %rbx
	movq	%rdi, %r15
gcTest36D:
	movq	%r11, %r12
	subq	%rsi, %r12
	jle	doGC36E
check.363:
	/* block check<10C4C> (ep<F457>,notStolen_1<F44D>) */
	cmpq	$1, %rbx
	je	S_case364
	cmpq	$3, %rbx
	je	S_case366
S_case364:
	movq	%r15, -64(%rbp)
case.365:
	/* flushLoads */
	/* block case<F45B> (ep<F45A>) */
	movq	$-1048576, %r14
	andq	%rsi, %r14
	movq	%rsi, 120(%r14)
	movq	$1, (%r14)
	movq	%rax, -80(%rbp)
	movq	%rcx, %r15
	movq	%rdx, %rbx
	movq	%rdi, %r12
	movq	%r8, -72(%rbp)
	movq	%r9, -88(%rbp)
	movq	%r10, -96(%rbp)
	movq	%r11, %r13
	movq	$-1048576, %rcx
	andq	%rsi, %rcx
	movq	%rcx, %rdi
	movq	-64(%rbp), %rcx
	movq	56(%rcx), %rcx
	movq	%rcx, %rsi
	call	PromoteObj
	movq	%rax, -56(%rbp)
	movq	-80(%rbp), %rax
	movq	%r15, %rcx
	movq	%rbx, %rdx
	movq	%r12, %rdi
	movq	-72(%rbp), %r8
	movq	-88(%rbp), %r9
	movq	-96(%rbp), %r10
	movq	%r13, %r11
	movq	120(%r14), %rsi
	movq	$3, (%r14)
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
	movq	-64(%rbp), %rdx
	movq	80(%rdx), %rdx
	movq	%rdx, %rsi
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
	movq	-64(%rbp), %r12
	movq	-56(%rbp), %r13
	movq	%r15, (%r13)
	movq	$-1048576, %r15
	andq	%rsi, %r15
	movq	%rsi, 120(%r15)
	movq	$1, (%r15)
	movq	%rax, -104(%rbp)
	movq	%rcx, %rbx
	movq	%rdx, %r12
	movq	%rdi, %r13
	movq	%r8, -88(%rbp)
	movq	%r9, -80(%rbp)
	movq	%r10, -72(%rbp)
	movq	%r11, -96(%rbp)
	movq	$-1048576, %rcx
	andq	%rsi, %rcx
	movq	%rcx, %rdi
	movq	-64(%rbp), %r14
	movq	64(%r14), %r14
	movq	%r14, %rsi
	call	PromoteObj
	movq	%rax, %r14
	movq	-104(%rbp), %rax
	movq	%rbx, %rcx
	movq	%r12, %rdx
	movq	%r13, %rdi
	movq	-88(%rbp), %r8
	movq	-80(%rbp), %r9
	movq	-72(%rbp), %r10
	movq	-96(%rbp), %r11
	movq	120(%r15), %rsi
	movq	$3, (%r15)
	movq	-64(%rbp), %r15
	movl	$1, %r15d
	lock
	xaddl	%r15d, (%r14)
	cmpl	$2, %r15d
	jne	L36F
L_true368:
	movq	-56(%rbp), %r15
	movq	-64(%rbp), %r14
then.36A:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block then<F468> (ep<F466>,r<F467>) */
	movq	72(%r14), %rdi
	movq	%r15, %r8
	movq	(%r15), %r9
	movq	8(%r15), %r10
	jmp	letJoinK.344
doGC36E:
	movq	$20, -8(%rsi)
	movq	%r15, (%rsi)
	movq	%rbx, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	%r15, %rdi
	movabsq	$retGC36C, %r8
	jmp	ASM_InvokeGC
	/* live= GP={%rbx %r15} spilled=  */
retGC36C:
	movq	8(%rdi), %rbx
	movq	(%rdi), %r15
	jmp	gcTest36D
S_case366:
case.367:
	/* Liveout:  GP={%r15 %r14 %r13 %r12 %r10 %r9 %r8 %rdi}  */
	/* block case<F477> (ep<F476>) */
	movq	$775, -8(%rsi)
	movabsq	$letJoinK.35F, %rcx
	movq	%rcx, (%rsi)
	movq	72(%r15), %rdx
	movq	%rdx, 8(%rsi)
	movq	80(%r15), %rbx
	movq	%rbx, 16(%rsi)
	movq	%rsi, %r14
	addq	$32, %rsi
	movq	8(%r15), %r10
	movq	(%r10), %rdi
	movq	48(%r15), %r8
	movl	40(%r15), %r9d
	movq	24(%r15), %r10
	movq	24(%r15), %r12
	movl	(%r12), %r12d
	movq	32(%r15), %r13
	movq	16(%r15), %r15
	jmp	tabFromToP.359
L36F:
else.369:
	/* Liveout:  GP={%rax %rdi}  */
	/* block else<F46E> () */
	movq	$-1048576, %rdx
	andq	%rsi, %rdx
	movq	$3, 8(%rdx)
	movq	40(%rdx), %rbx
	movq	8(%rbx), %r10
	movq	%r10, 40(%rdx)
	movq	(%rbx), %r12
	movq	(%r12), %r13
	movq	$1, %r14
	movq	%r14, %rax
	movq	%r12, %rdi
	jmp	*%r13
	.text
letJoinK.378:
	movq	%r8, %rcx
	movq	%rdi, %rdx
gcTest37A:
	movq	%r11, %r14
	subq	%rsi, %r14
	jle	doGC37B
check.370:
	/* block check<10C4F> (ep<F437>,v_0<F42C>) */
	movq	$-1048576, %r10
	andq	%rsi, %r10
	movq	32(%r10), %r12
	movq	8(%r12), %rbx
	cmpq	$1, %rbx
	jne	L_true371
else.372:
	/* Liveout:  GP={%rax %rdi}  */
	/* block else<F49B> (ep<F49A>) */
	movq	$133, -8(%rsi)
	movabsq	$str87, %r14
	movq	%r14, (%rsi)
	movl	$51, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movabsq	$tag74, %r15
	movq	%r15, (%rsi)
	movq	%r13, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	16(%rdx), %r14
	movq	(%r14), %rcx
	movq	%r15, %rax
	movq	%r14, %rdi
	jmp	*%rcx
doGC37B:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	%r13, %rdi
	movabsq	$retGC379, %r8
	jmp	ASM_InvokeGC
	/* live= GP={%rcx %rdx} spilled=  */
retGC379:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
	jmp	gcTest37A
L_true371:
then.373:
	/* block then<F441> (ep<F43E>,v_0<F440>,_t<F43F>) */
	movq	(%rbx), %rbx
	movq	(%rbx), %r13
	cmpq	$1, %r13
	je	L37C
L_true374:
then.376:
	/* Liveout:  GP={%r9 %r8 %rax %rdi}  */
	/* block then<F449> (ep<F446>,v_0<F448>,stk<F447>) */
	movq	(%r13), %r15
	movq	16(%r15), %r13
	movq	$257815, -8(%rsi)
	movabsq	$letJoinK.36B, %rbx
	movq	%rbx, (%rsi)
	movq	8(%rdx), %r10
	movq	%r10, 8(%rsi)
	movq	16(%rdx), %r12
	movq	%r12, 16(%rsi)
	movq	24(%rdx), %r14
	movq	%r14, 24(%rsi)
	movq	32(%rdx), %r15
	movq	%r15, 32(%rsi)
	movl	40(%rdx), %ebx
	movl	%ebx, 40(%rsi)
	movq	48(%rdx), %r10
	movq	%r10, 48(%rsi)
	movq	56(%rdx), %r12
	movq	%r12, 56(%rsi)
	movq	64(%rdx), %r14
	movq	%r14, 64(%rsi)
	movq	72(%rdx), %r15
	movq	%r15, 72(%rsi)
	movq	%rcx, 80(%rsi)
	movq	%rsi, %rbx
	addq	$96, %rsi
	movq	8(%r13), %rcx
	movq	16(%rdx), %r12
	movq	80(%rdx), %r10
	movq	(%r13), %rdx
	movq	%r12, %r9
	movq	%rbx, %r8
	movq	%r10, %rax
	movq	%rdx, %rdi
	jmp	*%rcx
L37C:
else.375:
	/* Liveout:  GP={%rax %rdi}  */
	/* block else<F492> (ep<F491>) */
	movq	$133, -8(%rsi)
	movabsq	$str377, %r12
	movq	%r12, (%rsi)
	movl	$44, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movabsq	$tag74, %r13
	movq	%r13, (%rsi)
	movq	%r10, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	16(%rdx), %r10
	movq	(%r10), %r14
	movq	%r12, %rax
	movq	%r10, %rdi
	jmp	*%r14
	.text
letJoinK.37E:
	movq	%rax, %rcx
	movq	%rdi, %rdx
	jmp	gcTest380
	/* live= GP={%rcx %rdx} spilled=  */
retGC37F:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest380:
	movq	%r11, %rbx
	subq	%rsi, %rbx
	jle	doGC381
check.37D:
	/* Liveout:  GP={%r15 %r14 %r13 %r12 %r10 %r9 %r8 %rdi}  */
	/* block check<10C52> (ep<F429>,_wild<F41E>) */
	movq	$257815, -8(%rsi)
	movabsq	$letJoinK.378, %rbx
	movq	%rbx, (%rsi)
	movq	8(%rdx), %r10
	movq	%r10, 8(%rsi)
	movq	16(%rdx), %r12
	movq	%r12, 16(%rsi)
	movq	32(%rdx), %r13
	movq	%r13, 24(%rsi)
	movq	40(%rdx), %r14
	movq	%r14, 32(%rsi)
	movl	48(%rdx), %r15d
	movl	%r15d, 40(%rsi)
	movq	56(%rdx), %rcx
	movq	%rcx, 48(%rsi)
	movq	64(%rdx), %rbx
	movq	%rbx, 56(%rsi)
	movq	72(%rdx), %r10
	movq	%r10, 64(%rsi)
	movq	80(%rdx), %r12
	movq	%r12, 72(%rsi)
	movq	88(%rdx), %r13
	movq	%r13, 80(%rsi)
	movq	%rsi, %r14
	addq	$96, %rsi
	movq	8(%rdx), %r15
	movq	(%r15), %rdi
	movq	24(%rdx), %r8
	movq	24(%rdx), %rcx
	movl	(%rcx), %r9d
	movq	56(%rdx), %r10
	movl	48(%rdx), %r12d
	movq	40(%rdx), %r13
	movq	16(%rdx), %r15
	jmp	tabFromToP.359
doGC381:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	movq	%rdx, %rdi
	movabsq	$retGC37F, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.389:
	movq	%r8, %rcx
	movq	%rdi, %rdx
gcTest38B:
	movq	%r11, %rbx
	subq	%rsi, %rbx
	jle	doGC38C
check.382:
	/* block check<10C55> (ep<F406>,k'<F403>) */
	movq	$20, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	88(%rdx), %r13
	movq	%r13, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	$-1048576, %r12
	andq	%rsi, %r12
	movq	32(%r12), %r14
	movq	8(%r14), %r10
	cmpq	$1, %r10
	jne	L_true383
else.384:
	/* Liveout:  GP={%rax %rdi}  */
	/* block else<F4BA> (ep<F4B9>) */
	movq	$133, -8(%rsi)
	movabsq	$str87, %rcx
	movq	%rcx, (%rsi)
	movl	$51, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movabsq	$tag74, %rbx
	movq	%rbx, (%rsi)
	movq	%r15, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	16(%rdx), %r15
	movq	(%r15), %r10
	movq	%rcx, %rax
	movq	%r15, %rdi
	jmp	*%r10
doGC38C:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	movq	%rdx, %rdi
	movabsq	$retGC38A, %r8
	jmp	ASM_InvokeGC
	/* live= GP={%rcx %rdx} spilled=  */
retGC38A:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
	jmp	gcTest38B
L_true383:
then.385:
	/* block then<F412> (ep<F40F>,thd<F411>,_t<F410>) */
	movq	(%r10), %r12
	movq	(%r12), %r14
	cmpq	$1, %r14
	je	L38D
L_true386:
then.388:
	/* Liveout:  GP={%r9 %r8 %rax %rdi}  */
	/* block then<F41A> (ep<F417>,thd<F419>,stk<F418>) */
	movq	(%r14), %r10
	movq	8(%r10), %r14
	movq	$515865, -8(%rsi)
	movabsq	$letJoinK.37E, %r12
	movq	%r12, (%rsi)
	movq	8(%rdx), %r13
	movq	%r13, 8(%rsi)
	movq	16(%rdx), %r15
	movq	%r15, 16(%rsi)
	movq	24(%rdx), %rcx
	movq	%rcx, 24(%rsi)
	movq	32(%rdx), %r10
	movq	%r10, 32(%rsi)
	movq	40(%rdx), %r12
	movq	%r12, 40(%rsi)
	movl	48(%rdx), %r13d
	movl	%r13d, 48(%rsi)
	movq	56(%rdx), %r15
	movq	%r15, 56(%rsi)
	movq	64(%rdx), %rcx
	movq	%rcx, 64(%rsi)
	movq	72(%rdx), %r10
	movq	%r10, 72(%rsi)
	movq	80(%rdx), %r12
	movq	%r12, 80(%rsi)
	movq	%rbx, 88(%rsi)
	movq	%rsi, %r10
	addq	$104, %rsi
	movq	8(%r14), %r15
	movq	16(%rdx), %rcx
	movq	(%r14), %r13
	movq	%rcx, %r9
	movq	%r10, %r8
	movq	%rbx, %rax
	movq	%r13, %rdi
	jmp	*%r15
L38D:
else.387:
	/* Liveout:  GP={%rax %rdi}  */
	/* block else<F4B1> (ep<F4B0>) */
	movq	$133, -8(%rsi)
	movabsq	$str377, %r14
	movq	%r14, (%rsi)
	movl	$44, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movabsq	$tag74, %r15
	movq	%r15, (%rsi)
	movq	%r13, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	16(%rdx), %r12
	movq	(%r12), %rcx
	movq	%r13, %rax
	movq	%r12, %rdi
	jmp	*%rcx
	.text
letJoinK.38F:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest391
	/* live= GP={%rcx %rdx} spilled=  */
retGC390:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest391:
	movq	%r11, %r10
	subq	%rsi, %r10
	jle	doGC392
check.38E:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<10C58> (ep<F4CB>,k<F4CA>) */
	movq	8(%rdx), %rdi
	movq	%rcx, %r8
	jmp	letJoinK.389
doGC392:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	%rbx, %rdi
	movabsq	$retGC390, %r8
	jmp	ASM_InvokeGC
	.text
gen.261:
	movq	%r9, %rdx
	movq	%r8, %rcx
	movq	%rdi, %rbx
	jmp	gcTest395
	/* live= GP={%rdx %rcx %rbx} spilled=  */
retGC394:
	movq	16(%rdi), %rdx
	movl	8(%rdi), %ecx
	movq	(%rdi), %rbx
gcTest395:
	movq	%r11, %r10
	subq	%rsi, %r10
	jle	doGC396
check.393:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<10C65> (ep<F523>,_t<F524>,retK<F525>) */
	movq	$10, -8(%rsi)
	imull	(%rbx), %ecx
	movl	8(%rbx), %r13d
	leal	(%rcx,%r13,1), %r12d
	movl	%r12d, (%rsi)
	movq	%rsi, %r10
	addq	$16, %rsi
	movq	(%rdx), %r14
	movq	%rdx, %rdi
	movq	%r10, %r8
	jmp	*%r14
doGC396:
	movq	$647, -8(%rsi)
	movq	%rbx, (%rsi)
	movl	%ecx, 8(%rsi)
	movq	%rdx, 16(%rsi)
	movq	%rsi, %r15
	addq	$32, %rsi
	movq	%r15, %rdi
	movabsq	$retGC394, %r8
	jmp	ASM_InvokeGC
	.text
rangePNoStep.3C5:
	movq	%r14, -64(%rbp)
	movq	%r13, -128(%rbp)
	movq	%r12, %rbx
	movq	%r9, %r14
	movq	%r8, -56(%rbp)
	movq	%rdi, -72(%rbp)
	movq	%r10, %r12
	movq	-72(%rbp), %r15
gcTest3C7:
	movq	%r11, %r13
	subq	%rsi, %r13
	jle	doGC3C8
	movq	%r12, %r13
	movq	-64(%rbp), %r12
check.397:
	/* block check<10C6D> (ep<F4E3>,param<F4E4>,from<F4E5>,a0<F4E6>,b0<F4E7>,retK<F4E8>,_exh<F4E9>) */
	cmpl	%ebx, %r13d
	je	L_true398
	movq	%rbx, %rcx
	movq	%r13, %r10
	movq	%r12, %rbx
	movq	-128(%rbp), %r13
	movq	%r15, -64(%rbp)
	movl	$1, %edx
else.399:
	/* block else<F518> (ep<F511>,param<F517>,retK<F516>,_exh<F515>,_lit<F514>,a0<F513>,b0<F512>) */
	cmpl	%ecx, %r10d
	je	L_true39B
else.39C:
	/* block else<F550> (ep<F549>,param<F54F>,retK<F54E>,_exh<F54D>,_lit<F54C>,a0<F54B>,b0<F54A>) */
	cmpl	$0, %edx
	jg	L_true39E
	movq	%r13, %r15
else.39F:
	/* block else<F60C> (ep<F606>,param<F60B>,retK<F60A>,_exh<F609>,_lit<F608>,a0<F607>) */
	movq	%r10, %rcx
	movq	%rdx, %r10
	movq	%rbx, %r12
	movq	%r15, %rdx
	movq	-56(%rbp), %r13
	movq	-64(%rbp), %rbx
	movq	$1, %r14
	jmp	letJoinK.3A1
doGC3C8:
	movq	$13199, -8(%rsi)
	movq	%r15, (%rsi)
	movq	-56(%rbp), %rdx
	movq	%rdx, 8(%rsi)
	movq	%r14, 16(%rsi)
	movl	%r12d, 24(%rsi)
	movl	%ebx, 32(%rsi)
	movq	-128(%rbp), %rbx
	movq	%rbx, 40(%rsi)
	movq	-64(%rbp), %r10
	movq	%r10, 48(%rsi)
	movq	%rsi, %r10
	addq	$64, %rsi
	movq	%r10, %rdi
	movabsq	$retGC3C6, %r8
	jmp	ASM_InvokeGC
	/* live= GP={%r12 %rbx %r13 %r14 %r15} spilled= GP={%r~1 %r~1}  */
retGC3C6:
	movq	48(%rdi), %r12
	movq	%r12, -64(%rbp)
	movq	40(%rdi), %r13
	movq	%r13, -128(%rbp)
	movl	32(%rdi), %ebx
	movl	24(%rdi), %r12d
	movq	16(%rdi), %r14
	movq	8(%rdi), %r15
	movq	%r15, -56(%rbp)
	movq	(%rdi), %r15
	jmp	gcTest3C7
L_true39B:
	movq	%r13, %r14
then.39D:
	/* block then<F546> (ep<F541>,retK<F545>,_exh<F544>,_lit<F543>,a0<F542>) */
	movl	$1, %r12d
	movq	$10, -8(%rsi)
	movl	%r12d, (%rsi)
	movq	%rsi, %r15
	addq	$16, %rsi
	movq	%r12, -120(%rbp)
	movq	%r10, %rcx
	movq	%rdx, %r13
	movq	%rbx, %r10
	movq	-64(%rbp), %r12
	jmp	letJoinK.3A9
L_true3B3:
	movq	%r14, %r12
	movq	%r15, %r14
	movq	%rdx, %rbx
then.3B4:
	/* block then<F5C9> (ep<F5C3>,param<F5C8>,retK<F5C7>,_exh<F5C6>,_lit<F5C5>,a0<F5C4>) */
	movq	(%r13), %r15
	movq	8(%r13), %rdx
	movl	(%r15), %r15d
	cmpl	(%rdx), %r15d
	jl	L_true3B5
	movq	%r14, %r15
else.3B6:
	/* block else<F5DC> (ep<F5D6>,param<F5DB>,retK<F5DA>,_exh<F5D9>,_lit<F5D8>,a0<F5D7>) */
	movq	%rcx, -136(%rbp)
	movq	%rbx, %rdx
	movq	$1, %rcx
letJoinK.3AE:
	/* block letJoinK<F569> (ep<F562>,param<F568>,retK<F567>,_exh<F566>,_lit<F565>,a0<F564>,_t<F563>) */
	cmpq	$1, %rcx
	je	S_case3AF
	cmpq	$3, %rcx
	jne	S_case3AF
S_case3B1:
	movq	-136(%rbp), %rcx
	movq	%r12, %r13
	movq	%r15, %r14
	movq	%rdx, %r12
case.3AD:
	/* block case<F5C0> (ep<F5BB>,retK<F5BF>,_exh<F5BE>,_lit<F5BD>,a0<F5BC>) */
	xorl	%ebx, %ebx
	movq	$10, -8(%rsi)
	movl	%ebx, (%rsi)
	movq	%rsi, %r15
	addq	$16, %rsi
	movq	%rbx, -120(%rbp)
letJoinK.3A9:
	/* block letJoinK<F520> (ep<F519>,retK<F51F>,_exh<F51E>,_lit<F51D>,a0<F51C>,sz<F51A>,_t<F51B>) */
	movq	$18, -8(%rsi)
	movl	%r13d, (%rsi)
	movl	%ecx, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	$133, -8(%rsi)
	movq	%rcx, (%rsi)
	movabsq	$gen.261, %rdx
	movq	%rdx, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	cmpl	$0, -120(%rbp)
	jle	L_true3AA
	movq	%r10, %rcx
	movq	%r14, %rdx
	movq	%r12, %r10
	movq	%rbx, %r13
else.3AB:
	/* Liveout:  GP={%r15 %r14 %r13 %r12 %r10 %r9 %r8 %rdi}  */
	/* block else<F53A> (ep<F535>,retK<F539>,_exh<F538>,sz<F537>,gen<F536>) */
	xorl	%r14d, %r14d
	movq	$10, -8(%rsi)
	movl	%r14d, (%rsi)
	movq	%rsi, %rbx
	addq	$16, %rsi
	movq	8(%r10), %r10
	movq	(%r10), %rdi
	movq	%rbx, %r8
	movq	%r14, %r9
	movq	%r15, %r10
	movl	(%r15), %r12d
	movq	%rdx, %r14
	movq	%rcx, %r15
	jmp	tabFromToP.359
L_true3B5:
	movq	%r14, %r15
	movq	%rbx, %rdx
then.3B7:
	/* block then<F5D4> (ep<F5CE>,param<F5D3>,retK<F5D2>,_exh<F5D1>,_lit<F5D0>,a0<F5CF>) */
	movq	%rcx, -136(%rbp)
	movq	$3, %rcx
	jmp	letJoinK.3AE
S_case3AF:
	movq	-136(%rbp), %rbx
	movq	%r12, %rcx
	movq	%r15, -56(%rbp)
	movq	%rdx, -144(%rbp)
case.3B0:
	/* block case<F570> (ep<F56A>,param<F56F>,retK<F56E>,_exh<F56D>,_lit<F56C>,a0<F56B>) */
	movq	(%r13), %rdx
	movq	8(%r13), %r12
	movl	(%rdx), %r15d
	subl	(%r12), %r15d
	movq	$10, -8(%rsi)
	movl	%r15d, (%rsi)
	movq	%rsi, %r13
	addq	$16, %rsi
	cmpl	$0, %r15d
	jl	L_true3B8
	movq	-56(%rbp), %r12
	jmp	letJoinK.3B9
L_true3B8:
	movq	-56(%rbp), %r12
then.3BA:
	/* block then<F5B1> (ep<F5AB>,retK<F5B0>,_exh<F5AF>,_lit<F5AE>,a0<F5AD>,_t<F5AC>) */
	movq	$10, -8(%rsi)
	negl	%r15d
	movl	%r15d, (%rsi)
	movq	%rsi, %r13
	addq	$16, %rsi
letJoinK.3B9:
	/* block letJoinK<F57E> (ep<F578>,retK<F57D>,_exh<F57C>,_lit<F57B>,a0<F57A>,_t<F579>) */
	cmpl	$0, %ecx
	jge	L3C9
L_true3BB:
	movq	%rbx, -136(%rbp)
then.3BD:
	/* block then<F5A2> (ep<F59C>,retK<F5A1>,_exh<F5A0>,_lit<F59F>,a0<F59E>,_t<F59D>) */
	movq	%rcx, %r14
	negl	%ecx
letJoinK.3BC:
	/* block letJoinK<F587> (ep<F580>,retK<F586>,_exh<F585>,_lit<F584>,a0<F583>,_t<F582>,b<F581>) */
	cmpl	$0, %ecx
	je	L_true3BE
	movq	%r14, -112(%rbp)
	movq	%r10, %r14
else.3BF:
	/* block else<F596> (ep<F58F>,retK<F595>,_exh<F594>,_lit<F593>,a0<F592>,_t<F591>,b<F590>) */
	movl	(%r13), %eax
	cdq
	idivl	%ecx
	movq	%rax, %r13
	incl	%r13d
	movq	$10, -8(%rsi)
	movl	%r13d, (%rsi)
	movq	%rsi, %r15
	addq	$16, %rsi
	movq	%r13, -120(%rbp)
	movq	-136(%rbp), %rcx
	movq	-112(%rbp), %r13
	movq	%r14, %r10
	movq	%r12, %r14
	movq	-144(%rbp), %r12
	jmp	letJoinK.3A9
L3C9:
	movq	%rbx, -136(%rbp)
	movq	%rcx, %r14
	jmp	letJoinK.3BC
L_true3BE:
then.3C0:
	/* Liveout:  GP={%rax %rdi}  */
	/* block then<F58B> (_exh<F58A>) */
	movq	$12, -8(%rsi)
	movabsq	$tag3C1, %r15
	movq	%r15, (%rsi)
	movq	%rsi, %r14
	addq	$16, %rsi
	movq	(%r10), %rcx
	movq	%r14, %rax
	movq	%r10, %rdi
	jmp	*%rcx
L_true3AA:
	movq	%r14, %r13
then.3AC:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block then<F532> (ep<F530>,retK<F531>) */
	movq	(%r13), %r14
	movq	%r13, %rdi
	movq	(%r12), %r8
	jmp	*%r14
L_true39E:
	movq	%rdx, %r12
	movq	-56(%rbp), %r14
	movq	-64(%rbp), %r15
then.3A0:
	/* block then<F5F5> (ep<F5EE>,param<F5F4>,retK<F5F3>,_exh<F5F2>,_lit<F5F1>,a0<F5F0>,b0<F5EF>) */
	cmpl	%ecx, %r10d
	jg	L_true3A7
	movq	%r10, %rdx
	movq	%r12, %r10
	movq	%rbx, %r12
	movq	%r15, %rbx
else.3A6:
	/* block else<F604> (ep<F5FE>,param<F603>,retK<F602>,_exh<F601>,_lit<F600>,a0<F5FF>) */
	movq	%rdx, %rcx
	movq	%r13, %rdx
	movq	%r14, %r13
	movq	$1, %r14
	jmp	letJoinK.3A1
L_true3A7:
	movq	%r10, %rcx
	movq	%r12, %r10
	movq	%rbx, %r12
then.3A8:
	/* block then<F5FC> (ep<F5F6>,param<F5FB>,retK<F5FA>,_exh<F5F9>,_lit<F5F8>,a0<F5F7>) */
	movq	%r13, %rdx
	movq	%r14, %r13
	movq	%r15, %rbx
	movq	$3, %r14
letJoinK.3A1:
	/* block letJoinK<F559> (ep<F552>,param<F558>,retK<F557>,_exh<F556>,_lit<F555>,a0<F554>,_t<F553>) */
	cmpq	$1, %r14
	jne	L3CA
S_case3A2:
	movq	%r10, %r14
	movq	%r12, %r10
	movq	%rdx, %r15
	movq	%rbx, %rdx
case.3A3:
	/* block case<F560> (ep<F55A>,param<F55F>,retK<F55E>,_exh<F55D>,_lit<F55C>,a0<F55B>) */
	cmpl	$0, %r14d
	jl	L_true3B3
	movq	%r14, %r12
	movq	%rdx, %r14
else.3B2:
	/* block else<F5E4> (ep<F5DE>,param<F5E3>,retK<F5E2>,_exh<F5E1>,_lit<F5E0>,a0<F5DF>) */
	movq	%rcx, -136(%rbp)
	movq	%r14, %rdx
	movq	$1, %rcx
	jmp	letJoinK.3AE
L3CA:
	cmpq	$3, %r14
	jne	S_case3A2
S_case3A4:
	movq	%r10, %r13
	movq	%r12, %r10
	movq	%rdx, %r12
	movq	%rbx, %rdx
case.3A5:
	/* block case<F5EB> (ep<F5E6>,retK<F5EA>,_exh<F5E9>,_lit<F5E8>,a0<F5E7>) */
	xorl	%r14d, %r14d
	movq	$10, -8(%rsi)
	movl	%r14d, (%rsi)
	movq	%rsi, %rbx
	addq	$16, %rsi
	movq	%r14, -120(%rbp)
	movq	%r12, %r14
	movq	%rdx, %r12
	movq	%rbx, %r15
	jmp	letJoinK.3A9
L_true398:
	movq	%r14, -56(%rbp)
	movq	%r12, %r13
then.39A:
	/* block then<F4F2> (retK<F4F1>,_exh<F4F0>,from<F4EF>) */
	movl	$1, %r15d
	cmpl	$0, %r15d
	jge	L3CB
L_true3C2:
then.3C4:
	/* Liveout:  GP={%rax %rdi}  */
	/* block then<F4F8> (_exh<F4F7>) */
	movq	$133, -8(%rsi)
	movabsq	$str73, %r15
	movq	%r15, (%rsi)
	movl	$29, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movabsq	$tag74, %rdx
	movq	%rdx, (%rsi)
	movq	%r14, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	(%r13), %rbx
	movq	%rcx, %rax
	movq	%r13, %rdi
	jmp	*%rbx
L3CB:
	movq	%r15, -72(%rbp)
	movl	$1, -64(%rbp)
else.3C3:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block else<F504> (retK<F503>,from<F502>,_lit<F501>,_lit<F500>) */
	movq	$-1048576, %r15
	andq	%rsi, %r15
	movq	%rsi, 120(%r15)
	movq	$1, (%r15)
	movq	%rax, -80(%rbp)
	movq	%rcx, -88(%rbp)
	movq	%rdx, -96(%rbp)
	movq	%rdi, -104(%rbp)
	movq	%r8, %r14
	movq	%r9, %r13
	movq	%r10, %r12
	movq	%r11, %rbx
	movq	$-1048576, %rcx
	andq	%rsi, %rcx
	movq	%rcx, %rdi
	movq	-56(%rbp), %rsi
	call	PromoteObj
	movq	%rax, -56(%rbp)
	movq	-80(%rbp), %rax
	movq	-88(%rbp), %rcx
	movq	-96(%rbp), %rdx
	movq	-104(%rbp), %rdi
	movq	%r14, %r8
	movq	%r13, %r9
	movq	%r12, %r10
	movq	%rbx, %r11
	movq	120(%r15), %rsi
	movq	$3, (%r15)
	movq	$-1048576, %r14
	andq	%rsi, %r14
	movq	$1, (%r14)
	movq	%rax, -80(%rbp)
	movq	%rcx, -88(%rbp)
	movq	%rdx, -96(%rbp)
	movq	%rsi, -104(%rbp)
	movq	%rdi, -112(%rbp)
	movq	%r8, -120(%rbp)
	movq	%r9, %r13
	movq	%r10, %r12
	movq	%r11, %rbx
	movq	$-1048576, %rcx
	andq	%rsi, %rcx
	movq	%rcx, %rdi
	movslq	-72(%rbp), %rdx
	movq	%rdx, %rsi
	movq	-56(%rbp), %rdx
	call	M_NewArray
	movq	%rax, %r15
	movq	-80(%rbp), %rax
	movq	-88(%rbp), %rcx
	movq	-96(%rbp), %rdx
	movq	-104(%rbp), %rsi
	movq	-112(%rbp), %rdi
	movq	-120(%rbp), %r8
	movq	%r13, %r9
	movq	%r12, %r10
	movq	%rbx, %r11
	movq	$3, (%r14)
	movq	$133, -8(%rsi)
	movq	%r15, (%rsi)
	movq	-72(%rbp), %r14
	movl	%r14d, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	movq	$521, -8(%rsi)
	movq	$3, (%rsi)
	movq	-64(%rbp), %r15
	movl	%r15d, 8(%rsi)
	movq	(%rdx), %r10
	movq	%r10, 16(%rsi)
	movl	8(%rdx), %r12d
	movl	%r12d, 24(%rsi)
	movq	%rsi, %rbx
	addq	$40, %rsi
	movq	-128(%rbp), %rcx
	movq	(%rcx), %r13
	movq	-128(%rbp), %rdi
	movq	%rbx, %r8
	jmp	*%r13
	.text
letJoinK.3CD:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest3CF
	/* live= GP={%r12 %r10 %rcx %rdx %rbx} spilled=  */
retGC3CE:
	movq	32(%rdi), %r12
	movq	24(%rdi), %r10
	movl	16(%rdi), %ecx
	movl	8(%rdi), %edx
	movq	(%rdi), %rbx
gcTest3CF:
	movq	%r11, %r15
	subq	%rsi, %r15
	jle	doGC3D0
check.3CC:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<10C73> (ep<F646>,_t<F642>,_t<F643>,_t2<F644>,_t3<F645>) */
	movq	$3083, -8(%rsi)
	movq	$1, (%rsi)
	movl	%edx, 8(%rsi)
	movl	%ecx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%r12, 32(%rsi)
	movq	%rsi, %r10
	addq	$48, %rsi
	movq	8(%rbx), %r12
	movq	(%r12), %r13
	movq	%r12, %rdi
	movq	%r10, %r8
	jmp	*%r13
doGC3D0:
	movq	$3211, -8(%rsi)
	movq	%rbx, (%rsi)
	movl	%edx, 8(%rsi)
	movl	%ecx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%r12, 32(%rsi)
	movq	%rsi, %r14
	addq	$48, %rsi
	movq	%r14, %rdi
	movabsq	$retGC3CE, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.3D5:
	movq	%r8, %r15
	movq	%rdi, %rbx
	jmp	gcTest3D7
	/* live= GP={%r15} spilled= GP={%r~1}  */
retGC3D6:
	movq	8(%rdi), %r15
	movq	(%rdi), %rbx
gcTest3D7:
	movq	%r11, %r12
	subq	%rsi, %r12
	jle	doGC3D8
	movq	%rbx, -56(%rbp)
check.3D1:
	/* flushLoads */
	/* block check<10C76> (ep<F658>,v_3<F655>) */
	movq	$-1048576, %rbx
	andq	%rsi, %rbx
	movq	%rsi, 120(%rbx)
	movq	$1, (%rbx)
	movq	%rax, -80(%rbp)
	movq	%rcx, %r12
	movq	%rdx, %r13
	movq	%rdi, -64(%rbp)
	movq	%r8, -72(%rbp)
	movq	%r9, -88(%rbp)
	movq	%r10, -96(%rbp)
	movq	%r11, %r14
	movq	$-1048576, %rcx
	andq	%rsi, %rcx
	movq	%rcx, %rdi
	movq	%r15, %rsi
	call	PromoteObj
	movq	%rax, %r15
	movq	-80(%rbp), %rax
	movq	%r12, %rcx
	movq	%r13, %rdx
	movq	-64(%rbp), %rdi
	movq	-72(%rbp), %r8
	movq	-88(%rbp), %r9
	movq	-96(%rbp), %r10
	movq	%r14, %r11
	movq	120(%rbx), %rsi
	movq	$3, (%rbx)
	movq	-56(%rbp), %r13
	movq	24(%r13), %rbx
	movq	%r15, 24(%rbx)
	movq	$-1048576, %rbx
	andq	%rsi, %rbx
	movq	%rsi, 120(%rbx)
	movq	$1, (%rbx)
	movq	%rax, -72(%rbp)
	movq	%rcx, -80(%rbp)
	movq	%rdx, -88(%rbp)
	movq	%rdi, %r12
	movq	%r8, -64(%rbp)
	movq	%r9, -96(%rbp)
	movq	%r10, %r13
	movq	%r11, %r14
	movq	$-1048576, %r10
	andq	%rsi, %r10
	movq	%r10, %rdi
	movq	-56(%rbp), %r15
	movq	8(%r15), %r10
	movq	%r10, %rsi
	call	PromoteObj
	movq	%rax, %r15
	movq	-72(%rbp), %rax
	movq	-80(%rbp), %rcx
	movq	-88(%rbp), %rdx
	movq	%r12, %rdi
	movq	-64(%rbp), %r8
	movq	-96(%rbp), %r9
	movq	%r13, %r10
	movq	%r14, %r11
	movq	120(%rbx), %rsi
	movq	$3, (%rbx)
	movq	-56(%rbp), %rcx
	movl	$1, %ecx
	lock
	xaddl	%ecx, (%r15)
	cmpl	$4, %ecx
	jne	L3D9
L_true3D2:
	movq	-56(%rbp), %r15
then.3D4:
	/* Liveout:  GP={%r12 %r10 %r9 %r8 %rdi}  */
	/* block then<F664> (ep<F663>) */
	movq	16(%r15), %rdi
	movq	24(%r15), %rdx
	movq	(%rdx), %rcx
	movl	(%rcx), %r8d
	movq	24(%r15), %r10
	movq	8(%r10), %rbx
	movl	(%rbx), %r9d
	movq	24(%r15), %r12
	movq	16(%r12), %r10
	movq	24(%r15), %r13
	movq	24(%r13), %r12
	jmp	letJoinK.3CD
doGC3D8:
	movq	$20, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%r15, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	%r14, %rdi
	movabsq	$retGC3D6, %r8
	jmp	ASM_InvokeGC
L3D9:
else.3D3:
	/* Liveout:  GP={%rax %rdi}  */
	/* block else<F672> () */
	movq	$-1048576, %rbx
	andq	%rsi, %rbx
	movq	$3, 8(%rbx)
	movq	40(%rbx), %r10
	movq	8(%r10), %r12
	movq	%r12, 40(%rbx)
	movq	(%r10), %r13
	movq	(%r13), %r14
	movq	$1, %r15
	movq	%r15, %rax
	movq	%r13, %rdi
	jmp	*%r14
	.text
slowClone_3.3DC:
	movq	%rax, %r14
	movq	%rdi, %r15
	jmp	gcTest3DE
	/* live= GP={%r14 %r15} spilled=  */
retGC3DD:
	movq	8(%rdi), %r14
	movq	(%rdi), %r15
gcTest3DE:
	movq	%r11, %r13
	subq	%rsi, %r13
	jle	doGC3DF
check.3DA:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block check<10C79> (ep<F650>,_unit<F64D>) */
	movq	$-1048576, %rbx
	andq	%rsi, %rbx
	movq	%rsi, 120(%rbx)
	movq	$1, (%rbx)
	movq	%rax, -56(%rbp)
	movq	%rcx, -96(%rbp)
	movq	%rdx, %r12
	movq	%rdi, %r13
	movq	%r8, -64(%rbp)
	movq	%r9, -88(%rbp)
	movq	%r10, -80(%rbp)
	movq	%r11, -72(%rbp)
	movq	$-1048576, %r10
	andq	%rsi, %r10
	movq	%r10, %rdi
	movq	32(%r15), %rcx
	movq	%rcx, %rsi
	call	PromoteObj
	movq	%rax, %r14
	movq	-56(%rbp), %rax
	movq	-96(%rbp), %rcx
	movq	%r12, %rdx
	movq	%r13, %rdi
	movq	-64(%rbp), %r8
	movq	-88(%rbp), %r9
	movq	-80(%rbp), %r10
	movq	-72(%rbp), %r11
	movq	120(%rbx), %rsi
	movq	$3, (%rbx)
	movq	$1801, -8(%rsi)
	movabsq	$letJoinK.3D5, %rcx
	movq	%rcx, (%rsi)
	movq	40(%r15), %rdx
	movq	%rdx, 8(%rsi)
	movq	48(%r15), %rbx
	movq	%rbx, 16(%rsi)
	movq	%r14, 24(%rsi)
	movq	%rsi, %rdx
	addq	$40, %rsi
	movq	8(%r15), %r10
	movq	(%r10), %rdi
	movq	24(%r15), %r8
	movq	%rdx, %r9
	movq	16(%r15), %r10
	jmp	m.3DB
doGC3DF:
	movq	$20, -8(%rsi)
	movq	%r15, (%rsi)
	movq	%r14, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	%r12, %rdi
	movabsq	$retGC3DD, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.3E4:
	movq	%r8, %r15
	movq	%rdi, %rbx
	jmp	gcTest3E6
	/* live= GP={%r15} spilled= GP={%r~1}  */
retGC3E5:
	movq	8(%rdi), %r15
	movq	(%rdi), %rbx
gcTest3E6:
	movq	%r11, %r12
	subq	%rsi, %r12
	jle	doGC3E7
	movq	%rbx, -56(%rbp)
check.3E0:
	/* flushLoads */
	/* block check<10C7C> (ep<F6D4>,v_2<F6D1>) */
	movq	$-1048576, %rbx
	andq	%rsi, %rbx
	movq	%rsi, 120(%rbx)
	movq	$1, (%rbx)
	movq	%rax, -80(%rbp)
	movq	%rcx, %r12
	movq	%rdx, %r13
	movq	%rdi, -64(%rbp)
	movq	%r8, -72(%rbp)
	movq	%r9, -88(%rbp)
	movq	%r10, -96(%rbp)
	movq	%r11, %r14
	movq	$-1048576, %rcx
	andq	%rsi, %rcx
	movq	%rcx, %rdi
	movq	%r15, %rsi
	call	PromoteObj
	movq	%rax, %r15
	movq	-80(%rbp), %rax
	movq	%r12, %rcx
	movq	%r13, %rdx
	movq	-64(%rbp), %rdi
	movq	-72(%rbp), %r8
	movq	-88(%rbp), %r9
	movq	-96(%rbp), %r10
	movq	%r14, %r11
	movq	120(%rbx), %rsi
	movq	$3, (%rbx)
	movq	-56(%rbp), %r13
	movq	24(%r13), %rbx
	movq	%r15, 16(%rbx)
	movq	$-1048576, %rbx
	andq	%rsi, %rbx
	movq	%rsi, 120(%rbx)
	movq	$1, (%rbx)
	movq	%rax, -72(%rbp)
	movq	%rcx, -80(%rbp)
	movq	%rdx, -88(%rbp)
	movq	%rdi, %r12
	movq	%r8, -64(%rbp)
	movq	%r9, -96(%rbp)
	movq	%r10, %r13
	movq	%r11, %r14
	movq	$-1048576, %r10
	andq	%rsi, %r10
	movq	%r10, %rdi
	movq	-56(%rbp), %r15
	movq	8(%r15), %r10
	movq	%r10, %rsi
	call	PromoteObj
	movq	%rax, %r15
	movq	-72(%rbp), %rax
	movq	-80(%rbp), %rcx
	movq	-88(%rbp), %rdx
	movq	%r12, %rdi
	movq	-64(%rbp), %r8
	movq	-96(%rbp), %r9
	movq	%r13, %r10
	movq	%r14, %r11
	movq	120(%rbx), %rsi
	movq	$3, (%rbx)
	movq	-56(%rbp), %rcx
	movl	$1, %ecx
	lock
	xaddl	%ecx, (%r15)
	cmpl	$4, %ecx
	jne	L3E8
L_true3E1:
	movq	-56(%rbp), %r15
then.3E3:
	/* Liveout:  GP={%r12 %r10 %r9 %r8 %rdi}  */
	/* block then<F6E0> (ep<F6DF>) */
	movq	16(%r15), %rdi
	movq	24(%r15), %rdx
	movq	(%rdx), %rcx
	movl	(%rcx), %r8d
	movq	24(%r15), %r10
	movq	8(%r10), %rbx
	movl	(%rbx), %r9d
	movq	24(%r15), %r12
	movq	16(%r12), %r10
	movq	24(%r15), %r13
	movq	24(%r13), %r12
	jmp	letJoinK.3CD
doGC3E7:
	movq	$20, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%r15, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	%r14, %rdi
	movabsq	$retGC3E5, %r8
	jmp	ASM_InvokeGC
L3E8:
else.3E2:
	/* Liveout:  GP={%rax %rdi}  */
	/* block else<F6EE> () */
	movq	$-1048576, %rbx
	andq	%rsi, %rbx
	movq	$3, 8(%rbx)
	movq	40(%rbx), %r10
	movq	8(%r10), %r12
	movq	%r12, 40(%rbx)
	movq	(%r10), %r13
	movq	(%r13), %r14
	movq	$1, %r15
	movq	%r15, %rax
	movq	%r13, %rdi
	jmp	*%r14
	.text
slowClone_2.3EA:
	movq	%rax, %r14
	movq	%rdi, %r15
	jmp	gcTest3EC
	/* live= GP={%r14 %r15} spilled=  */
retGC3EB:
	movq	8(%rdi), %r14
	movq	(%rdi), %r15
gcTest3EC:
	movq	%r11, %r13
	subq	%rsi, %r13
	jle	doGC3ED
check.3E9:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block check<10C7F> (ep<F6CC>,_unit<F6C5>) */
	movq	$-1048576, %rbx
	andq	%rsi, %rbx
	movq	%rsi, 120(%rbx)
	movq	$1, (%rbx)
	movq	%rax, -56(%rbp)
	movq	%rcx, -96(%rbp)
	movq	%rdx, %r12
	movq	%rdi, %r13
	movq	%r8, -64(%rbp)
	movq	%r9, -88(%rbp)
	movq	%r10, -80(%rbp)
	movq	%r11, -72(%rbp)
	movq	$-1048576, %r10
	andq	%rsi, %r10
	movq	%r10, %rdi
	movq	32(%r15), %rcx
	movq	%rcx, %rsi
	call	PromoteObj
	movq	%rax, %r14
	movq	-56(%rbp), %rax
	movq	-96(%rbp), %rcx
	movq	%r12, %rdx
	movq	%r13, %rdi
	movq	-64(%rbp), %r8
	movq	-88(%rbp), %r9
	movq	-80(%rbp), %r10
	movq	-72(%rbp), %r11
	movq	120(%rbx), %rsi
	movq	$3, (%rbx)
	movq	$1801, -8(%rsi)
	movabsq	$letJoinK.3E4, %rcx
	movq	%rcx, (%rsi)
	movq	40(%r15), %rdx
	movq	%rdx, 8(%rsi)
	movq	48(%r15), %rbx
	movq	%rbx, 16(%rsi)
	movq	%r14, 24(%rsi)
	movq	%rsi, %rdx
	addq	$40, %rsi
	movq	8(%r15), %r10
	movq	(%r10), %rdi
	movq	24(%r15), %r8
	movq	%rdx, %r9
	movq	16(%r15), %r10
	jmp	m.3DB
doGC3ED:
	movq	$20, -8(%rsi)
	movq	%r15, (%rsi)
	movq	%r14, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	%r12, %rdi
	movabsq	$retGC3EB, %r8
	jmp	ASM_InvokeGC
	.text
slowClone_1.3F2:
	movq	%rax, %r14
	movq	%rdi, %r13
	jmp	gcTest3F4
	/* live= GP={%r14} spilled= GP={%r~1}  */
retGC3F3:
	movq	8(%rdi), %r14
	movq	(%rdi), %r13
gcTest3F4:
	movq	%r11, %rbx
	subq	%rsi, %rbx
	jle	doGC3F5
	movq	%r13, -64(%rbp)
check.3EE:
	/* flushLoads */
	/* block check<10C82> (ep<F747>,_unit<F742>) */
	movq	$-1048576, %r12
	andq	%rsi, %r12
	movq	%rsi, 120(%r12)
	movq	$1, (%r12)
	movq	%rax, -80(%rbp)
	movq	%rcx, %r13
	movq	%rdx, %r14
	movq	%rdi, %r15
	movq	%r8, -72(%rbp)
	movq	%r9, -88(%rbp)
	movq	%r10, -96(%rbp)
	movq	%r11, %rbx
	movq	$-1048576, %r10
	andq	%rsi, %r10
	movq	%r10, %rdi
	movq	-64(%rbp), %r10
	movq	16(%r10), %rcx
	movq	%rcx, %rsi
	call	PromoteObj
	movq	%rax, -56(%rbp)
	movq	-80(%rbp), %rax
	movq	%r13, %rcx
	movq	%r14, %rdx
	movq	%r15, %rdi
	movq	-72(%rbp), %r8
	movq	-88(%rbp), %r9
	movq	-96(%rbp), %r10
	movq	%rbx, %r11
	movq	120(%r12), %rsi
	movq	$3, (%r12)
	movq	$-1048576, %r12
	andq	%rsi, %r12
	movq	%rsi, 120(%r12)
	movq	$1, (%r12)
	movq	%rax, %r14
	movq	%rcx, -72(%rbp)
	movq	%rdx, -80(%rbp)
	movq	%rdi, -88(%rbp)
	movq	%r8, -96(%rbp)
	movq	%r9, -104(%rbp)
	movq	%r10, %r15
	movq	%r11, %rbx
	movq	$-1048576, %r13
	andq	%rsi, %r13
	movq	%r13, %rdi
	movq	-64(%rbp), %r13
	movq	8(%r13), %rdx
	movq	%rdx, %rsi
	call	PromoteObj
	movq	%rax, %r13
	movq	%r14, %rax
	movq	-72(%rbp), %rcx
	movq	-80(%rbp), %rdx
	movq	-88(%rbp), %rdi
	movq	-96(%rbp), %r8
	movq	-104(%rbp), %r9
	movq	%r15, %r10
	movq	%rbx, %r11
	movq	120(%r12), %rsi
	movq	$3, (%r12)
	movq	-64(%rbp), %r15
	movq	-56(%rbp), %rbx
	movq	%r13, 8(%rbx)
	movq	$-1048576, %r12
	andq	%rsi, %r12
	movq	%rsi, 120(%r12)
	movq	$1, (%r12)
	movq	%rax, -104(%rbp)
	movq	%rcx, %r14
	movq	%rdx, %r15
	movq	%rdi, %rbx
	movq	%r8, -88(%rbp)
	movq	%r9, -80(%rbp)
	movq	%r10, -72(%rbp)
	movq	%r11, -96(%rbp)
	movq	$-1048576, %r13
	andq	%rsi, %r13
	movq	%r13, %rdi
	movq	-64(%rbp), %r10
	movq	24(%r10), %rcx
	movq	%rcx, %rsi
	call	PromoteObj
	movq	%rax, %r13
	movq	-104(%rbp), %rax
	movq	%r14, %rcx
	movq	%r15, %rdx
	movq	%rbx, %rdi
	movq	-88(%rbp), %r8
	movq	-80(%rbp), %r9
	movq	-72(%rbp), %r10
	movq	-96(%rbp), %r11
	movq	120(%r12), %rsi
	movq	$3, (%r12)
	movq	-64(%rbp), %r12
	movl	$1, %r14d
	lock
	xaddl	%r14d, (%r13)
	cmpl	$4, %r14d
	jne	L3F6
L_true3EF:
	movq	-56(%rbp), %r12
	movq	-64(%rbp), %r10
then.3F1:
	/* Liveout:  GP={%r12 %r10 %r9 %r8 %rdi}  */
	/* block then<F756> (ep<F754>,r<F755>) */
	movq	32(%r10), %rdi
	movq	(%r12), %r14
	movl	(%r14), %r8d
	movq	8(%r12), %r15
	movl	(%r15), %r9d
	movq	16(%r12), %r10
	movq	24(%r12), %r12
	jmp	letJoinK.3CD
doGC3F5:
	movq	$20, -8(%rsi)
	movq	%r13, (%rsi)
	movq	%r14, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	%rcx, %rdi
	movabsq	$retGC3F3, %r8
	jmp	ASM_InvokeGC
L3F6:
else.3F0:
	/* Liveout:  GP={%rax %rdi}  */
	/* block else<F760> () */
	movq	$-1048576, %rcx
	andq	%rsi, %rcx
	movq	$3, 8(%rcx)
	movq	40(%rcx), %rdx
	movq	8(%rdx), %rbx
	movq	%rbx, 40(%rcx)
	movq	(%rdx), %r10
	movq	(%r10), %r12
	movq	$1, %r13
	movq	%r13, %rax
	movq	%r10, %rdi
	jmp	*%r12
	.text
letJoinK.3F8:
	movq	%r8, %r12
	movq	%rdi, %rcx
	jmp	gcTest3FA
	/* live= GP={%r12 %rcx} spilled=  */
retGC3F9:
	movq	8(%rdi), %r12
	movq	(%rdi), %rcx
gcTest3FA:
	movq	%r11, %rbx
	subq	%rsi, %rbx
	jle	doGC3FB
check.3F7:
	/* Liveout:  GP={%r12 %r10 %r9 %r8 %rdi}  */
	/* block check<10C85> (ep<F893>,v_3<F88E>) */
	movq	24(%rcx), %rdi
	movl	8(%rcx), %r8d
	movl	16(%rcx), %r9d
	movq	32(%rcx), %r10
	jmp	letJoinK.3CD
doGC3FB:
	movq	$20, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%r12, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	movq	%rdx, %rdi
	movabsq	$retGC3F9, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.404:
	movq	%rax, %r13
	movq	%rdi, %rbx
gcTest406:
	movq	%r11, %r12
	subq	%rsi, %r12
	jle	doGC407
check.3FC:
	/* block check<10C88> (ep<F865>,notStolen_3<F85A>) */
	cmpq	$1, %r13
	je	S_case3FD
	cmpq	$3, %r13
	je	S_case3FF
S_case3FD:
	movq	%rbx, -64(%rbp)
case.3FE:
	/* flushLoads */
	/* flushLoads */
	/* flushLoads */
	/* block case<F869> (ep<F868>) */
	movq	$-1048576, %r12
	andq	%rsi, %r12
	movq	%rsi, 120(%r12)
	movq	$1, (%r12)
	movq	%rax, -80(%rbp)
	movq	%rcx, -88(%rbp)
	movq	%rdx, %r13
	movq	%rdi, %r14
	movq	%r8, -72(%rbp)
	movq	%r9, -96(%rbp)
	movq	%r10, %r15
	movq	%r11, %rbx
	movq	$-1048576, %r10
	andq	%rsi, %r10
	movq	%r10, %rdi
	movq	-64(%rbp), %rcx
	movq	64(%rcx), %rcx
	movq	%rcx, %rsi
	call	PromoteObj
	movq	%rax, -56(%rbp)
	movq	-80(%rbp), %rax
	movq	-88(%rbp), %rcx
	movq	%r13, %rdx
	movq	%r14, %rdi
	movq	-72(%rbp), %r8
	movq	-96(%rbp), %r9
	movq	%r15, %r10
	movq	%rbx, %r11
	movq	120(%r12), %rsi
	movq	$3, (%r12)
	movq	$-1048576, %r12
	andq	%rsi, %r12
	movq	%rsi, 120(%r12)
	movq	$1, (%r12)
	movq	%rax, %r14
	movq	%rcx, -72(%rbp)
	movq	%rdx, -80(%rbp)
	movq	%rdi, -88(%rbp)
	movq	%r8, -96(%rbp)
	movq	%r9, -104(%rbp)
	movq	%r10, %r15
	movq	%r11, %rbx
	movq	$-1048576, %r13
	andq	%rsi, %r13
	movq	%r13, %rdi
	movq	-64(%rbp), %rdx
	movq	88(%rdx), %rdx
	movq	%rdx, %rsi
	call	PromoteObj
	movq	%rax, %r13
	movq	%r14, %rax
	movq	-72(%rbp), %rcx
	movq	-80(%rbp), %rdx
	movq	-88(%rbp), %rdi
	movq	-96(%rbp), %r8
	movq	-104(%rbp), %r9
	movq	%r15, %r10
	movq	%rbx, %r11
	movq	120(%r12), %rsi
	movq	$3, (%r12)
	movq	-64(%rbp), %rbx
	movq	-56(%rbp), %r12
	movq	%r13, 16(%r12)
	movq	$-1048576, %r12
	andq	%rsi, %r12
	movq	%rsi, 120(%r12)
	movq	$1, (%r12)
	movq	%rax, -104(%rbp)
	movq	%rcx, %r14
	movq	%rdx, %r15
	movq	%rdi, %rbx
	movq	%r8, -96(%rbp)
	movq	%r9, -72(%rbp)
	movq	%r10, -80(%rbp)
	movq	%r11, -88(%rbp)
	movq	$-1048576, %r13
	andq	%rsi, %r13
	movq	%r13, %rdi
	movq	-64(%rbp), %r13
	movq	56(%r13), %rcx
	movq	%rcx, %rsi
	call	PromoteObj
	movq	%rax, %r13
	movq	-104(%rbp), %rax
	movq	%r14, %rcx
	movq	%r15, %rdx
	movq	%rbx, %rdi
	movq	-96(%rbp), %r8
	movq	-72(%rbp), %r9
	movq	-80(%rbp), %r10
	movq	-88(%rbp), %r11
	movq	120(%r12), %rsi
	movq	$3, (%r12)
	movq	-64(%rbp), %r14
	movq	-56(%rbp), %r15
	movq	%r13, 8(%r15)
	movq	$-1048576, %r12
	andq	%rsi, %r12
	movq	%rsi, 120(%r12)
	movq	$1, (%r12)
	movq	%rax, -72(%rbp)
	movq	%rcx, -80(%rbp)
	movq	%rdx, -88(%rbp)
	movq	%rdi, %r14
	movq	%r8, %r15
	movq	%r9, -104(%rbp)
	movq	%r10, -96(%rbp)
	movq	%r11, %rbx
	movq	$-1048576, %r13
	andq	%rsi, %r13
	movq	%r13, %rdi
	movq	-64(%rbp), %rcx
	movq	48(%rcx), %rcx
	movq	%rcx, %rsi
	call	PromoteObj
	movq	%rax, %r13
	movq	-72(%rbp), %rax
	movq	-80(%rbp), %rcx
	movq	-88(%rbp), %rdx
	movq	%r14, %rdi
	movq	%r15, %r8
	movq	-104(%rbp), %r9
	movq	-96(%rbp), %r10
	movq	%rbx, %r11
	movq	120(%r12), %rsi
	movq	$3, (%r12)
	movq	-64(%rbp), %rbx
	movq	-56(%rbp), %r12
	movq	%r13, (%r12)
	movq	$-1048576, %r12
	andq	%rsi, %r12
	movq	%rsi, 120(%r12)
	movq	$1, (%r12)
	movq	%rax, -96(%rbp)
	movq	%rcx, -104(%rbp)
	movq	%rdx, %r14
	movq	%rdi, %r15
	movq	%r8, %rbx
	movq	%r9, -88(%rbp)
	movq	%r10, -80(%rbp)
	movq	%r11, -72(%rbp)
	movq	$-1048576, %r13
	andq	%rsi, %r13
	movq	%r13, %rdi
	movq	-64(%rbp), %r13
	movq	72(%r13), %rcx
	movq	%rcx, %rsi
	call	PromoteObj
	movq	%rax, %r13
	movq	-96(%rbp), %rax
	movq	-104(%rbp), %rcx
	movq	%r14, %rdx
	movq	%r15, %rdi
	movq	%rbx, %r8
	movq	-88(%rbp), %r9
	movq	-80(%rbp), %r10
	movq	-72(%rbp), %r11
	movq	120(%r12), %rsi
	movq	$3, (%r12)
	movq	-64(%rbp), %r14
	movl	$3, %r14d
	lock
	xaddl	%r14d, (%r13)
	cmpl	$2, %r14d
	jne	L408
L_true401:
	movq	-56(%rbp), %r12
	movq	-64(%rbp), %r10
then.403:
	/* Liveout:  GP={%r12 %r10 %r9 %r8 %rdi}  */
	/* block then<F87A> (ep<F878>,r<F879>) */
	movq	80(%r10), %rdi
	movq	(%r12), %r14
	movl	(%r14), %r8d
	movq	8(%r12), %r15
	movl	(%r15), %r9d
	movq	16(%r12), %r10
	movq	24(%r12), %r12
	jmp	letJoinK.3CD
doGC407:
	movq	$20, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%r13, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	%rcx, %rdi
	movabsq	$retGC405, %r8
	jmp	ASM_InvokeGC
	/* live= GP={%r13 %rbx} spilled=  */
retGC405:
	movq	8(%rdi), %r13
	movq	(%rdi), %rbx
	jmp	gcTest406
S_case3FF:
case.400:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block case<F88D> (ep<F88C>) */
	movq	$3083, -8(%rsi)
	movabsq	$letJoinK.3F8, %r15
	movq	%r15, (%rsi)
	movl	24(%rbx), %ecx
	movl	%ecx, 8(%rsi)
	movl	32(%rbx), %r10d
	movl	%r10d, 16(%rsi)
	movq	80(%rbx), %r12
	movq	%r12, 24(%rsi)
	movq	88(%rbx), %r13
	movq	%r13, 32(%rsi)
	movq	%rsi, %r14
	addq	$48, %rsi
	movq	8(%rbx), %r15
	movq	(%r15), %rdi
	movq	40(%rbx), %r8
	movq	%r14, %r9
	movq	16(%rbx), %r10
	jmp	m.3DB
L408:
else.402:
	/* Liveout:  GP={%rax %rdi}  */
	/* block else<F884> () */
	movq	$-1048576, %rcx
	andq	%rsi, %rcx
	movq	$3, 8(%rcx)
	movq	40(%rcx), %rdx
	movq	8(%rdx), %rbx
	movq	%rbx, 40(%rcx)
	movq	(%rdx), %r10
	movq	(%r10), %r12
	movq	$1, %r13
	movq	%r13, %rax
	movq	%r10, %rdi
	jmp	*%r12
	.text
letJoinK.410:
	movq	%r8, %rcx
	movq	%rdi, %rdx
gcTest412:
	movq	%r11, %rbx
	subq	%rsi, %rbx
	jle	doGC413
check.409:
	/* block check<10C8B> (ep<F844>,v_2<F838>) */
	movq	$-1048576, %r10
	andq	%rsi, %r10
	movq	32(%r10), %r12
	movq	8(%r12), %rbx
	cmpq	$1, %rbx
	jne	L_true40A
else.40B:
	/* Liveout:  GP={%rax %rdi}  */
	/* block else<F8AE> (ep<F8AD>) */
	movq	$133, -8(%rsi)
	movabsq	$str87, %r14
	movq	%r14, (%rsi)
	movl	$51, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movabsq	$tag74, %r15
	movq	%r15, (%rsi)
	movq	%r13, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	16(%rdx), %r14
	movq	(%r14), %rcx
	movq	%r15, %rax
	movq	%r14, %rdi
	jmp	*%rcx
doGC413:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	%rcx, %rdi
	movabsq	$retGC411, %r8
	jmp	ASM_InvokeGC
	/* live= GP={%rcx %rdx} spilled=  */
retGC411:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
	jmp	gcTest412
L_true40A:
then.40C:
	/* block then<F84E> (ep<F84B>,v_2<F84D>,_t<F84C>) */
	movq	(%rbx), %rbx
	movq	(%rbx), %r13
	cmpq	$1, %r13
	je	L414
L_true40D:
then.40F:
	/* Liveout:  GP={%r9 %r8 %rax %rdi}  */
	/* block then<F856> (ep<F853>,v_2<F855>,stk<F854>) */
	movq	(%r13), %r15
	movq	16(%r15), %r13
	movq	$520985, -8(%rsi)
	movabsq	$letJoinK.404, %rbx
	movq	%rbx, (%rsi)
	movq	8(%rdx), %r10
	movq	%r10, 8(%rsi)
	movq	16(%rdx), %r12
	movq	%r12, 16(%rsi)
	movl	24(%rdx), %r14d
	movl	%r14d, 24(%rsi)
	movl	32(%rdx), %r15d
	movl	%r15d, 32(%rsi)
	movq	40(%rdx), %rbx
	movq	%rbx, 40(%rsi)
	movq	48(%rdx), %r10
	movq	%r10, 48(%rsi)
	movq	56(%rdx), %r12
	movq	%r12, 56(%rsi)
	movq	64(%rdx), %r14
	movq	%r14, 64(%rsi)
	movq	72(%rdx), %r15
	movq	%r15, 72(%rsi)
	movq	80(%rdx), %rbx
	movq	%rbx, 80(%rsi)
	movq	%rcx, 88(%rsi)
	movq	%rsi, %rbx
	addq	$104, %rsi
	movq	8(%r13), %r10
	movq	16(%rdx), %r15
	movq	88(%rdx), %r14
	movq	(%r13), %r12
	movq	%r15, %r9
	movq	%rbx, %r8
	movq	%r14, %rax
	movq	%r12, %rdi
	jmp	*%r10
L414:
else.40E:
	/* Liveout:  GP={%rax %rdi}  */
	/* block else<F8A5> (ep<F8A4>) */
	movq	$133, -8(%rsi)
	movabsq	$str377, %r12
	movq	%r12, (%rsi)
	movl	$44, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movabsq	$tag74, %r13
	movq	%r13, (%rsi)
	movq	%r10, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	16(%rdx), %r10
	movq	(%r10), %r14
	movq	%r12, %rax
	movq	%r10, %rdi
	jmp	*%r14
	.text
letJoinK.41D:
	movq	%rax, %r13
	movq	%rdi, %rbx
gcTest41F:
	movq	%r11, %r12
	subq	%rsi, %r12
	jle	doGC420
check.415:
	/* block check<10C8E> (ep<F811>,notStolen_2<F804>) */
	cmpq	$1, %r13
	je	S_case416
	cmpq	$3, %r13
	je	S_case418
S_case416:
	movq	%rbx, -64(%rbp)
case.417:
	/* flushLoads */
	/* flushLoads */
	/* block case<F815> (ep<F814>) */
	movq	$-1048576, %r12
	andq	%rsi, %r12
	movq	%rsi, 120(%r12)
	movq	$1, (%r12)
	movq	%rax, -96(%rbp)
	movq	%rcx, -72(%rbp)
	movq	%rdx, %r13
	movq	%rdi, %r14
	movq	%r8, -88(%rbp)
	movq	%r9, -80(%rbp)
	movq	%r10, %r15
	movq	%r11, %rbx
	movq	$-1048576, %r10
	andq	%rsi, %r10
	movq	%r10, %rdi
	movq	-64(%rbp), %rcx
	movq	72(%rcx), %rcx
	movq	%rcx, %rsi
	call	PromoteObj
	movq	%rax, -56(%rbp)
	movq	-96(%rbp), %rax
	movq	-72(%rbp), %rcx
	movq	%r13, %rdx
	movq	%r14, %rdi
	movq	-88(%rbp), %r8
	movq	-80(%rbp), %r9
	movq	%r15, %r10
	movq	%rbx, %r11
	movq	120(%r12), %rsi
	movq	$3, (%r12)
	movq	$-1048576, %r12
	andq	%rsi, %r12
	movq	%rsi, 120(%r12)
	movq	$1, (%r12)
	movq	%rax, %r14
	movq	%rcx, -72(%rbp)
	movq	%rdx, -80(%rbp)
	movq	%rdi, -88(%rbp)
	movq	%r8, -96(%rbp)
	movq	%r9, -104(%rbp)
	movq	%r10, %r15
	movq	%r11, %rbx
	movq	$-1048576, %r13
	andq	%rsi, %r13
	movq	%r13, %rdi
	movq	-64(%rbp), %rdx
	movq	64(%rdx), %rdx
	movq	%rdx, %rsi
	call	PromoteObj
	movq	%rax, %r13
	movq	%r14, %rax
	movq	-72(%rbp), %rcx
	movq	-80(%rbp), %rdx
	movq	-88(%rbp), %rdi
	movq	-96(%rbp), %r8
	movq	-104(%rbp), %r9
	movq	%r15, %r10
	movq	%rbx, %r11
	movq	120(%r12), %rsi
	movq	$3, (%r12)
	movq	-64(%rbp), %rbx
	movq	-56(%rbp), %r12
	movq	%r13, 8(%r12)
	movq	$-1048576, %r12
	andq	%rsi, %r12
	movq	%rsi, 120(%r12)
	movq	$1, (%r12)
	movq	%rax, -104(%rbp)
	movq	%rcx, %r14
	movq	%rdx, %r15
	movq	%rdi, %rbx
	movq	%r8, -96(%rbp)
	movq	%r9, -88(%rbp)
	movq	%r10, -72(%rbp)
	movq	%r11, -80(%rbp)
	movq	$-1048576, %r13
	andq	%rsi, %r13
	movq	%r13, %rdi
	movq	-64(%rbp), %r13
	movq	56(%r13), %rcx
	movq	%rcx, %rsi
	call	PromoteObj
	movq	%rax, %r13
	movq	-104(%rbp), %rax
	movq	%r14, %rcx
	movq	%r15, %rdx
	movq	%rbx, %rdi
	movq	-96(%rbp), %r8
	movq	-88(%rbp), %r9
	movq	-72(%rbp), %r10
	movq	-80(%rbp), %r11
	movq	120(%r12), %rsi
	movq	$3, (%r12)
	movq	-64(%rbp), %r14
	movq	-56(%rbp), %r15
	movq	%r13, (%r15)
	movq	$-1048576, %r12
	andq	%rsi, %r12
	movq	%rsi, 120(%r12)
	movq	$1, (%r12)
	movq	%rax, -104(%rbp)
	movq	%rcx, -96(%rbp)
	movq	%rdx, -88(%rbp)
	movq	%rdi, %r14
	movq	%r8, %r15
	movq	%r9, -80(%rbp)
	movq	%r10, -72(%rbp)
	movq	%r11, %rbx
	movq	$-1048576, %r13
	andq	%rsi, %r13
	movq	%r13, %rdi
	movq	-64(%rbp), %rcx
	movq	80(%rcx), %rcx
	movq	%rcx, %rsi
	call	PromoteObj
	movq	%rax, %r13
	movq	-104(%rbp), %rax
	movq	-96(%rbp), %rcx
	movq	-88(%rbp), %rdx
	movq	%r14, %rdi
	movq	%r15, %r8
	movq	-80(%rbp), %r9
	movq	-72(%rbp), %r10
	movq	%rbx, %r11
	movq	120(%r12), %rsi
	movq	$3, (%r12)
	movq	-64(%rbp), %rdx
	movl	$2, %r14d
	lock
	xaddl	%r14d, (%r13)
	cmpl	$3, %r14d
	jne	L421
L_true41A:
	movq	-56(%rbp), %r12
	movq	-64(%rbp), %r10
then.41C:
	/* Liveout:  GP={%r12 %r10 %r9 %r8 %rdi}  */
	/* block then<F824> (ep<F822>,r<F823>) */
	movq	88(%r10), %rdi
	movq	(%r12), %r14
	movl	(%r14), %r8d
	movq	8(%r12), %r15
	movl	(%r15), %r9d
	movq	16(%r12), %r10
	movq	24(%r12), %r12
	jmp	letJoinK.3CD
doGC420:
	movq	$20, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%r13, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	%rcx, %rdi
	movabsq	$retGC41E, %r8
	jmp	ASM_InvokeGC
	/* live= GP={%r13 %rbx} spilled=  */
retGC41E:
	movq	8(%rdi), %r13
	movq	(%rdi), %rbx
	jmp	gcTest41F
S_case418:
case.419:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block case<F837> (ep<F836>) */
	movq	$520985, -8(%rsi)
	movabsq	$letJoinK.410, %r15
	movq	%r15, (%rsi)
	movq	8(%rbx), %rcx
	movq	%rcx, 8(%rsi)
	movq	16(%rbx), %r10
	movq	%r10, 16(%rsi)
	movl	24(%rbx), %r12d
	movl	%r12d, 24(%rsi)
	movl	32(%rbx), %r13d
	movl	%r13d, 32(%rsi)
	movq	48(%rbx), %r14
	movq	%r14, 40(%rsi)
	movq	56(%rbx), %r15
	movq	%r15, 48(%rsi)
	movq	64(%rbx), %rcx
	movq	%rcx, 56(%rsi)
	movq	72(%rbx), %rdx
	movq	%rdx, 64(%rsi)
	movq	80(%rbx), %r10
	movq	%r10, 72(%rsi)
	movq	88(%rbx), %r12
	movq	%r12, 80(%rsi)
	movq	96(%rbx), %r13
	movq	%r13, 88(%rsi)
	movq	%rsi, %r14
	addq	$104, %rsi
	movq	8(%rbx), %r15
	movq	(%r15), %rdi
	movq	40(%rbx), %r8
	movq	%r14, %r9
	movq	16(%rbx), %r10
	jmp	m.3DB
L421:
else.41B:
	/* Liveout:  GP={%rax %rdi}  */
	/* block else<F82E> () */
	movq	$-1048576, %rcx
	andq	%rsi, %rcx
	movq	$3, 8(%rcx)
	movq	40(%rcx), %rdx
	movq	8(%rdx), %rbx
	movq	%rbx, 40(%rcx)
	movq	(%rdx), %r10
	movq	(%r10), %r12
	movq	$1, %r13
	movq	%r13, %rax
	movq	%r10, %rdi
	jmp	*%r12
	.text
letJoinK.430:
	movq	%rax, %r13
	movq	%rdi, %rbx
	jmp	gcTest432
	/* live= GP={%r13 %rbx} spilled=  */
retGC431:
	movq	8(%rdi), %r13
	movq	(%rdi), %rbx
gcTest432:
	movq	%r11, %r12
	subq	%rsi, %r12
	jle	doGC433
check.422:
	/* block check<10C91> (ep<F7CE>,notStolen_1<F7C0>) */
	cmpq	$1, %r13
	je	S_case423
	cmpq	$3, %r13
	je	S_case425
S_case423:
	movq	%rbx, -64(%rbp)
case.424:
	/* flushLoads */
	/* block case<F7D2> (ep<F7D1>) */
	movq	$-1048576, %r12
	andq	%rsi, %r12
	movq	%rsi, 120(%r12)
	movq	$1, (%r12)
	movq	%rax, -80(%rbp)
	movq	%rcx, %r13
	movq	%rdx, %r14
	movq	%rdi, %r15
	movq	%r8, -72(%rbp)
	movq	%r9, -88(%rbp)
	movq	%r10, -96(%rbp)
	movq	%r11, %rbx
	movq	$-1048576, %r10
	andq	%rsi, %r10
	movq	%r10, %rdi
	movq	-64(%rbp), %rcx
	movq	72(%rcx), %r10
	movq	%r10, %rsi
	call	PromoteObj
	movq	%rax, -56(%rbp)
	movq	-80(%rbp), %rax
	movq	%r13, %rcx
	movq	%r14, %rdx
	movq	%r15, %rdi
	movq	-72(%rbp), %r8
	movq	-88(%rbp), %r9
	movq	-96(%rbp), %r10
	movq	%rbx, %r11
	movq	120(%r12), %rsi
	movq	$3, (%r12)
	movq	$-1048576, %r12
	andq	%rsi, %r12
	movq	%rsi, 120(%r12)
	movq	$1, (%r12)
	movq	%rax, %r14
	movq	%rcx, -72(%rbp)
	movq	%rdx, -80(%rbp)
	movq	%rdi, -88(%rbp)
	movq	%r8, -96(%rbp)
	movq	%r9, -104(%rbp)
	movq	%r10, %r15
	movq	%r11, %rbx
	movq	$-1048576, %r13
	andq	%rsi, %r13
	movq	%r13, %rdi
	movq	-64(%rbp), %rdx
	movq	56(%rdx), %r13
	movq	%r13, %rsi
	call	PromoteObj
	movq	%rax, %r13
	movq	%r14, %rax
	movq	-72(%rbp), %rcx
	movq	-80(%rbp), %rdx
	movq	-88(%rbp), %rdi
	movq	-96(%rbp), %r8
	movq	-104(%rbp), %r9
	movq	%r15, %r10
	movq	%rbx, %r11
	movq	120(%r12), %rsi
	movq	$3, (%r12)
	movq	-64(%rbp), %rbx
	movq	-56(%rbp), %r12
	movq	%r13, (%r12)
	movq	$-1048576, %r12
	andq	%rsi, %r12
	movq	%rsi, 120(%r12)
	movq	$1, (%r12)
	movq	%rax, -104(%rbp)
	movq	%rcx, %r14
	movq	%rdx, %r15
	movq	%rdi, %rbx
	movq	%r8, -88(%rbp)
	movq	%r9, -80(%rbp)
	movq	%r10, -72(%rbp)
	movq	%r11, -96(%rbp)
	movq	$-1048576, %r13
	andq	%rsi, %r13
	movq	%r13, %rdi
	movq	-64(%rbp), %r13
	movq	80(%r13), %rcx
	movq	%rcx, %rsi
	call	PromoteObj
	movq	%rax, %r13
	movq	-104(%rbp), %rax
	movq	%r14, %rcx
	movq	%r15, %rdx
	movq	%rbx, %rdi
	movq	-88(%rbp), %r8
	movq	-80(%rbp), %r9
	movq	-72(%rbp), %r10
	movq	-96(%rbp), %r11
	movq	120(%r12), %rsi
	movq	$3, (%r12)
	movq	-64(%rbp), %r14
	movl	$1, %r14d
	lock
	xaddl	%r14d, (%r13)
	cmpl	$4, %r14d
	jne	L434
L_true42D:
	movq	-56(%rbp), %r12
	movq	-64(%rbp), %r10
then.42F:
	/* Liveout:  GP={%r12 %r10 %r9 %r8 %rdi}  */
	/* block then<F7DF> (ep<F7DD>,r<F7DE>) */
	movq	88(%r10), %rdi
	movq	(%r12), %r14
	movl	(%r14), %r8d
	movq	8(%r12), %r15
	movl	(%r15), %r9d
	movq	16(%r12), %r10
	movq	24(%r12), %r12
	jmp	letJoinK.3CD
doGC433:
	movq	$20, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%r13, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	%rcx, %rdi
	movabsq	$retGC431, %r8
	jmp	ASM_InvokeGC
S_case425:
case.426:
	/* block case<F7F2> (ep<F7F1>) */
	movq	$-1048576, %rcx
	andq	%rsi, %rcx
	movq	32(%rcx), %r15
	movq	8(%r15), %r14
	cmpq	$1, %r14
	jne	L_true427
else.428:
	/* Liveout:  GP={%rax %rdi}  */
	/* block else<F8C9> (ep<F8C8>) */
	movq	$133, -8(%rsi)
	movabsq	$str87, %r12
	movq	%r12, (%rsi)
	movl	$51, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movabsq	$tag74, %r14
	movq	%r14, (%rsi)
	movq	%r10, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	16(%rbx), %r15
	movq	(%r15), %rcx
	movq	%r13, %rax
	movq	%r15, %rdi
	jmp	*%rcx
L_true427:
then.429:
	/* block then<F7F9> (ep<F7F7>,_t<F7F8>) */
	movq	(%r14), %r10
	movq	(%r10), %rdx
	cmpq	$1, %rdx
	je	L435
L_true42A:
then.42C:
	/* Liveout:  GP={%r9 %r8 %rax %rdi}  */
	/* block then<F800> (ep<F7FE>,stk<F7FF>) */
	movq	(%rdx), %r10
	movq	16(%r10), %r10
	movq	$1045275, -8(%rsi)
	movabsq	$letJoinK.41D, %r13
	movq	%r13, (%rsi)
	movq	8(%rbx), %r14
	movq	%r14, 8(%rsi)
	movq	16(%rbx), %r15
	movq	%r15, 16(%rsi)
	movl	24(%rbx), %ecx
	movl	%ecx, 24(%rsi)
	movl	32(%rbx), %edx
	movl	%edx, 32(%rsi)
	movq	40(%rbx), %r12
	movq	%r12, 40(%rsi)
	movq	48(%rbx), %r13
	movq	%r13, 48(%rsi)
	movq	56(%rbx), %r14
	movq	%r14, 56(%rsi)
	movq	64(%rbx), %r15
	movq	%r15, 64(%rsi)
	movq	72(%rbx), %rcx
	movq	%rcx, 72(%rsi)
	movq	80(%rbx), %rdx
	movq	%rdx, 80(%rsi)
	movq	88(%rbx), %r12
	movq	%r12, 88(%rsi)
	movq	96(%rbx), %r13
	movq	%r13, 96(%rsi)
	movq	%rsi, %r12
	addq	$112, %rsi
	movq	8(%r10), %r14
	movq	16(%rbx), %rdx
	movq	104(%rbx), %rcx
	movq	(%r10), %r15
	movq	%rdx, %r9
	movq	%r12, %r8
	movq	%rcx, %rax
	movq	%r15, %rdi
	jmp	*%r14
L435:
else.42B:
	/* Liveout:  GP={%rax %rdi}  */
	/* block else<F8C0> (ep<F8BF>) */
	movq	$133, -8(%rsi)
	movabsq	$str377, %r13
	movq	%r13, (%rsi)
	movl	$44, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movabsq	$tag74, %r15
	movq	%r15, (%rsi)
	movq	%r12, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	16(%rbx), %rcx
	movq	(%rcx), %rdx
	movq	%r14, %rax
	movq	%rcx, %rdi
	jmp	*%rdx
L434:
else.42E:
	/* Liveout:  GP={%rax %rdi}  */
	/* block else<F7E9> () */
	movq	$-1048576, %rcx
	andq	%rsi, %rcx
	movq	$3, 8(%rcx)
	movq	40(%rcx), %rdx
	movq	8(%rdx), %rbx
	movq	%rbx, 40(%rcx)
	movq	(%rdx), %r10
	movq	(%r10), %r12
	movq	$1, %r13
	movq	%r13, %rax
	movq	%r10, %rdi
	jmp	*%r12
	.text
letJoinK.43D:
	movq	%rax, %rcx
	movq	%rdi, %rdx
gcTest43F:
	movq	%r11, %rbx
	subq	%rsi, %rbx
	jle	doGC440
check.436:
	/* block check<10C94> (ep<F7AC>,_wild<F79E>) */
	movq	$-1048576, %rbx
	andq	%rsi, %rbx
	movq	32(%rbx), %r10
	movq	8(%r10), %rcx
	cmpq	$1, %rcx
	jne	L_true437
else.438:
	/* Liveout:  GP={%rax %rdi}  */
	/* block else<F8DF> (ep<F8DE>) */
	movq	$133, -8(%rsi)
	movabsq	$str87, %r13
	movq	%r13, (%rsi)
	movl	$51, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movabsq	$tag74, %r14
	movq	%r14, (%rsi)
	movq	%r12, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	16(%rdx), %r14
	movq	(%r14), %rcx
	movq	%r15, %rax
	movq	%r14, %rdi
	jmp	*%rcx
doGC440:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	%rcx, %rdi
	movabsq	$retGC43E, %r8
	jmp	ASM_InvokeGC
	/* live= GP={%rcx %rdx} spilled=  */
retGC43E:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
	jmp	gcTest43F
L_true437:
then.439:
	/* block then<F7B5> (ep<F7B3>,_t<F7B4>) */
	movq	(%rcx), %rbx
	movq	(%rbx), %r13
	cmpq	$1, %r13
	je	L441
L_true43A:
then.43C:
	/* Liveout:  GP={%r9 %r8 %rax %rdi}  */
	/* block then<F7BC> (ep<F7BA>,stk<F7BB>) */
	movq	(%r13), %r15
	movq	16(%r15), %r13
	movq	$2093853, -8(%rsi)
	movabsq	$letJoinK.430, %rcx
	movq	%rcx, (%rsi)
	movq	8(%rdx), %rbx
	movq	%rbx, 8(%rsi)
	movq	16(%rdx), %r10
	movq	%r10, 16(%rsi)
	movl	24(%rdx), %r12d
	movl	%r12d, 24(%rsi)
	movl	32(%rdx), %r14d
	movl	%r14d, 32(%rsi)
	movq	40(%rdx), %r15
	movq	%r15, 40(%rsi)
	movq	48(%rdx), %rcx
	movq	%rcx, 48(%rsi)
	movq	56(%rdx), %rbx
	movq	%rbx, 56(%rsi)
	movq	64(%rdx), %r10
	movq	%r10, 64(%rsi)
	movq	72(%rdx), %r12
	movq	%r12, 72(%rsi)
	movq	80(%rdx), %r14
	movq	%r14, 80(%rsi)
	movq	88(%rdx), %r15
	movq	%r15, 88(%rsi)
	movq	96(%rdx), %rcx
	movq	%rcx, 96(%rsi)
	movq	104(%rdx), %rbx
	movq	%rbx, 104(%rsi)
	movq	%rsi, %rbx
	addq	$120, %rsi
	movq	8(%r13), %r10
	movq	16(%rdx), %r15
	movq	112(%rdx), %r14
	movq	(%r13), %r12
	movq	%r15, %r9
	movq	%rbx, %r8
	movq	%r14, %rax
	movq	%r12, %rdi
	jmp	*%r10
L441:
else.43B:
	/* Liveout:  GP={%rax %rdi}  */
	/* block else<F8D6> (ep<F8D5>) */
	movq	$133, -8(%rsi)
	movabsq	$str377, %r12
	movq	%r12, (%rsi)
	movl	$44, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movabsq	$tag74, %r13
	movq	%r13, (%rsi)
	movq	%r10, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	16(%rdx), %r10
	movq	(%r10), %r14
	movq	%r12, %rax
	movq	%r10, %rdi
	jmp	*%r14
	.text
letJoinK.449:
	movq	%r8, %rcx
	movq	%rdi, %rdx
gcTest44B:
	movq	%r11, %r15
	subq	%rsi, %r15
	jle	doGC44C
check.442:
	/* block check<10C97> (ep<F786>,k'<F778>) */
	movq	$20, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	112(%rdx), %r13
	movq	%r13, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	$-1048576, %r12
	andq	%rsi, %r12
	movq	32(%r12), %r14
	movq	8(%r14), %r10
	cmpq	$1, %r10
	jne	L_true443
else.444:
	/* Liveout:  GP={%rax %rdi}  */
	/* block else<F8F4> (ep<F8F3>) */
	movq	$133, -8(%rsi)
	movabsq	$str87, %rcx
	movq	%rcx, (%rsi)
	movl	$51, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movabsq	$tag74, %rbx
	movq	%rbx, (%rsi)
	movq	%r15, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	16(%rdx), %r15
	movq	(%r15), %r10
	movq	%rcx, %rax
	movq	%r15, %rdi
	jmp	*%r10
doGC44C:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	%r14, %rdi
	movabsq	$retGC44A, %r8
	jmp	ASM_InvokeGC
	/* live= GP={%rcx %rdx} spilled=  */
retGC44A:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
	jmp	gcTest44B
L_true443:
then.445:
	/* block then<F792> (ep<F78F>,thd<F791>,_t<F790>) */
	movq	(%r10), %r12
	movq	(%r12), %r14
	cmpq	$1, %r14
	je	L44D
L_true446:
then.448:
	/* Liveout:  GP={%r9 %r8 %rax %rdi}  */
	/* block then<F79A> (ep<F797>,thd<F799>,stk<F798>) */
	movq	(%r14), %r10
	movq	8(%r10), %r14
	movq	$4191007, -8(%rsi)
	movabsq	$letJoinK.43D, %r12
	movq	%r12, (%rsi)
	movq	8(%rdx), %r13
	movq	%r13, 8(%rsi)
	movq	16(%rdx), %r15
	movq	%r15, 16(%rsi)
	movl	24(%rdx), %ecx
	movl	%ecx, 24(%rsi)
	movl	32(%rdx), %r10d
	movl	%r10d, 32(%rsi)
	movq	40(%rdx), %r12
	movq	%r12, 40(%rsi)
	movq	48(%rdx), %r13
	movq	%r13, 48(%rsi)
	movq	56(%rdx), %r15
	movq	%r15, 56(%rsi)
	movq	64(%rdx), %rcx
	movq	%rcx, 64(%rsi)
	movq	72(%rdx), %r10
	movq	%r10, 72(%rsi)
	movq	80(%rdx), %r12
	movq	%r12, 80(%rsi)
	movq	88(%rdx), %r13
	movq	%r13, 88(%rsi)
	movq	96(%rdx), %r15
	movq	%r15, 96(%rsi)
	movq	104(%rdx), %rcx
	movq	%rcx, 104(%rsi)
	movq	%rbx, 112(%rsi)
	movq	%rsi, %r10
	addq	$128, %rsi
	movq	8(%r14), %r12
	movq	16(%rdx), %r13
	movq	(%r14), %rdx
	movq	%r13, %r9
	movq	%r10, %r8
	movq	%rbx, %rax
	movq	%rdx, %rdi
	jmp	*%r12
L44D:
else.447:
	/* Liveout:  GP={%rax %rdi}  */
	/* block else<F8EB> (ep<F8EA>) */
	movq	$133, -8(%rsi)
	movabsq	$str377, %r14
	movq	%r14, (%rsi)
	movl	$44, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movabsq	$tag74, %r15
	movq	%r15, (%rsi)
	movq	%r13, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	16(%rdx), %r12
	movq	(%r12), %rcx
	movq	%r13, %rax
	movq	%r12, %rdi
	jmp	*%rcx
	.text
letJoinK.44F:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest451
	/* live= GP={%rcx %rdx} spilled=  */
retGC450:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest451:
	movq	%r11, %r10
	subq	%rsi, %r10
	jle	doGC452
check.44E:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<10C9A> (ep<F904>,k<F903>) */
	movq	8(%rdx), %rdi
	movq	%rcx, %r8
	jmp	letJoinK.449
doGC452:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	%rbx, %rdi
	movabsq	$retGC450, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.45A:
	movq	%rax, %rcx
	movq	%rdi, %rdx
gcTest45C:
	movq	%r11, %rbx
	subq	%rsi, %rbx
	jle	doGC45D
check.453:
	/* block check<10C9D> (ep<F73F>,_wild<F731>) */
	movq	$44, -8(%rsi)
	movabsq	$slowClone_1.3F2, %r12
	movq	%r12, (%rsi)
	movq	72(%rdx), %r13
	movq	%r13, 8(%rsi)
	movq	80(%rdx), %r14
	movq	%r14, 16(%rsi)
	movq	88(%rdx), %r15
	movq	%r15, 24(%rsi)
	movq	96(%rdx), %rcx
	movq	%rcx, 32(%rsi)
	movq	%rsi, %r13
	addq	$48, %rsi
	movq	$-1048576, %rbx
	andq	%rsi, %rbx
	movq	32(%rbx), %rbx
	movq	8(%rbx), %rcx
	cmpq	$1, %rcx
	jne	L_true454
else.455:
	/* Liveout:  GP={%rax %rdi}  */
	/* block else<F913> (ep<F912>) */
	movq	$133, -8(%rsi)
	movabsq	$str87, %r12
	movq	%r12, (%rsi)
	movl	$51, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movabsq	$tag74, %r13
	movq	%r13, (%rsi)
	movq	%r10, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	24(%rdx), %r14
	movq	(%r14), %rcx
	movq	%r15, %rax
	movq	%r14, %rdi
	jmp	*%rcx
doGC45D:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	%r15, %rdi
	movabsq	$retGC45B, %r8
	jmp	ASM_InvokeGC
	/* live= GP={%rcx %rdx} spilled=  */
retGC45B:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
	jmp	gcTest45C
L_true454:
	movq	$1, %r10
then.456:
	/* block then<F771> (ep<F76D>,slowClone_1<F770>,con_NONE<F76F>,_t<F76E>) */
	movq	(%rcx), %rbx
	movq	$20, -8(%rsi)
	movq	(%rbx), %r12
	movq	%r12, (%rsi)
	movq	%r10, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	8(%rbx), %r12
	movq	$4191007, -8(%rsi)
	movabsq	$letJoinK.449, %r14
	movq	%r14, (%rsi)
	movq	16(%rdx), %r15
	movq	%r15, 8(%rsi)
	movq	24(%rdx), %rcx
	movq	%rcx, 16(%rsi)
	movl	32(%rdx), %r14d
	movl	%r14d, 24(%rsi)
	movl	40(%rdx), %r15d
	movl	%r15d, 32(%rsi)
	movq	48(%rdx), %rcx
	movq	%rcx, 40(%rsi)
	movq	56(%rdx), %rbx
	movq	%rbx, 48(%rsi)
	movq	64(%rdx), %r14
	movq	%r14, 56(%rsi)
	movq	72(%rdx), %r15
	movq	%r15, 64(%rsi)
	movq	80(%rdx), %rcx
	movq	%rcx, 72(%rsi)
	movq	88(%rdx), %rbx
	movq	%rbx, 80(%rsi)
	movq	96(%rdx), %r14
	movq	%r14, 88(%rsi)
	movq	104(%rdx), %r15
	movq	%r15, 96(%rsi)
	movq	112(%rdx), %rcx
	movq	%rcx, 104(%rsi)
	movq	%r10, 112(%rsi)
	movq	%rsi, %rbx
	addq	$128, %rsi
	cmpq	$1, %r12
	je	L45E
L_true457:
then.459:
	/* Liveout:  GP={%r12 %r10 %r9 %r8 %rdi}  */
	/* block then<F900> (ep<F8FC>,slowClone_1<F8FF>,c<F8FE>,letJoinK<F8FD>) */
	movq	$261, -8(%rsi)
	movabsq	$letJoinK.44F, %r10
	movq	%r10, (%rsi)
	movq	%rbx, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	8(%rdx), %r14
	movq	(%r14), %rdi
	movq	(%r12), %r8
	movq	%r13, %r9
	movq	24(%rdx), %r12
	jmp	wrap.F3
L45E:
else.458:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block else<F910> (slowClone_1<F90F>,letJoinK<F90E>) */
	movq	%rbx, %rdi
	movq	%r13, %r8
	jmp	letJoinK.449
	.text
letJoinK.466:
	movq	%r8, %rcx
	movq	%rdi, %rdx
gcTest468:
	movq	%r11, %r15
	subq	%rsi, %r15
	jle	doGC469
check.45F:
	/* block check<10CA0> (ep<F719>,k'<F70B>) */
	movq	$20, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	112(%rdx), %r13
	movq	%r13, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	$-1048576, %r12
	andq	%rsi, %r12
	movq	32(%r12), %r14
	movq	8(%r14), %r10
	cmpq	$1, %r10
	jne	L_true460
else.461:
	/* Liveout:  GP={%rax %rdi}  */
	/* block else<F928> (ep<F927>) */
	movq	$133, -8(%rsi)
	movabsq	$str87, %rcx
	movq	%rcx, (%rsi)
	movl	$51, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movabsq	$tag74, %rbx
	movq	%rbx, (%rsi)
	movq	%r15, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	24(%rdx), %r15
	movq	(%r15), %r10
	movq	%rcx, %rax
	movq	%r15, %rdi
	jmp	*%r10
doGC469:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	%r14, %rdi
	movabsq	$retGC467, %r8
	jmp	ASM_InvokeGC
	/* live= GP={%rcx %rdx} spilled=  */
retGC467:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
	jmp	gcTest468
L_true460:
then.462:
	/* block then<F725> (ep<F722>,thd<F724>,_t<F723>) */
	movq	(%r10), %r12
	movq	(%r12), %r14
	cmpq	$1, %r14
	je	L46A
L_true463:
then.465:
	/* Liveout:  GP={%r9 %r8 %rax %rdi}  */
	/* block then<F72D> (ep<F72A>,thd<F72C>,stk<F72B>) */
	movq	(%r14), %r10
	movq	8(%r10), %r14
	movq	$4187935, -8(%rsi)
	movabsq	$letJoinK.45A, %r12
	movq	%r12, (%rsi)
	movq	8(%rdx), %r13
	movq	%r13, 8(%rsi)
	movq	16(%rdx), %r15
	movq	%r15, 16(%rsi)
	movq	24(%rdx), %rcx
	movq	%rcx, 24(%rsi)
	movl	32(%rdx), %r10d
	movl	%r10d, 32(%rsi)
	movl	40(%rdx), %r12d
	movl	%r12d, 40(%rsi)
	movq	48(%rdx), %r13
	movq	%r13, 48(%rsi)
	movq	56(%rdx), %r15
	movq	%r15, 56(%rsi)
	movq	64(%rdx), %rcx
	movq	%rcx, 64(%rsi)
	movq	72(%rdx), %r10
	movq	%r10, 72(%rsi)
	movq	80(%rdx), %r12
	movq	%r12, 80(%rsi)
	movq	88(%rdx), %r13
	movq	%r13, 88(%rsi)
	movq	96(%rdx), %r15
	movq	%r15, 96(%rsi)
	movq	104(%rdx), %rcx
	movq	%rcx, 104(%rsi)
	movq	%rbx, 112(%rsi)
	movq	%rsi, %r10
	addq	$128, %rsi
	movq	8(%r14), %r12
	movq	24(%rdx), %r13
	movq	(%r14), %rdx
	movq	%r13, %r9
	movq	%r10, %r8
	movq	%rbx, %rax
	movq	%rdx, %rdi
	jmp	*%r12
L46A:
else.464:
	/* Liveout:  GP={%rax %rdi}  */
	/* block else<F91F> (ep<F91E>) */
	movq	$133, -8(%rsi)
	movabsq	$str377, %r14
	movq	%r14, (%rsi)
	movl	$44, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movabsq	$tag74, %r15
	movq	%r15, (%rsi)
	movq	%r13, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	24(%rdx), %r12
	movq	(%r12), %rcx
	movq	%r13, %rax
	movq	%r12, %rdi
	jmp	*%rcx
	.text
letJoinK.46C:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest46E
	/* live= GP={%rcx %rdx} spilled=  */
retGC46D:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest46E:
	movq	%r11, %r10
	subq	%rsi, %r10
	jle	doGC46F
check.46B:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<10CA3> (ep<F938>,k<F937>) */
	movq	8(%rdx), %rdi
	movq	%rcx, %r8
	jmp	letJoinK.466
doGC46F:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	%rbx, %rdi
	movabsq	$retGC46D, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.477:
	movq	%rax, %rcx
	movq	%rdi, %rdx
gcTest479:
	movq	%r11, %rbx
	subq	%rsi, %rbx
	jle	doGC47A
check.470:
	/* block check<10CA6> (ep<F6C2>,_wild<F6B5>) */
	movq	$60, -8(%rsi)
	movabsq	$slowClone_2.3EA, %r12
	movq	%r12, (%rsi)
	movq	16(%rdx), %r13
	movq	%r13, 8(%rsi)
	movq	24(%rdx), %r14
	movq	%r14, 16(%rsi)
	movq	48(%rdx), %r15
	movq	%r15, 24(%rsi)
	movq	80(%rdx), %rcx
	movq	%rcx, 32(%rsi)
	movq	88(%rdx), %rbx
	movq	%rbx, 40(%rsi)
	movq	96(%rdx), %r10
	movq	%r10, 48(%rsi)
	movq	%rsi, %r13
	addq	$64, %rsi
	movq	$-1048576, %rbx
	andq	%rsi, %rbx
	movq	32(%rbx), %r12
	movq	8(%r12), %rcx
	cmpq	$1, %rcx
	jne	L_true471
else.472:
	/* Liveout:  GP={%rax %rdi}  */
	/* block else<F947> (ep<F946>) */
	movq	$133, -8(%rsi)
	movabsq	$str87, %r14
	movq	%r14, (%rsi)
	movl	$51, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movabsq	$tag74, %r15
	movq	%r15, (%rsi)
	movq	%r13, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	24(%rdx), %r14
	movq	(%r14), %rcx
	movq	%r15, %rax
	movq	%r14, %rdi
	jmp	*%rcx
doGC47A:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	%r15, %rdi
	movabsq	$retGC478, %r8
	jmp	ASM_InvokeGC
	/* live= GP={%rcx %rdx} spilled=  */
retGC478:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
	jmp	gcTest479
L_true471:
	movq	$1, %r10
then.473:
	/* block then<F704> (ep<F700>,slowClone_2<F703>,con_NONE<F702>,_t<F701>) */
	movq	(%rcx), %rbx
	movq	$20, -8(%rsi)
	movq	(%rbx), %r12
	movq	%r12, (%rsi)
	movq	%r10, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	8(%rbx), %r12
	movq	$4187935, -8(%rsi)
	movabsq	$letJoinK.466, %r14
	movq	%r14, (%rsi)
	movq	8(%rdx), %r15
	movq	%r15, 8(%rsi)
	movq	16(%rdx), %rcx
	movq	%rcx, 16(%rsi)
	movq	24(%rdx), %r14
	movq	%r14, 24(%rsi)
	movl	32(%rdx), %r15d
	movl	%r15d, 32(%rsi)
	movl	40(%rdx), %ecx
	movl	%ecx, 40(%rsi)
	movq	48(%rdx), %rbx
	movq	%rbx, 48(%rsi)
	movq	56(%rdx), %r14
	movq	%r14, 56(%rsi)
	movq	64(%rdx), %r15
	movq	%r15, 64(%rsi)
	movq	72(%rdx), %rcx
	movq	%rcx, 72(%rsi)
	movq	80(%rdx), %rbx
	movq	%rbx, 80(%rsi)
	movq	88(%rdx), %r14
	movq	%r14, 88(%rsi)
	movq	96(%rdx), %r15
	movq	%r15, 96(%rsi)
	movq	104(%rdx), %rcx
	movq	%rcx, 104(%rsi)
	movq	%r10, 112(%rsi)
	movq	%rsi, %rbx
	addq	$128, %rsi
	cmpq	$1, %r12
	je	L47B
L_true474:
then.476:
	/* Liveout:  GP={%r12 %r10 %r9 %r8 %rdi}  */
	/* block then<F934> (ep<F930>,slowClone_2<F933>,c<F932>,letJoinK<F931>) */
	movq	$261, -8(%rsi)
	movabsq	$letJoinK.46C, %r10
	movq	%r10, (%rsi)
	movq	%rbx, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	8(%rdx), %r14
	movq	(%r14), %rdi
	movq	(%r12), %r8
	movq	%r13, %r9
	movq	24(%rdx), %r12
	jmp	wrap.F3
L47B:
else.475:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block else<F944> (slowClone_2<F943>,letJoinK<F942>) */
	movq	%rbx, %rdi
	movq	%r13, %r8
	jmp	letJoinK.466
	.text
letJoinK.483:
	movq	%r8, %rcx
	movq	%rdi, %rdx
gcTest485:
	movq	%r11, %r15
	subq	%rsi, %r15
	jle	doGC486
check.47C:
	/* block check<10CA9> (ep<F69D>,k'<F699>) */
	movq	$20, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	104(%rdx), %r13
	movq	%r13, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	$-1048576, %r12
	andq	%rsi, %r12
	movq	32(%r12), %r14
	movq	8(%r14), %r10
	cmpq	$1, %r10
	jne	L_true47D
else.47E:
	/* Liveout:  GP={%rax %rdi}  */
	/* block else<F95C> (ep<F95B>) */
	movq	$133, -8(%rsi)
	movabsq	$str87, %rcx
	movq	%rcx, (%rsi)
	movl	$51, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movabsq	$tag74, %rbx
	movq	%rbx, (%rsi)
	movq	%r15, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	24(%rdx), %r15
	movq	(%r15), %r10
	movq	%rcx, %rax
	movq	%r15, %rdi
	jmp	*%r10
doGC486:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	%r14, %rdi
	movabsq	$retGC484, %r8
	jmp	ASM_InvokeGC
	/* live= GP={%rcx %rdx} spilled=  */
retGC484:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
	jmp	gcTest485
L_true47D:
then.47F:
	/* block then<F6A9> (ep<F6A6>,thd<F6A8>,_t<F6A7>) */
	movq	(%r10), %r12
	movq	(%r12), %r14
	cmpq	$1, %r14
	je	L487
L_true480:
then.482:
	/* Liveout:  GP={%r9 %r8 %rax %rdi}  */
	/* block then<F6B1> (ep<F6AE>,thd<F6B0>,stk<F6AF>) */
	movq	(%r14), %r10
	movq	8(%r10), %r14
	movq	$2090781, -8(%rsi)
	movabsq	$letJoinK.477, %r12
	movq	%r12, (%rsi)
	movq	8(%rdx), %r13
	movq	%r13, 8(%rsi)
	movq	16(%rdx), %r15
	movq	%r15, 16(%rsi)
	movq	24(%rdx), %rcx
	movq	%rcx, 24(%rsi)
	movl	32(%rdx), %r10d
	movl	%r10d, 32(%rsi)
	movl	40(%rdx), %r12d
	movl	%r12d, 40(%rsi)
	movq	48(%rdx), %r13
	movq	%r13, 48(%rsi)
	movq	56(%rdx), %r15
	movq	%r15, 56(%rsi)
	movq	64(%rdx), %rcx
	movq	%rcx, 64(%rsi)
	movq	72(%rdx), %r10
	movq	%r10, 72(%rsi)
	movq	80(%rdx), %r12
	movq	%r12, 80(%rsi)
	movq	88(%rdx), %r13
	movq	%r13, 88(%rsi)
	movq	96(%rdx), %r15
	movq	%r15, 96(%rsi)
	movq	%rbx, 104(%rsi)
	movq	%rsi, %r10
	addq	$120, %rsi
	movq	8(%r14), %r12
	movq	24(%rdx), %r13
	movq	(%r14), %rcx
	movq	%r13, %r9
	movq	%r10, %r8
	movq	%rbx, %rax
	movq	%rcx, %rdi
	jmp	*%r12
L487:
else.481:
	/* Liveout:  GP={%rax %rdi}  */
	/* block else<F953> (ep<F952>) */
	movq	$133, -8(%rsi)
	movabsq	$str377, %r14
	movq	%r14, (%rsi)
	movl	$44, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movabsq	$tag74, %r15
	movq	%r15, (%rsi)
	movq	%r13, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	24(%rdx), %r12
	movq	(%r12), %rcx
	movq	%r13, %rax
	movq	%r12, %rdi
	jmp	*%rcx
	.text
letJoinK.489:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest48B
	/* live= GP={%rcx %rdx} spilled=  */
retGC48A:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest48B:
	movq	%r11, %r10
	subq	%rsi, %r10
	jle	doGC48C
check.488:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<10CAC> (ep<F96D>,k<F96C>) */
	movq	8(%rdx), %rdi
	movq	%rcx, %r8
	jmp	letJoinK.483
doGC48C:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	%rbx, %rdi
	movabsq	$retGC48A, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.48E:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest490
	/* live= GP={%rcx %rdx %rbx} spilled=  */
retGC48F:
	movl	16(%rdi), %ecx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest490:
	movq	%r11, %r10
	subq	%rsi, %r10
	jle	doGC491
check.48D:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<10CB0> (ep<F98F>,_t<F98D>,_t<F98E>) */
	movq	$521, -8(%rsi)
	movq	$3, (%rsi)
	movl	16(%rbx), %r12d
	movl	%r12d, 8(%rsi)
	movq	%rdx, 16(%rsi)
	movl	%ecx, 24(%rsi)
	movq	%rsi, %r10
	addq	$40, %rsi
	movq	8(%rbx), %r13
	movq	(%r13), %r14
	movq	%r13, %rdi
	movq	%r10, %r8
	jmp	*%r14
doGC491:
	movq	$391, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movl	%ecx, 16(%rsi)
	movq	%rsi, %r15
	addq	$32, %rsi
	movq	%r15, %rdi
	movabsq	$retGC48F, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.494:
	movq	%r8, %r14
	movq	%rdi, %r15
	jmp	gcTest496
	/* live= GP={%r14 %r15} spilled=  */
retGC495:
	movq	8(%rdi), %r14
	movq	(%rdi), %r15
gcTest496:
	movq	%r11, %rbx
	subq	%rsi, %rbx
	jle	doGC497
check.492:
	/* Liveout:  GP={%r12 %r10 %r9 %r8 %rdi}  */
	/* block check<10CB3> (ep<F9E3>,_t<F9DF>) */
	movq	$-1048576, %rbx
	andq	%rsi, %rbx
	movq	%rsi, 120(%rbx)
	movq	$1, (%rbx)
	movq	%rax, -72(%rbp)
	movq	%rcx, -96(%rbp)
	movq	%rdx, %r12
	movq	%rdi, -56(%rbp)
	movq	%r8, -64(%rbp)
	movq	%r9, -80(%rbp)
	movq	%r10, -88(%rbp)
	movq	%r11, %r13
	movq	$-1048576, %rcx
	andq	%rsi, %rcx
	movq	%rcx, %rdi
	movq	%r14, %rsi
	call	PromoteObj
	movq	%rax, %r14
	movq	-72(%rbp), %rax
	movq	-96(%rbp), %rcx
	movq	%r12, %rdx
	movq	-56(%rbp), %rdi
	movq	-64(%rbp), %r8
	movq	-80(%rbp), %r9
	movq	-88(%rbp), %r10
	movq	%r13, %r11
	movq	120(%rbx), %rsi
	movq	$3, (%rbx)
	movq	40(%r15), %rdx
	movq	(%rdx), %rcx
	movq	16(%r15), %r10
	movl	(%r10), %ebx
	shlq	$3, %rbx
	movq	%r14, (%rcx,%rbx,1)
	movq	16(%r15), %r12
	movl	(%r12), %r13d
	incl	%r13d
	movq	$10, -8(%rsi)
	movl	%r13d, (%rsi)
	movq	%rsi, %r14
	addq	$16, %rsi
	movq	8(%r15), %rcx
	movq	(%rcx), %rdi
	movq	%r14, %r8
	movq	%r13, %r9
	movq	24(%r15), %r10
	movq	32(%r15), %r12
	jmp	lp.493
doGC497:
	movq	$20, -8(%rsi)
	movq	%r15, (%rsi)
	movq	%r14, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	movq	%rdx, %rdi
	movabsq	$retGC495, %r8
	jmp	ASM_InvokeGC
	.text
lp.493:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest49D
	/* live= GP={%r12 %r10 %rcx %rdx %rbx} spilled=  */
retGC49C:
	movq	32(%rdi), %r12
	movq	24(%rdi), %r10
	movl	16(%rdi), %ecx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest49D:
	movq	%r11, %r13
	subq	%rsi, %r13
	jle	doGC49E
check.498:
	/* block check<10CB9> (ep<F9C6>,i<F9C7>,_t<F9C8>,retK<F9C9>,_exh<F9CA>) */
	cmpl	16(%rbx), %ecx
	jl	L49F
L_true499:
then.49B:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block then<F9D1> (ep<F9CF>,retK<F9D0>) */
	movq	%r10, %rdi
	movq	24(%rbx), %r15
	movq	(%r15), %r8
	movq	24(%rbx), %rcx
	movl	8(%rcx), %r9d
	jmp	letJoinK.48E
doGC49E:
	movq	$3467, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movl	%ecx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%r12, 32(%rsi)
	movq	%rsi, %rdx
	addq	$48, %rsi
	movq	%rdx, %rdi
	movabsq	$retGC49C, %r8
	jmp	ASM_InvokeGC
L49F:
else.49A:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block else<F9DC> (ep<F9D7>,i<F9DB>,retK<F9DA>,_exh<F9D9>,_t<F9D8>) */
	movq	$133, -8(%rsi)
	movq	%rbx, (%rsi)
	movabsq	$lp.493, %r14
	movq	%r14, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	$7949, -8(%rsi)
	movabsq	$letJoinK.494, %r14
	movq	%r14, (%rsi)
	movq	%r13, 8(%rsi)
	movq	%rdx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%r12, 32(%rsi)
	movq	24(%rbx), %r15
	movq	%r15, 40(%rsi)
	movq	%rsi, %r15
	addq	$56, %rsi
	movq	(%rbx), %rdx
	movq	8(%rdx), %r13
	movq	(%rdx), %rdi
	movq	8(%rbx), %r14
	shlq	$3, %rcx
	movq	(%r14,%rcx,1), %r8
	movq	%r15, %r9
	movq	%r12, %r10
	jmp	*%r13
	.text
letJoinK.4A4:
	movq	%r8, %r15
	movq	%rdi, %r13
	jmp	gcTest4A6
	/* live= GP={%r15 %r13} spilled=  */
retGC4A5:
	movq	8(%rdi), %r15
	movq	(%rdi), %r13
gcTest4A6:
	movq	%r11, %rbx
	subq	%rsi, %rbx
	jle	doGC4A7
check.4A0:
	/* block check<10CBC> (ep<F9A7>,init<F9A5>) */
	cmpl	$0, 32(%r13)
	jge	L4A8
L_true4A1:
then.4A3:
	/* Liveout:  GP={%rax %rdi}  */
	/* block then<F9AD> (ep<F9AC>) */
	movq	$133, -8(%rsi)
	movabsq	$str73, %r10
	movq	%r10, (%rsi)
	movl	$29, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movabsq	$tag74, %r14
	movq	%r14, (%rsi)
	movq	%rbx, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	16(%r13), %r15
	movq	(%r15), %rcx
	movq	%r12, %rax
	movq	%r15, %rdi
	jmp	*%rcx
doGC4A7:
	movq	$20, -8(%rsi)
	movq	%r13, (%rsi)
	movq	%r15, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	movq	%rdx, %rdi
	movabsq	$retGC4A5, %r8
	jmp	ASM_InvokeGC
L4A8:
	movq	%r15, -56(%rbp)
	movq	%r13, -64(%rbp)
else.4A2:
	/* Liveout:  GP={%r12 %r10 %r9 %r8 %rdi}  */
	/* block else<F9B7> (ep<F9B5>,init<F9B6>) */
	movq	$-1048576, %r14
	andq	%rsi, %r14
	movq	%rsi, 120(%r14)
	movq	$1, (%r14)
	movq	%rax, -72(%rbp)
	movq	%rcx, %r15
	movq	%rdx, %rbx
	movq	%rdi, -80(%rbp)
	movq	%r8, -96(%rbp)
	movq	%r9, %r12
	movq	%r10, -88(%rbp)
	movq	%r11, %r13
	movq	$-1048576, %rcx
	andq	%rsi, %rcx
	movq	%rcx, %rdi
	movq	-56(%rbp), %rsi
	call	PromoteObj
	movq	%rax, -56(%rbp)
	movq	-72(%rbp), %rax
	movq	%r15, %rcx
	movq	%rbx, %rdx
	movq	-80(%rbp), %rdi
	movq	-96(%rbp), %r8
	movq	%r12, %r9
	movq	-88(%rbp), %r10
	movq	%r13, %r11
	movq	120(%r14), %rsi
	movq	$3, (%r14)
	movq	$-1048576, %rbx
	andq	%rsi, %rbx
	movq	$1, (%rbx)
	movq	%rax, -72(%rbp)
	movq	%rcx, -88(%rbp)
	movq	%rdx, -96(%rbp)
	movq	%rsi, -104(%rbp)
	movq	%rdi, -112(%rbp)
	movq	%r8, %r12
	movq	%r9, %r13
	movq	%r10, -80(%rbp)
	movq	%r11, %r14
	movq	$-1048576, %r10
	andq	%rsi, %r10
	movq	%r10, %rdi
	movq	-64(%rbp), %r10
	movslq	32(%r10), %rcx
	movq	%rcx, %rsi
	movq	-56(%rbp), %rdx
	call	M_NewArray
	movq	%rax, %r15
	movq	-72(%rbp), %rax
	movq	-88(%rbp), %rcx
	movq	-96(%rbp), %rdx
	movq	-104(%rbp), %rsi
	movq	-112(%rbp), %rdi
	movq	%r12, %r8
	movq	%r13, %r9
	movq	-80(%rbp), %r10
	movq	%r14, %r11
	movq	$3, (%rbx)
	movq	$133, -8(%rsi)
	movq	%r15, (%rsi)
	movq	-64(%rbp), %r12
	movl	32(%r12), %edx
	movl	%edx, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	$1417, -8(%rsi)
	movq	-64(%rbp), %r13
	movq	8(%r13), %r10
	movq	%r10, (%rsi)
	movq	-64(%rbp), %r14
	movq	24(%r14), %r12
	movq	%r12, 8(%rsi)
	movq	-64(%rbp), %r15
	movl	32(%r15), %r13d
	movl	%r13d, 16(%rsi)
	movq	%rcx, 24(%rsi)
	movq	%rsi, %rbx
	addq	$40, %rsi
	movq	$133, -8(%rsi)
	movq	%rbx, (%rsi)
	movabsq	$lp.493, %r15
	movq	%r15, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movl	$1, %ecx
	movq	$10, -8(%rsi)
	movl	%ecx, (%rsi)
	movq	%rsi, %rdx
	addq	$16, %rsi
	movq	(%r14), %rdi
	movq	%rdx, %r8
	movq	%rcx, %r9
	movq	-64(%rbp), %rcx
	movq	40(%rcx), %r10
	movq	16(%rcx), %r12
	jmp	lp.493
	.text
m.3DB:
	movq	%r9, %r14
	movq	%r8, %r13
	movq	%rdi, %rcx
	jmp	gcTest4B8
	/* live= GP={%r10 %r14 %r13 %rcx} spilled=  */
retGC4B7:
	movq	24(%rdi), %r10
	movq	16(%rdi), %r14
	movq	8(%rdi), %r13
	movq	(%rdi), %rcx
gcTest4B8:
	movq	%r11, %r15
	subq	%rsi, %r15
	jle	doGC4B9
check.4A9:
	/* block check<10CC1> (ep<F61E>,r<F61F>,retK<F620>,_exh<F621>) */
	movq	(%r13), %r15
	cmpq	$1, %r15
	je	S_case4AA
	cmpq	$3, %r15
	je	S_case4AC
S_case4AA:
	movq	%rcx, -104(%rbp)
case.4AB:
	/* block case<F62B> (ep<F627>,r<F62A>,retK<F629>,_exh<F628>) */
	movl	8(%r13), %ecx
	movq	%rcx, -120(%rbp)
	movl	16(%r13), %ebx
	movq	32(%r13), %rdx
	movq	%rdx, -112(%rbp)
	movq	$10, -8(%rsi)
	movq	-120(%rbp), %r12
	movl	%r12d, (%rsi)
	movq	%rsi, -56(%rbp)
	addq	$16, %rsi
	movq	$10, -8(%rsi)
	movl	%ebx, (%rsi)
	movq	%rsi, -64(%rbp)
	addq	$16, %rsi
	movq	$36, -8(%rsi)
	movq	$1, (%rsi)
	movq	$1, 8(%rsi)
	movq	$1, 16(%rsi)
	movq	$1, 24(%rsi)
	movq	%rsi, %rdx
	addq	$40, %rsi
	movq	$10, -8(%rsi)
	movl	$1, (%rsi)
	movq	%rsi, %r15
	addq	$16, %rsi
	movq	$261, -8(%rsi)
	movabsq	$letJoinK.3CD, %r12
	movq	%r12, (%rsi)
	movq	%r14, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	$133, -8(%rsi)
	movq	-104(%rbp), %rcx
	movq	%rcx, (%rsi)
	movabsq	$m.3DB, %r12
	movq	%r12, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	$60, -8(%rsi)
	movabsq	$slowClone_3.3DC, %r12
	movq	%r12, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%r10, 16(%rsi)
	movq	-112(%rbp), %r12
	movq	%r12, 24(%rsi)
	movq	%rdx, 32(%rsi)
	movq	%r15, 40(%rsi)
	movq	%r14, 48(%rsi)
	movq	%rsi, %rcx
	addq	$64, %rsi
	movq	$-1048576, %r12
	andq	%rsi, %r12
	movq	32(%r12), %r12
	movq	8(%r12), %r12
	cmpq	$1, %r12
	jne	L_true4B1
else.4B2:
	/* Liveout:  GP={%rax %rdi}  */
	/* block else<F97C> (_exh<F97B>) */
	movq	$133, -8(%rsi)
	movabsq	$str87, %r14
	movq	%r14, (%rsi)
	movl	$51, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movabsq	$tag74, %rcx
	movq	%rcx, (%rsi)
	movq	%r13, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	(%r10), %rdx
	movq	%r15, %rax
	movq	%r10, %rdi
	jmp	*%rdx
L_true4B1:
	movq	%rbx, -80(%rbp)
	movq	%r14, -72(%rbp)
	movq	%r15, -88(%rbp)
	movq	%rdx, -96(%rbp)
	movq	%r10, -128(%rbp)
	movq	-104(%rbp), %rdx
	movq	$1, %r14
	movq	24(%r13), %r13
then.4B3:
	/* block then<F692> (ep<F684>,_exh<F691>,_t<F690>,_t<F68F>,_anon_<F68E>,_anon_<F68D>,_anon_<F68C>,_anon_<F68B>,r0<F68A>,c0<F689>,letJoinK<F688>,slowClone_3<F687>,con_NONE<F686>,_t<F685>) */
	movq	(%r12), %rbx
	movq	$20, -8(%rsi)
	movq	(%rbx), %r10
	movq	%r10, (%rsi)
	movq	%r14, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	8(%rbx), %r15
	movq	$133, -8(%rsi)
	movq	%rdx, (%rsi)
	movabsq	$m.3DB, %rbx
	movq	%rbx, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	$2090781, -8(%rsi)
	movabsq	$letJoinK.483, %r12
	movq	%r12, (%rsi)
	movq	(%rdx), %rbx
	movq	%rbx, 8(%rsi)
	movq	%r14, 16(%rsi)
	movq	-128(%rbp), %r14
	movq	%r14, 24(%rsi)
	movq	-120(%rbp), %rbx
	movl	%ebx, 32(%rsi)
	movq	-80(%rbp), %r12
	movl	%r12d, 40(%rsi)
	movq	%r13, 48(%rsi)
	movq	-112(%rbp), %r13
	movq	%r13, 56(%rsi)
	movq	-56(%rbp), %r14
	movq	%r14, 64(%rsi)
	movq	-64(%rbp), %rbx
	movq	%rbx, 72(%rsi)
	movq	-96(%rbp), %r12
	movq	%r12, 80(%rsi)
	movq	-88(%rbp), %r13
	movq	%r13, 88(%rsi)
	movq	-72(%rbp), %r14
	movq	%r14, 96(%rsi)
	movq	%r10, 104(%rsi)
	movq	%rsi, %rbx
	addq	$120, %rsi
	cmpq	$1, %r15
	je	L4BA
L_true4B4:
	movq	-128(%rbp), %r12
then.4B6:
	/* Liveout:  GP={%r12 %r10 %r9 %r8 %rdi}  */
	/* block then<F969> (ep<F964>,_exh<F968>,slowClone_3<F967>,c<F966>,letJoinK<F965>) */
	movq	$261, -8(%rsi)
	movabsq	$letJoinK.489, %r10
	movq	%r10, (%rsi)
	movq	%rbx, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	(%rdx), %r13
	movq	(%r13), %rdi
	movq	(%r15), %r8
	movq	%rcx, %r9
	jmp	wrap.F3
L4BA:
else.4B5:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block else<F978> (slowClone_3<F977>,letJoinK<F976>) */
	movq	%rbx, %rdi
	movq	%rcx, %r8
	jmp	letJoinK.483
doGC4B9:
	movq	$36, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%r13, 8(%rsi)
	movq	%r14, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %r14
	addq	$40, %rsi
	movq	%r14, %rdi
	movabsq	$retGC4B7, %r8
	jmp	ASM_InvokeGC
S_case4AC:
case.4AD:
	/* block case<F987> (ep<F983>,r<F986>,retK<F985>,_exh<F984>) */
	movl	24(%r13), %r15d
	movq	$263, -8(%rsi)
	movabsq	$letJoinK.48E, %rbx
	movq	%rbx, (%rsi)
	movq	%r14, 8(%rsi)
	movl	8(%r13), %r12d
	movl	%r12d, 16(%rsi)
	movq	%rsi, %rdx
	addq	$32, %rsi
	cmpl	$0, %r15d
	je	L_true4AE
	movq	16(%r13), %r14
else.4AF:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block else<F9A2> (ep<F99D>,_exh<F9A1>,_t<F9A0>,_t<F99F>,letJoinK<F99E>) */
	movq	$5901, -8(%rsi)
	movabsq	$letJoinK.4A4, %rbx
	movq	%rbx, (%rsi)
	movq	24(%rcx), %r12
	movq	%r12, 8(%rsi)
	movq	%r10, 16(%rsi)
	movq	%r14, 24(%rsi)
	movl	%r15d, 32(%rsi)
	movq	%rdx, 40(%rsi)
	movq	%rsi, %r13
	addq	$56, %rsi
	movq	24(%rcx), %r15
	movq	8(%r15), %rcx
	movq	(%r15), %rdi
	movq	$8, %rdx
	imulq	$0, %rdx
	movq	(%r14,%rdx,1), %r8
	movq	%r13, %r9
	jmp	*%rcx
L_true4AE:
then.4B0:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block then<F999> (ep<F997>,letJoinK<F998>) */
	movq	%rdx, %rdi
	movq	8(%rcx), %r8
	movl	16(%rcx), %r9d
	jmp	letJoinK.48E
	.text
mapP.4BC:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest4BE
	/* live= GP={%r12 %r10 %rcx %rdx %rbx} spilled=  */
retGC4BD:
	movq	32(%rdi), %r12
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest4BE:
	movq	%r11, %r13
	subq	%rsi, %r13
	jle	doGC4BF
check.4BB:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block check<10CC7> (ep<F611>,f<F612>,rope<F613>,retK<F614>,_exh<F615>) */
	movq	$1417, -8(%rsi)
	movq	(%rbx), %r14
	movq	%r14, (%rsi)
	movq	8(%rbx), %r15
	movq	%r15, 8(%rsi)
	movl	16(%rbx), %ebx
	movl	%ebx, 16(%rsi)
	movq	%rdx, 24(%rsi)
	movq	%rsi, %r13
	addq	$40, %rsi
	movq	$133, -8(%rsi)
	movq	%r13, (%rsi)
	movabsq	$m.3DB, %r14
	movq	%r14, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	(%r13), %rdi
	movq	%rcx, %r8
	movq	%r10, %r9
	movq	%r12, %r10
	jmp	m.3DB
doGC4BF:
	movq	$44, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%r12, 32(%rsi)
	movq	%rsi, %r15
	addq	$48, %rsi
	movq	%r15, %rdi
	movabsq	$retGC4BD, %r8
	jmp	ASM_InvokeGC
	.text
vecnorm.4C1:
	movq	%r8, %rcx
	movsd	 %xmm4, %xmm5
	movsd	 %xmm3, %xmm4
	movsd	 %xmm2, %xmm3
	movq	%rdi, %rdx
	movsd	 %xmm5, %xmm15
	movsd	 %xmm4, %xmm14
	movsd	 %xmm3, %xmm13
	jmp	gcTest4C3
	/* live= GP={%rcx %rdx} FP={%xmm2 %xmm1 %xmm0} spilled=  */
retGC4C2:
	movq	32(%rdi), %rcx
	movq	(%rdi), %rdx
	/* %f7863.d := mem.d[%rdi.64 +.64 24] */
	movsd	 24(%rdi), %xmm15
	/* %f7862.d := mem.d[%rdi.64 +.64 16] */
	movsd	 16(%rdi), %xmm14
	/* %f7861.d := mem.d[%rdi.64 +.64 8] */
	movsd	 8(%rdi), %xmm13
gcTest4C3:
	movq	%r11, %rbx
	subq	%rsi, %rbx
	jle	doGC4C4
	movsd	 %xmm15, %xmm2
	movsd	 %xmm14, %xmm1
	movsd	 %xmm13, %xmm0
check.4C0:
	/* Liveout:  GP={%r9 %r8 %rdi} FP={%xmm4 %xmm3 %xmm2}  */
	/* block check<10CD4> (ep<FA71>,_t<FA72>,_t<FA73>,_t<FA74>,retK<FA75>) */
	movsd	 %xmm2, %xmm8
	mulsd	 %xmm2, %xmm8
	movsd	 %xmm1, %xmm9
	mulsd	 %xmm1, %xmm9
	movsd	 %xmm0, %xmm7
	mulsd	 %xmm0, %xmm7
	addsd	 %xmm9, %xmm7
	addsd	 %xmm8, %xmm7
	sqrtsd	 %xmm7, %xmm6
	movq	$10, -8(%rsi)
	movsd	 %xmm6, (%rsi)
	movq	%rsi, %rbx
	addq	$16, %rsi
	movsd	 %xmm0, %xmm10
	divsd	 %xmm6, %xmm10
	movq	$10, -8(%rsi)
	movsd	 %xmm10, (%rsi)
	movq	%rsi, %r10
	addq	$16, %rsi
	movsd	 %xmm1, %xmm11
	divsd	 %xmm6, %xmm11
	movq	$10, -8(%rsi)
	movsd	 %xmm11, (%rsi)
	movq	%rsi, %r12
	addq	$16, %rsi
	movsd	 %xmm2, %xmm12
	divsd	 %xmm6, %xmm12
	movq	$10, -8(%rsi)
	movsd	 %xmm12, (%rsi)
	movq	%rsi, %r13
	addq	$16, %rsi
	movq	$28, -8(%rsi)
	movq	%r10, (%rsi)
	movq	%r12, 8(%rsi)
	movq	%r13, 16(%rsi)
	movq	%rsi, %r14
	addq	$32, %rsi
	movq	(%rcx), %r15
	movq	%rcx, %rdi
	movq	%r14, %r8
	movsd	 %xmm10, %xmm2
	movsd	 %xmm11, %xmm3
	movsd	 %xmm12, %xmm4
	movq	%rbx, %r9
	jmp	*%r15
doGC4C4:
	movq	$2059, -8(%rsi)
	movq	%rdx, (%rsi)
	movsd	 %xmm13, 8(%rsi)
	movsd	 %xmm14, 16(%rsi)
	movsd	 %xmm15, 24(%rsi)
	movq	%rcx, 32(%rsi)
	movq	%rsi, %rcx
	addq	$48, %rsi
	movq	%rcx, %rdi
	movabsq	$retGC4C2, %r8
	jmp	ASM_InvokeGC
	.text
cross.4C6:
	movq	%r8, %rcx
	movsd	 %xmm4, %xmm5
	movsd	 %xmm3, %xmm4
	movsd	 %xmm2, %xmm3
	movq	%rdi, %rdx
	movsd	 %xmm5, %xmm14
	movsd	 %xmm4, %xmm13
	movsd	 %xmm3, %xmm12
	jmp	gcTest4C8
	/* live= GP={%rcx %rdx} FP={%xmm2 %xmm1 %xmm0} spilled=  */
retGC4C7:
	movq	32(%rdi), %rcx
	movq	(%rdi), %rdx
	/* %f7918.d := mem.d[%rdi.64 +.64 24] */
	movsd	 24(%rdi), %xmm14
	/* %f7917.d := mem.d[%rdi.64 +.64 16] */
	movsd	 16(%rdi), %xmm13
	/* %f7916.d := mem.d[%rdi.64 +.64 8] */
	movsd	 8(%rdi), %xmm12
gcTest4C8:
	movq	%r11, %rbx
	subq	%rsi, %rbx
	jle	doGC4C9
	movsd	 %xmm14, %xmm2
	movsd	 %xmm13, %xmm1
	movsd	 %xmm12, %xmm0
check.4C5:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<10CDA> (ep<FA94>,_t<FA95>,_t<FA96>,_t<FA97>,retK<FA98>) */
	movq	$10, -8(%rsi)
	movq	16(%rdx), %rbx
	movsd	 %xmm1, %xmm7
	mulsd	 (%rbx), %xmm7
	movq	8(%rdx), %r10
	movsd	 %xmm2, %xmm6
	mulsd	 (%r10), %xmm6
	subsd	 %xmm7, %xmm6
	movsd	 %xmm6, (%rsi)
	movq	%rsi, %r10
	addq	$16, %rsi
	movq	$10, -8(%rsi)
	movq	(%rdx), %r13
	movsd	 %xmm2, %xmm9
	mulsd	 (%r13), %xmm9
	movq	16(%rdx), %r14
	movsd	 %xmm0, %xmm8
	mulsd	 (%r14), %xmm8
	subsd	 %xmm9, %xmm8
	movsd	 %xmm8, (%rsi)
	movq	%rsi, %r12
	addq	$16, %rsi
	movq	$10, -8(%rsi)
	movq	8(%rdx), %rbx
	movsd	 %xmm0, %xmm11
	mulsd	 (%rbx), %xmm11
	movq	(%rdx), %r13
	movsd	 %xmm1, %xmm10
	mulsd	 (%r13), %xmm10
	subsd	 %xmm11, %xmm10
	movsd	 %xmm10, (%rsi)
	movq	%rsi, %r15
	addq	$16, %rsi
	movq	$28, -8(%rsi)
	movq	%r10, (%rsi)
	movq	%r12, 8(%rsi)
	movq	%r15, 16(%rsi)
	movq	%rsi, %r14
	addq	$32, %rsi
	movq	(%rcx), %r15
	movq	%rcx, %rdi
	movq	%r14, %r8
	jmp	*%r15
doGC4C9:
	movq	$2187, -8(%rsi)
	movq	%rdx, (%rsi)
	movsd	 %xmm12, 8(%rsi)
	movsd	 %xmm13, 16(%rsi)
	movsd	 %xmm14, 24(%rsi)
	movq	%rcx, 32(%rsi)
	movq	%rsi, %rcx
	addq	$48, %rsi
	movq	%rcx, %rdi
	movabsq	$retGC4C7, %r8
	jmp	ASM_InvokeGC
	.text
veccross.4CB:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest4CD
	/* live= GP={%r12 %r10 %rcx %rdx %rbx} spilled=  */
retGC4CC:
	movq	32(%rdi), %r12
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest4CD:
	movq	%r11, %r13
	subq	%rsi, %r13
	jle	doGC4CE
check.4CA:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<10CE0> (ep<FA8A>,x1<FA8B>,y1<FA8C>,z1<FA8D>,retK<FA8E>) */
	movq	$28, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%r10, 16(%rsi)
	movq	%rsi, %r10
	addq	$32, %rsi
	movq	$133, -8(%rsi)
	movq	%r10, (%rsi)
	movabsq	$cross.4C6, %r14
	movq	%r14, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	(%r12), %r15
	movq	%r12, %rdi
	movq	%r13, %r8
	jmp	*%r15
doGC4CE:
	movq	$44, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%r12, 32(%rsi)
	movq	%rsi, %rcx
	addq	$48, %rsi
	movq	%rcx, %rdi
	movabsq	$retGC4CC, %r8
	jmp	ASM_InvokeGC
	.text
ambientsurf.4DC:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest4DE
	/* live= GP={%r10 %rcx %rdx %rbx} spilled=  */
retGC4DD:
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest4DE:
	movq	%r11, %r12
	subq	%rsi, %r12
	jg	L4E0
doGC4DF:
	movq	$36, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %rbx
	addq	$40, %rsi
	movq	%rbx, %rdi
	movabsq	$retGC4DD, %r8
	jmp	ASM_InvokeGC
L4E0:
check.4CF:
	/* block check<10CE9> (ep<FAB9>,surf<FABA>,retK<FABB>,_exh<FABC>) */
	cmpq	$1, %rdx
	jne	L_true4D0
else.4D1:
	/* Liveout:  GP={%rdi} FP={%xmm4 %xmm3 %xmm2}  */
	/* block else<FADC> (retK<FADB>) */
	movq	%rcx, %rdi
	movabsq	$flt4D3, %r13
	/* %xmm2.d := mem.d[flt4D3] */
	movsd	 (%r13), %xmm2
	movabsq	$flt4D3, %r14
	/* %xmm3.d := mem.d[flt4D3] */
	movsd	 (%r14), %xmm3
	movabsq	$flt4D3, %r15
	/* %xmm4.d := mem.d[flt4D3] */
	movsd	 (%r15), %xmm4
	jmp	letJoinK.4D4
L_true4D0:
then.4D2:
	/* block then<FAC4> (ep<FAC1>,surf<FAC3>,retK<FAC2>) */
	movq	(%rdx), %r12
	cmpq	$1, (%r12)
	jne	L4E1
L_true4D5:
then.4D7:
	/* Liveout:  GP={%rdi} FP={%xmm4 %xmm3 %xmm2}  */
	/* block then<FACE> (retK<FACD>,_anon_<FACC>) */
	movq	%rcx, %rdi
	/* %xmm2.d := mem.d[%r7970.64 +.64 8] */
	movsd	 8(%r12), %xmm2
	/* %xmm3.d := mem.d[%r7970.64 +.64 16] */
	movsd	 16(%r12), %xmm3
	/* %xmm4.d := mem.d[%r7970.64 +.64 24] */
	movsd	 24(%r12), %xmm4
	jmp	letJoinK.4D4
L4E1:
	movq	8(%rdx), %r10
else.4D6:
	/* block else<FAD6> (ep<10CE1>,retK<10CE2>,_anon_<10CE3>) */
gcTest4DA:
	movq	%r11, %rdx
	subq	%rsi, %rdx
	jle	doGC4DB
check.4D8:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block check<10CE4> (ep<FAD3>,retK<FAD5>,_anon_<FAD4>) */
	movq	%rbx, %rdi
	movq	%r10, %r8
	movq	%rcx, %r9
	movq	$1, %r10
	jmp	ambientsurf.4DC
doGC4DB:
	movq	$28, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%r10, 16(%rsi)
	movq	%rsi, %rcx
	addq	$32, %rsi
	movq	%rcx, %rdi
	movabsq	$retGC4D9, %r8
	jmp	ASM_InvokeGC
	/* live= GP={%r10 %rcx %rbx} spilled=  */
retGC4D9:
	movq	16(%rdi), %r10
	movq	8(%rdi), %rcx
	movq	(%rdi), %rbx
	jmp	gcTest4DA
	.text
diffusesurf.4EE:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest4F0
	/* live= GP={%r10 %rcx %rdx %rbx} spilled=  */
retGC4EF:
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest4F0:
	movq	%r11, %r13
	subq	%rsi, %r13
	jg	L4F2
doGC4F1:
	movq	$36, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %r12
	addq	$40, %rsi
	movq	%r12, %rdi
	movabsq	$retGC4EF, %r8
	jmp	ASM_InvokeGC
L4F2:
check.4E2:
	/* block check<10CF2> (ep<FAE3>,surf<FAE4>,retK<FAE5>,_exh<FAE6>) */
	cmpq	$1, %rdx
	jne	L_true4E3
else.4E4:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block else<FB0A> (retK<FB09>) */
	movq	$10, -8(%rsi)
	movabsq	$flt4D3, %r14
	movsd	 (%r14), %xmm0
	movsd	 %xmm0, (%rsi)
	movq	%rsi, %r13
	addq	$16, %rsi
	movq	$10, -8(%rsi)
	movabsq	$flt4D3, %rdx
	movsd	 (%rdx), %xmm1
	movsd	 %xmm1, (%rsi)
	movq	%rsi, %r15
	addq	$16, %rsi
	movq	$10, -8(%rsi)
	movabsq	$flt4D3, %r10
	movsd	 (%r10), %xmm2
	movsd	 %xmm2, (%rsi)
	movq	%rsi, %rbx
	addq	$16, %rsi
	movq	$28, -8(%rsi)
	movq	%r13, (%rsi)
	movq	%r15, 8(%rsi)
	movq	%rbx, 16(%rsi)
	movq	%rsi, %r12
	addq	$32, %rsi
	movq	%rcx, %rdi
	movq	%r12, %r8
	jmp	letJoinK.4E6
L_true4E3:
then.4E5:
	/* block then<FAEE> (ep<FAEB>,surf<FAED>,retK<FAEC>) */
	movq	(%rdx), %r12
	cmpq	$3, (%r12)
	jne	L4F3
L_true4E7:
then.4E9:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block then<FAF8> (retK<FAF7>,_anon_<FAF6>) */
	movq	$10, -8(%rsi)
	movsd	 8(%r12), %xmm3
	movsd	 %xmm3, (%rsi)
	movq	%rsi, %r15
	addq	$16, %rsi
	movq	$10, -8(%rsi)
	movsd	 16(%r12), %xmm4
	movsd	 %xmm4, (%rsi)
	movq	%rsi, %rdx
	addq	$16, %rsi
	movq	$10, -8(%rsi)
	movsd	 24(%r12), %xmm5
	movsd	 %xmm5, (%rsi)
	movq	%rsi, %rbx
	addq	$16, %rsi
	movq	$28, -8(%rsi)
	movq	%r15, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rbx, 16(%rsi)
	movq	%rsi, %r10
	addq	$32, %rsi
	movq	%rcx, %rdi
	movq	%r10, %r8
	jmp	letJoinK.4E6
L4F3:
	movq	8(%rdx), %r10
else.4E8:
	/* block else<FB04> (ep<10CEA>,retK<10CEB>,_anon_<10CEC>) */
gcTest4EC:
	movq	%r11, %r14
	subq	%rsi, %r14
	jle	doGC4ED
check.4EA:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block check<10CED> (ep<FB01>,retK<FB03>,_anon_<FB02>) */
	movq	%rbx, %rdi
	movq	%r10, %r8
	movq	%rcx, %r9
	movq	$1, %r10
	jmp	diffusesurf.4EE
doGC4ED:
	movq	$28, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%r10, 16(%rsi)
	movq	%rsi, %r13
	addq	$32, %rsi
	movq	%r13, %rdi
	movabsq	$retGC4EB, %r8
	jmp	ASM_InvokeGC
	/* live= GP={%r10 %rcx %rbx} spilled=  */
retGC4EB:
	movq	16(%rdi), %r10
	movq	8(%rdi), %rcx
	movq	(%rdi), %rbx
	jmp	gcTest4EC
	.text
retGC4FC:
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
	jmp	gcTest4FD
L4FF:
	movq	8(%rdx), %r12
else.4F9:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block else<FB35> (ep<FB32>,retK<FB34>,_anon_<FB33>) */
	movq	%rbx, %rdi
	movq	%r12, %r8
	movq	%rcx, %r9
	movq	$1, %r10
specularsurf.4FB:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
gcTest4FD:
	movq	%r11, %r12
	subq	%rsi, %r12
	jle	doGC4FE
check.4F4:
	/* block check<10CF7> (ep<FB15>,surf<FB16>,retK<FB17>,_exh<FB18>) */
	cmpq	$1, %rdx
	jne	L_true4F5
else.4F6:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block else<FB3B> (retK<FB3A>) */
	movq	$10, -8(%rsi)
	movabsq	$flt4D3, %r14
	movsd	 (%r14), %xmm0
	movsd	 %xmm0, (%rsi)
	movq	%rsi, %r13
	addq	$16, %rsi
	movq	$10, -8(%rsi)
	movabsq	$flt4D3, %rdx
	movsd	 (%rdx), %xmm1
	movsd	 %xmm1, (%rsi)
	movq	%rsi, %r15
	addq	$16, %rsi
	movq	$10, -8(%rsi)
	movabsq	$flt4D3, %rbx
	movsd	 (%rbx), %xmm2
	movsd	 %xmm2, (%rsi)
	movq	%rsi, %r10
	addq	$16, %rsi
	movq	(%rcx), %r12
	movq	%rcx, %rdi
	movq	%r13, %r8
	movq	%r15, %r9
	jmp	*%r12
doGC4FE:
	movq	$36, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %rcx
	addq	$40, %rsi
	movq	%rcx, %rdi
	movabsq	$retGC4FC, %r8
	jmp	ASM_InvokeGC
L_true4F5:
then.4F7:
	/* block then<FB20> (ep<FB1D>,surf<FB1F>,retK<FB1E>) */
	movq	(%rdx), %r10
	cmpq	$5, (%r10)
	jne	L4FF
L_true4F8:
then.4FA:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block then<FB2A> (retK<FB29>,_anon_<FB28>) */
	movq	$10, -8(%rsi)
	movsd	 8(%r10), %xmm3
	movsd	 %xmm3, (%rsi)
	movq	%rsi, %r13
	addq	$16, %rsi
	movq	$10, -8(%rsi)
	movsd	 16(%r10), %xmm4
	movsd	 %xmm4, (%rsi)
	movq	%rsi, %r14
	addq	$16, %rsi
	movq	$10, -8(%rsi)
	movsd	 24(%r10), %xmm5
	movsd	 %xmm5, (%rsi)
	movq	%rsi, %r10
	addq	$16, %rsi
	movq	(%rcx), %r15
	movq	%rcx, %rdi
	movq	%r13, %r8
	movq	%r14, %r9
	jmp	*%r15
	/* live= GP={%r10 %rcx %rdx %rbx} spilled=  */
	jmp	retGC4FC
	.text
retGC50A:
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
	jmp	gcTest50B
L50D:
	movq	8(%rdx), %r14
else.507:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block else<FB61> (ep<FB5E>,retK<FB60>,_anon_<FB5F>) */
	movq	%rbx, %rdi
	movq	%r14, %r8
	movq	%rcx, %r9
	movq	$1, %r10
specpowsurf.509:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
gcTest50B:
	movq	%r11, %r13
	subq	%rsi, %r13
	jle	doGC50C
check.500:
	/* block check<10CFC> (ep<FB45>,surf<FB46>,retK<FB47>,_exh<FB48>) */
	cmpq	$1, %rdx
	jne	L_true501
else.502:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block else<FB67> (retK<FB66>) */
	movq	$10, -8(%rsi)
	movabsq	$flt504, %rdx
	movsd	 (%rdx), %xmm0
	movsd	 %xmm0, (%rsi)
	movq	%rsi, %r15
	addq	$16, %rsi
	movq	%rcx, %rdi
	movq	%r15, %r8
	jmp	letJoinK.505
doGC50C:
	movq	$36, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %r12
	addq	$40, %rsi
	movq	%r12, %rdi
	movabsq	$retGC50A, %r8
	jmp	ASM_InvokeGC
L_true501:
then.503:
	/* block then<FB50> (ep<FB4D>,surf<FB4F>,retK<FB4E>) */
	movq	(%rdx), %r10
	cmpq	$7, (%r10)
	jne	L50D
L_true506:
then.508:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block then<FB5A> (retK<FB59>,_anon_<FB58>) */
	movq	$10, -8(%rsi)
	movsd	 8(%r10), %xmm1
	movsd	 %xmm1, (%rsi)
	movq	%rsi, %rbx
	addq	$16, %rsi
	movq	%rcx, %rdi
	movq	%rbx, %r8
	jmp	letJoinK.505
	/* live= GP={%r10 %rcx %rdx %rbx} spilled=  */
	jmp	retGC50A
	.text
reflectsurf.51A:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest51C
	/* live= GP={%r10 %rcx %rdx %rbx} spilled=  */
retGC51B:
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest51C:
	movq	%r11, %r12
	subq	%rsi, %r12
	jg	L51E
doGC51D:
	movq	$36, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %rcx
	addq	$40, %rsi
	movq	%rcx, %rdi
	movabsq	$retGC51B, %r8
	jmp	ASM_InvokeGC
L51E:
check.50E:
	/* block check<10D05> (ep<FB6D>,surf<FB6E>,retK<FB6F>,_exh<FB70>) */
	cmpq	$1, %rdx
	jne	L_true50F
else.510:
	/* Liveout:  GP={%rdi} FP={%xmm2}  */
	/* block else<FB8E> (retK<FB8D>) */
	movq	%rcx, %rdi
	movabsq	$flt4D3, %r13
	/* %xmm2.d := mem.d[flt4D3] */
	movsd	 (%r13), %xmm2
	jmp	letJoinK.512
L_true50F:
then.511:
	/* block then<FB78> (ep<FB75>,surf<FB77>,retK<FB76>) */
	movq	(%rdx), %r12
	cmpq	$9, (%r12)
	jne	L51F
L_true513:
then.515:
	/* Liveout:  GP={%rdi} FP={%xmm2}  */
	/* block then<FB82> (retK<FB81>,_anon_<FB80>) */
	movq	%rcx, %rdi
	/* %xmm2.d := mem.d[%r8216.64 +.64 8] */
	movsd	 8(%r12), %xmm2
	jmp	letJoinK.512
L51F:
	movq	8(%rdx), %r10
else.514:
	/* block else<FB88> (ep<10CFD>,retK<10CFE>,_anon_<10CFF>) */
gcTest518:
	movq	%r11, %r15
	subq	%rsi, %r15
	jle	doGC519
check.516:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block check<10D00> (ep<FB85>,retK<FB87>,_anon_<FB86>) */
	movq	%rbx, %rdi
	movq	%r10, %r8
	movq	%rcx, %r9
	movq	$1, %r10
	jmp	reflectsurf.51A
doGC519:
	movq	$28, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%r10, 16(%rsi)
	movq	%rsi, %r14
	addq	$32, %rsi
	movq	%r14, %rdi
	movabsq	$retGC517, %r8
	jmp	ASM_InvokeGC
	/* live= GP={%r10 %rcx %rbx} spilled=  */
retGC517:
	movq	16(%rdi), %r10
	movq	8(%rdi), %rcx
	movq	(%rdi), %rbx
	jmp	gcTest518
	.text
transmitsurf.52C:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest52E
	/* live= GP={%r10 %rcx %rdx %rbx} spilled=  */
retGC52D:
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest52E:
	movq	%r11, %r12
	subq	%rsi, %r12
	jg	L530
doGC52F:
	movq	$36, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %r10
	addq	$40, %rsi
	movq	%r10, %rdi
	movabsq	$retGC52D, %r8
	jmp	ASM_InvokeGC
L530:
check.520:
	/* block check<10D0E> (ep<FB93>,surf<FB94>,retK<FB95>,_exh<FB96>) */
	cmpq	$1, %rdx
	jne	L_true521
else.522:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block else<FBB5> (retK<FBB4>) */
	movq	$10, -8(%rsi)
	movabsq	$flt4D3, %r14
	movsd	 (%r14), %xmm0
	movsd	 %xmm0, (%rsi)
	movq	%rsi, %r13
	addq	$16, %rsi
	movq	%rcx, %rdi
	movq	%r13, %r8
	jmp	letJoinK.524
L_true521:
then.523:
	/* block then<FB9E> (ep<FB9B>,surf<FB9D>,retK<FB9C>) */
	movq	(%rdx), %r12
	cmpq	$11, (%r12)
	jne	L531
L_true525:
then.527:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block then<FBA8> (retK<FBA7>,_anon_<FBA6>) */
	movq	$10, -8(%rsi)
	movsd	 8(%r12), %xmm1
	movsd	 %xmm1, (%rsi)
	movq	%rsi, %rbx
	addq	$16, %rsi
	movq	%rcx, %rdi
	movq	%rbx, %r8
	jmp	letJoinK.524
L531:
	movq	8(%rdx), %r10
else.526:
	/* block else<FBAF> (ep<10D06>,retK<10D07>,_anon_<10D08>) */
gcTest52A:
	movq	%r11, %rdx
	subq	%rsi, %rdx
	jle	doGC52B
check.528:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block check<10D09> (ep<FBAC>,retK<FBAE>,_anon_<FBAD>) */
	movq	%rbx, %rdi
	movq	%r10, %r8
	movq	%rcx, %r9
	movq	$1, %r10
	jmp	transmitsurf.52C
doGC52B:
	movq	$28, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%r10, 16(%rsi)
	movq	%rsi, %r15
	addq	$32, %rsi
	movq	%r15, %rdi
	movabsq	$retGC529, %r8
	jmp	ASM_InvokeGC
	/* live= GP={%r10 %rcx %rbx} spilled=  */
retGC529:
	movq	16(%rdi), %r10
	movq	8(%rdi), %rcx
	movq	(%rdi), %rbx
	jmp	gcTest52A
	.text
refractsurf.53F:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest541
	/* live= GP={%r10 %rcx %rdx %rbx} spilled=  */
retGC540:
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest541:
	movq	%r11, %r12
	subq	%rsi, %r12
	jg	L543
doGC542:
	movq	$36, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %r10
	addq	$40, %rsi
	movq	%r10, %rdi
	movabsq	$retGC540, %r8
	jmp	ASM_InvokeGC
L543:
check.532:
	/* block check<10D17> (ep<FBBB>,surf<FBBC>,retK<FBBD>,_exh<FBBE>) */
	cmpq	$1, %rdx
	jne	L_true533
else.534:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block else<FBDD> (retK<FBDC>) */
	movq	$10, -8(%rsi)
	movabsq	$flt536, %r14
	movsd	 (%r14), %xmm0
	movsd	 %xmm0, (%rsi)
	movq	%rsi, %r13
	addq	$16, %rsi
	movq	%rcx, %rdi
	movq	%r13, %r8
	jmp	letJoinK.537
L_true533:
then.535:
	/* block then<FBC6> (ep<FBC3>,surf<FBC5>,retK<FBC4>) */
	movq	(%rdx), %r12
	cmpq	$13, (%r12)
	jne	L544
L_true538:
then.53A:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block then<FBD0> (retK<FBCF>,_anon_<FBCE>) */
	movq	$10, -8(%rsi)
	movsd	 8(%r12), %xmm1
	movsd	 %xmm1, (%rsi)
	movq	%rsi, %rbx
	addq	$16, %rsi
	movq	%rcx, %rdi
	movq	%rbx, %r8
	jmp	letJoinK.537
L544:
	movq	8(%rdx), %r10
else.539:
	/* block else<FBD7> (ep<10D0F>,retK<10D10>,_anon_<10D11>) */
gcTest53D:
	movq	%r11, %rdx
	subq	%rsi, %rdx
	jle	doGC53E
check.53B:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block check<10D12> (ep<FBD4>,retK<FBD6>,_anon_<FBD5>) */
	movq	%rbx, %rdi
	movq	%r10, %r8
	movq	%rcx, %r9
	movq	$1, %r10
	jmp	refractsurf.53F
doGC53E:
	movq	$28, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%r10, 16(%rsi)
	movq	%rsi, %r15
	addq	$32, %rsi
	movq	%r15, %rdi
	movabsq	$retGC53C, %r8
	jmp	ASM_InvokeGC
	/* live= GP={%r10 %rcx %rbx} spilled=  */
retGC53C:
	movq	16(%rdi), %r10
	movq	8(%rdi), %rcx
	movq	(%rdi), %rbx
	jmp	gcTest53D
	.text
retGC54D:
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
	jmp	gcTest54E
L550:
	movq	8(%rdx), %r12
else.54A:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block else<FC00> (ep<FBFD>,retK<FBFF>,_anon_<FBFE>) */
	movq	%rbx, %rdi
	movq	%r12, %r8
	movq	%rcx, %r9
	movq	$1, %r10
bodysurf.54C:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
gcTest54E:
	movq	%r11, %r12
	subq	%rsi, %r12
	jle	doGC54F
check.545:
	/* block check<10D1C> (ep<FBE3>,surf<FBE4>,retK<FBE5>,_exh<FBE6>) */
	cmpq	$1, %rdx
	jne	L_true546
else.547:
	/* Liveout:  GP={%rdi} FP={%xmm4 %xmm3 %xmm2}  */
	/* block else<FC06> (retK<FC05>) */
	movq	(%rcx), %r13
	movq	%rcx, %rdi
	movabsq	$flt536, %r14
	/* %xmm2.d := mem.d[flt536] */
	movsd	 (%r14), %xmm2
	movabsq	$flt536, %r15
	/* %xmm3.d := mem.d[flt536] */
	movsd	 (%r15), %xmm3
	movabsq	$flt536, %rcx
	/* %xmm4.d := mem.d[flt536] */
	movsd	 (%rcx), %xmm4
	jmp	*%r13
doGC54F:
	movq	$36, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %rbx
	addq	$40, %rsi
	movq	%rbx, %rdi
	movabsq	$retGC54D, %r8
	jmp	ASM_InvokeGC
L_true546:
then.548:
	/* block then<FBEE> (ep<FBEB>,surf<FBED>,retK<FBEC>) */
	movq	(%rdx), %r10
	cmpq	$15, (%r10)
	jne	L550
L_true549:
then.54B:
	/* Liveout:  GP={%rdi} FP={%xmm4 %xmm3 %xmm2}  */
	/* block then<FBF8> (retK<FBF7>,_anon_<FBF6>) */
	movq	(%rcx), %rdx
	movq	%rcx, %rdi
	/* %xmm2.d := mem.d[%r8383.64 +.64 8] */
	movsd	 8(%r10), %xmm2
	/* %xmm3.d := mem.d[%r8383.64 +.64 16] */
	movsd	 16(%r10), %xmm3
	/* %xmm4.d := mem.d[%r8383.64 +.64 24] */
	movsd	 24(%r10), %xmm4
	jmp	*%rdx
	/* live= GP={%r10 %rcx %rdx %rbx} spilled=  */
	jmp	retGC54D
	.text
implicitPolyhedron.577:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
gcTest579:
	movq	%r11, %r12
	subq	%rsi, %r12
	jle	doGC57A
check.551:
	/* block check<10D21> (ep<FC0D>,param<FC0E>,retK<FC0F>,_exh<FC10>) */
	movq	(%rdx), %r13
	movq	16(%rdx), %r14
	movq	24(%rdx), %r15
	movq	16(%r15), %rdx
	movq	16(%r13), %r10
	movsd	 (%r10), %xmm3
	mulsd	 (%rdx), %xmm3
	movq	8(%r15), %r12
	movq	8(%r13), %rdx
	movsd	 (%rdx), %xmm4
	mulsd	 (%r12), %xmm4
	movq	(%r15), %r10
	movq	(%r13), %r12
	movsd	 (%r12), %xmm2
	mulsd	 (%r10), %xmm2
	addsd	 %xmm4, %xmm2
	addsd	 %xmm3, %xmm2
	movq	16(%r14), %r15
	movq	16(%r13), %rdx
	movsd	 (%rdx), %xmm6
	mulsd	 (%r15), %xmm6
	movq	8(%r14), %r10
	movq	8(%r13), %r12
	movsd	 (%r12), %xmm7
	mulsd	 (%r10), %xmm7
	movq	(%r14), %r14
	movq	(%r13), %r15
	movsd	 (%r15), %xmm5
	mulsd	 (%r14), %xmm5
	addsd	 %xmm7, %xmm5
	addsd	 %xmm6, %xmm5
	movabsq	$flt4D3, %rdx
	ucomisd	 (%rdx), %xmm5
	jbe	L57B
L_true552:
	movsd	 %xmm5, %xmm7
	movsd	 %xmm2, %xmm6
then.554:
	/* block then<FCD5> (ep<FCD1>,retK<FCD4>,_t<FCD3>,_t<FCD2>) */
	ucomisd	 (%rbx), %xmm6
	jae	L57C
L_true55B:
	movsd	 %xmm7, %xmm9
	movsd	 %xmm6, %xmm8
then.55C:
	/* block then<FCDB> (ep<FCD7>,retK<FCDA>,_t<FCD9>,_t<FCD8>) */
	movsd	 %xmm9, %xmm3
	movsd	 %xmm8, %xmm2
	movq	$3, %r15
letJoinK.555:
	/* block letJoinK<FC3F> (ep<FC3A>,retK<FC3E>,_t<FC3D>,_t<FC3C>,_t<FC3B>) */
	cmpq	$1, %r15
	je	S_case556
	cmpq	$3, %r15
	je	S_case558
S_case556:
	movsd	 %xmm3, %xmm11
	movsd	 %xmm2, %xmm10
case.557:
	/* block case<FC44> (ep<FC40>,retK<FC43>,_t<FC42>,_t<FC41>) */
	movabsq	$signBit6455D, %r13
	movsd	 (%r13), %xmm12
	xorpd	 %xmm11, %xmm12
	movsd	 %xmm12, %xmm9
	divsd	 %xmm10, %xmm9
	movq	$10, -8(%rsi)
	movsd	 %xmm9, (%rsi)
	movq	%rsi, %r10
	addq	$16, %rsi
	ucomisd	 8(%rbx), %xmm9
	jb	L_true55E
	movsd	 %xmm9, %xmm13
	movsd	 %xmm10, %xmm12
else.55F:
	/* block else<FCC7> (ep<FCC2>,retK<FCC6>,_t<FCC5>,_t<FCC4>,res<FCC3>) */
	movsd	 %xmm13, %xmm15
	movsd	 %xmm12, %xmm14
	movq	$1, %r14
	jmp	letJoinK.561
L_true55E:
	movsd	 %xmm9, %xmm3
	movsd	 %xmm10, %xmm2
then.560:
	/* block then<FCB2> (ep<FCAD>,retK<FCB1>,_t<FCB0>,_t<FCAF>,res<FCAE>) */
	movabsq	$flt4D3, %r14
	ucomisd	 (%r14), %xmm2
	ja	L_true567
	movsd	 %xmm3, %xmm1
	movsd	 %xmm2, %xmm0
else.566:
	/* block else<FCC0> (ep<FCBB>,retK<FCBF>,_t<FCBE>,_t<FCBD>,res<FCBC>) */
	movsd	 %xmm1, %xmm15
	movsd	 %xmm0, %xmm14
	movq	$1, %r14
letJoinK.561:
	/* block letJoinK<FC4E> (ep<FC48>,retK<FC4D>,_t<FC4C>,_t<FC4B>,res<FC4A>,_t<FC49>) */
	cmpq	$1, %r14
	je	S_case562
	cmpq	$3, %r14
	je	S_case564
S_case562:
	movsd	 %xmm15, %xmm7
	movsd	 %xmm14, %xmm6
case.563:
	/* block case<FCA4> (ep<FC9F>,retK<FCA3>,_t<FCA2>,_t<FCA1>,res<FCA0>) */
	movsd	 %xmm7, %xmm9
	movsd	 %xmm6, %xmm8
	movq	16(%rbx), %r12
	jmp	letJoinK.565
S_case564:
	movq	%r10, %r12
	movsd	 %xmm15, %xmm9
	movsd	 %xmm14, %xmm8
letJoinK.565:
	/* block letJoinK<FC55> (ep<FC4F>,retK<FC54>,_t<FC53>,_t<FC52>,res<FC51>,tFar<FC50>) */
	movsd	 8(%rbx), %xmm15
	movabsq	$signBit6455D, %r15
	movsd	 (%r15), %xmm14
	xorpd	 %xmm15, %xmm14
	movsd	 %xmm14, %xmm13
	ucomisd	 %xmm13, %xmm9
	jbe	L57D
L_true569:
	movsd	 %xmm9, %xmm14
	movsd	 %xmm8, %xmm13
then.56B:
	/* block then<FC88> (ep<FC82>,retK<FC87>,_t<FC86>,_t<FC85>,res<FC84>,tFar<FC83>) */
	movabsq	$flt4D3, %rdx
	ucomisd	 (%rdx), %xmm13
	jae	L57E
L_true572:
	movsd	 %xmm14, %xmm15
then.573:
	/* block then<FC8F> (ep<FC8A>,retK<FC8E>,_t<FC8D>,res<FC8C>,tFar<FC8B>) */
	movsd	 %xmm15, %xmm11
	movq	$3, %r13
	jmp	letJoinK.56C
L57D:
	movsd	 %xmm9, %xmm10
else.56A:
	/* block else<FC9D> (ep<FC98>,retK<FC9C>,_t<FC9B>,res<FC9A>,tFar<FC99>) */
	movsd	 %xmm10, %xmm11
	movq	$1, %r13
letJoinK.56C:
	/* block letJoinK<FC5E> (ep<FC58>,retK<FC5D>,_t<FC5C>,res<FC5B>,tFar<FC5A>,_t<FC59>) */
	cmpq	$1, %r13
	je	S_case56D
	cmpq	$3, %r13
	je	S_case56F
S_case56D:
case.56E:
	/* block case<FC79> (ep<FC75>,retK<FC78>,res<FC77>,tFar<FC76>) */
	movsd	 8(%rbx), %xmm1
	movabsq	$signBit6455D, %rbx
	/* %f8524.d := fneg.d(mem.d[%r8517.64 +.64 8]) */
	movsd	 (%rbx), %xmm0
	/* %f8524.d := fneg.d(mem.d[%r8517.64 +.64 8]) */
	xorpd	 %xmm1, %xmm0
	movsd	 %xmm0, %xmm0
	jmp	letJoinK.570
S_case56F:
	movsd	 %xmm11, %xmm0
letJoinK.570:
	/* block letJoinK<FC64> (retK<FC63>,res<FC62>,tFar<FC61>,_t<FC60>) */
	ucomisd	 (%r12), %xmm0
	jbe	L57F
L_true574:
then.576:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block then<FC68> (retK<FC67>) */
	movq	$10, -8(%rsi)
	movabsq	$flt4D3, %r13
	movsd	 (%r13), %xmm2
	movsd	 %xmm2, (%rsi)
	movq	%rsi, %r12
	addq	$16, %rsi
	movq	$20, -8(%rsi)
	movq	$1, (%rsi)
	movq	%r12, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	%rcx, %rdi
	movq	%r14, %r8
	jmp	letJoinK.46
L57F:
else.575:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block else<FC71> (retK<FC70>,res<FC6F>) */
	movq	$20, -8(%rsi)
	movq	$3, (%rsi)
	movq	%r10, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	%rcx, %rdi
	movq	%r10, %r8
	jmp	letJoinK.46
L57E:
	movsd	 %xmm14, %xmm12
else.571:
	/* block else<FC96> (ep<FC91>,retK<FC95>,_t<FC94>,res<FC93>,tFar<FC92>) */
	movsd	 %xmm12, %xmm11
	movq	$1, %r13
	jmp	letJoinK.56C
L_true567:
	movsd	 %xmm3, %xmm5
	movsd	 %xmm2, %xmm4
then.568:
	/* block then<FCB9> (ep<FCB4>,retK<FCB8>,_t<FCB7>,_t<FCB6>,res<FCB5>) */
	movsd	 %xmm5, %xmm15
	movsd	 %xmm4, %xmm14
	movq	$3, %r14
	jmp	letJoinK.561
S_case558:
case.559:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block case<FCCB> (retK<FCCA>) */
	movq	$10, -8(%rsi)
	movabsq	$flt4D3, %r10
	movsd	 (%r10), %xmm8
	movsd	 %xmm8, (%rsi)
	movq	%rsi, %rbx
	addq	$16, %rsi
	movq	$20, -8(%rsi)
	movq	$1, (%rsi)
	movq	%rbx, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	%rcx, %rdi
	movq	%r12, %r8
	jmp	letJoinK.46
L57C:
	movsd	 %xmm7, %xmm5
	movsd	 %xmm6, %xmm4
else.55A:
	/* block else<FCE1> (ep<FCDD>,retK<FCE0>,_t<FCDF>,_t<FCDE>) */
	movsd	 %xmm5, %xmm3
	movsd	 %xmm4, %xmm2
	movq	$1, %r15
	jmp	letJoinK.555
L57B:
	movsd	 %xmm5, %xmm1
	movsd	 %xmm2, %xmm0
else.553:
	/* block else<FCE7> (ep<FCE3>,retK<FCE6>,_t<FCE5>,_t<FCE4>) */
	movsd	 %xmm1, %xmm3
	movsd	 %xmm0, %xmm2
	movq	$1, %r15
	jmp	letJoinK.555
doGC57A:
	movq	$36, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %r15
	addq	$40, %rsi
	movq	%r15, %rdi
	movabsq	$retGC578, %r8
	jmp	ASM_InvokeGC
	/* live= GP={%r10 %rcx %rdx %rbx} spilled=  */
retGC578:
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
	jmp	gcTest579
	.text
retGC58D:
	movq	40(%rdi), %r14
	movq	32(%rdi), %r10
	movq	24(%rdi), %rdx
	movq	16(%rdi), %r15
	movq	8(%rdi), %r13
	movq	(%rdi), %r12
	jmp	gcTest58E
S_case592:
case.593:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi} FP={%xmm2}  */
	/* block case<FD11> (ep<FD0D>,retK<FD10>,_anon_<FD0F>,_anon_<FD0E>) */
	movq	%r12, %rdi
	movq	%rdx, %r8
	movq	%r10, %r9
	/* %xmm2.d := mem.d[%r8695.64 +.64 0] */
	movsd	 (%r10), %xmm2
	movq	%rcx, %r10
polyhedronIntersect.594:
	movsd	 %xmm2, %xmm0
	movq	%r9, %rbx
	movq	%r8, %r15
	movq	%rdi, %r12
	movsd	 %xmm0, %xmm2
	jmp	gcTest599
	/* live= GP={%r10 %rbx %r15 %r12} FP={%xmm3} spilled=  */
retGC598:
	movq	32(%rdi), %r10
	movq	16(%rdi), %rbx
	movq	8(%rdi), %r15
	movq	(%rdi), %r12
	/* %f8775.d := mem.d[%rdi.64 +.64 24] */
	movsd	 24(%rdi), %xmm2
gcTest599:
	movq	%r11, %rcx
	subq	%rsi, %rcx
	jg	L59B
doGC59A:
	movq	$2827, -8(%rsi)
	movq	%r12, (%rsi)
	movq	%r15, 8(%rsi)
	movq	%rbx, 16(%rsi)
	movsd	 %xmm2, 24(%rsi)
	movq	%r10, 32(%rsi)
	movq	%rsi, %r14
	addq	$48, %rsi
	movq	%r14, %rdi
	movabsq	$retGC598, %r8
	jmp	ASM_InvokeGC
L59B:
	movsd	 %xmm2, %xmm3
check.580:
	/* block check<10D2E> (ep<FCEB>,lyst<FCEC>,n<FCED>,_t<FCEE>,retK<FCEF>) */
	cmpq	$1, %r15
	jne	L_true581
	movsd	 %xmm3, %xmm4
else.582:
	/* block else<FD35> (retK<FD34>,n<FD33>,_t<FD32>) */
	movabsq	$flt4D3, %rcx
	ucomisd	 (%rcx), %xmm4
	je	L_true584
else.585:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block else<FD40> (retK<FD3F>,n<FD3E>) */
	movq	%r10, %rdi
	movq	$3, %r8
	movq	%rbx, %r9
	jmp	letJoinK.587
L_true581:
	movq	%r10, %r13
	movq	%r12, %r14
	movsd	 %xmm3, %xmm5
then.583:
	/* block then<FCF9> (ep<FCF4>,retK<FCF8>,lyst<FCF7>,n<FCF6>,_t<FCF5>) */
	movq	8(%r15), %rcx
	movq	(%r15), %rdx
	movq	8(%rdx), %r15
	ucomisd	 (%r15), %xmm5
	ja	L_true588
	movq	%rcx, %r10
	movq	%r14, %r12
else.589:
	/* block else<FD2F> (ep<FD2A>,retK<FD2E>,n<FD2D>,_anon_<FD2C>,_anon_<FD2B>) */
	movq	%r15, %rcx
	movq	%r10, %rdx
	movq	$1, %r14
	jmp	letJoinK.58B
L_true588:
	movq	%rcx, %rdx
	movq	%r13, %r10
	movq	%r14, %r12
then.58A:
	/* block then<FD19> (ep<FD14>,retK<FD18>,n<FD17>,_anon_<FD16>,_anon_<FD15>) */
	movabsq	$flt4D3, %r13
	movsd	 (%r15), %xmm1
	ucomisd	 (%r13), %xmm1
	ja	L_true596
	movq	%r15, %rcx
else.595:
	/* block else<FD28> (ep<FD23>,retK<FD27>,n<FD26>,_anon_<FD25>,_anon_<FD24>) */
	movq	%r10, %r13
	movq	$1, %r14
letJoinK.58B:
	/* block letJoinK<FD05> (ep<10D22>,retK<10D23>,n<10D24>,_anon_<10D25>,_anon_<10D26>,_t<10D27>) */
	movq	%rcx, %r10
	movq	%rbx, %r15
gcTest58E:
	movq	%r11, %rcx
	subq	%rsi, %rcx
	jle	doGC58F
	movq	%r13, %rcx
check.58C:
	/* block check<10D28> (ep<FCFF>,retK<FD04>,n<FD03>,_anon_<FD02>,_anon_<FD01>,_t<FD00>) */
	movq	%r14, %rbx
	cmpq	$1, %rbx
	je	S_case590
	cmpq	$3, %rbx
	je	S_case592
S_case590:
case.591:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi} FP={%xmm2}  */
	/* block case<FD0A> (ep<FD06>,retK<FD09>,n<FD08>,_anon_<FD07>) */
	movq	%r12, %rdi
	movq	%rdx, %r8
	movq	%r15, %r9
	/* %xmm2.d := mem.d[%r8698.64 +.64 0] */
	movsd	 (%r15), %xmm2
	movq	%rcx, %r10
	jmp	polyhedronIntersect.594
doGC58F:
	movq	$52, -8(%rsi)
	movq	%r12, (%rsi)
	movq	%r13, 8(%rsi)
	movq	%r15, 16(%rsi)
	movq	%rdx, 24(%rsi)
	movq	%r10, 32(%rsi)
	movq	%r14, 40(%rsi)
	movq	%rsi, %rbx
	addq	$56, %rsi
	movq	%rbx, %rdi
	movabsq	$retGC58D, %r8
	jmp	ASM_InvokeGC
L_true596:
	movq	%r15, %rcx
then.597:
	/* block then<FD21> (ep<FD1C>,retK<FD20>,n<FD1F>,_anon_<FD1E>,_anon_<FD1D>) */
	movq	%r10, %r13
	movq	$3, %r14
	jmp	letJoinK.58B
L_true584:
then.586:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block then<FD3A> (retK<FD39>,n<FD38>) */
	movq	%r10, %rdi
	movq	$1, %r8
	movq	%rbx, %r9
	jmp	letJoinK.587
	/* live= GP={%r14 %r10 %rdx %r15 %rcx %r12} spilled=  */
	jmp	retGC58D
	.text
Primsurf.5A9:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest5AB
	/* live= GP={%r10 %rcx %rdx %rbx} spilled=  */
retGC5AA:
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest5AB:
	movq	%r11, %r12
	subq	%rsi, %r12
	jle	doGC5AC
check.59C:
	/* block check<10D33> (ep<FD45>,p<FD46>,retK<FD47>,_exh<FD48>) */
	movq	(%rdx), %r13
	cmpq	$11, %r13
	jne	L5AD
S_case59D:
case.59E:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block case<FD51> (p<FD50>,retK<FD4F>) */
	movq	%rcx, %rdi
	movq	56(%rdx), %r8
	jmp	letJoinK.5A8
doGC5AC:
	movq	$36, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %rcx
	addq	$40, %rsi
	movq	%rcx, %rdi
	movabsq	$retGC5AA, %r8
	jmp	ASM_InvokeGC
L5AD:
	cmpq	$9, %r13
	je	S_case59F
	cmpq	$7, %r13
	jne	L5AE
S_case5A1:
case.5A2:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block case<FD5F> (p<FD5E>,retK<FD5D>) */
	movq	%rcx, %rdi
	movq	64(%rdx), %r8
	jmp	letJoinK.5A8
S_case59F:
case.5A0:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block case<FD58> (p<FD57>,retK<FD56>) */
	movq	%rcx, %rdi
	movq	72(%rdx), %r8
	jmp	letJoinK.5A8
L5AE:
	cmpq	$5, %r13
	jne	L5AF
S_case5A3:
case.5A4:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block case<FD66> (p<FD65>,retK<FD64>) */
	movq	%rcx, %rdi
	movq	48(%rdx), %r8
	jmp	letJoinK.5A8
L5AF:
	cmpq	$1, %r13
	jne	L5B0
S_case5A5:
case.5A6:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block case<FD6D> (p<FD6C>,retK<FD6B>) */
	movq	%rcx, %rdi
	movq	40(%rdx), %r8
	jmp	letJoinK.5A8
L5B0:
default.5A7:
	/* Liveout:  GP={%rax %rdi}  */
	/* block default<FD73> (_exh<FD72>) */
	movq	$12, -8(%rsi)
	movabsq	$tagE7, %r14
	movq	%r14, (%rsi)
	movq	%rsi, %r12
	addq	$16, %rsi
	movq	(%r10), %r15
	movq	%r12, %rax
	movq	%r10, %rdi
	jmp	*%r15
	.text
Primnorm.5BE:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movsd	 %xmm4, %xmm5
	movsd	 %xmm3, %xmm4
	movsd	 %xmm2, %xmm3
	movq	%rdi, %rbx
	movsd	 %xmm5, %xmm0
	movsd	 %xmm4, %xmm15
	movsd	 %xmm3, %xmm14
	jmp	gcTest5C0
	/* live= GP={%r10 %rcx %rdx %rbx} FP={%xmm2 %xmm1 %xmm0} spilled=  */
retGC5BF:
	movq	48(%rdi), %r10
	movq	40(%rdi), %rcx
	movq	32(%rdi), %rdx
	movq	(%rdi), %rbx
	/* %f8998.d := mem.d[%rdi.64 +.64 24] */
	movsd	 24(%rdi), %xmm0
	/* %f8997.d := mem.d[%rdi.64 +.64 16] */
	movsd	 16(%rdi), %xmm15
	/* %f8996.d := mem.d[%rdi.64 +.64 8] */
	movsd	 8(%rdi), %xmm14
gcTest5C0:
	movq	%r11, %r12
	subq	%rsi, %r12
	jle	doGC5C1
	movsd	 %xmm0, %xmm2
	movsd	 %xmm15, %xmm1
	movsd	 %xmm14, %xmm0
check.5B1:
	/* block check<10D3B> (ep<FD79>,_t<FD7A>,_t<FD7B>,_t<FD7C>,p<FD7D>,retK<FD7E>,_exh<FD7F>) */
	movq	(%rdx), %r13
	cmpq	$11, %r13
	jne	L5C2
S_case5B2:
	movsd	 %xmm2, %xmm2
	movsd	 %xmm1, %xmm1
	movsd	 %xmm0, %xmm15
case.5B3:
	/* Liveout:  GP={%r8 %rdi} FP={%xmm4 %xmm3 %xmm2}  */
	/* block case<FD8B> (retK<FD8A>,p<FD89>,_t<FD88>,_t<FD87>,_t<FD86>) */
	movabsq	$flt536, %r10
	movsd	 (%r10), %xmm7
	movsd	 %xmm15, %xmm9
	subsd	 8(%rdx), %xmm9
	movsd	 %xmm7, %xmm8
	mulsd	 %xmm9, %xmm8
	movq	$10, -8(%rsi)
	movsd	 %xmm8, (%rsi)
	movq	%rsi, %r12
	addq	$16, %rsi
	movsd	 %xmm1, %xmm11
	subsd	 16(%rdx), %xmm11
	movsd	 %xmm7, %xmm10
	mulsd	 %xmm11, %xmm10
	movq	$10, -8(%rsi)
	movsd	 %xmm10, (%rsi)
	movq	%rsi, %r13
	addq	$16, %rsi
	movsd	 %xmm2, %xmm13
	subsd	 24(%rdx), %xmm13
	movsd	 %xmm7, %xmm12
	mulsd	 %xmm13, %xmm12
	movq	$10, -8(%rsi)
	movsd	 %xmm12, (%rsi)
	movq	%rsi, %r14
	addq	$16, %rsi
	movq	$28, -8(%rsi)
	movq	%r12, (%rsi)
	movq	%r13, 8(%rsi)
	movq	%r14, 16(%rsi)
	movq	%rsi, %r15
	addq	$32, %rsi
	movq	%rcx, %rdi
	movq	%r15, %r8
	movsd	 %xmm8, %xmm2
	movsd	 %xmm10, %xmm3
	movsd	 %xmm12, %xmm4
	jmp	letJoinK.5BD
doGC5C1:
	movq	$14351, -8(%rsi)
	movq	%rbx, (%rsi)
	movsd	 %xmm14, 8(%rsi)
	movsd	 %xmm15, 16(%rsi)
	movsd	 %xmm0, 24(%rsi)
	movq	%rdx, 32(%rsi)
	movq	%rcx, 40(%rsi)
	movq	%r10, 48(%rsi)
	movq	%rsi, %rcx
	addq	$64, %rsi
	movq	%rcx, %rdi
	movabsq	$retGC5BF, %r8
	jmp	ASM_InvokeGC
L5C2:
	cmpq	$9, %r13
	je	S_case5B4
	cmpq	$7, %r13
	jne	L5C3
S_case5B6:
	movsd	 %xmm2, %xmm11
	movsd	 %xmm1, %xmm10
	movsd	 %xmm0, %xmm9
case.5B7:
	/* Liveout:  GP={%r8 %rdi} FP={%xmm4 %xmm3 %xmm2}  */
	/* block case<FDB9> (retK<FDB8>,p<FDB7>,_t<FDB6>,_t<FDB5>,_t<FDB4>) */
	movabsq	$flt536, %r14
	movsd	 (%r14), %xmm6
	movsd	 %xmm9, %xmm8
	subsd	 8(%rdx), %xmm8
	movsd	 %xmm6, %xmm7
	mulsd	 %xmm8, %xmm7
	movq	$10, -8(%rsi)
	movsd	 %xmm7, (%rsi)
	movq	%rsi, %r15
	addq	$16, %rsi
	movsd	 %xmm10, %xmm10
	subsd	 16(%rdx), %xmm10
	movsd	 %xmm6, %xmm9
	mulsd	 %xmm10, %xmm9
	movq	$10, -8(%rsi)
	movsd	 %xmm9, (%rsi)
	movq	%rsi, %rbx
	addq	$16, %rsi
	movsd	 %xmm11, %xmm12
	subsd	 24(%rdx), %xmm12
	movsd	 %xmm6, %xmm11
	mulsd	 %xmm12, %xmm11
	movq	$10, -8(%rsi)
	movsd	 %xmm11, (%rsi)
	movq	%rsi, %r10
	addq	$16, %rsi
	movq	$28, -8(%rsi)
	movq	%r15, (%rsi)
	movq	%rbx, 8(%rsi)
	movq	%r10, 16(%rsi)
	movq	%rsi, %r12
	addq	$32, %rsi
	movq	%rcx, %rdi
	movq	%r12, %r8
	movsd	 %xmm7, %xmm2
	movsd	 %xmm9, %xmm3
	movsd	 %xmm11, %xmm4
	jmp	letJoinK.5BD
S_case5B4:
	movsd	 %xmm2, %xmm14
	movsd	 %xmm1, %xmm13
	movsd	 %xmm0, %xmm12
case.5B5:
	/* Liveout:  GP={%r8 %rdi} FP={%xmm4 %xmm3 %xmm2}  */
	/* block case<FDA2> (retK<FDA1>,p<FDA0>,_t<FD9F>,_t<FD9E>,_t<FD9D>) */
	movabsq	$flt536, %r13
	movsd	 (%r13), %xmm15
	movsd	 %xmm12, %xmm1
	subsd	 8(%rdx), %xmm1
	movsd	 %xmm15, %xmm0
	mulsd	 %xmm1, %xmm0
	movq	$10, -8(%rsi)
	movsd	 %xmm0, (%rsi)
	movq	%rsi, %r14
	addq	$16, %rsi
	movsd	 %xmm13, %xmm4
	subsd	 16(%rdx), %xmm4
	movsd	 %xmm15, %xmm3
	mulsd	 %xmm4, %xmm3
	movq	$10, -8(%rsi)
	movsd	 %xmm3, (%rsi)
	movq	%rsi, %r15
	addq	$16, %rsi
	movsd	 %xmm14, %xmm6
	subsd	 24(%rdx), %xmm6
	movsd	 %xmm15, %xmm5
	mulsd	 %xmm6, %xmm5
	movq	$10, -8(%rsi)
	movsd	 %xmm5, (%rsi)
	movq	%rsi, %rdx
	addq	$16, %rsi
	movq	$28, -8(%rsi)
	movq	%r14, (%rsi)
	movq	%r15, 8(%rsi)
	movq	%rdx, 16(%rsi)
	movq	%rsi, %rbx
	addq	$32, %rsi
	movq	%rcx, %rdi
	movq	%rbx, %r8
	movsd	 %xmm0, %xmm2
	movsd	 %xmm3, %xmm3
	movsd	 %xmm5, %xmm4
	jmp	letJoinK.5BD
L5C3:
	cmpq	$5, %r13
	jne	L5C4
S_case5B8:
	movsd	 %xmm2, %xmm8
	movsd	 %xmm1, %xmm7
	movsd	 %xmm0, %xmm6
case.5B9:
	/* Liveout:  GP={%r8 %rdi} FP={%xmm4 %xmm3 %xmm2}  */
	/* block case<FDD0> (retK<FDCF>,p<FDCE>,_t<FDCD>,_t<FDCC>,_t<FDCB>) */
	movabsq	$flt536, %r15
	movsd	 (%r15), %xmm13
	divsd	 32(%rdx), %xmm13
	movsd	 %xmm6, %xmm15
	subsd	 8(%rdx), %xmm15
	movsd	 %xmm13, %xmm14
	mulsd	 %xmm15, %xmm14
	movq	$10, -8(%rsi)
	movsd	 %xmm14, (%rsi)
	movq	%rsi, %rbx
	addq	$16, %rsi
	movsd	 %xmm7, %xmm1
	subsd	 16(%rdx), %xmm1
	movsd	 %xmm13, %xmm0
	mulsd	 %xmm1, %xmm0
	movq	$10, -8(%rsi)
	movsd	 %xmm0, (%rsi)
	movq	%rsi, %r10
	addq	$16, %rsi
	movsd	 %xmm8, %xmm5
	subsd	 24(%rdx), %xmm5
	movsd	 %xmm13, %xmm4
	mulsd	 %xmm5, %xmm4
	movq	$10, -8(%rsi)
	movsd	 %xmm4, (%rsi)
	movq	%rsi, %r12
	addq	$16, %rsi
	movq	$28, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%r10, 8(%rsi)
	movq	%r12, 16(%rsi)
	movq	%rsi, %r13
	addq	$32, %rsi
	movq	%rcx, %rdi
	movq	%r13, %r8
	movsd	 %xmm14, %xmm2
	movsd	 %xmm0, %xmm3
	movsd	 %xmm4, %xmm4
	jmp	letJoinK.5BD
L5C4:
	cmpq	$1, %r13
	jne	L5C5
S_case5BA:
	movsd	 %xmm2, %xmm5
	movsd	 %xmm1, %xmm4
	movsd	 %xmm0, %xmm3
case.5BB:
	/* Liveout:  GP={%r8 %rdi} FP={%xmm4 %xmm3 %xmm2}  */
	/* block case<FDE9> (retK<FDE8>,p<FDE7>,_t<FDE6>,_t<FDE5>,_t<FDE4>) */
	movabsq	$flt536, %rbx
	movsd	 (%rbx), %xmm6
	divsd	 32(%rdx), %xmm6
	movsd	 %xmm3, %xmm8
	subsd	 8(%rdx), %xmm8
	movsd	 %xmm6, %xmm7
	mulsd	 %xmm8, %xmm7
	movq	$10, -8(%rsi)
	movsd	 %xmm7, (%rsi)
	movq	%rsi, %r10
	addq	$16, %rsi
	movsd	 %xmm4, %xmm10
	subsd	 16(%rdx), %xmm10
	movsd	 %xmm6, %xmm9
	mulsd	 %xmm10, %xmm9
	movq	$10, -8(%rsi)
	movsd	 %xmm9, (%rsi)
	movq	%rsi, %r12
	addq	$16, %rsi
	movsd	 %xmm5, %xmm12
	subsd	 24(%rdx), %xmm12
	movsd	 %xmm6, %xmm11
	mulsd	 %xmm12, %xmm11
	movq	$10, -8(%rsi)
	movsd	 %xmm11, (%rsi)
	movq	%rsi, %r13
	addq	$16, %rsi
	movq	$28, -8(%rsi)
	movq	%r10, (%rsi)
	movq	%r12, 8(%rsi)
	movq	%r13, 16(%rsi)
	movq	%rsi, %r14
	addq	$32, %rsi
	movq	%rcx, %rdi
	movq	%r14, %r8
	movsd	 %xmm7, %xmm2
	movsd	 %xmm9, %xmm3
	movsd	 %xmm11, %xmm4
	jmp	letJoinK.5BD
L5C5:
default.5BC:
	/* Liveout:  GP={%rax %rdi}  */
	/* block default<FDFE> (_exh<FDFD>) */
	movq	$12, -8(%rsi)
	movabsq	$tagE7, %r14
	movq	%r14, (%rsi)
	movq	%rsi, %r12
	addq	$16, %rsi
	movq	(%r10), %r15
	movq	%r12, %rax
	movq	%r10, %rdi
	jmp	*%r15
	.text
anon.5C7:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest5C9
	/* live= GP={%r10 %rcx %rdx %rbx} spilled=  */
retGC5C8:
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest5C9:
	movq	%r11, %r13
	subq	%rsi, %r13
	jle	doGC5CA
check.5C6:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<10D40> (ep<FE93>,x<FE94>,retK<FE95>,_exh<FE96>) */
	movq	$20, -8(%rsi)
	movq	$1, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	%rcx, %rdi
	movq	%r10, %r8
	jmp	letJoinK.46
doGC5CA:
	movq	$36, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %r12
	addq	$40, %rsi
	movq	%r12, %rdi
	movabsq	$retGC5C8, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.5CC:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest5CE
	/* live= GP={%rcx %rdx} spilled=  */
retGC5CD:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest5CE:
	movq	%r11, %r15
	subq	%rsi, %r15
	jle	doGC5CF
check.5CB:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<10D43> (ep<FEE2>,_t<FEE0>) */
	movq	$20, -8(%rsi)
	movq	16(%rdx), %r10
	movq	%r10, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	8(%rdx), %r12
	movq	(%r12), %r13
	movq	%r12, %rdi
	movq	%rbx, %r8
	jmp	*%r13
doGC5CF:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	%r14, %rdi
	movabsq	$retGC5CD, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.587:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
gcTest5D7:
	movq	%r11, %r10
	subq	%rsi, %r10
	jle	doGC5D8
check.5D0:
	/* block check<10D47> (ep<FED0>,hit<FECC>,where'<FECD>) */
	cmpq	$1, %rdx
	jne	L5D9
S_case5D1:
case.5D2:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block case<FED4> (ep<FED3>) */
	movq	8(%rbx), %rdx
	movq	(%rdx), %rdi
	movq	40(%rbx), %r8
	movq	16(%rbx), %r9
	movq	24(%rbx), %r10
	jmp	sphmap.5D5
doGC5D8:
	movq	$28, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%rsi, %rbx
	addq	$32, %rsi
	movq	%rbx, %rdi
	movabsq	$retGC5D6, %r8
	jmp	ASM_InvokeGC
	/* live= GP={%rcx %rdx %rbx} spilled=  */
retGC5D6:
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
	jmp	gcTest5D7
L5D9:
	cmpq	$3, %rdx
	jne	S_case5D1
S_case5D3:
case.5D4:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block case<FEDD> (ep<FEDB>,where'<FEDC>) */
	movq	$20, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	32(%rbx), %r12
	movq	%r12, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	$775, -8(%rsi)
	movabsq	$letJoinK.5CC, %r14
	movq	%r14, (%rsi)
	movq	16(%rbx), %r15
	movq	%r15, 8(%rsi)
	movq	%r10, 16(%rsi)
	movq	%rsi, %r13
	addq	$32, %rsi
	movq	8(%rbx), %rcx
	movq	(%rcx), %rdi
	movq	40(%rbx), %r8
	movq	%r13, %r9
	movq	24(%rbx), %r10
	jmp	sphmap.5D5
	.text
anon.5DB:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest5DD
	/* live= GP={%r10 %rcx %rdx %rbx} spilled=  */
retGC5DC:
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest5DD:
	movq	%r11, %r12
	subq	%rsi, %r12
	jle	doGC5DE
check.5DA:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<10D4C> (ep<10007>,param<10008>,retK<10009>,_exh<1000A>) */
	movq	$36, -8(%rsi)
	movq	(%rdx), %r12
	movq	%r12, (%rsi)
	movq	8(%rdx), %r13
	movq	%r13, 8(%rsi)
	movq	(%rbx), %r14
	movq	%r14, 16(%rsi)
	movq	8(%rbx), %r15
	movq	%r15, 24(%rsi)
	movq	%rsi, %r10
	addq	$40, %rsi
	movq	%rcx, %rdi
	movq	%r10, %r8
	jmp	letJoinK.46
doGC5DE:
	movq	$36, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %rcx
	addq	$40, %rsi
	movq	%rcx, %rdi
	movabsq	$retGC5DC, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.5E0:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest5E2
	/* live= GP={%rcx %rdx} spilled=  */
retGC5E1:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest5E2:
	movq	%r11, %r14
	subq	%rsi, %r14
	jle	doGC5E3
check.5DF:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi} FP={%xmm2}  */
	/* block check<10D4F> (ep<1001E>,_t<1001B>) */
	movabsq	$flt4D3, %rbx
	movsd	 (%rbx), %xmm0
	movq	$10, -8(%rsi)
	movsd	 %xmm0, (%rsi)
	movq	%rsi, %r10
	addq	$16, %rsi
	movq	8(%rdx), %r12
	movq	(%r12), %rdi
	movq	%rcx, %r8
	movq	%r10, %r9
	movsd	 %xmm0, %xmm2
	movq	16(%rdx), %r10
	jmp	polyhedronIntersect.594
doGC5E3:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	%r13, %rdi
	movabsq	$retGC5E1, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.5E5:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest5E7
	/* live= GP={%rcx %rdx} spilled=  */
retGC5E6:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest5E7:
	movq	%r11, %r15
	subq	%rsi, %r15
	jle	doGC5E8
check.5E4:
	/* Liveout:  GP={%r12 %r10 %r9 %r8 %rdi}  */
	/* block check<10D52> (ep<10018>,_t<10014>) */
	movq	$775, -8(%rsi)
	movabsq	$letJoinK.5E0, %rbx
	movq	%rbx, (%rsi)
	movq	24(%rdx), %r10
	movq	%r10, 8(%rsi)
	movq	40(%rdx), %r12
	movq	%r12, 16(%rsi)
	movq	%rsi, %r10
	addq	$32, %rsi
	movq	8(%rdx), %r13
	movq	(%r13), %rdi
	movq	16(%rdx), %r8
	movq	%rcx, %r9
	movq	32(%rdx), %r12
	jmp	map_uncurried.5C
doGC5E8:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	%r14, %rdi
	movabsq	$retGC5E6, %r8
	jmp	ASM_InvokeGC
	.text
sphmap.5D5:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
gcTest61E:
	movq	%r11, %r12
	subq	%rsi, %r12
	jle	doGC61F
	movq	%r10, %r12
check.5E9:
	/* block check<10D57> (ep<FEBA>,l<FEBB>,retK<FEBC>,_exh<FEBD>) */
	cmpq	$1, %rdx
	jne	L_true5EA
else.5EB:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block else<1008C> (retK<1008B>) */
	movq	(%rcx), %r15
	movq	%rcx, %rdi
	movq	$1, %r8
	jmp	*%r15
L_true5EA:
then.5EC:
	/* block then<FEC6> (ep<FEC2>,l<FEC5>,retK<FEC4>,_exh<FEC3>) */
	movq	(%rdx), %r14
	movq	$133, -8(%rsi)
	movq	%rbx, (%rsi)
	movabsq	$sphmap.5D5, %r13
	movq	%r13, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	$7949, -8(%rsi)
	movabsq	$letJoinK.587, %r15
	movq	%r15, (%rsi)
	movq	%r10, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r12, 24(%rsi)
	movq	%r14, 32(%rsi)
	movq	8(%rdx), %rcx
	movq	%rcx, 40(%rsi)
	movq	%rsi, %r10
	addq	$56, %rsi
	movq	(%r14), %rdx
	cmpq	$7, %rdx
	jne	L620
S_case5ED:
case.5EE:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block case<FEF0> (letJoinK<FEEF>) */
	movq	$10, -8(%rsi)
	movabsq	$flt4D3, %r15
	movsd	 (%r15), %xmm5
	movsd	 %xmm5, (%rsi)
	movq	%rsi, %r14
	addq	$16, %rsi
	movq	%r10, %rdi
	movq	$3, %r8
	movq	%r14, %r9
	jmp	letJoinK.587
L620:
	cmpq	$5, %rdx
	jne	L621
S_case5EF:
case.5F0:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block case<FEF7> (letJoinK<FEF6>) */
	movq	$10, -8(%rsi)
	movabsq	$flt4D3, %r13
	movsd	 (%r13), %xmm4
	movsd	 %xmm4, (%rsi)
	movq	%rsi, %r12
	addq	$16, %rsi
	movq	%r10, %rdi
	movq	$3, %r8
	movq	%r12, %r9
	jmp	letJoinK.587
L621:
	cmpq	$3, %rdx
	jne	L622
S_case5F1:
case.5F2:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block case<FEFE> (letJoinK<FEFD>) */
	movq	$10, -8(%rsi)
	movabsq	$flt4D3, %rbx
	movsd	 (%rbx), %xmm3
	movsd	 %xmm3, (%rsi)
	movq	%rsi, %rdx
	addq	$16, %rsi
	movq	%r10, %rdi
	movq	$3, %r8
	movq	%rdx, %r9
	jmp	letJoinK.587
L622:
	cmpq	$1, %rdx
	jne	S_case5ED
S_case5F3:
case.5F4:
	/* block case<FF07> (ep<FF03>,_exh<FF06>,_anon_<FF05>,letJoinK<FF04>) */
	movq	8(%r14), %r13
	movq	(%r13), %r14
	cmpq	$11, %r14
	je	S_case5F5
	cmpq	$9, %r14
	jne	L623
S_case5F7:
case.5F8:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block case<FF15> (letJoinK<FF14>) */
	movq	$10, -8(%rsi)
	movabsq	$flt4D3, %r14
	movsd	 (%r14), %xmm1
	movsd	 %xmm1, (%rsi)
	movq	%rsi, %r13
	addq	$16, %rsi
	movq	%r10, %rdi
	movq	$3, %r8
	movq	%r13, %r9
	jmp	letJoinK.587
L628:
	cmpq	$1, %r14
	je	S_case5FF
S_case5F5:
case.5F6:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block case<FF0E> (letJoinK<FF0D>) */
	movq	$10, -8(%rsi)
	movabsq	$flt4D3, %rcx
	movsd	 (%rcx), %xmm2
	movsd	 %xmm2, (%rsi)
	movq	%rsi, %r15
	addq	$16, %rsi
	movq	%r10, %rdi
	movq	$3, %r8
	movq	%r15, %r9
	jmp	letJoinK.587
S_case5FF:
case.600:
	/* block case<10032> (ep<1002F>,letJoinK<10031>,_anon_<10030>) */
	movsd	 32(%r13), %xmm1
	movq	24(%rbx), %rcx
	movq	(%rcx), %r15
	movsd	 (%r15), %xmm2
	subsd	 8(%r13), %xmm2
	movq	24(%rbx), %r12
	movq	8(%r12), %rdx
	movsd	 (%rdx), %xmm3
	subsd	 16(%r13), %xmm3
	movq	24(%rbx), %r15
	movq	16(%r15), %r14
	movsd	 (%r14), %xmm4
	subsd	 24(%r13), %xmm4
	movq	32(%rbx), %rdx
	movq	16(%rdx), %rcx
	movsd	 %xmm4, %xmm6
	mulsd	 (%rcx), %xmm6
	movq	32(%rbx), %r13
	movq	8(%r13), %r12
	movsd	 %xmm3, %xmm7
	mulsd	 (%r12), %xmm7
	movq	32(%rbx), %r15
	movq	(%r15), %r14
	movsd	 %xmm2, %xmm5
	mulsd	 (%r14), %xmm5
	addsd	 %xmm7, %xmm5
	addsd	 %xmm6, %xmm5
	movsd	 %xmm1, %xmm9
	mulsd	 %xmm1, %xmm9
	movsd	 %xmm4, %xmm11
	mulsd	 %xmm4, %xmm11
	movsd	 %xmm3, %xmm12
	mulsd	 %xmm3, %xmm12
	movsd	 %xmm2, %xmm10
	mulsd	 %xmm2, %xmm10
	addsd	 %xmm12, %xmm10
	addsd	 %xmm11, %xmm10
	movsd	 %xmm5, %xmm8
	mulsd	 %xmm5, %xmm8
	subsd	 %xmm10, %xmm8
	addsd	 %xmm9, %xmm8
	movabsq	$flt4D3, %rcx
	ucomisd	 (%rcx), %xmm8
	jae	L624
L_true601:
then.603:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block then<1005E> (letJoinK<1005D>) */
	movq	$10, -8(%rsi)
	movabsq	$flt4D3, %rdx
	movsd	 (%rdx), %xmm4
	movsd	 %xmm4, (%rsi)
	movq	%rsi, %rcx
	addq	$16, %rsi
	movq	%r10, %rdi
	movq	$1, %r8
	movq	%rcx, %r9
	jmp	letJoinK.587
L624:
	movsd	 %xmm8, %xmm7
	movsd	 %xmm5, %xmm6
else.602:
	/* block else<10067> (letJoinK<10066>,_t<10065>,_t<10064>) */
	sqrtsd	 %xmm7, %xmm14
	movabsq	$signBit6455D, %rdx
	movsd	 (%rdx), %xmm15
	xorpd	 %xmm6, %xmm15
	movsd	 %xmm15, %xmm13
	subsd	 %xmm14, %xmm13
	movq	$10, -8(%rsi)
	movsd	 %xmm13, (%rsi)
	movq	%rsi, %rdx
	addq	$16, %rsi
	sqrtsd	 %xmm7, %xmm1
	movabsq	$signBit6455D, %rbx
	movsd	 (%rbx), %xmm2
	xorpd	 %xmm6, %xmm2
	movsd	 %xmm2, %xmm0
	addsd	 %xmm1, %xmm0
	movq	$10, -8(%rsi)
	movsd	 %xmm0, (%rsi)
	movq	%rsi, %rcx
	addq	$16, %rsi
	movabsq	$flt4D3, %r12
	ucomisd	 (%r12), %xmm13
	jae	L625
L_true604:
	movsd	 %xmm0, %xmm8
then.606:
	/* block then<10075> (letJoinK<10074>,_t<10073>,res<10072>) */
	movabsq	$flt4D3, %r13
	ucomisd	 (%r13), %xmm8
	jb	L_true607
else.608:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block else<10081> (letJoinK<10080>,res<1007F>) */
	movq	%r10, %rdi
	movq	$3, %r8
	movq	%rcx, %r9
	jmp	letJoinK.587
L625:
else.605:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block else<10087> (letJoinK<10086>,res<10085>) */
	movq	%r10, %rdi
	movq	$3, %r8
	movq	%rdx, %r9
	jmp	letJoinK.587
L_true607:
then.609:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block then<10079> (letJoinK<10078>) */
	movq	$10, -8(%rsi)
	movabsq	$flt4D3, %r15
	movsd	 (%r15), %xmm3
	movsd	 %xmm3, (%rsi)
	movq	%rsi, %r14
	addq	$16, %rsi
	movq	%r10, %rdi
	movq	$1, %r8
	movq	%r14, %r9
	jmp	letJoinK.587
L623:
	cmpq	$7, %r14
	jne	L626
S_case5F9:
case.5FA:
	/* block case<FF1D> (ep<FF1A>,letJoinK<FF1C>,_anon_<FF1B>) */
	movq	24(%rbx), %rcx
	movq	(%rcx), %r15
	movsd	 (%r15), %xmm12
	subsd	 8(%r13), %xmm12
	movq	24(%rbx), %r12
	movq	8(%r12), %rdx
	movsd	 (%rdx), %xmm13
	subsd	 16(%r13), %xmm13
	movq	24(%rbx), %r15
	movq	16(%r15), %r14
	movsd	 (%r14), %xmm14
	subsd	 24(%r13), %xmm14
	movq	32(%rbx), %rdx
	movq	(%rdx), %rcx
	movq	32(%rbx), %r13
	movq	8(%r13), %r12
	movq	32(%rbx), %r15
	movq	16(%r15), %r14
	movsd	 %xmm13, %xmm0
	mulsd	 (%r12), %xmm0
	movsd	 %xmm12, %xmm15
	mulsd	 (%rcx), %xmm15
	addsd	 %xmm0, %xmm15
	addsd	 (%r14), %xmm15
	movsd	 %xmm13, %xmm4
	mulsd	 %xmm13, %xmm4
	movsd	 %xmm12, %xmm3
	mulsd	 %xmm12, %xmm3
	addsd	 %xmm4, %xmm3
	addsd	 %xmm14, %xmm3
	movsd	 (%r12), %xmm6
	mulsd	 (%r12), %xmm6
	movsd	 (%rcx), %xmm5
	mulsd	 (%rcx), %xmm5
	addsd	 %xmm6, %xmm5
	movabsq	$flt60A, %rcx
	movsd	 (%rcx), %xmm2
	mulsd	 %xmm5, %xmm2
	mulsd	 %xmm3, %xmm2
	movsd	 %xmm15, %xmm1
	mulsd	 %xmm15, %xmm1
	subsd	 %xmm2, %xmm1
	movabsq	$flt4D3, %rdx
	ucomisd	 (%rdx), %xmm1
	jb	L_true614
	movsd	 %xmm1, %xmm15
	movq	32(%rbx), %r13
	movq	16(%r13), %r12
	movsd	 %xmm14, %xmm7
	mulsd	 (%r12), %xmm7
	movq	32(%rbx), %r15
	movq	8(%r15), %r14
	movsd	 %xmm13, %xmm8
	mulsd	 (%r14), %xmm8
	movq	32(%rbx), %rdx
	movq	(%rdx), %rcx
	movsd	 %xmm12, %xmm12
	mulsd	 (%rcx), %xmm12
	addsd	 %xmm8, %xmm12
	/* %f9214.d := fadd.d(fadd.d(fmul.d(%f9477.d,mem.d[(mem.64[(mem.64[%r9210.64 +.64 32]) +.64 0]) +.64 0]),fmul.d(%f9480.d,mem.d[(mem.64[(mem.64[%r9210.64 +.64 32]) +.64 8]) +.64 0])),fmul.d(%f9483.d,mem.d[(mem.64[(mem.64[%r9210.64 +.64 32]) +.64 16]) +.64 0])) */
	addsd	 %xmm7, %xmm12
else.615:
	/* block else<FF65> (letJoinK<FF64>,_t<FF63>,_t<FF62>) */
	sqrtsd	 %xmm15, %xmm10
	movabsq	$signBit6455D, %rbx
	movsd	 (%rbx), %xmm11
	xorpd	 %xmm12, %xmm11
	movsd	 %xmm11, %xmm9
	subsd	 %xmm10, %xmm9
	movq	$10, -8(%rsi)
	movsd	 %xmm9, (%rsi)
	movq	%rsi, %r13
	addq	$16, %rsi
	sqrtsd	 %xmm15, %xmm13
	movabsq	$signBit6455D, %r12
	movsd	 (%r12), %xmm14
	xorpd	 %xmm12, %xmm14
	movsd	 %xmm14, %xmm12
	addsd	 %xmm13, %xmm12
	movq	$10, -8(%rsi)
	movsd	 %xmm12, (%rsi)
	movq	%rsi, %r12
	addq	$16, %rsi
	movabsq	$flt4D3, %r14
	ucomisd	 (%r14), %xmm9
	jb	L_true617
else.618:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block else<FF85> (letJoinK<FF84>,res<FF83>) */
	movq	%r10, %rdi
	movq	$3, %r8
	movq	%r13, %r9
	jmp	letJoinK.587
L626:
	cmpq	$5, %r14
	jne	L627
S_case5FB:
case.5FC:
	/* block case<FF8B> (ep<FF88>,letJoinK<FF8A>,_anon_<FF89>) */
	movq	24(%rbx), %rdx
	movq	(%rdx), %rcx
	movsd	 (%rcx), %xmm4
	subsd	 8(%r13), %xmm4
	movq	24(%rbx), %r14
	movq	8(%r14), %r12
	movsd	 (%r12), %xmm5
	subsd	 16(%r13), %xmm5
	movq	24(%rbx), %rcx
	movq	16(%rcx), %r15
	movsd	 (%r15), %xmm6
	subsd	 24(%r13), %xmm6
	movq	32(%rbx), %r12
	movq	(%r12), %rdx
	movq	32(%rbx), %r14
	movq	8(%r14), %r13
	movq	32(%rbx), %rcx
	movq	16(%rcx), %r15
	movsd	 %xmm6, %xmm8
	mulsd	 (%r15), %xmm8
	movsd	 %xmm5, %xmm9
	mulsd	 (%r13), %xmm9
	movsd	 %xmm4, %xmm7
	mulsd	 (%rdx), %xmm7
	addsd	 %xmm9, %xmm7
	subsd	 %xmm8, %xmm7
	movsd	 %xmm6, %xmm13
	mulsd	 %xmm6, %xmm13
	movsd	 %xmm5, %xmm14
	mulsd	 %xmm5, %xmm14
	movsd	 %xmm4, %xmm12
	mulsd	 %xmm4, %xmm12
	addsd	 %xmm14, %xmm12
	subsd	 %xmm13, %xmm12
	movsd	 (%r15), %xmm0
	mulsd	 (%r15), %xmm0
	movsd	 (%r13), %xmm1
	mulsd	 (%r13), %xmm1
	movsd	 (%rdx), %xmm15
	mulsd	 (%rdx), %xmm15
	addsd	 %xmm1, %xmm15
	subsd	 %xmm0, %xmm15
	movabsq	$flt60A, %rdx
	movsd	 (%rdx), %xmm11
	mulsd	 %xmm15, %xmm11
	mulsd	 %xmm12, %xmm11
	movsd	 %xmm7, %xmm10
	mulsd	 %xmm7, %xmm10
	subsd	 %xmm11, %xmm10
	movabsq	$flt4D3, %r12
	ucomisd	 (%r12), %xmm10
	jb	L_true60B
	movsd	 %xmm10, %xmm7
	movq	32(%rbx), %r14
	movq	16(%r14), %r13
	movsd	 %xmm6, %xmm2
	mulsd	 (%r13), %xmm2
	movq	32(%rbx), %rcx
	movq	8(%rcx), %r15
	movsd	 %xmm5, %xmm3
	mulsd	 (%r15), %xmm3
	movq	32(%rbx), %rbx
	movq	(%rbx), %rdx
	movsd	 %xmm4, %xmm10
	mulsd	 (%rdx), %xmm10
	addsd	 %xmm3, %xmm10
	/* %f9199.d := fadd.d(fadd.d(fmul.d(%f9362.d,mem.d[(mem.64[(mem.64[%r9195.64 +.64 32]) +.64 0]) +.64 0]),fmul.d(%f9365.d,mem.d[(mem.64[(mem.64[%r9195.64 +.64 32]) +.64 8]) +.64 0])),fmul.d(%f9368.d,mem.d[(mem.64[(mem.64[%r9195.64 +.64 32]) +.64 16]) +.64 0])) */
	addsd	 %xmm2, %xmm10
else.60C:
	/* block else<FFD9> (letJoinK<FFD8>,_t<FFD7>,_t<FFD6>) */
	sqrtsd	 %xmm7, %xmm5
	movabsq	$signBit6455D, %r12
	movsd	 (%r12), %xmm6
	xorpd	 %xmm10, %xmm6
	movsd	 %xmm6, %xmm4
	subsd	 %xmm5, %xmm4
	movq	$10, -8(%rsi)
	movsd	 %xmm4, (%rsi)
	movq	%rsi, %r15
	addq	$16, %rsi
	sqrtsd	 %xmm7, %xmm8
	movabsq	$signBit6455D, %r13
	movsd	 (%r13), %xmm9
	xorpd	 %xmm10, %xmm9
	movsd	 %xmm9, %xmm7
	addsd	 %xmm8, %xmm7
	movq	$10, -8(%rsi)
	movsd	 %xmm7, (%rsi)
	movq	%rsi, %r14
	addq	$16, %rsi
	movabsq	$flt4D3, %rcx
	ucomisd	 (%rcx), %xmm4
	jb	L_true60E
else.60F:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block else<FFF9> (letJoinK<FFF8>,res<FFF7>) */
	movq	%r10, %rdi
	movq	$3, %r8
	movq	%r15, %r9
	jmp	letJoinK.587
L627:
	cmpq	$3, %r14
	jne	L628
S_case5FD:
case.5FE:
	/* Liveout:  GP={%r12 %r10 %r9 %r8 %rdi}  */
	/* block case<10000> (ep<FFFC>,_exh<FFFF>,letJoinK<FFFE>,_anon_<FFFD>) */
	movq	$20, -8(%rsi)
	movq	24(%rbx), %r15
	movq	%r15, (%rsi)
	movq	32(%rbx), %rcx
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	$133, -8(%rsi)
	movq	%r14, (%rsi)
	movabsq	$anon.5DB, %rdx
	movq	%rdx, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	$7949, -8(%rsi)
	movabsq	$letJoinK.5E5, %r14
	movq	%r14, (%rsi)
	movq	(%rbx), %r15
	movq	%r15, 8(%rsi)
	movq	8(%rbx), %rdx
	movq	%rdx, 16(%rsi)
	movq	16(%rbx), %r14
	movq	%r14, 24(%rsi)
	movq	%r12, 32(%rsi)
	movq	%r10, 40(%rsi)
	movq	%rsi, %r10
	addq	$56, %rsi
	movq	(%rbx), %r15
	movq	(%r15), %rdi
	movq	%rcx, %r8
	movq	8(%r13), %r9
	jmp	map_uncurried.5C
L_true60B:
then.60D:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block then<FFD0> (letJoinK<FFCF>) */
	movq	$10, -8(%rsi)
	movabsq	$flt4D3, %r14
	movsd	 (%r14), %xmm11
	movsd	 %xmm11, (%rsi)
	movq	%rsi, %r13
	addq	$16, %rsi
	movq	%r10, %rdi
	movq	$1, %r8
	movq	%r13, %r9
	jmp	letJoinK.587
L_true60E:
	movsd	 %xmm7, %xmm8
then.610:
	/* block then<FFE7> (letJoinK<FFE6>,_t<FFE5>,res<FFE4>) */
	movabsq	$flt4D3, %rdx
	ucomisd	 (%rdx), %xmm8
	jae	L629
L_true611:
then.613:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block then<FFEB> (letJoinK<FFEA>) */
	movq	$10, -8(%rsi)
	movabsq	$flt4D3, %r12
	movsd	 (%r12), %xmm10
	movsd	 %xmm10, (%rsi)
	movq	%rsi, %rbx
	addq	$16, %rsi
	movq	%r10, %rdi
	movq	$1, %r8
	movq	%rbx, %r9
	jmp	letJoinK.587
L629:
else.612:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block else<FFF3> (letJoinK<FFF2>,res<FFF1>) */
	movq	%r10, %rdi
	movq	$3, %r8
	movq	%r14, %r9
	jmp	letJoinK.587
L_true614:
then.616:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block then<FF5C> (letJoinK<FF5B>) */
	movq	$10, -8(%rsi)
	movabsq	$flt4D3, %r12
	movsd	 (%r12), %xmm0
	movsd	 %xmm0, (%rsi)
	movq	%rsi, %rbx
	addq	$16, %rsi
	movq	%r10, %rdi
	movq	$1, %r8
	movq	%rbx, %r9
	jmp	letJoinK.587
L_true617:
	movsd	 %xmm12, %xmm0
then.619:
	/* block then<FF73> (letJoinK<FF72>,_t<FF71>,res<FF70>) */
	movabsq	$flt4D3, %r15
	ucomisd	 (%r15), %xmm0
	jae	L62A
L_true61A:
then.61C:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block then<FF77> (letJoinK<FF76>) */
	movq	$10, -8(%rsi)
	movabsq	$flt4D3, %rdx
	movsd	 (%rdx), %xmm15
	movsd	 %xmm15, (%rsi)
	movq	%rsi, %rcx
	addq	$16, %rsi
	movq	%r10, %rdi
	movq	$1, %r8
	movq	%rcx, %r9
	jmp	letJoinK.587
L62A:
else.61B:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block else<FF7F> (letJoinK<FF7E>,res<FF7D>) */
	movq	%r10, %rdi
	movq	$3, %r8
	movq	%r12, %r9
	jmp	letJoinK.587
doGC61F:
	movq	$36, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %rcx
	addq	$40, %rsi
	movq	%rcx, %rdi
	movabsq	$retGC61D, %r8
	jmp	ASM_InvokeGC
	/* live= GP={%r12 %rcx %rdx %rbx} spilled=  */
retGC61D:
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
	jmp	gcTest61E
	.text
min.62F:
	movsd	 %xmm3, %xmm3
	movq	%r9, %rcx
	movsd	 %xmm2, %xmm2
	movq	%r8, %rdx
	movq	%rdi, %rbx
	movsd	 %xmm3, %xmm5
	movsd	 %xmm2, %xmm4
	jmp	gcTest631
	/* live= GP={%r12 %r10 %rcx %rdx %rbx} FP={%xmm1 %xmm0} spilled=  */
retGC630:
	movq	48(%rdi), %r12
	movq	40(%rdi), %r10
	movq	24(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
	/* %f9656.d := mem.d[%rdi.64 +.64 32] */
	movsd	 32(%rdi), %xmm5
	/* %f9654.d := mem.d[%rdi.64 +.64 16] */
	movsd	 16(%rdi), %xmm4
gcTest631:
	movq	%r11, %r15
	subq	%rsi, %r15
	jle	doGC632
	movsd	 %xmm5, %xmm1
	movsd	 %xmm4, %xmm0
check.62B:
	/* block check<10D5F> (ep<1009E>,_anon_<1009F>,_t<100A0>,_anon_<100A1>,_t<100A2>,retK<100A3>,_exh<100A4>) */
	ucomisd	 %xmm1, %xmm0
	jae	L633
L_true62C:
then.62E:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block then<100AB> (retK<100AA>,_anon_<100A9>) */
	movq	(%r10), %r13
	movq	%r10, %rdi
	movq	%rdx, %r8
	jmp	*%r13
doGC632:
	movq	$13583, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movsd	 %xmm4, 16(%rsi)
	movq	%rcx, 24(%rsi)
	movsd	 %xmm5, 32(%rsi)
	movq	%r10, 40(%rsi)
	movq	%r12, 48(%rsi)
	movq	%rsi, %r14
	addq	$64, %rsi
	movq	%r14, %rdi
	movabsq	$retGC630, %r8
	jmp	ASM_InvokeGC
L633:
else.62D:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block else<100B0> (retK<100AF>,_anon_<100AE>) */
	movq	(%r10), %r12
	movq	%r10, %rdi
	movq	%rcx, %r8
	jmp	*%r12
	.text
letJoinK.635:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest637
	/* live= GP={%rcx %rdx} spilled=  */
retGC636:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest637:
	movq	%r11, %r14
	subq	%rsi, %r14
	jle	doGC638
check.634:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block check<10D62> (ep<100B4>,_tpl<100B2>) */
	movq	8(%rdx), %rbx
	movq	(%rbx), %r12
	movq	%rbx, %rdi
	movq	$3, %r8
	movq	(%rcx), %r9
	movq	8(%rcx), %r10
	jmp	*%r12
doGC638:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	%r13, %rdi
	movabsq	$retGC636, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.641:
	movq	%r8, %rcx
	movq	%rdi, %rdx
gcTest643:
	movq	%r11, %r15
	subq	%rsi, %r15
	jle	doGC644
check.639:
	/* block check<10D65> (ep<10092>,dists<1008F>) */
	cmpq	$1, %rcx
	jne	L_true63A
else.63B:
	/* block else<100C1> (ep<100C0>) */
	cmpq	$1, 40(%rdx)
	jne	L_true63D
else.63E:
	/* Liveout:  GP={%rax %rdi}  */
	/* block else<100CF> (ep<100CE>) */
	movq	$133, -8(%rsi)
	movabsq	$str640, %r14
	movq	%r14, (%rsi)
	movl	$7, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movabsq	$tag74, %r15
	movq	%r15, (%rsi)
	movq	%r13, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	32(%rdx), %rbx
	movq	(%rbx), %rcx
	movq	%r10, %rax
	movq	%rbx, %rdi
	jmp	*%rcx
L_true63D:
	movq	$1, %r12
then.63F:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block then<100C7> (ep<100C5>,con_false<100C6>) */
	movq	24(%rdx), %rbx
	movq	(%rbx), %r13
	movq	%rbx, %rdi
	movq	%r12, %r8
	movq	16(%rdx), %r9
	movq	40(%rdx), %r14
	movq	(%r14), %r10
	jmp	*%r13
doGC644:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	%r14, %rdi
	movabsq	$retGC642, %r8
	jmp	ASM_InvokeGC
	/* live= GP={%rcx %rdx} spilled=  */
retGC642:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
	jmp	gcTest643
L_true63A:
then.63C:
	/* Liveout:  GP={%r13 %r12 %r10 %r9 %r8 %rdi}  */
	/* block then<10098> (ep<10096>,dists<10097>) */
	movq	$18, -8(%rsi)
	movq	$1, (%rsi)
	movabsq	$min.62F, %rbx
	movq	%rbx, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	$261, -8(%rsi)
	movabsq	$letJoinK.635, %r10
	movq	%r10, (%rsi)
	movq	24(%rdx), %r12
	movq	%r12, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	8(%rdx), %r13
	movq	(%r13), %rdi
	movq	%r15, %r8
	movq	(%rcx), %r9
	movq	8(%rcx), %r10
	movq	32(%rdx), %r13
	jmp	foldr_uncurried.40
	.text
trace.646:
	movq	%r9, %rbx
	movq	%r8, %rdx
	movq	%rdi, %rcx
	jmp	gcTest648
	/* live= GP={%r13 %r12 %r10 %rbx %rdx %rcx} spilled=  */
retGC647:
	movq	40(%rdi), %r13
	movq	32(%rdi), %r12
	movq	24(%rdi), %r10
	movq	16(%rdi), %rbx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rcx
gcTest648:
	movq	%r11, %r14
	subq	%rsi, %r14
	jle	doGC649
check.645:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block check<10D6C> (ep<FEAC>,spheres<FEAD>,pos<FEAE>,dir<FEAF>,retK<FEB0>,_exh<FEB1>) */
	movq	$44, -8(%rsi)
	movq	8(%rcx), %r14
	movq	%r14, (%rsi)
	movq	24(%rcx), %r15
	movq	%r15, 8(%rsi)
	movq	32(%rcx), %r14
	movq	%r14, 16(%rsi)
	movq	%rbx, 24(%rsi)
	movq	%r10, 32(%rsi)
	movq	%rsi, %r10
	addq	$48, %rsi
	movq	$133, -8(%rsi)
	movq	%r10, (%rsi)
	movabsq	$sphmap.5D5, %r15
	movq	%r15, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	$7949, -8(%rsi)
	movabsq	$letJoinK.641, %rbx
	movq	%rbx, (%rsi)
	movq	(%rcx), %r10
	movq	%r10, 8(%rsi)
	movq	16(%rcx), %r15
	movq	%r15, 16(%rsi)
	movq	%r12, 24(%rsi)
	movq	%r13, 32(%rsi)
	movq	%rdx, 40(%rsi)
	movq	%rsi, %rcx
	addq	$56, %rsi
	movq	(%r14), %rdi
	movq	%rdx, %r8
	movq	%rcx, %r9
	movq	%r13, %r10
	jmp	sphmap.5D5
doGC649:
	movq	$52, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rbx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%r12, 32(%rsi)
	movq	%r13, 40(%rsi)
	movq	%rsi, %rcx
	addq	$56, %rsi
	movq	%rcx, %rdi
	movabsq	$retGC647, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.650:
	movsd	 %xmm4, %xmm8
	movsd	 %xmm3, %xmm7
	movsd	 %xmm2, %xmm6
	movq	%rdi, %rbx
	movsd	 %xmm8, %xmm1
	movsd	 %xmm7, %xmm0
	movsd	 %xmm6, %xmm15
	jmp	gcTest652
	/* live= GP={%rbx} FP={%xmm11 %xmm10 %xmm9} spilled=  */
retGC651:
	movq	(%rdi), %rbx
	/* %f9914.d := mem.d[%rdi.64 +.64 24] */
	movsd	 24(%rdi), %xmm1
	/* %f9913.d := mem.d[%rdi.64 +.64 16] */
	movsd	 16(%rdi), %xmm0
	/* %f9912.d := mem.d[%rdi.64 +.64 8] */
	movsd	 8(%rdi), %xmm15
gcTest652:
	movq	%r11, %r12
	subq	%rsi, %r12
	jle	doGC653
	movsd	 %xmm1, %xmm11
	movsd	 %xmm0, %xmm10
	movsd	 %xmm15, %xmm9
check.64A:
	/* block check<10D71> (ep<10230>,_t<10225>,_t<10226>,_t<10227>) */
	movq	40(%rbx), %r13
	movq	16(%r13), %r12
	movsd	 24(%rbx), %xmm15
	mulsd	 (%r12), %xmm15
	movq	40(%rbx), %r15
	movq	8(%r15), %r14
	movsd	 16(%rbx), %xmm0
	mulsd	 (%r14), %xmm0
	movq	40(%rbx), %r13
	movq	(%r13), %r12
	movsd	 8(%rbx), %xmm14
	mulsd	 (%r12), %xmm14
	addsd	 %xmm0, %xmm14
	addsd	 %xmm15, %xmm14
	movabsq	$signBit6455D, %r14
	movsd	 (%r14), %xmm13
	xorpd	 %xmm14, %xmm13
	movsd	 %xmm13, %xmm12
	movsd	 48(%rbx), %xmm3
	movabsq	$signBit6455D, %r15
	movsd	 (%r15), %xmm2
	xorpd	 %xmm3, %xmm2
	movsd	 %xmm2, %xmm1
	movq	56(%rbx), %r13
	movq	(%r13), %r12
	movq	64(%rbx), %r15
	movq	(%r15), %r14
	movsd	 %xmm1, %xmm6
	mulsd	 (%r14), %xmm6
	mulsd	 (%r12), %xmm6
	movq	56(%rbx), %r13
	movq	8(%r13), %r12
	movq	64(%rbx), %r15
	movq	8(%r15), %r14
	movsd	 %xmm1, %xmm7
	mulsd	 (%r14), %xmm7
	mulsd	 (%r12), %xmm7
	movq	56(%rbx), %r13
	movq	16(%r13), %r12
	movq	64(%rbx), %r15
	movq	16(%r15), %r14
	movsd	 %xmm1, %xmm8
	mulsd	 (%r14), %xmm8
	mulsd	 (%r12), %xmm8
	movabsq	$flt4D3, %r12
	ucomisd	 (%r12), %xmm12
	jbe	L_true64B
	movsd	 %xmm11, -120(%rbp)
	movsd	 %xmm10, -112(%rbp)
	movsd	 %xmm9, -104(%rbp)
	movsd	 %xmm8, -144(%rbp)
	movsd	 %xmm7, -128(%rbp)
	movsd	 %xmm6, -136(%rbp)
	movsd	 %xmm12, %xmm12
else.64C:
	/* block else<10281> (ep<10279>,_t<10280>,_t<1027F>,_t<1027E>,_t<1027D>,_t<1027C>,_t<1027B>,_t<1027A>) */
	movq	$-1048576, %r15
	andq	%rsi, %r15
	movq	$1, (%r15)
	movq	%rax, -56(%rbp)
	movq	%rcx, %r13
	movq	%rdx, -64(%rbp)
	movq	%rsi, %r14
	movq	%rdi, %r12
	movq	%r8, -96(%rbp)
	movq	%r9, -80(%rbp)
	movq	%r10, -88(%rbp)
	movq	%r11, -72(%rbp)
	movsd	 %xmm12, %xmm9
	movsd	 %xmm9, %xmm0
	movq	72(%rbx), %rcx
	movsd	 (%rcx), %xmm10
	movsd	 %xmm10, %xmm1
	call	M_Pow
	movsd	 %xmm0, %xmm11
	movq	-56(%rbp), %rax
	movq	%r13, %rcx
	movq	-64(%rbp), %rdx
	movq	%r14, %rsi
	movq	%r12, %rdi
	movq	-96(%rbp), %r8
	movq	-80(%rbp), %r9
	movq	-88(%rbp), %r10
	movq	-72(%rbp), %r11
	movq	$3, (%r15)
	movsd	 -144(%rbp), %xmm15
	movsd	 -128(%rbp), %xmm14
	movsd	 -136(%rbp), %xmm13
	movq	56(%rbx), %r10
	movq	16(%r10), %rdx
	movsd	 %xmm11, %xmm2
	mulsd	 -120(%rbp), %xmm2
	/* %f9799.d := fmul.d(fmul.d(%f9866.d,%f9792.d),mem.d[(mem.64[(mem.64[%r9785.64 +.64 56]) +.64 16]) +.64 0]) */
	mulsd	 (%rdx), %xmm2
	movq	56(%rbx), %r13
	movq	8(%r13), %r12
	movsd	 %xmm11, %xmm1
	mulsd	 -112(%rbp), %xmm1
	/* %f9798.d := fmul.d(fmul.d(%f9866.d,%f9791.d),mem.d[(mem.64[(mem.64[%r9785.64 +.64 56]) +.64 8]) +.64 0]) */
	mulsd	 (%r12), %xmm1
	movq	56(%rbx), %r15
	movq	(%r15), %r14
	movsd	 %xmm11, %xmm0
	mulsd	 -104(%rbp), %xmm0
	/* %f9797.d := fmul.d(fmul.d(%f9866.d,%f9790.d),mem.d[(mem.64[(mem.64[%r9785.64 +.64 56]) +.64 0]) +.64 0]) */
	mulsd	 (%r14), %xmm0
	jmp	letJoinK.64F
doGC653:
	movq	$137, -8(%rsi)
	movq	%rbx, (%rsi)
	movsd	 %xmm15, 8(%rsi)
	movsd	 %xmm0, 16(%rsi)
	movsd	 %xmm1, 24(%rsi)
	movq	%rsi, %rcx
	addq	$40, %rsi
	movq	%rcx, %rdi
	movabsq	$retGC651, %r8
	jmp	ASM_InvokeGC
L_true64B:
	movsd	 %xmm8, %xmm5
	movsd	 %xmm7, %xmm4
	movsd	 %xmm6, %xmm3
then.64D:
	/* block then<10275> (ep<10271>,_t<10274>,_t<10273>,_t<10272>) */
	movsd	 %xmm5, %xmm15
	movsd	 %xmm4, %xmm14
	movsd	 %xmm3, %xmm13
	movabsq	$flt4D3, %r13
	/* %f9799.d := mem.d[flt4D3] */
	movsd	 (%r13), %xmm2
	movabsq	$flt4D3, %r14
	/* %f9798.d := mem.d[flt4D3] */
	movsd	 (%r14), %xmm1
	movabsq	$flt4D3, %r15
	/* %f9797.d := mem.d[flt4D3] */
	movsd	 (%r15), %xmm0
letJoinK.64F:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block letJoinK<10267> (ep<10260>,_t<10266>,_t<10265>,_t<10264>,_t<10261>,_t<10262>,_t<10263>) */
	movq	$10, -8(%rsi)
	movsd	 %xmm13, %xmm12
	addsd	 %xmm0, %xmm12
	movsd	 %xmm12, (%rsi)
	movq	%rsi, %rcx
	addq	$16, %rsi
	movq	$10, -8(%rsi)
	movsd	 %xmm14, %xmm13
	addsd	 %xmm1, %xmm13
	movsd	 %xmm13, (%rsi)
	movq	%rsi, %rdx
	addq	$16, %rsi
	movq	$10, -8(%rsi)
	movsd	 %xmm15, %xmm14
	addsd	 %xmm2, %xmm14
	movsd	 %xmm14, (%rsi)
	movq	%rsi, %r10
	addq	$16, %rsi
	movq	$28, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%r10, 16(%rsi)
	movq	%rsi, %r12
	addq	$32, %rsi
	movq	32(%rbx), %rdi
	movq	%r12, %r8
	jmp	letJoinK.46
	.text
letJoinK.659:
	movq	%r10, %r13
	movq	%r9, %r14
	movq	%r8, %rbx
	movq	%rdi, %r15
	jmp	gcTest65B
	/* live= GP={%r13 %r14 %r15} spilled= GP={%r~1}  */
retGC65A:
	movq	24(%rdi), %r13
	movq	16(%rdi), %r14
	movq	8(%rdi), %rbx
	movq	(%rdi), %r15
gcTest65B:
	movq	%r11, %r12
	subq	%rsi, %r12
	jle	doGC65C
	movq	%rbx, -72(%rbp)
check.654:
	/* block check<10D76> (ep<102A8>,x<1029D>,y<1029E>,z<1029F>) */
	movq	40(%r15), %r12
	movq	16(%r12), %rbx
	movsd	 24(%r15), %xmm5
	mulsd	 (%rbx), %xmm5
	movq	40(%r15), %r12
	movq	8(%r12), %rbx
	movsd	 16(%r15), %xmm6
	mulsd	 (%rbx), %xmm6
	movq	40(%r15), %r12
	movq	(%r12), %rbx
	movsd	 8(%r15), %xmm4
	mulsd	 (%rbx), %xmm4
	addsd	 %xmm6, %xmm4
	addsd	 %xmm5, %xmm4
	movq	56(%r15), %rbx
	movq	(%rbx), %rbx
	movq	64(%r15), %r12
	movq	(%r12), %r12
	movsd	 48(%r15), %xmm7
	mulsd	 (%r12), %xmm7
	mulsd	 (%rbx), %xmm7
	movq	56(%r15), %rbx
	movq	8(%rbx), %rbx
	movq	64(%r15), %r12
	movq	8(%r12), %r12
	movsd	 48(%r15), %xmm8
	mulsd	 (%r12), %xmm8
	mulsd	 (%rbx), %xmm8
	movq	56(%r15), %rbx
	movq	16(%rbx), %rbx
	movq	64(%r15), %r12
	movq	16(%r12), %r12
	movsd	 48(%r15), %xmm9
	mulsd	 (%r12), %xmm9
	mulsd	 (%rbx), %xmm9
	movabsq	$flt4D3, %rbx
	ucomisd	 (%rbx), %xmm4
	jbe	L_true655
	movq	%r13, -64(%rbp)
	movq	%r14, -56(%rbp)
	movsd	 %xmm9, -136(%rbp)
	movsd	 %xmm8, -144(%rbp)
	movsd	 %xmm7, -128(%rbp)
	movsd	 %xmm4, %xmm10
else.656:
	/* block else<102F9> (ep<102F1>,_t<102F8>,_t<102F7>,_t<102F6>,_t<102F5>,x<102F4>,y<102F3>,z<102F2>) */
	movq	$-1048576, %r12
	andq	%rsi, %r12
	movq	$1, (%r12)
	movq	%rax, %r13
	movq	%rcx, %r14
	movq	%rdx, %rbx
	movq	%rsi, -88(%rbp)
	movq	%rdi, -104(%rbp)
	movq	%r8, -80(%rbp)
	movq	%r9, -112(%rbp)
	movq	%r10, -96(%rbp)
	movq	%r11, -120(%rbp)
	movsd	 %xmm10, %xmm10
	movsd	 %xmm10, %xmm0
	movq	72(%r15), %r10
	movsd	 (%r10), %xmm11
	movsd	 %xmm11, %xmm1
	call	M_Pow
	movsd	 %xmm0, %xmm14
	movq	%r13, %rax
	movq	%r14, %rcx
	movq	%rbx, %rdx
	movq	-88(%rbp), %rsi
	movq	-104(%rbp), %rdi
	movq	-80(%rbp), %r8
	movq	-112(%rbp), %r9
	movq	-96(%rbp), %r10
	movq	-120(%rbp), %r11
	movq	$3, (%r12)
	movsd	 -136(%rbp), %xmm13
	movsd	 -144(%rbp), %xmm12
	movsd	 -128(%rbp), %xmm11
	movq	56(%r15), %r13
	movq	16(%r13), %r12
	movsd	 %xmm14, %xmm0
	movq	-64(%rbp), %r13
	mulsd	 (%r13), %xmm0
	/* %f9938.d := fmul.d(fmul.d(%f9999.d,mem.d[%r9931.64 +.64 0]),mem.d[(mem.64[(mem.64[%r9924.64 +.64 56]) +.64 16]) +.64 0]) */
	mulsd	 (%r12), %xmm0
	movq	56(%r15), %rcx
	movq	8(%rcx), %r14
	movsd	 %xmm14, %xmm15
	movq	-56(%rbp), %rcx
	mulsd	 (%rcx), %xmm15
	/* %f9937.d := fmul.d(fmul.d(%f9999.d,mem.d[%r9930.64 +.64 0]),mem.d[(mem.64[(mem.64[%r9924.64 +.64 56]) +.64 8]) +.64 0]) */
	mulsd	 (%r14), %xmm15
	movq	56(%r15), %rbx
	movq	(%rbx), %rdx
	movsd	 %xmm14, %xmm14
	movq	-72(%rbp), %rbx
	mulsd	 (%rbx), %xmm14
	/* %f9936.d := fmul.d(fmul.d(%f9999.d,mem.d[%r9929.64 +.64 0]),mem.d[(mem.64[(mem.64[%r9924.64 +.64 56]) +.64 0]) +.64 0]) */
	mulsd	 (%rdx), %xmm14
	jmp	letJoinK.658
doGC65C:
	movq	$36, -8(%rsi)
	movq	%r15, (%rsi)
	movq	%rbx, 8(%rsi)
	movq	%r14, 16(%rsi)
	movq	%r13, 24(%rsi)
	movq	%rsi, %r10
	addq	$40, %rsi
	movq	%r10, %rdi
	movabsq	$retGC65A, %r8
	jmp	ASM_InvokeGC
L_true655:
	movsd	 %xmm9, %xmm3
	movsd	 %xmm8, %xmm2
	movsd	 %xmm7, %xmm1
then.657:
	/* block then<102ED> (ep<102E9>,_t<102EC>,_t<102EB>,_t<102EA>) */
	movsd	 %xmm3, %xmm13
	movsd	 %xmm2, %xmm12
	movsd	 %xmm1, %xmm11
	movabsq	$flt4D3, %rcx
	/* %f9938.d := mem.d[flt4D3] */
	movsd	 (%rcx), %xmm0
	movabsq	$flt4D3, %rdx
	/* %f9937.d := mem.d[flt4D3] */
	movsd	 (%rdx), %xmm15
	movabsq	$flt4D3, %rbx
	/* %f9936.d := mem.d[flt4D3] */
	movsd	 (%rbx), %xmm14
letJoinK.658:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block letJoinK<102DF> (ep<102D8>,_t<102DE>,_t<102DD>,_t<102DC>,_t<102D9>,_t<102DA>,_t<102DB>) */
	movq	$10, -8(%rsi)
	movsd	 %xmm11, %xmm1
	addsd	 %xmm14, %xmm1
	movsd	 %xmm1, (%rsi)
	movq	%rsi, %r10
	addq	$16, %rsi
	movq	$10, -8(%rsi)
	movsd	 %xmm12, %xmm2
	addsd	 %xmm15, %xmm2
	movsd	 %xmm2, (%rsi)
	movq	%rsi, %r12
	addq	$16, %rsi
	movq	$10, -8(%rsi)
	movsd	 %xmm13, %xmm3
	addsd	 %xmm0, %xmm3
	movsd	 %xmm3, (%rsi)
	movq	%rsi, %r13
	addq	$16, %rsi
	movq	$28, -8(%rsi)
	movq	%r10, (%rsi)
	movq	%r12, 8(%rsi)
	movq	%r13, 16(%rsi)
	movq	%rsi, %r14
	addq	$32, %rsi
	movq	32(%r15), %rdi
	movq	%r14, %r8
	jmp	letJoinK.46
	.text
letJoinK.505:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest662
	/* live= GP={%rcx %rdx} spilled=  */
retGC661:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest662:
	movq	%r11, %rbx
	subq	%rsi, %rbx
	jle	doGC663
check.65D:
	/* block check<10D79> (ep<1021D>,spow<10211>) */
	movabsq	$flt4D3, %rbx
	movsd	 80(%rdx), %xmm0
	ucomisd	 (%rbx), %xmm0
	ja	L664
L_true65E:
then.660:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block then<10224> (ep<10222>,spow<10223>) */
	movq	$120853, -8(%rsi)
	movabsq	$letJoinK.650, %r14
	movq	%r14, (%rsi)
	movsd	 32(%rdx), %xmm5
	movsd	 %xmm5, 8(%rsi)
	movsd	 40(%rdx), %xmm6
	movsd	 %xmm6, 16(%rsi)
	movsd	 48(%rdx), %xmm7
	movsd	 %xmm7, 24(%rsi)
	movq	56(%rdx), %r15
	movq	%r15, 32(%rsi)
	movq	72(%rdx), %rbx
	movq	%rbx, 40(%rsi)
	movsd	 80(%rdx), %xmm8
	movsd	 %xmm8, 48(%rsi)
	movq	88(%rdx), %r10
	movq	%r10, 56(%rsi)
	movq	96(%rdx), %r12
	movq	%r12, 64(%rsi)
	movq	%rcx, 72(%rsi)
	movq	%rsi, %r13
	addq	$88, %rsi
	movq	16(%rdx), %r14
	movq	(%r14), %rdi
	movq	24(%rdx), %r8
	movq	%r13, %r9
	movq	64(%rdx), %r10
	jmp	bodysurf.54C
doGC663:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	%r15, %rdi
	movabsq	$retGC661, %r8
	jmp	ASM_InvokeGC
L664:
else.65F:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block else<1029C> (ep<1029A>,spow<1029B>) */
	movq	$120853, -8(%rsi)
	movabsq	$letJoinK.659, %r12
	movq	%r12, (%rsi)
	movsd	 32(%rdx), %xmm1
	movsd	 %xmm1, 8(%rsi)
	movsd	 40(%rdx), %xmm2
	movsd	 %xmm2, 16(%rsi)
	movsd	 48(%rdx), %xmm3
	movsd	 %xmm3, 24(%rsi)
	movq	56(%rdx), %r13
	movq	%r13, 32(%rsi)
	movq	72(%rdx), %r14
	movq	%r14, 40(%rsi)
	movsd	 80(%rdx), %xmm4
	movsd	 %xmm4, 48(%rsi)
	movq	88(%rdx), %r15
	movq	%r15, 56(%rsi)
	movq	96(%rdx), %rbx
	movq	%rbx, 64(%rsi)
	movq	%rcx, 72(%rsi)
	movq	%rsi, %r10
	addq	$88, %rsi
	movq	8(%rdx), %r12
	movq	(%r12), %rdi
	movq	24(%rdx), %r8
	movq	%r10, %r9
	movq	64(%rdx), %r10
	jmp	specularsurf.4FB
	.text
letJoinK.4E6:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest667
	/* live= GP={%rcx %rdx} spilled=  */
retGC666:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest667:
	movq	%r11, %r15
	subq	%rsi, %r15
	jle	doGC668
check.665:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block check<10D7C> (ep<1020E>,diff<10202>) */
	movq	$902939, -8(%rsi)
	movabsq	$letJoinK.505, %r10
	movq	%r10, (%rsi)
	movq	8(%rdx), %r12
	movq	%r12, 8(%rsi)
	movq	24(%rdx), %r13
	movq	%r13, 16(%rsi)
	movq	32(%rdx), %r14
	movq	%r14, 24(%rsi)
	movsd	 40(%rdx), %xmm0
	movsd	 %xmm0, 32(%rsi)
	movsd	 48(%rdx), %xmm1
	movsd	 %xmm1, 40(%rsi)
	movsd	 56(%rdx), %xmm2
	movsd	 %xmm2, 48(%rsi)
	movq	64(%rdx), %r15
	movq	%r15, 56(%rsi)
	movq	72(%rdx), %rbx
	movq	%rbx, 64(%rsi)
	movq	80(%rdx), %r10
	movq	%r10, 72(%rsi)
	movsd	 88(%rdx), %xmm3
	movsd	 %xmm3, 80(%rsi)
	movq	96(%rdx), %r12
	movq	%r12, 88(%rsi)
	movq	%rcx, 96(%rsi)
	movq	%rsi, %rbx
	addq	$112, %rsi
	movq	16(%rdx), %r13
	movq	(%r13), %rdi
	movq	32(%rdx), %r8
	movq	%rbx, %r9
	movq	72(%rdx), %r10
	jmp	specpowsurf.509
doGC668:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	%r14, %rdi
	movabsq	$retGC666, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.678:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
gcTest67A:
	movq	%r11, %r12
	subq	%rsi, %r12
	jle	doGC67B
check.669:
	/* block check<10D81> (ep<101F5>,hit<101E8>,unused<101E9>,unused<101EA>) */
	cmpq	$1, %rdx
	jne	L67C
S_case66A:
case.66B:
	/* block case<10333> (ep<10332>) */
	movq	$3, %r13
	jmp	letJoinK.66E
doGC67B:
	movq	$36, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %rbx
	addq	$40, %rsi
	movq	%rbx, %rdi
	movabsq	$retGC679, %r8
	jmp	ASM_InvokeGC
L67C:
	cmpq	$3, %rdx
	jne	S_case66A
S_case66C:
case.66D:
	/* block case<10336> (ep<10335>) */
	movq	$1, %r13
letJoinK.66E:
	/* block letJoinK<101FA> (ep<101F8>,_t<101F9>) */
	cmpq	$1, %r13
	je	S_case66F
	cmpq	$3, %r13
	je	S_case671
S_case66F:
case.670:
	/* block case<1032B> (ep<1032A>) */
	movq	104(%rbx), %r10
	movq	$3, %r12
letJoinK.673:
	/* block letJoinK<101FE> (ep<101FB>,inshadow<101FC>,lcolor<101FD>) */
	cmpq	$1, %r12
	je	S_case674
	cmpq	$3, %r12
	je	S_case676
S_case674:
case.675:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block case<10201> (ep<101FF>,lcolor<10200>) */
	movq	$757531, -8(%rsi)
	movabsq	$letJoinK.4E6, %r15
	movq	%r15, (%rsi)
	movq	16(%rbx), %rcx
	movq	%rcx, 8(%rsi)
	movq	24(%rbx), %rdx
	movq	%rdx, 16(%rsi)
	movq	32(%rbx), %r12
	movq	%r12, 24(%rsi)
	movq	40(%rbx), %r13
	movq	%r13, 32(%rsi)
	movsd	 48(%rbx), %xmm3
	movsd	 %xmm3, 40(%rsi)
	movsd	 56(%rbx), %xmm4
	movsd	 %xmm4, 48(%rsi)
	movsd	 64(%rbx), %xmm5
	movsd	 %xmm5, 56(%rsi)
	movq	72(%rbx), %r14
	movq	%r14, 64(%rsi)
	movq	80(%rbx), %r15
	movq	%r15, 72(%rsi)
	movq	88(%rbx), %rcx
	movq	%rcx, 80(%rsi)
	movsd	 96(%rbx), %xmm6
	movsd	 %xmm6, 88(%rsi)
	movq	%r10, 96(%rsi)
	movq	%rsi, %r14
	addq	$112, %rsi
	movq	8(%rbx), %rdx
	movq	(%rdx), %rdi
	movq	40(%rbx), %r8
	movq	%r14, %r9
	movq	80(%rbx), %r10
	jmp	diffusesurf.4EE
S_case671:
case.672:
	/* block case<1032F> (ep<1032E>) */
	movq	104(%rbx), %r10
	movq	$1, %r12
	jmp	letJoinK.673
S_case676:
case.677:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block case<10320> (ep<1031F>) */
	movq	$10, -8(%rsi)
	movabsq	$flt4D3, %r15
	movsd	 (%r15), %xmm0
	movsd	 %xmm0, (%rsi)
	movq	%rsi, %r14
	addq	$16, %rsi
	movq	$10, -8(%rsi)
	movabsq	$flt4D3, %rdx
	movsd	 (%rdx), %xmm1
	movsd	 %xmm1, (%rsi)
	movq	%rsi, %rcx
	addq	$16, %rsi
	movq	$10, -8(%rsi)
	movabsq	$flt4D3, %r12
	movsd	 (%r12), %xmm2
	movsd	 %xmm2, (%rsi)
	movq	%rsi, %r10
	addq	$16, %rsi
	movq	$28, -8(%rsi)
	movq	%r14, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%r10, 16(%rsi)
	movq	%rsi, %r13
	addq	$32, %rsi
	movq	72(%rbx), %rdi
	movq	%r13, %r8
	jmp	letJoinK.46
	/* live= GP={%r10 %rcx %rdx %rbx} spilled=  */
retGC679:
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
	jmp	gcTest67A
	.text
letJoinK.683:
	movq	%r9, %rcx
	movsd	 %xmm4, %xmm8
	movsd	 %xmm3, %xmm7
	movsd	 %xmm2, %xmm6
	movq	%r8, %r10
	movq	%rdi, %rdx
	movsd	 %xmm8, %xmm14
	movsd	 %xmm7, %xmm13
	movsd	 %xmm6, %xmm12
gcTest685:
	movq	%r11, %r15
	subq	%rsi, %r15
	jle	doGC686
	movsd	 %xmm14, %xmm2
	movsd	 %xmm13, %xmm1
	movsd	 %xmm12, %xmm0
check.67D:
	/* block check<10D88> (ep<101B9>,ldir<101A5>,_t<101A6>,_t<101A7>,_t<101A8>,unused<101A9>) */
	movq	96(%rdx), %r13
	movq	16(%r13), %r12
	movsd	 %xmm2, %xmm10
	mulsd	 (%r12), %xmm10
	movq	96(%rdx), %r15
	movq	8(%r15), %r14
	movsd	 %xmm1, %xmm11
	mulsd	 (%r14), %xmm11
	movq	96(%rdx), %rbx
	movq	(%rbx), %rcx
	movsd	 %xmm0, %xmm9
	mulsd	 (%rcx), %xmm9
	addsd	 %xmm11, %xmm9
	addsd	 %xmm10, %xmm9
	movq	128(%rdx), %r13
	movq	(%r13), %r12
	cmpq	$3, %r12
	je	S_case67E
	cmpq	$1, %r12
	je	S_case680
S_case67E:
	movsd	 %xmm9, %xmm5
case.67F:
	/* block case<10340> (ep<1033D>,ldir<1033F>,_t<1033E>) */
	movq	128(%rdx), %rcx
	movq	$10, -8(%rsi)
	movsd	 32(%rcx), %xmm9
	movsd	 %xmm9, (%rsi)
	movq	%rsi, %rbx
	addq	$16, %rsi
	movq	$10, -8(%rsi)
	movsd	 40(%rcx), %xmm10
	movsd	 %xmm10, (%rsi)
	movq	%rsi, %r12
	addq	$16, %rsi
	movq	$10, -8(%rsi)
	movsd	 48(%rcx), %xmm11
	movsd	 %xmm11, (%rsi)
	movq	%rsi, %r13
	addq	$16, %rsi
	movq	$28, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%r12, 8(%rsi)
	movq	%r13, 16(%rsi)
	movq	%rsi, %rbx
	addq	$32, %rsi
	movsd	 %xmm5, %xmm4
	jmp	letJoinK.682
doGC686:
	movq	$4493, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%r10, 8(%rsi)
	movsd	 %xmm12, 16(%rsi)
	movsd	 %xmm13, 24(%rsi)
	movsd	 %xmm14, 32(%rsi)
	movq	%rcx, 40(%rsi)
	movq	%rsi, %r14
	addq	$56, %rsi
	movq	%r14, %rdi
	movabsq	$retGC684, %r8
	jmp	ASM_InvokeGC
S_case680:
	movsd	 %xmm9, %xmm3
case.681:
	/* block case<1034D> (ep<1034A>,ldir<1034C>,_t<1034B>) */
	movq	128(%rdx), %r14
	movq	$10, -8(%rsi)
	movsd	 32(%r14), %xmm12
	movsd	 %xmm12, (%rsi)
	movq	%rsi, %r15
	addq	$16, %rsi
	movq	$10, -8(%rsi)
	movsd	 40(%r14), %xmm13
	movsd	 %xmm13, (%rsi)
	movq	%rsi, %rcx
	addq	$16, %rsi
	movq	$10, -8(%rsi)
	movsd	 48(%r14), %xmm14
	movsd	 %xmm14, (%rsi)
	movq	%rsi, %rbx
	addq	$16, %rsi
	movq	$28, -8(%rsi)
	movq	%r15, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rbx, 16(%rsi)
	movq	%rsi, %rbx
	addq	$32, %rsi
	movsd	 %xmm3, %xmm4
letJoinK.682:
	/* Liveout:  GP={%r13 %r12 %r10 %r9 %r8 %rdi}  */
	/* block letJoinK<101D1> (ep<101CD>,ldir<101D0>,_t<101CF>,_t<101CE>) */
	movq	$10, -8(%rsi)
	movq	(%r10), %r13
	movsd	 8(%rdx), %xmm0
	mulsd	 (%r13), %xmm0
	movsd	 64(%rdx), %xmm15
	addsd	 %xmm0, %xmm15
	movsd	 %xmm15, (%rsi)
	movq	%rsi, %r12
	addq	$16, %rsi
	movq	$10, -8(%rsi)
	movq	8(%r10), %r15
	movsd	 8(%rdx), %xmm2
	mulsd	 (%r15), %xmm2
	movsd	 72(%rdx), %xmm1
	addsd	 %xmm2, %xmm1
	movsd	 %xmm1, (%rsi)
	movq	%rsi, %r14
	addq	$16, %rsi
	movq	$10, -8(%rsi)
	movq	16(%r10), %r13
	movsd	 8(%rdx), %xmm5
	mulsd	 (%r13), %xmm5
	movsd	 80(%rdx), %xmm3
	addsd	 %xmm5, %xmm3
	movsd	 %xmm3, (%rsi)
	movq	%rsi, %rcx
	addq	$16, %rsi
	movq	$28, -8(%rsi)
	movq	%r12, (%rsi)
	movq	%r14, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%rsi, %r13
	addq	$32, %rsi
	movq	$1515293, -8(%rsi)
	movabsq	$letJoinK.678, %r14
	movq	%r14, (%rsi)
	movq	16(%rdx), %r15
	movq	%r15, 8(%rsi)
	movq	24(%rdx), %rcx
	movq	%rcx, 16(%rsi)
	movq	32(%rdx), %r14
	movq	%r14, 24(%rsi)
	movq	40(%rdx), %r15
	movq	%r15, 32(%rsi)
	movq	88(%rdx), %rcx
	movq	%rcx, 40(%rsi)
	movsd	 104(%rdx), %xmm6
	movsd	 %xmm6, 48(%rsi)
	movsd	 112(%rdx), %xmm7
	movsd	 %xmm7, 56(%rsi)
	movsd	 120(%rdx), %xmm8
	movsd	 %xmm8, 64(%rsi)
	movq	136(%rdx), %r12
	movq	%r12, 72(%rsi)
	movq	144(%rdx), %r14
	movq	%r14, 80(%rsi)
	movq	%r10, 88(%rsi)
	movsd	 %xmm4, 96(%rsi)
	movq	%rbx, 104(%rsi)
	movq	%rsi, %r12
	addq	$120, %rsi
	movq	56(%rdx), %r15
	movq	(%r15), %rdi
	movq	48(%rdx), %r8
	movq	%r13, %r9
	movq	144(%rdx), %r13
	jmp	trace.646
	/* live= GP={%rcx %r10 %rdx} FP={%xmm2 %xmm1 %xmm0} spilled=  */
retGC684:
	movq	40(%rdi), %rcx
	movq	8(%rdi), %r10
	movq	(%rdi), %rdx
	/* %f10389.d := mem.d[%rdi.64 +.64 32] */
	movsd	 32(%rdi), %xmm14
	/* %f10388.d := mem.d[%rdi.64 +.64 24] */
	movsd	 24(%rdi), %xmm13
	/* %f10387.d := mem.d[%rdi.64 +.64 16] */
	movsd	 16(%rdi), %xmm12
	jmp	gcTest685
	.text
letJoinK.688:
	movq	%r9, %rcx
	movsd	 %xmm4, %xmm5
	movsd	 %xmm3, %xmm4
	movsd	 %xmm2, %xmm3
	movq	%r8, %rdx
	movq	%rdi, %rbx
	movsd	 %xmm5, %xmm8
	movsd	 %xmm4, %xmm7
	movsd	 %xmm3, %xmm6
	jmp	gcTest68A
	/* live= GP={%rcx %rdx %rbx} FP={%xmm2 %xmm1 %xmm0} spilled=  */
retGC689:
	movq	40(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
	/* %f10426.d := mem.d[%rdi.64 +.64 32] */
	movsd	 32(%rdi), %xmm8
	/* %f10425.d := mem.d[%rdi.64 +.64 24] */
	movsd	 24(%rdi), %xmm7
	/* %f10424.d := mem.d[%rdi.64 +.64 16] */
	movsd	 16(%rdi), %xmm6
gcTest68A:
	movq	%r11, %r10
	subq	%rsi, %r10
	jle	doGC68B
	movsd	 %xmm8, %xmm2
	movsd	 %xmm7, %xmm1
	movsd	 %xmm6, %xmm0
check.687:
	/* Liveout:  GP={%r9 %r8 %rdi} FP={%xmm4 %xmm3 %xmm2}  */
	/* block check<10D8F> (ep<10376>,d<10370>,unused<10371>,unused<10372>,unused<10373>,unused<10374>) */
	movq	16(%rbx), %r10
	movq	(%r10), %r12
	movq	%r10, %rdi
	movq	%rdx, %r8
	movq	(%rdx), %r13
	/* %xmm2.d := mem.d[(mem.64[%r10397.64 +.64 0]) +.64 0] */
	movsd	 (%r13), %xmm2
	movq	8(%rdx), %r14
	/* %xmm3.d := mem.d[(mem.64[%r10397.64 +.64 8]) +.64 0] */
	movsd	 (%r14), %xmm3
	movq	16(%rdx), %r15
	/* %xmm4.d := mem.d[(mem.64[%r10397.64 +.64 16]) +.64 0] */
	movsd	 (%r15), %xmm4
	movq	8(%rbx), %r9
	jmp	*%r12
doGC68B:
	movq	$4493, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movsd	 %xmm6, 16(%rsi)
	movsd	 %xmm7, 24(%rsi)
	movsd	 %xmm8, 32(%rsi)
	movq	%rcx, 40(%rsi)
	movq	%rsi, %rcx
	addq	$56, %rsi
	movq	%rcx, %rdi
	movabsq	$retGC689, %r8
	jmp	ASM_InvokeGC
	.text
lightray_P_.691:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
gcTest693:
	movq	%r11, %r14
	subq	%rsi, %r14
	jle	doGC694
check.68C:
	/* block check<10D94> (ep<1019C>,l<1019D>,retK<1019E>,_exh<1019F>) */
	movq	$59538983, -8(%rsi)
	movabsq	$letJoinK.683, %r12
	movq	%r12, (%rsi)
	movsd	 (%rbx), %xmm0
	movsd	 %xmm0, 8(%rsi)
	movq	24(%rbx), %r13
	movq	%r13, 16(%rsi)
	movq	32(%rbx), %r14
	movq	%r14, 24(%rsi)
	movq	40(%rbx), %r15
	movq	%r15, 32(%rsi)
	movq	48(%rbx), %r12
	movq	%r12, 40(%rsi)
	movq	56(%rbx), %r13
	movq	%r13, 48(%rsi)
	movq	64(%rbx), %r14
	movq	%r14, 56(%rsi)
	movsd	 72(%rbx), %xmm1
	movsd	 %xmm1, 64(%rsi)
	movsd	 80(%rbx), %xmm2
	movsd	 %xmm2, 72(%rsi)
	movsd	 88(%rbx), %xmm3
	movsd	 %xmm3, 80(%rsi)
	movq	96(%rbx), %r15
	movq	%r15, 88(%rsi)
	movq	104(%rbx), %r12
	movq	%r12, 96(%rsi)
	movsd	 112(%rbx), %xmm4
	movsd	 %xmm4, 104(%rsi)
	movsd	 120(%rbx), %xmm5
	movsd	 %xmm5, 112(%rsi)
	movsd	 128(%rbx), %xmm6
	movsd	 %xmm6, 120(%rsi)
	movq	%rdx, 128(%rsi)
	movq	%rcx, 136(%rsi)
	movq	%r10, 144(%rsi)
	movq	%rsi, %r10
	addq	$160, %rsi
	movq	(%rdx), %r13
	cmpq	$3, %r13
	jne	L695
S_case68D:
case.68E:
	/* Liveout:  GP={%r8 %rdi} FP={%xmm4 %xmm3 %xmm2}  */
	/* block case<1035A> (ep<10357>,l<10359>,letJoinK<10358>) */
	movq	16(%rbx), %r12
	movq	(%r12), %rdi
	movsd	 8(%rdx), %xmm2
	/* %xmm2.d := fsub.d(mem.d[%r10441.64 +.64 8],mem.d[%r10440.64 +.64 72]) */
	subsd	 72(%rbx), %xmm2
	movsd	 16(%rdx), %xmm3
	/* %xmm3.d := fsub.d(mem.d[%r10441.64 +.64 16],mem.d[%r10440.64 +.64 80]) */
	subsd	 80(%rbx), %xmm3
	movsd	 24(%rdx), %xmm4
	/* %xmm4.d := fsub.d(mem.d[%r10441.64 +.64 24],mem.d[%r10440.64 +.64 88]) */
	subsd	 88(%rbx), %xmm4
	movq	%r10, %r8
	jmp	vecnorm.4C1
doGC694:
	movq	$36, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %r13
	addq	$40, %rsi
	movq	%r13, %rdi
	movabsq	$retGC692, %r8
	jmp	ASM_InvokeGC
	/* live= GP={%r10 %rcx %rdx %rbx} spilled=  */
retGC692:
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
	jmp	gcTest693
L695:
	cmpq	$1, %r13
	jne	S_case68D
S_case68F:
case.690:
	/* Liveout:  GP={%r8 %rdi} FP={%xmm4 %xmm3 %xmm2}  */
	/* block case<1036B> (ep<10368>,l<1036A>,letJoinK<10369>) */
	movq	$775, -8(%rsi)
	movabsq	$letJoinK.688, %r15
	movq	%r15, (%rsi)
	movq	8(%rbx), %rcx
	movq	%rcx, 8(%rsi)
	movq	%r10, 16(%rsi)
	movq	%rsi, %r14
	addq	$32, %rsi
	movq	16(%rbx), %rbx
	movq	(%rbx), %rdi
	/* %xmm2.d := mem.d[%r10438.64 +.64 8] */
	movsd	 8(%rdx), %xmm2
	/* %xmm3.d := mem.d[%r10438.64 +.64 16] */
	movsd	 16(%rdx), %xmm3
	/* %xmm4.d := mem.d[%r10438.64 +.64 24] */
	movsd	 24(%rdx), %xmm4
	movq	%r14, %r8
	jmp	vecnorm.4C1
	.text
f.697:
	movsd	 %xmm3, %xmm3
	movq	%r9, %rcx
	movsd	 %xmm2, %xmm2
	movq	%r8, %rdx
	movq	%rdi, %rbx
	movsd	 %xmm3, %xmm8
	movsd	 %xmm2, %xmm7
	jmp	gcTest699
	/* live= GP={%r12 %r10 %rcx %rdx %rbx} FP={%xmm1 %xmm0} spilled=  */
retGC698:
	movq	48(%rdi), %r12
	movq	40(%rdi), %r10
	movq	24(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
	/* %f10559.d := mem.d[%rdi.64 +.64 32] */
	movsd	 32(%rdi), %xmm8
	/* %f10557.d := mem.d[%rdi.64 +.64 16] */
	movsd	 16(%rdi), %xmm7
gcTest699:
	movq	%r11, %r13
	subq	%rsi, %r13
	jle	doGC69A
	movsd	 %xmm8, %xmm1
	movsd	 %xmm7, %xmm0
check.696:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<10D9C> (ep<103A2>,a<103A3>,_t<103A4>,b<103A5>,_t<103A6>,retK<103A7>,_exh<103A8>) */
	movq	$10, -8(%rsi)
	movsd	 %xmm0, %xmm4
	addsd	 %xmm1, %xmm4
	movsd	 %xmm4, (%rsi)
	movq	%rsi, %rbx
	addq	$16, %rsi
	movq	$10, -8(%rsi)
	movq	8(%rcx), %r13
	movq	8(%rdx), %r14
	movsd	 (%r14), %xmm5
	addsd	 (%r13), %xmm5
	movsd	 %xmm5, (%rsi)
	movq	%rsi, %r12
	addq	$16, %rsi
	movq	$10, -8(%rsi)
	movq	16(%rcx), %r13
	movq	16(%rdx), %r14
	movsd	 (%r14), %xmm6
	addsd	 (%r13), %xmm6
	movsd	 %xmm6, (%rsi)
	movq	%rsi, %r15
	addq	$16, %rsi
	movq	$28, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%r12, 8(%rsi)
	movq	%r15, 16(%rsi)
	movq	%rsi, %r15
	addq	$32, %rsi
	movq	(%r10), %rcx
	movq	%r10, %rdi
	movq	%r15, %r8
	jmp	*%rcx
doGC69A:
	movq	$13583, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movsd	 %xmm7, 16(%rsi)
	movq	%rcx, 24(%rsi)
	movsd	 %xmm8, 32(%rsi)
	movq	%r10, 40(%rsi)
	movq	%r12, 48(%rsi)
	movq	%rsi, %rdx
	addq	$64, %rsi
	movq	%rdx, %rdi
	movabsq	$retGC698, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.69C:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest69E
	/* live= GP={%rcx %rdx} spilled=  */
retGC69D:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest69E:
	movq	%r11, %r13
	subq	%rsi, %r13
	jle	doGC69F
check.69B:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<10D9F> (ep<1049F>,rcol<1049D>) */
	movq	8(%rdx), %rbx
	movq	(%rbx), %r10
	movq	%rbx, %rdi
	movq	%rcx, %r8
	jmp	*%r10
doGC69F:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	%r12, %rdi
	movabsq	$retGC69D, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.6A1:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest6A3
	/* live= GP={%rcx %rdx} spilled=  */
retGC6A2:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest6A3:
	movq	%r11, %r12
	subq	%rsi, %r12
	jle	doGC6A4
check.6A0:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<10DA2> (ep<104EF>,newcol<104EB>) */
	movq	$10, -8(%rsi)
	movq	16(%rdx), %r10
	movq	(%r10), %rbx
	movq	(%rcx), %r12
	movsd	 (%r12), %xmm1
	mulsd	 (%rbx), %xmm1
	movq	8(%rdx), %r14
	movq	(%r14), %r13
	movsd	 (%r13), %xmm0
	addsd	 %xmm1, %xmm0
	movsd	 %xmm0, (%rsi)
	movq	%rsi, %r14
	addq	$16, %rsi
	movq	$10, -8(%rsi)
	movq	16(%rdx), %rbx
	movq	8(%rbx), %r15
	movq	8(%rcx), %r10
	movsd	 (%r10), %xmm3
	mulsd	 (%r15), %xmm3
	movq	8(%rdx), %r13
	movq	8(%r13), %r12
	movsd	 (%r12), %xmm2
	addsd	 %xmm3, %xmm2
	movsd	 %xmm2, (%rsi)
	movq	%rsi, %r13
	addq	$16, %rsi
	movq	$10, -8(%rsi)
	movq	16(%rdx), %r10
	movq	16(%r10), %rbx
	movq	16(%rcx), %r12
	movsd	 (%r12), %xmm5
	mulsd	 (%rbx), %xmm5
	movq	8(%rdx), %rcx
	movq	16(%rcx), %r15
	movsd	 (%r15), %xmm4
	addsd	 %xmm5, %xmm4
	movsd	 %xmm4, (%rsi)
	movq	%rsi, %r15
	addq	$16, %rsi
	movq	$28, -8(%rsi)
	movq	%r14, (%rsi)
	movq	%r13, 8(%rsi)
	movq	%r15, 16(%rsi)
	movq	%rsi, %rbx
	addq	$32, %rsi
	movq	24(%rdx), %rdi
	movq	%rbx, %r8
	jmp	letJoinK.69C
doGC6A4:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	%r10, %rdi
	movabsq	$retGC6A2, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.6AB:
	movq	%r9, %rcx
	movq	%r8, %rbx
	movq	%rdi, %rdx
gcTest6AD:
	movq	%r11, %r12
	subq	%rsi, %r12
	jle	doGC6AE
check.6A5:
	/* block check<10DA7> (ep<104E8>,hit<104DC>,dist<104DD>,sp<104DE>) */
	movq	$1801, -8(%rsi)
	movabsq	$letJoinK.6A1, %r12
	movq	%r12, (%rsi)
	movq	72(%rdx), %r13
	movq	%r13, 8(%rsi)
	movq	80(%rdx), %r14
	movq	%r14, 16(%rsi)
	movq	88(%rdx), %r15
	movq	%r15, 24(%rsi)
	movq	%rsi, %r13
	addq	$40, %rsi
	cmpq	$1, %rbx
	jne	L6AF
S_case6A6:
case.6A7:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block case<10518> (ep<10516>,letJoinK<10517>) */
	movq	(%r13), %rbx
	movq	%r13, %rdi
	movq	8(%rdx), %r8
	jmp	*%rbx
doGC6AE:
	movq	$36, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rbx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %r10
	addq	$40, %rsi
	movq	%r10, %rdi
	movabsq	$retGC6AC, %r8
	jmp	ASM_InvokeGC
	/* live= GP={%r10 %rcx %rbx %rdx} spilled=  */
retGC6AC:
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rbx
	movq	(%rdi), %rdx
	jmp	gcTest6AD
L6AF:
	cmpq	$3, %rbx
	jne	S_case6A6
S_case6A8:
case.6A9:
	/* Liveout:  GP={%r15 %r14 %r13 %r12 %r10 %r9 %r8 %rdi} FP={%xmm7 %xmm6 %xmm5 %xmm4
 %xmm3 %xmm2}  */
	/* block case<1051F> (ep<1051B>,dist<1051E>,sp<1051D>,letJoinK<1051C>) */
	movq	$10, -8(%rsi)
	movsd	 (%rcx), %xmm0
	movsd	 %xmm0, (%rsi)
	movq	%rsi, %r15
	addq	$16, %rsi
	movq	16(%rdx), %rcx
	movq	(%rcx), %rdi
	movq	32(%rdx), %r8
	movq	%r10, %r9
	/* %xmm2.d := mem.d[%r10642.64 +.64 104] */
	movsd	 104(%rdx), %xmm2
	/* %xmm3.d := mem.d[%r10642.64 +.64 112] */
	movsd	 112(%rdx), %xmm3
	/* %xmm4.d := mem.d[%r10642.64 +.64 120] */
	movsd	 120(%rdx), %xmm4
	movq	64(%rdx), %r10
	/* %xmm5.d := mem.d[%r10642.64 +.64 40] */
	movsd	 40(%rdx), %xmm5
	/* %xmm6.d := mem.d[%r10642.64 +.64 48] */
	movsd	 48(%rdx), %xmm6
	/* %xmm7.d := mem.d[%r10642.64 +.64 56] */
	movsd	 56(%rdx), %xmm7
	movq	96(%rdx), %r12
	movq	24(%rdx), %r14
	jmp	shade.6AA
	.text
letJoinK.512:
	movsd	 %xmm2, %xmm13
	movq	%rdi, %rcx
	movsd	 %xmm13, %xmm0
	jmp	gcTest6D2
	/* live= GP={%rcx} FP={%xmm0} spilled=  */
retGC6D1:
	movq	(%rdi), %rcx
	/* %f10908.d := mem.d[%rdi.64 +.64 8] */
	movsd	 8(%rdi), %xmm0
gcTest6D2:
	movq	%r11, %r10
	subq	%rsi, %r10
	jle	doGC6D3
	movsd	 %xmm0, %xmm0
check.6B0:
	/* block check<10DAA> (ep<10483>,_t<1046D>) */
	movq	176(%rcx), %r14
	movsd	 %xmm0, %xmm14
	mulsd	 (%r14), %xmm14
	movq	$10, -8(%rsi)
	movsd	 %xmm14, (%rsi)
	movq	%rsi, %r15
	addq	$16, %rsi
	movq	184(%rcx), %rdx
	movsd	 %xmm0, %xmm15
	mulsd	 (%rdx), %xmm15
	movq	$10, -8(%rsi)
	movsd	 %xmm15, (%rsi)
	movq	%rsi, %rbx
	addq	$16, %rsi
	movq	192(%rcx), %r10
	movsd	 %xmm0, %xmm0
	mulsd	 (%r10), %xmm0
	movq	$10, -8(%rsi)
	movsd	 %xmm0, (%rsi)
	movq	%rsi, %r12
	addq	$16, %rsi
	movq	$28, -8(%rsi)
	movq	%r15, (%rsi)
	movq	%rbx, 8(%rsi)
	movq	%r12, 16(%rsi)
	movq	%rsi, %r10
	addq	$32, %rsi
	movq	160(%rcx), %r13
	cmpq	$1, %r13
	jne	L6D4
S_case6B1:
	movsd	 %xmm0, %xmm6
	movsd	 %xmm15, %xmm5
	movsd	 %xmm14, %xmm4
	jmp	letJoinK.6B2
doGC6D3:
	movq	$133, -8(%rsi)
	movq	%rcx, (%rsi)
	movsd	 %xmm0, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	%rbx, %rdi
	movabsq	$retGC6D1, %r8
	jmp	ASM_InvokeGC
L6D4:
	cmpq	$3, %r13
	jne	S_case6B1
S_case6B3:
	movsd	 %xmm0, %xmm3
	movsd	 %xmm15, %xmm2
	movsd	 %xmm14, %xmm1
case.6B4:
	/* block case<1058D> (ep<10589>,_t<1058C>,_t<1058B>,_t<1058A>) */
	movsd	 %xmm1, %xmm1
	addsd	 136(%rcx), %xmm1
	movq	$10, -8(%rsi)
	movsd	 %xmm1, (%rsi)
	movq	%rsi, %r14
	addq	$16, %rsi
	movsd	 %xmm2, %xmm2
	addsd	 144(%rcx), %xmm2
	movq	$10, -8(%rsi)
	movsd	 %xmm2, (%rsi)
	movq	%rsi, %r15
	addq	$16, %rsi
	movsd	 %xmm3, %xmm3
	addsd	 152(%rcx), %xmm3
	movq	$10, -8(%rsi)
	movsd	 %xmm3, (%rsi)
	movq	%rsi, %rdx
	addq	$16, %rsi
	movq	$28, -8(%rsi)
	movq	%r14, (%rsi)
	movq	%r15, 8(%rsi)
	movq	%rdx, 16(%rsi)
	movq	%rsi, %r10
	addq	$32, %rsi
	movsd	 %xmm3, %xmm6
	movsd	 %xmm2, %xmm5
	movsd	 %xmm1, %xmm4
letJoinK.6B2:
	/* block letJoinK<10498> (ep<10493>,reflectiv<10494>,_t<10495>,_t<10496>,_t<10497>) */
	ucomisd	 8(%rcx), %xmm6
	jb	L_true6B5
else.6B6:
	/* block else<10580> (ep<1057E>,reflectiv<1057F>) */
	movq	$1, %r13
letJoinK.6B8:
	/* block letJoinK<1049C> (ep<10499>,reflectiv<1049B>,_t<1049A>) */
	movq	$261, -8(%rsi)
	movabsq	$letJoinK.69C, %rbx
	movq	%rbx, (%rsi)
	movq	48(%rcx), %r12
	movq	%r12, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	cmpq	$1, %r13
	jne	L6D5
S_case6B9:
case.6BA:
	/* block case<104A7> (ep<104A4>,reflectiv<104A6>,letJoinK<104A5>) */
	movq	72(%rcx), %r14
	movq	(%r14), %r13
	movq	(%r10), %r15
	movsd	 (%r15), %xmm4
	mulsd	 (%r13), %xmm4
	movq	$10, -8(%rsi)
	movsd	 %xmm4, (%rsi)
	movq	%rsi, %r15
	addq	$16, %rsi
	movq	72(%rcx), %r12
	movq	8(%r12), %rdx
	movq	8(%r10), %r13
	movsd	 (%r13), %xmm5
	mulsd	 (%rdx), %xmm5
	movq	$10, -8(%rsi)
	movsd	 %xmm5, (%rsi)
	movq	%rsi, %r13
	addq	$16, %rsi
	movq	72(%rcx), %rdx
	movq	16(%rdx), %r14
	movq	16(%r10), %r12
	movsd	 (%r12), %xmm6
	mulsd	 (%r14), %xmm6
	movq	$10, -8(%rsi)
	movsd	 %xmm6, (%rsi)
	movq	%rsi, %r14
	addq	$16, %rsi
	movq	$28, -8(%rsi)
	movq	%r15, (%rsi)
	movq	%r13, 8(%rsi)
	movq	%r14, 16(%rsi)
	movq	%rsi, %rdx
	addq	$32, %rsi
	ucomisd	 8(%rcx), %xmm6
	jae	L6D6
L_true6C3:
	movsd	 %xmm5, %xmm11
	movsd	 %xmm4, %xmm10
then.6C5:
	/* block then<10540> (ep<1053A>,reflectiv<1053F>,letJoinK<1053E>,_t<1053D>,_t<1053C>,_tpl<1053B>) */
	ucomisd	 8(%rcx), %xmm11
	jae	L6D7
L_true6CC:
	movsd	 %xmm10, %xmm12
then.6CD:
	/* block then<10547> (ep<10542>,reflectiv<10546>,letJoinK<10545>,_t<10544>,_tpl<10543>) */
	ucomisd	 8(%rcx), %xmm12
	jb	L_true6CE
else.6CF:
	/* block else<10553> (ep<1054F>,reflectiv<10552>,letJoinK<10551>,_tpl<10550>) */
	movq	$1, %r12
	jmp	letJoinK.6C6
L6D7:
else.6CB:
	/* block else<10559> (ep<10555>,reflectiv<10558>,letJoinK<10557>,_tpl<10556>) */
	movq	$1, %r12
letJoinK.6C6:
	/* block letJoinK<104C3> (ep<104BE>,reflectiv<104C2>,letJoinK<104C1>,_tpl<104C0>,_t<104BF>) */
	cmpq	$1, %r12
	je	S_case6C7
	cmpq	$3, %r12
	je	S_case6C9
S_case6C7:
case.6C8:
	/* Liveout:  GP={%r13 %r12 %r10 %r9 %r8 %rdi}  */
	/* block case<104C8> (ep<104C4>,reflectiv<104C7>,letJoinK<104C6>,_tpl<104C5>) */
	movsd	 8(%rcx), %xmm8
	mulsd	 104(%rcx), %xmm8
	movsd	 80(%rcx), %xmm7
	addsd	 %xmm8, %xmm7
	movq	$10, -8(%rsi)
	movsd	 %xmm7, (%rsi)
	movq	%rsi, %r15
	addq	$16, %rsi
	movsd	 8(%rcx), %xmm10
	mulsd	 112(%rcx), %xmm10
	movsd	 88(%rcx), %xmm9
	addsd	 %xmm10, %xmm9
	movq	$10, -8(%rsi)
	movsd	 %xmm9, (%rsi)
	movq	%rsi, %r12
	addq	$16, %rsi
	movsd	 8(%rcx), %xmm12
	mulsd	 120(%rcx), %xmm12
	movsd	 96(%rcx), %xmm11
	addsd	 %xmm12, %xmm11
	movq	$10, -8(%rsi)
	movsd	 %xmm11, (%rsi)
	movq	%rsi, %r13
	addq	$16, %rsi
	movq	$28, -8(%rsi)
	movq	%r15, (%rsi)
	movq	%r12, 8(%rsi)
	movq	%r13, 16(%rsi)
	movq	%rsi, %r13
	addq	$32, %rsi
	movq	$1019681, -8(%rsi)
	movabsq	$letJoinK.6AB, %r14
	movq	%r14, (%rsi)
	movq	16(%rcx), %r15
	movq	%r15, 8(%rsi)
	movq	40(%rcx), %r14
	movq	%r14, 16(%rsi)
	movq	56(%rcx), %r15
	movq	%r15, 24(%rsi)
	movq	64(%rcx), %r12
	movq	%r12, 32(%rsi)
	movsd	 104(%rcx), %xmm13
	movsd	 %xmm13, 40(%rsi)
	movsd	 112(%rcx), %xmm14
	movsd	 %xmm14, 48(%rsi)
	movsd	 120(%rcx), %xmm15
	movsd	 %xmm15, 56(%rsi)
	movq	128(%rcx), %r14
	movq	%r14, 64(%rsi)
	movq	168(%rcx), %r15
	movq	%r15, 72(%rsi)
	movq	%r10, 80(%rsi)
	movq	%rbx, 88(%rsi)
	movq	%rdx, 96(%rsi)
	movsd	 %xmm7, 104(%rsi)
	movsd	 %xmm9, 112(%rsi)
	movsd	 %xmm11, 120(%rsi)
	movq	%rsi, %r12
	addq	$136, %rsi
	movq	32(%rcx), %rdx
	movq	(%rdx), %rdi
	movq	24(%rcx), %r8
	movq	%r13, %r9
	movq	128(%rcx), %r10
	movq	56(%rcx), %r13
	jmp	trace.646
S_case6C9:
case.6CA:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block case<10536> (ep<10534>,letJoinK<10535>) */
	movq	%rbx, %rdi
	movq	168(%rcx), %r8
	jmp	letJoinK.69C
L_true6CE:
then.6D0:
	/* block then<1054D> (ep<10549>,reflectiv<1054C>,letJoinK<1054B>,_tpl<1054A>) */
	movq	$3, %r12
	jmp	letJoinK.6C6
L_true6B5:
	movsd	 %xmm4, %xmm8
	movsd	 %xmm5, %xmm7
then.6B7:
	/* block then<1056B> (ep<10567>,reflectiv<1056A>,_t<10569>,_t<10568>) */
	ucomisd	 8(%rcx), %xmm7
	jb	L_true6BE
else.6BD:
	/* block else<1057C> (ep<1057A>,reflectiv<1057B>) */
	movq	$1, %r13
	jmp	letJoinK.6B8
L_true6BE:
	movsd	 %xmm8, %xmm9
then.6BF:
	/* block then<10570> (ep<1056D>,reflectiv<1056F>,_t<1056E>) */
	ucomisd	 8(%rcx), %xmm9
	jb	L_true6C0
else.6C1:
	/* block else<10578> (ep<10576>,reflectiv<10577>) */
	movq	$1, %r13
	jmp	letJoinK.6B8
L_true6C0:
then.6C2:
	/* block then<10574> (ep<10572>,reflectiv<10573>) */
	movq	$3, %r13
	jmp	letJoinK.6B8
L6D5:
	cmpq	$3, %r13
	jne	S_case6B9
S_case6BB:
case.6BC:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block case<10563> (ep<10561>,letJoinK<10562>) */
	movq	%rbx, %rdi
	movq	168(%rcx), %r8
	jmp	letJoinK.69C
L6D6:
else.6C4:
	/* block else<1055F> (ep<1055B>,reflectiv<1055E>,letJoinK<1055D>,_tpl<1055C>) */
	movq	$1, %r12
	jmp	letJoinK.6C6
	.text
letJoinK.6D9:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest6DB
	/* live= GP={%r10 %rcx %rdx %rbx} spilled=  */
retGC6DA:
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest6DB:
	movq	%r11, %r15
	subq	%rsi, %r15
	jle	doGC6DC
check.6D8:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block check<10DAF> (ep<1046A>,x<10452>,y<10453>,z<10454>) */
	movq	$4035051059, %r12
	movq	%r12, -8(%rsi)
	movabsq	$letJoinK.512, %r13
	movq	%r13, (%rsi)
	movsd	 8(%rbx), %xmm0
	movsd	 %xmm0, 8(%rsi)
	movq	24(%rbx), %r14
	movq	%r14, 16(%rsi)
	movq	32(%rbx), %r15
	movq	%r15, 24(%rsi)
	movq	40(%rbx), %r12
	movq	%r12, 32(%rsi)
	movq	48(%rbx), %r13
	movq	%r13, 40(%rsi)
	movq	56(%rbx), %r14
	movq	%r14, 48(%rsi)
	movq	64(%rbx), %r15
	movq	%r15, 56(%rsi)
	movq	72(%rbx), %r12
	movq	%r12, 64(%rsi)
	movq	80(%rbx), %r13
	movq	%r13, 72(%rsi)
	movsd	 88(%rbx), %xmm1
	movsd	 %xmm1, 80(%rsi)
	movsd	 96(%rbx), %xmm2
	movsd	 %xmm2, 88(%rsi)
	movsd	 104(%rbx), %xmm3
	movsd	 %xmm3, 96(%rsi)
	movsd	 120(%rbx), %xmm4
	movsd	 %xmm4, 104(%rsi)
	movsd	 128(%rbx), %xmm5
	movsd	 %xmm5, 112(%rsi)
	movsd	 136(%rbx), %xmm6
	movsd	 %xmm6, 120(%rsi)
	movq	144(%rbx), %r14
	movq	%r14, 128(%rsi)
	movsd	 152(%rbx), %xmm7
	movsd	 %xmm7, 136(%rsi)
	movsd	 160(%rbx), %xmm8
	movsd	 %xmm8, 144(%rsi)
	movsd	 168(%rbx), %xmm9
	movsd	 %xmm9, 152(%rsi)
	movq	176(%rbx), %r15
	movq	%r15, 160(%rsi)
	movq	184(%rbx), %r12
	movq	%r12, 168(%rsi)
	movq	%rdx, 176(%rsi)
	movq	%rcx, 184(%rsi)
	movq	%r10, 192(%rsi)
	movq	%rsi, %r10
	addq	$208, %rsi
	movq	16(%rbx), %r13
	movq	(%r13), %rdi
	movq	112(%rbx), %r8
	movq	%r10, %r9
	movq	64(%rbx), %r10
	jmp	reflectsurf.51A
doGC6DC:
	movq	$36, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %r14
	addq	$40, %rsi
	movq	%r14, %rdi
	movabsq	$retGC6DA, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.6DE:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest6E0
	/* live= GP={%rcx %rdx %rbx} spilled=  */
retGC6DF:
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest6E0:
	movq	%r11, %r10
	subq	%rsi, %r10
	jle	doGC6E1
check.6DD:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block check<10DB3> (ep<1044F>,tir<1043A>,trcol<1043B>) */
	movq	$1109655089, -8(%rsi)
	movabsq	$letJoinK.6D9, %r12
	movq	%r12, (%rsi)
	movsd	 8(%rbx), %xmm0
	movsd	 %xmm0, 8(%rsi)
	movq	24(%rbx), %r13
	movq	%r13, 16(%rsi)
	movq	32(%rbx), %r14
	movq	%r14, 24(%rsi)
	movq	40(%rbx), %r15
	movq	%r15, 32(%rsi)
	movq	48(%rbx), %r10
	movq	%r10, 40(%rsi)
	movq	56(%rbx), %r12
	movq	%r12, 48(%rsi)
	movq	64(%rbx), %r13
	movq	%r13, 56(%rsi)
	movq	72(%rbx), %r14
	movq	%r14, 64(%rsi)
	movq	80(%rbx), %r15
	movq	%r15, 72(%rsi)
	movq	88(%rbx), %r10
	movq	%r10, 80(%rsi)
	movsd	 96(%rbx), %xmm1
	movsd	 %xmm1, 88(%rsi)
	movsd	 104(%rbx), %xmm2
	movsd	 %xmm2, 96(%rsi)
	movsd	 112(%rbx), %xmm3
	movsd	 %xmm3, 104(%rsi)
	movq	120(%rbx), %r12
	movq	%r12, 112(%rsi)
	movsd	 128(%rbx), %xmm4
	movsd	 %xmm4, 120(%rsi)
	movsd	 136(%rbx), %xmm5
	movsd	 %xmm5, 128(%rsi)
	movsd	 144(%rbx), %xmm6
	movsd	 %xmm6, 136(%rsi)
	movq	152(%rbx), %r13
	movq	%r13, 144(%rsi)
	movsd	 160(%rbx), %xmm7
	movsd	 %xmm7, 152(%rsi)
	movsd	 168(%rbx), %xmm8
	movsd	 %xmm8, 160(%rsi)
	movsd	 176(%rbx), %xmm9
	movsd	 %xmm9, 168(%rsi)
	movq	%rdx, 176(%rsi)
	movq	%rcx, 184(%rsi)
	movq	%rsi, %r10
	addq	$200, %rsi
	movq	16(%rbx), %r14
	movq	(%r14), %rdi
	movq	120(%rbx), %r8
	movq	%r10, %r9
	movq	72(%rbx), %r10
	jmp	specularsurf.4FB
doGC6E1:
	movq	$28, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%rsi, %r15
	addq	$32, %rsi
	movq	%r15, %rdi
	movabsq	$retGC6DF, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.6E3:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest6E5
	/* live= GP={%rcx %rdx} spilled=  */
retGC6E4:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest6E5:
	movq	%r11, %rbx
	subq	%rsi, %rbx
	jle	doGC6E6
check.6E2:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block check<10DB6> (ep<10641>,newcol<10639>) */
	movq	$10, -8(%rsi)
	movq	(%rcx), %r10
	movsd	 (%r10), %xmm0
	mulsd	 32(%rdx), %xmm0
	addsd	 8(%rdx), %xmm0
	movsd	 %xmm0, (%rsi)
	movq	%rsi, %rbx
	addq	$16, %rsi
	movq	$10, -8(%rsi)
	movq	8(%rcx), %r13
	movsd	 (%r13), %xmm1
	mulsd	 40(%rdx), %xmm1
	addsd	 16(%rdx), %xmm1
	movsd	 %xmm1, (%rsi)
	movq	%rsi, %r12
	addq	$16, %rsi
	movq	$10, -8(%rsi)
	movq	16(%rcx), %r15
	movsd	 (%r15), %xmm2
	mulsd	 48(%rdx), %xmm2
	addsd	 24(%rdx), %xmm2
	movsd	 %xmm2, (%rsi)
	movq	%rsi, %r14
	addq	$16, %rsi
	movq	$28, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%r12, 8(%rsi)
	movq	%r14, 16(%rsi)
	movq	%rsi, %rcx
	addq	$32, %rsi
	movq	56(%rdx), %rdi
	movq	$1, %r8
	movq	%rcx, %r9
	jmp	letJoinK.6DE
doGC6E6:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	movq	%rdx, %rdi
	movabsq	$retGC6E4, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.6EC:
	movq	%r9, %rcx
	movq	%r8, %rbx
	movq	%rdi, %rdx
gcTest6EE:
	movq	%r11, %r13
	subq	%rsi, %r13
	jle	doGC6EF
check.6E7:
	/* block check<10DBB> (ep<10636>,hit<10628>,dist<10629>,sp<1062A>) */
	movq	$16401, -8(%rsi)
	movabsq	$letJoinK.6E3, %r12
	movq	%r12, (%rsi)
	movsd	 40(%rdx), %xmm0
	movsd	 %xmm0, 8(%rsi)
	movsd	 48(%rdx), %xmm1
	movsd	 %xmm1, 16(%rsi)
	movsd	 56(%rdx), %xmm2
	movsd	 %xmm2, 24(%rsi)
	movsd	 64(%rdx), %xmm3
	movsd	 %xmm3, 32(%rsi)
	movsd	 72(%rdx), %xmm4
	movsd	 %xmm4, 40(%rsi)
	movsd	 80(%rdx), %xmm5
	movsd	 %xmm5, 48(%rsi)
	movq	88(%rdx), %r13
	movq	%r13, 56(%rsi)
	movq	%rsi, %r13
	addq	$72, %rsi
	cmpq	$1, %rbx
	jne	L6F0
S_case6E8:
case.6E9:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block case<1065F> (ep<1065D>,letJoinK<1065E>) */
	movq	(%r13), %r10
	movq	%r13, %rdi
	movq	8(%rdx), %r8
	jmp	*%r10
doGC6EF:
	movq	$36, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rbx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %r12
	addq	$40, %rsi
	movq	%r12, %rdi
	movabsq	$retGC6ED, %r8
	jmp	ASM_InvokeGC
	/* live= GP={%r10 %rcx %rbx %rdx} spilled=  */
retGC6ED:
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rbx
	movq	(%rdi), %rdx
	jmp	gcTest6EE
L6F0:
	cmpq	$3, %rbx
	jne	S_case6E8
S_case6EA:
case.6EB:
	/* Liveout:  GP={%r15 %r14 %r13 %r12 %r10 %r9 %r8 %rdi} FP={%xmm7 %xmm6 %xmm5 %xmm4
 %xmm3 %xmm2}  */
	/* block case<10666> (ep<10662>,dist<10665>,sp<10664>,letJoinK<10663>) */
	movq	$10, -8(%rsi)
	movsd	 (%rcx), %xmm6
	movsd	 %xmm6, (%rsi)
	movq	%rsi, %r15
	addq	$16, %rsi
	movq	16(%rdx), %r14
	movq	(%r14), %rdi
	movq	32(%rdx), %r8
	movq	%r10, %r9
	/* %xmm2.d := mem.d[%r11092.64 +.64 112] */
	movsd	 112(%rdx), %xmm2
	/* %xmm3.d := mem.d[%r11092.64 +.64 120] */
	movsd	 120(%rdx), %xmm3
	/* %xmm4.d := mem.d[%r11092.64 +.64 128] */
	movsd	 128(%rdx), %xmm4
	movq	104(%rdx), %r10
	movq	104(%rdx), %rbx
	movq	(%rbx), %rcx
	/* %xmm5.d := mem.d[(mem.64[(mem.64[%r11092.64 +.64 104]) +.64 0]) +.64 0] */
	movsd	 (%rcx), %xmm5
	movq	104(%rdx), %r14
	movq	8(%r14), %r12
	/* %xmm6.d := mem.d[(mem.64[(mem.64[%r11092.64 +.64 104]) +.64 8]) +.64 0] */
	movsd	 (%r12), %xmm6
	movq	104(%rdx), %rbx
	movq	16(%rbx), %rcx
	/* %xmm7.d := mem.d[(mem.64[(mem.64[%r11092.64 +.64 104]) +.64 16]) +.64 0] */
	movsd	 (%rcx), %xmm7
	movq	96(%rdx), %r12
	movq	24(%rdx), %r14
	jmp	shade.6AA
	.text
letJoinK.537:
	movq	%r8, %rcx
	movq	%rdi, %rdx
gcTest70E:
	movq	%r11, %r12
	subq	%rsi, %r12
	jle	doGC70F
check.6F1:
	/* block check<10DBE> (ep<105C1>,index<105AF>) */
	movq	72(%rdx), %r15
	movq	(%r15), %r14
	movsd	 144(%rdx), %xmm4
	mulsd	 (%r14), %xmm4
	movq	$10, -8(%rsi)
	movsd	 %xmm4, (%rsi)
	movq	%rsi, %r15
	addq	$16, %rsi
	movq	72(%rdx), %r10
	movq	8(%r10), %rbx
	movsd	 152(%rdx), %xmm5
	mulsd	 (%rbx), %xmm5
	movq	$10, -8(%rsi)
	movsd	 %xmm5, (%rsi)
	movq	%rsi, %r12
	addq	$16, %rsi
	movq	72(%rdx), %r14
	movq	16(%r14), %r13
	movsd	 160(%rdx), %xmm6
	mulsd	 (%r13), %xmm6
	movq	$10, -8(%rsi)
	movsd	 %xmm6, (%rsi)
	movq	%rsi, %rbx
	addq	$16, %rsi
	movq	$28, -8(%rsi)
	movq	%r15, (%rsi)
	movq	%r12, 8(%rsi)
	movq	%rbx, 16(%rsi)
	movq	%rsi, %rbx
	addq	$32, %rsi
	ucomisd	 8(%rdx), %xmm6
	jb	L_true6F2
else.6F3:
	/* block else<106FF> (ep<106FC>,index<106FE>,_tpl<106FD>) */
	movq	$1, %r13
letJoinK.6F5:
	/* block letJoinK<105DB> (ep<105D7>,index<105DA>,_tpl<105D9>,_t<105D8>) */
	cmpq	$1, %r13
	jne	L710
S_case6F6:
case.6F7:
	/* block case<105DF> (ep<105DC>,index<105DE>,_tpl<105DD>) */
	movq	104(%rdx), %r12
	movq	16(%r12), %r10
	movq	64(%rdx), %r14
	movq	16(%r14), %r13
	movsd	 (%r13), %xmm10
	mulsd	 (%r10), %xmm10
	movq	104(%rdx), %r10
	movq	8(%r10), %r15
	movq	64(%rdx), %r13
	movq	8(%r13), %r12
	movsd	 (%r12), %xmm11
	mulsd	 (%r15), %xmm11
	movq	104(%rdx), %r15
	movq	(%r15), %r14
	movq	64(%rdx), %r12
	movq	(%r12), %r10
	movsd	 (%r10), %xmm9
	mulsd	 (%r14), %xmm9
	addsd	 %xmm11, %xmm9
	addsd	 %xmm10, %xmm9
	movabsq	$signBit6455D, %r13
	movsd	 (%r13), %xmm8
	xorpd	 %xmm9, %xmm8
	movsd	 %xmm8, %xmm7
	movabsq	$flt4D3, %r14
	ucomisd	 (%r14), %xmm7
	jae	L711
L_true700:
	movsd	 %xmm7, %xmm8
then.702:
	/* block then<106B8> (ep<106B4>,index<106B7>,_tpl<106B6>,_t<106B5>) */
	movabsq	$flt707, %r12
	movsd	 (%r12), %xmm2
	movabsq	$flt536, %r13
	movsd	 (%r13), %xmm9
	/* %f11201.d := fdiv.d(mem.d[flt536],mem.d[%r11203.64 +.64 0]) */
	divsd	 (%rcx), %xmm9
	movabsq	$signBit6455D, %r14
	/* %f11200.d := fneg.d(%f11205.d) */
	movsd	 (%r14), %xmm3
	/* %f11200.d := fneg.d(%f11205.d) */
	xorpd	 %xmm8, %xmm3
	movsd	 %xmm3, %xmm7
	movq	104(%rdx), %rcx
	movq	16(%rcx), %r15
	movsd	 %xmm2, %xmm6
	/* %f11199.d := fmul.d(%f11325.d,mem.d[(mem.64[(mem.64[%r11202.64 +.64 104]) +.64 16]) +.64 0]) */
	mulsd	 (%r15), %xmm6
	movq	104(%rdx), %r12
	movq	8(%r12), %r10
	movsd	 %xmm2, %xmm5
	/* %f11198.d := fmul.d(%f11325.d,mem.d[(mem.64[(mem.64[%r11202.64 +.64 104]) +.64 8]) +.64 0]) */
	mulsd	 (%r10), %xmm5
	movq	104(%rdx), %r14
	movq	(%r14), %r13
	movsd	 %xmm2, %xmm4
	/* %f11197.d := fmul.d(%f11325.d,mem.d[(mem.64[(mem.64[%r11202.64 +.64 104]) +.64 0]) +.64 0]) */
	mulsd	 (%r13), %xmm4
letJoinK.703:
	/* block letJoinK<10600> (ep<105F9>,_tpl<105FF>,_t<105FA>,_t<105FB>,_t<105FC>,_t<105FD>,_t<105FE>) */
	movsd	 %xmm7, %xmm1
	mulsd	 %xmm7, %xmm1
	movabsq	$flt536, %r15
	movsd	 (%r15), %xmm0
	subsd	 %xmm1, %xmm0
	movsd	 %xmm9, %xmm15
	mulsd	 %xmm9, %xmm15
	mulsd	 %xmm0, %xmm15
	movabsq	$flt536, %rcx
	movsd	 (%rcx), %xmm14
	subsd	 %xmm15, %xmm14
	movabsq	$flt4D3, %r10
	ucomisd	 (%r10), %xmm14
	jb	L_true704
	movsd	 %xmm6, %xmm15
	movsd	 %xmm5, %xmm13
	movsd	 %xmm4, %xmm12
	movsd	 %xmm14, %xmm11
	movsd	 %xmm7, %xmm10
	movsd	 %xmm9, %xmm9
else.705:
	/* block else<10699> (ep<10691>,_tpl<10698>,_t<10697>,_t<10696>,_t<10695>,_t<10694>,_t<10693>,_t<10692>) */
	sqrtsd	 %xmm11, %xmm5
	movsd	 %xmm9, %xmm4
	mulsd	 %xmm10, %xmm4
	subsd	 %xmm5, %xmm4
	movq	64(%rdx), %rcx
	movq	(%rcx), %r15
	movsd	 %xmm9, %xmm7
	mulsd	 (%r15), %xmm7
	movsd	 %xmm4, %xmm6
	mulsd	 %xmm12, %xmm6
	addsd	 %xmm7, %xmm6
	movq	$10, -8(%rsi)
	movsd	 %xmm6, (%rsi)
	movq	%rsi, %rcx
	addq	$16, %rsi
	movq	64(%rdx), %r12
	movq	8(%r12), %r10
	movsd	 %xmm9, %xmm0
	mulsd	 (%r10), %xmm0
	movsd	 %xmm4, %xmm8
	mulsd	 %xmm13, %xmm8
	addsd	 %xmm0, %xmm8
	movq	$10, -8(%rsi)
	movsd	 %xmm8, (%rsi)
	movq	%rsi, %r13
	addq	$16, %rsi
	movq	64(%rdx), %r15
	movq	16(%r15), %r14
	movsd	 %xmm9, %xmm2
	mulsd	 (%r14), %xmm2
	movsd	 %xmm4, %xmm1
	mulsd	 %xmm15, %xmm1
	addsd	 %xmm2, %xmm1
	movq	$10, -8(%rsi)
	movsd	 %xmm1, (%rsi)
	movq	%rsi, %r10
	addq	$16, %rsi
	movq	$28, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%r13, 8(%rsi)
	movq	%r10, 16(%rsi)
	movq	%rsi, %r10
	addq	$32, %rsi
	movsd	 %xmm1, %xmm0
	movsd	 %xmm8, %xmm15
	movsd	 %xmm6, %xmm14
	movq	$1, %r12
	jmp	letJoinK.708
doGC70F:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	%r10, %rdi
	movabsq	$retGC70D, %r8
	jmp	ASM_InvokeGC
	/* live= GP={%rcx %rdx} spilled=  */
retGC70D:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
	jmp	gcTest70E
L_true704:
then.706:
	/* block then<10688> (ep<10686>,_tpl<10687>) */
	movabsq	$flt4D3, %r12
	movsd	 (%r12), %xmm3
	movq	$10, -8(%rsi)
	movsd	 %xmm3, (%rsi)
	movq	%rsi, %r13
	addq	$16, %rsi
	movabsq	$flt4D3, %r14
	movsd	 (%r14), %xmm4
	movq	$10, -8(%rsi)
	movsd	 %xmm4, (%rsi)
	movq	%rsi, %r15
	addq	$16, %rsi
	movabsq	$flt4D3, %rcx
	movsd	 (%rcx), %xmm5
	movq	$10, -8(%rsi)
	movsd	 %xmm5, (%rsi)
	movq	%rsi, %r10
	addq	$16, %rsi
	movq	$28, -8(%rsi)
	movq	%r13, (%rsi)
	movq	%r15, 8(%rsi)
	movq	%r10, 16(%rsi)
	movq	%rsi, %r10
	addq	$32, %rsi
	movsd	 %xmm5, %xmm0
	movsd	 %xmm4, %xmm15
	movsd	 %xmm3, %xmm14
	movq	$3, %r12
letJoinK.708:
	/* block letJoinK<10610> (ep<10609>,_tpl<1060F>,tir<1060A>,newdir<1060B>,_t<1060C>,_t<1060D>,_t<1060E>) */
	cmpq	$1, %r12
	je	S_case709
	cmpq	$3, %r12
	je	S_case70B
S_case709:
	movsd	 %xmm0, %xmm3
	movsd	 %xmm15, %xmm2
	movsd	 %xmm14, %xmm1
case.70A:
	/* Liveout:  GP={%r13 %r12 %r10 %r9 %r8 %rdi}  */
	/* block case<10617> (ep<10611>,_tpl<10616>,newdir<10615>,_t<10614>,_t<10613>,_t<10612>) */
	movsd	 %xmm1, %xmm7
	mulsd	 8(%rdx), %xmm7
	movsd	 80(%rdx), %xmm6
	addsd	 %xmm7, %xmm6
	movq	$10, -8(%rsi)
	movsd	 %xmm6, (%rsi)
	movq	%rsi, %r12
	addq	$16, %rsi
	movsd	 %xmm2, %xmm9
	mulsd	 8(%rdx), %xmm9
	movsd	 88(%rdx), %xmm8
	addsd	 %xmm9, %xmm8
	movq	$10, -8(%rsi)
	movsd	 %xmm8, (%rsi)
	movq	%rsi, %r13
	addq	$16, %rsi
	movsd	 %xmm3, %xmm11
	mulsd	 8(%rdx), %xmm11
	movsd	 96(%rdx), %xmm10
	addsd	 %xmm11, %xmm10
	movq	$10, -8(%rsi)
	movsd	 %xmm10, (%rsi)
	movq	%rsi, %r14
	addq	$16, %rsi
	movq	$28, -8(%rsi)
	movq	%r12, (%rsi)
	movq	%r13, 8(%rsi)
	movq	%r14, 16(%rsi)
	movq	%rsi, %r13
	addq	$32, %rsi
	movq	$1838883, -8(%rsi)
	movabsq	$letJoinK.6EC, %r15
	movq	%r15, (%rsi)
	movq	16(%rdx), %rcx
	movq	%rcx, 8(%rsi)
	movq	40(%rdx), %r12
	movq	%r12, 16(%rsi)
	movq	48(%rdx), %r14
	movq	%r14, 24(%rsi)
	movq	56(%rdx), %r15
	movq	%r15, 32(%rsi)
	movsd	 112(%rdx), %xmm12
	movsd	 %xmm12, 40(%rsi)
	movsd	 120(%rdx), %xmm13
	movsd	 %xmm13, 48(%rsi)
	movsd	 128(%rdx), %xmm14
	movsd	 %xmm14, 56(%rsi)
	movsd	 144(%rdx), %xmm15
	movsd	 %xmm15, 64(%rsi)
	movsd	 152(%rdx), %xmm0
	movsd	 %xmm0, 72(%rsi)
	movsd	 160(%rdx), %xmm1
	movsd	 %xmm1, 80(%rsi)
	movq	168(%rdx), %rcx
	movq	%rcx, 88(%rsi)
	movq	%rbx, 96(%rsi)
	movq	%r10, 104(%rsi)
	movsd	 %xmm6, 112(%rsi)
	movsd	 %xmm8, 120(%rsi)
	movsd	 %xmm10, 128(%rsi)
	movq	%rsi, %r12
	addq	$144, %rsi
	movq	32(%rdx), %rbx
	movq	(%rbx), %rdi
	movq	24(%rdx), %r8
	movq	%r13, %r9
	movq	48(%rdx), %r13
	jmp	trace.646
S_case70B:
case.70C:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block case<10681> (ep<10680>) */
	movq	168(%rdx), %rdi
	movq	$3, %r8
	movq	136(%rdx), %r9
	jmp	letJoinK.6DE
L710:
	cmpq	$3, %r13
	jne	S_case6F6
S_case6F8:
case.6F9:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block case<106DA> (ep<106D9>) */
	movq	168(%rdx), %rdi
	movq	$1, %r8
	movq	136(%rdx), %r9
	jmp	letJoinK.6DE
L711:
	movsd	 %xmm7, %xmm3
else.701:
	/* block else<106CE> (ep<106CA>,index<106CD>,_tpl<106CC>,_t<106CB>) */
	movsd	 %xmm3, %xmm7
	/* %f11201.d := mem.d[%r11192.64 +.64 0] */
	movsd	 (%rcx), %xmm9
	movq	104(%rdx), %rcx
	movq	16(%rcx), %r15
	/* %f11199.d := mem.d[(mem.64[(mem.64[%r11191.64 +.64 104]) +.64 16]) +.64 0] */
	movsd	 (%r15), %xmm6
	movq	104(%rdx), %r12
	movq	8(%r12), %r10
	/* %f11198.d := mem.d[(mem.64[(mem.64[%r11191.64 +.64 104]) +.64 8]) +.64 0] */
	movsd	 (%r10), %xmm5
	movq	104(%rdx), %r14
	movq	(%r14), %r13
	/* %f11197.d := mem.d[(mem.64[(mem.64[%r11191.64 +.64 104]) +.64 0]) +.64 0] */
	movsd	 (%r13), %xmm4
	jmp	letJoinK.703
L_true6F2:
	movsd	 %xmm5, %xmm1
	movsd	 %xmm4, %xmm0
then.6F4:
	/* block then<106E5> (ep<106E0>,index<106E4>,_t<106E3>,_t<106E2>,_tpl<106E1>) */
	ucomisd	 8(%rdx), %xmm1
	jb	L_true6FB
else.6FA:
	/* block else<106FA> (ep<106F7>,index<106F9>,_tpl<106F8>) */
	movq	$1, %r13
	jmp	letJoinK.6F5
L_true6FB:
	movsd	 %xmm0, %xmm2
then.6FC:
	/* block then<106EB> (ep<106E7>,index<106EA>,_t<106E9>,_tpl<106E8>) */
	ucomisd	 8(%rdx), %xmm2
	jb	L_true6FD
else.6FE:
	/* block else<106F5> (ep<106F2>,index<106F4>,_tpl<106F3>) */
	movq	$1, %r13
	jmp	letJoinK.6F5
L_true6FD:
then.6FF:
	/* block then<106F0> (ep<106ED>,index<106EF>,_tpl<106EE>) */
	movq	$3, %r13
	jmp	letJoinK.6F5
	.text
letJoinK.716:
	movsd	 %xmm4, %xmm8
	movsd	 %xmm3, %xmm7
	movsd	 %xmm2, %xmm6
	movq	%rdi, %rcx
	movsd	 %xmm8, %xmm15
	movsd	 %xmm7, %xmm14
	movsd	 %xmm6, %xmm13
	jmp	gcTest718
	/* live= GP={%rcx} FP={%xmm2 %xmm1 %xmm0} spilled=  */
retGC717:
	movq	(%rdi), %rcx
	/* %f11561.d := mem.d[%rdi.64 +.64 24] */
	movsd	 24(%rdi), %xmm15
	/* %f11560.d := mem.d[%rdi.64 +.64 16] */
	movsd	 16(%rdi), %xmm14
	/* %f11559.d := mem.d[%rdi.64 +.64 8] */
	movsd	 8(%rdi), %xmm13
gcTest718:
	movq	%r11, %rdx
	subq	%rsi, %rdx
	jle	doGC719
	movsd	 %xmm15, %xmm2
	movsd	 %xmm14, %xmm1
	movsd	 %xmm13, %xmm0
check.712:
	/* block check<10DC3> (ep<1042C>,_t<10413>,_t<10414>,_t<10415>) */
	movq	184(%rcx), %rbx
	movsd	 %xmm0, %xmm9
	mulsd	 (%rbx), %xmm9
	movq	184(%rcx), %r10
	movsd	 %xmm1, %xmm10
	mulsd	 (%r10), %xmm10
	movq	184(%rcx), %r12
	movsd	 %xmm2, %xmm11
	mulsd	 (%r12), %xmm11
	movq	$71826991, -8(%rsi)
	movabsq	$letJoinK.6DE, %r13
	movq	%r13, (%rsi)
	movsd	 8(%rcx), %xmm12
	movsd	 %xmm12, 8(%rsi)
	movq	16(%rcx), %r14
	movq	%r14, 16(%rsi)
	movq	24(%rcx), %r15
	movq	%r15, 24(%rsi)
	movq	40(%rcx), %rdx
	movq	%rdx, 32(%rsi)
	movq	48(%rcx), %rbx
	movq	%rbx, 40(%rsi)
	movq	56(%rcx), %r10
	movq	%r10, 48(%rsi)
	movq	64(%rcx), %r12
	movq	%r12, 56(%rsi)
	movq	72(%rcx), %r13
	movq	%r13, 64(%rsi)
	movq	80(%rcx), %r14
	movq	%r14, 72(%rsi)
	movq	88(%rcx), %r15
	movq	%r15, 80(%rsi)
	movq	104(%rcx), %rdx
	movq	%rdx, 88(%rsi)
	movsd	 112(%rcx), %xmm13
	movsd	 %xmm13, 96(%rsi)
	movsd	 120(%rcx), %xmm14
	movsd	 %xmm14, 104(%rsi)
	movsd	 128(%rcx), %xmm15
	movsd	 %xmm15, 112(%rsi)
	movq	136(%rcx), %rbx
	movq	%rbx, 120(%rsi)
	movsd	 152(%rcx), %xmm0
	movsd	 %xmm0, 128(%rsi)
	movsd	 160(%rcx), %xmm1
	movsd	 %xmm1, 136(%rsi)
	movsd	 168(%rcx), %xmm2
	movsd	 %xmm2, 144(%rsi)
	movq	176(%rcx), %r10
	movq	%r10, 152(%rsi)
	movsd	 %xmm9, 160(%rsi)
	movsd	 %xmm10, 168(%rsi)
	movsd	 %xmm11, 176(%rsi)
	movq	%rsi, %rdx
	addq	$192, %rsi
	movq	184(%rcx), %r12
	movsd	 (%r12), %xmm3
	ucomisd	 8(%rcx), %xmm3
	jae	L71A
L_true713:
then.715:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block then<105A5> (ep<105A3>,letJoinK<105A4>) */
	movq	%rdx, %rdi
	movq	$1, %r8
	movq	216(%rcx), %r9
	jmp	letJoinK.6DE
doGC719:
	movq	$137, -8(%rsi)
	movq	%rcx, (%rsi)
	movsd	 %xmm13, 8(%rsi)
	movsd	 %xmm14, 16(%rsi)
	movsd	 %xmm15, 24(%rsi)
	movq	%rsi, %r15
	addq	$40, %rsi
	movq	%r15, %rdi
	movabsq	$retGC717, %r8
	jmp	ASM_InvokeGC
L71A:
	movsd	 %xmm11, %xmm5
	movsd	 %xmm10, %xmm4
	movsd	 %xmm9, %xmm3
else.714:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block else<105AE> (ep<105A9>,_t<105AD>,_t<105AC>,_t<105AB>,letJoinK<105AA>) */
	movq	$286391853, -8(%rsi)
	movabsq	$letJoinK.537, %r14
	movq	%r14, (%rsi)
	movsd	 8(%rcx), %xmm6
	movsd	 %xmm6, 8(%rsi)
	movq	40(%rcx), %r15
	movq	%r15, 16(%rsi)
	movq	48(%rcx), %rbx
	movq	%rbx, 24(%rsi)
	movq	56(%rcx), %r10
	movq	%r10, 32(%rsi)
	movq	64(%rcx), %r12
	movq	%r12, 40(%rsi)
	movq	80(%rcx), %r13
	movq	%r13, 48(%rsi)
	movq	88(%rcx), %r14
	movq	%r14, 56(%rsi)
	movq	96(%rcx), %r15
	movq	%r15, 64(%rsi)
	movq	104(%rcx), %rbx
	movq	%rbx, 72(%rsi)
	movsd	 112(%rcx), %xmm7
	movsd	 %xmm7, 80(%rsi)
	movsd	 120(%rcx), %xmm8
	movsd	 %xmm8, 88(%rsi)
	movsd	 128(%rcx), %xmm9
	movsd	 %xmm9, 96(%rsi)
	movq	144(%rcx), %r10
	movq	%r10, 104(%rsi)
	movsd	 192(%rcx), %xmm10
	movsd	 %xmm10, 112(%rsi)
	movsd	 200(%rcx), %xmm11
	movsd	 %xmm11, 120(%rsi)
	movsd	 208(%rcx), %xmm12
	movsd	 %xmm12, 128(%rsi)
	movq	216(%rcx), %r12
	movq	%r12, 136(%rsi)
	movsd	 %xmm3, 144(%rsi)
	movsd	 %xmm4, 152(%rsi)
	movsd	 %xmm5, 160(%rsi)
	movq	%rdx, 168(%rsi)
	movq	%rsi, %r13
	addq	$184, %rsi
	movq	32(%rcx), %r14
	movq	(%r14), %rdi
	movq	136(%rcx), %r8
	movq	%r13, %r9
	movq	80(%rcx), %r10
	jmp	refractsurf.53F
	.text
letJoinK.524:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest71D
	/* live= GP={%rcx %rdx} spilled=  */
retGC71C:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest71D:
	movq	%r11, %rbx
	subq	%rsi, %rbx
	jle	doGC71E
check.71B:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block check<10DC6> (ep<103FD>,transmitted<103E2>) */
	movq	216(%rdx), %r10
	movq	(%r10), %rbx
	movsd	 152(%rdx), %xmm0
	addsd	 (%rbx), %xmm0
	movq	$10, -8(%rsi)
	movsd	 %xmm0, (%rsi)
	movq	%rsi, %r10
	addq	$16, %rsi
	movq	216(%rdx), %r13
	movq	8(%r13), %r12
	movsd	 160(%rdx), %xmm1
	addsd	 (%r12), %xmm1
	movq	$10, -8(%rsi)
	movsd	 %xmm1, (%rsi)
	movq	%rsi, %r14
	addq	$16, %rsi
	movq	216(%rdx), %rbx
	movq	16(%rbx), %r15
	movsd	 168(%rdx), %xmm2
	addsd	 (%r15), %xmm2
	movq	$10, -8(%rsi)
	movsd	 %xmm2, (%rsi)
	movq	%rsi, %r12
	addq	$16, %rsi
	movq	$28, -8(%rsi)
	movq	%r10, (%rsi)
	movq	%r14, 8(%rsi)
	movq	%r12, 16(%rsi)
	movq	%rsi, %r13
	addq	$32, %rsi
	movq	$18842910265, %r14
	movq	%r14, -8(%rsi)
	movabsq	$letJoinK.716, %r15
	movq	%r15, (%rsi)
	movsd	 8(%rdx), %xmm3
	movsd	 %xmm3, 8(%rsi)
	movq	16(%rdx), %rbx
	movq	%rbx, 16(%rsi)
	movq	24(%rdx), %r10
	movq	%r10, 24(%rsi)
	movq	32(%rdx), %r12
	movq	%r12, 32(%rsi)
	movq	48(%rdx), %r14
	movq	%r14, 40(%rsi)
	movq	56(%rdx), %r15
	movq	%r15, 48(%rsi)
	movq	64(%rdx), %rbx
	movq	%rbx, 56(%rsi)
	movq	72(%rdx), %r10
	movq	%r10, 64(%rsi)
	movq	80(%rdx), %r12
	movq	%r12, 72(%rsi)
	movq	88(%rdx), %r14
	movq	%r14, 80(%rsi)
	movq	96(%rdx), %r15
	movq	%r15, 88(%rsi)
	movq	104(%rdx), %rbx
	movq	%rbx, 96(%rsi)
	movq	112(%rdx), %r10
	movq	%r10, 104(%rsi)
	movsd	 120(%rdx), %xmm4
	movsd	 %xmm4, 112(%rsi)
	movsd	 128(%rdx), %xmm5
	movsd	 %xmm5, 120(%rsi)
	movsd	 136(%rdx), %xmm6
	movsd	 %xmm6, 128(%rsi)
	movq	144(%rdx), %r12
	movq	%r12, 136(%rsi)
	movq	176(%rdx), %r14
	movq	%r14, 144(%rsi)
	movsd	 184(%rdx), %xmm7
	movsd	 %xmm7, 152(%rsi)
	movsd	 192(%rdx), %xmm8
	movsd	 %xmm8, 160(%rsi)
	movsd	 200(%rdx), %xmm9
	movsd	 %xmm9, 168(%rsi)
	movq	208(%rdx), %r15
	movq	%r15, 176(%rsi)
	movq	%rcx, 184(%rsi)
	movsd	 %xmm0, 192(%rsi)
	movsd	 %xmm1, 200(%rsi)
	movsd	 %xmm2, 208(%rsi)
	movq	%r13, 216(%rsi)
	movq	%rsi, %r13
	addq	$232, %rsi
	movq	40(%rdx), %rcx
	movq	(%rcx), %rdi
	movq	144(%rdx), %r8
	movq	%r13, %r9
	movq	88(%rdx), %r10
	jmp	bodysurf.54C
doGC71E:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	movq	%rdx, %rdi
	movabsq	$retGC71C, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.720:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest722
	/* live= GP={%rcx %rdx} spilled=  */
retGC721:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest722:
	movq	%r11, %r13
	subq	%rsi, %r13
	jle	doGC723
check.71F:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block check<10DC9> (ep<103DF>,diff<103C3>) */
	movq	$26344422969, %r10
	movq	%r10, -8(%rsi)
	movabsq	$letJoinK.524, %r12
	movq	%r12, (%rsi)
	movsd	 8(%rdx), %xmm0
	movsd	 %xmm0, 8(%rsi)
	movq	16(%rdx), %r13
	movq	%r13, 16(%rsi)
	movq	24(%rdx), %r14
	movq	%r14, 24(%rsi)
	movq	40(%rdx), %r15
	movq	%r15, 32(%rsi)
	movq	48(%rdx), %rbx
	movq	%rbx, 40(%rsi)
	movq	56(%rdx), %r10
	movq	%r10, 48(%rsi)
	movq	64(%rdx), %r12
	movq	%r12, 56(%rsi)
	movq	72(%rdx), %r13
	movq	%r13, 64(%rsi)
	movq	80(%rdx), %r14
	movq	%r14, 72(%rsi)
	movq	88(%rdx), %r15
	movq	%r15, 80(%rsi)
	movq	96(%rdx), %rbx
	movq	%rbx, 88(%rsi)
	movq	104(%rdx), %r10
	movq	%r10, 96(%rsi)
	movq	112(%rdx), %r12
	movq	%r12, 104(%rsi)
	movq	120(%rdx), %r13
	movq	%r13, 112(%rsi)
	movsd	 128(%rdx), %xmm1
	movsd	 %xmm1, 120(%rsi)
	movsd	 136(%rdx), %xmm2
	movsd	 %xmm2, 128(%rsi)
	movsd	 144(%rdx), %xmm3
	movsd	 %xmm3, 136(%rsi)
	movq	152(%rdx), %r14
	movq	%r14, 144(%rsi)
	movsd	 160(%rdx), %xmm4
	movsd	 %xmm4, 152(%rsi)
	movsd	 168(%rdx), %xmm5
	movsd	 %xmm5, 160(%rsi)
	movsd	 176(%rdx), %xmm6
	movsd	 %xmm6, 168(%rsi)
	movq	184(%rdx), %r15
	movq	%r15, 176(%rsi)
	movsd	 192(%rdx), %xmm7
	movsd	 %xmm7, 184(%rsi)
	movsd	 200(%rdx), %xmm8
	movsd	 %xmm8, 192(%rsi)
	movsd	 208(%rdx), %xmm9
	movsd	 %xmm9, 200(%rsi)
	movq	216(%rdx), %rbx
	movq	%rbx, 208(%rsi)
	movq	%rcx, 216(%rsi)
	movq	%rsi, %rbx
	addq	$232, %rsi
	movq	32(%rdx), %r10
	movq	(%r10), %rdi
	movq	152(%rdx), %r8
	movq	%rbx, %r9
	movq	96(%rdx), %r10
	jmp	transmitsurf.52C
doGC723:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	%r12, %rdi
	movabsq	$retGC721, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.725:
	movq	%r8, %r10
	movq	%rdi, %rcx
	jmp	gcTest727
	/* live= GP={%r10 %rcx} spilled=  */
retGC726:
	movq	8(%rdi), %r10
	movq	(%rdi), %rcx
gcTest727:
	movq	%r11, %r15
	subq	%rsi, %r15
	jle	doGC728
check.724:
	/* Liveout:  GP={%r13 %r12 %r10 %r9 %r8 %rdi}  */
	/* block check<10DCC> (ep<1039D>,_t<10385>) */
	movq	$18, -8(%rsi)
	movq	$1, (%rsi)
	movabsq	$f.697, %rdx
	movq	%rdx, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	$10, -8(%rsi)
	movabsq	$flt4D3, %r12
	movsd	 (%r12), %xmm0
	movsd	 %xmm0, (%rsi)
	movq	%rsi, %rbx
	addq	$16, %rsi
	movq	$10, -8(%rsi)
	movabsq	$flt4D3, %r14
	movsd	 (%r14), %xmm1
	movsd	 %xmm1, (%rsi)
	movq	%rsi, %r13
	addq	$16, %rsi
	movq	$10, -8(%rsi)
	movabsq	$flt4D3, %r12
	movsd	 (%r12), %xmm2
	movsd	 %xmm2, (%rsi)
	movq	%rsi, %rdx
	addq	$16, %rsi
	movq	$28, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%r13, 8(%rsi)
	movq	%rdx, 16(%rsi)
	movq	%rsi, %rbx
	addq	$32, %rsi
	movq	$18329108025, %r13
	movq	%r13, -8(%rsi)
	movabsq	$letJoinK.720, %r14
	movq	%r14, (%rsi)
	movsd	 16(%rcx), %xmm3
	movsd	 %xmm3, 8(%rsi)
	movq	24(%rcx), %rdx
	movq	%rdx, 16(%rsi)
	movq	32(%rcx), %r12
	movq	%r12, 24(%rsi)
	movq	40(%rcx), %r13
	movq	%r13, 32(%rsi)
	movq	48(%rcx), %r14
	movq	%r14, 40(%rsi)
	movq	56(%rcx), %rdx
	movq	%rdx, 48(%rsi)
	movq	64(%rcx), %r12
	movq	%r12, 56(%rsi)
	movq	72(%rcx), %r13
	movq	%r13, 64(%rsi)
	movq	80(%rcx), %r14
	movq	%r14, 72(%rsi)
	movq	88(%rcx), %rdx
	movq	%rdx, 80(%rsi)
	movq	96(%rcx), %r12
	movq	%r12, 88(%rsi)
	movq	104(%rcx), %r13
	movq	%r13, 96(%rsi)
	movq	112(%rcx), %r14
	movq	%r14, 104(%rsi)
	movq	120(%rcx), %rdx
	movq	%rdx, 112(%rsi)
	movq	128(%rcx), %r12
	movq	%r12, 120(%rsi)
	movsd	 136(%rcx), %xmm4
	movsd	 %xmm4, 128(%rsi)
	movsd	 144(%rcx), %xmm5
	movsd	 %xmm5, 136(%rsi)
	movsd	 152(%rcx), %xmm6
	movsd	 %xmm6, 144(%rsi)
	movq	160(%rcx), %r13
	movq	%r13, 152(%rsi)
	movsd	 168(%rcx), %xmm7
	movsd	 %xmm7, 160(%rsi)
	movsd	 176(%rcx), %xmm8
	movsd	 %xmm8, 168(%rsi)
	movsd	 184(%rcx), %xmm9
	movsd	 %xmm9, 176(%rsi)
	movq	192(%rcx), %r14
	movq	%r14, 184(%rsi)
	movsd	 200(%rcx), %xmm10
	movsd	 %xmm10, 192(%rsi)
	movsd	 208(%rcx), %xmm11
	movsd	 %xmm11, 200(%rsi)
	movsd	 216(%rcx), %xmm12
	movsd	 %xmm12, 208(%rsi)
	movq	224(%rcx), %rdx
	movq	%rdx, 216(%rsi)
	movq	%rsi, %r12
	addq	$232, %rsi
	movq	8(%rcx), %r13
	movq	(%r13), %rdi
	movq	%r15, %r8
	movq	%rbx, %r9
	movq	104(%rcx), %r13
	jmp	foldr_uncurried.40
doGC728:
	movq	$20, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%r10, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	%r14, %rdi
	movabsq	$retGC726, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.5BD:
	movsd	 %xmm4, %xmm5
	movsd	 %xmm3, %xmm4
	movsd	 %xmm2, %xmm3
	movq	%r8, %rcx
	movq	%rdi, %rdx
	movsd	 %xmm5, %xmm14
	movsd	 %xmm4, %xmm13
	movsd	 %xmm3, %xmm12
	jmp	gcTest72C
	/* live= GP={%rcx %rdx} FP={%xmm2 %xmm1 %xmm0} spilled=  */
retGC72B:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
	/* %f11951.d := mem.d[%rdi.64 +.64 32] */
	movsd	 32(%rdi), %xmm14
	/* %f11950.d := mem.d[%rdi.64 +.64 24] */
	movsd	 24(%rdi), %xmm13
	/* %f11949.d := mem.d[%rdi.64 +.64 16] */
	movsd	 16(%rdi), %xmm12
gcTest72C:
	movq	%r11, %r15
	subq	%rsi, %r15
	jle	doGC72D
	movsd	 %xmm14, %xmm2
	movsd	 %xmm13, %xmm1
	movsd	 %xmm12, %xmm0
check.729:
	/* Liveout:  GP={%r12 %r10 %r9 %r8 %rdi}  */
	/* block check<10DD2> (ep<10167>,norm<1014A>,_t<1014B>,_t<1014C>,_t<1014D>) */
	movq	160(%rdx), %r10
	movq	16(%r10), %rbx
	movsd	 %xmm2, %xmm8
	mulsd	 (%rbx), %xmm8
	movq	160(%rdx), %r13
	movq	8(%r13), %r12
	movsd	 %xmm1, %xmm9
	mulsd	 (%r12), %xmm9
	movq	160(%rdx), %r15
	movq	(%r15), %r14
	movsd	 %xmm0, %xmm7
	mulsd	 (%r14), %xmm7
	addsd	 %xmm9, %xmm7
	addsd	 %xmm8, %xmm7
	movabsq	$flt72A, %rbx
	movsd	 (%rbx), %xmm6
	mulsd	 %xmm7, %xmm6
	movsd	 %xmm6, %xmm10
	mulsd	 %xmm0, %xmm10
	movq	160(%rdx), %r12
	movq	(%r12), %r10
	movsd	 (%r10), %xmm7
	addsd	 %xmm10, %xmm7
	movq	$10, -8(%rsi)
	movsd	 %xmm7, (%rsi)
	movq	%rsi, %r12
	addq	$16, %rsi
	movsd	 %xmm6, %xmm11
	mulsd	 %xmm1, %xmm11
	movq	160(%rdx), %r14
	movq	8(%r14), %r13
	movsd	 (%r13), %xmm9
	addsd	 %xmm11, %xmm9
	movq	$10, -8(%rsi)
	movsd	 %xmm9, (%rsi)
	movq	%rsi, %r15
	addq	$16, %rsi
	movsd	 %xmm6, %xmm13
	mulsd	 %xmm2, %xmm13
	movq	160(%rdx), %r10
	movq	16(%r10), %rbx
	movsd	 (%rbx), %xmm12
	addsd	 %xmm13, %xmm12
	movq	$10, -8(%rsi)
	movsd	 %xmm12, (%rsi)
	movq	%rsi, %r13
	addq	$16, %rsi
	movq	$28, -8(%rsi)
	movq	%r12, (%rsi)
	movq	%r15, 8(%rsi)
	movq	%r13, 16(%rsi)
	movq	%rsi, %r13
	addq	$32, %rsi
	movq	$1638179, -8(%rsi)
	movsd	 24(%rdx), %xmm14
	movsd	 %xmm14, (%rsi)
	movq	32(%rdx), %r15
	movq	%r15, 8(%rsi)
	movq	40(%rdx), %rbx
	movq	%rbx, 16(%rsi)
	movq	48(%rdx), %r10
	movq	%r10, 24(%rsi)
	movq	56(%rdx), %r12
	movq	%r12, 32(%rsi)
	movq	64(%rdx), %r14
	movq	%r14, 40(%rsi)
	movq	96(%rdx), %r15
	movq	%r15, 48(%rsi)
	movq	112(%rdx), %rbx
	movq	%rbx, 56(%rsi)
	movq	120(%rdx), %r10
	movq	%r10, 64(%rsi)
	movsd	 176(%rdx), %xmm15
	movsd	 %xmm15, 72(%rsi)
	movsd	 184(%rdx), %xmm0
	movsd	 %xmm0, 80(%rsi)
	movsd	 192(%rdx), %xmm1
	movsd	 %xmm1, 88(%rsi)
	movq	200(%rdx), %r12
	movq	%r12, 96(%rsi)
	movq	%rcx, 104(%rsi)
	movsd	 %xmm7, 112(%rsi)
	movsd	 %xmm9, 120(%rsi)
	movsd	 %xmm12, 128(%rsi)
	movq	%rsi, %r14
	addq	$144, %rsi
	movq	$133, -8(%rsi)
	movq	%r14, (%rsi)
	movabsq	$lightray_P_.691, %r14
	movq	%r14, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	$36658216251, %r15
	movq	%r15, -8(%rsi)
	movabsq	$letJoinK.725, %rbx
	movq	%rbx, (%rsi)
	movq	8(%rdx), %r10
	movq	%r10, 8(%rsi)
	movsd	 24(%rdx), %xmm2
	movsd	 %xmm2, 16(%rsi)
	movq	56(%rdx), %r14
	movq	%r14, 24(%rsi)
	movq	72(%rdx), %r15
	movq	%r15, 32(%rsi)
	movq	80(%rdx), %rbx
	movq	%rbx, 40(%rsi)
	movq	88(%rdx), %r10
	movq	%r10, 48(%rsi)
	movq	96(%rdx), %r14
	movq	%r14, 56(%rsi)
	movq	104(%rdx), %r15
	movq	%r15, 64(%rsi)
	movq	112(%rdx), %rbx
	movq	%rbx, 72(%rsi)
	movq	120(%rdx), %r10
	movq	%r10, 80(%rsi)
	movq	128(%rdx), %r14
	movq	%r14, 88(%rsi)
	movq	136(%rdx), %r15
	movq	%r15, 96(%rsi)
	movq	144(%rdx), %rbx
	movq	%rbx, 104(%rsi)
	movq	152(%rdx), %r10
	movq	%r10, 112(%rsi)
	movq	160(%rdx), %r14
	movq	%r14, 120(%rsi)
	movq	168(%rdx), %r15
	movq	%r15, 128(%rsi)
	movsd	 176(%rdx), %xmm3
	movsd	 %xmm3, 136(%rsi)
	movsd	 184(%rdx), %xmm4
	movsd	 %xmm4, 144(%rsi)
	movsd	 192(%rdx), %xmm5
	movsd	 %xmm5, 152(%rsi)
	movq	200(%rdx), %rbx
	movq	%rbx, 160(%rsi)
	movsd	 208(%rdx), %xmm8
	movsd	 %xmm8, 168(%rsi)
	movsd	 216(%rdx), %xmm10
	movsd	 %xmm10, 176(%rsi)
	movsd	 224(%rdx), %xmm11
	movsd	 %xmm11, 184(%rsi)
	movq	%rcx, 192(%rsi)
	movsd	 %xmm7, 200(%rsi)
	movsd	 %xmm9, 208(%rsi)
	movsd	 %xmm12, 216(%rsi)
	movq	%r13, 224(%rsi)
	movq	%rsi, %r10
	addq	$240, %rsi
	movq	16(%rdx), %r13
	movq	(%r13), %rdi
	movq	%r12, %r8
	movq	152(%rdx), %r9
	movq	144(%rdx), %r12
	jmp	map_uncurried.5C
doGC72D:
	movq	$395, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movsd	 %xmm12, 16(%rsi)
	movsd	 %xmm13, 24(%rsi)
	movsd	 %xmm14, 32(%rsi)
	movq	%rsi, %r14
	addq	$48, %rsi
	movq	%r14, %rdi
	movabsq	$retGC72B, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.4D4:
	movsd	 %xmm4, %xmm5
	movsd	 %xmm3, %xmm4
	movsd	 %xmm2, %xmm3
	movq	%rdi, %rcx
	movsd	 %xmm5, %xmm15
	movsd	 %xmm4, %xmm14
	movsd	 %xmm3, %xmm13
	jmp	gcTest738
	/* live= GP={%rcx} FP={%xmm2 %xmm1 %xmm0} spilled=  */
retGC737:
	movq	(%rdi), %rcx
	/* %f12086.d := mem.d[%rdi.64 +.64 24] */
	movsd	 24(%rdi), %xmm15
	/* %f12085.d := mem.d[%rdi.64 +.64 16] */
	movsd	 16(%rdi), %xmm14
	/* %f12084.d := mem.d[%rdi.64 +.64 8] */
	movsd	 8(%rdi), %xmm13
gcTest738:
	movq	%r11, %rdx
	subq	%rsi, %rdx
	jle	doGC739
	movsd	 %xmm15, %xmm2
	movsd	 %xmm14, %xmm1
	movsd	 %xmm13, %xmm0
check.72E:
	/* block check<10DD7> (ep<1013E>,_t<1011E>,_t<1011F>,_t<10120>) */
	movq	$4831836987, %rbx
	movq	%rbx, -8(%rsi)
	movabsq	$letJoinK.5BD, %r10
	movq	%r10, (%rsi)
	movq	8(%rcx), %r12
	movq	%r12, 8(%rsi)
	movq	16(%rcx), %r13
	movq	%r13, 16(%rsi)
	movsd	 24(%rcx), %xmm6
	movsd	 %xmm6, 24(%rsi)
	movq	32(%rcx), %r14
	movq	%r14, 32(%rsi)
	movq	40(%rcx), %r15
	movq	%r15, 40(%rsi)
	movq	48(%rcx), %rdx
	movq	%rdx, 48(%rsi)
	movq	56(%rcx), %rbx
	movq	%rbx, 56(%rsi)
	movq	64(%rcx), %r10
	movq	%r10, 64(%rsi)
	movq	72(%rcx), %r12
	movq	%r12, 72(%rsi)
	movq	80(%rcx), %r13
	movq	%r13, 80(%rsi)
	movq	88(%rcx), %r14
	movq	%r14, 88(%rsi)
	movq	96(%rcx), %r15
	movq	%r15, 96(%rsi)
	movq	112(%rcx), %rdx
	movq	%rdx, 104(%rsi)
	movq	120(%rcx), %rbx
	movq	%rbx, 112(%rsi)
	movq	128(%rcx), %r10
	movq	%r10, 120(%rsi)
	movq	136(%rcx), %r12
	movq	%r12, 128(%rsi)
	movq	144(%rcx), %r13
	movq	%r13, 136(%rsi)
	movq	152(%rcx), %r14
	movq	%r14, 144(%rsi)
	movq	160(%rcx), %r15
	movq	%r15, 152(%rsi)
	movq	176(%rcx), %rdx
	movq	%rdx, 160(%rsi)
	movq	184(%rcx), %rbx
	movq	%rbx, 168(%rsi)
	movsd	 192(%rcx), %xmm7
	movsd	 %xmm7, 176(%rsi)
	movsd	 200(%rcx), %xmm8
	movsd	 %xmm8, 184(%rsi)
	movsd	 208(%rcx), %xmm9
	movsd	 %xmm9, 192(%rsi)
	movq	240(%rcx), %r10
	movq	%r10, 200(%rsi)
	movsd	 %xmm0, %xmm10
	mulsd	 216(%rcx), %xmm10
	movsd	 %xmm10, 208(%rsi)
	movsd	 %xmm1, %xmm11
	mulsd	 224(%rcx), %xmm11
	movsd	 %xmm11, 216(%rsi)
	movsd	 %xmm2, %xmm12
	mulsd	 232(%rcx), %xmm12
	movsd	 %xmm12, 224(%rsi)
	movq	%rsi, %rdx
	addq	$240, %rsi
	movq	168(%rcx), %r13
	movq	(%r13), %r12
	cmpq	$7, %r12
	jne	L73A
S_case72F:
case.730:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi} FP={%xmm4 %xmm3 %xmm2}  */
	/* block case<1071B> (ep<10719>,letJoinK<1071A>) */
	movq	104(%rcx), %r14
	movq	(%r14), %rdi
	/* %xmm2.d := mem.d[%r11967.64 +.64 192] */
	movsd	 192(%rcx), %xmm2
	/* %xmm3.d := mem.d[%r11967.64 +.64 200] */
	movsd	 200(%rcx), %xmm3
	/* %xmm4.d := mem.d[%r11967.64 +.64 208] */
	movsd	 208(%rcx), %xmm4
	movq	168(%rcx), %r15
	movq	8(%r15), %r8
	movq	%rdx, %r9
	movq	152(%rcx), %r10
	jmp	Primnorm.5BE
L73A:
	cmpq	$5, %r12
	jne	L73B
S_case731:
case.732:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi} FP={%xmm4 %xmm3 %xmm2}  */
	/* block case<10728> (ep<10726>,letJoinK<10727>) */
	movq	104(%rcx), %r12
	movq	(%r12), %rdi
	/* %xmm2.d := mem.d[%r11965.64 +.64 192] */
	movsd	 192(%rcx), %xmm2
	/* %xmm3.d := mem.d[%r11965.64 +.64 200] */
	movsd	 200(%rcx), %xmm3
	/* %xmm4.d := mem.d[%r11965.64 +.64 208] */
	movsd	 208(%rcx), %xmm4
	movq	168(%rcx), %r13
	movq	8(%r13), %r8
	movq	%rdx, %r9
	movq	152(%rcx), %r10
	jmp	Primnorm.5BE
L73B:
	cmpq	$3, %r12
	jne	L73C
S_case733:
case.734:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi} FP={%xmm4 %xmm3 %xmm2}  */
	/* block case<10735> (ep<10733>,letJoinK<10734>) */
	movq	104(%rcx), %rbx
	movq	(%rbx), %rdi
	/* %xmm2.d := mem.d[%r11963.64 +.64 192] */
	movsd	 192(%rcx), %xmm2
	/* %xmm3.d := mem.d[%r11963.64 +.64 200] */
	movsd	 200(%rcx), %xmm3
	/* %xmm4.d := mem.d[%r11963.64 +.64 208] */
	movsd	 208(%rcx), %xmm4
	movq	168(%rcx), %r10
	movq	8(%r10), %r8
	movq	%rdx, %r9
	movq	152(%rcx), %r10
	jmp	Primnorm.5BE
L73C:
	cmpq	$1, %r12
	jne	S_case72F
S_case735:
case.736:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi} FP={%xmm4 %xmm3 %xmm2}  */
	/* block case<10742> (ep<10740>,letJoinK<10741>) */
	movq	104(%rcx), %r14
	movq	(%r14), %rdi
	/* %xmm2.d := mem.d[%r11961.64 +.64 192] */
	movsd	 192(%rcx), %xmm2
	/* %xmm3.d := mem.d[%r11961.64 +.64 200] */
	movsd	 200(%rcx), %xmm3
	/* %xmm4.d := mem.d[%r11961.64 +.64 208] */
	movsd	 208(%rcx), %xmm4
	movq	168(%rcx), %r15
	movq	8(%r15), %r8
	movq	%rdx, %r9
	movq	152(%rcx), %r10
	jmp	Primnorm.5BE
doGC739:
	movq	$137, -8(%rsi)
	movq	%rcx, (%rsi)
	movsd	 %xmm13, 8(%rsi)
	movsd	 %xmm14, 16(%rsi)
	movsd	 %xmm15, 24(%rsi)
	movq	%rsi, %rcx
	addq	$40, %rsi
	movq	%rcx, %rdi
	movabsq	$retGC737, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.5A8:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest73F
	/* live= GP={%rcx %rdx} spilled=  */
retGC73E:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest73F:
	movq	%r11, %r13
	subq	%rsi, %r13
	jle	doGC740
check.73D:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block check<10DDA> (ep<1011B>,surf<10107>) */
	movq	$139586435903, %r10
	movq	%r10, -8(%rsi)
	movabsq	$letJoinK.4D4, %r12
	movq	%r12, (%rsi)
	movq	8(%rdx), %r13
	movq	%r13, 8(%rsi)
	movq	16(%rdx), %r14
	movq	%r14, 16(%rsi)
	movsd	 24(%rdx), %xmm0
	movsd	 %xmm0, 24(%rsi)
	movq	32(%rdx), %r15
	movq	%r15, 32(%rsi)
	movq	40(%rdx), %rbx
	movq	%rbx, 40(%rsi)
	movq	56(%rdx), %r10
	movq	%r10, 48(%rsi)
	movq	64(%rdx), %r12
	movq	%r12, 56(%rsi)
	movq	72(%rdx), %r13
	movq	%r13, 64(%rsi)
	movq	80(%rdx), %r14
	movq	%r14, 72(%rsi)
	movq	88(%rdx), %r15
	movq	%r15, 80(%rsi)
	movq	96(%rdx), %rbx
	movq	%rbx, 88(%rsi)
	movq	104(%rdx), %r10
	movq	%r10, 96(%rsi)
	movq	112(%rdx), %r12
	movq	%r12, 104(%rsi)
	movq	120(%rdx), %r13
	movq	%r13, 112(%rsi)
	movq	128(%rdx), %r14
	movq	%r14, 120(%rsi)
	movq	136(%rdx), %r15
	movq	%r15, 128(%rsi)
	movq	144(%rdx), %rbx
	movq	%rbx, 136(%rsi)
	movq	152(%rdx), %r10
	movq	%r10, 144(%rsi)
	movq	160(%rdx), %r12
	movq	%r12, 152(%rsi)
	movq	168(%rdx), %r13
	movq	%r13, 160(%rsi)
	movq	176(%rdx), %r14
	movq	%r14, 168(%rsi)
	movq	184(%rdx), %r15
	movq	%r15, 176(%rsi)
	movq	192(%rdx), %rbx
	movq	%rbx, 184(%rsi)
	movsd	 200(%rdx), %xmm1
	movsd	 %xmm1, 192(%rsi)
	movsd	 208(%rdx), %xmm2
	movsd	 %xmm2, 200(%rsi)
	movsd	 216(%rdx), %xmm3
	movsd	 %xmm3, 208(%rsi)
	movsd	 224(%rdx), %xmm4
	movsd	 %xmm4, 216(%rsi)
	movsd	 232(%rdx), %xmm5
	movsd	 %xmm5, 224(%rsi)
	movsd	 240(%rdx), %xmm6
	movsd	 %xmm6, 232(%rsi)
	movq	%rcx, 240(%rsi)
	movq	%rsi, %rbx
	addq	$256, %rsi
	movq	48(%rdx), %r10
	movq	(%r10), %rdi
	movq	%rcx, %r8
	movq	%rbx, %r9
	movq	160(%rdx), %r10
	jmp	ambientsurf.4DC
doGC740:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	%r12, %rdi
	movabsq	$retGC73E, %r8
	jmp	ASM_InvokeGC
	.text
shade.6AA:
	movsd	 %xmm7, %xmm0
	movsd	 %xmm6, %xmm15
	movsd	 %xmm5, %xmm14
	movsd	 %xmm4, %xmm13
	movsd	 %xmm3, %xmm12
	movsd	 %xmm2, %xmm11
	movq	%r9, %rdx
	movq	%r8, %rcx
	movq	%rdi, %rbx
	movq	%r15, -56(%rbp)
	movq	%r12, %r15
	movq	%rdx, %r12
	movsd	 %xmm0, %xmm5
	movsd	 %xmm15, %xmm4
	movsd	 %xmm14, %xmm3
	movsd	 %xmm13, %xmm2
	movsd	 %xmm12, %xmm1
	movsd	 %xmm11, %xmm0
	jmp	gcTest74B
	/* live= GP={%rdx %r14 %r13 %r10 %r15 %rcx %r12} FP={%xmm11 %xmm10 %xmm9 %xmm8 %xmm7 %xmm6} spilled= GP={%r~1}  */
retGC74A:
	movq	104(%rdi), %r12
	movq	%r12, -56(%rbp)
	movq	96(%rdi), %r14
	movq	88(%rdi), %r13
	movq	80(%rdi), %r15
	movq	48(%rdi), %r10
	movq	16(%rdi), %r12
	movq	8(%rdi), %rcx
	movq	(%rdi), %rbx
	/* %f12317.d := mem.d[%rdi.64 +.64 72] */
	movsd	 72(%rdi), %xmm5
	/* %f12316.d := mem.d[%rdi.64 +.64 64] */
	movsd	 64(%rdi), %xmm4
	/* %f12315.d := mem.d[%rdi.64 +.64 56] */
	movsd	 56(%rdi), %xmm3
	/* %f12313.d := mem.d[%rdi.64 +.64 40] */
	movsd	 40(%rdi), %xmm2
	/* %f12312.d := mem.d[%rdi.64 +.64 32] */
	movsd	 32(%rdi), %xmm1
	/* %f12311.d := mem.d[%rdi.64 +.64 24] */
	movsd	 24(%rdi), %xmm0
gcTest74B:
	movq	%r11, %rdx
	subq	%rsi, %rdx
	jle	doGC74C
	movq	-56(%rbp), %rdx
	movq	%r15, -56(%rbp)
	movq	%r12, %r15
	movq	%rbx, %r12
	movsd	 %xmm5, %xmm11
	movsd	 %xmm4, %xmm10
	movsd	 %xmm3, %xmm9
	movsd	 %xmm2, %xmm8
	movsd	 %xmm1, %xmm7
	movsd	 %xmm0, %xmm6
check.741:
	/* block check<10DE9> (ep<100EB>,lights<100EC>,sp<100ED>,_t<100EE>,_t<100EF>,_t<100F0>,dir<100F1>,_t<100F2>,_t<100F3>,_t<100F4>,contrib<100F6>,retK<100F7>,_exh<100F8>,argFormalWrap<10ABE>) */
	movsd	 (%rdx), %xmm1
	movq	$133, -8(%rsi)
	movq	%r12, (%rsi)
	movabsq	$shade.6AA, %rdx
	movq	%rdx, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	movq	$4294966079, %rbx
	movq	%rbx, -8(%rsi)
	movabsq	$letJoinK.5A8, %rbx
	movq	%rbx, (%rsi)
	movq	(%r12), %rbx
	movq	%rbx, 8(%rsi)
	movq	8(%r12), %rbx
	movq	%rbx, 16(%rsi)
	movsd	 16(%r12), %xmm2
	movsd	 %xmm2, 24(%rsi)
	movq	24(%r12), %rbx
	movq	%rbx, 32(%rsi)
	movq	32(%r12), %rbx
	movq	%rbx, 40(%rsi)
	movq	40(%r12), %rbx
	movq	%rbx, 48(%rsi)
	movq	48(%r12), %rbx
	movq	%rbx, 56(%rsi)
	movq	56(%r12), %rbx
	movq	%rbx, 64(%rsi)
	movq	64(%r12), %rbx
	movq	%rbx, 72(%rsi)
	movq	72(%r12), %rbx
	movq	%rbx, 80(%rsi)
	movq	80(%r12), %rbx
	movq	%rbx, 88(%rsi)
	movq	88(%r12), %rbx
	movq	%rbx, 96(%rsi)
	movq	96(%r12), %rbx
	movq	%rbx, 104(%rsi)
	movq	112(%r12), %rbx
	movq	%rbx, 112(%rsi)
	movq	120(%r12), %rbx
	movq	%rbx, 120(%rsi)
	movq	128(%r12), %rbx
	movq	%rbx, 128(%rsi)
	movq	136(%r12), %rbx
	movq	%rbx, 136(%rsi)
	movq	%rdx, 144(%rsi)
	movq	%r13, 152(%rsi)
	movq	%r14, 160(%rsi)
	movq	%rcx, 168(%rsi)
	movq	%r15, 176(%rsi)
	movq	%r10, 184(%rsi)
	movq	-56(%rbp), %rbx
	movq	%rbx, 192(%rsi)
	movsd	 %xmm1, %xmm4
	mulsd	 %xmm9, %xmm4
	movsd	 %xmm6, %xmm3
	addsd	 %xmm4, %xmm3
	movsd	 %xmm3, 200(%rsi)
	movsd	 %xmm1, %xmm12
	mulsd	 %xmm10, %xmm12
	movsd	 %xmm7, %xmm5
	addsd	 %xmm12, %xmm5
	movsd	 %xmm5, 208(%rsi)
	movsd	 %xmm1, %xmm14
	mulsd	 %xmm11, %xmm14
	movsd	 %xmm8, %xmm13
	addsd	 %xmm14, %xmm13
	movsd	 %xmm13, 216(%rsi)
	movabsq	$flt536, %r10
	movsd	 (%r10), %xmm15
	movsd	 %xmm15, 224(%rsi)
	movabsq	$flt536, %r13
	movsd	 (%r13), %xmm0
	movsd	 %xmm0, 232(%rsi)
	movabsq	$flt536, %rcx
	movsd	 (%rcx), %xmm1
	movsd	 %xmm1, 240(%rsi)
	movq	%rsi, %rcx
	addq	$256, %rsi
	movq	(%r15), %rdx
	cmpq	$7, %rdx
	jne	L74D
S_case742:
	movq	%r15, %r13
	movq	%r14, %r10
case.743:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block case<10755> (ep<10751>,_exh<10754>,sp<10753>,letJoinK<10752>) */
	movq	104(%r12), %r14
	movq	(%r14), %rdi
	movq	8(%r13), %r8
	movq	%rcx, %r9
	jmp	Primsurf.5A9
L74D:
	cmpq	$5, %rdx
	jne	L74E
S_case744:
	movq	%r15, %r10
	movq	%r14, %r13
case.745:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block case<1075F> (ep<1075B>,_exh<1075E>,sp<1075D>,letJoinK<1075C>) */
	movq	104(%r12), %r12
	movq	(%r12), %rdi
	movq	8(%r10), %r8
	movq	%rcx, %r9
	movq	%r13, %r10
	jmp	Primsurf.5A9
L74E:
	cmpq	$3, %rdx
	jne	L74F
S_case746:
	movq	%r15, %rbx
case.747:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block case<10769> (ep<10765>,_exh<10768>,sp<10767>,letJoinK<10766>) */
	movq	104(%r12), %r10
	movq	(%r10), %rdi
	movq	8(%rbx), %r8
	movq	%rcx, %r9
	movq	%r14, %r10
	jmp	Primsurf.5A9
L74F:
	cmpq	$1, %rdx
	jne	S_case742
S_case748:
	movq	%r15, %rdx
	movq	%r14, %r10
case.749:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block case<10773> (ep<1076F>,_exh<10772>,sp<10771>,letJoinK<10770>) */
	movq	104(%r12), %rbx
	movq	(%rbx), %rdi
	movq	8(%rdx), %r8
	movq	%rcx, %r9
	jmp	Primsurf.5A9
doGC74C:
	movq	$1975197, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%r12, 16(%rsi)
	movsd	 %xmm0, 24(%rsi)
	movsd	 %xmm1, 32(%rsi)
	movsd	 %xmm2, 40(%rsi)
	movq	%r10, 48(%rsi)
	movsd	 %xmm3, 56(%rsi)
	movsd	 %xmm4, 64(%rsi)
	movsd	 %xmm5, 72(%rsi)
	movq	%r15, 80(%rsi)
	movq	%r13, 88(%rsi)
	movq	%r14, 96(%rsi)
	movq	-56(%rbp), %r10
	movq	%r10, 104(%rsi)
	movq	%rsi, %r15
	addq	$120, %rsi
	movq	%r15, %rdi
	movabsq	$retGC74A, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.755:
	movq	%r9, %rcx
	movq	%r8, %rbx
	movq	%rdi, %rdx
gcTest757:
	movq	%r11, %r14
	subq	%rsi, %r14
	jle	doGC758
check.750:
	/* block check<10DEE> (ep<108FE>,hit<108F3>,dist<108F4>,sp<108F5>) */
	cmpq	$1, %rbx
	jne	L759
S_case751:
case.752:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block case<10902> (ep<10901>) */
	movq	56(%rdx), %r10
	movq	(%r10), %r12
	movq	%r10, %rdi
	movq	40(%rdx), %r8
	jmp	*%r12
doGC758:
	movq	$36, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rbx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %r13
	addq	$40, %rsi
	movq	%r13, %rdi
	movabsq	$retGC756, %r8
	jmp	ASM_InvokeGC
	/* live= GP={%r10 %rcx %rbx %rdx} spilled=  */
retGC756:
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rbx
	movq	(%rdi), %rdx
	jmp	gcTest757
L759:
	cmpq	$3, %rbx
	jne	S_case751
S_case753:
case.754:
	/* Liveout:  GP={%r15 %r14 %r13 %r12 %r10 %r9 %r8 %rdi} FP={%xmm7 %xmm6 %xmm5 %xmm4
 %xmm3 %xmm2}  */
	/* block case<10909> (ep<10906>,dist<10908>,sp<10907>) */
	movq	$10, -8(%rsi)
	movabsq	$flt536, %r13
	movsd	 (%r13), %xmm0
	movsd	 %xmm0, (%rsi)
	movq	%rsi, %r12
	addq	$16, %rsi
	movq	$10, -8(%rsi)
	movabsq	$flt536, %r15
	movsd	 (%r15), %xmm1
	movsd	 %xmm1, (%rsi)
	movq	%rsi, %r14
	addq	$16, %rsi
	movq	$10, -8(%rsi)
	movabsq	$flt536, %r13
	movsd	 (%r13), %xmm2
	movsd	 %xmm2, (%rsi)
	movq	%rsi, %rbx
	addq	$16, %rsi
	movq	$28, -8(%rsi)
	movq	%r12, (%rsi)
	movq	%r14, 8(%rsi)
	movq	%rbx, 16(%rsi)
	movq	%rsi, %r12
	addq	$32, %rsi
	movq	$10, -8(%rsi)
	movsd	 (%rcx), %xmm3
	movsd	 %xmm3, (%rsi)
	movq	%rsi, %r15
	addq	$16, %rsi
	movq	48(%rdx), %r14
	movq	(%r14), %rdi
	movq	8(%rdx), %r8
	movq	%r10, %r9
	/* %xmm2.d := mem.d[%r12334.64 +.64 16] */
	movsd	 16(%rdx), %xmm2
	/* %xmm3.d := mem.d[%r12334.64 +.64 24] */
	movsd	 24(%rdx), %xmm3
	/* %xmm4.d := mem.d[%r12334.64 +.64 32] */
	movsd	 32(%rdx), %xmm4
	movq	72(%rdx), %r10
	movq	72(%rdx), %rbx
	movq	(%rbx), %rcx
	/* %xmm5.d := mem.d[(mem.64[(mem.64[%r12334.64 +.64 72]) +.64 0]) +.64 0] */
	movsd	 (%rcx), %xmm5
	movq	72(%rdx), %r14
	movq	8(%r14), %r13
	/* %xmm6.d := mem.d[(mem.64[(mem.64[%r12334.64 +.64 72]) +.64 8]) +.64 0] */
	movsd	 (%r13), %xmm6
	movq	72(%rdx), %rbx
	movq	16(%rbx), %rcx
	/* %xmm7.d := mem.d[(mem.64[(mem.64[%r12334.64 +.64 72]) +.64 16]) +.64 0] */
	movsd	 (%rcx), %xmm7
	movq	56(%rdx), %r13
	movq	64(%rdx), %r14
	jmp	shade.6AA
	.text
letJoinK.75B:
	movq	%r9, %rcx
	movsd	 %xmm4, %xmm5
	movsd	 %xmm3, %xmm4
	movsd	 %xmm2, %xmm3
	movq	%r8, %r10
	movq	%rdi, %rdx
	movsd	 %xmm5, %xmm11
	movsd	 %xmm4, %xmm10
	movsd	 %xmm3, %xmm9
	jmp	gcTest75D
	/* live= GP={%rcx %r10 %rdx} FP={%xmm2 %xmm1 %xmm0} spilled=  */
retGC75C:
	movq	40(%rdi), %rcx
	movq	8(%rdi), %r10
	movq	(%rdi), %rdx
	/* %f12440.d := mem.d[%rdi.64 +.64 32] */
	movsd	 32(%rdi), %xmm11
	/* %f12439.d := mem.d[%rdi.64 +.64 24] */
	movsd	 24(%rdi), %xmm10
	/* %f12438.d := mem.d[%rdi.64 +.64 16] */
	movsd	 16(%rdi), %xmm9
gcTest75D:
	movq	%r11, %r12
	subq	%rsi, %r12
	jle	doGC75E
	movsd	 %xmm11, %xmm2
	movsd	 %xmm10, %xmm1
	movsd	 %xmm9, %xmm0
check.75A:
	/* Liveout:  GP={%r13 %r12 %r10 %r9 %r8 %rdi}  */
	/* block check<10DF5> (ep<108F0>,dir<108E2>,unused<108E3>,unused<108E4>,unused<108E5>,unused<108E6>) */
	movq	$127253, -8(%rsi)
	movabsq	$letJoinK.755, %rbx
	movq	%rbx, (%rsi)
	movq	8(%rdx), %r12
	movq	%r12, 8(%rsi)
	movsd	 16(%rdx), %xmm6
	movsd	 %xmm6, 16(%rsi)
	movsd	 24(%rdx), %xmm7
	movsd	 %xmm7, 24(%rsi)
	movsd	 32(%rdx), %xmm8
	movsd	 %xmm8, 32(%rsi)
	movq	48(%rdx), %r13
	movq	%r13, 40(%rsi)
	movq	72(%rdx), %r14
	movq	%r14, 48(%rsi)
	movq	80(%rdx), %r15
	movq	%r15, 56(%rsi)
	movq	88(%rdx), %rcx
	movq	%rcx, 64(%rsi)
	movq	%r10, 72(%rsi)
	movq	%rsi, %r12
	addq	$88, %rsi
	movq	64(%rdx), %rbx
	movq	(%rbx), %rdi
	movq	56(%rdx), %r8
	movq	40(%rdx), %r9
	movq	88(%rdx), %r13
	jmp	trace.646
doGC75E:
	movq	$4493, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%r10, 8(%rsi)
	movsd	 %xmm9, 16(%rsi)
	movsd	 %xmm10, 24(%rsi)
	movsd	 %xmm11, 32(%rsi)
	movq	%rcx, 40(%rsi)
	movq	%rsi, %r10
	addq	$56, %rsi
	movq	%r10, %rdi
	movabsq	$retGC75C, %r8
	jmp	ASM_InvokeGC
	.text
anon.760:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest762
	/* live= GP={%r10 %rcx %rdx %rbx} spilled=  */
retGC761:
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest762:
	movq	%r11, %r12
	subq	%rsi, %r12
	jle	doGC763
check.75F:
	/* Liveout:  GP={%r8 %rdi} FP={%xmm4 %xmm3 %xmm2}  */
	/* block check<10DFA> (ep<108C1>,x1<108C2>,retK<108C3>,_exh<108C4>) */
	movq	152(%rbx), %r12
	cvtsi2sd	 (%r12), %xmm0
	cvtsi2sd	 (%rdx), %xmm1
	movq	$520473, -8(%rsi)
	movabsq	$letJoinK.75B, %r14
	movq	%r14, (%rsi)
	movq	8(%rbx), %r15
	movq	%r15, 8(%rsi)
	movsd	 16(%rbx), %xmm2
	movsd	 %xmm2, 16(%rsi)
	movsd	 24(%rbx), %xmm3
	movsd	 %xmm3, 24(%rsi)
	movsd	 32(%rbx), %xmm4
	movsd	 %xmm4, 32(%rsi)
	movq	40(%rbx), %rdx
	movq	%rdx, 40(%rsi)
	movq	48(%rbx), %r12
	movq	%r12, 48(%rsi)
	movq	56(%rbx), %r13
	movq	%r13, 56(%rsi)
	movq	64(%rbx), %r14
	movq	%r14, 64(%rsi)
	movq	72(%rbx), %r15
	movq	%r15, 72(%rsi)
	movq	%rcx, 80(%rsi)
	movq	%r10, 88(%rsi)
	movq	%rsi, %r13
	addq	$104, %rsi
	movq	(%rbx), %rcx
	movq	(%rcx), %rdi
	movsd	 %xmm1, %xmm5
	mulsd	 104(%rbx), %xmm5
	movsd	 %xmm0, %xmm6
	mulsd	 80(%rbx), %xmm6
	movsd	 128(%rbx), %xmm2
	addsd	 %xmm6, %xmm2
	/* %xmm2.d := fadd.d(fadd.d(mem.d[%r12447.64 +.64 128],fmul.d(%f12455.d,mem.d[%r12447.64 +.64 80])),fmul.d(%f12457.d,mem.d[%r12447.64 +.64 104])) */
	addsd	 %xmm5, %xmm2
	movsd	 %xmm1, %xmm7
	mulsd	 112(%rbx), %xmm7
	movsd	 %xmm0, %xmm8
	mulsd	 88(%rbx), %xmm8
	movsd	 136(%rbx), %xmm3
	addsd	 %xmm8, %xmm3
	/* %xmm3.d := fadd.d(fadd.d(mem.d[%r12447.64 +.64 136],fmul.d(%f12455.d,mem.d[%r12447.64 +.64 88])),fmul.d(%f12457.d,mem.d[%r12447.64 +.64 112])) */
	addsd	 %xmm7, %xmm3
	movsd	 %xmm1, %xmm9
	mulsd	 120(%rbx), %xmm9
	movsd	 %xmm0, %xmm10
	mulsd	 96(%rbx), %xmm10
	movsd	 144(%rbx), %xmm4
	addsd	 %xmm10, %xmm4
	/* %xmm4.d := fadd.d(fadd.d(mem.d[%r12447.64 +.64 144],fmul.d(%f12455.d,mem.d[%r12447.64 +.64 96])),fmul.d(%f12457.d,mem.d[%r12447.64 +.64 120])) */
	addsd	 %xmm9, %xmm4
	movq	%r13, %r8
	jmp	vecnorm.4C1
doGC763:
	movq	$36, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %rdx
	addq	$40, %rsi
	movq	%rdx, %rdi
	movabsq	$retGC761, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.765:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest767
	/* live= GP={%rcx %rdx} spilled=  */
retGC766:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest767:
	movq	%r11, %r12
	subq	%rsi, %r12
	jle	doGC768
check.764:
	/* Liveout:  GP={%r12 %r10 %r9 %r8 %rdi}  */
	/* block check<10DFD> (ep<10937>,_t<10935>) */
	movq	8(%rdx), %rbx
	movq	(%rbx), %rdi
	movq	32(%rdx), %r8
	movq	%rcx, %r9
	movq	16(%rdx), %r10
	movq	24(%rdx), %r12
	jmp	mapP.4BC
doGC768:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	%r10, %rdi
	movabsq	$retGC766, %r8
	jmp	ASM_InvokeGC
	.text
anon.76A:
	movq	%r9, %rcx
	movq	%r8, %r12
	movq	%rdi, %r15
	jmp	gcTest76C
	/* live= GP={%r14 %r12 %r15} spilled= GP={%r~1}  */
retGC76B:
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %r12
	movq	(%rdi), %r15
gcTest76C:
	movq	%r11, %rdx
	subq	%rsi, %rdx
	jle	doGC76D
	movq	%r10, %r14
	movq	%rcx, -56(%rbp)
check.769:
	/* Liveout:  GP={%r14 %r13 %r12 %r10 %r9 %r8 %rdi}  */
	/* block check<10E02> (ep<108A5>,x1<108A6>,retK<108A7>,_exh<108A8>) */
	movq	$67236265, -8(%rsi)
	movq	16(%r15), %rbx
	movq	%rbx, (%rsi)
	movq	24(%r15), %r10
	movq	%r10, 8(%rsi)
	movsd	 32(%r15), %xmm0
	movsd	 %xmm0, 16(%rsi)
	movsd	 40(%r15), %xmm1
	movsd	 %xmm1, 24(%rsi)
	movsd	 48(%r15), %xmm2
	movsd	 %xmm2, 32(%rsi)
	movq	56(%r15), %r13
	movq	%r13, 40(%rsi)
	movq	64(%r15), %rcx
	movq	%rcx, 48(%rsi)
	movq	72(%r15), %rdx
	movq	%rdx, 56(%rsi)
	movq	80(%r15), %rbx
	movq	%rbx, 64(%rsi)
	movq	88(%r15), %r10
	movq	%r10, 72(%rsi)
	movsd	 104(%r15), %xmm3
	movsd	 %xmm3, 80(%rsi)
	movsd	 112(%r15), %xmm4
	movsd	 %xmm4, 88(%rsi)
	movsd	 120(%r15), %xmm5
	movsd	 %xmm5, 96(%rsi)
	movsd	 128(%r15), %xmm6
	movsd	 %xmm6, 104(%rsi)
	movsd	 136(%r15), %xmm7
	movsd	 %xmm7, 112(%rsi)
	movsd	 144(%r15), %xmm8
	movsd	 %xmm8, 120(%rsi)
	movsd	 152(%r15), %xmm9
	movsd	 %xmm9, 128(%rsi)
	movsd	 160(%r15), %xmm10
	movsd	 %xmm10, 136(%rsi)
	movsd	 168(%r15), %xmm11
	movsd	 %xmm11, 144(%rsi)
	movq	%r12, 152(%rsi)
	movq	%rsi, %rdx
	addq	$168, %rsi
	movq	$133, -8(%rsi)
	movq	%rdx, (%rsi)
	movabsq	$anon.760, %r12
	movq	%r12, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	xorl	%r10d, %r10d
	movq	$10, -8(%rsi)
	movl	%r10d, (%rsi)
	movq	%rsi, %rcx
	addq	$16, %rsi
	movl	96(%r15), %r12d
	decl	%r12d
	movq	$10, -8(%rsi)
	movl	%r12d, (%rsi)
	movq	%rsi, %r13
	addq	$16, %rsi
	movq	$20, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%r13, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	$3851, -8(%rsi)
	movabsq	$letJoinK.765, %r13
	movq	%r13, (%rsi)
	movq	8(%r15), %r13
	movq	%r13, 8(%rsi)
	movq	-56(%rbp), %r13
	movq	%r13, 16(%rsi)
	movq	%r14, 24(%rsi)
	movq	%rdx, 32(%rsi)
	movq	%rsi, %r13
	addq	$48, %rsi
	movq	(%r15), %r15
	movq	(%r15), %rdi
	movq	%rbx, %r8
	movq	%rcx, %r9
	jmp	rangePNoStep.3C5
doGC76D:
	movq	$36, -8(%rsi)
	movq	%r15, (%rsi)
	movq	%r12, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %rcx
	addq	$40, %rsi
	movq	%rcx, %rdi
	movabsq	$retGC76B, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.771:
	movq	%r8, %r14
	movq	%rdi, %r15
	jmp	gcTest773
	/* live= GP={%r14 %r15} spilled=  */
retGC772:
	movq	8(%rdi), %r14
	movq	(%rdi), %r15
gcTest773:
	movq	%r11, %rbx
	subq	%rsi, %rbx
	jle	doGC774
check.76E:
	/* Liveout:  GP={%r12 %r10 %r9 %r8 %rdi}  */
	/* block check<10E05> (ep<1099B>,_tpl<10994>) */
	movq	$-1048576, %rbx
	andq	%rsi, %rbx
	movq	$1, (%rbx)
	movq	%rax, %r12
	movq	%rcx, -56(%rbp)
	movq	%rdx, -96(%rbp)
	movq	%rsi, %r13
	movq	%rdi, -104(%rbp)
	movq	%r8, -64(%rbp)
	movq	%r9, -88(%rbp)
	movq	%r10, -80(%rbp)
	movq	%r11, -72(%rbp)
	movq	8(%r15), %rcx
	movq	%rcx, %rdi
	movslq	16(%r15), %rdx
	movq	%rdx, %rsi
	movslq	48(%r15), %rdx
	movq	(%r14), %r10
	movsd	 (%r10), %xmm0
	movsd	 %xmm0, %xmm0
	movq	8(%r14), %rcx
	movsd	 (%rcx), %xmm1
	movsd	 %xmm1, %xmm1
	movq	16(%r14), %r10
	movsd	 (%r10), %xmm2
	movsd	 %xmm2, %xmm2
	call	M_UpdateImage3d
	movq	%r12, %rax
	movq	-56(%rbp), %rcx
	movq	-96(%rbp), %rdx
	movq	%r13, %rsi
	movq	-104(%rbp), %rdi
	movq	-64(%rbp), %r8
	movq	-88(%rbp), %r9
	movq	-80(%rbp), %r10
	movq	-72(%rbp), %r11
	movq	$3, (%rbx)
	movl	48(%r15), %r12d
	incl	%r12d
	movq	$10, -8(%rsi)
	movl	%r12d, (%rsi)
	movq	%rsi, %r13
	addq	$16, %rsi
	movq	24(%r15), %r14
	movq	(%r14), %rdi
	movq	%r13, %r8
	movq	%r12, %r9
	movq	32(%r15), %r10
	movq	40(%r15), %r12
	jmp	loop.770
doGC774:
	movq	$20, -8(%rsi)
	movq	%r15, (%rsi)
	movq	%r14, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	%r15, %rdi
	movabsq	$retGC772, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.777:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest779
	/* live= GP={%rcx %rdx} spilled=  */
retGC778:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest779:
	movq	%r11, %r15
	subq	%rsi, %r15
	jle	doGC77A
check.775:
	/* Liveout:  GP={%r13 %r12 %r10 %r9 %r8 %rdi}  */
	/* block check<10E08> (ep<10991>,_t<1098B>) */
	movq	$7439, -8(%rsi)
	movabsq	$letJoinK.771, %rbx
	movq	%rbx, (%rsi)
	movq	16(%rdx), %r10
	movq	%r10, 8(%rsi)
	movl	24(%rdx), %r12d
	movl	%r12d, 16(%rsi)
	movq	32(%rdx), %r13
	movq	%r13, 24(%rsi)
	movq	48(%rdx), %r14
	movq	%r14, 32(%rsi)
	movq	56(%rdx), %r15
	movq	%r15, 40(%rsi)
	movl	64(%rdx), %ebx
	movl	%ebx, 48(%rsi)
	movq	%rsi, %r12
	addq	$64, %rsi
	movq	8(%rdx), %r10
	movq	(%r10), %rdi
	movq	%rcx, %r8
	movq	40(%rdx), %r9
	movq	40(%rdx), %r13
	movl	(%r13), %r10d
	movq	56(%rdx), %r13
	jmp	sub.776
doGC77A:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	%r14, %rdi
	movabsq	$retGC778, %r8
	jmp	ASM_InvokeGC
	.text
loop.770:
	movq	%r9, %rcx
	movq	%r8, %rbx
	movq	%rdi, %rdx
	jmp	gcTest781
	/* live= GP={%r13 %r10 %rcx %rbx %rdx} spilled=  */
retGC780:
	movq	32(%rdi), %r12
	movq	24(%rdi), %r10
	movl	16(%rdi), %ecx
	movq	8(%rdi), %rbx
	movq	(%rdi), %rdx
gcTest781:
	movq	%r11, %r13
	subq	%rsi, %r13
	jle	doGC782
	movq	%r12, %r13
check.77B:
	/* block check<10E0E> (ep<1097C>,j<1097D>,_t<1097E>,retK<1097F>,_exh<10980>) */
	cmpl	8(%rdx), %ecx
	jge	L783
L_true77C:
then.77E:
	/* Liveout:  GP={%r13 %r12 %r10 %r9 %r8 %rdi}  */
	/* block then<1098A> (ep<10985>,j<10989>,retK<10988>,_exh<10987>,_t<10986>) */
	movq	$133, -8(%rsi)
	movq	%rdx, (%rsi)
	movabsq	$loop.770, %r12
	movq	%r12, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	$31507, -8(%rsi)
	movabsq	$letJoinK.777, %r14
	movq	%r14, (%rsi)
	movq	(%rdx), %r15
	movq	%r15, 8(%rsi)
	movq	16(%rdx), %r14
	movq	%r14, 16(%rsi)
	movl	48(%rdx), %r15d
	movl	%r15d, 24(%rsi)
	movq	%r12, 32(%rsi)
	movq	%rbx, 40(%rsi)
	movq	%r10, 48(%rsi)
	movq	%r13, 56(%rsi)
	movl	%ecx, 64(%rsi)
	movq	%rsi, %r12
	addq	$80, %rsi
	movq	(%rdx), %rcx
	movq	(%rcx), %rdi
	movq	24(%rdx), %r8
	movq	40(%rdx), %r9
	movq	40(%rdx), %rdx
	movl	(%rdx), %r10d
	jmp	sub.776
doGC782:
	movq	$3467, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rbx, 8(%rsi)
	movl	%ecx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%r12, 32(%rsi)
	movq	%rsi, %rbx
	addq	$48, %rsi
	movq	%rbx, %rdi
	movabsq	$retGC780, %r8
	jmp	ASM_InvokeGC
L783:
else.77D:
	/* Liveout:  GP={%r12 %r10 %r9 %r8 %rdi}  */
	/* block else<109C2> (ep<109BF>,retK<109C1>,_exh<109C0>) */
	movl	48(%rdx), %r12d
	incl	%r12d
	movq	$10, -8(%rsi)
	movl	%r12d, (%rsi)
	movq	%rsi, %r14
	addq	$16, %rsi
	movq	32(%rdx), %r15
	movq	(%r15), %rdi
	movq	%r14, %r8
	movq	%r12, %r9
	movq	%r13, %r12
	jmp	output.77F
	.text
output.77F:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest78A
	/* live= GP={%r12 %r10 %rcx %rdx %rbx} spilled=  */
retGC789:
	movq	32(%rdi), %r12
	movq	24(%rdi), %r10
	movl	16(%rdi), %ecx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest78A:
	movq	%r11, %r13
	subq	%rsi, %r13
	jle	doGC78B
check.784:
	/* block check<10E14> (ep<10965>,i<10966>,_t<10967>,retK<10968>,_exh<10969>) */
	cmpl	8(%rbx), %ecx
	jge	L78C
L_true785:
then.787:
	/* Liveout:  GP={%r12 %r10 %r9 %r8 %rdi}  */
	/* block then<10973> (ep<1096E>,i<10972>,retK<10971>,_exh<10970>,_t<1096F>) */
	movq	$133, -8(%rsi)
	movq	%rbx, (%rsi)
	movabsq	$output.77F, %r13
	movq	%r13, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	$7823, -8(%rsi)
	movq	(%rbx), %r15
	movq	%r15, (%rsi)
	movl	8(%rbx), %r13d
	movl	%r13d, 8(%rsi)
	movq	16(%rbx), %r15
	movq	%r15, 16(%rsi)
	movq	24(%rbx), %rbx
	movq	%rbx, 24(%rsi)
	movq	%r14, 32(%rsi)
	movq	%rdx, 40(%rsi)
	movl	%ecx, 48(%rsi)
	movq	%rsi, %r14
	addq	$64, %rsi
	movq	$133, -8(%rsi)
	movq	%r14, (%rsi)
	movabsq	$loop.770, %r14
	movq	%r14, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	xorl	%r15d, %r15d
	movq	$10, -8(%rsi)
	movl	%r15d, (%rsi)
	movq	%rsi, %rcx
	addq	$16, %rsi
	movq	(%r13), %rdi
	movq	%rcx, %r8
	movq	%r15, %r9
	jmp	loop.770
doGC78B:
	movq	$3467, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movl	%ecx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%r12, 32(%rsi)
	movq	%rsi, %rdx
	addq	$48, %rsi
	movq	%rdx, %rdi
	movabsq	$retGC789, %r8
	jmp	ASM_InvokeGC
L78C:
else.786:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block else<109D0> (retK<109CF>) */
	movq	%r10, %rdi
	movq	$1, %r8
	jmp	letJoinK.788
	.text
letJoinK.78F:
	movq	%r8, %rbx
	movq	%rdi, %r14
	jmp	gcTest791
	/* live= GP={%r14} spilled= GP={%r~1}  */
retGC790:
	movq	8(%rdi), %rbx
	movq	(%rdi), %r14
gcTest791:
	movq	%r11, %r12
	subq	%rsi, %r12
	jle	doGC792
	movq	%rbx, -64(%rbp)
check.78D:
	/* Liveout:  GP={%r12 %r10 %r9 %r8 %rdi}  */
	/* block check<10E1A> (ep<1095B>,scene<10954>) */
	movq	$-1048576, %r15
	andq	%rsi, %r15
	movq	$1, (%r15)
	movq	%rax, -72(%rbp)
	movq	%rcx, %r12
	movq	%rdx, %r13
	movq	%rsi, -112(%rbp)
	movq	%rdi, -96(%rbp)
	movq	%r8, -104(%rbp)
	movq	%r9, -88(%rbp)
	movq	%r10, -80(%rbp)
	movq	%r11, %rbx
	call	M_GetTime
	movq	%rax, -56(%rbp)
	movq	-72(%rbp), %rax
	movq	%r12, %rcx
	movq	%r13, %rdx
	movq	-112(%rbp), %rsi
	movq	-96(%rbp), %rdi
	movq	-104(%rbp), %r8
	movq	-88(%rbp), %r9
	movq	-80(%rbp), %r10
	movq	%rbx, %r11
	movq	$3, (%r15)
	movq	$1673, -8(%rsi)
	movq	24(%r14), %r12
	movq	%r12, (%rsi)
	movl	32(%r14), %r13d
	movl	%r13d, 8(%rsi)
	movq	40(%r14), %r15
	movq	%r15, 16(%rsi)
	movq	-64(%rbp), %r13
	movq	%r13, 24(%rsi)
	movq	%rsi, %r10
	addq	$40, %rsi
	movq	$133, -8(%rsi)
	movq	%r10, (%rsi)
	movabsq	$output.77F, %rcx
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	xorl	%r13d, %r13d
	movq	$10, -8(%rsi)
	movl	%r13d, (%rsi)
	movq	%rsi, %r15
	addq	$16, %rsi
	movq	$779, -8(%rsi)
	movabsq	$letJoinK.788, %rdx
	movq	%rdx, (%rsi)
	movq	8(%r14), %rbx
	movq	%rbx, 8(%rsi)
	movq	40(%r14), %r10
	movq	%r10, 16(%rsi)
	movq	48(%r14), %rcx
	movq	%rcx, 24(%rsi)
	movq	-56(%rbp), %rcx
	movq	%rcx, 32(%rsi)
	movq	%rsi, %r10
	addq	$48, %rsi
	movq	(%r12), %rdi
	movq	%r15, %r8
	movq	%r13, %r9
	movq	16(%r14), %r12
	jmp	output.77F
doGC792:
	movq	$20, -8(%rsi)
	movq	%r14, (%rsi)
	movq	%rbx, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	movq	%rdx, %rdi
	movabsq	$retGC790, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.794:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest796
	/* live= GP={%rcx %rdx} spilled=  */
retGC795:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest796:
	movq	%r11, %r14
	subq	%rsi, %r14
	jle	doGC797
check.793:
	/* Liveout:  GP={%r12 %r10 %r9 %r8 %rdi}  */
	/* block check<10E1D> (ep<10951>,_t<1094A>) */
	movq	$5903, -8(%rsi)
	movabsq	$letJoinK.78F, %rbx
	movq	%rbx, (%rsi)
	movq	8(%rdx), %r10
	movq	%r10, 8(%rsi)
	movq	16(%rdx), %r12
	movq	%r12, 16(%rsi)
	movq	32(%rdx), %r13
	movq	%r13, 24(%rsi)
	movl	40(%rdx), %r14d
	movl	%r14d, 32(%rsi)
	movq	48(%rdx), %r15
	movq	%r15, 40(%rsi)
	movq	56(%rdx), %rbx
	movq	%rbx, 48(%rsi)
	movq	%rsi, %r10
	addq	$64, %rsi
	movq	24(%rdx), %r12
	movq	(%r12), %rdi
	movq	64(%rdx), %r8
	movq	%rcx, %r9
	movq	16(%rdx), %r12
	jmp	mapP.4BC
doGC797:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	%r13, %rdi
	movabsq	$retGC795, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.79C:
	movq	%r9, %r12
	movsd	 %xmm4, %xmm4
	movsd	 %xmm3, %xmm1
	movsd	 %xmm2, %xmm0
	movq	%r8, %rbx
	movq	%rdi, %r14
	movsd	 %xmm4, %xmm9
	movsd	 %xmm1, %xmm8
	movsd	 %xmm0, %xmm7
	jmp	gcTest79E
	/* live= GP={%r12 %rbx %r14} FP={%xmm0 %xmm15 %xmm14} spilled=  */
retGC79D:
	movq	40(%rdi), %r12
	movq	8(%rdi), %rbx
	movq	(%rdi), %r14
	/* %f13077.d := mem.d[%rdi.64 +.64 32] */
	movsd	 32(%rdi), %xmm9
	/* %f13076.d := mem.d[%rdi.64 +.64 24] */
	movsd	 24(%rdi), %xmm8
	/* %f13075.d := mem.d[%rdi.64 +.64 16] */
	movsd	 16(%rdi), %xmm7
gcTest79E:
	movq	%r11, %r13
	subq	%rsi, %r13
	jle	doGC79F
	movsd	 %xmm9, %xmm0
	movsd	 %xmm8, %xmm15
	movsd	 %xmm7, %xmm14
check.798:
	/* Liveout:  GP={%r14 %r13 %r12 %r10 %r9 %r8 %rdi}  */
	/* block check<10E24> (ep<10851>,scrnj<10834>,_t<10835>,_t<10836>,_t<10837>,unused<10838>) */
	cvtsi2sd	 144(%r14), %xmm5
	cvtsi2sd	 144(%r14), %xmm6
	movabsq	$flt79A, %r13
	movabsq	$flt799, %r15
	movsd	 64(%r14), %xmm8
	divsd	 (%r15), %xmm8
	mulsd	 48(%r14), %xmm8
	divsd	 (%r13), %xmm8
	movq	184(%r14), %rbx
	movabsq	$flt799, %r12
	movsd	 (%r12), %xmm7
	mulsd	 (%rbx), %xmm7
	mulsd	 %xmm8, %xmm7
	divsd	 %xmm5, %xmm7
	movabsq	$flt79A, %r13
	movabsq	$flt799, %r15
	movsd	 64(%r14), %xmm10
	divsd	 (%r15), %xmm10
	mulsd	 48(%r14), %xmm10
	divsd	 (%r13), %xmm10
	movq	184(%r14), %rbx
	movabsq	$flt799, %r12
	movsd	 (%r12), %xmm9
	mulsd	 (%rbx), %xmm9
	mulsd	 %xmm10, %xmm9
	divsd	 %xmm6, %xmm9
	movq	192(%r14), %r15
	movq	(%r15), %r13
	movsd	 %xmm7, -128(%rbp)
	movsd	 -128(%rbp), %xmm10
	mulsd	 (%r13), %xmm10
	movsd	 %xmm10, -128(%rbp)
	movq	192(%r14), %r12
	movq	8(%r12), %rbx
	movsd	 %xmm7, -120(%rbp)
	movsd	 -120(%rbp), %xmm11
	mulsd	 (%rbx), %xmm11
	movsd	 %xmm11, -120(%rbp)
	movq	192(%r14), %r15
	movq	16(%r15), %r13
	movsd	 %xmm7, -112(%rbp)
	movsd	 -112(%rbp), %xmm12
	mulsd	 (%r13), %xmm12
	movsd	 %xmm12, -112(%rbp)
	movsd	 %xmm9, -152(%rbp)
	movsd	 -152(%rbp), %xmm13
	mulsd	 %xmm14, %xmm13
	movsd	 %xmm13, -152(%rbp)
	movsd	 %xmm9, -144(%rbp)
	movsd	 -144(%rbp), %xmm14
	mulsd	 %xmm15, %xmm14
	movsd	 %xmm14, -144(%rbp)
	movsd	 %xmm9, -136(%rbp)
	movsd	 -136(%rbp), %xmm1
	mulsd	 %xmm0, %xmm1
	movsd	 %xmm1, -136(%rbp)
	movabsq	$flt79B, %rbx
	movsd	 %xmm5, -160(%rbp)
	movsd	 -160(%rbp), %xmm2
	mulsd	 (%rbx), %xmm2
	movsd	 %xmm2, -160(%rbp)
	movabsq	$flt79B, %r12
	movsd	 %xmm6, -168(%rbp)
	movsd	 -168(%rbp), %xmm3
	mulsd	 (%r12), %xmm3
	movsd	 %xmm3, -168(%rbp)
	movq	$-1048576, %r15
	andq	%rsi, %r15
	movq	$1, (%r15)
	movq	%rax, %r13
	movq	%rcx, -64(%rbp)
	movq	%rdx, %rbx
	movq	%rsi, %r12
	movq	%rdi, -104(%rbp)
	movq	%r8, -88(%rbp)
	movq	%r9, -96(%rbp)
	movq	%r10, -80(%rbp)
	movq	%r11, -72(%rbp)
	call	M_GetTime
	movq	%rax, -56(%rbp)
	movq	%r13, %rax
	movq	-64(%rbp), %rcx
	movq	%rbx, %rdx
	movq	%r12, %rsi
	movq	-104(%rbp), %rdi
	movq	-88(%rbp), %r8
	movq	-96(%rbp), %r9
	movq	-80(%rbp), %r10
	movq	-72(%rbp), %r11
	movq	$3, (%r15)
	movq	$509869, -8(%rsi)
	movq	24(%r14), %r15
	movq	%r15, (%rsi)
	movq	32(%r14), %rcx
	movq	%rcx, 8(%rsi)
	movq	56(%r14), %rdx
	movq	%rdx, 16(%rsi)
	movq	72(%r14), %rbx
	movq	%rbx, 24(%rsi)
	movsd	 80(%r14), %xmm11
	movsd	 %xmm11, 32(%rsi)
	movsd	 88(%r14), %xmm12
	movsd	 %xmm12, 40(%rsi)
	movsd	 96(%r14), %xmm13
	movsd	 %xmm13, 48(%rsi)
	movq	104(%r14), %r10
	movq	%r10, 56(%rsi)
	movq	112(%r14), %r12
	movq	%r12, 64(%rsi)
	movq	120(%r14), %r13
	movq	%r13, 72(%rsi)
	movq	128(%r14), %r15
	movq	%r15, 80(%rsi)
	movq	136(%r14), %rcx
	movq	%rcx, 88(%rsi)
	movl	144(%r14), %edx
	movl	%edx, 96(%rsi)
	movsd	 -128(%rbp), %xmm4
	movsd	 %xmm4, 104(%rsi)
	movsd	 -120(%rbp), %xmm5
	movsd	 %xmm5, 112(%rsi)
	movsd	 -112(%rbp), %xmm6
	movsd	 %xmm6, 120(%rsi)
	movsd	 -152(%rbp), %xmm7
	movsd	 %xmm7, 128(%rsi)
	movsd	 -144(%rbp), %xmm8
	movsd	 %xmm8, 136(%rsi)
	movsd	 -136(%rbp), %xmm9
	movsd	 %xmm9, 144(%rsi)
	movsd	 -168(%rbp), %xmm0
	mulsd	 -152(%rbp), %xmm0
	movsd	 -160(%rbp), %xmm15
	mulsd	 -128(%rbp), %xmm15
	addsd	 %xmm0, %xmm15
	movsd	 160(%r14), %xmm14
	subsd	 %xmm15, %xmm14
	movsd	 %xmm14, 152(%rsi)
	movsd	 -168(%rbp), %xmm3
	mulsd	 -144(%rbp), %xmm3
	movsd	 -160(%rbp), %xmm2
	mulsd	 -120(%rbp), %xmm2
	addsd	 %xmm3, %xmm2
	movsd	 168(%r14), %xmm1
	subsd	 %xmm2, %xmm1
	movsd	 %xmm1, 160(%rsi)
	movsd	 -168(%rbp), %xmm6
	mulsd	 -136(%rbp), %xmm6
	movsd	 -160(%rbp), %xmm5
	mulsd	 -112(%rbp), %xmm5
	addsd	 %xmm6, %xmm5
	movsd	 176(%r14), %xmm4
	subsd	 %xmm5, %xmm4
	movsd	 %xmm4, 168(%rsi)
	movq	%rsi, %r13
	addq	$184, %rsi
	movq	$133, -8(%rsi)
	movq	%r13, (%rsi)
	movabsq	$anon.76A, %rbx
	movq	%rbx, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	xorl	%r10d, %r10d
	movq	$10, -8(%rsi)
	movl	%r10d, (%rsi)
	movq	%rsi, %rcx
	addq	$16, %rsi
	movl	144(%r14), %r12d
	decl	%r12d
	movq	$10, -8(%rsi)
	movl	%r12d, (%rsi)
	movq	%rsi, %r13
	addq	$16, %rsi
	movq	$20, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%r13, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	$44819, -8(%rsi)
	movabsq	$letJoinK.794, %r15
	movq	%r15, (%rsi)
	movq	8(%r14), %r13
	movq	%r13, 8(%rsi)
	movq	16(%r14), %r15
	movq	%r15, 16(%rsi)
	movq	32(%r14), %r13
	movq	%r13, 24(%rsi)
	movq	40(%r14), %r15
	movq	%r15, 32(%rsi)
	movl	144(%r14), %r13d
	movl	%r13d, 40(%rsi)
	movq	152(%r14), %r15
	movq	%r15, 48(%rsi)
	movq	-56(%rbp), %r15
	movq	%r15, 56(%rsi)
	movq	%rdx, 64(%rsi)
	movq	%rsi, %r13
	addq	$80, %rsi
	movq	24(%r14), %rdx
	movq	(%rdx), %rdi
	movq	%rbx, %r8
	movq	%rcx, %r9
	movq	16(%r14), %r14
	jmp	rangePNoStep.3C5
doGC79F:
	movq	$4493, -8(%rsi)
	movq	%r14, (%rsi)
	movq	%rbx, 8(%rsi)
	movsd	 %xmm7, 16(%rsi)
	movsd	 %xmm8, 24(%rsi)
	movsd	 %xmm9, 32(%rsi)
	movq	%r12, 40(%rsi)
	movq	%rsi, %rbx
	addq	$56, %rsi
	movq	%rbx, %rdi
	movabsq	$retGC79D, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.7A1:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest7A3
	/* live= GP={%rcx %rdx} spilled=  */
retGC7A2:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest7A3:
	movq	%r11, %rbx
	subq	%rsi, %rbx
	jle	doGC7A4
check.7A0:
	/* Liveout:  GP={%r8 %rdi} FP={%xmm4 %xmm3 %xmm2}  */
	/* block check<10E27> (ep<10831>,_t<10818>) */
	movq	$3320930099, %r10
	movq	%r10, -8(%rsi)
	movabsq	$letJoinK.79C, %r12
	movq	%r12, (%rsi)
	movq	8(%rdx), %r13
	movq	%r13, 8(%rsi)
	movq	16(%rdx), %r14
	movq	%r14, 16(%rsi)
	movq	24(%rdx), %r15
	movq	%r15, 24(%rsi)
	movq	32(%rdx), %rbx
	movq	%rbx, 32(%rsi)
	movq	40(%rdx), %r10
	movq	%r10, 40(%rsi)
	movsd	 48(%rdx), %xmm0
	movsd	 %xmm0, 48(%rsi)
	movq	56(%rdx), %r12
	movq	%r12, 56(%rsi)
	movsd	 64(%rdx), %xmm1
	movsd	 %xmm1, 64(%rsi)
	movq	72(%rdx), %r13
	movq	%r13, 72(%rsi)
	movsd	 80(%rdx), %xmm2
	movsd	 %xmm2, 80(%rsi)
	movsd	 88(%rdx), %xmm3
	movsd	 %xmm3, 88(%rsi)
	movsd	 96(%rdx), %xmm4
	movsd	 %xmm4, 96(%rsi)
	movq	104(%rdx), %r14
	movq	%r14, 104(%rsi)
	movq	112(%rdx), %r15
	movq	%r15, 112(%rsi)
	movq	120(%rdx), %rbx
	movq	%rbx, 120(%rsi)
	movq	128(%rdx), %r10
	movq	%r10, 128(%rsi)
	movq	136(%rdx), %r12
	movq	%r12, 136(%rsi)
	movl	144(%rdx), %r13d
	movl	%r13d, 144(%rsi)
	movq	152(%rdx), %r14
	movq	%r14, 152(%rsi)
	movsd	 160(%rdx), %xmm5
	movsd	 %xmm5, 160(%rsi)
	movsd	 168(%rdx), %xmm6
	movsd	 %xmm6, 168(%rsi)
	movsd	 176(%rdx), %xmm7
	movsd	 %xmm7, 176(%rsi)
	movq	184(%rdx), %r15
	movq	%r15, 184(%rsi)
	movq	192(%rdx), %rbx
	movq	%rbx, 192(%rsi)
	movq	%rsi, %rbx
	addq	$208, %rsi
	movq	56(%rdx), %r10
	movq	(%r10), %rdi
	movq	(%rcx), %r12
	/* %xmm2.d := mem.d[(mem.64[%r13100.64 +.64 0]) +.64 0] */
	movsd	 (%r12), %xmm2
	movq	8(%rcx), %r13
	/* %xmm3.d := mem.d[(mem.64[%r13100.64 +.64 8]) +.64 0] */
	movsd	 (%r13), %xmm3
	movq	16(%rcx), %r14
	/* %xmm4.d := mem.d[(mem.64[%r13100.64 +.64 16]) +.64 0] */
	movsd	 (%r14), %xmm4
	movq	%rbx, %r8
	jmp	vecnorm.4C1
doGC7A4:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	%r15, %rdi
	movabsq	$retGC7A2, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.7A6:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest7A8
	/* live= GP={%rcx %rdx} spilled=  */
retGC7A7:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest7A8:
	movq	%r11, %rbx
	subq	%rsi, %rbx
	jle	doGC7A9
check.7A5:
	/* Liveout:  GP={%r8 %rdi} FP={%xmm4 %xmm3 %xmm2}  */
	/* block check<10E2A> (ep<10815>,_t<107FC>) */
	movq	$3320930099, %rbx
	movq	%rbx, -8(%rsi)
	movabsq	$letJoinK.7A1, %r10
	movq	%r10, (%rsi)
	movq	8(%rdx), %r12
	movq	%r12, 8(%rsi)
	movq	16(%rdx), %r13
	movq	%r13, 16(%rsi)
	movq	24(%rdx), %r14
	movq	%r14, 24(%rsi)
	movq	32(%rdx), %r15
	movq	%r15, 32(%rsi)
	movq	40(%rdx), %rbx
	movq	%rbx, 40(%rsi)
	movsd	 48(%rdx), %xmm0
	movsd	 %xmm0, 48(%rsi)
	movq	56(%rdx), %r10
	movq	%r10, 56(%rsi)
	movsd	 64(%rdx), %xmm1
	movsd	 %xmm1, 64(%rsi)
	movq	72(%rdx), %r12
	movq	%r12, 72(%rsi)
	movsd	 80(%rdx), %xmm2
	movsd	 %xmm2, 80(%rsi)
	movsd	 88(%rdx), %xmm3
	movsd	 %xmm3, 88(%rsi)
	movsd	 96(%rdx), %xmm4
	movsd	 %xmm4, 96(%rsi)
	movq	104(%rdx), %r13
	movq	%r13, 104(%rsi)
	movq	112(%rdx), %r14
	movq	%r14, 112(%rsi)
	movq	120(%rdx), %r15
	movq	%r15, 120(%rsi)
	movq	128(%rdx), %rbx
	movq	%rbx, 128(%rsi)
	movq	136(%rdx), %r10
	movq	%r10, 136(%rsi)
	movl	144(%rdx), %r12d
	movl	%r12d, 144(%rsi)
	movq	152(%rdx), %r13
	movq	%r13, 152(%rsi)
	movsd	 160(%rdx), %xmm5
	movsd	 %xmm5, 160(%rsi)
	movsd	 168(%rdx), %xmm6
	movsd	 %xmm6, 168(%rsi)
	movsd	 176(%rdx), %xmm7
	movsd	 %xmm7, 176(%rsi)
	movq	192(%rdx), %r14
	movq	%r14, 184(%rsi)
	movq	200(%rdx), %r15
	movq	%r15, 192(%rsi)
	movq	%rsi, %r14
	addq	$208, %rsi
	movq	(%rcx), %rdi
	movq	184(%rdx), %rbx
	movq	(%rbx), %rcx
	/* %xmm2.d := mem.d[(mem.64[(mem.64[%r13171.64 +.64 184]) +.64 0]) +.64 0] */
	movsd	 (%rcx), %xmm2
	movq	184(%rdx), %r12
	movq	8(%r12), %r10
	/* %xmm3.d := mem.d[(mem.64[(mem.64[%r13171.64 +.64 184]) +.64 8]) +.64 0] */
	movsd	 (%r10), %xmm3
	movq	184(%rdx), %r15
	movq	16(%r15), %r13
	/* %xmm4.d := mem.d[(mem.64[(mem.64[%r13171.64 +.64 184]) +.64 16]) +.64 0] */
	movsd	 (%r13), %xmm4
	movq	%r14, %r8
	jmp	cross.4C6
doGC7A9:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	%rcx, %rdi
	movabsq	$retGC7A7, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.7AB:
	movq	%r9, %rcx
	movsd	 %xmm4, %xmm5
	movsd	 %xmm3, %xmm4
	movsd	 %xmm2, %xmm3
	movq	%r8, %rdx
	movq	%rdi, %rbx
	movsd	 %xmm5, %xmm0
	movsd	 %xmm4, %xmm15
	movsd	 %xmm3, %xmm14
	jmp	gcTest7AD
	/* live= GP={%rcx %rdx %rbx} FP={%xmm2 %xmm1 %xmm0} spilled=  */
retGC7AC:
	movq	40(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
	/* %f13320.d := mem.d[%rdi.64 +.64 32] */
	movsd	 32(%rdi), %xmm0
	/* %f13319.d := mem.d[%rdi.64 +.64 24] */
	movsd	 24(%rdi), %xmm15
	/* %f13318.d := mem.d[%rdi.64 +.64 16] */
	movsd	 16(%rdi), %xmm14
gcTest7AD:
	movq	%r11, %r12
	subq	%rsi, %r12
	jle	doGC7AE
	movsd	 %xmm0, %xmm2
	movsd	 %xmm15, %xmm1
	movsd	 %xmm14, %xmm0
check.7AA:
	/* Liveout:  GP={%r12 %r10 %r9 %r8 %rdi}  */
	/* block check<10E31> (ep<107F9>,scrni<107DB>,unused<107DC>,unused<107DD>,unused<107DE>,unused<107DF>) */
	movq	$7615897397, %r10
	movq	%r10, -8(%rsi)
	movabsq	$letJoinK.7A6, %r12
	movq	%r12, (%rsi)
	movq	8(%rbx), %r13
	movq	%r13, 8(%rsi)
	movq	16(%rbx), %r14
	movq	%r14, 16(%rsi)
	movq	24(%rbx), %r15
	movq	%r15, 24(%rsi)
	movq	32(%rbx), %rcx
	movq	%rcx, 32(%rsi)
	movq	40(%rbx), %r10
	movq	%r10, 40(%rsi)
	movsd	 48(%rbx), %xmm6
	movsd	 %xmm6, 48(%rsi)
	movq	56(%rbx), %r12
	movq	%r12, 56(%rsi)
	movsd	 72(%rbx), %xmm7
	movsd	 %xmm7, 64(%rsi)
	movq	80(%rbx), %r13
	movq	%r13, 72(%rsi)
	movsd	 88(%rbx), %xmm8
	movsd	 %xmm8, 80(%rsi)
	movsd	 96(%rbx), %xmm9
	movsd	 %xmm9, 88(%rsi)
	movsd	 104(%rbx), %xmm10
	movsd	 %xmm10, 96(%rsi)
	movq	112(%rbx), %r14
	movq	%r14, 104(%rsi)
	movq	120(%rbx), %r15
	movq	%r15, 112(%rsi)
	movq	128(%rbx), %rcx
	movq	%rcx, 120(%rsi)
	movq	136(%rbx), %r10
	movq	%r10, 128(%rsi)
	movq	144(%rbx), %r12
	movq	%r12, 136(%rsi)
	movl	152(%rbx), %r13d
	movl	%r13d, 144(%rsi)
	movq	160(%rbx), %r14
	movq	%r14, 152(%rsi)
	movsd	 168(%rbx), %xmm11
	movsd	 %xmm11, 160(%rsi)
	movsd	 176(%rbx), %xmm12
	movsd	 %xmm12, 168(%rsi)
	movsd	 184(%rbx), %xmm13
	movsd	 %xmm13, 176(%rsi)
	movq	192(%rbx), %r15
	movq	%r15, 184(%rsi)
	movq	200(%rbx), %rcx
	movq	%rcx, 192(%rsi)
	movq	%rdx, 200(%rsi)
	movq	%rsi, %r12
	addq	$216, %rsi
	movq	64(%rbx), %rbx
	movq	(%rbx), %rdi
	movq	(%rdx), %r8
	movq	8(%rdx), %r9
	movq	16(%rdx), %r10
	jmp	veccross.4CB
doGC7AE:
	movq	$4493, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movsd	 %xmm14, 16(%rsi)
	movsd	 %xmm15, 24(%rsi)
	movsd	 %xmm0, 32(%rsi)
	movq	%rcx, 40(%rsi)
	movq	%rsi, %r10
	addq	$56, %rsi
	movq	%r10, %rdi
	movabsq	$retGC7AC, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.7B0:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest7B2
	/* live= GP={%rcx %rdx} spilled=  */
retGC7B1:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest7B2:
	movq	%r11, %rbx
	subq	%rsi, %rbx
	jle	doGC7B3
check.7AF:
	/* Liveout:  GP={%r8 %rdi} FP={%xmm4 %xmm3 %xmm2}  */
	/* block check<10E34> (ep<107D8>,_t<107BE>) */
	movq	$6641868597, %r10
	movq	%r10, -8(%rsi)
	movabsq	$letJoinK.7AB, %r12
	movq	%r12, (%rsi)
	movq	8(%rdx), %r13
	movq	%r13, 8(%rsi)
	movq	16(%rdx), %r14
	movq	%r14, 16(%rsi)
	movq	24(%rdx), %r15
	movq	%r15, 24(%rsi)
	movq	32(%rdx), %rbx
	movq	%rbx, 32(%rsi)
	movq	40(%rdx), %r10
	movq	%r10, 40(%rsi)
	movsd	 48(%rdx), %xmm0
	movsd	 %xmm0, 48(%rsi)
	movq	56(%rdx), %r12
	movq	%r12, 56(%rsi)
	movq	64(%rdx), %r13
	movq	%r13, 64(%rsi)
	movsd	 72(%rdx), %xmm1
	movsd	 %xmm1, 72(%rsi)
	movq	80(%rdx), %r14
	movq	%r14, 80(%rsi)
	movsd	 88(%rdx), %xmm2
	movsd	 %xmm2, 88(%rsi)
	movsd	 96(%rdx), %xmm3
	movsd	 %xmm3, 96(%rsi)
	movsd	 104(%rdx), %xmm4
	movsd	 %xmm4, 104(%rsi)
	movq	112(%rdx), %r15
	movq	%r15, 112(%rsi)
	movq	120(%rdx), %rbx
	movq	%rbx, 120(%rsi)
	movq	128(%rdx), %r10
	movq	%r10, 128(%rsi)
	movq	136(%rdx), %r12
	movq	%r12, 136(%rsi)
	movq	144(%rdx), %r13
	movq	%r13, 144(%rsi)
	movl	152(%rdx), %r14d
	movl	%r14d, 152(%rsi)
	movq	160(%rdx), %r15
	movq	%r15, 160(%rsi)
	movsd	 168(%rdx), %xmm5
	movsd	 %xmm5, 168(%rsi)
	movsd	 176(%rdx), %xmm6
	movsd	 %xmm6, 176(%rsi)
	movsd	 184(%rdx), %xmm7
	movsd	 %xmm7, 184(%rsi)
	movq	192(%rdx), %rbx
	movq	%rbx, 192(%rsi)
	movq	200(%rdx), %r10
	movq	%r10, 200(%rsi)
	movq	%rsi, %rbx
	addq	$216, %rsi
	movq	56(%rdx), %r12
	movq	(%r12), %rdi
	movq	(%rcx), %r13
	/* %xmm2.d := mem.d[(mem.64[%r13328.64 +.64 0]) +.64 0] */
	movsd	 (%r13), %xmm2
	movq	8(%rcx), %r14
	/* %xmm3.d := mem.d[(mem.64[%r13328.64 +.64 8]) +.64 0] */
	movsd	 (%r14), %xmm3
	movq	16(%rcx), %r15
	/* %xmm4.d := mem.d[(mem.64[%r13328.64 +.64 16]) +.64 0] */
	movsd	 (%r15), %xmm4
	movq	%rbx, %r8
	jmp	vecnorm.4C1
doGC7B3:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	%rcx, %rdi
	movabsq	$retGC7B1, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.7B5:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest7B7
	/* live= GP={%rcx %rdx} spilled=  */
retGC7B6:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest7B7:
	movq	%r11, %r13
	subq	%rsi, %r13
	jle	doGC7B8
check.7B4:
	/* Liveout:  GP={%r8 %rdi} FP={%xmm4 %xmm3 %xmm2}  */
	/* block check<10E37> (ep<107BB>,_t<107A0>) */
	movq	$6641868597, %r10
	movq	%r10, -8(%rsi)
	movabsq	$letJoinK.7B0, %r12
	movq	%r12, (%rsi)
	movq	8(%rdx), %r13
	movq	%r13, 8(%rsi)
	movq	16(%rdx), %r14
	movq	%r14, 16(%rsi)
	movq	24(%rdx), %r15
	movq	%r15, 24(%rsi)
	movq	32(%rdx), %rbx
	movq	%rbx, 32(%rsi)
	movq	40(%rdx), %r10
	movq	%r10, 40(%rsi)
	movsd	 48(%rdx), %xmm0
	movsd	 %xmm0, 48(%rsi)
	movq	56(%rdx), %r12
	movq	%r12, 56(%rsi)
	movq	64(%rdx), %r13
	movq	%r13, 64(%rsi)
	movsd	 96(%rdx), %xmm1
	movsd	 %xmm1, 72(%rsi)
	movq	104(%rdx), %r14
	movq	%r14, 80(%rsi)
	movsd	 112(%rdx), %xmm2
	movsd	 %xmm2, 88(%rsi)
	movsd	 120(%rdx), %xmm3
	movsd	 %xmm3, 96(%rsi)
	movsd	 128(%rdx), %xmm4
	movsd	 %xmm4, 104(%rsi)
	movq	136(%rdx), %r15
	movq	%r15, 112(%rsi)
	movq	144(%rdx), %rbx
	movq	%rbx, 120(%rsi)
	movq	152(%rdx), %r10
	movq	%r10, 128(%rsi)
	movq	160(%rdx), %r12
	movq	%r12, 136(%rsi)
	movq	168(%rdx), %r13
	movq	%r13, 144(%rsi)
	movl	176(%rdx), %r14d
	movl	%r14d, 152(%rsi)
	movq	184(%rdx), %r15
	movq	%r15, 160(%rsi)
	movsd	 192(%rdx), %xmm5
	movsd	 %xmm5, 168(%rsi)
	movsd	 200(%rdx), %xmm6
	movsd	 %xmm6, 176(%rsi)
	movsd	 208(%rdx), %xmm7
	movsd	 %xmm7, 184(%rsi)
	movq	216(%rdx), %rbx
	movq	%rbx, 192(%rsi)
	movq	224(%rdx), %r10
	movq	%r10, 200(%rsi)
	movq	%rsi, %rbx
	addq	$216, %rsi
	movq	(%rcx), %rdi
	/* %xmm2.d := mem.d[%r13401.64 +.64 72] */
	movsd	 72(%rdx), %xmm2
	/* %xmm3.d := mem.d[%r13401.64 +.64 80] */
	movsd	 80(%rdx), %xmm3
	/* %xmm4.d := mem.d[%r13401.64 +.64 88] */
	movsd	 88(%rdx), %xmm4
	movq	%rbx, %r8
	jmp	cross.4C6
doGC7B8:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	%r12, %rdi
	movabsq	$retGC7B6, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.7BA:
	movq	%r9, %rcx
	movsd	 %xmm4, %xmm5
	movsd	 %xmm3, %xmm4
	movsd	 %xmm2, %xmm3
	movq	%r8, %rdx
	movq	%rdi, %rbx
	movsd	 %xmm5, %xmm5
	movsd	 %xmm4, %xmm4
	movsd	 %xmm3, %xmm3
	jmp	gcTest7BC
	/* live= GP={%rcx %rdx %rbx} FP={%xmm2 %xmm1 %xmm0} spilled=  */
retGC7BB:
	movq	40(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
	/* %f13550.d := mem.d[%rdi.64 +.64 32] */
	movsd	 32(%rdi), %xmm5
	/* %f13549.d := mem.d[%rdi.64 +.64 24] */
	movsd	 24(%rdi), %xmm4
	/* %f13548.d := mem.d[%rdi.64 +.64 16] */
	movsd	 16(%rdi), %xmm3
gcTest7BC:
	movq	%r11, %r15
	subq	%rsi, %r15
	jle	doGC7BD
	movsd	 %xmm5, %xmm2
	movsd	 %xmm4, %xmm1
	movsd	 %xmm3, %xmm0
check.7B9:
	/* Liveout:  GP={%r12 %r10 %r9 %r8 %rdi}  */
	/* block check<10E3E> (ep<1079D>,lookdir<10786>,unused<10787>,unused<10788>,unused<10789>,dist<1078A>) */
	movq	$53134548795, %r10
	movq	%r10, -8(%rsi)
	movabsq	$letJoinK.7B5, %r12
	movq	%r12, (%rsi)
	movq	8(%rbx), %r13
	movq	%r13, 8(%rsi)
	movq	16(%rbx), %r14
	movq	%r14, 16(%rsi)
	movq	24(%rbx), %r15
	movq	%r15, 24(%rsi)
	movq	32(%rbx), %r10
	movq	%r10, 32(%rsi)
	movq	40(%rbx), %r12
	movq	%r12, 40(%rsi)
	movsd	 48(%rbx), %xmm6
	movsd	 %xmm6, 48(%rsi)
	movq	56(%rbx), %r13
	movq	%r13, 56(%rsi)
	movq	64(%rbx), %r14
	movq	%r14, 64(%rsi)
	movsd	 72(%rbx), %xmm7
	movsd	 %xmm7, 72(%rsi)
	movsd	 80(%rbx), %xmm8
	movsd	 %xmm8, 80(%rsi)
	movsd	 88(%rbx), %xmm9
	movsd	 %xmm9, 88(%rsi)
	movsd	 96(%rbx), %xmm10
	movsd	 %xmm10, 96(%rsi)
	movq	104(%rbx), %r15
	movq	%r15, 104(%rsi)
	movsd	 112(%rbx), %xmm11
	movsd	 %xmm11, 112(%rsi)
	movsd	 120(%rbx), %xmm12
	movsd	 %xmm12, 120(%rsi)
	movsd	 128(%rbx), %xmm13
	movsd	 %xmm13, 128(%rsi)
	movq	136(%rbx), %r10
	movq	%r10, 136(%rsi)
	movq	144(%rbx), %r12
	movq	%r12, 144(%rsi)
	movq	152(%rbx), %r13
	movq	%r13, 152(%rsi)
	movq	160(%rbx), %r14
	movq	%r14, 160(%rsi)
	movq	168(%rbx), %r15
	movq	%r15, 168(%rsi)
	movl	176(%rbx), %r10d
	movl	%r10d, 176(%rsi)
	movq	184(%rbx), %r12
	movq	%r12, 184(%rsi)
	movsd	 192(%rbx), %xmm14
	movsd	 %xmm14, 192(%rsi)
	movsd	 200(%rbx), %xmm15
	movsd	 %xmm15, 200(%rsi)
	movsd	 208(%rbx), %xmm0
	movsd	 %xmm0, 208(%rsi)
	movq	%rdx, 216(%rsi)
	movq	%rcx, 224(%rsi)
	movq	%rsi, %r12
	addq	$240, %rsi
	movq	64(%rbx), %r13
	movq	(%r13), %rdi
	movq	(%rdx), %r8
	movq	8(%rdx), %r9
	movq	16(%rdx), %r10
	jmp	veccross.4CB
doGC7BD:
	movq	$4493, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movsd	 %xmm3, 16(%rsi)
	movsd	 %xmm4, 24(%rsi)
	movsd	 %xmm5, 32(%rsi)
	movq	%rcx, 40(%rsi)
	movq	%rsi, %r14
	addq	$56, %rsi
	movq	%r14, %rdi
	movabsq	$retGC7BB, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.7C1:
	movq	%r8, %r15
	movq	%rdi, %r12
	jmp	gcTest7C3
	/* live= GP={%r12} spilled= GP={%r~1}  */
retGC7C2:
	movq	8(%rdi), %r15
	movq	(%rdi), %r12
gcTest7C3:
	movq	%r11, %rbx
	subq	%rsi, %rbx
	jle	doGC7C4
	movq	%r15, -72(%rbp)
check.7BE:
	/* Liveout:  GP={%r8 %rdi} FP={%xmm4 %xmm3 %xmm2}  */
	/* block check<10E41> (ep<FEA2>,world<FE9D>) */
	movq	$44, -8(%rsi)
	movq	24(%r12), %r13
	movq	%r13, (%rsi)
	movq	32(%r12), %r14
	movq	%r14, 8(%rsi)
	movq	80(%r12), %r15
	movq	%r15, 16(%rsi)
	movq	168(%r12), %rbx
	movq	%rbx, 24(%rsi)
	movq	176(%r12), %r13
	movq	%r13, 32(%rsi)
	movq	%rsi, %rbx
	addq	$48, %rsi
	movq	$133, -8(%rsi)
	movq	%rbx, (%rsi)
	movabsq	$trace.646, %r14
	movq	%r14, 8(%rsi)
	movq	%rsi, -64(%rbp)
	addq	$24, %rsi
	movq	$33553829, -8(%rsi)
	movq	24(%r12), %rbx
	movq	%rbx, (%rsi)
	movq	32(%r12), %r13
	movq	%r13, 8(%rsi)
	movsd	 72(%r12), %xmm0
	movsd	 %xmm0, 16(%rsi)
	movq	80(%r12), %r14
	movq	%r14, 24(%rsi)
	movq	88(%r12), %r15
	movq	%r15, 32(%rsi)
	movq	104(%r12), %rbx
	movq	%rbx, 40(%rsi)
	movq	112(%r12), %r13
	movq	%r13, 48(%rsi)
	movq	120(%r12), %r14
	movq	%r14, 56(%rsi)
	movq	128(%r12), %r15
	movq	%r15, 64(%rsi)
	movq	136(%r12), %rbx
	movq	%rbx, 72(%rsi)
	movq	144(%r12), %r13
	movq	%r13, 80(%rsi)
	movq	152(%r12), %r14
	movq	%r14, 88(%rsi)
	movq	160(%r12), %r15
	movq	%r15, 96(%rsi)
	movq	184(%r12), %rbx
	movq	%rbx, 104(%rsi)
	movq	192(%r12), %r13
	movq	%r13, 112(%rsi)
	movq	296(%r12), %r14
	movq	%r14, 120(%rsi)
	movq	-72(%rbp), %r13
	movq	%r13, 128(%rsi)
	movq	-64(%rbp), %r14
	movq	%r14, 136(%rsi)
	movq	%rsi, %r15
	addq	$152, %rsi
	movq	$133, -8(%rsi)
	movq	%r15, (%rsi)
	movabsq	$shade.6AA, %r15
	movq	%r15, 8(%rsi)
	movq	%rsi, -56(%rbp)
	addq	$24, %rsi
	movq	$-1048576, %r13
	andq	%rsi, %r13
	movq	$1, (%r13)
	movq	%rax, -112(%rbp)
	movq	%rcx, %rbx
	movq	%rdx, -96(%rbp)
	movq	%rsi, -120(%rbp)
	movq	%rdi, -104(%rbp)
	movq	%r8, -128(%rbp)
	movq	%r9, %r14
	movq	%r10, -80(%rbp)
	movq	%r11, %r15
	call	M_ReadInt
	movq	%rax, -88(%rbp)
	movq	-112(%rbp), %rax
	movq	%rbx, %rcx
	movq	-96(%rbp), %rdx
	movq	-120(%rbp), %rsi
	movq	-104(%rbp), %rdi
	movq	-128(%rbp), %r8
	movq	%r14, %r9
	movq	-80(%rbp), %r10
	movq	%r15, %r11
	movq	$3, (%r13)
	movq	$-1048576, %r14
	andq	%rsi, %r14
	movq	$1, (%r14)
	movq	%rax, -104(%rbp)
	movq	%rcx, -112(%rbp)
	movq	%rdx, -120(%rbp)
	movq	%rsi, -128(%rbp)
	movq	%rdi, -136(%rbp)
	movq	%r8, %rbx
	movq	%r9, -96(%rbp)
	movq	%r10, %r13
	movq	%r11, %r15
	movslq	-88(%rbp), %rcx
	movq	%rcx, %rdi
	movslq	-88(%rbp), %rdx
	movq	%rdx, %rsi
	call	M_NewImage
	movq	%rax, -80(%rbp)
	movq	-104(%rbp), %rax
	movq	-112(%rbp), %rcx
	movq	-120(%rbp), %rdx
	movq	-128(%rbp), %rsi
	movq	-136(%rbp), %rdi
	movq	%rbx, %r8
	movq	-96(%rbp), %r9
	movq	%r13, %r10
	movq	%r15, %r11
	movq	$3, (%r14)
	movsd	 200(%r12), %xmm1
	subsd	 264(%r12), %xmm1
	movsd	 208(%r12), %xmm3
	subsd	 272(%r12), %xmm3
	movsd	 216(%r12), %xmm4
	subsd	 280(%r12), %xmm4
	movq	$1594941239, -8(%rsi)
	movabsq	$letJoinK.7BA, %r10
	movq	%r10, (%rsi)
	movq	8(%r12), %r13
	movq	%r13, 8(%rsi)
	movq	16(%r12), %r14
	movq	%r14, 16(%rsi)
	movq	40(%r12), %r15
	movq	%r15, 24(%rsi)
	movq	48(%r12), %rcx
	movq	%rcx, 32(%rsi)
	movq	56(%r12), %rdx
	movq	%rdx, 40(%rsi)
	movsd	 64(%r12), %xmm5
	movsd	 %xmm5, 48(%rsi)
	movq	88(%r12), %rbx
	movq	%rbx, 56(%rsi)
	movq	96(%r12), %r10
	movq	%r10, 64(%rsi)
	movsd	 224(%r12), %xmm6
	movsd	 %xmm6, 72(%rsi)
	movsd	 232(%r12), %xmm7
	movsd	 %xmm7, 80(%rsi)
	movsd	 240(%r12), %xmm8
	movsd	 %xmm8, 88(%rsi)
	movsd	 248(%r12), %xmm9
	movsd	 %xmm9, 96(%rsi)
	movq	256(%r12), %r13
	movq	%r13, 104(%rsi)
	movsd	 264(%r12), %xmm10
	movsd	 %xmm10, 112(%rsi)
	movsd	 272(%r12), %xmm11
	movsd	 %xmm11, 120(%rsi)
	movsd	 280(%r12), %xmm12
	movsd	 %xmm12, 128(%rsi)
	movq	288(%r12), %r14
	movq	%r14, 136(%rsi)
	movq	296(%r12), %r15
	movq	%r15, 144(%rsi)
	movq	-72(%rbp), %r15
	movq	%r15, 152(%rsi)
	movq	-64(%rbp), %rcx
	movq	%rcx, 160(%rsi)
	movq	-56(%rbp), %rdx
	movq	%rdx, 168(%rsi)
	movq	-88(%rbp), %rbx
	movl	%ebx, 176(%rsi)
	movq	-80(%rbp), %r10
	movq	%r10, 184(%rsi)
	movsd	 %xmm1, 192(%rsi)
	movsd	 %xmm3, 200(%rsi)
	movsd	 %xmm4, 208(%rsi)
	movq	%rsi, %rbx
	addq	$224, %rsi
	movq	88(%r12), %rcx
	movq	(%rcx), %rdi
	movsd	 %xmm1, %xmm2
	movsd	 %xmm3, %xmm3
	movsd	 %xmm4, %xmm4
	movq	%rbx, %r8
	jmp	vecnorm.4C1
doGC7C4:
	movq	$20, -8(%rsi)
	movq	%r12, (%rsi)
	movq	%r15, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	movq	%rdx, %rdi
	movabsq	$retGC7C2, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.7DC:
	movq	%r9, %r12
	movq	%r8, %r13
	movq	%rdi, %r14
	jmp	gcTest7DE
	/* live= GP={%r10 %r12 %r13} spilled= GP={%r~1}  */
retGC7DD:
	movl	24(%rdi), %r10d
	movq	16(%rdi), %r12
	movq	8(%rdi), %r13
	movq	(%rdi), %r14
gcTest7DE:
	movq	%r11, %r15
	subq	%rsi, %r15
	jle	doGC7DF
	movq	%r14, -240(%rbp)
check.7C5:
	/* Liveout:  GP={%r12 %r10 %r9 %r8 %rdi}  */
	/* block check<10E46> (ep<EF12>,empty<EF09>,_t<EF0A>,_t<EF0B>) */
	movq	$521, -8(%rsi)
	movq	$3, (%rsi)
	movl	$0, 8(%rsi)
	movq	%r12, 16(%rsi)
	movl	%r10d, 24(%rsi)
	movq	%rsi, %r14
	addq	$40, %rsi
	movq	$18, -8(%rsi)
	movq	$1, (%rsi)
	movabsq	$subInBounds.236, %r15
	movq	%r15, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	$18, -8(%rsi)
	movq	$1, (%rsi)
	movabsq	$leftmostLeaf.248, %rcx
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	$18, -8(%rsi)
	movq	$1, (%rsi)
	movabsq	$rightmostLeaf.259, %rdx
	movq	%rdx, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	movq	$27537, -8(%rsi)
	movq	-240(%rbp), %rcx
	movq	40(%rcx), %rcx
	movq	%rcx, (%rsi)
	movq	-240(%rbp), %rcx
	movq	48(%rcx), %rcx
	movq	%rcx, 8(%rsi)
	movq	%r13, 16(%rsi)
	movl	$128, 24(%rsi)
	movq	%r12, 32(%rsi)
	movl	%r10d, 40(%rsi)
	movq	%rbx, 48(%rsi)
	movq	%rdx, 56(%rsi)
	movq	%rsi, %rbx
	addq	$72, %rsi
	movq	$133, -8(%rsi)
	movq	%rbx, (%rsi)
	movabsq	$tabFromToP.359, %rbx
	movq	%rbx, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movq	%r14, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	$133, -8(%rsi)
	movq	%r13, (%rsi)
	movabsq	$rangePNoStep.3C5, %r14
	movq	%r14, 8(%rsi)
	movq	%rsi, -56(%rbp)
	addq	$24, %rsi
	movq	$391, -8(%rsi)
	movq	-240(%rbp), %rdx
	movq	48(%rdx), %rdx
	movq	%rdx, (%rsi)
	movq	%r12, 8(%rsi)
	movl	%r10d, 16(%rsi)
	movq	%rsi, %rcx
	addq	$32, %rsi
	movq	$133, -8(%rsi)
	movq	%rcx, (%rsi)
	movabsq	$mapP.4BC, %rbx
	movq	%rbx, 8(%rsi)
	movq	%rsi, -64(%rbp)
	addq	$24, %rsi
	movq	$12, -8(%rsi)
	movq	%r15, (%rsi)
	movq	%rsi, %r10
	addq	$16, %rsi
	movq	$133, -8(%rsi)
	movq	%r10, (%rsi)
	movabsq	$sub.776, %r12
	movq	%r12, 8(%rsi)
	movq	%rsi, -72(%rbp)
	addq	$24, %rsi
	movabsq	$flt7C7, %r13
	movsd	 (%r13), %xmm15
	movabsq	$flt7C8, %r14
	movsd	 (%r14), %xmm0
	movq	$10, -8(%rsi)
	movsd	 %xmm0, (%rsi)
	movq	%rsi, -192(%rbp)
	addq	$16, %rsi
	movq	$18, -8(%rsi)
	movq	$1, (%rsi)
	movabsq	$vecnorm.4C1, %r15
	movq	%r15, 8(%rsi)
	movq	%rsi, -80(%rbp)
	addq	$24, %rsi
	movq	$18, -8(%rsi)
	movq	$1, (%rsi)
	movabsq	$veccross.4CB, %rcx
	movq	%rcx, 8(%rsi)
	movq	%rsi, -88(%rbp)
	addq	$24, %rsi
	movq	$18, -8(%rsi)
	movq	$1, (%rsi)
	movabsq	$ambientsurf.4DC, %rdx
	movq	%rdx, 8(%rsi)
	movq	%rsi, -96(%rbp)
	addq	$24, %rsi
	movq	$18, -8(%rsi)
	movq	$1, (%rsi)
	movabsq	$diffusesurf.4EE, %rbx
	movq	%rbx, 8(%rsi)
	movq	%rsi, -104(%rbp)
	addq	$24, %rsi
	movq	$18, -8(%rsi)
	movq	$1, (%rsi)
	movabsq	$specularsurf.4FB, %r10
	movq	%r10, 8(%rsi)
	movq	%rsi, -112(%rbp)
	addq	$24, %rsi
	movq	$18, -8(%rsi)
	movq	$1, (%rsi)
	movabsq	$specpowsurf.509, %r12
	movq	%r12, 8(%rsi)
	movq	%rsi, -120(%rbp)
	addq	$24, %rsi
	movq	$18, -8(%rsi)
	movq	$1, (%rsi)
	movabsq	$reflectsurf.51A, %r13
	movq	%r13, 8(%rsi)
	movq	%rsi, -128(%rbp)
	addq	$24, %rsi
	movq	$18, -8(%rsi)
	movq	$1, (%rsi)
	movabsq	$transmitsurf.52C, %r14
	movq	%r14, 8(%rsi)
	movq	%rsi, -136(%rbp)
	addq	$24, %rsi
	movq	$18, -8(%rsi)
	movq	$1, (%rsi)
	movabsq	$refractsurf.53F, %r15
	movq	%r15, 8(%rsi)
	movq	%rsi, -144(%rbp)
	addq	$24, %rsi
	movq	$18, -8(%rsi)
	movq	$1, (%rsi)
	movabsq	$bodysurf.54C, %rcx
	movq	%rcx, 8(%rsi)
	movq	%rsi, -152(%rbp)
	addq	$24, %rsi
	movq	$519, -8(%rsi)
	movsd	 %xmm15, (%rsi)
	movsd	 %xmm0, 8(%rsi)
	movq	-192(%rbp), %rbx
	movq	%rbx, 16(%rsi)
	movq	%rsi, %rdx
	addq	$32, %rsi
	movq	$133, -8(%rsi)
	movq	%rdx, (%rsi)
	movabsq	$implicitPolyhedron.577, %rbx
	movq	%rbx, 8(%rsi)
	movq	%rsi, -160(%rbp)
	addq	$24, %rsi
	movq	$18, -8(%rsi)
	movq	$1, (%rsi)
	movabsq	$polyhedronIntersect.594, %r10
	movq	%r10, 8(%rsi)
	movq	%rsi, -168(%rbp)
	addq	$24, %rsi
	movq	$18, -8(%rsi)
	movq	$1, (%rsi)
	movabsq	$Primsurf.5A9, %r12
	movq	%r12, 8(%rsi)
	movq	%rsi, -176(%rbp)
	addq	$24, %rsi
	movq	$18, -8(%rsi)
	movq	$1, (%rsi)
	movabsq	$Primnorm.5BE, %r13
	movq	%r13, 8(%rsi)
	movq	%rsi, -184(%rbp)
	addq	$24, %rsi
	movq	$34, -8(%rsi)
	movq	$1, (%rsi)
	movabsq	$flt7CA, %r14
	movsd	 (%r14), %xmm1
	movsd	 %xmm1, 8(%rsi)
	movabsq	$flt4D3, %r15
	movsd	 (%r15), %xmm2
	movsd	 %xmm2, 16(%rsi)
	movabsq	$flt4D3, %rcx
	movsd	 (%rcx), %xmm3
	movsd	 %xmm3, 24(%rsi)
	movq	%rsi, %rcx
	addq	$40, %rsi
	movq	$34, -8(%rsi)
	movq	$3, (%rsi)
	movabsq	$flt7CB, %rdx
	movsd	 (%rdx), %xmm4
	movsd	 %xmm4, 8(%rsi)
	movabsq	$flt4D3, %rbx
	movsd	 (%rbx), %xmm5
	movsd	 %xmm5, 16(%rsi)
	movabsq	$flt4D3, %r10
	movsd	 (%r10), %xmm6
	movsd	 %xmm6, 24(%rsi)
	movq	%rsi, %r10
	addq	$40, %rsi
	movq	$34, -8(%rsi)
	movq	$5, (%rsi)
	movabsq	$flt7CC, %r13
	movsd	 (%r13), %xmm7
	movsd	 %xmm7, 8(%rsi)
	movabsq	$flt7CD, %r14
	movsd	 (%r14), %xmm8
	movsd	 %xmm8, 16(%rsi)
	movabsq	$flt7CD, %r15
	movsd	 (%r15), %xmm9
	movsd	 %xmm9, 24(%rsi)
	movq	%rsi, %r12
	addq	$40, %rsi
	movq	$18, -8(%rsi)
	movq	$11, (%rsi)
	movabsq	$flt7CE, %rbx
	movsd	 (%rbx), %xmm10
	movsd	 %xmm10, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	$1, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movq	%r12, (%rsi)
	movq	%r13, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movq	%r10, (%rsi)
	movq	%r14, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%r15, 8(%rsi)
	movq	%rsi, -200(%rbp)
	addq	$24, %rsi
	movq	$58, -8(%rsi)
	movq	$3, (%rsi)
	movabsq	$flt60A, %rcx
	movsd	 (%rcx), %xmm11
	movsd	 %xmm11, 8(%rsi)
	movabsq	$flt7CF, %rdx
	movsd	 (%rdx), %xmm12
	movsd	 %xmm12, 16(%rsi)
	movabsq	$flt799, %rbx
	movsd	 (%rbx), %xmm13
	movsd	 %xmm13, 24(%rsi)
	movabsq	$flt7D0, %r10
	movsd	 (%r10), %xmm14
	movsd	 %xmm14, 32(%rsi)
	movabsq	$flt7D0, %r12
	movsd	 (%r12), %xmm0
	movsd	 %xmm0, 40(%rsi)
	movabsq	$flt7D0, %r13
	movsd	 (%r13), %xmm1
	movsd	 %xmm1, 48(%rsi)
	movq	%rsi, %r13
	addq	$64, %rsi
	movq	$58, -8(%rsi)
	movq	$3, (%rsi)
	movabsq	$flt536, %r14
	movsd	 (%r14), %xmm2
	movsd	 %xmm2, 8(%rsi)
	movabsq	$flt7D1, %r15
	movsd	 (%r15), %xmm3
	movsd	 %xmm3, 16(%rsi)
	movabsq	$flt60A, %rcx
	movsd	 (%rcx), %xmm4
	movsd	 %xmm4, 24(%rsi)
	movabsq	$flt7D0, %rdx
	movsd	 (%rdx), %xmm5
	movsd	 %xmm5, 32(%rsi)
	movabsq	$flt7D0, %rbx
	movsd	 (%rbx), %xmm6
	movsd	 %xmm6, 40(%rsi)
	movabsq	$flt7D0, %r10
	movsd	 (%r10), %xmm7
	movsd	 %xmm7, 48(%rsi)
	movq	%rsi, %r10
	addq	$64, %rsi
	movq	$58, -8(%rsi)
	movq	$3, (%rsi)
	movabsq	$flt7D2, %r14
	movsd	 (%r14), %xmm8
	movsd	 %xmm8, 8(%rsi)
	movabsq	$flt536, %r15
	movsd	 (%r15), %xmm9
	movsd	 %xmm9, 16(%rsi)
	movabsq	$flt7D3, %rcx
	movsd	 (%rcx), %xmm10
	movsd	 %xmm10, 24(%rsi)
	movabsq	$flt7D0, %rdx
	movsd	 (%rdx), %xmm11
	movsd	 %xmm11, 32(%rsi)
	movabsq	$flt7D0, %rbx
	movsd	 (%rbx), %xmm12
	movsd	 %xmm12, 40(%rsi)
	movabsq	$flt7D0, %r12
	movsd	 (%r12), %xmm13
	movsd	 %xmm13, 48(%rsi)
	movq	%rsi, %r12
	addq	$64, %rsi
	movq	$20, -8(%rsi)
	movq	%r12, (%rsi)
	movq	$1, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movq	%r10, (%rsi)
	movq	%r14, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movq	%r13, (%rsi)
	movq	%r15, 8(%rsi)
	movq	%rsi, -208(%rbp)
	addq	$24, %rsi
	movabsq	$flt7D4, %rcx
	movsd	 (%rcx), %xmm11
	movq	$10, -8(%rsi)
	movsd	 %xmm11, (%rsi)
	movq	%rsi, %rdx
	addq	$16, %rsi
	movabsq	$flt7D5, %rbx
	movsd	 (%rbx), %xmm12
	movq	$10, -8(%rsi)
	movsd	 %xmm12, (%rsi)
	movq	%rsi, %r10
	addq	$16, %rsi
	movabsq	$flt7D6, %r12
	movsd	 (%r12), %xmm13
	movq	$10, -8(%rsi)
	movsd	 %xmm13, (%rsi)
	movq	%rsi, %r13
	addq	$16, %rsi
	movq	$28, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%r10, 8(%rsi)
	movq	%r13, 16(%rsi)
	movq	%rsi, -216(%rbp)
	addq	$32, %rsi
	movq	$10, -8(%rsi)
	movabsq	$flt7D7, %r15
	movsd	 (%r15), %xmm14
	movsd	 %xmm14, (%rsi)
	movq	%rsi, %r14
	addq	$16, %rsi
	movq	$10, -8(%rsi)
	movabsq	$flt7D8, %rdx
	movsd	 (%rdx), %xmm0
	movsd	 %xmm0, (%rsi)
	movq	%rsi, %rcx
	addq	$16, %rsi
	movq	$10, -8(%rsi)
	movabsq	$flt7D9, %r10
	movsd	 (%r10), %xmm1
	movsd	 %xmm1, (%rsi)
	movq	%rsi, %rbx
	addq	$16, %rsi
	movq	$28, -8(%rsi)
	movq	%r14, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rbx, 16(%rsi)
	movq	%rsi, -224(%rbp)
	addq	$32, %rsi
	movq	$10, -8(%rsi)
	movabsq	$flt4D3, %r13
	movsd	 (%r13), %xmm2
	movsd	 %xmm2, (%rsi)
	movq	%rsi, %r12
	addq	$16, %rsi
	movq	$10, -8(%rsi)
	movabsq	$flt7CF, %r15
	movsd	 (%r15), %xmm3
	movsd	 %xmm3, (%rsi)
	movq	%rsi, %r14
	addq	$16, %rsi
	movq	$10, -8(%rsi)
	movabsq	$flt799, %rdx
	movsd	 (%rdx), %xmm4
	movsd	 %xmm4, (%rsi)
	movq	%rsi, %rcx
	addq	$16, %rsi
	movq	$28, -8(%rsi)
	movq	%r12, (%rsi)
	movq	%r14, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%rsi, %rbx
	addq	$32, %rsi
	movq	$10, -8(%rsi)
	movabsq	$flt536, %r12
	movsd	 (%r12), %xmm5
	movsd	 %xmm5, (%rsi)
	movq	%rsi, %r10
	addq	$16, %rsi
	movq	$20, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%r10, 8(%rsi)
	movq	%rsi, -232(%rbp)
	addq	$24, %rsi
	movq	$10, -8(%rsi)
	movabsq	$flt4D3, %r14
	movsd	 (%r14), %xmm6
	movsd	 %xmm6, (%rsi)
	movq	%rsi, %r13
	addq	$16, %rsi
	movq	$10, -8(%rsi)
	movabsq	$flt536, %rcx
	movsd	 (%rcx), %xmm7
	movsd	 %xmm7, (%rsi)
	movq	%rsi, %r15
	addq	$16, %rsi
	movq	$10, -8(%rsi)
	movabsq	$flt7D3, %rbx
	movsd	 (%rbx), %xmm8
	movsd	 %xmm8, (%rsi)
	movq	%rsi, %rdx
	addq	$16, %rsi
	movq	$28, -8(%rsi)
	movq	%r13, (%rsi)
	movq	%r15, 8(%rsi)
	movq	%rdx, 16(%rsi)
	movq	%rsi, %r10
	addq	$32, %rsi
	movq	$10, -8(%rsi)
	movabsq	$flt60A, %r13
	movsd	 (%r13), %xmm9
	movsd	 %xmm9, (%rsi)
	movq	%rsi, %r12
	addq	$16, %rsi
	movq	$20, -8(%rsi)
	movq	%r10, (%rsi)
	movq	%r12, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	$10, -8(%rsi)
	movabsq	$flt4D3, %r15
	movsd	 (%r15), %xmm10
	movsd	 %xmm10, (%rsi)
	movq	%rsi, %r14
	addq	$16, %rsi
	movq	$10, -8(%rsi)
	movabsq	$flt60A, %rdx
	movsd	 (%rdx), %xmm14
	movsd	 %xmm14, (%rsi)
	movq	%rsi, %rcx
	addq	$16, %rsi
	movq	$10, -8(%rsi)
	movabsq	$flt7DA, %r10
	movsd	 (%r10), %xmm0
	movsd	 %xmm0, (%rsi)
	movq	%rsi, %rbx
	addq	$16, %rsi
	movq	$28, -8(%rsi)
	movq	%r14, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rbx, 16(%rsi)
	movq	%rsi, %r12
	addq	$32, %rsi
	movq	$10, -8(%rsi)
	movabsq	$flt7CF, %r15
	movsd	 (%r15), %xmm1
	movsd	 %xmm1, (%rsi)
	movq	%rsi, %r14
	addq	$16, %rsi
	movq	$20, -8(%rsi)
	movq	%r12, (%rsi)
	movq	%r14, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	$10, -8(%rsi)
	movabsq	$flt7DB, %rdx
	movsd	 (%rdx), %xmm2
	movsd	 %xmm2, (%rsi)
	movq	%rsi, %rcx
	addq	$16, %rsi
	movq	$10, -8(%rsi)
	movabsq	$flt7D3, %r10
	movsd	 (%r10), %xmm3
	movsd	 %xmm3, (%rsi)
	movq	%rsi, %rbx
	addq	$16, %rsi
	movq	$10, -8(%rsi)
	movabsq	$flt536, %r14
	movsd	 (%r14), %xmm4
	movsd	 %xmm4, (%rsi)
	movq	%rsi, %r12
	addq	$16, %rsi
	movq	$28, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%rbx, 8(%rsi)
	movq	%r12, 16(%rsi)
	movq	%rsi, %rcx
	addq	$32, %rsi
	movq	$10, -8(%rsi)
	movabsq	$flt799, %rbx
	movsd	 (%rbx), %xmm5
	movsd	 %xmm5, (%rsi)
	movq	%rsi, %rdx
	addq	$16, %rsi
	movq	$20, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	$10, -8(%rsi)
	movabsq	$flt7DB, %r12
	movsd	 (%r12), %xmm6
	movsd	 %xmm6, (%rsi)
	movq	%rsi, %r10
	addq	$16, %rsi
	movq	$10, -8(%rsi)
	movabsq	$flt799, %rcx
	movsd	 (%rcx), %xmm7
	movsd	 %xmm7, (%rsi)
	movq	%rsi, %r14
	addq	$16, %rsi
	movq	$10, -8(%rsi)
	movabsq	$flt7CF, %r12
	movsd	 (%r12), %xmm8
	movsd	 %xmm8, (%rsi)
	movq	%rsi, %rdx
	addq	$16, %rsi
	movq	$28, -8(%rsi)
	movq	%r10, (%rsi)
	movq	%r14, 8(%rsi)
	movq	%rdx, 16(%rsi)
	movq	%rsi, %r14
	addq	$32, %rsi
	movq	$10, -8(%rsi)
	movabsq	$flt7DA, %rdx
	movsd	 (%rdx), %xmm9
	movsd	 %xmm9, (%rsi)
	movq	%rsi, %rcx
	addq	$16, %rsi
	movq	$20, -8(%rsi)
	movq	%r14, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	movq	$10, -8(%rsi)
	movabsq	$flt7DB, %r10
	movsd	 (%r10), %xmm10
	movsd	 %xmm10, (%rsi)
	movq	%rsi, %r10
	addq	$16, %rsi
	movq	$10, -8(%rsi)
	movabsq	$flt7DA, %r14
	movsd	 (%r14), %xmm14
	movsd	 %xmm14, (%rsi)
	movq	%rsi, %r12
	addq	$16, %rsi
	movq	$10, -8(%rsi)
	movabsq	$flt60A, %r14
	movsd	 (%r14), %xmm0
	movsd	 %xmm0, (%rsi)
	movq	%rsi, %rcx
	addq	$16, %rsi
	movq	$28, -8(%rsi)
	movq	%r10, (%rsi)
	movq	%r12, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%rsi, %rcx
	addq	$32, %rsi
	movq	$10, -8(%rsi)
	movabsq	$flt7D3, %r12
	movsd	 (%r12), %xmm1
	movsd	 %xmm1, (%rsi)
	movq	%rsi, %r10
	addq	$16, %rsi
	movq	$20, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%r10, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movq	%r14, (%rsi)
	movq	$1, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movq	%r15, (%rsi)
	movq	%rbx, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movq	%r13, (%rsi)
	movq	%r10, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movq	-232(%rbp), %r10
	movq	%r10, (%rsi)
	movq	%r12, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	$28, -8(%rsi)
	movq	$3, (%rsi)
	movq	%r13, 8(%rsi)
	movq	-200(%rbp), %r12
	movq	%r12, 16(%rsi)
	movq	%rsi, %r14
	addq	$32, %rsi
	movq	$20, -8(%rsi)
	movq	%r14, (%rsi)
	movq	$1, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	$18, -8(%rsi)
	movq	$1, (%rsi)
	movabsq	$anon.5C7, %r15
	movq	%r15, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	$26942329749325, %rcx
	movq	%rcx, -8(%rsi)
	movabsq	$letJoinK.7C1, %rdx
	movq	%rdx, (%rsi)
	movq	-240(%rbp), %r15
	movq	8(%r15), %rbx
	movq	%rbx, 8(%rsi)
	movq	-240(%rbp), %rcx
	movq	16(%rcx), %r10
	movq	%r10, 16(%rsi)
	movq	-240(%rbp), %rdx
	movq	24(%rdx), %r12
	movq	%r12, 24(%rsi)
	movq	-240(%rbp), %rbx
	movq	32(%rbx), %r15
	movq	%r15, 32(%rsi)
	movq	-56(%rbp), %r10
	movq	%r10, 40(%rsi)
	movq	-64(%rbp), %r12
	movq	%r12, 48(%rsi)
	movq	-72(%rbp), %r15
	movq	%r15, 56(%rsi)
	movabsq	$flt7C6, %rcx
	movsd	 (%rcx), %xmm2
	movsd	 %xmm2, 64(%rsi)
	movsd	 %xmm15, 72(%rsi)
	movq	-192(%rbp), %rcx
	movq	%rcx, 80(%rsi)
	movq	-80(%rbp), %rdx
	movq	%rdx, 88(%rsi)
	movq	-88(%rbp), %rbx
	movq	%rbx, 96(%rsi)
	movq	-96(%rbp), %r10
	movq	%r10, 104(%rsi)
	movq	-104(%rbp), %r12
	movq	%r12, 112(%rsi)
	movq	-112(%rbp), %r15
	movq	%r15, 120(%rsi)
	movq	-120(%rbp), %rcx
	movq	%rcx, 128(%rsi)
	movq	-128(%rbp), %rdx
	movq	%rdx, 136(%rsi)
	movq	-136(%rbp), %rbx
	movq	%rbx, 144(%rsi)
	movq	-144(%rbp), %r10
	movq	%r10, 152(%rsi)
	movq	-152(%rbp), %r12
	movq	%r12, 160(%rsi)
	movq	-160(%rbp), %r15
	movq	%r15, 168(%rsi)
	movq	-168(%rbp), %rcx
	movq	%rcx, 176(%rsi)
	movq	-176(%rbp), %rdx
	movq	%rdx, 184(%rsi)
	movq	-184(%rbp), %rbx
	movq	%rbx, 192(%rsi)
	movabsq	$flt4D3, %rdx
	movsd	 (%rdx), %xmm3
	movsd	 %xmm3, 200(%rsi)
	movabsq	$flt4D3, %rbx
	movsd	 (%rbx), %xmm4
	movsd	 %xmm4, 208(%rsi)
	movabsq	$flt4D3, %r10
	movsd	 (%r10), %xmm5
	movsd	 %xmm5, 216(%rsi)
	movabsq	$flt4D3, %r12
	movsd	 (%r12), %xmm6
	movsd	 %xmm6, 224(%rsi)
	movabsq	$flt4D3, %r15
	movsd	 (%r15), %xmm7
	movsd	 %xmm7, 232(%rsi)
	movabsq	$flt536, %rcx
	movsd	 (%rcx), %xmm8
	movsd	 %xmm8, 240(%rsi)
	movabsq	$flt7C9, %rdx
	movsd	 (%rdx), %xmm9
	movsd	 %xmm9, 248(%rsi)
	movq	-208(%rbp), %r10
	movq	%r10, 256(%rsi)
	movsd	 %xmm11, 264(%rsi)
	movsd	 %xmm12, 272(%rsi)
	movsd	 %xmm13, 280(%rsi)
	movq	-216(%rbp), %r12
	movq	%r12, 288(%rsi)
	movq	-224(%rbp), %r15
	movq	%r15, 296(%rsi)
	movq	%rsi, %r10
	addq	$312, %rsi
	movq	-240(%rbp), %rcx
	movq	32(%rcx), %rbx
	movq	(%rbx), %rdi
	movq	%r14, %r8
	movq	%r13, %r9
	movq	-240(%rbp), %rdx
	movq	16(%rdx), %r12
	jmp	map_uncurried.5C
doGC7DF:
	movq	$905, -8(%rsi)
	movq	%r14, (%rsi)
	movq	%r13, 8(%rsi)
	movq	%r12, 16(%rsi)
	movl	%r10d, 24(%rsi)
	movq	%rsi, %r10
	addq	$40, %rsi
	movq	%r10, %rdi
	movabsq	$retGC7DD, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.7EA:
	movq	%rdi, %rcx
	jmp	gcTest7EC
retGC7EB:
	movq	(%rdi), %rcx
gcTest7EC:
	movq	%r11, %r15
	subq	%rsi, %r15
	jle	doGC7ED
check.7E0:
	/* block check<10E48> (ep<EED9>) */
	movq	$-1048576, %r13
	andq	%rsi, %r13
	movq	32(%r13), %r14
	cmpq	$1, 8(%r14)
	je	L7EE
L_true7E1:
then.7E3:
	/* block then<10A3A> (ep<10A39>) */
	movq	$-1048576, %r15
	andq	%rsi, %r15
	movq	32(%r15), %rdx
	cmpq	$1, 8(%rdx)
	jne	letJoinK.7E4
L7EF:
else.7E9:
	/* Liveout:  GP={%rax %rdi}  */
	/* block else<10A42> (ep<10A41>) */
	movq	$133, -8(%rsi)
	movabsq	$str87, %r10
	movq	%r10, (%rsi)
	movl	$51, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movabsq	$tag74, %r12
	movq	%r12, (%rsi)
	movq	%rbx, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	16(%rcx), %r13
	movq	(%r13), %r15
	movq	%r14, %rax
	movq	%r13, %rdi
	jmp	*%r15
L7EE:
else.7E2:
	/* block else<10A4B> (ep<10A4A>) */
	movq	$20, -8(%rsi)
	movq	$1, (%rsi)
	movq	$1, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	$-1048576, %r10
	andq	%rsi, %r10
	movq	32(%r10), %rdx
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
	movq	$-1048576, %r12
	andq	%rsi, %r12
	movq	%r10, 32(%r12)
letJoinK.7E4:
	/* block letJoinK<EEE1> (ep<EEE0>) */
	movq	$-1048576, %rbx
	andq	%rsi, %rbx
	movq	32(%rbx), %r15
	movq	8(%r15), %rdx
	cmpq	$1, %rdx
	jne	L_true7E5
else.7E6:
	/* Liveout:  GP={%rax %rdi}  */
	/* block else<10A31> (ep<10A30>) */
	movq	$133, -8(%rsi)
	movabsq	$str87, %rbx
	movq	%rbx, (%rsi)
	movl	$51, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movabsq	$tag74, %r10
	movq	%r10, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	16(%rcx), %r10
	movq	(%r10), %r13
	movq	%r12, %rax
	movq	%r10, %rdi
	jmp	*%r13
L_true7E5:
then.7E7:
	/* Liveout:  GP={%r13 %r12 %r10 %r9 %r8 %rdi}  */
	/* block then<EEE8> (ep<EEE6>,_t<EEE7>) */
	movq	(%rdx), %r14
	movq	$20, -8(%rsi)
	movq	56(%rcx), %rdx
	movq	%rdx, (%rsi)
	movq	(%r14), %rbx
	movq	%rbx, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movq	%r15, (%rsi)
	movq	8(%r14), %r12
	movq	%r12, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	$-1048576, %rdx
	andq	%rsi, %rdx
	movq	32(%rdx), %r13
	movq	$12, -8(%rsi)
	movq	%r10, (%rsi)
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
	movq	$-1048576, %rbx
	andq	%rsi, %rbx
	movq	%r15, 32(%rbx)
	xorl	%r15d, %r15d
	movq	$10, -8(%rsi)
	movl	%r15d, (%rsi)
	movq	%rsi, %rdx
	addq	$16, %rsi
	movq	$18, -8(%rsi)
	movq	$1, (%rsi)
	movabsq	$anon.21C, %r12
	movq	%r12, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	$16143, -8(%rsi)
	movabsq	$letJoinK.7DC, %r13
	movq	%r13, (%rsi)
	movq	8(%rcx), %r14
	movq	%r14, 8(%rsi)
	movq	16(%rcx), %rbx
	movq	%rbx, 16(%rsi)
	movq	24(%rcx), %r12
	movq	%r12, 24(%rsi)
	movq	32(%rcx), %r13
	movq	%r13, 32(%rsi)
	movq	40(%rcx), %r14
	movq	%r14, 40(%rsi)
	movq	48(%rcx), %rbx
	movq	%rbx, 48(%rsi)
	movq	%rsi, %r12
	addq	$64, %rsi
	movq	40(%rcx), %r13
	movq	(%r13), %rdi
	movq	%rdx, %r8
	movq	%r15, %r9
	movq	16(%rcx), %r13
	jmp	tabulate.7E
doGC7ED:
	movq	$12, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%rsi, %r14
	addq	$16, %rsi
	movq	%r14, %rdi
	movabsq	$retGC7EB, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.215:
	movq	%rdi, %rcx
	jmp	gcTest7F2
	/* live= GP={%rcx} spilled=  */
retGC7F1:
	movq	(%rdi), %rcx
gcTest7F2:
	movq	%r11, %r13
	subq	%rsi, %r13
	jle	doGC7F3
check.7F0:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block check<10E4A> (ep<EECF>) */
	movq	$32529, -8(%rsi)
	movabsq	$letJoinK.7EA, %rbx
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
	movq	64(%rcx), %rbx
	movq	%rbx, 56(%rsi)
	movq	%rsi, %rdx
	addq	$72, %rsi
	movq	48(%rcx), %r10
	movq	(%r10), %rdi
	movq	72(%rcx), %r8
	movq	%rdx, %r9
	movq	16(%rcx), %r10
	jmp	wait.AA
doGC7F3:
	movq	$12, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%rsi, %r12
	addq	$16, %rsi
	movq	%r12, %rdi
	movabsq	$retGC7F1, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.7F5:
	movq	%rdi, %rcx
	jmp	gcTest7F7
	/* live= GP={%rcx} spilled=  */
retGC7F6:
	movq	(%rdi), %rcx
gcTest7F7:
	movq	%r11, %r12
	subq	%rsi, %r12
	jle	doGC7F8
check.7F4:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block check<10E4C> (ep<10A68>) */
	movq	16(%rcx), %rdx
	movq	(%rdx), %rdi
	movl	24(%rcx), %ebx
	movq	%rbx, %r8
	incl	%r8d
	movq	32(%rcx), %r9
	movq	8(%rcx), %r10
	jmp	spawn.20C
doGC7F8:
	movq	$12, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%rsi, %r10
	addq	$16, %rsi
	movq	%r10, %rdi
	movabsq	$retGC7F6, %r8
	jmp	ASM_InvokeGC
	.text
k.7FE:
	movq	%rax, %r13
	movq	%rdi, %r12
	jmp	gcTest800
	/* live= GP={%r13} spilled= GP={%r~1}  */
retGC7FF:
	movq	8(%rdi), %r13
	movq	(%rdi), %r12
gcTest800:
	movq	%r11, %r14
	subq	%rsi, %r14
	jle	doGC801
	movq	%r12, -120(%rbp)
check.7F9:
	/* block check<10E4F> (ep<EE0A>,x<EE02>) */
	movq	$36, -8(%rsi)
	movq	-120(%rbp), %r15
	movq	88(%r15), %r14
	movq	%r14, (%rsi)
	movq	-120(%rbp), %rbx
	movq	64(%rbx), %r15
	movq	%r15, 8(%rsi)
	movq	-120(%rbp), %r12
	movq	72(%r12), %rbx
	movq	%rbx, 16(%rsi)
	movq	-120(%rbp), %r13
	movq	80(%r13), %r12
	movq	%r12, 24(%rsi)
	movq	%rsi, -56(%rbp)
	addq	$40, %rsi
	movq	$-1048576, %rbx
	andq	%rsi, %rbx
	movq	%rsi, 120(%rbx)
	movq	$1, (%rbx)
	movq	%rax, -72(%rbp)
	movq	%rcx, %r12
	movq	%rdx, %r13
	movq	%rdi, -64(%rbp)
	movq	%r8, %r14
	movq	%r9, -80(%rbp)
	movq	%r10, -88(%rbp)
	movq	%r11, %r15
	movq	$-1048576, %rcx
	andq	%rsi, %rcx
	movq	%rcx, %rdi
	movq	-56(%rbp), %rsi
	call	PromoteObj
	movq	%rax, -56(%rbp)
	movq	-72(%rbp), %rax
	movq	%r12, %rcx
	movq	%r13, %rdx
	movq	-64(%rbp), %rdi
	movq	%r14, %r8
	movq	-80(%rbp), %r9
	movq	-88(%rbp), %r10
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
	movabsq	$spawnFn.1FC, %rbx
	movq	%rbx, 8(%rsi)
	movq	%rsi, -64(%rbp)
	addq	$24, %rsi
	movq	$-1048576, %rbx
	andq	%rsi, %rbx
	movq	$1, (%rbx)
	movq	%rax, -104(%rbp)
	movq	%rcx, -112(%rbp)
	movq	%rdx, -80(%rbp)
	movq	%rsi, %r12
	movq	%rdi, -96(%rbp)
	movq	%r8, %r13
	movq	%r9, %r14
	movq	%r10, -88(%rbp)
	movq	%r11, %r15
	call	GetNumVProcs
	movq	%rax, -72(%rbp)
	movq	-104(%rbp), %rax
	movq	-112(%rbp), %rcx
	movq	-80(%rbp), %rdx
	movq	%r12, %rsi
	movq	-96(%rbp), %rdi
	movq	%r13, %r8
	movq	%r14, %r9
	movq	-88(%rbp), %r10
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
	movq	%rdi, -88(%rbp)
	movq	%r8, -96(%rbp)
	movq	%r9, -104(%rbp)
	movq	%r10, -112(%rbp)
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
	movq	-88(%rbp), %rdi
	movq	-96(%rbp), %r8
	movq	-104(%rbp), %r9
	movq	-112(%rbp), %r10
	movq	%rbx, %r11
	movq	120(%r12), %rsi
	movq	$3, (%r12)
	movq	$44, -8(%rsi)
	movabsq	$init.207, %rcx
	movq	%rcx, (%rsi)
	movq	-120(%rbp), %r15
	movq	16(%r15), %rdx
	movq	%rdx, 8(%rsi)
	movq	-120(%rbp), %rcx
	movq	48(%rcx), %rbx
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
	movabsq	$spawn.20C, %r13
	movq	%r13, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	xorl	%r15d, %r15d
	movq	$130837, -8(%rsi)
	movabsq	$letJoinK.215, %r14
	movq	%r14, (%rsi)
	movq	-120(%rbp), %r13
	movq	8(%r13), %rbx
	movq	%rbx, 8(%rsi)
	movq	-120(%rbp), %r14
	movq	16(%r14), %r10
	movq	%r10, 16(%rsi)
	movq	-120(%rbp), %rbx
	movq	24(%rbx), %r12
	movq	%r12, 24(%rsi)
	movq	-120(%rbp), %r10
	movq	32(%r10), %r13
	movq	%r13, 32(%rsi)
	movq	-120(%rbp), %r12
	movq	40(%r12), %r14
	movq	%r14, 40(%rsi)
	movq	-120(%rbp), %r13
	movq	48(%r13), %rbx
	movq	%rbx, 48(%rsi)
	movq	-120(%rbp), %r14
	movq	56(%r14), %r10
	movq	%r10, 56(%rsi)
	movq	-56(%rbp), %rbx
	movq	%rbx, 64(%rsi)
	movq	-80(%rbp), %r10
	movq	%r10, 72(%rsi)
	movq	%rsi, %r13
	addq	$88, %rsi
	cmpl	-72(%rbp), %r15d
	jl	L802
L_true7FB:
then.7FD:
	/* Liveout:  GP={%rdi}  */
	/* block then<10A5E> (letJoinK<10A5D>) */
	movq	%r13, %rdi
	jmp	letJoinK.215
doGC801:
	movq	$20, -8(%rsi)
	movq	%r12, (%rsi)
	movq	%r13, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	%r10, %rdi
	movabsq	$retGC7FF, %r8
	jmp	ASM_InvokeGC
L802:
	movq	-120(%rbp), %r12
else.7FC:
	/* Liveout:  GP={%r12 %r10 %r9 %r8 %rdi}  */
	/* block else<10A66> (ep<10A60>,spawnFn<10A65>,init<10A64>,spawn<10A63>,_t<10A62>,letJoinK<10A61>) */
	movq	$2827, -8(%rsi)
	movabsq	$letJoinK.7F5, %r14
	movq	%r14, (%rsi)
	movq	16(%r12), %rbx
	movq	%rbx, 8(%rsi)
	movq	%rdx, 16(%rsi)
	movl	%r15d, 24(%rsi)
	movq	%r13, 32(%rsi)
	movq	%rsi, %r10
	addq	$48, %rsi
	movq	-64(%rbp), %r13
	movq	(%r13), %rdi
	movq	%r15, %r8
	movq	%rcx, %r9
	movq	16(%r12), %r12
	jmp	spawnFn.1FC
	.text
letJoinK.804:
	movq	%rdi, %rcx
	jmp	gcTest806
	/* live= GP={%rcx} spilled=  */
retGC805:
	movq	(%rdi), %rcx
gcTest806:
	movq	%r11, %rdx
	subq	%rsi, %rdx
	jle	doGC807
check.803:
	/* Liveout:  GP={%rax %rdi}  */
	/* block check<10E51> (ep<10A7D>) */
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
doGC807:
	movq	$12, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%rsi, %r15
	addq	$16, %rsi
	movq	%r15, %rdi
	movabsq	$retGC805, %r8
	jmp	ASM_InvokeGC
	.text
k.809:
	movq	%rax, %rcx
	movq	%rdi, %rdx
	jmp	gcTest80B
	/* live= GP={%rcx %rdx} spilled=  */
retGC80A:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest80B:
	movq	%r11, %r10
	subq	%rsi, %r10
	jle	doGC80C
check.808:
	/* Liveout:  GP={%rdi}  */
	/* block check<10E54> (ep<10A93>,x<10A92>) */
	movq	8(%rdx), %rdi
	jmp	letJoinK.804
doGC80C:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	%rbx, %rdi
	movabsq	$retGC80A, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.812:
	movq	%r8, %rbx
	movq	%rdi, %r13
gcTest814:
	movq	%r11, %r12
	subq	%rsi, %r12
	jle	doGC815
check.80D:
	/* block check<10E57> (ep<EC5F>,act<EC56>) */
	movq	$20, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	64(%r13), %r14
	movq	40(%r14), %r15
	movq	%r15, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	64(%r13), %rbx
	movq	%r12, 40(%rbx)
	movq	64(%r13), %r12
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
	movq	$36, -8(%rsi)
	movabsq	$schedulerLoop.1BA, %rdx
	movq	%rdx, (%rsi)
	movq	16(%r13), %rbx
	movq	%rbx, 8(%rsi)
	movq	56(%r13), %r10
	movq	%r10, 16(%rsi)
	movq	%r15, 24(%rsi)
	movq	%rsi, %rcx
	addq	$40, %rsi
	movq	$20, -8(%rsi)
	movabsq	$initK.1C0, %r12
	movq	%r12, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	$12, -8(%rsi)
	movq	%r15, (%rsi)
	movq	%rsi, %r14
	addq	$16, %rsi
	movq	$20, -8(%rsi)
	movq	%r14, (%rsi)
	movabsq	$spawnFn.1D8, %r15
	movq	%r15, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	movq	$18, -8(%rsi)
	movq	$1, (%rsi)
	movabsq	$removeFn.1DD, %rcx
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	$100, -8(%rsi)
	movabsq	$k.7FE, %rbx
	movq	%rbx, (%rsi)
	movq	8(%r13), %r10
	movq	%r10, 8(%rsi)
	movq	16(%r13), %r14
	movq	%r14, 16(%rsi)
	movq	24(%r13), %r15
	movq	%r15, 24(%rsi)
	movq	32(%r13), %rbx
	movq	%rbx, 32(%rsi)
	movq	40(%r13), %r10
	movq	%r10, 40(%rsi)
	movq	48(%r13), %r14
	movq	%r14, 48(%rsi)
	movq	56(%r13), %r15
	movq	%r15, 56(%rsi)
	movq	%rdx, 64(%rsi)
	movq	%rcx, 72(%rsi)
	movq	$1, 80(%rsi)
	movq	%r12, 88(%rsi)
	movq	%rsi, %rdx
	addq	$104, %rsi
	movq	$-1048576, %r15
	andq	%rsi, %r15
	movq	$3, 8(%r15)
	movq	$28, -8(%rsi)
	movq	$-1048576, %rbx
	andq	%rsi, %rbx
	movq	32(%rbx), %r10
	movq	%r10, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	80(%r15), %r12
	movq	%r12, 16(%rsi)
	movq	%rsi, %rcx
	addq	$32, %rsi
	movq	%rcx, 80(%r15)
	movq	$10, -8(%rsi)
	movabsq	$letJoinK.804, %r14
	movq	%r14, (%rsi)
	movq	%rsi, %r13
	addq	$16, %rsi
	movq	16(%r15), %rcx
	cmpq	$1, %rcx
	jne	L816
S_case80E:
case.80F:
	/* Liveout:  GP={%rdi}  */
	/* block case<10A8A> (vp<10A89>,letJoinK<10A88>) */
	movq	$1, 8(%r15)
	movq	%r13, %rdi
	jmp	letJoinK.804
doGC815:
	movq	$20, -8(%rsi)
	movq	%r13, (%rsi)
	movq	%rbx, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	%rcx, %rdi
	movabsq	$retGC813, %r8
	jmp	ASM_InvokeGC
	/* live= GP={%rbx %r13} spilled=  */
retGC813:
	movq	8(%rdi), %rbx
	movq	(%rdi), %r13
	jmp	gcTest814
L816:
	cmpq	$3, %rcx
	jne	S_case80E
S_case810:
case.811:
	/* Liveout:  GP={%rax %rdi}  */
	/* block case<10A90> (vp<10A8F>,letJoinK<10A8E>) */
	movq	$1, 16(%r15)
	movq	$20, -8(%rsi)
	movabsq	$k.809, %rbx
	movq	%rbx, (%rsi)
	movq	%r13, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movq	$1, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	40(%r15), %r12
	movq	8(%r12), %r13
	movq	%r13, 40(%r15)
	movq	(%r12), %r14
	movq	(%r14), %r15
	movq	%r10, %rax
	movq	%r14, %rdi
	jmp	*%r15
	.text
letJoinK.17F:
	movq	%rdi, %rcx
	jmp	gcTest819
	/* live= GP={%rcx} spilled=  */
retGC818:
	movq	(%rdi), %rcx
gcTest819:
	movq	%r11, %r14
	subq	%rsi, %r14
	jle	doGC81A
check.817:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block check<10E59> (ep<EC53>) */
	movq	$32531, -8(%rsi)
	movabsq	$letJoinK.812, %rbx
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
	movq	48(%rcx), %rdx
	movq	%rdx, 48(%rsi)
	movq	56(%rcx), %rbx
	movq	%rbx, 56(%rsi)
	movq	72(%rcx), %r10
	movq	%r10, 64(%rsi)
	movq	%rsi, %rdx
	addq	$80, %rsi
	movq	64(%rcx), %r12
	movq	(%r12), %rdi
	movq	72(%rcx), %r8
	movq	%rdx, %r9
	movq	16(%rcx), %r10
	jmp	mkSwitch.15D
doGC81A:
	movq	$12, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%rsi, %r13
	addq	$16, %rsi
	movq	%r13, %rdi
	movabsq	$retGC818, %r8
	jmp	ASM_InvokeGC
	.text
letJoinK.81C:
	movq	%rdi, %rcx
	jmp	gcTest81E
	/* live= GP={%rcx} spilled=  */
retGC81D:
	movq	(%rdi), %rcx
gcTest81E:
	movq	%r11, %r10
	subq	%rsi, %r10
	jle	doGC81F
check.81B:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block check<10E5B> (ep<10AAE>) */
	movq	8(%rcx), %rdx
	movq	(%rdx), %rdi
	movq	24(%rcx), %r8
	movq	16(%rcx), %r9
	jmp	lp.176
doGC81F:
	movq	$12, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%rsi, %rbx
	addq	$16, %rsi
	movq	%rbx, %rdi
	movabsq	$retGC81D, %r8
	jmp	ASM_InvokeGC
	.text
main.826:
Main_init:
mantEntry:
	movq	%r9, %r13
	movq	%r8, %r14
	movq	%rax, %r12
	movq	%rdi, %rbx
gcTest82A:
	movq	%r11, %r15
	subq	%rsi, %r15
	jg	L82C
doGC82B:
	movq	$36, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%r12, 8(%rsi)
	movq	%r14, 16(%rsi)
	movq	%r13, 24(%rsi)
	movq	%rsi, %rcx
	addq	$40, %rsi
	movq	%rcx, %rdi
	movabsq	$retGC829, %r8
	jmp	ASM_InvokeGC
L82C:
	movq	%r14, -96(%rbp)
check.824:
	/* block check<10E60> (dummyEP<E806>,argFormalWrap<10ABC>,retK<E808>,_exh<E809>) */
	movq	$18, -8(%rsi)
	movq	$1, (%rsi)
	movabsq	$foldr_uncurried.40, %rbx
	movq	%rbx, 8(%rsi)
	movq	%rsi, -64(%rbp)
	addq	$24, %rsi
	movq	$18, -8(%rsi)
	movq	$1, (%rsi)
	movabsq	$map_uncurried.5C, %r12
	movq	%r12, 8(%rsi)
	movq	%rsi, -72(%rbp)
	addq	$24, %rsi
	movq	$18, -8(%rsi)
	movq	$1, (%rsi)
	movabsq	$tabulate.7E, %r14
	movq	%r14, 8(%rsi)
	movq	%rsi, -80(%rbp)
	addq	$24, %rsi
	movq	$18, -8(%rsi)
	movq	$1, (%rsi)
	movabsq	$get_D_ite.88, %rbx
	movq	%rbx, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	$18, -8(%rsi)
	movq	$1, (%rsi)
	movabsq	$set_D_ite.8E, %r14
	movq	%r14, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	$18, -8(%rsi)
	movq	$1, (%rsi)
	movabsq	$wait.AA, %rbx
	movq	%rbx, 8(%rsi)
	movq	%rsi, -88(%rbp)
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movq	%r15, (%rsi)
	movq	%r12, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	$133, -8(%rsi)
	movq	%r12, (%rsi)
	movabsq	$wrap.F3, %r14
	movq	%r14, 8(%rsi)
	movq	%rsi, -104(%rbp)
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movabsq	$switch.157, %rbx
	movq	%rbx, (%rsi)
	movq	%r13, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	$12, -8(%rsi)
	movq	%r15, (%rsi)
	movq	%rsi, %r12
	addq	$16, %rsi
	movq	$133, -8(%rsi)
	movq	%r12, (%rsi)
	movabsq	$mkSwitch.15D, %r14
	movq	%r14, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	$-1048576, %r14
	andq	%rsi, %r14
	movq	$3, 8(%r14)
	movq	$12, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rsi, %r15
	addq	$16, %rsi
	movq	$133, -8(%rsi)
	movq	%r15, (%rsi)
	movabsq	$initVPFields.171, %r12
	movq	%r12, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movq	%r13, (%rsi)
	movq	%r12, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	$133, -8(%rsi)
	movq	%r15, (%rsi)
	movabsq	$lp.176, %r15
	movq	%r15, 8(%rsi)
	movq	%rsi, -56(%rbp)
	addq	$24, %rsi
	movq	%r12, -112(%rbp)
	movq	%r14, -120(%rbp)
	movq	%rbx, -128(%rbp)
	movq	%r13, -136(%rbp)
	movq	$229271, -8(%rsi)
	movq	-96(%rbp), %r12
	movq	%r12, (%rsi)
	movq	-136(%rbp), %r13
	movq	%r13, 8(%rsi)
	movq	-64(%rbp), %r14
	movq	%r14, 16(%rsi)
	movq	-72(%rbp), %r15
	movq	%r15, 24(%rsi)
	movq	-80(%rbp), %rbx
	movq	%rbx, 32(%rsi)
	movq	-88(%rbp), %r12
	movq	%r12, 40(%rsi)
	movq	-104(%rbp), %r13
	movq	%r13, 48(%rsi)
	movq	-128(%rbp), %r14
	movq	%r14, 56(%rsi)
	movq	-120(%rbp), %r15
	movq	%r15, 64(%rsi)
	movq	-112(%rbp), %rbx
	movq	%rbx, 72(%rsi)
	movq	-56(%rbp), %r12
	movq	%r12, 80(%rsi)
	movq	%rsi, %rbx
	addq	$96, %rsi
	movq	$-1048576, %r12
	andq	%rsi, %r12
	movq	%rbx, 96(%r12)
	movq	%rsi, 120(%r12)
	movq	%r11, 320(%r12)
	movq	$-1048576, %r15
	andq	%rsi, %r15
	movq	$1, (%r15)
	movq	%rax, -72(%rbp)
	movq	%rsi, -80(%rbp)
	movq	%rdi, -64(%rbp)
	movq	%r8, %r12
	movq	%r9, %r13
	movq	%r11, %r14
	movq	$-1048576, %rbx
	andq	%rsi, %rbx
	movq	%rbx, %rdi
	call	ListVProcs
	movq	%rax, -56(%rbp)
	movq	-72(%rbp), %rax
	movq	-80(%rbp), %rsi
	movq	-64(%rbp), %rdi
	movq	%r12, %r8
	movq	%r13, %r9
	movq	%r14, %r11
	movq	$3, (%r15)
	movq	$-1048576, %r15
	andq	%rsi, %r15
	movq	120(%r15), %rsi
	movq	320(%r15), %r11
	movq	96(%r15), %r13
	movq	80(%r13), %r13
	movq	%r13, -64(%rbp)
	movq	96(%r15), %r14
	movq	72(%r14), %r14
	movq	%r14, -72(%rbp)
	movq	96(%r15), %rcx
	movq	64(%rcx), %rcx
	movq	%rcx, -80(%rbp)
	movq	96(%r15), %rdx
	movq	56(%rdx), %rdx
	movq	%rdx, -88(%rbp)
	movq	96(%r15), %rbx
	movq	48(%rbx), %rbx
	movq	%rbx, -96(%rbp)
	movq	96(%r15), %r10
	movq	40(%r10), %rbx
	movq	96(%r15), %r12
	movq	32(%r12), %r14
	movq	96(%r15), %r13
	movq	24(%r13), %r13
	movq	96(%r15), %rcx
	movq	16(%rcx), %r12
	movq	96(%r15), %rdx
	movq	8(%rdx), %r10
	movq	96(%r15), %r15
	movq	(%r15), %rdx
	movq	-56(%rbp), %rcx
allocCCall.820:
	/* block allocCCall<10E6C> (vps<EC50>,retK<10E61>,_exh<10E62>,foldr_uncurried<10E63>,map_uncurried<10E64>,tabulate<10E65>,wait<10E66>,wrap<10E67>,mkSwitch<10E68>,vp<10E69>,initVPFields<10E6A>,lp<10E6B>) */
	movq	$65301, -8(%rsi)
	movabsq	$letJoinK.17F, %r15
	movq	%r15, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%r10, 16(%rsi)
	movq	%r12, 24(%rsi)
	movq	%r13, 32(%rsi)
	movq	%r14, 40(%rsi)
	movq	%rbx, 48(%rsi)
	movq	-96(%rbp), %r15
	movq	%r15, 56(%rsi)
	movq	-88(%rbp), %rdx
	movq	%rdx, 64(%rsi)
	movq	-80(%rbp), %rbx
	movq	%rbx, 72(%rsi)
	movq	%rsi, %rbx
	addq	$88, %rsi
	cmpq	$1, %rcx
	je	L82D
L_true821:
then.823:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block then<10AA9> (_exh<10AA8>,initVPFields<10AA7>,lp<10AA6>,vps<10AA5>,letJoinK<10AA4>) */
	movq	$1801, -8(%rsi)
	movabsq	$letJoinK.81C, %r12
	movq	%r12, (%rsi)
	movq	-64(%rbp), %r12
	movq	%r12, 8(%rsi)
	movq	%rbx, 16(%rsi)
	movq	8(%rcx), %r13
	movq	%r13, 24(%rsi)
	movq	%rsi, %rdx
	addq	$40, %rsi
	movq	-72(%rbp), %r13
	movq	(%r13), %rdi
	movq	(%rcx), %r14
	movq	(%r14), %r8
	movq	%rdx, %r9
	jmp	initVPFields.171
L82D:
else.822:
	/* Liveout:  GP={%rdi}  */
	/* block else<10ABA> (letJoinK<10AB9>) */
	movq	%rbx, %rdi
	jmp	letJoinK.17F
	/* live= GP={%r13 %r12 %rbx} spilled= GP={%r~1}  */
retGC829:
	movq	24(%rdi), %r13
	movq	16(%rdi), %r14
	movq	8(%rdi), %r12
	movq	(%rdi), %rbx
	jmp	gcTest82A
	.global	mantEntry
	.global	Main_init
	.text
letJoinK.788:
	movq	%r8, %r12
	movq	%rdi, %rbx
	jmp	gcTest836
	/* live= GP={%r12} spilled= GP={%r~1}  */
retGC835:
	movq	8(%rdi), %r12
	movq	(%rdi), %rbx
gcTest836:
	movq	%r11, %r13
	subq	%rsi, %r13
	jle	doGC837
	movq	%rbx, -56(%rbp)
check.830:
	/* block check<10E17> (ep<109D9>,unused<109D5>) */
	movq	$-1048576, %rbx
	andq	%rsi, %rbx
	movq	$1, (%rbx)
	movq	%rax, %r15
	movq	%rcx, %r14
	movq	%rdx, -64(%rbp)
	movq	%rsi, -72(%rbp)
	movq	%rdi, -88(%rbp)
	movq	%r8, -96(%rbp)
	movq	%r9, %r13
	movq	%r10, -80(%rbp)
	movq	%r11, %r12
	movq	-56(%rbp), %r10
	movq	16(%r10), %r10
	movq	%r10, %rdi
	movabsq	$str831, %rcx
	movq	%rcx, %rsi
	call	M_OutputImage
	movq	%r15, %rax
	movq	%r14, %rcx
	movq	-64(%rbp), %rdx
	movq	-72(%rbp), %rsi
	movq	-88(%rbp), %rdi
	movq	-96(%rbp), %r8
	movq	%r13, %r9
	movq	-80(%rbp), %r10
	movq	%r12, %r11
	movq	$3, (%rbx)
	movq	$-1048576, %r15
	andq	%rsi, %r15
	movq	$1, (%r15)
	movq	%rax, -64(%rbp)
	movq	%rcx, %r14
	movq	%rdx, %r13
	movq	%rsi, -72(%rbp)
	movq	%rdi, -96(%rbp)
	movq	%r8, %r12
	movq	%r9, %rbx
	movq	%r10, -88(%rbp)
	movq	%r11, -80(%rbp)
	movq	-56(%rbp), %rcx
	movq	16(%rcx), %rcx
	movq	%rcx, %rdi
	call	M_FreeImage
	movq	-64(%rbp), %rax
	movq	%r14, %rcx
	movq	%r13, %rdx
	movq	-72(%rbp), %rsi
	movq	-96(%rbp), %rdi
	movq	%r12, %r8
	movq	%rbx, %r9
	movq	-88(%rbp), %r10
	movq	-80(%rbp), %r11
	movq	$3, (%r15)
	movq	-56(%rbp), %rbx
	movq	$12, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rsi, %r12
	addq	$16, %rsi
	movq	$-1048576, %r13
	andq	%rsi, %r13
	movq	%r12, 96(%r13)
	movq	%rsi, 120(%r13)
	movq	%r11, 320(%r13)
	movq	$-1048576, %r12
	andq	%rsi, %r12
	movq	$1, (%r12)
	movq	%rax, -64(%rbp)
	movq	%rsi, -72(%rbp)
	movq	%rdi, -80(%rbp)
	movq	%r8, %r15
	movq	%r9, -112(%rbp)
	movq	%r11, %r14
	movq	-56(%rbp), %rbx
	movq	32(%rbx), %r13
	subq	24(%rbx), %r13
	movq	%r13, %rdi
	call	M_LongToString
	movq	%rax, -56(%rbp)
	movq	-64(%rbp), %rax
	movq	-72(%rbp), %rsi
	movq	-80(%rbp), %rdi
	movq	%r15, %r8
	movq	-112(%rbp), %r9
	movq	%r14, %r11
	movq	$3, (%r12)
	movq	$-1048576, %r14
	andq	%rsi, %r14
	movq	120(%r14), %rsi
	movq	320(%r14), %r11
	movq	96(%r14), %r15
	movq	(%r15), %r12
	movq	%r12, -64(%rbp)
allocCCall.82E:
	/* Liveout:  GP={%rax %rdi}  */
	/* block allocCCall<10E6F> (res<109E4>,ep<10E6E>) */
	movq	$-1048576, %r12
	andq	%rsi, %r12
	movq	$1, (%r12)
	movq	%rax, -72(%rbp)
	movq	%rcx, -96(%rbp)
	movq	%rdx, %rbx
	movq	%rsi, %r15
	movq	%rdi, -80(%rbp)
	movq	%r8, -88(%rbp)
	movq	%r9, -104(%rbp)
	movq	%r10, %r14
	movq	%r11, %r13
	movq	-56(%rbp), %rcx
	movq	(%rcx), %rcx
	movq	%rcx, %rdi
	call	M_Print
	movq	-72(%rbp), %rax
	movq	-96(%rbp), %rcx
	movq	%rbx, %rdx
	movq	%r15, %rsi
	movq	-80(%rbp), %rdi
	movq	-88(%rbp), %r8
	movq	-104(%rbp), %r9
	movq	%r14, %r10
	movq	%r13, %r11
	movq	$3, (%r12)
	movq	-64(%rbp), %rdx
	movq	8(%rdx), %rdx
	movq	(%rdx), %rbx
	movq	$1, %r10
	movq	%r10, %rax
	movq	%rdx, %rdi
	jmp	*%rbx
doGC837:
	movq	$20, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%r12, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	%rcx, %rdi
	movabsq	$retGC835, %r8
	jmp	ASM_InvokeGC
	.text
sub.776:
	movq	%r13, -88(%rbp)
	movq	%r12, -64(%rbp)
	movq	%r9, %rbx
	movq	%r8, %r13
	movq	%rdi, -104(%rbp)
	movq	%r10, %r15
	movq	-104(%rbp), %r12
gcTest84E:
	movq	%r11, %r14
	subq	%rsi, %r14
	jg	L850
doGC84F:
	movq	$7053, -8(%rsi)
	movq	%r12, (%rsi)
	movq	%r13, 8(%rsi)
	movq	%rbx, 16(%rsi)
	movl	%r15d, 24(%rsi)
	movq	-64(%rbp), %r15
	movq	%r15, 32(%rsi)
	movq	-88(%rbp), %rcx
	movq	%rcx, 40(%rsi)
	movq	%rsi, %r13
	addq	$56, %rsi
	movq	%r13, %rdi
	movabsq	$retGC84D, %r8
	jmp	ASM_InvokeGC
L850:
check.842:
	/* block check<10CCE> (ep<FA08>,pa<FA09>,i<FA0A>,_t<FA0B>,retK<FA0C>,_exh<FA0D>) */
	cmpl	$0, %r15d
	jge	L_true843
	movq	%r13, -96(%rbp)
	movq	%rbx, -56(%rbp)
	movq	%r12, -80(%rbp)
else.841:
	/* block else<FA69> (ep<FA64>,retK<FA68>,_exh<FA67>,i<FA66>,_t<FA65>) */
	movq	-88(%rbp), %r14
	movq	$1, %r15
	jmp	letJoinK.83D
L851:
	movq	-88(%rbp), %r14
else.84B:
	/* block else<FA50> (ep<FA4B>,retK<FA4F>,_exh<FA4E>,i<FA4D>,_t<FA4C>) */
	movq	$1, %r15
letJoinK.83D:
	/* block letJoinK<FA19> (ep<FA13>,retK<FA18>,_exh<FA17>,i<FA16>,_t<FA15>,_t<FA14>) */
	cmpq	$1, %r15
	je	S_case83E
	cmpq	$3, %r15
	jne	S_case83E
S_case83F:
	movq	-56(%rbp), %r13
	movq	-64(%rbp), %r12
case.840:
	/* Liveout:  GP={%r12 %r10 %r9 %r8 %rdi}  */
	/* block case<FA2E> (ep<FA2A>,retK<FA2D>,i<FA2C>,_t<FA2B>) */
	movq	-80(%rbp), %r14
	movq	(%r14), %r15
	movq	(%r15), %rdi
	movq	-96(%rbp), %r8
	movq	%r13, %r9
	movl	(%r13), %r10d
	jmp	subInBounds.236
S_case83E:
case.839:
	/* block case<FA1C> (_exh<FA1B>) */
	movq	$133, -8(%rsi)
	movabsq	$str83A, %r13
	movq	%r13, (%rsi)
	movl	$23, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	$133, -8(%rsi)
	movabsq	$str83B, %r15
	movq	%r15, (%rsi)
	movl	$1, 8(%rsi)
	movq	%rsi, -72(%rbp)
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movq	%r14, (%rsi)
	movq	%rbx, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	$-1048576, %r13
	andq	%rsi, %r13
	movq	%r12, 96(%r13)
	movq	%rsi, 120(%r13)
	movq	%r11, 320(%r13)
	movq	$-1048576, %r12
	andq	%rsi, %r12
	movq	$1, (%r12)
	movq	%rax, -88(%rbp)
	movq	%rsi, -64(%rbp)
	movq	%rdi, -80(%rbp)
	movq	%r8, %r13
	movq	%r9, %r14
	movq	%r11, %r15
	movq	%rbx, %rdi
	movq	-72(%rbp), %rsi
	call	M_StringConcat2
	movq	%rax, -56(%rbp)
	movq	-88(%rbp), %rax
	movq	-64(%rbp), %rsi
	movq	-80(%rbp), %rdi
	movq	%r13, %r8
	movq	%r14, %r9
	movq	%r15, %r11
	movq	$3, (%r12)
	movq	$-1048576, %r14
	andq	%rsi, %r14
	movq	120(%r14), %rsi
	movq	320(%r14), %r11
	movq	96(%r14), %r15
	movq	8(%r15), %r12
	movq	%r12, -64(%rbp)
	movq	96(%r14), %rbx
	movq	(%rbx), %r13
	movq	%r13, -80(%rbp)
allocCCall.838:
	/* Liveout:  GP={%rax %rdi}  */
	/* block allocCCall<10E73> (res<FA23>,_exh<10E71>,_slit<10E72>) */
	movq	$-1048576, %rbx
	andq	%rsi, %rbx
	movq	$1, (%rbx)
	movq	%rax, %r14
	movq	%rcx, %r15
	movq	%rdx, %r12
	movq	%rsi, -88(%rbp)
	movq	%rdi, -72(%rbp)
	movq	%r8, -96(%rbp)
	movq	%r9, -104(%rbp)
	movq	%r10, -112(%rbp)
	movq	%r11, %r13
	movq	-56(%rbp), %rcx
	movq	(%rcx), %rcx
	movq	%rcx, %rdi
	call	M_Print
	movq	%r14, %rax
	movq	%r15, %rcx
	movq	%r12, %rdx
	movq	-88(%rbp), %rsi
	movq	-72(%rbp), %rdi
	movq	-96(%rbp), %r8
	movq	-104(%rbp), %r9
	movq	-112(%rbp), %r10
	movq	%r13, %r11
	movq	$3, (%rbx)
	movq	$20, -8(%rsi)
	movabsq	$tag74, %rbx
	movq	%rbx, (%rsi)
	movq	-64(%rbp), %rdx
	movq	%rdx, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	movq	-80(%rbp), %rbx
	movq	(%rbx), %r10
	movq	-80(%rbp), %r12
	movq	%rdx, %rax
	movq	%r12, %rdi
	jmp	*%r10
L_true843:
	movq	%r15, -72(%rbp)
	movq	%r12, -80(%rbp)
then.844:
	/* block then<FA39> (ep<FA33>,retK<FA38>,_exh<FA37>,i<FA36>,_t<FA35>,_t<FA34>) */
	movq	(%r13), %r12
	cmpq	$1, %r12
	je	S_case845
	cmpq	$3, %r12
	je	S_case847
S_case845:
case.846:
	/* block case<FA58> (ep<FA52>,retK<FA57>,_exh<FA56>,i<FA55>,_t<FA54>,_t<FA53>) */
	movq	%r13, -96(%rbp)
	movq	%rbx, -56(%rbp)
	movl	16(%r13), %r12d
letJoinK.849:
	/* block letJoinK<FA43> (ep<FA3C>,retK<FA42>,_exh<FA41>,i<FA40>,_t<FA3F>,_t<FA3E>,_t<FA3D>) */
	cmpl	%r12d, -72(%rbp)
	jge	L851
L_true84A:
	movq	-88(%rbp), %r14
then.84C:
	/* block then<FA49> (ep<FA44>,retK<FA48>,_exh<FA47>,i<FA46>,_t<FA45>) */
	movq	$3, %r15
	jmp	letJoinK.83D
S_case847:
	movq	%r13, -96(%rbp)
case.848:
	/* block case<FA61> (ep<FA5B>,retK<FA60>,_exh<FA5F>,i<FA5E>,_t<FA5D>,_t<FA5C>) */
	movq	%rbx, -56(%rbp)
	movq	-96(%rbp), %r14
	movl	8(%r14), %r12d
	jmp	letJoinK.849
	/* live= GP={%r15 %rbx %r13 %r12} spilled= GP={%r~1 %r~1}  */
retGC84D:
	movq	40(%rdi), %rbx
	movq	%rbx, -88(%rbp)
	movq	32(%rdi), %r12
	movq	%r12, -64(%rbp)
	movl	24(%rdi), %r15d
	movq	16(%rdi), %rbx
	movq	8(%rdi), %r13
	movq	(%rdi), %r12
	jmp	gcTest84E
	.text
tabFromToP.359:
	movq	%r15, -56(%rbp)
	movq	%r14, -112(%rbp)
	movq	%r13, -104(%rbp)
	movq	%r9, %rbx
	movq	%r8, %r13
	movq	%rdi, -64(%rbp)
	movq	%r10, -120(%rbp)
	movq	-64(%rbp), %r15
gcTest863:
	movq	%r11, %r14
	subq	%rsi, %r14
	jle	doGC864
check.855:
	/* block check<10C61> (ep<EFB1>,lo<EFB2>,_t<EFB3>,hi<EFB4>,_t<EFB5>,f<EFB6>,retK<EFB7>,_exh<EFB8>) */
	cmpl	%r12d, %ebx
	jg	L_true856
	movq	%r13, -96(%rbp)
	movq	-56(%rbp), %r13
	movq	-112(%rbp), %rdx
else.857:
	/* block else<EFD4> (ep<EFCC>,retK<EFD3>,_exh<EFD2>,lo<EFD1>,hi<EFD0>,f<EFCF>,_t<EFCE>,_t<EFCD>) */
	movq	%r12, %rcx
	subl	%ebx, %ecx
	cmpl	24(%r15), %ecx
	jle	L_true858
else.859:
	/* block else<F00C> (ep<F004>,retK<F00B>,_exh<F00A>,lo<F009>,hi<F008>,f<F007>,_t<F006>,_t<F005>) */
	leal	(%r12,%rbx,1), %r10d
	cmpl	$0, %r10d
	jge	L85B
	incl	%r10d
L85B:
	sarl	$1, %r10d
	movq	$10, -8(%rsi)
	movl	%r10d, (%rsi)
	movq	%rsi, -56(%rbp)
	addq	$16, %rsi
	movq	$20, -8(%rsi)
	movq	$1, (%rsi)
	movq	$1, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	$10, -8(%rsi)
	movl	$1, (%rsi)
	movq	%rsi, %rbx
	addq	$16, %rsi
	movq	$125717, -8(%rsi)
	movabsq	$letJoinK.344, %r12
	movq	%r12, (%rsi)
	movq	(%r15), %rcx
	movq	%rcx, 8(%rsi)
	movq	16(%r15), %r12
	movq	%r12, 16(%rsi)
	movl	24(%r15), %ecx
	movl	%ecx, 24(%rsi)
	movq	32(%r15), %r12
	movq	%r12, 32(%rsi)
	movl	40(%r15), %ecx
	movl	%ecx, 40(%rsi)
	movq	48(%r15), %r12
	movq	%r12, 48(%rsi)
	movq	56(%r15), %rcx
	movq	%rcx, 56(%rsi)
	movq	%rdx, 64(%rsi)
	movq	%r13, 72(%rsi)
	movq	%rsi, %rdx
	addq	$88, %rsi
	movq	$133, -8(%rsi)
	movq	%r15, (%rsi)
	movabsq	$tabFromToP.359, %rcx
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	$126741, -8(%rsi)
	movabsq	$slowClone_1.35A, %rcx
	movq	%rcx, (%rsi)
	movq	%r12, 8(%rsi)
	movq	%r13, 16(%rsi)
	movq	-120(%rbp), %rcx
	movq	%rcx, 24(%rsi)
	movq	-104(%rbp), %r12
	movq	%r12, 32(%rsi)
	movl	%r10d, 40(%rsi)
	movq	-56(%rbp), %rcx
	movq	%rcx, 48(%rsi)
	movq	%r14, 56(%rsi)
	movq	%rbx, 64(%rsi)
	movq	%rdx, 72(%rsi)
	movq	%rsi, %rcx
	addq	$88, %rsi
	movq	$-1048576, %r12
	andq	%rsi, %r12
	movq	32(%r12), %r12
	movq	8(%r12), %r12
	cmpq	$1, %r12
	je	L865
L_true85C:
	movq	%r10, -64(%rbp)
	movq	%rdx, -72(%rbp)
	movq	%rbx, -80(%rbp)
	movq	%r14, -88(%rbp)
	movq	$1, %r14
then.85E:
	/* block then<F3FC> (ep<F3EF>,_exh<F3FB>,lo<F3FA>,hi<F3F9>,f<F3F8>,_t<F3F7>,res<F3F6>,r0<F3F5>,c0<F3F4>,letJoinK<F3F3>,slowClone_1<F3F2>,con_NONE<F3F1>,_t<F3F0>) */
	movq	(%r12), %r10
	movq	$20, -8(%rsi)
	movq	(%r10), %r12
	movq	%r12, (%rsi)
	movq	%r14, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	8(%r10), %r14
	movq	$133, -8(%rsi)
	movq	%r15, (%rsi)
	movabsq	$tabFromToP.359, %rbx
	movq	%rbx, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	movq	$515865, -8(%rsi)
	movabsq	$letJoinK.389, %rbx
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%r13, 16(%rsi)
	movq	-96(%rbp), %rdx
	movq	%rdx, 24(%rsi)
	movq	-120(%rbp), %rbx
	movq	%rbx, 32(%rsi)
	movq	-104(%rbp), %r10
	movq	%r10, 40(%rsi)
	movq	-64(%rbp), %rdx
	movl	%edx, 48(%rsi)
	movq	-56(%rbp), %rbx
	movq	%rbx, 56(%rsi)
	movq	-88(%rbp), %r10
	movq	%r10, 64(%rsi)
	movq	-80(%rbp), %rdx
	movq	%rdx, 72(%rsi)
	movq	-72(%rbp), %rbx
	movq	%rbx, 80(%rsi)
	movq	%r12, 88(%rsi)
	movq	%rsi, %rbx
	addq	$104, %rsi
	cmpq	$1, %r14
	jne	L_true85F
else.860:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block else<F4D6> (slowClone_1<F4D5>,letJoinK<F4D4>) */
	movq	%rbx, %rdi
	movq	%rcx, %r8
	jmp	letJoinK.389
doGC864:
	movq	$30097, -8(%rsi)
	movq	%r15, (%rsi)
	movq	%r13, 8(%rsi)
	movl	%ebx, 16(%rsi)
	movq	-120(%rbp), %r10
	movq	%r10, 24(%rsi)
	movl	%r12d, 32(%rsi)
	movq	-104(%rbp), %r12
	movq	%r12, 40(%rsi)
	movq	-112(%rbp), %r13
	movq	%r13, 48(%rsi)
	movq	-56(%rbp), %r14
	movq	%r14, 56(%rsi)
	movq	%rsi, %r10
	addq	$72, %rsi
	movq	%r10, %rdi
	movabsq	$retGC862, %r8
	jmp	ASM_InvokeGC
L_true858:
	movq	-104(%rbp), %r10
then.85A:
	/* Liveout:  GP={%r13 %r12 %r10 %r9 %r8 %rdi}  */
	/* block then<EFDD> (ep<EFD7>,retK<EFDC>,_exh<EFDB>,f<EFDA>,_t<EFD9>,_t<EFD8>) */
	movq	$133, -8(%rsi)
	movq	%r10, (%rsi)
	movl	%ebx, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	$133, -8(%rsi)
	movq	%r14, (%rsi)
	movabsq	$f_P_.262, %r10
	movq	%r10, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	$10, -8(%rsi)
	movq	%r12, %r10
	subl	%ebx, %r10d
	movl	%r10d, (%rsi)
	movq	%rsi, %r14
	addq	$16, %rsi
	movq	$20, -8(%rsi)
	movq	%r14, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	$263, -8(%rsi)
	movabsq	$letJoinK.267, %r14
	movq	%r14, (%rsi)
	movq	%rdx, 8(%rsi)
	subl	%ebx, %r12d
	movl	%r12d, 16(%rsi)
	movq	%rsi, %r12
	addq	$32, %rsi
	movq	(%r15), %rdx
	movq	(%rdx), %rdi
	movq	(%rcx), %r8
	movq	(%rcx), %rbx
	movl	(%rbx), %r9d
	movq	8(%rcx), %r10
	jmp	tabulate.7E
L_true856:
then.853:
	/* block then<EFBE> (_exh<EFBD>) */
	movq	$133, -8(%rsi)
	movabsq	$str854, %r13
	movq	%r13, (%rsi)
	movl	$23, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	$133, -8(%rsi)
	movabsq	$str83B, %r14
	movq	%r14, (%rsi)
	movl	$1, 8(%rsi)
	movq	%rsi, -72(%rbp)
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movq	-56(%rbp), %r12
	movq	%r12, (%rsi)
	movq	%r13, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	$-1048576, %rbx
	andq	%rsi, %rbx
	movq	%r15, 96(%rbx)
	movq	%rsi, 120(%rbx)
	movq	%r11, 320(%rbx)
	movq	$-1048576, %r14
	andq	%rsi, %r14
	movq	$1, (%r14)
	movq	%rax, %r15
	movq	%rsi, -88(%rbp)
	movq	%rdi, -64(%rbp)
	movq	%r8, -80(%rbp)
	movq	%r9, %rbx
	movq	%r11, %r12
	movq	%r13, %rdi
	movq	-72(%rbp), %rsi
	call	M_StringConcat2
	movq	%rax, -56(%rbp)
	movq	%r15, %rax
	movq	-88(%rbp), %rsi
	movq	-64(%rbp), %rdi
	movq	-80(%rbp), %r8
	movq	%rbx, %r9
	movq	%r12, %r11
	movq	$3, (%r14)
	movq	$-1048576, %r12
	andq	%rsi, %r12
	movq	120(%r12), %rsi
	movq	320(%r12), %r11
	movq	96(%r12), %r13
	movq	8(%r13), %r13
	movq	%r13, -64(%rbp)
	movq	96(%r12), %r14
	movq	(%r14), %r14
	movq	%r14, -80(%rbp)
allocCCall.852:
	/* Liveout:  GP={%rax %rdi}  */
	/* block allocCCall<10E77> (res<EFC5>,_exh<10E75>,_slit<10E76>) */
	movq	$-1048576, %r13
	andq	%rsi, %r13
	movq	$1, (%r13)
	movq	%rax, -72(%rbp)
	movq	%rcx, -96(%rbp)
	movq	%rdx, -104(%rbp)
	movq	%rsi, -112(%rbp)
	movq	%rdi, %r15
	movq	%r8, %rbx
	movq	%r9, %r12
	movq	%r10, -88(%rbp)
	movq	%r11, %r14
	movq	-56(%rbp), %rcx
	movq	(%rcx), %rcx
	movq	%rcx, %rdi
	call	M_Print
	movq	-72(%rbp), %rax
	movq	-96(%rbp), %rcx
	movq	-104(%rbp), %rdx
	movq	-112(%rbp), %rsi
	movq	%r15, %rdi
	movq	%rbx, %r8
	movq	%r12, %r9
	movq	-88(%rbp), %r10
	movq	%r14, %r11
	movq	$3, (%r13)
	movq	$20, -8(%rsi)
	movabsq	$tag74, %rbx
	movq	%rbx, (%rsi)
	movq	-64(%rbp), %rdx
	movq	%rdx, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	movq	-80(%rbp), %rbx
	movq	(%rbx), %r10
	movq	-80(%rbp), %r12
	movq	%rdx, %rax
	movq	%r12, %rdi
	jmp	*%r10
	/* live= GP={%r12 %rbx %r13 %r15} spilled= GP={%r~1 %r~1 %r~1 %r~1}  */
retGC862:
	movq	56(%rdi), %r15
	movq	%r15, -56(%rbp)
	movq	48(%rdi), %rbx
	movq	%rbx, -112(%rbp)
	movq	40(%rdi), %r12
	movq	%r12, -104(%rbp)
	movl	32(%rdi), %r12d
	movq	24(%rdi), %r13
	movq	%r13, -120(%rbp)
	movl	16(%rdi), %ebx
	movq	8(%rdi), %r13
	movq	(%rdi), %r15
	jmp	gcTest863
L865:
else.85D:
	/* Liveout:  GP={%rax %rdi}  */
	/* block else<F4DA> (_exh<F4D9>) */
	movq	$133, -8(%rsi)
	movabsq	$str87, %r15
	movq	%r15, (%rsi)
	movl	$51, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movabsq	$tag74, %rdx
	movq	%rdx, (%rsi)
	movq	%r14, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	(%r13), %rbx
	movq	%rcx, %rax
	movq	%r13, %rdi
	jmp	*%rbx
L_true85F:
then.861:
	/* Liveout:  GP={%r12 %r10 %r9 %r8 %rdi}  */
	/* block then<F4C7> (ep<F4C2>,_exh<F4C6>,slowClone_1<F4C5>,c<F4C4>,letJoinK<F4C3>) */
	movq	$261, -8(%rsi)
	movabsq	$letJoinK.38F, %r10
	movq	%r10, (%rsi)
	movq	%rbx, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	8(%r15), %r12
	movq	(%r12), %rdi
	movq	(%r14), %r8
	movq	%rcx, %r9
	movq	%r13, %r12
	jmp	wrap.F3
	.text
letJoinK.7:
	movq	%r8, %r12
	movq	%rdi, %rbx
	jmp	gcTest86D
	/* live= GP={%r12 %rbx} spilled=  */
retGC86C:
	movq	8(%rdi), %r12
	movq	(%rdi), %rbx
gcTest86D:
	movq	%r11, %r13
	subq	%rsi, %r13
	jle	doGC86E
check.869:
	/* block check<10B39> (ep<EA84>,item<EA81>) */
	cmpq	$1, %r12
	jne	L_true86A
else.867:
	/* block else<EA9B> (ep<EA9A>) */
	movq	$20, -8(%rsi)
	movabsq	$wakeupK.F9, %r12
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
allocCCall.866:
	/* Liveout:  GP={%rax %rdi}  */
	/* block allocCCall<10E79> (sleepK<EAA4>) */
	movq	(%rbx), %r13
	movq	$1, %r14
	movq	%r14, %rax
	movq	%rbx, %rdi
	jmp	*%r13
doGC86E:
	movq	$20, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%r12, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	%r10, %rdi
	movabsq	$retGC86C, %r8
	jmp	ASM_InvokeGC
L_true86A:
then.86B:
	/* Liveout:  GP={%rax %rdi}  */
	/* block then<EA8A> (ep<EA88>,item<EA89>) */
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
	.long	1732720534
	.global	SequentialFlag
SequentialFlag:
	.long	0
	.align	8
flt7DB:
	.double 0.6e1
	.align	8
flt7D9:
	.double 0.753e0
	.align	8
flt7D8:
	.double 0.361e0
	.align	8
flt7D7:
	.double 0.78e-1
	.align	8
flt7D6:
	.double 0.17e1
	.align	8
flt7D5:
	.double 0.13e1
	.align	8
flt7D4:
	.double 0.21e1
	.align	8
flt7D1:
	.double -0.4e1
	.align	8
flt7D0:
	.double 0.288675e0
	.align	8
flt7CD:
	.double 0.4e0
	.align	8
flt7CC:
	.double 0.8e0
	.align	8
flt7C9:
	.double 0.45e2
	.align	8
flt7C6:
	.double 0.314159265359e1
	.align	8
flt79A:
	.double 0.180e3
	.align	8
flt799:
	.double 0.2e1
	.align	8
flt72A:
	.double -0.2e1
	.align	8
flt60A:
	.double 0.4e1
	.align	8
flt504:
	.double 0.8e1
	.align	8
flt4D3:
	.double 0.0e0
	.align	8
flt7DA:
	.double 0.7e1
	.align	8
flt7D3:
	.double 0.5e1
	.align	8
flt7D2:
	.double -0.3e1
	.align	8
flt7CF:
	.double 0.3e1
	.align	8
flt7CE:
	.double 0.7e0
	.align	8
flt7CB:
	.double 0.3e0
	.align	8
flt7CA:
	.double 0.1e0
	.align	8
flt7C8:
	.double 0.1e21
	.align	8
flt7C7:
	.double 0.1e-5
	.align	8
flt79B:
	.double 0.5e0
	.align	8
flt707:
	.double -0.1e1
	.align	8
flt536:
	.double 0.1e1
	.align	8
str87:
	.asciz	"FLS.ite: nonexistant implicit threading environment"
	.align	8
str377:
	.asciz	"ImplicitThread.@peek: empty work-group stack"
	.align	8
str831:
	.asciz	"out.ppm"
	.align	8
str640:
	.asciz	"List.hd"
	.align	8
str83A:
	.asciz	"subscript out of bounds"
	.align	8
str73:
	.asciz	"Array64.@array: negative size"
	.align	8
str854:
	.asciz	"todo: downward tabulate"
	.align	8
str83B:
	.asciz	"\n"
	.align	8
str21B:
	.asciz	"instantiating a 0-length array"
	.align	8
tag3C1:
	.asciz	"Div"
	.align	8
tag74:
	.asciz	"Fail"
	.align	8
tagE7:
	.asciz	"Match"
	.align	8
signBit6455D:
	.quad	9223372036854775808
