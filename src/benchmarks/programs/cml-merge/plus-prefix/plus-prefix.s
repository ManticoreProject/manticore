	.text
letJoinK.E:
	movq	%rdi, %r10
	movq	%r10, -88(%rbp)
	movq	%rax, %r13
	movq	%rdi, %r14
	movq	%r8, %r12
	movq	%r12, -64(%rbp)
	movq	%r9, %r15
	movq	%rsi, %rbx
	movq	%r11, %r12
	call	_M_GetTime
	movq	%rax, %rcx
	movq	%r13, %rax
	movq	%r14, %rdi
	movq	-64(%rbp), %r8
	movq	%r15, %r9
	movq	%rbx, %rsi
	movq	%r12, %r11
	movq	-88(%rbp), %rdx
	subq	16(%rdx), %rcx
	movq	$10, -8(%rsi)
	movq	%rcx, (%rsi)
	movq	%rsi, %rbx
	movq	%rbx, -72(%rbp)
	addq	$16, %rsi
	cmpq	$0, %rcx
	jge	L1C
L_true10:
then.C:
	/* block then<9773> (ep<9771>,_t<9772>) */
	movq	$133, -8(%rsi)
	movabsq	$strD, %r14
	movq	%r14, (%rsi)
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
letJoinK.8:
	/* block letJoinK<9728> (ep<9725>,sign<9726>,t<9727>) */
	movq	-72(%rbp), %rcx
	movq	(%rcx), %rax
	cdq
	movq	$1000, %r12
	idivq	%r12
	movq	%rax, %r10
	movq	%r10, %rax
	cdq
	movq	$1000, %r13
	idivq	%r13
	cmpq	$10, %rdx
	jl	L_true9
else.A:
	/* block else<9757> (ep<9753>,sign<9756>,t<9755>,_t<9754>) */
	cmpq	$100, %rdx
	jl	L_true11
else.12:
	/* block else<9769> (ep<9765>,sign<9768>,t<9767>,_t<9766>) */
	movq	$133, -8(%rsi)
	movabsq	$str14, %rcx
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
	movq	%rax, %r14
	movq	%rdi, %r13
	movq	%r13, -80(%rbp)
	movq	%r8, %r13
	movq	%r9, %r12
	movq	%r11, %rbx
	movq	-56(%rbp), %rdi
	movq	%rcx, %r10
	movq	%r10, %rsi
	call	_M_StringConcat2
	movq	%rax, %rcx
	movq	%rcx, -56(%rbp)
	movq	%r14, %rax
	movq	-80(%rbp), %rdi
	movq	%r13, %r8
	movq	%r12, %r9
	movq	%rbx, %r11
	movq	128(%r15), %rsi
letJoinK.17:
	/* Liveout:  GP={%rax %rdi}  */
	/* block letJoinK<9733> (ep<972F>,sign<9732>,t<9731>,frac<9730>) */
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
	movq	%r11, %rdx
	movq	%rdx, -72(%rbp)
	movq	-64(%rbp), %rdi
	movq	%rcx, %rsi
	call	_M_StringConcat2
	movq	%rax, %rcx
	movq	%r12, %rax
	movq	%r13, %rdi
	movq	%r14, %r8
	movq	%r15, %r9
	movq	-72(%rbp), %r11
	movq	128(%rbx), %rsi
	movq	%r11, %r15
	movq	%rsi, 128(%r15)
	movq	%rax, %r14
	movq	%rdi, %r13
	movq	%r8, %r12
	movq	%r9, %rbx
	movq	%rbx, -64(%rbp)
	movq	%r11, %rbx
	movq	%rcx, %rdx
	movq	%rdx, %rdi
	movq	-56(%rbp), %rsi
	call	_M_StringConcat2
	movq	%rax, %rdx
	movq	%r14, %rax
	movq	%r13, %rdi
	movq	%r12, %r8
	movq	-64(%rbp), %r9
	movq	%rbx, %r11
	movq	128(%r15), %rsi
	movq	$133, -8(%rsi)
	movabsq	$str18, %r12
	movq	%r12, (%rsi)
	movl	$1, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	%r11, %r14
	movq	%rsi, 128(%r14)
	movq	%rax, %r15
	movq	%rdi, %rbx
	movq	%r8, %r12
	movq	%r9, %r13
	movq	%r11, %r10
	movq	%r10, -80(%rbp)
	movq	%rdx, %r10
	movq	%r10, %rdi
	movq	%rcx, %rsi
	call	_M_StringConcat2
	movq	%rax, %rcx
	movq	%r15, %rax
	movq	%rbx, %rdi
	movq	%r12, %r8
	movq	%r13, %r9
	movq	-80(%rbp), %r11
	movq	128(%r14), %rsi
	movq	%rax, %r13
	movq	%rdi, %r12
	movq	%r8, %r14
	movq	%r14, -56(%rbp)
	movq	%r9, %rbx
	movq	%rsi, %r15
	movq	%r11, %r14
	movq	(%rcx), %rcx
	movq	%rcx, %rdi
	call	_M_Print
	movq	%r13, %rax
	movq	%r12, %rdi
	movq	-56(%rbp), %r8
	movq	%rbx, %r9
	movq	%r15, %rsi
	movq	%r14, %r11
	movq	-88(%rbp), %r15
	movq	8(%r15), %rbx
	movq	(%rbx), %r10
	movq	$1, %r12
	movq	%r12, %rax
	movq	%rbx, %rdi
	jmp	*%r10
L1C:
else.6:
	/* block else<977B> (ep<9779>,res<977A>) */
	movq	$133, -8(%rsi)
	movabsq	$str7, %rbx
	movq	%rbx, (%rsi)
	movl	$0, 8(%rsi)
	movq	%rsi, %r15
	movq	%r15, -64(%rbp)
	addq	$24, %rsi
	jmp	letJoinK.8
L_true11:
then.13:
	/* block then<975D> (ep<9759>,sign<975C>,t<975B>,_t<975A>) */
	movq	$133, -8(%rsi)
	movabsq	$str1A, %r13
	movq	%r13, (%rsi)
	movl	$2, 8(%rsi)
	movq	%rsi, %rcx
	movq	%rcx, -56(%rbp)
	addq	$24, %rsi
	movq	%r11, %rbx
	movq	%rsi, 128(%rbx)
	movq	%rax, %r12
	movq	%rdi, %r13
	movq	%r8, %r14
	movq	%r9, %r15
	movq	%r11, %r10
	movq	%r10, -80(%rbp)
	movq	%rdx, %rdi
	call	_M_LongToString
	movq	%rax, %rcx
	movq	%r12, %rax
	movq	%r13, %rdi
	movq	%r14, %r8
	movq	%r15, %r9
	movq	-80(%rbp), %r11
	movq	128(%rbx), %rsi
	movq	%r11, %r15
	movq	%rsi, 128(%r15)
	movq	%rax, %r14
	movq	%rdi, %r13
	movq	%r8, %r12
	movq	%r12, -80(%rbp)
	movq	%r9, %r12
	movq	%r11, %rbx
	movq	-56(%rbp), %rdi
	movq	%rcx, %rsi
	call	_M_StringConcat2
	movq	%rax, %rcx
	movq	%rcx, -56(%rbp)
	movq	%r14, %rax
	movq	%r13, %rdi
	movq	-80(%rbp), %r8
	movq	%r12, %r9
	movq	%rbx, %r11
	movq	128(%r15), %rsi
	jmp	letJoinK.17
L_true9:
then.B:
	/* block then<974B> (ep<9747>,sign<974A>,t<9749>,_t<9748>) */
	movq	$133, -8(%rsi)
	movabsq	$str1B, %r14
	movq	%r14, (%rsi)
	movl	$3, 8(%rsi)
	movq	%rsi, %rbx
	movq	%rbx, -56(%rbp)
	addq	$24, %rsi
	movq	%r11, %rbx
	movq	%rsi, 128(%rbx)
	movq	%rax, %r12
	movq	%rdi, %r13
	movq	%r8, %r14
	movq	%r9, %r15
	movq	%r11, %r10
	movq	%r10, -80(%rbp)
	movq	%rdx, %rdi
	call	_M_LongToString
	movq	%rax, %rcx
	movq	%r12, %rax
	movq	%r13, %rdi
	movq	%r14, %r8
	movq	%r15, %r9
	movq	-80(%rbp), %r11
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
	movq	%rcx, %rdx
	movq	%rdx, %rsi
	call	_M_StringConcat2
	movq	%rax, %r10
	movq	%r10, -56(%rbp)
	movq	%r14, %rax
	movq	%r13, %rdi
	movq	%r12, %r8
	movq	-80(%rbp), %r9
	movq	%rbx, %r11
	movq	128(%r15), %rsi
	jmp	letJoinK.17
	.text
initial_D_dict.1E:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest20
	/* live= GP={%rcx %rdx} spilled=  */
retGC1F:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest20:
	movq	%r11, %rbx
	movq	448(%rbx), %rbx
	subq	%rsi, %rbx
	jle	doGC21
check.1D:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block check<9807> (ep<96B6>,retK<96B7>) */
	movq	$10, -8(%rsi)
	movl	$0, (%rsi)
	movq	%rsi, %r10
	addq	$16, %rsi
	movq	$20, -8(%rsi)
	movq	%r10, (%rsi)
	movq	$1, 8(%rsi)
	movq	%rsi, %r12
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movq	%r12, (%rsi)
	movq	$1, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	(%rcx), %r14
	movq	%rcx, %rdi
	xorl	%r8d, %r8d
	incl	%r8d
	movq	%r13, %r9
	jmp	*%r14
doGC21:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	%r15, %rdi
	movabsq	$retGC1F, %r8
	jmp	_ASM_InvokeGC
	.text
lp.26:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest28
	/* live= GP={%r10 %rcx %rdx %rbx} spilled=  */
retGC27:
	movq	24(%rdi), %r10
	movq	16(%rdi), %rcx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest28:
	movq	%r11, %r12
	movq	448(%r12), %r12
	subq	%rsi, %r12
	jle	doGC29
check.22:
	/* block check<980C> (ep<96FD>,ls<96FE>,acc<96FF>,retK<9700>) */
	cmpq	$1, %rdx
	je	L2A
L_true23:
then.25:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block then<9709> (ep<9705>,retK<9708>,ls<9707>,acc<9706>) */
	movq	$20, -8(%rsi)
	movq	(%rdx), %r14
	movq	%r14, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	%rbx, %rdi
	movq	8(%rdx), %r8
	movq	%r13, %r9
	jmp	lp.26
doGC29:
	movq	$36, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %r15
	addq	$40, %rsi
	movq	%r15, %rdi
	movabsq	$retGC27, %r8
	jmp	_ASM_InvokeGC
L2A:
else.24:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block else<9712> (retK<9711>,acc<9710>) */
	movq	%r10, %rdi
	movq	%rcx, %r8
	jmp	letJoinK.E
	.text
lp.2F:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest31
	/* live= GP={%r12 %r10 %rcx %rdx %rbx} spilled=  */
retGC30:
	movq	32(%rdi), %r12
	movq	24(%rdi), %r10
	movl	16(%rdi), %ecx
	movq	8(%rdi), %rdx
	movq	(%rdi), %rbx
gcTest31:
	movq	%r11, %r13
	movq	448(%r13), %r13
	subq	%rsi, %r13
	jle	doGC32
check.2B:
	/* block check<9812> (ep<96E0>,xs<96E1>,_t<96E2>,res<96E3>,retK<96E4>) */
	cmpq	$1, %rdx
	je	L33
L_true2C:
then.2E:
	/* Liveout:  GP={%r12 %r10 %r9 %r8 %rdi}  */
	/* block then<96EE> (ep<96E9>,retK<96ED>,xs<96EC>,res<96EB>,_t<96EA>) */
	movq	(%rdx), %r14
	movl	(%r14), %r13d
	leal	(%rcx,%r13,1), %ecx
	movq	$10, -8(%rsi)
	movl	%ecx, (%rsi)
	movq	%rsi, %r15
	addq	$16, %rsi
	movq	$20, -8(%rsi)
	movq	%r15, (%rsi)
	movq	%r10, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	%rbx, %rdi
	movq	8(%rdx), %r8
	movq	%rcx, %r9
	jmp	lp.2F
doGC32:
	movq	$3339, -8(%rsi)
	movq	%rbx, (%rsi)
	movq	%rdx, 8(%rsi)
	movl	%ecx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%r12, 32(%rsi)
	movq	%rsi, %rcx
	addq	$48, %rsi
	movq	%rcx, %rdi
	movabsq	$retGC30, %r8
	jmp	_ASM_InvokeGC
L33:
else.2D:
	/* Liveout:  GP={%r10 %r9 %r8 %rdi}  */
	/* block else<96FA> (retK<96F9>,res<96F8>) */
	movq	$18, -8(%rsi)
	movq	$1, (%rsi)
	movabsq	$lp.26, %r15
	movq	%r15, 8(%rsi)
	movq	%rsi, %r14
	addq	$24, %rsi
	movq	(%r14), %rdi
	movq	%r10, %r8
	movq	$1, %r9
	movq	%r12, %r10
	jmp	lp.26
	.text
letJoinK.35:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest37
	/* live= GP={%rcx %rdx} spilled=  */
retGC36:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest37:
	movq	%r11, %rbx
	movq	448(%rbx), %rbx
	subq	%rsi, %rbx
	jle	doGC38
check.34:
	/* Liveout:  GP={%r12 %r10 %r9 %r8 %rdi}  */
	/* block check<9815> (ep<96DB>,a<96D9>) */
	movq	$18, -8(%rsi)
	movq	$1, (%rsi)
	movabsq	$lp.2F, %r12
	movq	%r12, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	$263, -8(%rsi)
	movabsq	$letJoinK.E, %r13
	movq	%r13, (%rsi)
	movq	8(%rdx), %r14
	movq	%r14, 8(%rsi)
	movq	16(%rdx), %r15
	movq	%r15, 16(%rsi)
	movq	%rsi, %r12
	addq	$32, %rsi
	movq	(%r10), %rdi
	movq	%rcx, %r8
	xorl	%r9d, %r9d
	movq	$1, %r10
	jmp	lp.2F
doGC38:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	%rcx, %rdi
	movabsq	$retGC36, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.3A:
	movq	%r8, %rcx
	movq	%rdi, %rdx
	jmp	gcTest3C
	/* live= GP={%rcx %rdx} spilled=  */
retGC3B:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest3C:
	movq	%r11, %rbx
	movq	448(%rbx), %rbx
	subq	%rsi, %rbx
	jle	doGC3D
check.39:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<9818> (ep<97A9>,_t<97A8>) */
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
doGC3D:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r15
	addq	$24, %rsi
	movq	%r15, %rdi
	movabsq	$retGC3B, %r8
	jmp	_ASM_InvokeGC
	.text
loop.43:
	movq	%r9, %rdx
	movq	%r8, %rcx
	movq	%rdi, %r15
	jmp	gcTest45
	/* live= GP={%rdx %rcx %r15} spilled=  */
retGC44:
	movq	16(%rdi), %rdx
	movl	8(%rdi), %ecx
	movq	(%rdi), %r15
gcTest45:
	movq	%r11, %r14
	movq	448(%r14), %rbx
	subq	%rsi, %rbx
	jle	doGC46
check.3E:
	/* block check<981C> (ep<9790>,a0<9791>,retK<9792>) */
	movq	(%r15), %rbx
	cmpl	(%rbx), %ecx
	jne	L47
L_true3F:
then.41:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block then<979A> (retK<9799>) */
	movq	(%rdx), %r12
	movq	%rdx, %rdi
	movq	$1, %r8
	jmp	*%r12
doGC46:
	movq	$647, -8(%rsi)
	movq	%r15, (%rsi)
	movl	%ecx, 8(%rsi)
	movq	%rdx, 16(%rsi)
	movq	%rsi, %r13
	addq	$32, %rsi
	movq	%r13, %rdi
	movabsq	$retGC44, %r8
	jmp	_ASM_InvokeGC
L47:
	movq	%rcx, -56(%rbp)
	movq	%rdx, -72(%rbp)
	movq	%r15, -64(%rbp)
else.40:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block else<97A0> (ep<979D>,retK<979F>,a0<979E>) */
	movq	%rax, %r12
	movq	%rdi, %r13
	movq	%r8, %r14
	movq	%r9, %r15
	movq	%rsi, %r10
	movq	%r10, -80(%rbp)
	movq	%r11, %rbx
	xorl	%r10d, %r10d
	movslq	%r10d, %r10
	movq	%r10, %rdi
	movl	$100, %ecx
	movslq	%ecx, %rcx
	movq	%rcx, %rsi
	call	_M_RandomInt
	movq	%rax, %rdx
	movq	%r12, %rax
	movq	%r13, %rdi
	movq	%r14, %r8
	movq	%r15, %r9
	movq	-80(%rbp), %rsi
	movq	%rbx, %r11
	movq	$10, -8(%rsi)
	movl	%edx, (%rsi)
	movq	%rsi, %rdx
	addq	$16, %rsi
	movq	$775, -8(%rsi)
	movabsq	$letJoinK.3A, %r10
	movq	%r10, (%rsi)
	movq	-72(%rbp), %r12
	movq	%r12, 8(%rsi)
	movq	%rdx, 16(%rsi)
	movq	%rsi, %rbx
	addq	$32, %rsi
	movq	-64(%rbp), %rdi
	movq	-56(%rbp), %r8
	incl	%r8d
	movq	%rbx, %r9
	jmp	loop.43
	.text
letJoinK.56:
	movq	%rdi, %r15
gcTest58:
	movq	%r11, %rbx
	movq	448(%rbx), %r10
	subq	%rsi, %r10
	jle	doGC59
	movq	%r15, -56(%rbp)
check.48:
	/* block check<981E> (ep<96C6>) */
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
	movq	$1, %rdx
	movq	%rdx, %rsi
	call	_AllocVector
	movq	-64(%rbp), %rax
	movq	%r12, %rdi
	movq	%r13, %r8
	movq	%r14, %r9
	movq	%r15, %r11
	movq	128(%rbx), %rsi
	movq	$10, -8(%rsi)
	movl	$200000, (%rsi)
	movq	%rsi, %r13
	movq	%r13, -64(%rbp)
	addq	$16, %rsi
	movq	%r11, %r12
	movq	%rsi, 128(%r12)
	movq	%rax, %r13
	movq	%rdi, %r14
	movq	%r8, %r15
	movq	%r9, %rbx
	movq	%r11, %rcx
	movq	%rcx, -72(%rbp)
	call	_M_Arguments
	movq	%rax, %r10
	movq	%r13, %rax
	movq	%r14, %rdi
	movq	%r15, %r8
	movq	%rbx, %r9
	movq	-72(%rbp), %r11
	movq	128(%r12), %rsi
	cmpq	$1, %r10
	jne	L_true4B
	jmp	letJoinK.4C
doGC59:
	movq	$12, -8(%rsi)
	movq	%r15, (%rsi)
	movq	%rsi, %rdx
	addq	$16, %rsi
	movq	%rdx, %rdi
	movabsq	$retGC57, %r8
	jmp	_ASM_InvokeGC
	/* live= spilled= GP={%r~1}  */
retGC57:
	movq	(%rdi), %r15
	jmp	gcTest58
L_true4F:
then.50:
	/* block then<97BF> (ep<97BD>,res<97BE>) */
	movq	(%rcx), %rbx
	movq	%rbx, -64(%rbp)
letJoinK.4C:
	/* block letJoinK<96D4> (ep<96D2>,n<96D3>) */
	movq	%rax, %r10
	movq	%r10, -72(%rbp)
	movq	%rdi, %r13
	movq	%r8, %r14
	movq	%r9, %r15
	movq	%rsi, %rcx
	movq	%rcx, -80(%rbp)
	movq	%r11, %r12
	call	_M_GetTime
	movq	%rax, %rdx
	movq	-72(%rbp), %rax
	movq	%r13, %rdi
	movq	%r14, %r8
	movq	%r15, %r9
	movq	-80(%rbp), %rsi
	movq	%r12, %r11
	movq	$263, -8(%rsi)
	movabsq	$letJoinK.35, %r14
	movq	%r14, (%rsi)
	movq	-56(%rbp), %rbx
	movq	8(%rbx), %r15
	movq	%r15, 8(%rsi)
	movq	%rdx, 16(%rsi)
	movq	%rsi, %r13
	addq	$32, %rsi
	movq	-64(%rbp), %r10
	cmpl	$0, (%r10)
	jge	L5A
L_true51:
then.53:
	/* Liveout:  GP={%rax %rdi}  */
	/* block then<9782> (ep<9781>) */
	movq	$133, -8(%rsi)
	movabsq	$str54, %r12
	movq	%r12, (%rsi)
	movl	$4, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	$20, -8(%rsi)
	movabsq	$tag55, %r14
	movq	%r14, (%rsi)
	movq	%r10, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	-56(%rbp), %r12
	movq	16(%r12), %r15
	movq	(%r15), %rcx
	movq	%r13, %rax
	movq	%r15, %rdi
	jmp	*%rcx
L5A:
	movq	-64(%rbp), %r14
else.52:
	/* Liveout:  GP={%r9 %r8 %rdi}  */
	/* block else<978D> (n<978C>,letJoinK<978B>) */
	movq	$12, -8(%rsi)
	movq	%r14, (%rsi)
	movq	%rsi, %rcx
	addq	$16, %rsi
	movq	$133, -8(%rsi)
	movq	%rcx, (%rsi)
	movabsq	$loop.43, %rbx
	movq	%rbx, 8(%rsi)
	movq	%rsi, %rdx
	addq	$24, %rsi
	movq	(%rdx), %rdi
	xorl	%r8d, %r8d
	movq	%r13, %r9
	jmp	loop.43
L_true4B:
then.4D:
	/* block then<97B7> (ep<97B4>,_wlit<97B6>,args<97B5>) */
	movq	%r11, %rbx
	movq	%rsi, 128(%rbx)
	movq	%rax, %r15
	movq	%rdi, %rdx
	movq	%rdx, -72(%rbp)
	movq	%r8, %r14
	movq	%r9, %r13
	movq	%r11, %r12
	movq	(%r10), %rcx
	movq	%rcx, %rdi
	call	_M_IntFromString
	movq	%rax, %rcx
	movq	%r15, %rax
	movq	-72(%rbp), %rdi
	movq	%r14, %r8
	movq	%r13, %r9
	movq	%r12, %r11
	movq	128(%rbx), %rsi
	cmpq	$1, %rcx
	jne	L_true4F
	jmp	letJoinK.4C
	.text
schedCont.5C:
	movq	%rax, %rcx
	movq	%rdi, %rdx
	jmp	gcTest5E
	/* live= GP={%rcx %rdx} spilled=  */
retGC5D:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest5E:
	movq	%r11, %r14
	movq	448(%r14), %rbx
	subq	%rsi, %rbx
	jle	doGC5F
check.5B:
	/* Liveout:  GP={%rax %rdi}  */
	/* block check<9821> (ep<97CA>,k<97C9>) */
	movq	(%rcx), %r10
	movq	$1, %r12
	movq	%r12, %rax
	movq	%rcx, %rdi
	jmp	*%r10
doGC5F:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %r13
	addq	$24, %rsi
	movq	%r13, %rdi
	movabsq	$retGC5D, %r8
	jmp	_ASM_InvokeGC
	.text
shutdownCont.62:
	movq	%rax, %rcx
	movq	%rdi, %r15
	jmp	gcTest64
	/* live= GP={%rcx %r15} spilled=  */
retGC63:
	movq	8(%rdi), %rcx
	movq	(%rdi), %r15
gcTest64:
	movq	%r11, %r10
	movq	448(%r10), %r12
	subq	%rsi, %r12
	jle	doGC65
check.60:
	/* Liveout:  GP={%rdi}  */
	/* block check<9824> (ep<97DB>,_wild<97D8>) */
	movq	%rax, %r13
	movq	%r13, -56(%rbp)
	movq	%rdi, %rbx
	movq	%r8, %r12
	movq	%r9, %r13
	movq	%rsi, %r14
	movq	%r11, %rcx
	movq	%rcx, -64(%rbp)
	movq	8(%r15), %rcx
	movq	%rcx, %rdi
	call	_VProcExit
	movq	-56(%rbp), %rax
	movq	%rbx, %rdi
	movq	%r12, %r8
	movq	%r13, %r9
	movq	%r14, %rsi
	movq	-64(%rbp), %r11
	movq	16(%r15), %rdi
	jmp	letJoinK.56
doGC65:
	movq	$20, -8(%rsi)
	movq	%r15, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rbx
	addq	$24, %rsi
	movq	%rbx, %rdi
	movabsq	$retGC63, %r8
	jmp	_ASM_InvokeGC
	.text
dummyK.67:
	movq	%rax, %rcx
	movq	%rdi, %rdx
	jmp	gcTest69
	/* live= GP={%rcx %rdx} spilled=  */
retGC68:
	movq	8(%rdi), %rcx
	movq	(%rdi), %rdx
gcTest69:
	movq	%r11, %rbx
	movq	448(%rbx), %rbx
	subq	%rsi, %rbx
	jle	doGC6A
check.66:
	/* Liveout:  GP={%rax %rdi}  */
	/* block check<9827> (ep<97F1>,x<97F0>) */
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
doGC6A:
	movq	$20, -8(%rsi)
	movq	%rdx, (%rsi)
	movq	%rcx, 8(%rsi)
	movq	%rsi, %rcx
	addq	$24, %rsi
	movq	%rcx, %rdi
	movabsq	$retGC68, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.6C:
	movq	%r9, %r10
	movq	%r8, %rbx
	movq	%rdi, %rdx
	jmp	gcTest6E
	/* live= GP={%r10 %rbx} spilled= GP={%r~1}  */
retGC6D:
	movq	16(%rdi), %r10
	movl	8(%rdi), %ebx
	movq	(%rdi), %rdx
gcTest6E:
	movq	%r11, %r14
	movq	448(%r14), %r15
	subq	%rsi, %r15
	jle	doGC6F
	movq	%rdx, -56(%rbp)
check.6B:
	/* Liveout:  GP={%rdi}  */
	/* block check<982B> (ep<97E8>,_t<97E4>,_t<97E5>) */
	movq	$1289, -8(%rsi)
	movl	$-1, (%rsi)
	movq	$1, 8(%rsi)
	movl	%ebx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %rcx
	addq	$40, %rsi
	movq	%r11, %r15
	movq	%rsi, 128(%r15)
	movq	%rax, %rdx
	movq	%rdx, -64(%rbp)
	movq	%rdi, %r14
	movq	%r8, %r13
	movq	%r9, %r12
	movq	%r11, %rbx
	movq	%r11, %rdx
	movq	%rdx, %rdi
	movq	%rcx, %rsi
	call	_PromoteObj
	movq	%rax, %r10
	movq	-64(%rbp), %rax
	movq	%r14, %rdi
	movq	%r13, %r8
	movq	%r12, %r9
	movq	%rbx, %r11
	movq	128(%r15), %rsi
	movq	-56(%rbp), %rbx
	movq	8(%rbx), %r12
	movq	%r10, 24(%r12)
	movq	$10, -8(%rsi)
	movabsq	$dummyK.67, %r13
	movq	%r13, (%rsi)
	movq	%rsi, %rcx
	addq	$16, %rsi
	movq	%r11, %r15
	movq	%rsi, 128(%r15)
	movq	%rax, %r14
	movq	%rdi, %r13
	movq	%r8, %r12
	movq	%r9, %rbx
	movq	%r11, %r10
	movq	%r10, -64(%rbp)
	movq	%r11, %rdx
	movq	%rdx, %rdi
	movq	%rcx, %rsi
	call	_PromoteObj
	movq	%rax, %r10
	movq	%r14, %rax
	movq	%r13, %rdi
	movq	%r12, %r8
	movq	%rbx, %r9
	movq	-64(%rbp), %r11
	movq	128(%r15), %rsi
	movq	-56(%rbp), %r12
	movq	8(%r12), %r12
	movq	%r10, 48(%r12)
	movq	-56(%rbp), %r13
	movq	16(%r13), %rdi
	jmp	letJoinK.56
doGC6F:
	movq	$647, -8(%rsi)
	movq	%rdx, (%rsi)
	movl	%ebx, 8(%rsi)
	movq	%r10, 16(%rsi)
	movq	%rsi, %r13
	addq	$32, %rsi
	movq	%r13, %rdi
	movabsq	$retGC6D, %r8
	jmp	_ASM_InvokeGC
	.text
letJoinK.71:
	movq	%r9, %r10
	movq	%r8, %rdx
	movq	%rdi, %rbx
	jmp	gcTest73
	/* live= GP={%r10 %rdx %rbx} spilled=  */
retGC72:
	movq	16(%rdi), %r10
	movl	8(%rdi), %edx
	movq	(%rdi), %rbx
gcTest73:
	movq	%r11, %r12
	movq	448(%r12), %r13
	subq	%rsi, %r13
	jle	doGC74
check.70:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<982F> (ep<97D1>,_t<97CF>,_t<97D0>) */
	movq	$1289, -8(%rsi)
	movl	$-1, (%rsi)
	movq	$1, 8(%rsi)
	movl	%edx, 16(%rsi)
	movq	%r10, 24(%rsi)
	movq	%rsi, %r12
	addq	$40, %rsi
	movq	16(%rbx), %r13
	movq	%r12, 24(%r13)
	movq	$519, -8(%rsi)
	movabsq	$shutdownCont.62, %r14
	movq	%r14, (%rsi)
	movq	16(%rbx), %r15
	movq	%r15, 8(%rsi)
	movq	24(%rbx), %rcx
	movq	%rcx, 16(%rsi)
	movq	%rsi, %rcx
	addq	$32, %rsi
	movq	%r11, %r15
	movq	%rsi, 128(%r15)
	movq	%rax, %r14
	movq	%r14, -56(%rbp)
	movq	%rdi, %r14
	movq	%r8, %r13
	movq	%r9, %r12
	movq	%r11, %rdx
	movq	%rdx, -64(%rbp)
	movq	%r11, %rdx
	movq	%rdx, %rdi
	movq	%rcx, %rsi
	call	_PromoteObj
	movq	%rax, %r10
	movq	-56(%rbp), %rax
	movq	%r14, %rdi
	movq	%r13, %r8
	movq	%r12, %r9
	movq	-64(%rbp), %r11
	movq	128(%r15), %rsi
	movq	16(%rbx), %r12
	movq	%r10, 64(%r12)
	movq	$519, -8(%rsi)
	movabsq	$letJoinK.6C, %r14
	movq	%r14, (%rsi)
	movq	16(%rbx), %r15
	movq	%r15, 8(%rsi)
	movq	24(%rbx), %rcx
	movq	%rcx, 16(%rsi)
	movq	%rsi, %r13
	addq	$32, %rsi
	movq	8(%rbx), %rdx
	movq	(%rdx), %rdi
	movq	%r13, %r8
	jmp	initial_D_dict.1E
doGC74:
	movq	$647, -8(%rsi)
	movq	%rbx, (%rsi)
	movl	%edx, 8(%rsi)
	movq	%r10, 16(%rsi)
	movq	%rsi, %rbx
	addq	$32, %rsi
	movq	%rbx, %rdi
	movabsq	$retGC72, %r8
	jmp	_ASM_InvokeGC
	.text
main.76:
_Main_init:
_mantEntry:
	movq	%r9, %rcx
	movq	%r8, %rdx
	movq	%rax, %rbx
	movq	%rdi, %r10
	jmp	gcTest7A
	/* live= GP={%rcx %rdx %rbx %r10} spilled=  */
retGC79:
	movq	24(%rdi), %rcx
	movq	16(%rdi), %rdx
	movq	8(%rdi), %rbx
	movq	(%rdi), %r10
gcTest7A:
	movq	%r11, %r14
	movq	448(%r14), %r12
	subq	%rsi, %r12
	jg	L7C
doGC7B:
	movq	$36, -8(%rsi)
	movq	%r10, (%rsi)
	movq	%rbx, 8(%rsi)
	movq	%rdx, 16(%rsi)
	movq	%rcx, 24(%rsi)
	movq	%rsi, %r13
	addq	$40, %rsi
	movq	%r13, %rdi
	movabsq	$retGC79, %r8
	jmp	_ASM_InvokeGC
L7C:
check.75:
	/* Liveout:  GP={%r8 %rdi}  */
	/* block check<9834> (dummyEP<96B0>,argFormalWrap<9804>,retK<96B2>,_exh<96B3>) */
	movq	$18, -8(%rsi)
	movq	$1, (%rsi)
	movabsq	$initial_D_dict.1E, %r13
	movq	%r13, 8(%rsi)
	movq	%rsi, %r10
	addq	$24, %rsi
	movq	$775, -8(%rsi)
	movabsq	$letJoinK.56, %r15
	movq	%r15, (%rsi)
	movq	%rdx, 8(%rsi)
	movq	%rcx, 16(%rsi)
	movq	%rsi, %r14
	addq	$32, %rsi
	movq	$10, -8(%rsi)
	movabsq	$schedCont.5C, %rdx
	movq	%rdx, (%rsi)
	movq	%rsi, %rcx
	addq	$16, %rsi
	movq	%rcx, 40(%r11)
	movq	$1289, -8(%rsi)
	movabsq	$letJoinK.71, %r12
	movq	%r12, (%rsi)
	movq	%r10, 8(%rsi)
	movq	%r11, 16(%rsi)
	movq	%r14, 24(%rsi)
	movq	%rsi, %rbx
	addq	$40, %rsi
	movq	(%r10), %rdi
	movq	%rbx, %r8
	jmp	initial_D_dict.1E
	.globl	_mantEntry
	.globl	_Main_init
	.text
	.const_data
	.globl	_mantMagic
_mantMagic:
	.long	38464718
	.globl	_SequentialFlag
_SequentialFlag:
	.long	1
	.align	8
str7:
	.asciz	""
	.align	8
str18:
	.asciz	"\n"
	.align	8
str54:
	.asciz	"size"
	.align	8
strD:
	.asciz	"-"
	.align	8
str1B:
	.asciz	".00"
	.align	8
str1A:
	.asciz	".0"
	.align	8
str14:
	.asciz	"."
	.align	8
tag55:
	.asciz	"Fail"
