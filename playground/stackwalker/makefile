program: 
	llc factorial.ll
	sed -i 's/globl	main/globl	main, __LLVM_StackMaps/' factorial.s
	clang -c factorial.s -o factorial.o
	clang -c stackwalker.c -o stackwalker.o
	clang -c stackhelper.s -o stackhelper.o
	gcc -static factorial.o stackhelper.o stackwalker.o
clean:
	rm factorial.o stackwalker.o stackhelper.o a.out
