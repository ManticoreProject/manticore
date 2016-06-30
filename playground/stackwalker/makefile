program: 
	llc factorial.ll
	clang -c factorial.s -o factorial.o
	clang -c stackwalker.c -o stackwalker.o
	clang -c stackhelper.s -o stackhelper.o
clean:
	rm factorial.o stackwalker.o stackhelper.o a.out testing
