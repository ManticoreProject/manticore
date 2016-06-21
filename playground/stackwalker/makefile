program: 
	llc factorial.ll
	clang -c factorial.s -o factorial.o
	gcc -c stackwalker.c -o stackwalker.o
	gcc -static factorial.o stackwalker.o

clean:
	rm factorial.o factorial.s stackwalker.o a.out
