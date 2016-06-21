program: factorial.o stackwalker.o
	clang -S factorial.ll
	clang -c factorial.ll -o factorial.o
	gcc -c stackwalker.c -o stackwalker.o
	gcc -static factorial.o stackwalker.o
