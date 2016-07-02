program: 
	llc factorial.ll
	clang -c factorial.s -o factorial.o
	objcopy --globalize-symbol=__LLVM_StackMaps factorial.o factorial.o
	clang -c stackwalker.c -o stackwalker.o
	clang -c stackhelper.s -o stackhelper.o
	ld -z relro --hash-style=gnu --build-id --eh-frame-hdr -m elf_x86_64 -dynamic-linker /lib64/ld-linux-x86-64.so.2 -o a.out /usr/lib/gcc/x86_64-linux-gnu/4.9/../../../x86_64-linux-gnu/crt1.o /usr/lib/gcc/x86_64-linux-gnu/4.9/../../../x86_64-linux-gnu/crti.o /usr/lib/gcc/x86_64-linux-gnu/4.9/crtbegin.o -L/usr/lib/gcc/x86_64-linux-gnu/4.9 -L/usr/lib/gcc/x86_64-linux-gnu/4.9/../../../x86_64-linux-gnu -L/lib/x86_64-linux-gnu -L/lib/../lib64 -L/usr/lib/x86_64-linux-gnu -L/usr/lib/gcc/x86_64-linux-gnu/4.9/../../.. -L/home/waleed/clang+llvm-3.8.0-x86_64-linux-gnu-ubuntu-14.04/bin/../lib -L/lib -L/usr/lib factorial.o stackhelper.o stackwalker.o -lgcc --as-needed -lgcc_s --no-as-needed -lc -lgcc --as-needed -lgcc_s --no-as-needed /usr/lib/gcc/x86_64-linux-gnu/4.9/crtend.o /usr/lib/gcc/x86_64-linux-gnu/4.9/../../../x86_64-linux-gnu/crtn.o
clean:
	rm factorial.o stackwalker.o stackhelper.o a.out
