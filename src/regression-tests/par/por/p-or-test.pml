fun fib (i : long) = (case i
       of 0 => 0
	| 1 => 1
	| n => fib(i-1) + fib(i-2)
      (* end case *));

fun loop () = if false
    then NONE
    else (print "loop\n"; fib(20); loop());

fun f1 () =
    SOME (fib(1))
;

fun doit () = let
    fun f () = por(loop, f1)
    in
      por(loop, f);
      readint()
    end
;

doit()
