fun fib (i : long) = (case i
       of 0 => 0
	| 1 => 1
	| n => fib(i-1) + fib(i-2)
      (* end case *));

let pval x = fib (20)
in
   if (readint() = 0)
      then print(ltos(x))
      else (
	  print("should cancel previous fib\n");
	  print(ltos(fib(20))^": previous fib should be cancelled\n"))
end
