fun fib (i : long) = 
  if (i=0) then 0
  else if (i=1) then 1
  else fib(i-1) + fib(i-2);

(*(case i
       of 0 => 0
	| 1 => 1
	| n => fib(i-1) + fib(i-2)
      (* end case *));
*)

let pval x = fib (20)
in
  if false
  then print ("done\n")
  else print (ltos(x))
(*   if (readint() = 0)
      then print(ltos(x))
      else (
	  print("should cancel previous fib\n");
	  print(ltos(fib(10))^": previous fib should be cancelled\n"))*)
end
