(* Test cancel for correct runtime behavior.
 *   (run me by saying either echo 0|./a.out or echo 1|./a.out.)
 *)
fun fib (i : long) = 
  if (i=0) then 0
  else if (i=1) then 1
  else fib(i-1) + fib(i-2);

let pval x = (print "in pval\n"; fib (40))
in
  if (readint() = 0)
     then print ("should terminate quickly with fib(30)="^ltos(fib(30))^"\n")
     else print ("should terminate slowly with fib(40)="^ltos(x)^"\n")
end
