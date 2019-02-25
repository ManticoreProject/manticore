


fun tailcount (n : long) k =
  if n = 0
    then Cont.throw (k, 0)
  else tailcount (n-1) k

(* try to trigger overflow if tailcall is not optimized *)
val n = Cont.callec (tailcount 1000000000)
val _ = (print (Long.toString n); print "\n")
