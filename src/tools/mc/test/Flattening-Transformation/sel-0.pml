val sz = 16

val field = let
  val rng = [| 0 to sz-1 |]
  in
    [| [| (i,j) | i in rng |] | j in rng |]
  end
  
fun pr (arr : (int * int) parray parray) = let
  val n = PArray.length arr
  fun lp i = 
    if (i >= n) then ()
    else (Print.printLn (PArray.tos_intPair (arr!i));
	  lp (i+1))
  in
    lp 0
  end

val _ = pr field	   

val _ = Print.printLn "done."
