fun addInto ps (i:int,n:int) = let
  fun ai ps = (case ps
    of nil => [(i,n)]
     | ((j,m)::t) => 
         if i=j then (i,n+m)::t
         else (j,m)::ai(t)
    (* end case *))
  in
    ai ps
  end

val pairs = [(0,10), (1,11), (2,12)]
val pairs' = addInto pairs (2,10)

fun show ps = let
  val itos = Int.toString
  fun p (m,n) = "(" ^ itos m ^ "," ^ itos n ^ ")"
  val ss = String.concat (List.map p ps)
  in
    Print.printLn ss
  end

val _ = show (addInto pairs (0,100))
val _ = show (addInto pairs (1,100))
val _ = show (addInto pairs (2,100))
val _ = show (addInto pairs (3,100))

val _ = Print.printLn "done"
