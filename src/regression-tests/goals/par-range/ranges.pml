(* cheating...these names are supposed to be bound already... *)

fun sumP_int a = let
  fun add (x, y) = x + y
  in
    PArray.reduce (add, 0, a)
  end

val sumP = sumP_int

val sub = PArray.sub

val lenP = PArray.length

(* real stuff *)

val itos = Int.toString

fun vtos v =
   let val n = lenP v
       fun build (m, acc) =
         if (m >= n) then
           acc
         else if (m = (n - 1)) then 
           build (m+1, acc ^ (itos (sub (v, m))))
	 else
	   build (m+1, acc ^ (itos (sub (v, m))) ^ ",")
   in
     "[|" ^ (build (0, "")) ^ "|]"
   end

val r1 = [| 0 to 100 |]
val _ = Print.printLn (vtos r1)

val s = sumP r1
val _ = Print.printLn (itos s)

val _ = Print.printLn "Done."

