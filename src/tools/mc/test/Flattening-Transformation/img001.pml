(* I want to see if simple image processing makes it through the FT *)
(* (ramping up to ray tracing) *)

val sz = 64
val img = Image.new (sz, sz)
val field = [| [| (i,j) | j in [| 0 to sz-1 |] |] | i in [| 0 to sz-1 |] |]

fun setWhite (i, j) = let
  val itos = Int.toString
  val _ = Print.printLn ("setting " ^ itos i ^ "," ^ itos j ^ " to white")
  in
    Image.update3d (img, i, j, 1.0, 1.0, 1.0)
  end

(* fun app f = let *)
(*   fun appf (arr : (int * int) parray parray) = let *)
(*     val n = PArray.length arr *)
(*     fun lp i =  *)
(*       if (i>=n) then () else (f (arr!i); lp (i+1)) *)
(*     in *)
(*       lp 0 *)
(*     end *)
(*   in *)
(*     appf *)
(*   end *)

val _ = PArray.app (PArray.app setWhite) field 

val _ = Image.output("out-img000.ppm", img)
val _ = Image.free img

val _ = Print.printLn "done."
