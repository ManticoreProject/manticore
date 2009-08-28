(* smvm.pml
 * 
 * Sparse matrix vector multiplication.
 * Common example in the literature.
 *) 

structure SMVM = struct

(* convenience bindings *)

  val println = Print.printLn

  val sub = PArray.sub
  val sumP = (fn a => reduceP (fn (x,y) => x + y, 0.0, a))

  val _ = Rand.seed ()

(* tos : sparse_vec -> string *)

  val tos = let
    fun pr (n, x) = String.concat ["(", Int.toString n, ",", Float.toString x, ")"]
    in
      PArray.toString pr ","
    end

  fun randomRow len = [| (i, 1.0) | i in [| 0 to (len-1) |] 
			            where Rand.inRangeInt (0, 4) = 0 |]

(* a non-random row *)
  fun mkRow len = [| (i, 1.0) | i in [| 0 to (len-1) |] where i < 10 |]
(*
  val _ = let
    val ns = [| Rand.inRangeInt(0,10) | n in [| 0 to 9 |] |]
    in
      Print.printLn (PArray.toString Int.toString "," ns)
    end
*)

  fun mkRandMatrix (nRows, nCols) = [| randomRow nCols | r in [| 1 to nRows |] |]

  fun mkMatrix (nRows, nCols) = [| mkRow nCols | r in [| 1 to nRows |] |]

(*
  val _ = let
    val sa = mkSparseMatrix (20, 5)
    fun atos a = PArray.toString tos "\n" a
    in
      println (atos sa)
    end
*)

  fun dotp (sv, v) = sumP [| x * subP(v, i) | (i,x) in sv |]

(*
  val _ = let
    val sv = [| (1, 1.0) |]
    val v = tabP (10, fn _ => 1.0)
    in
      println ("expecting 1.0 => " ^ Float.toString (dotp (sv, v)))
    end
*)

  fun smvm (sm, v) = [| dotp (sv, v) | sv in sm |]

  fun smtos sm = PArray.toString (PArray.toString Float.toString ",") "\n" sm

(*
  val _ = let
    val sm = mkSparseMatrix (200, 200)
    val v = tabP (200, fn _ => 1.0)
    val v' = smvm (sm, v)
    in
      println (PArray.toString Float.toString "\n" v')
    end
*)

end
