(* smvm-seq.pml
 * 
 * Sparse matrix vector multiplication.
 * Common example in the literature.
 *) 

structure SMVMSeq = struct

(* convenience bindings *)

  val println = Print.printLn

  val sub = List.nth
  val tab = List.tabulate
  val sum = (List.foldl (fn (x,y) => x+y) 0.0)

  val _ = Rand.seed ()

(* tos : sparse_vec -> string *)

  fun listToString elt sep xs = let
    val ss = List.map elt xs
    in
      "[" ^ String.concatWith sep ss ^ "]"
    end

  fun tos sv = let
    fun pr (n, x) = String.concat ["(", Int.toString n, ",", Float.toString x, ")"]
    val ss = List.map pr sv
    in
      "[" ^ String.concatWith "," ss ^ "]"
    end

  fun randomRow len = let
    fun lp (i, acc) =
      if i=len then
        List.rev acc
      else let
        val r = Rand.inRangeInt (0, 4)
        in
          if r=0 then
            lp (i+1, (i, 1.0) :: acc)
	  else
	    lp (i+1, acc)
        end
    in
      lp (0, nil)
    end

  fun mkRow len = let
    val len' = if len<10 then len else 10
    in
      List.tabulate (len', fn n => (n, 1.0))
    end

(*
  val _ = let
    val ns = [| Rand.inRangeInt(0,10) | n in [| 0 to 9 |] |]
    in
      Print.printLn (PArray.toString Int.toString "," ns)
    end
*)

  fun mkSparseMatrix (nRows, nCols) = let
    fun lp (i, acc) = 
      if i=nRows then
        List.rev acc
      else
        lp (i+1, randomRow(nCols) :: acc)
    in
      lp (0, nil)
    end

  fun mkMatrix (nRows, nCols) = let
    fun lp (i, acc) =
      if i=nRows then
        List.rev acc
      else
       lp (i+1, mkRow(nCols) :: acc)
    in
      lp (0, nil)
    end

(*
  val _ = let
    val sa = mkSparseMatrix (20, 5)
    fun atos a = PArray.toString tos "\n" a
    in
      println (atos sa)
    end
*)

  fun dotp (sv, v) = sum (List.map (fn (i,x) => x * sub(v,i)) sv)

(*
  val _ = let
    val sv = [(1, 1.0)]
    val v = tab (10, fn _ => 1.0)
    in
      println ("expecting 1.0 => " ^ Float.toString (dotp (sv, v)))
    end
*)

  fun smvm (sm, v) = List.map (fn sv => dotp (sv, v)) sm

(*
  val _ = let
    val sm = mkSparseMatrix (200, 200)
    val v = tab (200, fn _ => 1.0)
    val v' = smvm (sm, v)
    in
      println (listToString Float.toString "\n" v')
    end
*)

end
