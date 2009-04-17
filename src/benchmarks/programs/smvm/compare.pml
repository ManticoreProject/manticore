(* compare.pml
 * 
 * Sparse matrix vector multiplication.
 * Common example in the literature.
 *) 

structure Compare = struct

(* convenience bindings *)

  val println = Print.printLn

(* mkSparseMatrix makes a list-based sparse matrix *)
  fun mkSparseMatrix (nRows, nCols) = let
    fun lp (i, acc) = 
      if i=nRows then
        List.rev acc
      else
        lp (i+1, SMVMSeq.randomRow(nCols) :: acc)
    in
      lp (0, nil)
    end

  fun cvt sm = let
    val rs = List.map Ropes.fromList sm
    in
      Ropes.fromList rs
    end

  fun go sz = let
    val _ = println ("Testing \"square\" sparse matrices of size " ^ Int.toString sz)
    val smSeq = mkSparseMatrix (sz, sz)
    val smPar = cvt smSeq
    val vSeq = List.tabulate (sz, fn _ => 1.0)
    val vPar = Ropes.fromList vSeq 
    val (seqRes, seqTime) = Time.timeToEval (fn () => SMVMSeq.smvm (smSeq, vSeq))
    val (parRes, parTime) = Time.timeToEval (fn () => SMVM.smvm (smPar, vPar))
(*
    val _ = println ("seq time: " ^ Long.toString seqTime)
    val _ = println ("par time: " ^ Long.toString parTime)
*)
    val _ = println (Int.toString sz ^ "," ^
		     Long.toString seqTime ^ "," ^ 
		     Long.toString parTime)
    in
      println ""
    end

  fun supergo () = let
    fun lp n = 
      if n > 9999 then ()
      else (go n; lp (n*2))
    in
      lp 1
    end

  val _ = supergo ()

  val _ = println "Done."

end
