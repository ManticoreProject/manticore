(* compare.pml
 * 
 * Sparse matrix vector multiplication.
 * Common example in the literature.
 *) 

structure Compare = struct

(* convenience bindings *)

  val println = Print.printLn

  val runseq = false

  val mkMatrix = SMVMSeq.mkMatrix

  fun cvt sm = let
    val rs = List.map PArray.fromList sm
    in
      PArray.fromList rs
    end

  fun go sz = let
    val _ = println ("Testing \"square\" sparse matrices of size " ^ Int.toString sz)
    val smSeq = mkMatrix (sz, sz)
    val smPar = cvt smSeq
    val vSeq = List.tabulate (sz, fn _ => 1.0)
    val vPar = tabP (sz, fn _ => 1.0)
    val (parRes, parTime) = Time.timeToEval (fn () => SMVM.smvm (smPar, vPar))
    val (seqRes, seqTime) = 
      if runseq then
        Time.timeToEval (fn () => SMVMSeq.smvm (smSeq, vSeq))
      else
	(nil, 0)
    val _ = println (Int.toString sz ^ "," ^
		     Long.toString seqTime ^ "," ^ 
		     Long.toString parTime)
    in
      println ""
    end

  fun supergo (lower, upper, step) = let
    fun lp n = 
      if n > upper then ()
      else (go n; lp (n+step))
    in
      lp lower
    end

  val _ = let
    val it = 500000
    in
      supergo (it, it, 1)
    end

  val _ = println "Done."

end
