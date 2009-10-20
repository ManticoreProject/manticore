(* smvm-seq.pml
 * 
 * Sparse matrix vector multiplication.
 * Common example in the literature.
 *) 

structure SMVMSeq = struct

(* convenience bindings *)

  val println = (fn s => (print s; print "\n"))

  val sub = List.nth
  val tab = List.tabulate
  val sum = (List.foldl (fn (x,y) => x+y) 0.0)

  val rrr = Random.rand (0, 1000)

  fun listToString elt sep xs = let
    val ss = List.map elt xs
    in
      "[" ^ String.concatWith sep ss ^ "]"
    end

  fun tos sv = let
    fun pr (n, x) = String.concat ["(", Int.toString n, ",", Real.toString x, ")"]
    val ss = List.map pr sv
    in
      "[" ^ String.concatWith "," ss ^ "]"
    end

  fun randomRow len = let
    fun lp (i, acc) =
      if i=len then
        List.rev acc
      else let
        val r = Random.randRange (0, 4) rrr
        in
          if r=0 then
            lp (i+1, (i, 1.0) :: acc)
	  else
	    lp (i+1, acc)
        end
    in
      lp (0, nil)
    end

  fun mkSparseMatrix (nRows, nCols) = let
    fun lp (i, acc) = 
      if i=nRows then
        List.rev acc
      else
        lp (i+1, randomRow(nCols) :: acc)
    in
      lp (0, nil)
    end

  fun dotp (sv, v) = sum (List.map (fn (i,x) => x * sub(v,i)) sv)

  fun smvm (sm, v) = List.map (fn sv => dotp (sv, v)) sm

  fun timeToEval f = let
    val t0 = Time.toMicroseconds (Time.now ())
    val x = f ()
    val t1 = Time.toMicroseconds (Time.now ())
    in
      (x, t1-t0)
    end

  fun go sz = let
    val _ = println ("Testing \"square\" sparse matrices of size " ^ Int.toString sz)
    val sm = mkSparseMatrix (sz, sz)
    val v = List.tabulate (sz, fn _ => 1.0)
    val (smlRes, smlTime) = timeToEval (fn () => smvm (sm, v))
    val _ = println (Int.toString sz ^ "," ^
		     LargeInt.toString smlTime)
    in
      println ""
    end

  fun supergo () = let
    fun lp n = 
      if n > 6000 then ()
      else (go n; lp (n+1000))
    in
      lp 3000
    end

  val _ = supergo ()

end
