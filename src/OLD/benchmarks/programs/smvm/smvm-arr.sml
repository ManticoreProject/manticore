(* smvm-arr.pml
 * 
 * Sparse matrix vector multiplication.
 * Common example in the literature.
 *) 

structure SMVMArr = struct

(* convenience bindings *)

  val println = (fn s => (print s; print "\n"))

  val tab = List.tabulate
  val sum = (Array.foldl (fn (x,y) => x+y) 0.0)

  val rrr = Random.rand (0, 1000)

(* arrayToListMap : ('a -> 'b) -> 'a array -> 'b list *)
  fun arrayToListMap f a = let
    val n = Array.length a
    fun lp (i, acc) = 
      if i=n then List.rev acc
      else let
        val x = f (Array.sub (a, i))
        in
          lp (i+1, x::acc)
        end
    in
      lp (0, [])
    end

  fun arrayToString etos sep a = let
    val ss = arrayToListMap etos a
    in
      "[" ^ String.concatWith sep ss ^ "]"
    end

  fun tos sv = let
    fun pr (n, x) = String.concat ["(", Int.toString n, ",", Real.toString x, ")"]
    val ss = arrayToListMap pr sv
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
      Array.fromList (lp (0, nil))
    end

  fun mkSparseMatrix (nRows, nCols) = let
    fun lp (i, acc) = 
      if i=nRows then
        List.rev acc
      else
        lp (i+1, randomRow(nCols) :: acc)
    in
      Array.fromList (lp (0, nil))
    end

  fun dotp (sv, v) = let
    val n = Array.length sv
    fun lp (i, acc) = 
      if i = n then
        acc
      else let
        val (j, x) = Array.sub (sv, i)
        val p = x * Array.sub (v, j)
        in
          lp (i+1, p+acc)
	end  
    in
      lp (0, 0.0)
    end

(* arrayMap : ('a -> 'b) -> 'b -> 'a array -> 'b array *)
  fun arrayMap f init a = let
    val n = Array.length a
    val b = Array.array (n, init)
    fun lp i =
      if i = n then ()
      else (Array.update (b, i, f (Array.sub (a, i)));
	    lp (i+1))
    in
      (lp 0; b)
    end

  fun smvm (sm, v) = arrayMap (fn sv => dotp (sv, v)) 0.0 sm

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
    val v = Array.array (sz, 1.0)
    val (arrRes, arrTime) = timeToEval (fn () => smvm (sm, v))
    val _ = println (Int.toString sz ^ "," ^
		     LargeInt.toString arrTime)
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
