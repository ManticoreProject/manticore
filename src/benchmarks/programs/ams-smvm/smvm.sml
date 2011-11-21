(* smvm.sml
 * 
 * Sparse matrix vector multiplication.
 * Common example in the literature.
 *) 


structure Rope = RopeImplFn (
		   structure Seq = VectorSeq
		   structure RT = SimpleRuntime
		   val C = 2.0
		)

structure SMVM = struct

  fun sum a = Rope.reduce (fn (x,y) => x + y) 0.0 a

  fun dotp (sv, v) = sum (Rope.map (fn (i, x) => x * Rope.sub (v, i)) sv)

  fun smvm (sm, v) = Rope.map (fn sv => dotp (sv, v)) sm

end

structure Main =
  struct

    structure A = Array

  (* reads from any matrix-market named mtx.txt -- just strip off the header comments first *)
    fun readFromFile () =
	let
	    val f = TextIO.openIn "../../input-data/mtx.txt"
	    fun rdd d = Option.valOf (Real.fromString d)
	    fun rdi d = Option.valOf (Int.fromString d)
	  (* number of rows, number of columns, number of nonzeros *)
	    val R::C::N::nil = String.tokens (fn c => c = #" ") (Option.valOf (TextIO.inputLine f))
	    val R = rdi R
	    val C = rdi C
	    val N = rdi N
	    val rows = A.array (R, nil)   (* rows[r] contains the list of (c,v) pairs for occupying the rth row *)
	    fun lp () =
		(case TextIO.inputLine f
		  of NONE => ()
		   | SOME line => 
		     let
		       (* row, column, value *)
			 val r::c::v::nil = String.tokens (fn c => c = #" ") line
			 val r = rdi r - 1
			 val c = rdi c - 1
			 val cols = A.sub (rows, r)
		     in
			 A.update (rows, r, (c, rdd v)::cols);
			 lp ()
		     end)
	in
	    lp ();
	    (rows, C)
	end

    fun rows2sm rows = Rope.fromList (List.tabulate (A.length rows, fn r => Rope.fromList (A.sub (rows, r))))

    val epsilon = 0.0000000001

    fun bumpSV1 sv = Rope.map (fn (i,x) => (i+1, x+epsilon)) sv
    fun bumpSV2 sv = Rope.map (fn (i,x) => (i-1, x-epsilon)) sv
    fun bumpSM sm = Rope.map (fn sv => bumpSV2 (bumpSV1 sv)) sm

    val rand = Random.rand (0, 100000)
	
    fun main (_, args) =
	let
	    val (mtx, C) = readFromFile ()
	    val (mtx, v) = (bumpSM (rows2sm mtx),
			    Rope.fromList (List.tabulate (C, fn _ => Random.randReal rand)))
            fun doitN (n) = (if n=0 then () else (
                             SMVM.smvm (mtx, v);
                             doitN (n-1)))
                           
	    fun doit () = doitN 100
	in
	    RunSeq.run doit
	end

  end
