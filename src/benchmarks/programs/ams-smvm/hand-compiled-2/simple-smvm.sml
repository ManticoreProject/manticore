structure Main = struct

  val sum = Vector.foldl op+ 0.0

  fun dotp v sv = sum (Vector.map (fn (i,x) => x * (Vector.sub (v, i))) sv)

  fun smvm (sm, v) = Vector.map (dotp v) sm

(* **** driver follows **** *)

  fun smv sz = let
    val vlen = 1000
    fun upto n = (n * (n+1)) div 2
    fun f i = ((i*7) mod vlen, real i * 0.001)
    fun sv n = Vector.tabulate (n+1, fn i => f (i + upto(n)))
    val sm = Vector.tabulate (sz, sv)
    val v = Vector.tabulate (vlen, fn _ => 1.0)
    in
      (sm, v)
    end

  fun getArgs args = let
    val defaultSize = 256
    fun finish (SOME s, vrb) = (s, vrb)
      | finish (NONE, vrb) = (defaultSize, vrb)
    fun lp (args, size, verbose) = (case args
      of [] => finish (size, verbose)
       | [arg] => finish (if arg = "-v" then (size, true) else (size, verbose))
       | arg1::arg2::t => 
           if arg1 = "-v" then lp (arg2::t, size, true) 
           else if arg1 = "-size" then lp (t, Int.fromString arg2, verbose)
           else lp (arg2::t, size, verbose)
      (* end case *))
    in
      lp (args, NONE, false)
    end
	
  val itos = Int.toString
  val ftos = Real.toString

  fun println s = (TextIO.print s; TextIO.print "\n")

  fun showFrom (lo, hi, sums) = let
    val n = Vector.length sums
    in
      if hi > n then ()
      else let
        val _ = print (itos lo ^ " to " ^ itos hi ^ ":\t")
        fun lp i = 
          if i > hi then println ""
          else (print (ftos (Vector.sub (sums, i)) ^ " "); lp (i+1))
        in
          lp lo
        end
    end

  fun tellMeAbout sums = let
    val n = Vector.length sums
    val _ = println ("number of sums: " ^ itos n)
    fun lp m = 
      if (m+5)>n then ()
      else let
        val _ = showFrom (m, m+5, sums)
        in
          if m=0 then lp 10 else lp (m*10)
        end
    in
      lp 0
    end

  fun main (_, args) = let
    val (sz, verbose) = getArgs args
    val (sm, v) = smv sz
    fun doit () = smvm (smv sz)
    val sums = RunSeq.runMicrosec doit
    val _ = if verbose then tellMeAbout sums else ()
    in
      OS.Process.success
    end

end
