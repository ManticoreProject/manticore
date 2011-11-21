structure Main = struct

  fun mkTestF sz = List.tabulate (sz, fn i => List.tabulate (sz, fn j => 1))

  fun fromTo (loIncl, hiExcl) = let
    val n = hiExcl - loIncl 
    in
      List.tabulate (n, fn i => (loIncl+i) mod 5)
    end

  fun mkTestF' n = let
    fun lp (i, lastHi, acc) = 
      if i>=n then List.rev acc
      else let
        val lo = lastHi
	val hi = lo+i+1
        val lf = fromTo (lo, hi)
        in 
          lp (i+1, hi, lf::acc)
        end
    in
      lp (0, 0, [])
    end

  fun segsum nss = List.map (List.foldl op+ 0) nss
  
  val dfltN = 256

  fun getArgs args = let
    fun finish (SOME s, vrb) = (s, vrb)
      | finish (NONE, vrb) = (dfltN, vrb)
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

  fun println s = (TextIO.print s; TextIO.print "\n")

  fun showFrom (lo, hi, sums) = let
    val n = List.length sums
    in
      if hi > n then ()
      else let
        val _ = print (itos lo ^ " to " ^ itos hi ^ ":\t")
        fun lp i = 
          if i > hi then println ""
          else (print (itos (List.nth (sums, i)) ^ " "); lp (i+1))
        in
          lp lo
        end
    end

  fun tellMeAbout sums = let
    val n = List.length sums
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
    fun doit () = segsum (mkTestF' sz)
    val sums = RunSeq.runMicrosec doit
    val _ = if verbose then tellMeAbout sums else ()
    in
      OS.Process.success
    end

end
