(* prototype of parallel segmented sum *)

val ln = Print.printLn
val itos = Int.toString

fun plus (x:double, y:double) = x+y

fun sum (xs : double parray) = PArray.reduce plus (0.0) xs

fun dotp (sv, v) = sum [| x * (v!i) | (i,x) in sv |]

fun smvm (sm, v) = [| dotp (sv, v) | sv in sm |]

(* **** driver stuff from here on **** *)

fun smv sz = let
  val vlen = 1000
  fun upto n = (n * (n+1)) div 2
  fun f i = ((i * 7) mod vlen, Double.fromInt i * 0.001)
  fun sv n = [| f (i + upto n) | i in [| 0 to n |] |]
  val sm = [| sv i | i in [| 0 to (sz-1) |] |]
  val v = [| 1.0 | _ in [| 1 to vlen |] |]
  in
    (sm, v)
  end

fun getSize args = let
  val defaultSize = 256
  fun lp (args, verbose, size) = (case args
    of s::ss =>
         if String.same (s, "-size") then (case ss
           of s'::ss' => lp (ss', verbose, Int.fromString s')
            | nil => lp ([], verbose, SOME defaultSize)
             (* end case *))
	 else if String.same (s, "-v") then
           lp (ss, true, size)
         else (* silently traverse others *)
           lp (ss, verbose, size)
     | nil => (case size
         of NONE => (defaultSize, verbose)
          | SOME sz => (sz, verbose)
         (* end case *))
    (* end case *))
  in
    lp (args, false, NONE)
  end

fun incantation thunk = let
  val rwg = ImplicitThread.runOnWorkGroup
  val wg = WorkStealing.workGroup ()
  in
    rwg (wg, thunk)
  end

fun showFrom (lo, hi, sums) = let
  val n = PArray.length sums 
  in
    if hi > n then ()
    else let
      val _ = Print.print (itos lo ^ " to " ^ itos hi ^ ": ")
      fun lp i = 
        if i > hi then 
          Print.printLn ""
	else let
          val s = sums!i
	  val _ = Print.print (Double.toString s ^ " ")
          in
            lp (i+1)
          end
      in
        lp lo
      end
  end

fun tellMeAbout sums = let
  val n = PArray.length sums
  val _ = Print.printLn ("number of sums: " ^ itos n)
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

fun doit sz = let
  val (sm, v) = smv sz 
  in
    smvm (sm, v)
  end

val _ = let
  val (sz, verbose) = getSize (CommandLine.arguments ())
  fun doit' () = RunPar.runMicrosec (fn () => doit sz)
  val sums = incantation doit'
  in
    if not verbose then ()
    else tellMeAbout sums
  end
