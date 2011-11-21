(* prototype of parallel segmented sum *)

fun fail proc msg = raise Fail ("nested-seg-sum." ^ proc ^ ": " ^ msg)

val ln = Print.printLn
val itos = Int.toString

fun plus (n:int, m:int) = n+m
fun sum ns = PArray.reduce (plus, 0, ns)

fun segsum nss = [| sum ns | ns in nss |]

(* **** driver stuff from here on **** *)

fun mkTestF' sz = let
  fun upto n = (n * (n+1)) div 2
  fun mk n = [| m mod 5 | m in [| upto n to (upto(n+1)-1) |] |]
  in 
    [| mk i | i in [| 0 to (sz-1) |] |]
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
      val _ = Print.print (Int.toString lo ^ " to " ^ Int.toString hi ^ ": ")
      fun lp i = 
        if i > hi then 
          Print.printLn ""
	else let
          val s = sums!i
	  val _ = Print.print (Int.toString s ^ " ")
          in
            lp (i+1)
          end
      in
        lp lo
      end
  end

fun tellMeAbout sums = let
  val n = PArray.length sums
  val _ = Print.printLn ("number of sums: " ^ Int.toString n)
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

fun doit sz = segsum (mkTestF' sz)

val _ = let
  val (sz, verbose) = getSize (CommandLine.arguments ())
  fun doit' () = RunPar.runMicrosec (fn () => doit sz)
  val sums = incantation doit'
  in
    if not verbose then ()
    else tellMeAbout sums
  end
