(* prototype of parallel segmented sum *)

structure R = IntRope
structure S = Shape
structure F = FArray
structure IF = IntFArray
structure DF = DoubleFArray

val fail = Fail.fail "seg-sum" 

val ln = Print.printLn
val itos = Int.toString

fun ptos (n,m) = "(" ^ itos n ^ "," ^ itos m ^ ")"

fun pstos ps = String.concat (List.map ptos ps)

fun psstos pss = String.concatWith ";" (List.map pstos pss)

fun writePairs res pss = let
  fun sub i = IntSeq.sub (res, i)
  fun upd (i, x) = IntSeq.update (res, i, x)
  fun lp1 ps = (case ps
    of nil => ()
     | (i,n)::t => (upd (i, n); lp1 t)
    (* end case *))
  fun lp0 ps = (case ps
    of nil => ()
     | (i,n)::t => (upd (i, n+sub(i)); lp1 t)
    (* end case *))
  in 
    List.app lp0 pss
  end

fun segdesFromShape s = let
  fun lp (i, ls, acc) = (case ls
    of nil => List.rev acc
     | l::ls => (case l
         of S.Lf(lo,hi) => lp (i+1, ls, (i,hi-lo)::acc)
	  | S.Nd _ => fail "segdesFromShape" "Nd"
         (* end case *))
    (* end case *))
  in
    case s
      of S.Nd ls => let
           val sd = lp (0, ls, nil) 
           (* val _ = ln "original shape:" *)
	   (* val _ = ln (Shape.toString s) *)
	   (* val _ = ln "segdes:" *)
	   (* val _ = ln (pstos sd) *)
           in
             sd
           end
       | S.Lf _ => fail "segdesFromShape" "Lf"
  end

fun partsum (v, lo, len) = let
  val hi = lo+len
  fun sub i = IntSeq.sub (v, i)
  fun lp (i, acc) =
    if i>=hi then acc
    else lp (i+1, acc + sub(i))
  in
    lp (lo, 0)
  end

fun segsumv (v, ps) = let
  fun lp (i, ps) = (case ps
    of nil => nil
     | (j,n)::t => let
         val s = partsum (v, i, n)
         in
           (j,s)::lp(i+n,t)
         end
    (* end case *))
  in
    lp (0, ps)
  end

fun split (n, ps) = 
  if n<0 then fail "split" "n<0"
  else let
    fun lp (n, ps, acc) = 
      if n=0 then (List.rev acc, ps)
      else (case ps
        of nil => fail "split" "nil"
	 | (i,m)::t =>
             if (n<m) then 
               lp (0, (i,m-n)::t, (i,n)::acc)
	     else (* n>=m *)
               lp (n-m, t, (i,m)::acc)               
        (* end case *))
    in
      lp (n, ps, nil)
    end

fun segsum nss = let
  val (IF.FArray (data, shape)) = nss
  val segdes = segdesFromShape shape
  fun lp (r, ps) = (case r
    of R.Leaf v => segsumv(v,ps)::nil
     | R.Cat (_, _, rL, rR) => let
         val nL = R.length rL
         val (psL, psR) = split (nL, ps)
	 val (sumsL, sumsR) = (| lp (rL, psL), lp (rR, psR) |)
         in
	   sumsL @ sumsR
         end
    (* end case *))
  val pss = lp (data, segdes)
  (* val _ = Print.printLn "in segsum: computed pss:" *)
  (* val _ = Print.printLn (psstos pss) *)
  val sums = IntSeq.tabulate (List.length segdes, fn _ => 0)
  val _ = writePairs sums pss
  val data' = R.fromSeq sums
  val shape' = S.Lf (0, R.length data')
  in
    IF.FArray (data', shape')
  end

(* **** driver stuff from here on **** *)

fun mkTestF sz = let
  val data = IntRope.tab (sz*sz, fn _ => 1)
  val leaves = List.tabulate (sz, fn i => S.Lf (i*sz, i*sz+sz))
  val shape = S.Nd leaves
  in
    IF.FArray (data, shape)
  end

fun mkLeaves n = let
  fun lp (i, lastHi, acc) = 
    if i>=n then List.rev acc
    else let
      val lo = lastHi
      val hi = lo+i+1
      val lf = S.Lf (lo, hi)
      in 
        lp (i+1, hi, lf::acc)
      end
  in
    lp (0, 0, [])
  end

fun mkTestF' sz = let
  val len = (sz * (sz + 1)) div 2
  val shape = S.Nd (mkLeaves sz)
  val data = IntRope.tabulate (len, fn i => i mod 5)
  in 
    IF.FArray (data, shape)	      
  end

fun showMe (ns : IF.int_farray) = let
  val len = IF.length ns
  val (IF.FArray (data, shape)) = ns
  fun pr i = Print.print (Int.toString (IntRope.sub (data, i)) ^ " ")
  fun lp i = if (i >= len) then Print.printLn "" else (pr i; lp (i+1))
  in
    lp 0
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
  val n = IF.length sums 
  in
    if hi > n then ()
    else let
      val _ = Print.print (Int.toString lo ^ " to " ^ Int.toString hi ^ ": ")
      val (IF.FArray (data, _)) = sums
      fun lp i = 
        if i > hi then 
          Print.printLn ""
	else let
          val s = IntRope.sub (data, i)
	  val _ = Print.print (Int.toString s ^ " ")
          in
            lp (i+1)
          end
      in
        lp lo
      end
  end

fun tellMeAbout sums = let
  val n = IF.length sums
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
