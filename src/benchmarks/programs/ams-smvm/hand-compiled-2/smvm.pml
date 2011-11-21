(* prototype of parallel segmented sum *)

structure S = Shape
structure F = FArray
structure IS = IntSeq
structure DS = DoubleSeq
structure IR = IntRope
structure DR = DoubleRope
structure IF = IntFArray
structure DF = DoubleFArray

val fail = Fail.fail "seg-sum" 

val ln = Print.printLn
val itos = Int.toString

fun ptos (n,m) = "(" ^ itos n ^ "," ^ itos m ^ ")"

fun pstos ps = String.concat (List.map ptos ps)

fun psstos pss = String.concatWith ";" (List.map pstos pss)

fun writePairs res pss = let
  fun sub i = DS.sub (res, i)
  fun upd (i, x) = DS.update (res, i, x)
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
	   (* val _ = ln (S.toString s) *)
	   (* val _ = ln "segdes:" *)
	   (* val _ = ln (pstos sd) *)
           in
             sd
           end
       | S.Lf _ => fail "segdesFromShape" "Lf"
  end

fun partsum (v, lo, len) = let
  val hi = lo+len
  fun sub i = DS.sub (v, i)
  fun lp (i, acc) =
    if i>=hi then acc
    else lp (i+1, acc + sub(i))
  in
    lp (lo, 0.0:double)
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
  val (DF.FArray (data, shape)) = nss
  val segdes = segdesFromShape shape
  fun lp (r, ps) = (case r
    of DR.Leaf v => segsumv(v,ps)::nil
     | DR.Cat (_, _, rL, rR) => let
         val nL = DR.length rL
         val (psL, psR) = split (nL, ps)
	 val (sumsL, sumsR) = (| lp (rL, psL), lp (rR, psR) |)
         in
	   sumsL @ sumsR
         end
    (* end case *))
  val pss = lp (data, segdes)
  (* val _ = Print.printLn "in segsum: computed pss:" *)
  (* val _ = Print.printLn (psstos pss) *)
  val sums = DS.tabulate (List.length segdes, fn _ => 0.0)
  val _ = writePairs sums pss
  val data' = DR.fromSeq sums
  val shape' = S.Lf (0, DR.length data')
  in
    DF.FArray (data', shape')
  end

fun prodv (iv : IS.int_seq, dv : DS.double_seq, v : DS.double_seq) = let
  val n = IS.length iv
  fun prod (i:int) = let
    val j = IS.sub (iv, i)
    val x = DS.sub (dv, i)
    in
      x * DS.sub (v, j)
    end
  in
    DS.tabulate (n, prod)
  end

fun products (sm : IF.int_farray * DF.double_farray, v : DF.double_farray) : DF.double_farray = let
  val (indices, values) = sm
  val (IF.FArray (iData, iShape)) = indices
  val (DF.FArray (dData, dShape)) = values
  val _ = if S.same (iShape, dShape) then () else fail "products" "shapes"
  val (DF.FArray (vData, _)) = v
  val vSeq = DR.toSeq vData
  fun lp (irope, drope) = (case (irope, drope)
    of (IR.Leaf iv, DR.Leaf dv) => let
          val pv = prodv (iv, dv, vSeq)
          in
            DR.Leaf pv
          end
     | (IR.Cat (n, d, irL, irR), DR.Cat (_, _, drL, drR)) => let
         val (prodsL, prodsR) = (| lp (irL, drL), lp (irR, drR) |)
         in
           DR.Cat (n, d, prodsL, prodsR)
         end
     | _ => fail "products" "lp"
    (* end case *))
  val prods = lp (iData, dData)
  in
    DF.FArray (prods, iShape)
  end

fun smvm (sm, v) = let
  val prods = products (sm, v)
  val sums = segsum prods
  in
    sums
  end

(* **** driver stuff from here on **** *)

fun upto n = (n * (n+1)) div 2

fun mkLeaf n = S.Lf (upto n, upto (n+1))

fun mkLeaves n = List.tabulate (n, mkLeaf)

fun smv sz = let
  val n = upto sz
  val vlen = 1000
  val indices = IR.tab (n, fn i => (i * 7) mod vlen)
  val values = DR.tab (n, fn i => (Double.fromInt i) * 0.001)
  val shape = S.Nd (mkLeaves sz)
  val sm = (IF.FArray (indices, shape), DF.FArray (values, shape))
  val r = DR.tab (vlen, fn i => 1.0)
  val v = DF.FArray (r, S.Lf (0, vlen))
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
  val n = DF.length sums 
  in
    if hi > n then ()
    else let
      val _ = Print.print (itos lo ^ " to " ^ itos hi ^ ": ")
      val (DF.FArray (data, _)) = sums
      fun lp i = 
        if i > hi then 
          Print.printLn ""
	else let
          val s = DR.sub (data, i)
	  val _ = Print.print (Double.toString s ^ " ")
          in
            lp (i+1)
          end
      in
        lp lo
      end
  end

fun tellMeAbout sums = let
  val n = DF.length sums
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
