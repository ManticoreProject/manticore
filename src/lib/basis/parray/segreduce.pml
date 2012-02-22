(* Segmented Reduce and its associated utilities. *)

structure S = Shape
structure F = FArray

fun fail a b = raise Fail (String.concat["seg-sum", a, b])

(* writePairs : 'a seq * (int * 'a) list -> 'a list *)
fun writePairs res pss = let
  fun sub i = Seq.sub (res, i)
  fun upd (i, x) = Seq.update (res, i, x)
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

(* segdesFromShape : shape -> (int * int) list *)
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

(* partReduce : ('a -> 'a) * 'a * 'a seq * int * int -> 'a *)
fun partReduce (f, init, data, lo, hi) = let
  fun sub i = Seq.sub(v,i)
  fun lp (i, acc) =
    if i >= hi then acc
    else lp (i+1, f(acc, sub i))
  in
    lp (lo, init)
  end

(* segReducev : ('a -> 'a) * 'a * 'a seq * (int * int) list -> (int * 'a) list *)
fun segReducev (f, init, v, ps) = let
  fun lp (i, ps) = (case ps
    of nil => nil
     | (j,n)::t => let
	 val s = partreduce (f, init, v, i, n)
	 in
	   (j,s)::lp(i+n,t)
	 end
    (* end case *))
  in
    lp (0, ps)
  end

(* segreduce : ('a -> 'a) * 'a * 'a farray farray -> 'a farray *)
fun segreduce (f, init, nss) = let
  val (FArray (data, shape)) = nss
  val segdes = segdesFromShape shape
  fun lp (r, ps) = (case r
    of Rope.Leaf v => segReducev(f,init,v,ps)::nil
     | Rope.Cat (_, _, rL, rR) => let
         val nL = Rope.length rL
         val (psL, psR) = split (nL, ps)
	 val (sumsL, sumsR) = (| lp (rL, psL), lp (rR, psR) |)
         in
	   sumsL @ sumsR
         end
    (* end case *))
  val pss = lp (data, segdes)
  (* val _ = Print.printLn "in segsum: computed pss:" *)
  (* val _ = Print.printLn (psstos pss) *)
  val sums = Seq.tabulate (List.length segdes, fn _ => 0.0)
  val _ = writePairs sums pss
  val data' = Rope.fromSeq sums
  val shape' = S.Lf (0, Rope.length data')
  in
    FArray (data', shape')
  end
