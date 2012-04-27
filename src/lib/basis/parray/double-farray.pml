(* double-farray.pml  
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Monomorphic FArrays of doubles.
 *)

structure DoubleFArray = struct

  structure S = Shape
  structure R = DoubleRope
  structure DS = DoubleSeq
  structure SR = SegReduce

  (* ***** FLATTENED ARRAYS ***** *)  

  fun failwith s = (Print.printLn s; raise Fail s)

  datatype double_farray = FArray of R.double_rope * S.shape
  
(* empty : 'a f_array *)
  val empty = FArray (R.empty (), S.Lf (0, 0))

(* dataOf : int_farray -> 'a rope *)
  fun dataOf (FArray (r, _)) = r

  (* (\* concatRope : int_farray rope -> int_farray *\) *)
  (*   fun concatRope fs = let *)
  (*     val len = R.length fs *)
  (*     fun lp (curr, i, data, ts) = *)
  (*       if curr >= len then *)
  (*         FArray (data, S.Nd (List.rev ts)) *)
  (* 	else let *)
  (*         val FArray (d, t) = R.sub (fs, curr) *)
  (*         val data' = R.concat (data, d) *)
  (*         val t' = S.incrBy i t *)
  (*         val ts' = t'::ts *)
  (*         val i' = S.maxIdx t' *)
  (*         in *)
  (*           lp (curr+1, i', data', ts') *)
  (* 	  end *)
  (*     in *)
  (* 	lp (0, 0, R.empty (), nil) *)
  (*     end *)

(*
  (* flatten : int_farray f_array -> int_farray *)
  (* doesn't make sense -- these things can't be nested *)
    fun flatten f = concatRope (dataOf f)
*)

(* length : int_farray -> int *)
  fun length (FArray (_, t)) = (case t
    of S.Lf (lo, hi) => hi-lo (* note: hi is excl upper bound, lo is incl lower bound *)
     | S.Nd ts => List.length ts
    (* end case *))

(* flatSub : int_farray * int -> int *)
  fun flatSub (FArray (data, shape), i) = (case shape
    of S.Lf (lo, hi) => let
	 val _ = ()
         (* val _ = Print.printLn ("flatSub called.") *)
	 (* val _ = Print.printLn ("Lf " ^ Int.toString lo ^ "/" ^ Int.toString hi) *)
	 (* val _ = Print.printLn ("i is " ^ Int.toString i) *)
	 (* val _ = Print.printLn ("the length of data is " ^ Int.toString (R.length data)) *)
         (* val _ = Print.printLn (R.toString data) *)
         in
           R.sub (data, lo+i)
         end
     | S.Nd ts => failwith "flatSub - Nd"
    (* end case *))

(* nestedSub : int_farray * int -> int_farray *)
(* the f_array returned is one level less deep than the arg *)
  fun nestedSub (FArray (data, shape), i) = (case shape
    of S.Nd ts => FArray (data, List.nth (ts, i))
     | S.Lf _ => failwith "nestedSub - Lf"
    (* end case *))

(* tab : int * (int -> double) -> double_farray *)
  fun tab (n, f : int -> double) =
    if n <= 0 then 
      empty
    else let
      val data = R.tab (n, f)
      val shape = S.Lf (0, n)
      in
        FArray (data, shape)
      end

(* tabFromToStep : int * int * int * (int -> 'a) -> int_farray *)
  fun tabFromToStep (from, to_, step, f) = let
    val data = R.tabFromToStep (from, to_, step, f)
    val shape = S.Lf (0, R.length data)
    in
      FArray (data, shape)
    end

(* flatMap : (int -> int) -> int_array -> int_array *)
  fun flatMap f (FArray (data, shape)) = let
    val data' = R.map f data
    in
      FArray (data', shape)
    end

(* nestedMap : (int -> 'b) -> int_farray -> 'b f_array *)
  fun nestedMap f (FArray (data, shape)) = failwith "nestedMap - todo"

(* clean : int_farray -> int_farray *)
  fun clean (FArray (data, shape)) = (case shape
    of S.Lf (lo, hi) =>
         if lo = 0 andalso hi = R.length data then 
           FArray (data, shape)
	 else let
		   (* FIXME this is a slow implementation *)
           val data' = R.fromSeq (R.partialSeq (data, lo, hi))
           in
             FArray (data', S.Lf (0, hi-lo))
           end
     | S.Nd ts => let
         val (lo, hi) = S.span shape
           (* FIXME this is a slow implementation *)
	 val data' = R.fromSeq (R.partialSeq (data, lo, hi))
         in
           if lo = 0 then
             FArray (data', shape)
	   else if lo > 0 then
	     FArray (data', S.incrBy (~lo) shape)
	   else
	     failwith "clean - bug"
	 end
    (* end case *))

(* reduce : (dbl * dbl -> dbl) -> dbl -> dbl_farray -> dbl *) 
  fun reduce assocOp zero (FArray (data, shape)) = (case shape
    of S.Lf (lo, hi) => let 
         val FArray (data', shape') = clean (FArray (data, shape))
         in 
           R.reduce assocOp zero data'
         end
       | S.Nd _ => failwith "reduce - flat double_farray expected"
      (* end case *))

(* flatApp : (int -> unit) -> int_farray -> unit *)
  fun flatApp f (FArray (data, shape)) = (case shape
    of S.Lf (lo, hi) =>
         (* remember, farrays might carry ballast *)
         if lo = 0 andalso hi = R.length data then 
           R.app f data
         else
           flatApp f (clean (FArray (data, shape)))
     | S.Nd _ => failwith "flatApp - Nd"
    (* end case *))

(* fromList : int list -> int_farray *)
  fun fromList ns = let
    val data = R.fromList ns
    val shape = S.Lf (0, R.length data)
    in
      FArray (data, shape)
    end

(* Segmented Reduce
   This is almost identical to the code in SR, but
   rewritten to use monomorphic Double Ropes and Double Seqs *)

(*  fun stopwatch (label, thunk) = let
    val t0 = Time.now ()
    val x = thunk ()
    val t1 = Time.now ()
    val _ = Print.printLn ("time in " ^ label ^ ": " ^ Time.toStringMicrosec(t1-t0))
    in
      x
    end*)
  fun stopwatch (_, thunk) = thunk () 

  (* writePairs : 'a double_seq * (int * 'a) list -> 'a list *)
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

  (* partReduce : ('a -> 'a) * 'a * 'a double_seq * int * int -> 'a *)
  fun partReduce (f, init, data, lo, len) = let
    val hi = lo+len
    val dataLen = DS.length data
    fun sub i = DS.unsafeSub(data,i)
    fun lp (i, acc) =
      if i >= hi then acc
      else lp (i+1, f(acc, (sub i)))
    in
      if hi > dataLen
      then
          raise Fail "partReduce: index out of bounds"
      else 
          lp (lo, init)
    end

  (* segReducev : ('a -> 'a) * 'a * 'a double_seq * (int * int) list -> (int * 'a) list *)
  fun segReducev (f, init, v, ps) = let
    fun lp (i, ps) = (case ps
      of nil => nil
       | (j,n)::t => let
           val s = partReduce (f, init, v, i, n)
           in
             (j,s)::lp(i+n,t)
           end
      (* end case *))
    in
      lp (0, ps)
    end

  (* segreduce : ('a -> 'a) * 'a * 'a double_farray farray -> 'a double_farray *)
  fun segreduce' (f, init, nss) = let
    val (FArray (data, shape)) = nss
    val segdes = stopwatch ("segreduce'.segdesFromShape", fn () => SR.segdesFromShape shape)
    fun lp (r, ps) = (case r
      of R.Leaf v => segReducev(f,init,v,ps)::nil
       | R.Cat (_, _, rL, rR) => let
           val nL = R.length rL
           val (psL, psR) = SR.split (nL, ps)
           val (sumsL, sumsR) = (| lp (rL, psL), lp (rR, psR) |)
           in
             sumsL @ sumsR
           end
      (* end case *))
    val pss = stopwatch ("segreduce'.lp", fn () => lp (data, segdes))
    (* val _ = Print.printLn "in segsum: computed pss:" *)
    (* val _ = Print.printLn (psstos pss) *)
    val reductions = stopwatch ("segreduce'.reductions", fn () => DS.tabulate (List.length segdes, fn _ => init))
    val _ = stopwatch ("segreduce'.writePairs", fn () => writePairs reductions pss)
    val data' = stopwatch ("segreduce'.fromSeq", fn () => R.fromSeq reductions)
    val shape' = S.Lf (0, R.length data')
    in
      FArray (data', shape')
    end

  fun mergeAppend (psL, psR) =
   (case (psL, psR)
     of (nil, _) => psR
      | ((i,x)::nil, (j,y)::more) => if (i=j) then (i,x+y)::more else (i,x)::psR
      | (p::ps, _) => p::mergeAppend(ps,psR)
     (* end case *))

  fun yell s = Print.printLn s

  fun mkRope arr = let
    val n = DS.length arr
    fun subArr (lo, hi) = DS.tabulate (hi-lo+1, fn i => DS.sub (arr,lo+i))
    fun lp (lo, hi) = let
      val len = hi-lo+1
      in
        if len>LeafSize.getMax() then let
          val m = lo + (len div 2)
          val (rL, rR) = (| lp (lo,m), lp(m+1,hi) |)
          in
            R.nccat2 (rL, rR)
          end
        else
          R.Leaf (subArr (lo, hi))
      end
    in
      lp (0, n-1)
    end

  fun newSegReduce (f, ident, xss) = let
    val (FArray (data, shape)) = xss
    val segdes = SR.segdesFromShape shape
    val nSegs = List.length segdes
    val sums = DS.tabulate (nSegs, fn _ => ident)
    fun updateSums (i, x) = DS.update (sums, i, x)
    fun lp (r, ps) = (case r
      of R.Leaf v => segReducev (f, ident, v, ps)
       | R.Cat (_, _, rL, rR) => let
           val nL = R.length rL
           val (psL, psR) = SR.split (nL, ps)
           val (sumsL, sumsR) = (| lp(rL,psL), lp(rR,psR) |)
           val res = stopwatch ("mergeAppend", fn () => mergeAppend (sumsL, sumsR)) 
           in
             res
           end
      (* end case *))
    val ps = stopwatch ("newSegReduce main loop", fn () => lp (data, segdes))
    val _ = stopwatch ("updateSums", fn () => List.app updateSums ps)
    val data' = stopwatch ("mkRope", fn () => mkRope sums)
    val shape' = S.Lf (0, nSegs)
    in
      FArray (data', shape')
    end

  fun mikeSegReduce _ = raise Fail "todo"

  fun segreduce (f, init, nss) = stopwatch ("segreduce", fn () => newSegReduce (f, init, nss))

end
