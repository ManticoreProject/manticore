(* double-farray.pml  
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Monomorphic FArrays of doubles.
 *)

structure DoubleFArray = struct

  structure S = ShapeTree
  structure R = DoubleRope

  val fail = Fail.fail "DoubleFArray"

  (* ***** FLATTENED ARRAYS ***** *)  

  datatype double_farray = FArray of R.double_rope * S.shape_tree
  
(* empty : 'a f_array *)
  val empty = FArray (R.empty, S.Lf (0, 0))

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
  (* 	lp (0, 0, R.empty, nil) *)
  (*     end *)

(*
  (* flatten : int_farray f_array -> int_farray *)
  (* doesn't make sense -- these things can't be nested *)
    fun flatten f = concatRope (dataOf f)
*)

(* length : double_farray -> int *)
  fun length (FArray (_, t)) = (case t
    of S.Lf (lo, hi) => hi-lo (* note: hi is excl upper bound, lo is incl lower bound *)
     | S.Nd ts => List.length ts
    (* end case *))

(* flatSub : double_farray * int -> double *)
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
     | S.Nd ts => fail "flatSub" "Nd"
    (* end case *))

(* nestedSub : double_farray * int -> double_farray *)
(* the f_array returned is one level less deep than the arg *)
  fun nestedSub (FArray (data, shape), i) = (case shape
    of S.Nd ts => FArray (data, List.nth (ts, i))
     | S.Lf _ => fail "nestedSub" "Lf"
    (* end case *))

(* tab : int * (int -> double) -> double_farray *)
  fun tab (n, f : int -> double) =
    if n <= 0 then 
      empty
    else let
      val data = R.tabP (n, f)
      val shape = S.Lf (0, n)
      in
        FArray (data, shape)
      end

(* tabFromToStep : int * int * int * (int -> double) -> double_farray *)
  fun tabFromToStep (from, to_, step, f : int -> double) = let
    val data = R.tabFromToStepP (from, to_, step, f)
    val shape = S.Lf (0, R.length data)
    in
      FArray (data, shape)
    end

(* flatMap : (double -> double) -> double_array -> double_array *)
  fun flatMap (f : double -> double) (FArray (data, shape)) = let
    val data' = R.mapP (f, data)
    in
      FArray (data', shape)
    end

(* nestedMap : (double -> 'b) -> double_farray -> 'b f_array *)
  fun nestedMap f (FArray (data, shape)) = fail "nestedMap" "todo"

(* clean : double_farray -> double_farray *)
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
	     fail "clean" "bug"
	 end
    (* end case *))

(* reduce : (dbl * dbl -> dbl) -> dbl -> dbl_farray -> dbl *) 
  fun reduce assocOp zero (FArray (data, shape)) = (case shape
    of S.Lf (lo, hi) => let 
         val FArray (data', shape') = clean (FArray (data, shape))
         in 
           R.reduceP (assocOp, zero, data')
         end
       | S.Nd _ => fail "reduce" "flat int_farray expected"
      (* end case *))

(* flatApp : (double -> unit) -> double_farray -> unit *)
  fun flatApp (f : double -> unit) (FArray (data, shape)) = (case shape
    of S.Lf (lo, hi) =>
         (* remember, farrays might carry ballast *)
         if lo = 0 andalso hi = R.length data then 
           R.app (f, data)
         else
           flatApp f (clean (FArray (data, shape)))
     | S.Nd _ => fail "flatApp" "Nd"
    (* end case *))

(* fromList : double list -> double_farray *)
  fun fromList (ns : double list) = let
    val data = R.fromList ns
    val shape = S.Lf (0, R.length data)
    in
      FArray (data, shape)
    end

end
