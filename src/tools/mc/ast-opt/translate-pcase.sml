(* translate-pcase.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure TranslatePCase (* : sig

  (* An AST to AST translation of parallel cases. *)
    val tr : (AST.exp -> AST.exp) 
             -> AST.exp list * AST.pmatch list * AST.ty 
             -> AST.exp

  end *) = struct

    structure A = AST
    structure B = Basis
    structure F = Futures
    structure R = Ropes
    structure U = UnseenBasis

    structure CompletionBitstring : sig
      type t
      val eq  : t * t -> bool
      val sub : t * t -> bool
      val compare : t * t -> order
      val toString : t -> string
      val fromPPats : A.ppat list -> t
      val allOnes : int -> t
    end = struct
      datatype bit = Zero | One
      type t = bit list
      fun bitEq (b1:bit, b2:bit) = (b1=b2)
      fun eq (c1, c2) = 
            if (List.length c1) <> (List.length c2) then
              raise Fail "UnequalLengths"
	    else ListPair.all bitEq (c1, c2)
      (* c1 < c2 if everywhere c1 is 1, c2 is 1. *)
      (* If one thinks of c1 and c2 as bit-vector sets, this is the subset relationship. *)
      fun sub (c1, c2) = let
        fun s ([], []) = true
	  | s (One::t1, One::t2) = s (t1, t2)
	  | s (Zero::t1,  _::t2) = s (t1, t2)
	  | s _ =  raise Fail "bug" (* unequal length lists screened out below*)
        in
	  if (length c1) <> (length c2) then
	    raise Fail "UnequalLengths"
	  else s (c1, c2)
        end
      fun toString cb = concat (map (fn Zero => "0" | One => "1") cb)
      fun compare (c1, c2) = 
        if (length c1) <> (length c2) then
          raise Fail "Unequal Lengths"
	else String.compare (toString c1, toString c2)
      fun fromPPats ps = map (fn A.NDWildPat _ => Zero | _ => One) ps
      fun allOnes n = List.tabulate (n, fn _ => One) 
    end

    structure CB = CompletionBitstring
    type cbits = CB.t

    (* maps from cbits to 'a *)
    structure CBM = RedBlackMapFn (struct
	              type ord_key = cbits
		      val compare = CB.compare
	            end)

    type matchmap = (A.match list) CBM.map (* maps of cbits to matches (case arms) *)

    (* PCaseExp of (exp list * pmatch list * ty)       (* ty is result type *) *)   
    fun tr trExp (es, pms, t) = let

      val nExps = List.length es

      val esTy = A.TupleTy (map TypeOf.exp es)

      (* FIXME *)
      (* I need to make the plumbing for the trap datatype. *)
      fun mkValPat p = raise Fail "todo: mkValPat"
      fun mkExnPat p = raise Fail "todo: mkExnPat"

      (* xfromPPats : ppat list -> pat *)
      (* A function to transform ppat lists to tuple pats for use in case exps.*)
      (* e.g. the pcase branch *)
      (*   ? & 1 & (x,y) *)
      (* becomes *)
      (*   (_, Val(1), Val(x,y)) *)
      fun xformPPats ps = let
	    fun x ([], acc) = rev acc
	      | x (A.NDWildPat ty :: t, acc) = x (t, A.WildPat ty :: acc)
	      | x (A.HandlePat p :: t, acc) = x (t, mkExnPat p :: acc)
	      | x (A.Pat p :: t, acc) = x (t, mkValPat p :: acc)
            in
	      A.TuplePat (x (ps, []))
            end

      (* mergeIntoMap : given a map, a key (a cbits), a match, and a bool, *)
      (*                merge the given match into that map. *)
      fun mergeIntoMap (m, k, match, belongsAtEnd) = let
	    val matches = CBM.find (m, k)
            in
	      case matches
	       of NONE => CBM.insert (m, k, [match])
		| SOME ms => CBM.insert (m, k, if belongsAtEnd
					       then ms @ [match]
					       else match :: ms)
            end

      (* build a map of each completion bitstring to its possible matches *)
      fun buildMap ([A.Otherwise e], m) : matchmap = let
	    val match = A.PatMatch (A.WildPat esTy, e)
            in
	      mergeIntoMap (m, CB.allOnes nExps, match, true)
            end
	| buildMap ([A.PMatch _], _) = raise Fail "ill-formed pcase: pmatch is last"
	| buildMap (A.PMatch (ps, e) :: t, m) = let
	    val key = CB.fromPPats ps
	    val match = A.PatMatch (xformPPats ps, e)
	    val m' = mergeIntoMap (m, key, match, false)
	    in 
	      buildMap (t, m')
	    end
	| buildMap (A.Otherwise _ :: _, _) = raise Fail "ill-formed pcase: otherwise is not last"
	| buildMap ([], _) = raise Fail "bug: shouldn't empty out pcase (it might have been empty to start with)"

      (* A function to build a batch of functions (a state machine in another form) *)
      (* given a map of completion bitstrings to match lists. *)
      fun buildFuns (m : matchmap) : A.lambda list = let
	    val kmss = CBM.listItemsi m
	    fun mkGo matches = raise Fail "todo" (* make go() out of the top-level matches *)
	    fun b ([], matches, lams) = (mkGo matches) :: lams
	      | b ((cb,ms)::t, matches, lams) = let
		  val m = raise Fail "todo" (* make a match out of the cb *)
		  val f = raise Fail "todo" (* make a lam out of the ms *)
                  in
		    b (t, m::matches, f::lams)
                  end
            in
	      raise Fail "todo"
            end

      in
        raise Fail "todo"
      end

  end
