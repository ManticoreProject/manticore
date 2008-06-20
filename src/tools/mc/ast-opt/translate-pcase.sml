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

    structure CB = CompletionBitstring
    type cbits = CB.t

    (* maps from cbits to 'a *)
    structure CBM = RedBlackMapFn (struct
	              type ord_key = cbits
		      val compare = CB.compare
	            end)

    type matchmap = (A.match list) CBM.map (* maps of cbits to matches (case arms) *)

    (* A pcase looks like this: *)
    (* PCaseExp of (exp list * pmatch list * ty)       (* ty is result type *) *)   
    fun tr trExp (es, pms, pcaseResultTy) = let

      val nExps = List.length es

      val eTys = map TypeOf.exp es

      val esTupTy = A.TupleTy eTys

      (* Given a pattern p : tau, produce the pattern Val(p) : tau trap. *)
      fun mkValPat p = A.ConPat (Basis.trapVal, [TypeOf.pat p], p)

      (* Given a pattern p and type tau, produce the pattern Exn(p) : tau trap. *)
      fun mkExnPat (p, ty) = A.ConPat (Basis.trapExn, [ty], p)

      (* xfromPPats : ppat list -> pat *)
      (* A function to transform ppat lists to tuple pats for use in case exps. *)
      (* e.g. the pcase branch *)
      (*   ? & 1 & (x,y) *)
      (* becomes *)
      (*   (_, Val(1), Val(x,y)) *)
      fun xformPPats ps = let
	    fun x ([], acc) = rev acc
	      | x (A.NDWildPat ty :: t, acc) = x (t, A.WildPat ty :: acc)
	      | x (A.HandlePat (p, ty) :: t, acc) = x (t, mkExnPat(p,ty) :: acc)
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
	    val match = A.PatMatch (A.WildPat esTupTy, e)
            in
	      mergeIntoMap (m, CB.allOnes nExps, match, true)
            end
	| buildMap ([A.PMatch _], _) = 
            raise Fail "ill-formed pcase: pmatch, rather than otherwise, is last"
	| buildMap (A.PMatch (ps, e) :: t, m) = let
	    val key = CB.fromPPats ps
	    val match = A.PatMatch (xformPPats ps, e)
	    val m' = mergeIntoMap (m, key, match, false)
	    in 
	      buildMap (t, m')
	    end
	| buildMap (A.Otherwise _ :: _, _) = 
            raise Fail "ill-formed pcase: otherwise is not last"
	| buildMap ([], _) = raise Fail "bug" (* shouldn't reach this *)

      (* optTy : A.ty -> A.ty *)
      fun optTy t = A.ConTy ([t], Basis.optionTyc)

      (* trapTy : A.ty -> A.ty *)
      fun trapTy t = A.ConTy ([t], Basis.trapTyc)

      (* tup of traps for types corres. to Ones -> pcaseResultTy *)
      (* e.g., if eTys (defined locally above) is [int, bool, string] *)
      (*       and cb is 101 then this function yields *)
      (*       the type (int trap * string trap) *)
      fun mkTy cb = let
        fun b ([], [], tys) = rev tys
	  | b (CB.Zero::t1, _::t2, tys) = b (t1, t2, tys)
	  | b (CB.One::t1, t::t2, tys) = b (t1, t2, trapTy t :: tys)
	  | b _ = raise Fail "length of completion bitstring doesn't match # of expressions in pcase"
        in
          A.TupleTy (b (cb, eTys, []))
        end 

      (* mkMatch: Given a completion bitstring and a function name,   *)
      (*          construct a match with the right SOMEs, NONEs etc.  *)
      (* ex: mkMatch(1001, state1001) yields                          *)
      (*       | (SOME(t1), NONE, NONE, SOME(t2)) => state1001(t1,t2) *)
      (* mkMatch : cbits * A.var -> A.match * (A.var list)            *)
      fun mkMatch (cb, fV) = let
	fun m ([], [], _, optPats, argVs) = let
              val tupPat = A.TuplePat (List.rev optPats)
	      val argVs' = List.rev argVs
              val fcall = A.ApplyExp (A.VarExp (fV, []), 
				      A.TupleExp (map (fn v => A.VarExp (v, [])) argVs'),
				      pcaseResultTy)
              in
	        (A.PatMatch (tupPat, fcall), argVs')
	      end
	  | m (CB.Zero::bs, t::ts, n, optPats, argVs) = let
              val p = A.ConstPat (A.DConst (Basis.optionNONE, [trapTy t]))
              in
                m (bs, ts, n, p::optPats, argVs)
              end
	  | m (CB.One::bs, t::ts, n, optPats, argVs) = let
              val varName = "t" ^ Int.toString n
	      val argV = Var.new ("t" ^ Int.toString n, trapTy t)
	      val p = A.ConPat (Basis.optionSOME, [trapTy t], A.VarPat argV) 
              in
		m (bs, ts, n+1, p::optPats, argV::argVs)
              end
	  | m _ = raise Fail "ran out of types"
        in
          m (cb, eTys, 1, [], [])
        end

      (* typeOfVar : A.var -> A.ty *)
      fun typeOfVar v = TypeOf.pat (A.VarPat v)

      val pollV = let
        fun pollTy tv = A.FunTy (F.futureTy tv, B.optionTy (trapTy tv))
        in
	  BasisUtils.polyVar ("poll", fn tv => pollTy tv)
        end
	
      (* mkPoll: Given a variable f bound to some 'a future, return poll(f). *)
      fun mkPoll fV = let
        (* FIXME: dummy implementation *)
        val ty = typeOfVar fV
	val inst = (case ty
		     of A.ConTy ([i], futureTyc) => i
		      | _ => raise Fail "bug")
        in
          A.ApplyExp (A.VarExp (pollV, [inst]),
		      A.VarExp (fV, []),
		      B.optionTy (trapTy inst))
        end

      (* mkLam: Given a function name, a list of variable expressions, and case arms, *)
      (*        cobble together the corresponding function. *)
      (* mkLam : A.var * (A.var list) * (A.match list) -> A.lambda *)
      fun mkLam (fName, vars, caseArms) = let
        val argV = Var.new ("x", A.TupleTy (map typeOfVar vars))
        val body = A.CaseExp (A.VarExp (argV, []), caseArms, pcaseResultTy)
        in
	  A.FB (fName, argV, body)
        end

      (* futureVars : A.exp list -> A.var list *)
      fun futureVars es = let
        fun loop ([], _, acc) = rev acc
	  | loop (e::es, n, acc) = let
              val fV = Var.new ("f" ^ Int.toString n,
				F.futureTy (TypeOf.exp e))

	      in
		loop (es, n+1, fV::acc)
	      end
        in
	  loop (es, 1, [])
        end

      (* mkStateMachine *)
      (* A function to build a batch of functions implementing a state machine *)
      (* given a map of completion bitstrings to match lists. *)
      fun mkStateMachine (m : matchmap) : A.exp = let
	    val kmss = CBM.listItemsi m
	    val goV = Var.new ("go", A.FunTy (Basis.unitTy, pcaseResultTy))
	    val applyGo = A.ApplyExp (A.VarExp (goV, []), A.TupleExp [], pcaseResultTy)
	    val u = Var.new ("u", Basis.unitTy)
	    val fVs = futureVars es
	    fun mkGo matches = let
              val pollTuple = A.TupleExp (map mkPoll fVs)
              fun mkNone ty = A.ConstPat (A.DConst (Basis.optionNONE, 
						    [Basis.optionTy (trapTy ty)]))
	      val nones = A.TuplePat (map mkNone eTys)
	      val loopMatch = A.PatMatch (nones, applyGo)
	      val body = A.CaseExp (pollTuple, matches @ [loopMatch], pcaseResultTy)
	      val arg = Var.new ("u", Basis.unitTy) 
              in
		A.FB (goV, arg, body)
              end
	    fun b ([], matches, lams, fnames) = mkGo matches :: lams
	      | b ((cb,ms)::t, matches, lams, fnames) = let
                  val name = "state" ^ CB.toString cb
		  val ty = A.FunTy (mkTy cb, pcaseResultTy)
		  val nameV = Var.new (name, ty)
		  val (m, vs) = mkMatch (cb, nameV)
		  val f = mkLam (nameV, vs, ms)
                  in
		    b (t, m::matches, f::lams, name::fnames)
                  end
	    val lams = b (kmss, [], [], []) 
	    val knot = A.FunBind lams           
	    fun bind (v, e, e') = A.LetExp (A.ValBind (A.VarPat v, F.mkFuture e), e')
        in
	  ListPair.foldrEq bind (A.LetExp (knot, applyGo)) (fVs, es)
        end
      in
	mkStateMachine (buildMap (pms, CBM.empty))
      end

  (* --- some tests follow --- *)

  structure T = TestUtils

  val zero = T.int 0
  val one = T.int 1

  val c0 = A.PCaseExp ([zero],
		       [A.Otherwise one],
		       Basis.intTy)

  val c1 = A.PCaseExp ([zero, zero],
		       [A.PMatch ([A.NDWildPat Basis.intTy, A.NDWildPat Basis.intTy], one),
 		        A.Otherwise one],
		       Basis.intTy)

  fun t (A.PCaseExp (es, pms, ty)) = tr (fn e => e) (es, pms, ty)
    | t _ = raise Fail "bug"

  fun mkTest pcase = (PrintAST.printExp pcase;
		      PrintAST.printComment "-->";
		      PrintAST.printExp (t pcase))

  fun test 0 = mkTest c0
    | test 1 = mkTest c1
    | test _ = print "No such test.\n"

end
    

