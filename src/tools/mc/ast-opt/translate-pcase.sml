(* translate-pcase.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

(* NOTE This translation is NOT RECURSIVE. *)
(*      It assumes the incoming expressions have been translated. *)

(* TODO Test exceptions. *)

(* TODO The compiler should check to make sure Otherwise is last. Currently, it does so in this module. *)

(* TODO Somewhere the compiler should check for multiple Otherwises in pcases. *)

(* TODO Somewhere the compiler should append an "Otherwise => raise ..." to any pcase without one. *)

(* TODO For 'a computations, should we make an 'a trap future, rather than an 'a future? *)

structure TranslatePCase (* : sig

  (* An AST to AST translation of parallel cases. *)
    val tr : (AST.exp -> AST.exp) 
             -> AST.exp list * AST.pmatch list * AST.ty 
             -> AST.exp

  end *) = struct

    structure A = AST
    structure B = Basis
    structure F = Future1
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

      local
        (* xformPPats : ppat list -> pat *)
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
        (* cbOf : A.pmatch -> cbits *)
        fun cbOf (A.PMatch (pps, _)) = CB.fromPPats pps
	  | cbOf (A.Otherwise _) = CB.allOnes nExps
        (* initMap : cbits list -> matchmap *)
        fun initMap cbs : matchmap = let
          fun i ([], m) = m
	    | i (c::cs, m) = 
	        if CBM.inDomain (m, c)
		then i (cs, m)
		else i (cs, CBM.insert (m, c, []))
          in
            i (cbs, CBM.empty)
          end
	(* mergeOne : given a map, a key (a cbits), and a pmatch, *)
	(*            merge the corresponding match into that map. *)
	fun mergeOne (m, cb, pm) = let
	  val belongsLast = (case pm of A.Otherwise _ => true | _ => false)
	  val match = 
           (case pm
	     of A.Otherwise e => A.PatMatch (A.WildPat esTupTy, e)
	      | A.PMatch (ps, e) => A.PatMatch (xformPPats ps, e)
	    (* end case *))
	  val (m', ms) = CBM.remove (m, cb)
          in
	    CBM.insert (m', cb, if belongsLast
				then ms @ [match]
				else match :: ms)
	  end
        (* merge : A.pmatch * matchmap -> matchmap *)
        fun merge (pm, m) = let
          val cb = cbOf pm
	  fun loop ([], m') = m'
	    | loop (cb'::t, m') = loop (t, if CB.sub(cb,cb')
					   then mergeOne (m', cb', pm)
					   else m')
	  in
	    loop (CBM.listKeys m, m)
	  end
      in
      (* buildMap : A.pmatch list -> A.matchmap *)
      (* build map : collect bitstrings, then merge branches in *)
      fun buildMap pms = let
        val cbs = map cbOf pms
        in
	  List.foldl merge (initMap cbs) pms
        end
      end (* local *)

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
		      | _ => raise Fail "unexpected non-future type")
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

      (* mkStateMachine : matchmap -> exp *)
      (* A function to build a batch of functions implementing a state machine *)
      (* given a map of completion bitstrings -> lists of matches. *)
      fun mkStateMachine (m : matchmap) : A.exp = let
	    val kmss = CBM.listItemsi m
	    val goV = Var.new ("go", A.FunTy (Basis.unitTy, pcaseResultTy))
	    val applyGo = A.ApplyExp (A.VarExp (goV, []), A.TupleExp [], pcaseResultTy)
            val default = A.WildPat (A.TupleTy (map (Basis.optionTy o trapTy) eTys))
	    val defaultMatch = A.PatMatch (default, applyGo)
	    val u = Var.new ("u", Basis.unitTy)
	    val fVs = futureVars es
	    fun mkGo matches = let
              val pollTuple = A.TupleExp (map mkPoll fVs)
	      val body = A.CaseExp (pollTuple, matches @ [defaultMatch], pcaseResultTy)
	      val arg = Var.new ("u", Basis.unitTy) 
              in
		A.FB (goV, arg, body)
              end
  	    fun isAllOnes cb = CB.eq (cb, CB.allOnes (CB.length cb))
	    fun b ([], matches, lams, fnames) = mkGo matches :: lams
	      | b ((cb,ms)::t, matches, lams, fnames) = let
                  val name = "state" ^ CB.toString cb
		  val ty = A.FunTy (mkTy cb, pcaseResultTy)
		  val nameV = Var.new (name, ty)
		  val (m, vs) = mkMatch (cb, nameV)
		  val f = mkLam (nameV, vs, if isAllOnes cb
					    then ms else 
					    ms @ [defaultMatch])
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
	mkStateMachine (buildMap pms)
      end

  (* --- some tests follow --- *)

(*
FIXME: enabling these tests results in an exception being raised from the basis environment module. this
happens because the test tries to load operations over futures before the basis libraries are available.

  structure T = TestUtils

  val zero = T.int 0
  val one = T.int 1

  fun t (A.PCaseExp (es, pms, ty)) = tr (fn e => e) (es, pms, ty)
    | t _ = raise Fail "expecting a pcase"

  val c0 = A.PCaseExp ([zero],
		       [A.Otherwise one],
		       Basis.intTy)

  val c1 = A.PCaseExp ([zero, zero],
		       [A.PMatch ([T.intPPat 1, A.NDWildPat Basis.intTy], zero),
 		        A.Otherwise one],
		       Basis.intTy)

  val c2 = let
    val n = Var.new ("n", Basis.intTy)
    val m = Var.new ("m", Basis.intTy)
    in
      A.PCaseExp ([A.VarExp (n, []), A.VarExp (m, [])],
		  [A.PMatch ([T.intPPat 1, A.NDWildPat Basis.intTy], zero),
		   A.PMatch ([A.NDWildPat Basis.intTy, T.intPPat 2], zero),
		   A.Otherwise one],
		  Basis.intTy)
    end

  val c3 = A.PCaseExp ([zero],
		       [A.Otherwise (t c0)],
		       Basis.intTy)

  fun mkTest pcase = (PrintAST.printExpNoTypes pcase;
		      PrintAST.printComment "-->";
		      PrintAST.printExpNoTypes (t pcase))

  fun test 0 = mkTest c0
    | test 1 = mkTest c1
    | test 2 = mkTest c2
    | test 3 = mkTest c3
    | test _ = print "No such test.\n"
*)
end
    

