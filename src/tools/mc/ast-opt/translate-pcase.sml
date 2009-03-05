(* translate-pcase.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

(* FIXME : poll is not implemented! *)

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

  structure CB = CompletionBitstring
  type cbits = CB.t

(* maps from cbits to 'a *)
  structure CBM = RedBlackMapFn (struct
				   type ord_key = cbits
				   val compare = CB.compare
				 end)

  type matchmap = (A.match list) CBM.map (* maps of cbits to matches (case arms) *)

  (* NOTE: making the Val, Exn dcons etc. at link time causes a link time error. *)
  (* I'm doing all this memo stuff to work around that. - ams *)

  type 'a memo = 'a option ref
		 
  fun getMemo (get : unit -> 'a) (m : 'a memo) : 'a =
      (case !m
        of NONE => let
               val x = get ()
               val _ = (m := SOME x)
           in
	       x
	   end
	 | SOME x => x)
      
  local

    val memoSOME : AST.dcon memo = ref NONE
    val memoNONE : AST.dcon memo = ref NONE 
    val memoVal  : AST.dcon memo = ref NONE
    val memoExn  : AST.dcon memo = ref NONE
				 
    val memoFuture : Types.tycon memo = ref NONE
    val memoOption : Types.tycon memo = ref NONE
    val memoTrap   : Types.tycon memo = ref NONE
				
  in
      
  (* optSOME : unit -> A.dcon *)
  (* memoized SOME dcon from the basis *)
    fun optSOME () = let
      fun get _ = BasisEnv.getDConFromBasis ["Option", "SOME"]
      in
	getMemo get memoSOME
      end
	    
  (* optNONE : unit -> A.dcon *)
  (* memoized NONE dcon from the basis *)
    fun optNONE () = let
      fun get _ = BasisEnv.getDConFromBasis ["Option", "NONE"]
      in
        getMemo get memoNONE
      end

  (* trapVal : unit -> A.dcon *)
    fun trapVal () = let
      fun get _ = BasisEnv.getDConFromBasis ["Trap", "Val"]
      in 
        getMemo get memoVal
      end

  (* trapExn : unit -> A.dcon *)
    fun trapExn () = let
      fun get _ = BasisEnv.getDConFromBasis ["Trap", "Exn"]
      in
        getMemo get memoExn
      end

  (* futureTyc : unit -> AST.tycon *)
    fun futureTyc () = let
      fun get _ = BasisEnv.getTyConFromBasis ["Future1", "future"]
      in
        getMemo get memoFuture
      end

  (* optionTyc : unit -> AST.tycon *)
    fun optionTyc () = let
      fun get _ = BasisEnv.getTyConFromBasis ["Option", "option"]
      in
        getMemo get memoOption
      end

  (* trapTyc : unit -> AST.tycon *)
    fun trapTyc () = let
      fun get _ = BasisEnv.getTyConFromBasis ["Trap", "trap"]
      in
        getMemo get memoTrap
      end

  (* mkValPat : AST.pat -> AST.pat *)
  (* Given a pattern (p : t), produce the pattern (Val p : t trap). *)
    fun mkValPat pat = let
      val conVal = trapVal ()
      val ty = TypeOf.pat pat
      in
        AST.ConPat (conVal, [ty], pat)
      end

  (* mkExnPat : (AST.pat * AST.ty) -> AST.pat *)
  (* Given a pattern p and type t, produce the pattern (Exn p : t trap). *)  
    fun mkExnPat (pat, ty) = let
      val conExn = trapExn ()
      in
        AST.ConPat (conExn, [ty], pat)
      end

  (* mkOptTy : AST.ty -> AST.ty *)
  (* Given type t, make type (t option). *)
    fun mkOptTy ty = AST.ConTy ([ty], optionTyc ())

  (* mkFutureTy : AST.ty -> AST.ty *)
    fun mkFutureTy ty = AST.ConTy ([ty], futureTyc ())

  (* mkTrapTy : AST.ty -> AST.ty *)
    fun mkTrapTy ty = AST.ConTy ([ty], trapTyc ())

  end (* local *)

(* futureVars : A.exp list -> A.var list *)
  fun futureVars es = let
    fun go ([], _, acc) = rev acc
      | go (e::es, n, acc) = let
	  val ty = TypeOf.exp e
	  val name = "f" ^ Int.toString n
          val fV = Var.new (name, mkFutureTy ty)
	  in
	    go (es, n+1, fV::acc)
  	  end
    in
      go (es, 1, [])
    end

(* xformPPats : AST.ppat list -> AST.pat *)
(* A function to transform ppat lists to tuple pats for use in case exps. *)
(* ex: ? & 1 & (x, y) --> (_, Val 1, Val (x, y)) *)
  fun xformPPats ps = let
    fun xform (AST.NDWildPat ty) = AST.WildPat ty
      | xform (AST.HandlePat (p, ty)) = mkExnPat (p, ty)
      | xform (AST.Pat p) = mkValPat p
    in
      AST.TuplePat (map xform ps)
    end

(* initMap : cbits list -> matchmap *)
  fun initMap (cbs : cbits list) : matchmap = let
    fun go ([], m) = m
      | go (c::cs, m) = 
	if CBM.inDomain (m, c) then 
	  go (cs, m)
	else 
	  go (cs, CBM.insert (m, c, []))
    in
      go (cbs, CBM.empty)
    end

(* cbOf : int -> A.pmatch -> cbits *)
  fun cbOf _ (A.PMatch (pps, _)) = CB.fromPPats pps
    | cbOf n (A.Otherwise _) = CB.allOnes n

(* mergeOne : given a map, a key (a cbits), and a pmatch,
 * merge the corresponding match into that map. *)
  fun mergeOne (t : AST.ty) (m : matchmap, cb : cbits, pm : AST.pmatch) 
      : matchmap = let
    val match = 
     (case pm
        of A.Otherwise e => A.PatMatch (A.WildPat t, e)
	 | A.PMatch (ps, e) => A.PatMatch (xformPPats ps, e))
    val (m', ms) = CBM.remove (m, cb)
    val belongsLast = (case pm of A.Otherwise _ => true | _ => false)
    val ms' = if belongsLast then ms @ [match] else match :: ms
    in
      CBM.insert (m', cb, ms')
    end

(* merge : int -> AST.ty -> A.pmatch * matchmap -> matchmap *)
  fun merge (n : int) (t : A.ty) (pm : A.pmatch, m : matchmap) 
      : matchmap = let
    val cb = cbOf n pm
    fun go ([], m') = m'
      | go (cb' :: r, m') = let
          val m'' = if CB.sub (cb, cb') then mergeOne t (m', cb', pm) else m'
          in
	    go (r, m'')
          end
    in
      go (CBM.listKeys m, m)
    end

(* mkTy : AST.ty list -> cbits -> AST.ty *)
(* tup of traps for types corres. to Ones -> pcaseResultTy *)
(* e.g., if ts is [int, bool, string] *)
(* and cb is 101 then this function yields *)
(* the type (int trap * string trap) *)
  fun mkTy ts cb = let
    fun go ([], [], tys) = AST.TupleTy (rev tys)
      | go (CB.Zero :: r1, _ :: r2, tys) = go (r1, r2, tys)
      | go (CB.One  :: r1, t :: r2, tys) = go (r1, r2, mkTrapTy t :: tys)
      | go _ = raise Fail
          "length of completion bitstring doesn't match # of exps in pcase"
    in
      go (cb, ts, [])
    end 

(* mkMatch: Given a completion bitstring and a function name,   *)
(* construct a match with the right SOMEs, NONEs etc.  *)
(* ex: mkMatch(1001, state1001) yields                          *)
(*       | (SOME(t1), NONE, NONE, SOME(t2)) => state1001(t1,t2) *)
(* mkMatch : cbits * A.var -> A.match * (A.var list)            *)
  fun mkMatch (pcaseResultTy, eTys, cb, fV) = let
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
          val p = A.ConstPat (A.DConst (optNONE (), [mkTrapTy t]))
          in
            m (bs, ts, n, p::optPats, argVs)
          end
      | m (CB.One::bs, t::ts, n, optPats, argVs) = let
          val varName = "t" ^ Int.toString n
	  val argV = Var.new ("t" ^ Int.toString n, mkTrapTy t)
	  val p = A.ConPat (optSOME (), [mkTrapTy t], A.VarPat argV) 
          in
	    m (bs, ts, n+1, p::optPats, argV::argVs)
          end
      | m _ = raise Fail "ran out of types"
    in
      m (cb, eTys, 1, [], [])
    end

(* typeOfVar : A.var -> A.ty *)
  fun typeOfVar v = TypeOf.pat (A.VarPat v)

  local
    val memoPoll : AST.var memo = ref NONE
  in
  (* pollV : unit -> A.var *)
  (* FIXME: dummy implementation of poll *)
    fun pollV () = let
      fun forall mkTy = let
        val tv = TyVar.new (Atom.atom "'a")
        in
	  AST.TyScheme ([tv], mkTy (AST.VarTy tv))
	end
      fun get _ = let
        fun pollTy tv = A.FunTy (F.futureTy tv, mkOptTy (mkTrapTy tv))
        in
	  (* FIXME get the real poll from the basis *)
          Var.newPoly ("poll", forall (fn tv => pollTy tv))
        end
      in
	getMemo get memoPoll
      end
  end (* local *)
	
(* mkPoll: Given a variable f bound to some 'a future, return (poll f).
 * ex: mkPoll (nf : int future) --> 
 *  ((poll : int future -> int trap option) (nf : int future)) : int trap option 
 *)
  fun mkPoll (futV : AST.var) : AST.exp = let
    val ty = typeOfVar futV
    val inst = (case ty
		  of A.ConTy ([i], futureTyc) => i
		   | _ => raise Fail "unexpected non-future type")
    in
      A.ApplyExp (A.VarExp (pollV (), [inst]),
		  A.VarExp (futV, []), (* FIXME Should this be the empty list? *)
		  mkOptTy (mkTrapTy inst))
    end


(* mkLam : AST.ty * AST.var * AST.var list * match list -> A.lambda *)
(* This function is called to finish off the translation in tr below. *)
  fun mkLam (pcaseResultTy, fNameV, vars, caseArms) = let
    val argV = Var.new ("x", A.TupleTy (map typeOfVar vars))
    val body = A.CaseExp (A.VarExp (argV, []), caseArms, pcaseResultTy)
    in
      A.FB (fNameV, argV, body)
    end

(* mkStateMachine : AST.exp list * matchmap * AST.ty -> AST.exp *)
(* A function to build a batch of functions implementing a state machine *)
(* Given a map of completion bitstrings, produce lists of matches. *)
  fun mkStateMachine (es : AST.exp list,
		      m : matchmap,
		      pcaseResultTy : AST.ty) : AST.exp = let
    val expTys = List.map TypeOf.exp es
    val kmss = CBM.listItemsi m
    val goV = Var.new ("go", A.FunTy (Basis.unitTy, pcaseResultTy))
    val applyGo = A.ApplyExp (A.VarExp (goV, []), A.TupleExp [], pcaseResultTy)
    val default = A.WildPat (A.TupleTy (map (mkOptTy o mkTrapTy) expTys))
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
	  val ty = A.FunTy (mkTy expTys cb, pcaseResultTy)
	  val nameV = Var.new (name, ty)
	  val (m, vs) = mkMatch (pcaseResultTy, expTys, cb, nameV)
	  val f = mkLam (pcaseResultTy, nameV, vs, if isAllOnes cb
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

(* A pcase looks like this:
 * PCaseExp of (exp list * pmatch list * ty) (ty is result type ) 
 *)   
  fun tr trExp (es, pms, pcaseResultTy) = let
    (* buildMap : A.pmatch list -> A.matchmap *)
    (* build map : collect bitstrings, then merge branches in *)
      fun buildMap pms = let
        val nExps = List.length es
        val expTys = map TypeOf.exp es
        val cbs = map (cbOf nExps) pms
        in
	  List.foldl (merge nExps (AST.TupleTy expTys)) (initMap cbs) pms
        end
      in
	mkStateMachine (es, buildMap pms, pcaseResultTy)
      end

  (* --- some tests follow --- *)

(*
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

(*  link time error:
  val c3 = A.PCaseExp ([zero],
		       [A.Otherwise (t c0)],
		       Basis.intTy)
*)
  fun mkTest pcase = (PrintAST.printExpNoTypes pcase;
		      PrintAST.printComment "-->";
		      PrintAST.printExpNoTypes (t pcase))

  fun test 0 = mkTest c0
    | test 1 = mkTest c1
    | test 2 = mkTest c2
(*  | test 3 = mkTest c3 *)
    | test _ = print "No such test.\n"

*)

end
    

