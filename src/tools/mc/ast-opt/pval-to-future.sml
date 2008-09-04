(* pval-to-future.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 * 
 * This module rewrites parallel bindings in terms of futures, touches and cancels.
 *)

structure Future1 =
  struct

    structure A = AST
    structure T = Types

    local
	fun getVar (ModuleEnv.Var v) = v
	  | getVar _ = raise Fail "getVar"
	val getVar = getVar o BasisEnv.getValFromBasis
	fun getTyc (ModuleEnv.TyCon tyc) = tyc
	  | getTyc _ = raise Fail "getTyc"
	val getTyc = getTyc o BasisEnv.getTyFromBasis
	fun future1Tyc () = getTyc ["Future1", "future"]
	fun future1Ty t = T.ConTy ([t], future1Tyc())
	fun future1 () = getVar ["Future1", "future"]
	fun touch1 () = getVar ["Future1", "touch"]
	fun cancel1 () = getVar ["Future1", "cancel"]

	(* mkThunk : A.exp -> A.exp *)
	(* Consumes e; produces (fn u => e) (for fresh u : unit). *)
	fun mkThunk e = A.FunExp (Var.new ("_", Basis.unitTy), e, TypeOf.exp e)		    

	(* isFuture : A.exp -> bool *)
	fun isFuture e = (case TypeOf.exp e
			    of T.ConTy (_, c) => TyCon.same (c, future1Tyc())
			     | _ => false)

        (* typeOfFuture : A.exp -> T.ty *)
        (* Precondition: The argument must be a future. *)
        (* The function raises Fail if the precondition is not met. *)
        (* ex: typeOfFuture (future (fn () => 8))     ==> int *)
        (* ex: typeOfFuture (future (fn () => 8 > 8)) ==> bool *)
	fun typeOfFuture e =
	    let val t = TypeOf.exp e
		fun mkMsg t = ("typeOfFuture: expected future type, got "
			       ^ (PrintTypes.toString t))
	    in
		case t
		  of T.ConTy ([t'], c) => if TyCon.same (c, future1Tyc()) 
					  then t'
					  else raise Fail (mkMsg t')
		   | _ => raise Fail (mkMsg t)
	    end

        (* mkTch : var -> A.exp -> A.exp *)
	fun mkTch touchvar e =
	    if (isFuture e) then
		let val t = typeOfFuture e
		    val touch = A.VarExp (touchvar(), [t])
		in
		    A.ApplyExp (touch, e, t)
		end
	    else
		let val ts = Var.toString (touchvar())
		in 
		    raise Fail (ts ^ ": argument is not a future")
		end

	(* mkCan : var -> A.exp -> A.exp *)
	fun mkCan cancelvar e =
	    if (isFuture e) then
		let val cancel = A.VarExp (cancelvar(), [typeOfFuture e])
		in
		    A.ApplyExp (cancel, e, Basis.unitTy)
		end
	    else
		let val cs = Var.toString (cancelvar())
		in
		    raise Fail (cs ^ ": argument is not a future")
		end

	(* mkFut : var -> A.exp -> A.exp *)
	(* Consumes futvar -> q and e; produces futvar (q, fn u => e). *)
	fun mkFut futvar e = 
	    let val te = TypeOf.exp e
	    in
		A.ApplyExp (A.VarExp (futvar(), [te]),
			    mkThunk e,
			    future1Ty te)
	    end
    in

    (* mkTouch1 : A.exp -> A.exp * A.exp *)
    (* Precondition: The argument must be a future. *)
    (* The function raises Fail if the precondition is not met. *)
    val mkFuture1Touch = mkTch touch1

    (* mkCancel1 : A.exp -> A.exp * A.exp *)
    (* Precondition: The argument e1 must be a future. *)
    (* The function raises Fail if the precondition is not met. *)
    val mkFuture1Cancel = mkCan cancel1

    (* mkFuture1 : A.exp * A.exp -> A.exp *)
    val mkFuture1Spawn = mkFut future1

    end (* local *)

  end (* Futures *)

structure PValToFuture =
  struct

    structure A = AST
    structure T = Types
    structure F = Future1
    structure VSet = Var.Set

  (* For those variables that are bound with a pval and embedded in a pattern,
   * we will maintain a list of selector functions to extract those variables
   * from their patters at use sites. See PValBind below for more details. *)
    val selectors : A.exp Var.Tbl.hash_table = Var.Tbl.mkTable (128, Fail "not found")

    (* grandUnion : VSet.set list -> VSet.set *)
    (* Computes the union of all the sets in the given list. *)
    val grandUnion = List.foldl VSet.union VSet.empty

    (* minus : VSet.set * Var.var -> VSet.set *)
    (* Unlike VSet.delete, this doesn't throw an exception if the elt is absent. *)
    fun minus (s, x) = VSet.difference (s, VSet.singleton x)

    (* plus : VSet.set * Var.var -> VSet.set *)
    fun plus (s, x) = VSet.union (s, VSet.singleton x)

    (* varsInPat : A.pat -> VSet.set *)
    val varsInPat =
	let (* vs : A.pat -> A.var list *)
	    fun vs (A.ConPat (_, _, p)) = vs p
	      | vs (A.TuplePat ps) = List.concat (map vs ps)
	      | vs (A.VarPat x) = [x]
	      | vs (A.WildPat t) = []
	      | vs (A.ConstPat _) = []
	in
	    VSet.fromList o vs
	end

    (* trExp : A.exp * VSet.set -> A.exp * VSet.set *)
    (* Consumes an expression and the set of pval-bound variables *)
    (*   live in the expression. *)
    (* Produces a rewritten-with-futures-etc. expression and the set *)
    (*   of pval-bound variables live therein. *)
    fun trExp (exp, pLive) = (
	  case exp
	   of A.LetExp (b, e) => trLet (b, e, pLive)
	    | A.IfExp (e1, e2, e3, t) => ifExp (e1, e2, e3, t, pLive)
	    | A.CaseExp (e, ms, t) => caseExp (e, ms, t, pLive)
	    | A.HandleExp (e, ms, t) => raise Fail "HandleExp"
	    | A.RaiseExp (e, t) =>  
	      let val (e', pLive') = trExp (e, pLive)
	      in
		  (A.RaiseExp (e', t), pLive')
	      end
	    | A.FunExp (x, e, t) => 
	      let val (e', pLive') = trExp (e, pLive)
	      in
		  (A.FunExp (x, e', t), pLive')
	      end
	    | A.ApplyExp (e1, e2, t) =>  
	      let val (e1', live1) = trExp (e1, pLive)
		  val (e2', live2) = trExp (e2, pLive)
		  val pLive' = VSet.union (live1, live2)
	      in
		  (A.ApplyExp (e1', e2', t), pLive')
	      end
	    | m as A.VarArityOpExp _ =>  (m, pLive)
	    | A.TupleExp es => 
	      let val ess = map (fn e => trExp (e, pLive)) es
		  val (es', ss) = ListPair.unzip ess
	      in
		  (A.TupleExp es', grandUnion ss)
	      end
	    | A.RangeExp (e1, e2, oe3, t) =>  
	      let val (e1', live1) = trExp (e1, pLive)
		  val (e2', live2) = trExp (e2, pLive)
	      in
		  case oe3
		   of NONE => (A.RangeExp (e1', e2', NONE, t),
			       VSet.union (live1, live2))
		    | SOME e3 => 
		      let val (e3', live3) = trExp (e3, pLive)
		      in
			  (A.RangeExp (e1', e2', SOME e3', t),
			   grandUnion [live1, live2, live3])
		      end
	      end
	    | A.PTupleExp es =>  
	      let val (es', pLive') = trExps (es, pLive)
	      in
		  (A.PTupleExp es', pLive')
	      end
	    | A.PArrayExp (es, t) =>  
	      let val (es', pLive') = trExps (es, pLive)
	      in
		  (A.PArrayExp (es', t), pLive')
	      end
	    | A.PCompExp (e, pes, oe) =>  
	      let val (e', pLive') = trExp (e, pLive)
		  val (ps, es) = ListPair.unzip pes
		  val (es', pLive'') = trExps (es, pLive)
		  val pes' = ListPair.zip (ps, es')
	      in
		  case oe
	           of NONE => (A.PCompExp (e', pes', NONE),
			       VSet.union (pLive', pLive''))
		    | SOME pred => raise Fail "PComp"
	      end
	    | A.PChoiceExp (es, t) =>  
	      let val (es', pLive') = trExps (es, pLive)
	      in
		  (A.PChoiceExp (es', t), pLive')
	      end
	    | A.SpawnExp e => 
	      let val (e', pLive') = trExp (e, pLive)
	      in
		  (A.SpawnExp e', pLive')
	      end
	    | k as A.ConstExp _ => (k, VSet.empty)
	    | v as A.VarExp (x, ts) =>  
	      if VSet.member (pLive, x) 
	      then 
		  let val touchV = F.mkFuture1Touch v
		      val optSel = Var.Tbl.find selectors x
		  in
		      (case optSel
			of NONE => (touchV, VSet.singleton x)
			 | SOME (s as A.FunExp (_, _, resTy)) => 
			   (A.ApplyExp (s, touchV, resTy), VSet.singleton x)
			 | SOME _ => raise Fail "compiler bug" (* shouldn't happen *)
		      (* end case *))
		  end
	      else (v, VSet.empty)
	    | A.SeqExp (e1, e2) =>  
	      let val (e1', live1) = trExp (e1, pLive)
		  val (e2', live2) = trExp (e2, pLive)
	      in
		  (A.SeqExp (e1', e2'), VSet.union (live1, live2))
	      end
	    | ov as A.OverloadExp _ =>  (ov, pLive)
	    | A.ExpansionOptsExp (opts, e) =>
	      let val (e', pLive') = ExpansionOpts.withExpansionOpts(fn () => trExp(e, pLive), opts)
	      in 
		  (A.ExpansionOptsExp (opts, e'), pLive')
	      end
	    | _ => raise Fail ""
        (* end case *))

    (* trExps: A.exp list * VSet.set -> A.exp list * VSet.set *)
    and trExps (es, pLive) =
	let val ess = map (fn e => trExp (e, pLive)) es
	    val (es', ss) = ListPair.unzip ess
	in
	    (es', grandUnion ss)
	end

    and trLet (b, e2, pLive) = (
	  case b
	   of A.ValBind (p, e1) => 
	      let val pvs = varsInPat p
		      val (e1', plive1) = trExp (e1, pLive)
		      val (e2', plive2) = trExp (e2, pLive)
		      val pliveOut = VSet.union (plive1, VSet.difference (plive2, pvs))
		      val b' = A.ValBind (p, e1')
	      in
		  (A.LetExp (b', e2'), pliveOut)
	      end
	    | A.PValBind (p, e1) => 
	      if not(ExpansionOpts.isEnabled(ExpansionOpts.PVAL[ExpansionOpts.FUTURES_WITH_GANG_SCHEDULING]))
	      then 
		  let val pvs = varsInPat p
		      val (e1', plive1) = trExp (e1, pLive)
		      val (e2', plive2) = trExp (e2, pLive)
		      val pliveOut = VSet.union (plive1, VSet.difference (plive2, pvs))
		      val b' = A.PValBind (p, e1')
		  in
		      (A.LetExp (b', e2'), pliveOut)
		  end
	      else						  
	      (case p
		of A.ConPat (c, ts, p) => raise Fail "letExp | PValBind | ConPat"
                       (*
                           let pval Foo(x,y) = f(z)
                           in
                               if somebool then
                                 19
                               else
                                 min(x,y)
                           end
                         -->
                           let val tf = future (fn () => f(z))
                           in
                               if somebool then
                                 (cancel tf; 19)
                               else
                                 (min(#1(touch(tf)),#2(touch(tf))))                       
                           end
                        *)
		 | A.TuplePat ps => raise Fail "letExp | PValBind | TuplePat"
		         (* let fun collectVars [] = []
			       | collectVars (p::ps) = 
				   (case p
				      of A.VarPat x => x :: (collectVars ps)
				       | A.WildPat _ => collectVars ps
				       | A.ConstPat _ => collectVars ps
				       | A.ConPat _ => raise Fail "nested tuple pats in pvals not yet supported"
				       | A.TuplePat _ => raise Fail "nested tuple pats in pvals not yet supported"
				   (* end case *))
			     fun oneBasedPosition x =
				 let fun pos (n, []) = raise Fail "not found"
				       | pos (n, A.VarPat(y)::ys) = 
					   if Var.same(x,y) then n else pos (n+1, ys)
				       | pos (n, _::ys) = pos (n+1, ys)
				 in
				     pos (1, ps)
				 end
			     fun recordSelector v =
				 let val pos = oneBasedPosition v
				     val h = Hash.mkHash (pos, map TypeOf.pat ps)
				 in
				     Var.Tbl.insert selectors (v, h)
				 end
			     val vs = collectVars ps
			     val (e1', live1) = trExp (e1, pLive)
			     val e1f = F.mkFuture e1'
			     val tf = Var.new ("tf", TypeOf.exp e1f)
			     (* rewrite all vars in the tuple pat to tf *)
			     (* FIXME This is too lossy! *)
			     val e2' = List.foldl (fn (v, e) => VarSubst.subst1(v,tf,e)) e2 vs
			     val _ = List.app recordSelector vs
			     val (e2t, live2) = trExp (e2', plus (pLive, tf))
			     val pLive' = 

				 let val s = grandUnion [VSet.singleton tf, live1, live2]
				 in
				     List.foldl (fn (v, s) => minus (s, v)) s vs
				 end
			 in
			     (A.LetExp (A.ValBind (A.VarPat tf, e1f), e2t), pLive')
			    end *)
		 | A.VarPat x =>
		   let val (e1', live1) = trExp (e1, pLive)
		       val e1f = F.mkFuture1Spawn e1'
		       val xf  = Var.new (Var.nameOf x ^ "f", TypeOf.exp e1f)
		       val e2' = VarSubst.subst1 (x, xf, e2)
		       val (e2t, live2) = trExp (e2', plus (pLive, xf))
		       val pLive' = 
			   let val s = grandUnion [VSet.singleton xf, live1, live2]
			   in
			       minus (s, x)
			   end
		   in
		       (A.LetExp (A.ValBind (A.VarPat xf, e1f), e2t), pLive')
		   end
		 | A.WildPat t =>
                   (* This is an odd case:
                            let pval _ = f(x) in y end
                          Note any such application of f is only being evaluated for its
                          effect. We have a choice: package it as a future and 
                          evaluate it at some undetermined point in time, or
                          evaluate it at the binding site and keep going.
                          For the time being, let's take the latter, more conservative
                          strategy, which should save the programmer from self-foot-shooting.
                        *)
                   (A.LetExp (A.ValBind (p, e1), e2), pLive)
 		 | A.ConstPat k => 
                   (* Similar to WildPat case. *)
                   (A.LetExp (A.ValBind (p, e1), e2), pLive)
	      (* end case *))
	    | A.FunBind lams => 
	      let val lams' = List.map (lambda pLive) lams
		  val (e2', live1) = trExp (e2, pLive)
	      in
		  (A.LetExp (A.FunBind lams', e2'), live1)
	      end
	    | A.PrimVBind (v, rhs) => 
	      let val (e2', pLive') = trExp (e2, pLive)
	      in
		  (A.LetExp (A.PrimVBind(v, rhs), e2'), pLive')
	      end
	    | A.PrimCodeBind code =>
	      let val (e2', pLive') = trExp (e2, pLive)
	      in
		  (A.LetExp (A.PrimCodeBind code, e2'), pLive')
	      end
    (* end case *))
			      
    (* lambda : VSet.set -> A.lambda -> A.lambda *)
    and lambda pLive (A.FB (f, x, e)) = A.FB (f, x, #1 (trExp (e, pLive)))
						
    (* cancel : A.var * A.exp -> A.exp *)
    and cancel (x, e) = A.SeqExp (F.mkFuture1Cancel (A.VarExp (x, [])), e)
    (*                                 ^^               *)
    (* ??? Is it OK not to instantiate the 'a future? ??? *)

    (* ifExp : A.exp * A.exp * A.exp * T.ty * VSet.set -> A.exp * VSet.set *)
    and ifExp (e1, e2, e3, t, pLive) = 
	let val (e1', pLiveC) = trExp (e1, pLive)
	    val (e2', pLiveT) = trExp (e2, pLive)
	    val (e3', pLiveF) = trExp (e3, pLive)
	    (* compute the variables that need to be cancelled *)
	    val canT = VSet.difference (VSet.intersection (pLive, pLiveF), pLiveT)
	    val canF = VSet.difference (VSet.intersection (pLive, pLiveT), pLiveF)
	    val e2' = VSet.foldl cancel e2' canT
	    val e3' = VSet.foldl cancel e3' canF
	    val pLive' = grandUnion [pLiveC, pLiveT, pLiveF]
        in
	    (A.IfExp (e1', e2', e3', t), pLive')
        end

    (* caseExp : A.exp * A.match list * T.ty * VSet.set -> A.exp * VSet.set *)
    and caseExp (e, ms, t, pLive) = 
	let val (e', pLive') = trExp (e, pLive)
	    val mvs = map (fn m => match (m, pLive)) ms
	    val (ms', pLiveHd::pLiveTl) = ListPair.unzip mvs
            fun loop ([], _, _, matchAcc, setAcc) = (rev matchAcc, setAcc)
	      | loop (m::ms, currLive, others, matchAcc, setAcc) = let
		    val othersLive = grandUnion others
		    val cancelUs = VSet.difference (VSet.intersection (pLive, othersLive), currLive)
		in
		    case m
		     of A.PatMatch (p, e) => let
                            val e' = VSet.foldl cancel e cancelUs
			    val m' = A.PatMatch (p, e')
			    val mAcc = m'::matchAcc
			    val sAcc = VSet.union (setAcc, currLive)
                        in
			    case others
			     of [] => (rev mAcc, sAcc)
			      | h::t => loop (ms, h, t @ [currLive], mAcc, sAcc)
			end
		      | A.CondMatch (p, e1, e2) => raise Fail "todo"
                end
	    val (ms'', pLive'') = loop (ms', pLiveHd, pLiveTl, [], VSet.empty)
	in
	    (A.CaseExp (e', ms'', t), VSet.union (pLive', pLive''))
        end
 
    (* match : A.match * VSet.set -> A.match * VSet.set *)
    and match (A.PatMatch (p, e), pLive) = 
	let val (e', pLive') = trExp (e, pLive)
	in
	    (A.PatMatch (p, e'), pLive')
        end
      | match (A.CondMatch (p, e1, e2), pLive) = 
	let val (e1', pLive1) = trExp (e1, pLive)
	    val (e2', pLive2) = trExp (e2, pLive)
        in
	    (A.CondMatch (p, e1', e2'), VSet.union (pLive1, pLive2))
	end

    fun tr exp = (
	Var.Tbl.clear selectors;
	#1(trExp(exp, VSet.empty)))

  end
