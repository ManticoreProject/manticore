(* fut-par-let.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 * 
 * This module rewrites parallel bindings in terms of futures, touches and cancels.
 *
 * Note this rewriting is type preserving.
 *)

structure FutParLet : sig

    (* val futurize : AST.module -> AST.module *)
    val futurizeExp : AST.exp -> AST.exp
    val test        : int -> unit

  end = 

  struct

    structure A = AST
    structure B = Basis
    structure F = Futures
    structure T = Types
    structure U = UnseenBasis

    (* fail : string -> 'a *)
    fun fail msg = raise Fail msg

    (* todo : string -> 'a *)
    fun todo thing = fail ("todo: " ^ thing)

    infixr **
    (* (**) : ('a -> 'b) * ('c -> 'd) -> ('a * 'c) -> ('b * 'd) *)
    fun f ** g = (fn (a, c) => (f a, g c))

    (* id : 'a -> 'a *)
    val id = fn x => x

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

    fun futurizeExp e =
	let (* exp : A.exp * VSet.set -> A.exp * VSet.set *)
	    (* Consumes an expression and the set of pval-bound variables *)
	    (*   live in the expression. *)
	    (* Produces a rewritten-with-futures-etc. expression and the set *)
	    (*   of pval-bound variables live therein. *)
	    fun exp (A.LetExp (b, e), pLive) = letExp (b, e, pLive)
	      | exp (A.IfExp (e1, e2, e3, t), pLive) = ifExp (e1, e2, e3, t, pLive)
	      | exp (A.CaseExp (e, ms, t), pLive) = caseExp (e, ms, t, pLive)
	      | exp (A.HandleExp (e, ms, t), pLive) = todo "HandleExp"
	      | exp (A.RaiseExp (e, t), pLive) = 
		  let val (e', pLive') = exp (e, pLive)
		  in
		      (A.RaiseExp (e', t), pLive')
		  end
	      | exp (A.FunExp (x, e, t), pLive) =
		  let val (e', pLive') = exp (e, pLive)
		  in
		      (A.FunExp (x, e', t), pLive')
		  end
	      | exp (A.ApplyExp (e1, e2, t), pLive) = 
		  let val (e1', live1) = exp (e1, pLive)
		      val (e2', live2) = exp (e2, pLive)
		      val pLive' = VSet.union (live1, live2)
		  in
		      (A.ApplyExp (e1', e2', t), pLive')
		  end
	      | exp (m as A.VarArityOpExp _, pLive) = (m, pLive)
	      | exp (A.TupleExp es, pLive) =
		  let val ess = map (fn e => exp (e, pLive)) es
		    val (es', ss) = ListPair.unzip ess
		  in
		      (A.TupleExp es', grandUnion ss)
		  end
	      | exp (A.RangeExp (e1, e2, oe3, t), pLive) = 
		  let val (e1', live1) = exp (e1, pLive)
		      val (e2', live2) = exp (e2, pLive)
		  in
		      case oe3
		       of NONE => (A.RangeExp (e1', e2', NONE, t),
				   VSet.union (live1, live2))
			| SOME e3 => 
			    let val (e3', live3) = exp (e3, pLive)
			    in
				(A.RangeExp (e1', e2', SOME e3', t),
				 grandUnion [live1, live2, live3])
			    end
		  end
	      | exp (A.PTupleExp es, pLive) = 
		  let val (es', pLive') = exps (es, pLive)
		  in
		      (A.PTupleExp es', pLive')
		  end
	      | exp (A.PArrayExp (es, t), pLive) = 
		  let val (es', pLive') = exps (es, pLive)
		  in
		      (A.PArrayExp (es', t), pLive')
		  end
	      | exp (A.PCompExp (e, pes, oe), pLive) = 
		  let val (e', pLive') = exp (e, pLive)
		      val (ps, es) = ListPair.unzip pes
		      val (es', pLive'') = exps (es, pLive)
		      val pes' = ListPair.zip (ps, es')
		  in
		      case oe
	               of NONE => (A.PCompExp (e', pes', NONE),
				   VSet.union (pLive', pLive''))
			| SOME pred => todo "PComp"
		  end
	      | exp (A.PChoiceExp (es, t), pLive) = 
		  let val (es', pLive') = exps (es, pLive)
		  in
		      (A.PChoiceExp (es', t), pLive')
		  end
	      | exp (A.SpawnExp e, pLive) =
		  let val (e', pLive') = exp (e, pLive)
		  in
		      (A.SpawnExp e', pLive')
		  end
	      | exp (k as A.ConstExp _, _) = (k, VSet.empty)
	      | exp (v as A.VarExp (x, ts), pLive) = 
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
	      | exp (A.SeqExp (e1, e2), pLive) = 
		  let val (e1', live1) = exp (e1, pLive)
		      val (e2', live2) = exp (e2, pLive)
		  in
		      (A.SeqExp (e1', e2'), VSet.union (live1, live2))
		  end
	      | exp (ov as A.OverloadExp _, pLive) = (ov, pLive)
						     
	    (* exps: A.exp list * VSet.set -> A.exp list * VSet.set *)
	    and exps (es, pLive) =
		  let val ess = map (fn e => exp (e, pLive)) es
		      val (es', ss) = ListPair.unzip ess
		  in
		      (es', grandUnion ss)
		  end
		  
	    (* letExp : A.binding * A.exp * VSet.set -> A.exp -> VSet.set *)
	    and letExp (A.ValBind (p, e1), e2, pLive) = 
		  let val pvs = varsInPat p
		      val (e1', plive1) = exp (e1, pLive)
		      val (e2', plive2) = exp (e2, pLive)
		      val pliveOut = VSet.union (plive1, VSet.difference (plive2, 
									  pvs))
		      val b' = A.ValBind (p, e1')
		  in
		      (A.LetExp (b', e2'), pliveOut)
		  end
	      | letExp (A.PValBind (p, e1), e2, pLive) = 
                  (case p
		    of A.ConPat (c, ts, p) => todo "letExp | PValBind | ConPat"
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
		     | A.TuplePat ps => todo "letExp | PValBind | TuplePat"
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
			     val (e1', live1) = exp (e1, pLive)
			     val e1f = F.mkFuture e1'
			     val tf = Var.new ("tf", TypeOf.exp e1f)
			     (* rewrite all vars in the tuple pat to tf *)
			     (* FIXME This is too lossy! *)
			     val e2' = List.foldl (fn (v, e) => VarSubst.subst1(v,tf,e)) e2 vs
			     val _ = List.app recordSelector vs
			     val (e2t, live2) = exp (e2', plus (pLive, tf))
			     val pLive' = 

				 let val s = grandUnion [VSet.singleton tf, live1, live2]
				 in
				     List.foldl (fn (v, s) => minus (s, v)) s vs
				 end
			 in
			     (A.LetExp (A.ValBind (A.VarPat tf, e1f), e2t), pLive')
			 end *)
		     | A.VarPat x =>
		         let val (e1', live1) = exp (e1, pLive)
			     val e1f = F.mkFuture1Spawn e1'
			     val xf  = Var.new (Var.nameOf x ^ "f", TypeOf.exp e1f)
			     val e2' = VarSubst.subst1 (x, xf, e2)
			     val (e2t, live2) = exp (e2', plus (pLive, xf))
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
		  
	      (*
	       The more general idea is to transform
		   
		   let pval p = e in e' end
		   
		   into
		   
		   let val f = future (fn () => case e
						 of p => vs p
				      (* where vs p produces the tuple of vars in p *))
		   in
                       [x -> #n (touch f)] e'     (* if f is a tuple *)
                   (* where n is the position of x in (vs p) *)
		   end
	       
               -ams
	       *)
	      | letExp (A.FunBind lams, e, pLive) = 
		  let val lams' = map (lambda pLive) lams
		      val (e', live1) = exp (e, pLive)			
		  in
		      (A.LetExp (A.FunBind lams', e'), live1)
		  end
		  
	    (* lambda : VSet.set -> A.lambda -> A.lambda *)
	    and lambda pLive (A.FB (f, x, e)) = A.FB (f, x, #1 (exp (e, pLive)))
						
	    (* cancel : A.var * A.exp -> A.exp *)
	    and cancel (x, e) = A.SeqExp (F.mkFuture1Cancel (A.VarExp (x, [])), e)
            (*                                 ^^               *)
            (* ??? Is it OK not to instantiate the 'a future? ??? *)

	    (* ifExp : A.exp * A.exp * A.exp * T.ty * VSet.set -> A.exp * VSet.set *)
	    and ifExp (e1, e2, e3, t, pLive) = let 
              val (e1', pLiveC) = exp (e1, pLive)
	      val (e2', pLiveT) = exp (e2, pLive)
	      val (e3', pLiveF) = exp (e3, pLive)
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
	    and caseExp (e, ms, t, pLive) = let
              val (e', pLive') = exp (e, pLive)
	      val mvs = map (fn m => match (m, pLive)) ms
	      val (ms', pLiveHd::pLiveTl) = ListPair.unzip mvs
              fun loop ([], _, _, matchAcc, setAcc) = (rev matchAcc, setAcc)
		| loop (m::ms, currLive, others, matchAcc, setAcc) = let
		    val othersLive = grandUnion others
		    val cancelUs = VSet.difference (VSet.intersection (pLive, othersLive),
						    currLive)
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
	    and match (A.PatMatch (p, e), pLive) = let
                  val (e', pLive') = exp (e, pLive)
                  in
		    (A.PatMatch (p, e'), pLive')
                  end
	      | match (A.CondMatch (p, e1, e2), pLive) = let
                  val (e1', pLive1) = exp (e1, pLive)
		  val (e2', pLive2) = exp (e2, pLive)
                  in
		    (A.CondMatch (p, e1', e2'), VSet.union (pLive1, pLive2))
		  end
	in
	    (Var.Tbl.clear selectors;
	    (* A.Module {exns = exns, body = #1 (exp (body, VSet.empty))}) *)
             #1 (exp (e, VSet.empty)))
	end
	    
    (**** tests ****)

    local

	structure U = TestUtils

	val t0 = U.int 0

	(* t1 = let pval x = fact 10 in x end *)
	val t1 =
	    let val x = Var.new ("x", Basis.intTy)
		val b = A.VarExp (x, [])
	    in
		U.plet (x, U.fact 10, b)
	    end

	(* t2 = let pval x = fact 10 in if true then x else 1 *)
	val t2 = 
	    let val x = Var.new ("x", Basis.intTy)
		val b = U.ifexp (U.trueExp, A.VarExp (x, []), U.int 1)
	    in
		U.plet (x, U.fact 10, b)
	    end

	(* t3 = let pval x = fact 10 in
                let pval y = fact 11 in
                if true then x else y *)
	val t3 =
	    let val x = Var.new ("x", Basis.intTy)
		val y = Var.new ("y", Basis.intTy)
		val i = U.ifexp (U.trueExp,
				 A.VarExp (x, []),
				 A.VarExp (y, []))
	    in
		U.plet (x, U.fact 10, U.plet (y, U.fact 11, i))
	    end

	(* t4 = let pval x = fact 10 in
		let pval y = fact 11 in
	        if true then if false then x else y
                        else if false than y else x *)
	val t4 =
	    let val x = Var.new ("x", Basis.intTy)
		val y = Var.new ("y", Basis.intTy)
		val xe = A.VarExp (x, [])
		val ye = A.VarExp (y, [])
		val i = U.ifexp (U.trueExp,
				 U.ifexp (U.falseExp, xe, ye),
				 U.ifexp (U.falseExp, ye, xe))
	    in
		U.plet (x, U.fact 10, U.plet (y, U.fact 11, i))
	    end

	(* t5 = let pval (a, b) = mkPair 20 in
                if true then 0 else a+b 
         *)
	val t5 = 
	    let val a = Var.new ("a", Basis.intTy)
		val b = Var.new ("b", Basis.intTy)
		val ae = A.VarExp (a, [])
		val be = A.VarExp (b, [])
		val i = U.ifexp (U.trueExp,
				 U.int 0,
				 U.add (ae, be))
	    in
		A.LetExp (A.PValBind (A.TuplePat [A.VarPat a, A.VarPat b],
				      U.mkPair 20),
			  i)
	    end

	(* t6 = let pval a = fact(100) 
         *          pval b = fact(99)
         *      in
         *        case true
         *         of true => a 
         *          | false => b
         *      end
         *)
	val t6 = let
          val a = Var.new ("a", Basis.intTy)
	  val b = Var.new ("b", Basis.intTy)
	  val truPat = A.ConstPat (A.DConst (Basis.boolTrue, []))
	  val flsPat = A.ConstPat (A.DConst (Basis.boolFalse, []))
          in
	    A.LetExp (A.PValBind (A.VarPat a, U.fact 100),
            A.LetExp (A.PValBind (A.VarPat b, U.fact 99),
            A.CaseExp (U.trueExp,
                       [A.PatMatch (truPat, A.VarExp (a, [])),
			A.PatMatch (flsPat, A.VarExp (b, []))],
		       Basis.intTy)))
	end

	(* t7 = let pval a = fact(100) 
         *          pval b = fact(99)
         *      in
         *        case 1
         *         of 0 => a 
         *          | 1 => a+b
         *          | 2 => b
         *          | _ => 0
         *      end
         *)
	val t7 = let
          val a = Var.new ("a", Basis.intTy)
	  val b = Var.new ("b", Basis.intTy)
	  val ae = A.VarExp (a, [])
	  val be = A.VarExp (b, [])
	  fun k n = A.ConstPat (A.LConst (Literal.Int (IntInf.fromInt n), Basis.intTy))
          in
	    A.LetExp (A.PValBind (A.VarPat a, U.fact 100),
            A.LetExp (A.PValBind (A.VarPat b, U.fact 99),
            A.CaseExp (U.int 1,
                       [A.PatMatch (k 0, ae),
			A.PatMatch (k 1, U.add (ae, be)),
			A.PatMatch (k 2, be),
			A.PatMatch (A.WildPat Basis.intTy, U.int 0)],
		       Basis.intTy)))
	  end

	(* t8 = let 
             pval x = fact(100)
             pval y = x
             in
               if b
               then ... x ...
               else ... y ...
             end
         *)
	val t8 = let
	    val x = Var.new ("x", Basis.intTy)
	    val y = Var.new ("y", Basis.intTy)
	    val xe = A.VarExp (x, [])
	    val ye = A.VarExp (y, [])
	    in
	      A.LetExp (A.PValBind (A.VarPat x, U.fact 100),
              A.LetExp (A.PValBind (A.VarPat y, xe),
              U.ifexp  (U.trueExp, xe, ye)))
	    end

	(* testPVal : A.exp -> unit *)
	fun testPVal e = (PrintAST.printExp e;
			  PrintAST.printComment "-->";
			  PrintAST.printExp (futurizeExp e))

    in
        (* test : int -> unit *)
        val test = U.mkTest testPVal [t0,t1,t2,t3,t4,t5,t6,t7,t8]
    end

  end
