(* fut-par-arr.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 * 
 * This module rewrites parallel arrays in terms of futures, touches, and ropes.
 *)

structure FutParArr : sig

    val futurize : AST.module -> AST.module
    val test : int -> unit

  end  = 

  struct

    structure A = AST
    structure T = Types
    structure F = Futures
    structure B = Basis
    structure R = Ropes

    (* mkRope : A.exp -> A.exp *)
    fun mkRope e = 
	  let val t = TypeOf.exp e
	  in
	      A.ApplyExp (A.VarExp (R.ropeFromList, [t]), e, R.ropeTy t)
	  end

    (* module : A.module -> A.module *)
    fun module m = let
	  val anyChange = ref false
	  val workQ = Var.new ("workQ", Basis.workQueueTy)
	  val workQExp = A.VarExp (workQ, [])
	(* parr : A.exp list * A.ty -> A.exp *)
	(* Precondition: The argument to the function, a list, must not be empty. *)
	(* Consumes a list whose members are the contents of a parallel array, *)
	(* and produces a LetExp that is a "futurized" and "roped" parray. *)
	(* Note: the first member of the array is not futurized (an optimization). *)
	  fun parr (e::es, t) = let
	      (* mkVar : int * ty -> var *)
		fun mkVar (n, t) = 
		      let val name = "f" ^ Int.toString n
		      in
			  Var.newWithKind (name, A.VK_Pat, t)
		      end
		(* build : exp list * int * binding list * exp list -> exp *) 
		fun build ([], _, bs, accExps) = 
		      let val list = ASTUtil.mkList (exp e :: accExps, t)
		      in
			List.foldr A.LetExp (mkRope list) bs
		      end
		  | build (e::es, n, bs, accExps) =
		      if F.isFutureCand e then
			  let val fe = F.mkFuture1 (workQExp, exp e)
			      val f_n = mkVar (n, TypeOf.exp fe)
			      val b = A.ValBind (A.VarPat f_n, fe)
			      val t = F.mkTouch1 (workQExp, A.VarExp (f_n, []))
			  in
			      build (es, n+1, b::bs, t::accExps)
			  end
		      else
			      build (es, n, bs, exp e :: accExps)
		in
		  anyChange := true;
		  build (rev es, 1, [], [])
		end
	    | parr ([], _) = raise Fail "parr: expected non-empty list of expressions"
				
	(* exp : A.exp -> A.exp *)
	(* n.b. Type-preserving. *)
	  and exp (A.LetExp (b, e)) = A.LetExp (binding b, exp e)
	    | exp (A.IfExp (e1, e2, e3, t)) = A.IfExp (exp e1, exp e2, exp e3, t)
	    | exp (A.CaseExp (e, ms, t)) = A.CaseExp (exp e, map match ms, t)
	    | exp (A.HandleExp (e, ms, t)) = A.HandleExp (exp e, map match ms, t) 
	    | exp (A.RaiseExp (e, t)) = A.RaiseExp (exp e, t)
	    | exp (A.FunExp (x, e, t)) = A.FunExp (x, exp e, t)
	    | exp (A.ApplyExp (e1, e2, t)) = A.ApplyExp (exp e1, exp e2, t)
	    | exp (A.TupleExp es) = A.TupleExp (map exp es)
	    | exp (A.RangeExp (e1, e2, oe3, t)) =
		A.RangeExp(exp e1, exp e2, Option.map exp oe3, t)
	    | exp (A.PTupleExp es) = A.PTupleExp (map exp es)
	    | exp (A.PArrayExp (es, t)) = parr (es, t)
	    | exp (A.PCompExp (e, pes, oe)) = 
		A.PCompExp(exp e, List.map (fn (p,e) => (p, exp e)) pes, Option.map exp oe)
	    | exp (A.PChoiceExp (es, t)) = A.PChoiceExp(map exp es, t)
	    | exp (A.SpawnExp e) = A.SpawnExp (exp e)
	    | exp (k as (A.ConstExp _)) = k
	    | exp (v as (A.VarExp _)) = v
	    | exp (A.SeqExp (e1, e2)) = A.SeqExp (exp e1, exp e2)
	    | exp (ov as (A.OverloadExp _)) = ov
					      
	  (* match : A.match -> A.match *)
	  and match (A.PatMatch (p, e)) = A.PatMatch (p, exp e)
	    | match (A.CondMatch (p, cond, e)) = A.CondMatch (p, exp cond, exp e)

	  (* binding : A.binding -> A.binding *)
	  and binding (A.ValBind (p, e)) = A.ValBind (p, exp e)
	    | binding (A.PValBind (p, e)) = A.PValBind (p, exp e)
	    | binding (A.FunBind lams) = A.FunBind (map lambda lams)
					 
	  (* lambda : A.lambda -> A.lambda *)
	  and lambda (A.FB (f, x, b)) = A.FB (f, x, exp b)

	  val m' = exp m
	  in
	    if !anyChange
	      then A.LetExp(
		  A.ValBind(A.VarPat workQ, F.mkNewWorkQueue ()),
		  A.LetExp(A.ValBind(A.WildPat Basis.workQueueTy, F.mkGetWork1All workQExp),
		m'))
	      else m
	  end

  (* futurize : A.module -> A.module *)
    fun futurize m = module m

    (**** tests ****)

    local

	structure U = TestUtils

	(* test cases *)

	(* t0 = (| fact 10, fact 11 |) *)
	val t0 = U.parr ([U.fact 10, U.fact 11], Basis.intTy)

	(* test : A.exp -> unit *)
	fun testPArr e = (PrintAST.print e;
			  U.describe (SOME "futurizing");
			  PrintAST.print (futurize e))
    in

        (* test : int -> unit *)
        val test = U.mkTest testPArr [t0]

    end

  end
