(* fut-par-tup.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 * 
 * This module rewrites parallel tuples in terms of futures and touches.
 *
 * Roughly, the transformation turns
 *  (| e1, e2, ..., en |)
 * into
 *   let val f2 = future (fn () => e2)
 *       ...
 *       val fn = future (fn () => en)
 *   in
 *       (e1, touch f2, ..., touch fn)
 *   end
 *
 * Note this rewriting is type preserving.
 *)

structure FutParTup : sig

    val futurize : AST.module -> AST.module
    val test : int -> unit

  end  = 

  struct

    structure A = AST
    structure B = Basis
    structure F = Futures
    structure T = Types
    structure U = UnseenBasis

    infixr **
    (* (**) : ('a -> 'b) * ('c -> 'd) -> ('a * 'c) -> ('b * 'd) *)
    fun f ** g = (fn (a, c) => (f a, g c))

    (* id : 'a -> 'a *)
    val id = fn x => x

    (* module : A.module -> A.module *)
    fun module m = let
	  val anyChange = ref false
	  val workQ = Var.new ("workQ", U.workQueueTy)
	  val workQExp = A.VarExp (workQ, [])
	(* ptuple : A.exp list -> A.exp *)
	(* Precondition: The argument to the function, a list, must not be empty. *)
	(* Consumes a list whose members are the contents of a parallel tuple, *)
	(* and produces a LetExp that is a "futurized" ptuple. *)
	(* Note: the first member of the list is not futurized (an optimization). *)
	  fun ptuple (e::es) = 
		let (* mkVar : int * ty -> var *)
		    fun mkVar (n, t) = 
			  let val name = "f" ^ Int.toString n
			  in
			      Var.newWithKind (name, A.VK_Pat, t)
			  end
		    (* build : exp list * int * binding list * exp list -> exp *) 
		    fun build ([], _, bs, tupExps) = 
			  let val tup = A.TupleExp (exp e :: tupExps)
			  in
			      foldr A.LetExp tup bs
			  end
		      | build (e::es, n, bs, tupExps) =
			  if F.isFutureCand e then
			      let val fe = F.mkFuture1 (workQExp, exp e)
				  val f_n = mkVar (n, TypeOf.exp fe)
				  val b = A.ValBind (A.VarPat f_n, fe)
				  val t = F.mkTouch1 (workQExp, A.VarExp (f_n, []))
			      in
				  build (es, n+1, b::bs, t::tupExps)
			      end
			  else
			      build (es, n, bs, exp e :: tupExps)
		in
		  anyChange := true;
		  build (rev es, 1, [], [])
		end
	    | ptuple [] = raise Fail "ptuple: expected non-empty list of expressions"
				
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
	    | exp (A.RangeExp (e1, e2, oe3, t)) = A.RangeExp (exp e1,
							      exp e2,
							      Option.map exp oe3,
							      t)
	    | exp (A.PTupleExp es) = ptuple es
	    | exp (A.PArrayExp (es, t)) = A.PArrayExp (map exp es, t)
	    | exp (A.PCompExp (e, pes, oe)) = 
	      A.PCompExp (exp e,
			  map (id ** exp) pes,
			  Option.map exp oe)
	    | exp (A.PChoiceExp (es, t)) = A.PChoiceExp (map exp es, t)
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
		  m')
(*		  A.LetExp(A.ValBind(A.WildPat U.workQueueTy, F.mkGetWork1All workQExp),
		m'))*)
	      else m
	  end

    (* futurize : A.module -> A.module *)
    fun futurize m = module m

    (**** tests ****)

    local

	structure U = TestUtils

	(* test cases *)

	(* t0 = (| fact 10, fact 11 |) *)
	val t0 = U.ptup [U.fact 10, U.fact 11]

	(* t1 = (| (| fact 10, fact 11|), (| fact 10, fact 11 |) |) *)
	val t1 = U.ptup [t0, t0]

	(* t2 = (| fact 10, fact 11, fact 12, fact 13, fact 14 |) *)
	val t2 = U.ptup (map U.fact [10,11,12,13,14])

	(* t3 = (| (| fact 10, fact 11 |), fact 12 |) *)
	val t3 = U.ptup [U.ptup [U.fact 10,
				 U.fact 11],
			 U.fact 12]

	(* t4 = (|1, 2, 3|) *)
	val t4 = U.ptup [U.int 1, U.int 2, U.int 3]

	(* t4 = (|1, fact 20, 3|) *)
	val t5 = U.ptup [U.int 1, U.fact 20, U.int 3]

	(* test : A.exp -> unit *)
	fun testPTup e = (PrintAST.print e;
			  U.describe (SOME "futurizing");
			  PrintAST.print (futurize e))
    in

        (* test : int -> unit *)
        val test = U.mkTest testPTup [t0,t1,t2,t3,t4,t5]

    end

  end
