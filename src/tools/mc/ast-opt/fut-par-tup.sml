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
    structure T = Types
    structure F = Futures

    infixr **
    (* (**) : ('a -> 'b) * ('c -> 'd) -> ('a * 'c) -> ('b * 'd) *)
    fun f ** g = (fn (a, c) => (f a, g c))

    (* id : 'a -> 'a *)
    val id = fn x => x

    infixr 5 :>: (* same precedence as :: *)
    (* (:>:) : ('a * 'b) * ('a list * 'b list) -> 'a list * 'b list *)
    fun (x,y) :>: (xs,ys) = (x::xs, y::ys)
 
    (* module : A.module -> A.module *)
    fun module m = 
	let val q = Var.new ("q", Basis.workQueueTy)
	    val qe = A.VarExp (q, [])
	    (* ptuple : A.exp list -> A.exp *)
	    (* Precondition: The argument to the function, a list, must not be empty. *)
	    (* Consumes a list whose members are the contents of a parallel tuple, *)
	    (* and produces a LetExp that is a "futurized" ptuple. *)
	    (* Note: the first member of the list is not futurized (an optimization). *)
	    fun ptuple (e::es) = 
  		let (* mkFutBinds : A.exp list -> A.binding list * A.var list *)
		    fun mkFutBinds ([], n) = ([],[])
		      | mkFutBinds (e::es, n) =
			let val fe = F.mkFuture1 (qe, e)
			    val f_n = Var.newWithKind ("f" ^ Int.toString n,
						       A.VK_Pat,
						       TypeOf.exp fe)
			    val b = A.ValBind (A.VarPat f_n, fe)
			in
			    (b, f_n) :>: mkFutBinds (es, n+1)
			end
		    (* letMany : A.binding list * A.exp -> A.exp *)
		    (* pre: there is at least one binding *)
		    fun letMany (b::[], e) = A.LetExp (b, e)
		      | letMany (b::bs, e) = A.LetExp (b, letMany (bs, e))
		      | letMany ([], _) = raise Fail "letMany: argument must have\
                                                     \ at least one binding"
		    val (bs, vs) = mkFutBinds (map exp es, 1)
		    val touches = map (fn v => F.mkTouch1 (qe, A.VarExp (v, []))) vs
		in
		    letMany (bs, A.TupleExp (exp e :: touches))
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

	in
	    A.LetExp (A.ValBind (A.VarPat q, F.mkNewWorkQueue ()),
		      A.LetExp (A.ValBind (A.WildPat Basis.workQueueTy, 
					   F.mkGetWork1All (A.VarExp (q, []))),
				exp m))
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

	(* test : A.exp -> unit *)
	fun testPTup e = (PrintAST.print e;
			  U.describe (SOME "futurizing");
			  PrintAST.print (futurize e))
    in

        (* test : int -> unit *)
        val test = U.mkTest testPTup [t0,t1,t2,t3]

    end

  end
