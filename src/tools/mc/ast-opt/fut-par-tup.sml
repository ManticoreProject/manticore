(* fut-par-tup.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 * 
 * This module rewrites parallel tuples in terms of futures and touches.
 *
 * Roughly, the transformation turns
 *  (| e1 ... en |)
 * into
 *   let val f1 = future e1
 *       ...
 *       val fn = future en
 *   in
 *       (touch f1, ..., touch fn)
 *   end
 *
 * Note this rewriting is type preserving.
 *)

structure FutParTup (* : sig

    val futurize : A.module -> A.module

  end *) = 

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
 
    (* ptuple : A.exp list -> A.exp *)
    (* Precondition: The argument to the function, a list, must not be empty. *)
    (* Consumes a list whose members are the contents of a parallel tuple, *)
    (* and produces a LetExp that is a "futurized" ptuple. *)
    (* Note: the first member of the list is not futurized (an optimization). *)
    fun ptuple (e::es) = 
  	  let (* mkFutBinds : A.exp list -> A.binding list * A.var list *)
	      fun mkFutBinds ([], n) = ([],[])
		| mkFutBinds (e::es, n) =
		    let val fe = F.future e
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
		| letMany ([], _) = raise Fail 
			            "letMany: argument must have at least one binding"
	      val (bs, vs) = mkFutBinds (map exp es, 1)
	      val touches = map (fn v => F.touch (A.VarExp (v, []))) vs
	  in
	      letMany (bs, A.TupleExp (exp e :: touches))
	  end
      | ptuple [] = raise Fail "ptuple: expected non-empty list of expressions"

    (* exp : A.exp -> A.exp *)
    and exp (A.LetExp (b, e)) = A.LetExp (binding b, exp e)
      | exp (A.IfExp (e1, e2, e3, t)) = A.IfExp (exp e1, exp e2, exp e3, t)
      | exp (A.CaseExp (e, pes, t)) = A.CaseExp (exp e,
						 map (id ** exp) pes,
						 t)
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

    (* binding : A.binding -> A.binding *)
    and binding (A.ValBind (p, e)) = A.ValBind (p, exp e)
      | binding (A.PValBind (p, e)) = A.PValBind (p, exp e)
      | binding (A.FunBind lams) = A.FunBind (map lambda lams)

    (* lambda : A.lambda -> A.lambda *)
    and lambda (A.FB (f, x, b)) = A.FB (f, x, exp b)

    (* module : A.module -> A.module *)
    fun module m = exp m

    (* futurize : A,module -> A.module *)
    fun futurize m = module m

    (**** tests ****)

    local
	structure U = TestUtils
	infixr arrow
	(* (arrow) : T.ty * T.ty -> T.ty *)
	fun dom arrow rng = A.FunTy (dom, rng)
	val intTy = Basis.intTy
	(* int : int -> A.Exp *)
	fun int n = A.ConstExp (A.LConst (Literal.Int n, intTy))
	(* ptup : A.exp list -> A.exp *)
	fun ptup es = A.PTupleExp es
	val fact = A.VarExp (Var.new ("fact", intTy arrow intTy), [])
        (* apply : A.exp -> A.exp -> A.exp *)
	fun apply e1 e2 = 
	      let val rty = (case TypeOf.exp e1
			       of T.FunTy (d, r) => r
				| _ => raise Fail "expected a function")
	      in
		  A.ApplyExp (e1, e2, rty)
	      end
	(* sep : string option -> unit *)
	fun sep NONE = PrintAST.printComment "-->"
	  | sep (SOME s) = PrintAST.printComment (s ^ " -->")
	(* test cases *)
	val t0 = ptup [(apply fact o int) 10,
		       (apply fact o int) 11]
	val t1 = ptup [t0, t0]
	val t2 = ptup (map (apply fact o int) [10,11,12,13,14])
	val t3 = ptup [ptup [apply fact (int 10),
			     apply fact (int 11)],
		       apply fact (int 15)]
	(* test : A.exp -> unit *)
	fun test e = (PrintAST.print e;
		      sep (SOME "futurizing");
		      PrintAST.print (futurize e))
    in
        fun test0 () = test t0
	fun test1 () = test t1
	fun test2 () = test t2
        fun test3 () = test t3
	fun test4 () = (PrintAST.print t1;
			sep (SOME "flattening");
			test (FlatParTup.flattenModule t1))
    end

  end
