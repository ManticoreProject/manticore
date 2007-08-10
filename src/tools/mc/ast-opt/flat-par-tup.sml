(* flat-par-tup.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

(* Q1 What is the ty list for in a VarExp? *)

structure FlatParTup (* : sig

    val flatten : AST.module -> AST.module

  end *) = 

  struct

    structure A = AST
    structure T = Types

    (* id : a -> a *)
    val id = (fn z => z)

    (* (**) : ((a -> b) * (c -> d)) -> ((a * c) -> (b * d)) *)
    infixr **
    fun f ** g = (fn (a, b) => (f a, g b))

    (* removeParens : A.exp list -> A.exp list *)
    (* pre: argument must not be empty *)
    fun removeParens es =
	let (* exp : exp -> exp list *)
	    fun exp (A.TupleExp es) = exps es
	      | exp (A.PTupleExp es) = exps es
	      | exp e = [e]
	    (* exps : exp list -> exp list *)
	    and exps ([e]) = exp e
	      | exps (e::es) = exp e @ exps es
	      | exps [] = raise Fail "empty"
	in
	    exps es
	end

    (* flattenTup : A.exp -> A.exp *)
    fun flattenTup (A.TupleExp es) = A.TupleExp (removeParens es)
      | flattenTup (A.PTupleExp es) = A.PTupleExp (removeParens es)
      | flattenTup e = e

    local
	(* letterSeed : int ref *)
	val letterSeed = ref 0		 
	(* letters : string vector *)
	val letters = 
	    let val alphabet = "abcdefghijklmnopqrstuvwxyz"
	    in
		Vector.fromList (List.map Char.toString (explode alphabet))
	    end
    in
        (* resetVarNames : unit -> unit *)
        fun resetVarNames () = (letterSeed := 0)
        (* nextVarName : unit -> string *)
        fun nextVarName () =
	    let val s = !letterSeed
		val a = Vector.sub (letters, s mod 26)
		val n = s div 26
		val x = if (n > 0) 
			then a ^ (Int.toString n)
			else a
	    in
		letterSeed := s + 1;
		x
	    end
    end

    (* freshVar: (string option * T.ty) -> A.var *)
    fun freshVar (os, t) = Var.new (case os 
				      of NONE => nextVarName () 
				       | SOME s => s,
				    t) 

    (* mkVarTup : T.ty -> A.exp *)
    fun mkVarTup (T.TupleTy ts) =
	  let fun v var = A.VarExp (var, []) 
	      (* build : T.ty list -> A.exp list *)
	      fun build ([], acc) = rev acc
		| build (t::ts, acc) = 
		    (case t
		      of T.TupleTy _ => build (ts, (mkVarTup t) :: acc)
		       | _ => build (ts, (v (freshVar (NONE, t))) :: acc))
	  in
	      A.PTupleExp (build (ts, []))
	  end
      | mkVarTup _ = raise Fail "not a tuple type"


    (* makeNester : T.ty -> A.var * A.exp *)
    fun makeNester t =
	let val nestedVarTup = mkVarTup t
	    val flatVarTup = flattenTup nestedVarTup
	    val nestedTupTy = t
	    val flatTupTy = TypeOf.exp flatVarTup
	    val nest = Var.new ("nest", T.FunTy (flatTupTy, nestedTupTy))
	    val x = Var.new ("x", flatTupTy)
	    val pat = 
		(* flatVarTup is a parallel tuple of variable expressions *)
		let (* p : A.exp -> A.pat *)
		    fun p (A.VarExp (v, ts)) = A.VarPat v
		      | p _ = raise Fail "expected a VarExp"
		    (* es : A.exp list *)
		    val es = case flatVarTup
			       of (A.PTupleExp es) => es
				| _ => raise Fail "expected a PTupleExp"
		in
		    A.TuplePat (map p es)
		end
	    val body = A.CaseExp (A.VarExp (x, []),
				  [(pat, Unpar.unpar nestedVarTup)],
				  nestedTupTy)
	in
	    resetVarNames ();
	    (nest, A.FB (nest, x, body))
	end

    (**** main traversal of the AST ****)
	
    (* exp : A.exp -> A.exp *)
    fun exp (A.LetExp (b, e)) = A.LetExp (binding b, exp e)
      | exp (A.IfExp (e1, e2, e3, t)) = A.IfExp (exp e1, exp e2, exp e3, t)
      | exp (A.CaseExp (e, pes, t)) = A.CaseExp (exp e, List.map (id ** exp) pes, t)
      | exp (A.ApplyExp (e1, e2, t)) = A.ApplyExp (exp e1, exp e2, t)
      | exp (A.TupleExp es) = A.TupleExp (List.map exp es)
      | exp (A.RangeExp (e1, e2, oe3, t)) = A.RangeExp (exp e1, exp e2, Option.map exp oe3, t)
      | exp (p as A.PTupleExp es) =
	  let val t = TypeOf.exp p
	      val (f, lam) = makeNester t 
	  in
	      A.LetExp (A.FunBind [lam],
			A.ApplyExp (A.VarExp (f, []), flattenTup p, t))
	  end
      | exp (A.PArrayExp (es, t)) = A.PArrayExp (List.map exp es, t)
      | exp (A.PCompExp (e, pes, eo)) = A.PCompExp (exp e, 
						    List.map (id ** exp) pes, 
						    Option.map exp eo)
      | exp (A.PChoiceExp (es, t)) = A.PChoiceExp (List.map exp es, t)
      | exp (A.SpawnExp e) = A.SpawnExp (exp e)
      | exp (k as (A.ConstExp _)) = k
      | exp (v as (A.VarExp _)) = v
      | exp (A.SeqExp (e1, e2)) = A.SeqExp (exp e1, exp e2)
      | exp (v as (A.OverloadExp ovr)) = v

    (* binding : A.binding -> A.binding *)
    and binding (A.ValBind (p, e)) = A.ValBind (p, exp e)
      | binding (A.PValBind (p, e)) = A.PValBind (p, exp e)
      | binding (A.FunBind ls) = A.FunBind (List.map lambda ls)

    (* lambda : A.lambda -> A.lambda *)
    and lambda (A.FB (v1, v2, e)) = A.FB (v1, v2, exp e)

    (* flatten : A.module -> A.module *)
    fun flatten m = exp m

    (**** tests ****)

    local
	structure P = PrintAST
	fun ptup es = A.PTupleExp es
	fun int n = A.ConstExp (A.LConst (Literal.Int n, Basis.intTy))
	val t0 = ptup [int 0, 
		       ptup [int 1, int 2],
		       int 3,
		       ptup [int 4,
			     int 5,
			     ptup [int 6, int 7]]]
    in
        fun test0 () = (P.print t0;
			P.printComment "-->";
			P.print (flatten t0))
    end

  end
