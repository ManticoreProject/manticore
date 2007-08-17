(* fut-par-let.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 * 
 * This module rewrites parallel bindings in terms of futures, touches and cancels.
 *
 * Note this rewriting is type preserving.
 *)

structure FutParLet (* : sig

    val futurize : A.module -> A.module
    val test : int -> unit

  end *) = 

  struct

    structure A = AST
    structure T = Types
    structure F = Futures

    (* fail : string -> 'a *)
    fun fail msg = raise Fail msg

    (* todo : string -> 'a *)
    fun todo thing = fail ("todo: " ^ thing)

    infixr **
    (* (**) : ('a -> 'b) * ('c -> 'd) -> ('a * 'c) -> ('b * 'd) *)
    fun f ** g = (fn (a, c) => (f a, g c))

    (* id : 'a -> 'a *)
    val id = fn x => x

    (* exp : A.exp -> A.exp *)
    fun exp (A.LetExp (b, e)) =
	  (case b
	     of A.PValBind (p, e') => pval (p, e', e)
	      | _ => A.LetExp (binding b, exp e)
	    (* end case *))
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
      | exp (A.PTupleExp es) = A.PTupleExp (map exp es)
      | exp (A.PArrayExp (es, t)) = A.PArrayExp (map exp es, t)
      | exp (A.PCompExp (e, pes, oe)) = A.PCompExp (exp e,
						    map (id ** exp) pes,
						    Option.map exp oe)
      | exp (A.PChoiceExp (es, t)) = A.PChoiceExp (map exp es, t)
      | exp (A.SpawnExp e) = A.SpawnExp (exp e)
      | exp (k as A.ConstExp _) = k
      | exp (v as A.VarExp _) = v
      | exp (A.SeqExp (e1, e2)) = A.SeqExp (exp e1, exp e2)
      | exp (ov as A.OverloadExp _) = ov

    (* pval : A.pat * A.exp * A.exp -> A.exp *)
    and pval (p, e1, e2) = todo "pval"

    (* binding : A.binding -> A.binding *)
    (* Pre: The argument is not a PValBind. *)
    and binding (A.ValBind (p, e)) = A.ValBind (p, exp e)
      | binding (A.PValBind _) = fail "binding: unexpected PValBind"
      | binding (A.FunBind lams) = A.FunBind (map lambda lams)

    (* lambda : A.lambda -> A.lambda *)
    and lambda (A.FB (v1, v2, e)) = A.FB (v1, v2, exp e)

    (**** tests ****)

    local

	structure U = TestUtils

	(* t0 = let pval x = fact 10 in (x, 0) end *)
	val t0 = 
	    let val x = Var.new ("x", Basis.intTy)
	    in
		U.pval (x, U.fact 10, U.tup [A.VarExp (x, []), 
					     U.int 0])
	    end

    in

        fun test n = fail "test"

    end

  end
