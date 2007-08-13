(* test-utils.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure TestUtils = 

  struct

    (* fail : string -> 'a *)
    fun fail msg = raise Fail msg

    structure A = AST
    structure T = Types
    structure P = PrintAST
    structure PTy = PrintTypes
    structure B = Basis

    (* int : int -> A.exp *)
    fun int n = A.ConstExp (A.LConst (Literal.Int n, B.intTy))

    (* apply : A.exp -> A.exp -> A.exp *)
    fun apply e1 e2 = 
	let val (dty, rty) = (case TypeOf.exp e2
			        of T.FunTy t => t
				 | _ => fail "apply: expected a function")
	in
	    if TypeUtil.same (dty, TypeOf.exp e2) then
		A.ApplyExp (e1, e2, rty)
	    else
		fail "apply: type mismatch in application"
	end

    (* tup : A.exp list -> A.exp *)
    fun tup es = A.TupleExp es

    (* ptup : A.exp list -> A.exp *)
    fun ptup es = A.PTupleExp es

    (* some : A.exp -> A.exp *)
    fun some e = 
	let val t = TypeOf.exp e
	    val optionTySch = DataCon.typeOf B.optionSOME
	    val t' = TypeUtil.apply (optionTySch, [t])
	    val some = A.ConstExp (A.DConst (B.optionSOME, [t]))
	in
	    A.ApplyExp (some, e, t')
	end

    fun none t = A.ConstExp (A.DConst (B.optionNONE, [t]))

    (* test : (A.exp -> A.exp) -> A.exp -> unit *)
    fun test ee e = (P.print e;
		     P.printComment "-->";
		     P.print (ee e))

  end
