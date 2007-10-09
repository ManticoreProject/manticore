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

    (* intPat : int -> A.pat *)
    fun intPat n = A.ConstPat (A.LConst (Literal.Int n, B.intTy))

    (* varPat : var -> A.pat *)
    fun varPat x = A.VarPat x

    (* apply : A.exp -> A.exp -> A.exp *)
    fun apply e1 e2 = 
	let val (dty, rty) = (case TypeOf.exp e1
			        of T.FunTy t => t
				 | _ => fail ("apply: expected a function; "
					      ^ " type is " 
					      ^ TypeUtil.toString (TypeOf.exp e1)))

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

    (* parr : A.exp list * T.ty -> A.exp *)
    fun parr (es, t) = A.PArrayExp (es, t)

    (* fact : int -> A.exp *)
    fun fact n =
	let val f = Var.new ("fact", T.FunTy (Basis.intTy, Basis.intTy))
	in
	    apply (A.VarExp (f, [])) (int n)
	end

    val isZeroVar = Var.new ("isZero", T.FunTy (Basis.intTy, Basis.boolTy))

    (* isZero : int -> A.exp *)
    fun isZero n = apply (A.VarExp (isZeroVar, [])) (int n)

    (* ifexp : A.exp * A.exp * A.exp -> A.exp *)
    fun ifexp (e1, e2, e3) =
	let val t2 = TypeOf.exp e2
	in
	    if not (TypeUtil.same (TypeOf.exp e1, Basis.boolTy)) then
		fail "ifexp: first expression not boolean"
	    else if TypeUtil.same (t2, TypeOf.exp e3) then
		A.IfExp (e1, e2, e3, t2)
	    else
		fail "ifexp: types of branches don't match"
	end

    (* caseExp: A.exp * (A.pat * A.exp) list -> A.exp *) 
    fun caseExp (e, pes as (mp,me)::_) =
	let val t = TypeOf.exp me
	in
	    A.CaseExp (e, List.map A.PatMatch pes, t)
	end
      | caseExp (e, []) = fail "caseExp: no branches"

    (* plet : var * A.exp * A.exp -> A.exp *)
    fun plet (x, e, e') = A.LetExp (A.PValBind (A.VarPat x, e), e')
		
    (* trueExp : A.exp *)
    val trueExp = A.ConstExp (A.DConst (Basis.boolTrue, []))

    (* falseExp : A.exp *)
    val falseExp = A.ConstExp (A.DConst (Basis.boolFalse, []))


    local
	val tv = TyVar.new(Atom.atom "'a")
	val tv' = AST.VarTy tv
        val optionTyc = TyCon.newDataTyc (Atom.atom "option", [tv])
	val optionNONE = DataCon.new optionTyc (Atom.atom "NONE", NONE)
	val optionSOME = DataCon.new optionTyc (Atom.atom "SOME", SOME(tv'))

    in

        (*  some : A.exp -> A.exp *)
        fun some e = 
	      let val t = TypeOf.exp e
		  val optionTySch = DataCon.typeOf optionSOME
		  val some = A.ConstExp (A.DConst (optionSOME, [t]))
		  val rngTy = (case TypeUtil.apply (optionTySch, [t])
				of T.FunTy (d, r) => r
				 | _ => fail "some: expected FunTy")
	      in
		  A.ApplyExp (some, e, rngTy)
	      end

        (*  none: T.ty -> A.exp *)
	fun none t = A.ConstExp (A.DConst (optionNONE, [t]))

    end (* local *)

    (* describe : string option -> unit *)
    fun describe NONE = P.printComment "-->"
      | describe (SOME s) = P.printComment (s ^ " -->")

    (* mkTest : ('a -> unit) -> 'a list -> (int -> unit) *)
    fun mkTest testFunction terms n = testFunction (List.nth (terms, n))
	handle Subscript => 
	       let fun println s = print (s ^ "\n")
		   fun tell n = 	
		       let val nstr = Int.toString (n-1)
			   val msg = "Please choose a test value \
				     \between 0 and " ^ nstr ^ "."
		       in
			   println msg
		       end
	       in
		   tell (length terms)
	       end
	
  end
