(* derived-forms.sml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 * 
 * This module spells out a parallel choice as a pcase.
 * Specifically: two arguments e1 and e2 (presumably from e1 |?| e2)
 * are embedded in
 *   pcase e1 & e2
 *     of res1 & ? => res1
 *      | ? & res2 => res2
 *
 * It also desugars |andalso|, |orelse|, and |*|.
 *
 * This module is --not yet active--. It is compiled, but not used anywhere.
 *)

structure DerivedForms =

  struct

    structure A = AST
    structure U = ASTUtil
    structure T = Types

    fun pchoice (e1, e2) = let
      val t1 = TypeOf.exp e1
      val t2 = t1 (* TypeOf.exp e2 *)
      (* n.b. t1 and t2 must be the same -- checked elsewhere *)
      val res1 = Var.new ("res1", t1)
      val res2 = Var.new ("res2", t2)
      val arm1 = A.PMatch ([A.Pat (A.VarPat res1), A.NDWildPat t2],
			   A.VarExp (res1, []))
      val arm2 = A.PMatch ([A.NDWildPat t1, A.Pat (A.VarPat res2)],
			   A.VarExp (res2, []))
      in
	A.PCaseExp ([e1, e2], [arm1, arm2], t1)
      end

    val trueExp   = U.trueExp
    val falseExp  = U.falseExp
    val truePPat  = A.Pat (A.ConstPat U.trueConst)
    val falsePPat = A.Pat (A.ConstPat U.falseConst)

  (* e1 |orelse| e2 --> pcase e1 & e2
   *                      of true & ? => true
   *                       | ? & true => true
   *                       | false & false => false 
   *)
    fun porelse (e1, e2) = let
      val boolWild = A.NDWildPat Basis.boolTy
      (* e1 and e2 must be of type boolean -- checked elsewhere *)
      val arm1 = A.PMatch ([truePPat, boolWild], trueExp)
      val arm2 = A.PMatch ([boolWild, truePPat], trueExp)
      val arm3 = A.PMatch ([falsePPat, falsePPat], falseExp)
      in
        A.PCaseExp ([e1, e2], [arm1, arm2, arm3], Basis.boolTy)
      end

  (* e1 |andalso| e2 --> pcase e1 & e2
   *                       of false & ? => false
   *                        | ? & false => false
   *                        | true & true => true
   *)
    fun pandalso (e1, e2) = let
      val boolWild = A.NDWildPat Basis.boolTy
      (* e1 and e2 must be of type boolean -- checked elsewhere *)
      val arm1 = A.PMatch ([falsePPat, boolWild], falseExp)
      val arm2 = A.PMatch ([boolWild, falsePPat], falseExp)
      val arm3 = A.PMatch ([truePPat, truePPat], trueExp)
      in
        A.PCaseExp ([e1, e2], [arm1, arm2, arm3], Basis.boolTy)
      end

    val zeroConst = U.mkIntConst 0
    val zeroPPat = A.Pat (A.ConstPat zeroConst)
    val zeroExp = A.ConstExp zeroConst

  (* e1 |*| e2 --> pcase e1 & e2
   *                of 0 & ? => 0
   *                 | ? & 0 => 0
   *                 | m & n => (m * n)
   *)
  (* For now, only works on ints. *)
    fun pmul (e1, e2) = let
      val intWild = A.NDWildPat Basis.intTy
      (* assume e1 and e2 are of type int *)
      (* TODO overload this to the extent possible (i.e. not just ints) *)
      (* check types... *)
      val t1 = TypeOf.exp e1
      val t2 = TypeOf.exp e2
      val chk1 = if TypeUtil.same (t1, Basis.intTy) then () 
		 else raise Fail "pmul's first operand is not an int"
      val chk2 = if TypeUtil.same (t2, Basis.intTy) then () 
		 else raise Fail "pmul's second operand is not an int"
      (* now generate the code...*)
      val m = Var.new ("m", Basis.intTy)
      val n = Var.new ("n", Basis.intTy)
      val mn = A.ApplyExp (A.VarExp (Basis.int_times, []),
			   A.TupleExp [A.VarExp (m, []), A.VarExp (n, [])], 
			   Basis.intTy)
      val arm1 = A.PMatch ([zeroPPat, intWild], zeroExp)
      val arm2 = A.PMatch ([intWild, zeroPPat], zeroExp)
      val arm3 = A.PMatch ([A.Pat (A.VarPat m), A.Pat (A.VarPat n)], mn)
      in
        A.PCaseExp ([e1, e2], [arm1, arm2, arm3], Basis.intTy)
      end

  end
