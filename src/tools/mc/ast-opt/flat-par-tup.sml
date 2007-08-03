(* flat-par-tup.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Adam Shaw
 *)

structure FlatParTup : sig

    val fpt : AST.module -> AST.module

  end = 

  struct

    structure A = AST

    infixr **
    fun f ** g = (fn (a, b) => (f a, g b))

    fun exp (A.LetExp (b, e)) = A.LetExp (binding b, exp e)
      | exp (A.IfExp (e1, e2, e3, t)) = A.IfExp (exp e1, exp e2, exp e3, t)
      | exp (A.CaseExp (e, pes, t)) = A.CaseExp (exp e, List.map (pat ** exp) pes, t)
      | exp (A.ApplyExp (e1, e2, t)) = A.ApplyExp (exp e1, exp e2, t)
      | exp (A.TupleExp es) = A.TupleExp (List.map exp es)
      | exp (A.RangeExp (e1, e2, oe3, t)) = A.RangeExp (exp e1, exp e2, Option.map exp oe3, t)
      | exp (A.PTupleExp es) = raise Fail "todo"
      | exp (A.PArrayExp (es, t)) = A.PArrayExp (List.map exp es, t)
      | exp (A.ComprehendExp (e, pes, eo)) = A.ComprehendExp (exp e, 
							      List.map (pat ** exp) pes, 
							      Option.map exp eo)
      | exp (A.PChoiceExp (es, t)) = A.PChoiceExp (List.map exp es, t)
      | exp (A.SpawnExp e) = A.SpawnExp (exp e)
      | exp (A.ConstExp k) = A.ConstExp (const k)
      | exp (A.VarExp (v, ts)) = A.VarExp (var v, ts)
      | exp (A.SeqExp (e1, e2)) = A.SeqExp (exp e1, exp e2)
      | exp (A.OverloadExp ovr) = raise Fail "todo"

    and binding (A.ValBind (p, e)) = A.ValBind (pat p, exp e)
      | binding (A.PValBind (p, e)) = A.PValBind (pat p, exp e)
      | binding (A.FunBind ls) = A.FunBind (List.map lambda ls)

    and lambda (A.FB (v1, v2, e)) = A.FB (var v1, var v2, exp e)

    and pat (A.ConPat (d, ts, p)) = A.ConPat (d, ts, pat p)
      | pat (A.TuplePat ps) = A.TuplePat (List.map pat ps)
      | pat (A.VarPat v) = A.VarPat (var v)
      | pat (A.ConstPat k) = A.ConstPat (const k)

    and const (A.DConst (d, ts)) = A.DConst (d, ts)
      | const (A.LConst (l, t)) = A.LConst (l, t)

    and overload_var (A.Unknown (t, vs)) = A.Unknown (t, List.map var vs)
      | overload_var (A.Instance v) = A.Instance (var v)

    and var_kind k = k

    and var v = v

    fun fpt m = exp m

  end
