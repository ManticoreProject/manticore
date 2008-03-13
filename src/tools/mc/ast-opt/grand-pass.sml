(* grand-pass.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * This is an AST-to-AST pass that links together all the ast-opt passes. 
 *
 * This pass currently includes the following AST-to-AST translations:
 * - translation of parallel tuples into futures/touches
 * - rewriting of sumP into sumPQ
 *)

structure GrandPass : sig

    val transform : AST.module -> AST.module

  end = struct

    structure A = AST
    structure B = Basis
    structure F = Futures
    structure V = Var
    structure U = UnseenBasis

  (* trExp : exp -> exp *)
    fun trExp (A.LetExp (b, e)) = A.LetExp (binding b, trExp e)
      | trExp (A.IfExp (e1, e2, e3, t)) = A.IfExp (trExp e1, trExp e2, trExp e3, t)
      | trExp (A.CaseExp (e, ms, t)) = A.CaseExp (trExp e, map match ms, t)
      | trExp (A.HandleExp (e, ms, t)) = A.HandleExp (trExp e, map match ms, t)
      | trExp (A.RaiseExp (e, t)) = A.RaiseExp (trExp e, t)
      | trExp (A.FunExp (x, e, t)) = A.FunExp (x, trExp e, t)
      | trExp (A.ApplyExp (e1, e2, t)) = A.ApplyExp (trExp e1, trExp e2, t)
      | trExp (m as A.VarArityOpExp _) = m
      | trExp (A.TupleExp es) = A.TupleExp (map trExp es)
      | trExp (A.RangeExp (e1, e2, oe3, t)) = 
	  A.RangeExp (trExp e1, trExp e2, Option.map trExp oe3, t)
      | trExp (ptup as A.PTupleExp es) = 
	  (case trPTup es
	     of SOME e => e
	      | NONE => A.TupleExp (map trExp es)
	    (* end case *))
      | trExp (A.PArrayExp (es, t)) = A.PArrayExp (map trExp es, t)
      | trExp (A.PCompExp (e, pes, oe)) = trPComp (e, pes, oe)
      | trExp (A.PChoiceExp (es, t)) = A.PChoiceExp (map trExp es, t)
      | trExp (A.SpawnExp e) = A.SpawnExp (trExp e)
      | trExp (k as A.ConstExp _) = k
      | trExp (v as A.VarExp (x, ts)) = v
(*	  (case trVar (x, ts)
	     of SOME e => e
	      | NONE => v) *)
      | trExp (A.SeqExp (e1, e2)) = A.SeqExp (trExp e1, trExp e2)
      | trExp (x as A.OverloadExp _) = x

    and binding (A.ValBind (p, e)) = A.ValBind (p, trExp e)
      | binding (A.PValBind (p, e)) = A.PValBind (p, trExp e)
      | binding (A.DValBind (p, e)) = A.DValBind (p, trExp e)
      | binding (A.FunBind lams) = A.FunBind (map lambda lams)

    and lambda (A.FB (f, x, e)) = A.FB (f, x, trExp e)

    and match (A.PatMatch (p, e)) = A.PatMatch (p, trExp e)
      | match (A.CondMatch (p, e1, e2)) = A.CondMatch (p, trExp e1, trExp e2)

    and trPTup arg = TranslatePtup.tr trExp arg

    and trPComp arg = TranslatePComp.tr trExp arg

  (* transform : A.module -> A.module *)
    fun transform (A.Module {exns, body}) = let
      val body' = trExp body
      in
        A.Module {exns = exns, body = body'}
      end

  end
