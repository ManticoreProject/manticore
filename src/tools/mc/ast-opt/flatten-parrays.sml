(* flatten-parrays.sml
 *
 * COPYRIGHT (c) 2010 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

(* Stub. - ams *)

structure FlattenPArrays = struct

  structure A = AST
  structure B = Basis
  structure T = Types

  fun ty (t: A.ty) : A.ty = raise Fail "todo"

  fun var (x: A.var) : A.var = raise Fail "todo" (* change type if necessary *)

  fun exp (e: A.exp) : A.exp =
    (case e
       of A.LetExp (b, e) => A.LetExp (binding b, exp e)
	| A.IfExp (e1, e2, e3, t) => A.IfExp (exp e1, exp e2, exp e3, ty t)
        | A.CaseExp (e, ms, t) => A.CaseExp (exp e, List.map match ms, ty t)
	| A.PCaseExp (es, ms, t) => 
	    A.PCaseExp (List.map exp es, List.map pmatch ms, ty t)
	| A.HandleExp (e, ms, t) => A.HandleExp (exp e, List.map match ms, ty t)
	| A.RaiseExp (e, t) => A.RaiseExp (exp e, ty t)
	| A.FunExp (x, e, t) => A.FunExp (var x, exp e, ty t)
	| A.ApplyExp (e1, e2, t) => A.ApplyExp (exp e1, exp e2, ty t)
	| A.VarArityOpExp (oper, n, t) => A.VarArityOpExp (oper, n, ty t)
	| A.TupleExp es => A.TupleExp (List.map exp es)
	| A.RangeExp (e1, e2, optE, t) => 
	    A.RangeExp (exp e1, exp e2, Option.map exp optE, ty t)
	| A.PTupleExp es => A.PTupleExp (List.map exp es)
	| A.PArrayExp (es, t) => parray (es, t)
	| A.PCompExp (e, pes, optE) => 
	    A.PCompExp (exp e, 
			List.map (fn(p,e)=>(pat p, exp e)) pes, 
			Option.map exp optE)
	| A.PChoiceExp (es, t) => A.PChoiceExp (List.map exp es, ty t)
	| A.SpawnExp e => A.SpawnExp (exp e)
	| k as A.ConstExp _ => k
	| A.VarExp (x, ts) => A.VarExp (var x, List.map ty ts)
	| A.SeqExp (e1, e2) => A.SeqExp (exp e1, exp e2)
	| ov as A.OverloadExp _ => raise Fail "?"
	| A.ExpansionOptsExp (opts, e) => A.ExpansionOptsExp (opts, exp e)
      (* end case *))
  and binding (b: A.binding) : A.binding =
    (case b
       of A.ValBind (p, e) => A.ValBind (pat p, exp e)
	| A.PValBind (p, e) => A.PValBind (pat p, exp e)
	| A.FunBind lams => A.FunBind (List.map lambda lams)
	| A.PrimVBind (x, rhs) => A.PrimVBind (var x, rhs)
	| c as A.PrimCodeBind _ => c
      (* end case *))
  and lambda (A.FB (f, x, e)) = A.FB (var f, var x, exp e)
  and match (m: A.match) : A.match =
    (case m
       of A.PatMatch (p, e) => A.PatMatch (pat p, exp e)
	| A.CondMatch (p, e1, e2) => A.CondMatch (pat p, exp e1, exp e2)
      (* end case *))
  and pmatch (m: A.pmatch) : A.pmatch =
    (case m
       of A.PMatch (ps, e) => A.PMatch (List.map ppat ps, exp e)
	| A.Otherwise e => A.Otherwise (exp e)
      (* end case *))
  and pat (p: A.pat) : A.pat =
    (case p
       of A.ConPat (c, ts, p) => A.ConPat (c, List.map ty ts, pat p)
	| A.TuplePat ps => A.TuplePat (List.map pat ps)
	| A.VarPat x => A.VarPat (var x)
	| A.WildPat t => A.WildPat (ty t)
	| A.ConstPat _ => p
      (* end case *))
  and ppat (p: A.ppat) : A.ppat =
    (case p
       of A.NDWildPat t => A.NDWildPat (ty t)
	| A.HandlePat (p, t) => A.HandlePat (pat p, ty t)
	| A.Pat p => A.Pat (pat p)
      (* end case *))
  and parray (es: A.exp list, t: A.ty) = raise Fail "todo"

  fun translate (e: A.exp) : A.exp = exp e

end

