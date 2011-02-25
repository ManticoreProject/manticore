(* flatten-terms.sml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Translate AST terms to FLAST terms without flattening. 
 * Flattening comes later.
 * docs in /path/to/manti-papers/papers/notes/amsft/
 *)

structure TranslateTermsFT = struct

  structure A = AST
  structure F = FLAST
  structure T = FTTypes

  fun trTy t = TranslateTypesFT.trTy t

  val trTyScheme : A.ty_scheme -> F.ty_scheme = 
   (fn (A.TyScheme (vs, t)) => F.TyScheme (vs, trTy t))

  val trVar : A.var -> F.var = 
   (fn (VarRep.V {name, id, kind, useCnt, ty, props}) => let
      val tschRef = !ty
      val tsch = !tschRef
      in
        VarRep.V {name = name,
		  id = id,
		  kind = ref (!kind),
		  useCnt = ref (!useCnt),
		  ty = ref (ref (trTyScheme tsch)),
		  props = props}
      end)

(* pointwise function composition *)
  infixr oo
  fun f oo g = (fn (x, y) => (f x, g y))

(* NOTE: the translation is agnostic to whether or not PTuples, PCases, etc.
 * are still around; it just translates them.
 *)
  fun trExp (e : A.exp) : F.exp = let
    fun exp (A.LetExp (b, e)) = F.LetExp (binding b, exp e)
      | exp (A.IfExp (e1, e2, e3, t)) = F.IfExp (exp e1, exp e2, exp e3, trTy t)
      | exp (A.CaseExp (e, ms, t)) = F.CaseExp (exp e, List.map match ms, trTy t)
      | exp (A.PCaseExp (es, ms, t)) = 
          F.PCaseExp (List.map exp es, List.map pmatch ms, trTy t)
      | exp (A.HandleExp (e, ms, t)) = F.HandleExp (exp e, List.map match ms, trTy t)
      | exp (A.RaiseExp (e, t)) = F.RaiseExp (exp e, trTy t)
      | exp (A.FunExp (x, e, t)) = F.FunExp (trVar x, exp e, trTy t)
      | exp (A.ApplyExp (e1, e2, t)) = F.ApplyExp (exp e1, exp e2, trTy t)
      | exp (A.VarArityOpExp (oper, n, t)) = F.VarArityOpExp (vop oper, n, trTy t)
      | exp (A.TupleExp es) = let
          val intfTy  = Types.TupleTy (List.map TypeOf.exp es)
	  in
	    F.TupleExp (List.map exp es, intfTy)
	  end
      | exp (A.RangeExp (e1, e2, optE, t)) = 
          F.RangeExp (exp e1, exp e2, Option.map exp optE, trTy t)
      | exp (A.PTupleExp es) = F.PTupleExp (List.map exp es)
      | exp (A.PArrayExp (es, t)) = trPArray (es, t) (* t is element type *)
      | exp (A.PCompExp (e, pes, optE)) = 
          F.PCompExp (exp e, List.map (pat oo exp) pes, Option.map exp optE)
      | exp (A.PChoiceExp (es, t)) = F.PChoiceExp (List.map exp es, trTy t)
      | exp (A.SpawnExp e) = F.SpawnExp (exp e)
      | exp (A.ConstExp k) = F.ConstExp (const k)
      | exp (A.VarExp (x, ts)) = F.VarExp (trVar x, List.map trTy ts)
      | exp (A.SeqExp (e1, e2)) = F.SeqExp (exp e1, exp e2)
      | exp (A.OverloadExp vr) = F.OverloadExp (ref (ov (!vr)))
      | exp (A.ExpansionOptsExp (os, e)) = F.ExpansionOptsExp (os, exp e)
    and binding (A.ValBind (p, e)) = F.ValBind (pat p, exp e)
      | binding (A.PValBind (p, e)) = F.PValBind (pat p, exp e)
      | binding (A.FunBind fs) = F.FunBind (List.map lambda fs)
      | binding (A.PrimVBind (x, r)) = F.PrimVBind (trVar x, r)
      | binding (A.PrimCodeBind c) = F.PrimCodeBind c
    and pat (A.ConPat (c, ts, p)) = F.ConPat (c, List.map trTy ts, pat p)
      | pat (A.TuplePat ps) = F.TuplePat (List.map pat ps)
      | pat (A.VarPat x) = F.VarPat (trVar x)
      | pat (A.WildPat t) = F.WildPat (trTy t)
      | pat (A.ConstPat k) = F.ConstPat (const k)
    and ppat (A.NDWildPat t) = F.NDWildPat (trTy t)
      | ppat (A.HandlePat (p, t)) = F.HandlePat (pat p, trTy t)
      | ppat (A.Pat p) = F.Pat (pat p)
    and lambda (A.FB (f, x, e)) = F.FB (trVar f, trVar x, exp e)
    and const (A.DConst (c, ts)) = F.DConst (c, List.map trTy ts)
      | const (A.LConst (k, t)) = F.LConst (k, trTy t)
    and match (A.PatMatch (p, e)) = F.PatMatch (pat p, exp e)
      | match (A.CondMatch (p, e1, e2)) = F.CondMatch (pat p, exp e1, exp e2)
    and pmatch (A.PMatch (ps, e)) = F.PMatch (List.map ppat ps, exp e)
      | pmatch (A.Otherwise (ts, e)) = F.Otherwise (List.map trTy ts, exp e)
    and ov (A.Unknown (t, xs)) = F.Unknown (trTy t, List.map trVar xs)
      | ov (A.Instance x) = F.Instance (trVar x)
    and vop A.MapP = F.MapP
    in
      exp e
    end

  and trPArray (es, eltTy) = (*
let
    val r = FlattenTypes.flatten (Basis.parrayTy eltTy)
    val fl = FTSynthOps.flatten r
    in
*)
      raise Fail "todo"
        (* FLASTUtil.mkApplyExp (fl, List.map trExp es) 
    end
*)
end
