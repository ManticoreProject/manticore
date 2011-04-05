(* fusion.sml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Fusion of flatten operators.
 *
 *)

structure FlattenOpFusion : sig

  val fuseOp  : AST.fl_op -> AST.fl_op
  val fuseExp : AST.exp -> AST.exp

end = struct

  structure A = AST
  structure T = Types 

  fun changeCell () = let
    val c = ref false
    val reset   = fn () => (c := false)
    val chg     = fn x => (c := true; x)
    val changed = fn () => !c
    in
      {reset=reset, change=chg, changed=changed}
    end

  fun fuseOp (oper : AST.fl_op) : AST.fl_op = let
    val {reset, change, changed} = changeCell ()
    fun f (id as A.ID _) = id
      | f (un as A.Unzip _) = un
      | f (cm as A.Cat _) = cm
      | f (mp as A.Map (oper, n)) = (case oper
          of A.ID t => change (A.ID (A.FArrayTy (t, n)))
	   | _ => mp)
      | f (A.Compose (o1, o2)) = 
         (case (fuseOp o1, fuseOp o2)
	   of (A.ID _, o2') => change o2'
	    | (o1', A.ID _) => change o1'
	    | (o1', o2') => A.Compose (o1', o2')
	   (* end case *))
      | f (cc as A.CrossCompose gs) = let
          val gs' = List.map fuseOp gs
          in
	    if List.all (fn A.ID _ => true | _ => false) gs'
            then let
              val t = T.TupleTy (List.map FlattenOp.typeOf gs')
	      in
                change (A.ID (T.FunTy (t, t)))
	      end
	    else A.CrossCompose gs'
	  end
    val oper' = f oper
    in
      if changed ()
        then (reset (); f oper')
        else oper'
    end

  fun fuseExp (e : A.exp) : A.exp = let
    val {reset, change, changed} = changeCell ()
    fun exp (A.LetExp (b, e)) = A.LetExp (binding b, exp e)
      | exp (A.IfExp (e1, e2, e3, t)) = A.IfExp (exp e1, exp e2, exp e3, t)
      | exp (A.CaseExp (e, ms, t)) = A.CaseExp (exp e, map match ms, t)
      | exp (A.PCaseExp (es, pms, t)) = A.PCaseExp (map exp es, map pmatch pms, t)
      | exp (A.HandleExp (e, ms, t)) = A.HandleExp (exp e, map match ms, t)
      | exp (A.RaiseExp (e, t)) = A.RaiseExp (exp e, t)
      | exp (A.FunExp (x, e, t)) = A.FunExp (x, exp e, t)
      | exp (A.ApplyExp (e1, e2, t)) = 
         (case exp e1
	    of A.FlOp (A.ID _) => change (exp e2)
            | e1' => A.ApplyExp (e1', exp e2, t)
           (* end case *))
      | exp (m as A.VarArityOpExp _) = m
      | exp (A.TupleExp es) = A.TupleExp (map exp es)
      | exp (A.RangeExp (e1, e2, oe3, t)) = A.RangeExp (exp e1, exp e2, Option.map exp oe3, t)
      | exp (A.PTupleExp es) = A.PTupleExp (map exp es)
      | exp (A.PArrayExp (es, t)) = A.PArrayExp (map exp es, t)
      | exp (A.PCompExp (e, pes, opred)) = let
	  val pes' = map (fn (p,e) => (pat p, exp e)) pes
	  val opred' = Option.map exp opred
          in
            A.PCompExp (exp e, pes', opred')
	  end
      | exp (A.PChoiceExp (es, t)) = A.PChoiceExp (map exp es, t)
      | exp (A.SpawnExp e) = A.SpawnExp (exp e) 
      | exp (k as A.ConstExp _) = k
      | exp (v as A.VarExp (x, ts)) = v
      | exp (A.SeqExp (e1, e2)) = A.SeqExp (exp e1, exp e2)
      | exp (ov as A.OverloadExp _) = ov
      | exp (A.ExpansionOptsExp (os, e)) = A.ExpansionOptsExp (os, exp e)
      | exp (A.FTupleExp es) = A.FTupleExp (List.map exp es)
      | exp (A.FArrayExp (es, n, t)) = A.FArrayExp (List.map exp es, ntree n, t)
      | exp (A.FlOp oper) = A.FlOp (fuseOp oper)
      | exp (pop as A.PArrayOp _) = pop
    and ntree (A.Lf (e1, e2)) = A.Lf (exp e1, exp e2)
      | ntree (A.Nd ns) = A.Nd (List.map ntree ns)
    and match (A.PatMatch (p, e)) = A.PatMatch (pat p, exp e)
      | match (A.CondMatch (p, cond, e)) = A.CondMatch (pat p, exp cond, exp e)
    and pmatch (A.PMatch (pps, e)) = A.PMatch (map ppat pps, exp e)
      | pmatch (A.Otherwise (ts, e)) = A.Otherwise (ts, exp e)
    and ppat (A.NDWildPat t) = A.NDWildPat t
      | ppat (A.HandlePat (p, t)) = A.HandlePat (pat p, t)
      | ppat (A.Pat p) = A.Pat (pat p)
    and pat (A.ConPat (c, ts, p)) = A.ConPat (c, ts, pat p)
      | pat (A.TuplePat ps) = A.TuplePat (List.map pat ps)
      | pat (v as A.VarPat x) = A.VarPat x		    
      | pat (A.WildPat t) = A.WildPat t
      | pat (k as A.ConstPat _) = k
    and binding (A.ValBind (p, e)) = A.ValBind (pat p, exp e)
      | binding (A.PValBind (p, e)) = A.PValBind (pat p, exp e)
      | binding (A.FunBind ls) = A.FunBind (List.map lambda ls)
      | binding (primV as A.PrimVBind _) = primV
      | binding (code as A.PrimCodeBind _) = code 
    and lambda (A.FB (f, x, e)) = A.FB (f, x, exp e)
    val e' = exp e
    in
      if changed ()
        then (reset (); exp e')
        else e'
    end

end
