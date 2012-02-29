(* seg-reduce.sml
 *
 * COPYRIGHT (c) 2012 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Find terms that look like this:
 *  [| reduceUncurried (oper, ident, ns) | ns in nss |]
 * and turn them into this:
 *  PArray.segReduce (oper, ident, nss)
 *
 * Type-preserving translation.
 *)

structure SegReduce : sig

  val translate : AST.exp -> AST.exp

end = struct

  structure A = AST

  fun isReduceU e =
   (case e
      of A.VarExp (x, ts) => let
           val reduceUncurriedVar = DelayedBasis.Var.parrayReduce ()
           in
             Var.same (x, reduceUncurriedVar)
           end
       | _ => false)

  fun translate e = let
    fun exp (A.PCompExp (e, pes, optE)) = let
          val e' = translate e
          val pes' = List.map (fn (p,e) => (p, translate e)) pes
          val optE' = Option.map translate optE
          in case e'
            of A.ApplyExp (f, args, t) =>
                 if (isReduceU f) then
                   raise Fail "SegReduce.translate -- todo: FOUND ONE!!!!"
                 else
                   A.PCompExp (e', pes', optE')
             | _ => A.PCompExp (e', pes', optE')
          end
      | exp (A.LetExp (b,e)) =
          A.LetExp(translateBinding b,
                   translate e)
      | exp (A.IfExp (e1,e2,e3,t)) =
          A.IfExp (translate e1,
                   translate e2,
                   translate e3,
                   t)
      | exp (A.CaseExp (e,ms,t)) =
          A.CaseExp (translate e,
                     List.map translateMatch ms,
                     t)
      | exp (A.PCaseExp (es,pms,t)) =
          A.PCaseExp (List.map translate es,
                      List.map translatePMatch pms,
                      t)
      | exp (A.HandleExp (e,ms,t)) =
          A.HandleExp (translate e,
                       List.map translateMatch ms,
                       t)
      | exp (A.RaiseExp (e,t)) =
          A.RaiseExp (translate e,
                      t)
      | exp (A.FunExp (v,e,t)) =
          A.FunExp (v,
                    translate e,
                    t)
      | exp (A.ApplyExp (e1,e2,t)) =
          A.ApplyExp (translate e1,
                      translate e2,
                      t)
      | exp (A.TupleExp (es)) =
          A.TupleExp (List.map translate es)
      | exp (A.RangeExp (e1, e2, optE, t)) =
          A.RangeExp (translate e1,
                      translate e2,
                      Option.map translate optE,
                      t)
      | exp (A.PTupleExp (es)) =
          A.PTupleExp (List.map translate es)
      | exp (A.PArrayExp (es,t)) =
          A.PArrayExp (List.map translate es,
                       t)
      | exp (A.PChoiceExp (es,t)) =
          A.PChoiceExp (List.map translate es,
                        t)
      | exp (A.SpawnExp (e)) =
          A.SpawnExp (translate e)
      | exp (A.SeqExp (e1,e2)) =
          A.SeqExp (translate e1,
                    translate e2)
      | exp (A.ExpansionOptsExp (eos,e)) =
          A.ExpansionOptsExp (eos,
                              translate e)
      | exp e = e
    in
      exp e
    end

  and translateBinding (A.ValBind(p,e)) = A.ValBind(p, translate e)
    | translateBinding (A.PValBind(p,e)) = A.PValBind(p, translate e)
    | translateBinding b = b

  and translateMatch (A.PatMatch(p,e)) = A.PatMatch(p, translate e)
    | translateMatch (A.CondMatch(p,e1,e2)) = A.CondMatch(p, translate e1, translate e2)

  and translatePMatch (A.PMatch(ps,e)) = A.PMatch(ps, translate e)
    | translatePMatch (A.Otherwise(ts, e)) = A.Otherwise(ts, translate e)

end
