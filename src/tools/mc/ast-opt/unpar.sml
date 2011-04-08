(* unpar.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * The function unparTup recursively turns parallel tuples into sequential tuples.
 *)

structure Unpar : sig

  (* translate parallel tuples into tuples *)
  (* FIXME These are the same. Are both called? *)
    val unparTup      : AST.exp -> AST.exp
    val unparTupInExp : AST.exp -> AST.exp

  (* translate parallel expressions into their sequential counterparts *)
    val unpar : AST.exp -> AST.exp

  end = struct

    structure A = AST

  (* FIXME Does any other code use this? *)
  (* This is not a complete (or good) representation of options of interest. *)
  (* - ams *)
    datatype unpar_opts 
      = PTUP 
      | ALL

  (* FIXME Don't *all* types need to be translated? *)
    fun unparExp (opts: unpar_opts) : A.exp -> A.exp = let 
      fun exp (A.LetExp (b, e)) = A.LetExp (binding b, exp e)
	| exp (A.IfExp (e1, e2, e3, t)) = A.IfExp (exp e1, exp e2, exp e3, t)
	| exp (A.CaseExp (e, ms, t)) = A.CaseExp (exp e, map match ms, t)
	| exp (A.PCaseExp (es, ms, ty)) = 
            (case opts 
	       of ALL => pcase (es, ms, ty)
		| PTUP => A.PCaseExp (List.map exp es, List.map pmatch ms, ty)
	      (* end case *))
	| exp (A.HandleExp (e, ms, t)) = A.HandleExp (exp e, map match ms, t)
	| exp (A.RaiseExp (e, t)) = A.RaiseExp (exp e, t)
	| exp (A.FunExp (x, e, t)) = A.FunExp (x, exp e, t)
	| exp (A.ApplyExp (e1, e2, t)) = A.ApplyExp (exp e1, exp e2, t)
	| exp (m as A.VarArityOpExp _) = m
	| exp (A.TupleExp es) = A.TupleExp (map exp es)
	| exp (A.RangeExp (e1, e2, oe3, t)) = let
	    val e1' = exp e1
	    val e2' = exp e2
	    val oe3' = Option.map exp oe3
	    val t' = Elaborate.trTy t
            in
	      TranslateRange.tr (e1', e2', oe3', t')
	    end
	| exp (A.PTupleExp es) = A.TupleExp (map exp es)
	| exp (A.PArrayExp (es, t)) = let
	    val a = ParrLitToRope.tr (map exp es, Elaborate.trTy t)
          (* rewrites with parallel tuples *)
	    in
	      exp a
	    end
	| exp (e as A.PCompExp (e', pes, oe)) = 
	    TranslatePComp.tr exp (e', pes, oe)
	| exp (A.PChoiceExp (es, t)) = A.PChoiceExp (map exp es, t)
	| exp (A.SpawnExp e) = A.SpawnExp (exp e)
	| exp (k as A.ConstExp c) = k
	| exp (x as A.VarExp (v, ts)) = x
	| exp (A.SeqExp (e1, e2)) = A.SeqExp (exp e1, exp e2)
	| exp (A.OverloadExp ovr) = A.OverloadExp ovr
	| exp (A.ExpansionOptsExp(opts, e)) = A.ExpansionOptsExp(opts, exp e)
	| exp (A.FTupleExp es) = A.FTupleExp (List.map exp es)
	| exp (A.FArrayExp (es, n, t)) = A.FArrayExp (List.map exp es, ntree n, t)
	| exp (A.FlOp oper) = A.FlOp oper
	| exp (A.PArrayOp oper) = raise Fail "PArrayOp"
      and ntree (A.Lf (e1, e2)) = A.Lf (exp e1, exp e2)
	| ntree (A.Nd ns) = A.Nd (List.map ntree ns)
      and pcase (es, ms, ty) = let
            fun pm (A.PMatch (ps, e)) = let
	          val ps' = List.map ppat ps
                  in
                    A.PatMatch (A.TuplePat ps', exp e)
                  end
	      | pm (A.Otherwise (ts, e)) = let
		  val ws = List.map (A.WildPat o TypeOf.exp) es
                  in
	            A.PatMatch (A.TuplePat ws, exp e)
		end
            val tup = A.TupleExp (List.map exp es)
            in
              A.CaseExp (tup, List.map pm ms, ty)
            end
      and ppat (A.NDWildPat ty) = A.WildPat ty
	| ppat (A.HandlePat (p, ty)) = raise Fail "not supported: HandlePat"
	| ppat (A.Pat p) = p
      and binding (A.ValBind (p, e)) = A.ValBind (p, exp e)
	| binding (A.PValBind (p, e)) = 
	  (* eliminate pvals *)
            (case opts
	       of ALL => A.ValBind (p, exp e)
		| _   => A.PValBind (p, exp e)
	      (* end case *))
	| binding (A.FunBind lams) = A.FunBind (map lambda lams)
	| binding (A.PrimVBind (v, code)) = A.PrimVBind (v, code)
	| binding (A.PrimCodeBind code) = A.PrimCodeBind code
      and lambda (A.FB (f, x, e)) = A.FB (f, x, exp e)
      and match (A.PatMatch (p, e)) = A.PatMatch (p, exp e)
	| match (A.CondMatch (p, e1, e2)) = A.CondMatch (p, exp e1, exp e2)
    (* This pmatch function leaves the pmatches as such;    *)
    (*  inside the pcase function they are translated away. *)
      and pmatch (A.PMatch (ps, e)) = A.PMatch (ps, exp e)
	| pmatch (A.Otherwise (ts, e)) = A.Otherwise (ts, exp e) 
      in
	exp
      end

(* FIXME these two are the same (?) ...*)
    fun unparTupInExp e = unparExp PTUP e
    fun unparTup body   = unparExp PTUP body

    fun unpar body = unparExp ALL body

  end
