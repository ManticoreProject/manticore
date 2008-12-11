(* elaborate.sml
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

structure Elaborate : sig

    val elaborate : AST.exp -> AST.exp

  end = struct

    structure A = AST
    structure B = Basis
    structure T = Types
    structure V = Var

  (* trTy : ty -> ty *)
  (* For now, this function just translates the parray type to the rope type. *)
  (* It could serve arbitrary other purposes easily enough. *)
    fun trTy t = let
      fun isPArrayTyc (c as T.Tyc _) = TyCon.same (c, Basis.parrayTyc)
      val ropeTyc = (case BasisEnv.getTyFromBasis ["Ropes", "rope"]
		       of ModuleEnv.TyCon c => c
			| _ => raise Fail "expected ModuleEnv.ty_def TyCon variant"
		       (* end case *))
      fun parrayTycToRopeTyc (T.Tyc {params, ...}) = let
        val (T.Tyc {stamp, name, arity, props, def, ...}) = ropeTyc
        in
          T.Tyc {stamp=stamp, name=name, arity=arity, 
		 params=params, props=props, def=def}
        end
      fun ty (e as T.ErrorTy) = e
	| ty (T.MetaTy m) = T.MetaTy (meta m)
	| ty (a as T.VarTy _) = a
	| ty (T.ConTy (ts, c)) = T.ConTy (map ty ts, tycon c)
	| ty (T.FunTy (t0, t1)) = T.FunTy (ty t0, ty t1)
	| ty (T.TupleTy ts) = T.TupleTy (map ty ts)
      and meta (m as T.MVar {stamp, info}) = (info := meta_info (!info); m)
      and meta_info (T.INSTANCE t) = T.INSTANCE (ty t)
	| meta_info i = i
      and tycon c = if isPArrayTyc c then parrayTycToRopeTyc c else c
      in
        ty t
      end

    and trTyScheme (T.TyScheme (tvs, t)) = T.TyScheme (tvs, trTy t)

  (* trExp : exp -> exp *)
    and trExp (A.LetExp (b, e)) = A.LetExp (trBinding b, trExp e)
      | trExp (A.IfExp (e1, e2, e3, t)) = A.IfExp (trExp e1, trExp e2, trExp e3, trTy t)
      | trExp (A.CaseExp (e, ms, t)) = A.CaseExp (trExp e, map trMatch ms, trTy t)
      | trExp (A.PCaseExp (es, pms, t)) = trPCase (map trExp es, pms, trTy t)
      | trExp (A.HandleExp (e, ms, t)) = A.HandleExp (trExp e, map trMatch ms, trTy t)
      | trExp (A.RaiseExp (e, t)) = A.RaiseExp (trExp e, trTy t)
      | trExp (A.FunExp (x, e, t)) = A.FunExp (trVar x, trExp e, trTy t)
      | trExp (A.ApplyExp (e1, e2, t)) = A.ApplyExp (trExp e1, trExp e2, trTy t)
      | trExp (A.VarArityOpExp (m, n, t)) = A.VarArityOpExp (m, n, trTy t)
      | trExp (A.TupleExp es) = A.TupleExp (map trExp es)
      | trExp (A.RangeExp (e1, e2, oe3, t)) = 
	  A.RangeExp (trExp e1, trExp e2, Option.map trExp oe3, trTy t)
      | trExp (ptup as A.PTupleExp es) = 
	  (case trPTup es
	     of SOME e => e
	      | NONE => A.TupleExp (map trExp es)
	    (* end case *))
      | trExp (A.PArrayExp (es, t)) = ParrLitToRope.tr(map trExp es, trTy t)
      | trExp (A.PCompExp (e, pes, oe)) = trPComp (e, pes, oe)
      | trExp (A.PChoiceExp (es, t)) = A.PChoiceExp (map trExp es, trTy t)
      | trExp (A.SpawnExp e) = A.SpawnExp (trExp e)
      | trExp (k as A.ConstExp _) = k
      | trExp (A.VarExp (x, ts)) = A.VarExp (trVar x, map trTy ts)
(*	  (case trVar (x, ts)
	     of SOME e => e
	      | NONE => v) *)
      | trExp (A.SeqExp (e1, e2)) = A.SeqExp (trExp e1, trExp e2)
      | trExp (ov as A.OverloadExp xRef) = (xRef := trOverloadVar (!xRef); ov)
      | trExp (A.ExpansionOptsExp (opts, e)) = A.ExpansionOptsExp (opts, trExp e)

(*    withtype var = (var_kind, ty_scheme ref) VarRep.var_rep *)

    and trVar x = let
      val y = RopeOps.tr x
      val (VarRep.V {ty, ...}) = y
      in
        (ty := ref (trTyScheme (!(!ty))); y)
      end

    and trBinding (A.ValBind (p, e)) = A.ValBind (trPat p, trExp e)
      | trBinding (A.PValBind (p, e)) = A.PValBind (trPat p, trExp e)
      | trBinding (A.FunBind lams) = A.FunBind (map trLambda lams)
      | trBinding (A.PrimVBind (v, prim)) = A.PrimVBind (trVar v, prim)
      | trBinding (A.PrimCodeBind pc) = A.PrimCodeBind pc

    and trPat (A.ConPat (c, ts, p)) = A.ConPat (c, map trTy ts, trPat p)
      | trPat (A.TuplePat ps) = A.TuplePat (map trPat ps)
      | trPat (A.VarPat x) = A.VarPat (trVar x)
      | trPat (A.WildPat t) = A.WildPat (trTy t)
      | trPat (k as A.ConstPat _) = k

    and trOverloadVar (A.Unknown (t, xs)) = A.Unknown (trTy t, xs)
      | trOverloadVar (x as A.Instance _) = x 

    and trLambda (A.FB (f, x, e)) = A.FB (f, x, trExp e)

    and trMatch (A.PatMatch (p, e)) = A.PatMatch (p, trExp e)
      | trMatch (A.CondMatch (p, e1, e2)) = A.CondMatch (p, trExp e1, trExp e2)

    and trPTup arg = TranslatePtup.tr trExp arg

    and trPComp arg = TranslatePComp.tr trExp arg

    and trPCase arg = TranslatePCase.tr trExp arg

  (* elaborate : A.exp -> A.exp *)
    fun elaborate body = trExp body

  end
