structure LTCPVal : sig

    val transform : AST.module -> AST.module

  end = struct

    structure A = AST
    structure B = Basis

    fun splitCtx (pvEnv, ctx, e) = (case e
        of A.ApplyExp (e1, e2, ty) => let
	   fun ctx1 (e) = A.ApplyExp (e, e2, ty)
	   fun ctx2 (e) = A.ApplyExp (e1, e, ty)
           in
	       (case splitCtx (pvEnv, ctx1, e1)
		 of NONE => splitCtx (pvEnv, ctx2, e2)
		  | split => split
	       (* end case *))
           end
	 | A.VarExp (v, tys) => (case Var.Map.lookup (pvEnv, v)
           of NONE => NONE
	    | SOME _ => SOME (v, tys, ctx)
           (* end case *))
        (* end case *))

   (* build an environment mapping pvals to their state components (done flags and ivars) *)
    fun rememberPVals (e) = let
        val pvEnv = ref Var.Map.empty
        fun ins (v) = let
            val doneFlg = raise Fail ""
            val ivar = raise Fail ""
            in
	       pvEnv := Var.Map.insert(!pvEnv, v, (doneFlg, ivar))
            end
        fun exp (e) = (case e
             of A.LetExp (bind, e) => (case bind
                of A.PValBind (pat, e) => ((case pat
                   of A.VarPat v => ins(v)
		    | (A.WildPat _ | A.ConstPat _ ) => ()
		    | _ => raise Fail "todo"
		   (* end case *));
                   exp(e))
		 | _ => ()
                (* end case *))
             (* end case *))
        in
	   exp(e);
	   !pvEnv
        end (* rememberPVals *)

    fun rewriteLambda (pvEnv, fb as A.FB (f, p, e)) = (case splitCtx (pvEnv, fn x => x, e)
        of NONE => fb
	 | SOME _ => raise Fail "todo"
        (* end case *))

    fun transform (A.Module {exns, body}) = let
	val env = rememberPVals(body)
        in
	    raise Fail "todo"
        end (* transform *)

  end (* LTCPVal *)
