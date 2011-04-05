(* flatten-terms.sml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Flatten AST types.
 *
 *)

structure FlattenTerms : sig

  val flatten : AST.exp -> AST.exp

end = struct

  structure A = AST
  structure T = Types 
  structure B = Basis

  structure AU = ASTUtil
  structure TU = TypeUtil

  structure FEnv = FlattenEnv

  type env = FEnv.env

  fun assert (msg : string) (fact : bool) : unit = 
    if not fact then raise Fail ("assertion failure: " ^ msg) else ()

  infixr @@ (* infix pair map operator *)
  fun f  @@ (a, b) = (f a, f b)

(* fpTy : env -> ty -> ty *)
(* flatten and prune type *)
  fun fpTy env = FlattenTypes.flattenTy env o TU.prune

(* fpTySch : env -> ty_scheme -> ty_scheme *)
(* flatten and prune type scheme *)
  fun fpTySch env = (fn T.TyScheme (vs, t) => T.TyScheme (vs, fpTy env t))

(* exp : env -> exp -> exp *)
  fun exp (env : env) (e : A.exp) : A.exp = let
    val ty = fpTy env
    fun ex (A.LetExp (b, e)) = let
          val (env', b') = binding env b
          val e' = exp env' e
          in
	    A.LetExp (b', e')
          end
      | ex (A.IfExp (e1, e2, e3, t)) = A.IfExp (ex e1, ex e2, ex e3, ty t)
      | ex (A.CaseExp (e, ms, t)) = let
          val e' = ex e
	  val ms' = List.map (match env) ms
	  val t' = ty t
          in
	    A.CaseExp (e', ms', t')
	  end
      | ex (A.HandleExp (e, ms, t)) = let
          val e' = ex e
	  val ms' = List.map (match env) ms
	  val t' = ty t
          in
	    A.HandleExp (e', ms', t')
	  end
      | ex (A.RaiseExp (e, t)) = A.RaiseExp (ex e, ty t)
      | ex (A.FunExp (x, e, t)) = let
          val (env', x') = var env x
          val e' = exp env' e
	  val t' = ty t
          in
	    A.FunExp (x', e', t')
	  end
      | ex (A.ApplyExp (e1, e2, t)) = let
	  val (e1', e2') = ex @@ (e1, e2)
          val t' = ty t
          in
	    A.ApplyExp (e1', e2', t')
	  end
      | ex (A.VarArityOpExp _) = raise Fail "todo"
      | ex (A.TupleExp es) = A.TupleExp (List.map ex es)
      | ex (A.RangeExp (e1, e2, optE, t)) = let
          val (e1', e2') = ex @@ (e1, e2)
	  val optE' = Option.map ex optE
	  val t' = ty t
	  in
	    A.RangeExp (e1', e2', optE', t')
	  end
      | ex (A.PTupleExp es) = A.PTupleExp (List.map ex es)
      | ex (A.PArrayExp ([], t)) = let
          val lf = A.Lf (AU.mkInt 0, AU.mkInt 1)
          in
	    A.FArrayExp ([], lf, ty t)
	  end
      | ex (A.PArrayExp (es, t)) = let
	  val r = ty t
	  val _ = print "\ntraversing parray\n"          
	  val _ = print ("```````````` t is " ^ TU.toString t ^ "\n")
	  val _ = print ("```````````` r is " ^ TU.toString r ^ "\n")
	  val es' = List.map ex es
        (* check that r is in fact the flattened element type *)
          val _ = (case es'
            of [] => ()
	     | e'::_ => let
                 val t' = TypeOf.exp e'
                 in
                   if TypeUtil.same (r,t') then () 
		   else 
		    (print "flattening parray type mismatch\n";
		     print "e' = ";
		     PrintAST.printExp e';
		     print ("r = " ^ TU.toString r ^ "\n");
		     print ("t' = " ^ TU.toString t' ^ "\n");
		     raise Fail "flattening parray type mismatch")
	         end) 
	  val lf = A.Lf (AU.mkInt 0, AU.mkInt (List.length es))
	  val f = A.FArrayExp (es', lf, r)
	  val r' = TU.deepPrune r
	  val _ = print ("``````` constructing flattening operator for {" ^ TU.toString r' ^ ",lf}\n")
	  val oper = FlattenOp.construct r'
	  val _ = print ("`````````````` made " ^ FlattenOp.toString oper ^ "\n")
	  val _ = print ("``````` done constructing flattening op for {" ^ TU.toString r' ^ ",lf}\n")
        (* record the insertion of this operator in env *)
	  val () = FEnv.insertFlOp (env, oper)
        (* check the types before we construct the application *)
	  val () = let
            val opTy = FlattenOp.typeOf oper
	    val argTy = TU.deepPrune (TypeOf.exp f)
            in case TU.deepPrune opTy
              of T.FunTy (domTy, rngTy) => 
                   if TU.same (domTy, argTy) then ()
		   else let
	             val _ = print "type mismatch inserting flattener:\n"
                     val _ = print ("operator " ^ FlattenOp.toString oper ^ "\n")
		     val _ = print ("oper's domTy " ^ TU.toString domTy ^ "\n")
		     val _ = print ("argTy " ^ TU.toString argTy ^ "\n")
                     in
	               raise Fail "type mismatch in application"
		     end
	       | _ => raise Fail "not a fn"
	    end
          in
	    AU.mkApplyExp (A.FlOp oper, [f])
	  end
      | ex (A.PCompExp (e, pes, optE)) = raise Fail "todo"
      | ex (A.PChoiceExp (es, t)) = A.PChoiceExp (List.map ex es, ty t)
      | ex (A.SpawnExp e) = A.SpawnExp (ex e)
      | ex (A.ConstExp c) = A.ConstExp (const env c)
      | ex (ve as A.VarExp (x, ts)) = 
        (* replace parr prims with parr ops *)
         (case FEnv.findParrPrim (env, x)
	   of SOME replacement => (case replacement
             of NONE => ve
	      | SOME pop => let
                  val t = (ty o TU.domainType o TypeOf.exp) ve
(* +debug *)
		  val _ = print ("!!!!! TYPE OF " ^ Var.toString x ^ " " ^ 
				 TU.toString (TypeOf.exp ve) ^ "\n")
		  val _ = print ("!!!!! FLAT TYPE OF " ^ Var.toString x ^ " " ^ 
				 TU.toString t ^ "\n")
(* -debug *)
                  in
		    A.PArrayOp (pop t)
		  end
             (* end case *))
	    | NONE => let
                val ts' = List.map ty ts
		val _ = () (* print ("+++++ inspecting " ^ Var.toString x ^ "\n") *)
                in case FEnv.findVar (env, x)
		  of NONE => let
                       val _ = () (* print ("found nothing\n") *)
		       in 
			 A.VarExp (x, ts')
		       end
		   | SOME y => let
		       val _ = () (* print ("found " ^ Var.toString y ^ "\n") *)
		       in
			 A.VarExp (y, ts')
		       end
	        end
	   (* end case *))
      | ex (A.SeqExp (e1, e2)) = A.SeqExp (ex @@ (e1, e2))
      | ex (A.OverloadExp xr) = raise Fail "unresolved overloading"
      | ex (A.ExpansionOptsExp (opts, e)) = A.ExpansionOptsExp (opts, ex e)
      | ex (A.FTupleExp es) = raise Fail "exp: FTupleExp"
      | ex (A.FArrayExp (es, n, t)) = raise Fail "exp: FArrayExp"
      | ex (oper as A.FlOp _) = oper
      | ex (A.PArrayOp _) = raise Fail "unexpected PArrayOp"
    in
      ex e
    end

  and binding (env : env) (b : A.binding) : env * A.binding =
   (case b
     of A.ValBind (p, e) => let
          val (env', p') = pat env p
          val e' = exp env e
          in
	    (env', A.ValBind (p', e'))
	  end
      | A.PValBind (p, e) => let
          val (env', p') = pat env p
          val e' = exp env e
          in
            (env', A.PValBind (p', e'))
	  end
      | A.FunBind lams => let
          val (env', lams') = lambdas env lams
          in
            (env', A.FunBind lams')
          end
      | A.PrimVBind (x, rhs) => let
          (* since we can't flatten the rhs, register x as mapping to itself, *)
          (*   leaving its type unchanged *)
          val env' = FEnv.insertVar (env, x, x)
	  val _ = Var.setInterfaceTy (x, Var.typeOf x);
          in
	    (env', b)
	  end
      | A.PrimCodeBind c => (env, b)
    (* end case *))

  and match (env : env) (m : A.match) : A.match =
   (case m
     of A.PatMatch (p, e) => let
          val (env', p') = pat env p
	  val e' = exp env' e
          in
	    A.PatMatch (p', e')
	  end
      | A.CondMatch (p, e1, e2) => let
          val (env', p') = pat env p
	  val (e1', e2') = (exp env') @@ (e1, e2)
          in
	    A.CondMatch (p', e1', e2')
	  end          
    (* end case *))

  and pmatch (env : env) (m : A.pmatch) : A.pmatch =
   (case m
     of A.PMatch (ps, e) => let
          val (env', ps') = ppats env ps
	  val e' = exp env' e
          in
	    A.PMatch (ps', e')
	  end
      | A.Otherwise (ts, e) => let
          val ts' = List.map (fpTy env) ts
	  val e' = exp env e
          in
	    A.Otherwise (ts', e')
	  end
    (* end case *))

  and ppats (env : env) (ps : A.ppat list) : env * A.ppat list = let
    fun ppat env =
     (fn A.NDWildPat t => (env, A.NDWildPat (fpTy env t))
       | A.HandlePat (p, t) => let
           val (env', p') = pat env p
	   val t' = fpTy env t
	   in
	     (env', A.HandlePat (p', t'))
	   end
       | A.Pat p => let
	   val (env', p') = pat env p
	   in
	     (env', A.Pat p')
	   end
      (* end fn *))
    fun lp ([], acc, env') = (env', rev acc)
      | lp (p::ps, acc, env') = let 
          val (env'', p') = ppat env p
          in
	    lp (ps, p'::acc, env'')
	  end
    in
      lp (ps, [], env)
    end

  and lambdas (env : env) (lams : A.lambda list) : env * A.lambda list = let
    fun parrPrim (A.FB (f, x, b)) = FEnv.isParrPrim (env, f)
    fun go ls = let
    (* we proceed in two passes -- first, translate all the fn names, *)
    (*   then translate all the function args and bodies *)
    (* first pass: bindLp translates and collects all the function names *)
      fun bindLp ([], env, fs) = (env, rev fs)
	| bindLp (A.FB(f,x,b)::t, env, fs) = let
            val (env', f') = var env f
            in
              bindLp (t, env', (f',x,b)::fs)
	    end
      val (env', fs) = bindLp (lams, env, [])
    (* second pass operator: trFun translates a function's arg and body *)
      fun trFun (f', x, b) : A.lambda = let
        val (env'', x') = var env' x
	val b' = exp env'' b
        in
	  A.FB (f', x', b')
	end
    (* second pass *)
      val lams' : A.lambda list = List.map trFun fs
      in
        (env':env, lams':A.lambda list)
      end
    in
      case lams 
       of [lam] => if parrPrim lam then (env, [lam]) else go lams
	| _ => if List.exists parrPrim lams then
                 raise Fail "unexpected mutually recursive parr prims"
	       else go lams
    end

  and pat (env : env) (p : A.pat) : env * A.pat =
   (case p
     of A.ConPat (dcon, ts, p) => let
          val dcon' = (case FEnv.findDCon (env, dcon)
			of NONE => dcon
			 | SOME c => c) (* FIXME think about this... *)
			                (* right now only dcons with args are in env... *)
			                (* therefore if it's not in the env it _should_ mean *)
			                (* there's nothing about it to flatten...*)
	                                (* -- maybe "unflattenable" dcons should just map to themselves *)
          val ts' = List.map (fpTy env) ts
          val (env', p') = pat env p
          in
	    (env', A.ConPat (dcon', ts', p'))
	  end
      | A.TuplePat [] (* unit *) => (env, p)
      | A.TuplePat ps => let
	  fun lp ([], acc, env') = (env', List.rev acc)
	    | lp (p::ps, acc, env') = let
                val (env'', p') = pat env' p
                in
		  lp (ps, p'::acc, env'')
	        end
	  val (env', ps') = lp (ps, [], env)
          in
	    (env', A.TuplePat ps')
	  end
      | A.VarPat x => let
          val (env', x') = var env x
          in
	    (env', A.VarPat x')
	  end
      | A.WildPat t => (env, A.WildPat (fpTy env t))
      | A.ConstPat c => (env, A.ConstPat (const env c))
    (* end case *))

(* this function deals with vars at their binding sites *)
  and var (env : env) (x : A.var) : env * A.var = let
    val tySch = Var.typeOf x
    val needsFl = FlattenTypes.mustFlattenTyScheme (env, tySch)
    in 
      if needsFl then let
        val tySch' = fpTySch env tySch
        val x' = Var.newPoly (Var.nameOf x ^ "_", tySch')
        val env' = FEnv.insertVar (env, x, x')
        val () = (* record interface type on new var *)
	  Var.setInterfaceTy (x', tySch)
	val _ = print ("flatten-terms: new var " ^ Var.nameOf x' ^ " with new type " ^
		       TypeUtil.schemeToString tySch' ^ "\n")
        in
	  (env', x')
	end
      else 
        (env, x)
    end

  and const (env : env) (c : A.const) : A.const = 
   (case c
     of A.DConst (dcon, ts) => let
          val ts' = List.map (fpTy env) ts
	  val dcon' = (case FEnv.findDCon (env, dcon)
	    of SOME dcon' => dcon'
	     | NONE => dcon
            (* end case *))
	  in 
	    A.DConst (dcon', ts')
	  end
      | A.LConst (l, t) => A.LConst (l, t) (* a literal's type shouldn't require flattening *)
    (* end case *))

  fun flatten (e : A.exp) : A.exp = let
    val env = FlattenEnv.mkEnv ()
    val e' = exp env e
    in
      e'
    end

end
