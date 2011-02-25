(* flatten-translate-terms.sml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Translate AST terms to FLAST terms without flattening. 
 * Flattening comes later.
 * docs in /path/to/manti-papers/papers/notes/amsft/
 *)

structure FlattenTranslateTerms = struct

  structure F = FLAST

  structure ATy = Types
  structure FTy = FTTypes

  structure FE   = FlattenEnv
  structure FTTy = FlattenTranslateTypes

  type env = FE.env

  val trTy = FTTy.trTy
  val trDCon = FTTy.trDCon
  
  val trScheme : env * ATy.ty_scheme -> F.ty_scheme = FTTy.trScheme

  fun trVar (env : FE.env, x : AST.var) : F.var * FE.env = let
    val fx = FTVar.newPoly (Var.nameOf x,
			    trScheme (env, Var.typeOf x))
(* newPoly takes ty_schemes, so I'm using it. -ams *)
    in
      (fx, FE.insertVar (env, x, fx))
    end

(* NOTE: the translation is agnostic to whether or not PTuples, PCases, etc.
 * are still around; it just translates them.
 *)
  fun trExp (env : env, e : AST.exp) : F.exp = let
    fun ty t = trTy (env, t)
    fun exp (AST.LetExp (b, e)) = let
          val (b', env') = trBind (env, b)
	  val e' = trExp (env', e)
          in
	    F.LetExp (b', e')
          end 
      | exp (AST.IfExp (e1, e2, e3, t)) = 
          F.IfExp (exp e1, exp e2, exp e3, ty t)
      | exp (AST.CaseExp (e, ms, t)) = 
          F.CaseExp (exp e, List.map (trMatch env) ms, ty t)
      | exp (AST.PCaseExp (es, ms, t)) = 
          F.PCaseExp (List.map exp es, List.map (trPMatch env) ms, ty t) 
      | exp (AST.HandleExp (e, ms, t)) = 
          F.HandleExp (exp e, List.map (trMatch env) ms, ty t) 
      | exp (AST.RaiseExp (e, t)) = F.RaiseExp (exp e, ty t)
      | exp (AST.FunExp (x, e, t)) = let
          val (x', env') = trVar (env, x)
          val e' = trExp (env', e)
          in
	    F.FunExp (x', e', ty t)
	  end
      | exp (AST.ApplyExp (e1, e2, t)) =
	  F.ApplyExp (exp e1, exp e2, ty t) 
      | exp (AST.VarArityOpExp (oper, n, t)) = raise Fail "unsupported"
      | exp (tup as AST.TupleExp es) = let
          val astTy = TypeOf.exp tup
	  in
	    F.TupleExp (List.map exp es, astTy)
	  end
      | exp (AST.RangeExp (e1, e2, optE, t)) = 
          F.RangeExp (exp e1, exp e2, Option.map exp optE, ty t) 
      | exp (AST.PTupleExp es) = F.PTupleExp (List.map exp es) 
      | exp (AST.PArrayExp (es, t)) = F.PArrayExp (List.map exp es, ty t)
      | exp (AST.PCompExp (e, pes, optE)) = pcomp (e, pes, optE) 
      | exp (AST.PChoiceExp (es, t)) = F.PChoiceExp (List.map exp es, ty t)
      | exp (AST.SpawnExp e) = F.SpawnExp (exp e)
      | exp (AST.ConstExp k) = F.ConstExp (trConst (env, k))
      | exp (AST.VarExp (x, ts)) = let
          val (x', _) = trVar (env, x)
          in
            F.VarExp (x', List.map ty ts)
	  end
      | exp (AST.SeqExp (e1, e2)) = F.SeqExp (exp e1, exp e2)
      | exp (AST.OverloadExp vref) = 
          (case !vref
	    of AST.Unknown (t, vs) => raise Fail "todo"
	     | AST.Instance x => let
                 val (x', env') = trVar (env, x)
                 in
		   F.OverloadExp (ref (F.Instance x'))
		 end)
      | exp (AST.ExpansionOptsExp (os, e)) = F.ExpansionOptsExp (os, exp e)
    and pcomp (e, pes, optE) = raise Fail "todo"
    in
      exp e
    end


  and trBind (env : env, b : AST.binding) : F.binding * env = let
        fun bind b =
             (case b
	       of AST.ValBind (p, e) => let
		    val (p', env') = trPat (env, p)
                    val e' = trExp (env, e)
		    in
		      (F.ValBind (p', e'), env')
		    end
		| AST.PValBind (p, e) => let
		    val e' = trExp (env, e)
		    val (p', env') = trPat (env, p)
		    in
		      (F.PValBind (p', e'), env')
		    end
		| AST.FunBind lams => let
                    val (lams', env') = trLams (env, lams)
                    in
		      (F.FunBind lams', env')
		    end
		| AST.PrimVBind (x, rhs) => let
		    val (fx, env') = trVar (env, x) 
                    in
		      (F.PrimVBind (fx, rhs), env')
		    end		        
		| AST.PrimCodeBind c => (F.PrimCodeBind c, env)
	      (* end case *))
        in
          bind b
        end

  and trPat (env : env, p : AST.pat) : F.pat * env = 
       (case p
	  of AST.ConPat (c, ts, p) => let
               val c' = valOf (FE.findDCon (env, c))
	       val ts' = List.map (fn t => trTy (env, t)) ts
	       val (p', env') = trPat (env, p)
               in
	         (F.ConPat (c', ts', p'), env')
	       end
	   | AST.TuplePat [] => (F.TuplePat [], env)
	   | AST.TuplePat ps => let
               fun lp ([], env', acc) = (F.TuplePat (rev acc), env')
		 | lp (q::qs, env, acc) = let
		   (* we are counting on the fact that all vars are distinct *)
		   (* - that's why it's OK to "roll" the environment along... *)
		     val (q', env') = trPat (env, q)
                     in
		       lp (qs, env', q'::acc)
		     end
               in
		 lp (ps, env, [])
	       end
	   | AST.VarPat x => let
               val (fx, env') = trVar (env, x)
               in
		 (F.VarPat fx, env')
	       end
	   | AST.WildPat t => (F.WildPat (trTy (env, t)), env)
	   | AST.ConstPat c => let
               val c' = trConst (env, c)
	       in
	         (F.ConstPat c', env)
	       end
         (* end case *))

  and trMatch (env : env) (m : AST.match) = 
       (case m
	  of AST.PatMatch (p, e) => let
               val (p', env') = trPat (env, p)
	       val e' = trExp (env', e)
	       in
		 F.PatMatch (p', e')
	       end
	   | AST.CondMatch (p, e1, e2) => let
               val (p', env') = trPat (env, p)
	       val e1' = trExp (env', e1)
	       val e2' = trExp (env', e2)
	       in
		 F.CondMatch (p', e1', e2')
	       end
         (* end case *))

  and trPMatch (env : env) (m : AST.pmatch) : F.pmatch = 
       (case m
	  of AST.PMatch (ps, e) => let
               val (ps', env') = trPPats (env, ps)
	       val e' = trExp (env', e)
	       in
                 F.PMatch (ps', e')
	       end
	   | AST.Otherwise (ts, e) => let
               val ts' = List.map (fn t => trTy (env, t)) ts
	       in
		 F.Otherwise (ts', trExp (env, e))
	       end
         (* end case *))

  and trPPat (env : env, p: AST.ppat) : F.ppat * env =
       (case p
	  of AST.NDWildPat t => (F.NDWildPat (trTy (env, t)), env)
	   | AST.HandlePat (p, t) => let
               val (p', env') = trPat (env, p)
               val t' = trTy (env, t)
	       in
		 (F.HandlePat (p', t'), env')
	       end
	   | AST.Pat p => let
               val (p', env') = trPat (env, p)
	       in
		 (F.Pat p', env')
	       end
         (* end case *))

  and trPPats (env : env, ps : AST.ppat list) : F.ppat list * env = let
        fun lp ([], env', acc) = (rev acc, env')
	  | lp (p::ps, env, acc) = let
              val (p', env') = trPPat (env, p)
              (* relying on the fact that vars are distinct in this list of ppats *)
              in
		lp (ps, env', p'::acc) 
	      end
        in
          lp (ps, env, [])
        end

  and trLams (env : env, lams : AST.lambda list) : F.lambda list * env = let
        fun bindLp ([], env, fs) = (env, rev fs)
	  | bindLp (AST.FB(f,x,b)::t, env, fs) = let
              val (f', env') = trVar (env, f)
              in
                bindLp (t, env', (f',x,b)::fs)
	      end
	val (env', fs) = bindLp (lams, env, [])
	fun trFun (f', x, b) = let
              val (x', env'') = trVar (env', x)
	      val b' = trExp (env'', b)
              in
	        F.FB (f', x', b')
	      end
	val lams' = List.map trFun fs
        in
          (lams', env')
        end

  and trConst (env : env, c : AST.const) : F.const = 
       (case c
	  of AST.DConst (c, ts) => let
               val c' = valOf (FE.findDCon (env, c))
	       val ts' = List.map (fn t => trTy (env, t)) ts
               in
		 F.DConst (c', ts')
	       end
	   | AST.LConst (lit, t) => F.LConst (lit, trTy (env, t))
         (* end case *))          

end
