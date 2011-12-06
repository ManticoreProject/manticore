(* ast-cost.sml
*
* COPYRIGHT (c) 2006 The Manticore Project (http://manticore.cs.uchicago.edu)
* All rights reserved.
*)

structure ASTCost : sig

        val costAST : (TranslateEnv.env * AST.exp) -> int
        val translate : (TranslateEnv.env * AST.exp) -> AST.exp

end = struct


    structure A = AST
    structure V = Var
    structure Ty = Types


    (* f is the variable we want to assign a value to *)
local
        val {setFn, getFn, peekFn, clrFn} = V.newProp (fn f => ~1)
in
        fun clearCost f = clrFn f
        fun setCost (f, cost) = setFn (f, cost)
        fun getCost f = getFn f
end  

    (* prune out overload nodes.
   * NOTE: we should probably have a pass that does this before
   * AST optimization.
   *)
    fun prune (AST.OverloadExp(ref(AST.Instance x))) = AST.VarExp(x, [])
      | prune (AST.OverloadExp _) = raise Fail "unresolved overloading"
      | prune e = e

    (* function to assign cost values to AST expressions *)
    fun costAST (env,exp) = (case prune exp 
            of e as AST.LetExp(_,_) => let
                        fun costBinds (A.LetExp(b, e)) = let
                                val c1 = cost_binding(b,env)
                                val c2 = costBinds(e)
                                val c = c1 + c2
                           in   
                                c
                           end
                           | costBinds e = costAST(env,e)
        
                        val _ = TextIO.print("LetExp normal \n")
                        val c = costBinds(e)
                in      
                        TextIO.print(String.concat["Cost are " , Int.toString c, "end Let\n"]);
                        c
                end
	    | AST.IfExp(e1, e2, e3, ty) => let
                        val _ = TextIO.print("IfExp of type\n") 
                        val _ = TextIO.print(TypeUtil.toString ty)
                        val c1 = costAST(env,e1)
                        val c2 = costAST(env,e2)
                        val c3 = costAST(env,e3)
                        val c = 1 + c1 + Int.max(c2,c3)
                        val _ = TextIO.print(String.concat[" If exp Cost are " , Int.toString c, " (** end IF **) "])
                in      
                        c
                end
	    | AST.CaseExp(e, rules, ty) => let
                        val _ = TextIO.print("CaseExp of type\n")
                        val c1 = costAST(env,e)
                        val c2 = casematch(rules, env)
                        val c = c1 + c2
                        val _ = TextIO.print(String.concat["cost are ",Int.toString c, " * (end case) * \n"])
                in     
                        c
                end
            (* FIX ME need function costs *)
            | AST.FunExp(x, body, ty) => let
                        val _ = TextIO.print("FunExp of type \n")
                        val _ = mysize(ty)
                        val _ = TextIO.print(TypeUtil.toString ty)
                        val _ = TextIO.print (V.toString x)
                        val c1 = costAST(env,body)
                        val c = c1
                        val _ = TextIO.print(String.concat["Cost are", Int.toString c, "\n"])  
                in      
                        c
                end
            (* FIX ME add cost of eval function *)
            | AST.ApplyExp(e1, e2, ty) => let
                        val _ = TextIO.print("ApplyExp of type (")
                        val _ = TextIO.print(TypeUtil.toString ty)
                        val c1 = costAST(env,e1)
                        val c2 = costAST(env,e2)
                        val c = c1 + c2 + 1
                        val _ =  TextIO.print(String.concat["Apply costs are ", Int.toString c, ") \n ** end apply ** \n"])
                in
                        c 
                end
            | AST.TupleExp[] => 0
            | AST.TupleExp (exps) => let
(* FIX ME : need to check for not closed expressions *)
                        val _ = TextIO.print("TupleExp not unit \n")
                        val exps' = List.map (fn e => costAST(env, e)) exps
                        fun addL(L) =
                                if L=[] 
                                then 0 
                                else hd(L) + addL(tl(L))
                        val c = addL(exps')
                        val _ = TextIO.print(String.concat["TupleExp costs are ", Int.toString c, "\n (** End Tupleexp **) \n"])
                in           
                        c
                end
            (* FIX ME VAR EXP *)
            | AST.VarExp(x, tys) => let 
                                val _ = TextIO.print(String.concat["Var exp ", printvar(x), " \n"])
                        in
                               0
                        end
            | AST.PCaseExp _ => raise Fail "PCaseExp" (* FIXME *)
	    | AST.HandleExp(e, mc, ty) =>  let
                        val _ = TextIO.print("HandleExp\n")
                in           
                        0
                end
		       
	    | AST.RaiseExp(e, ty) => let
                        val _ = TextIO.print("RaiseExp\n")
                in           
                        0
                end
	    | AST.VarArityOpExp (oper, i, ty) => 0
	    | AST.RangeExp (lo, hi, optStep, ty) => raise Fail "FIXME (range construction)"
            | AST.PTupleExp[] => 0
            | AST.PTupleExp (exps) => let
                        val _ = TextIO.print("PTupleExp\n")
                      (*  val c1 = costAST(env,e) *)
                        (* We need the maximum of all the ptuple expressions *)
(* FIX ME : need to check for not closed expressions *)
                        fun maxList(L) = 
                                if L=[] 
                                then ~1 
                                else Int.max(hd(L), maxList(tl(L)))
                        val exps' = List.map (fn e => costAST(env, e)) exps
                        val c = maxList(exps');
                        val _ = TextIO.print(String.concat["PTupleExp costs are ", Int.toString c, "\n (** End PTupleexp **) \n"])
                in                          
                        c
                end
	    | AST.PArrayExp(exps, ty) => raise Fail "unexpected PArrayExp"
	    | AST.PCompExp _ => raise Fail "unexpected PCompExp"
	    | AST.PChoiceExp _ => raise Fail "unexpected PChoiceExp"
	    | AST.SpawnExp e => let
                        val _ = TextIO.print("SpawnExp\n")
                in           
                        costAST(env,e)
                end
	    | AST.ConstExp (constexp) => const_cost(constexp)
	    | AST.SeqExp (e1,e2) => let
                        val _ = TextIO.print("SeqExp\n")
                        val c1 = costAST(env,e1)
                        val c2 = costAST(env,e2)
                in           
                        c1 + c2
                end
	    | AST.OverloadExp _ => raise Fail "unresolved overloading"
	    | AST.ExpansionOptsExp (opts, e) => let
                        val _ = TextIO.print("ExpansionoptsExp\n")
                in           
                        costAST(env,e)
                end
        )

        and const_cost(conexpr) = (case conexpr
                of AST.DConst(_,tylist) => let 
                                        val _ = TextIO.print("DConst exp \n")
                                in
                                        length(tylist)
                                end
                | AST.LConst (_) => let 
                                        val _ = TextIO.print("LConst exp \n")
                                in
                                        1
                                end
        )

        and cost_binding (e,env) = (case e
                        of AST.ValBind(p, e) => let
                                                val _ = TextIO.print("val XX = \n")
                                        in
                                                costAST(env,e)
                                        end
                        | AST.PValBind(p, e) => let
                                                val _ = TextIO.print("pval XX =")
                                        in
                                                costAST(env,e)
                                        end
                        | AST.FunBind(lam) => let
                                        val _ = TextIO.print("FunExp in LetExp \n")
                                        val c = costlambda(lam,env)
                                        val _ = TextIO.print(String.concat["with cost  ", Int.toString c, "\n"])
                                in
                                        c
                                end
                        (* these should be the primitive operators *)
                        (* | AST.PrimVBind (x, _) => mysize(V.typeof x) *)
                        | AST.PrimVBind (x, _) => let 
                                        val _ = TextIO.print(String.concat["Primbinding ",printvar(x), " with cost 1 \n"])
                                in
                                        1
                                end
                        | AST.PrimCodeBind _ => let 
                                        val _ = TextIO.print("AST.PrimCodeBind with cost 0 \n")
                                in
                                        0
                                end
                        
        )
        (* for the case e of pat => expr take the maximum of the (pat,expr) pair *)
        and casematch (rule::rest, env) = (case rule 
                                of AST.PatMatch (pat,e) => let
                                        val _ = TextIO.print("PatMatch \n")
                                        val c = costAST(env,e)
                                in      
                                        Int.max(c,casematch(rest, env))
                                end
                                (* CondMatch is not used yet *)
                                | AST.CondMatch (pat,e1,e2) => 0
                                )
                        | casematch ([], _) = 0
        (* FIX ME *)
        and costlambda ([], _) = 0
                | costlambda ( A.FB(f, x, e)::l, env ) = let
                        (* look up cost of the function *)
                        val _ = TextIO.print(String.concat["Funbinding ", printvar(f), " to " , printvar(x)," \n"])
                        val c = costAST(env,e)
                        val _ = setCost (f, c)
                        val _ = TextIO.print(String.concat["Funbinding ", printvar(f), " has cost ", Int.toString c," \n (** End Funbinding **) \n"])
                        val readout = getCost(f)
                        val _ = TextIO.print(String.concat["REading out the cost of the function = ", Int.toString readout, " !!******!!! "])
                in
                        1 + costlambda(l,env)
                end

        (* MISSING and pmatch *)

        (* we shouldn't need this
        and cost_pat (expr,env) = (case expr 
                of AST.ConPat (dcon,_,pat) => TextIO.print ("Conpat\n")
                | AST.TuplePat(p::pat) => TextIO.print ("TuplePat\n")
                | AST.VarPat(v) => TextIO.print ("VarPat\n")
                | AST.WildPat(ty) => TextIO.print ("WildPat\n")
                | AST.ConstPat(con) => TextIO.print ("ConstPat\n")
        )
        *)

        (* Types.ty -> cost *)
        (* type const should just contain basic types like numbers and string/chars *)
        and mysize (ty1) = let
	  fun toS(Ty.ErrorTy) = 0
	    | toS (Ty.MetaTy mv) =  0
	    | toS (Ty.VarTy tv) = 1 (* FIX ME type variable *)
	    | toS (Ty.ConTy([], tyc)) = 0
	    | toS (Ty.ConTy([ty], tyc)) = 1
	    | toS (Ty.ConTy(tys, tyc)) = length(tys)
	   (* | toS (Ty.FunTy(ty1 as Ty.FunTy _, ty2)) = TextIO.print ("funType 1\n") *)
	    | toS (Ty.FunTy(ty1, ty2)) = 0
	    | toS (Ty.TupleTy []) = 0
	    | toS (Ty.TupleTy tys) = length(tys)
	  in
	    toS(ty1)
	  end

        and printvar(x) = String.concat[Var.toString x, " : ", TypeUtil.schemeToString (Var.typeOf x),"\n"]

(*
-----------------------------------------------------------------------------------------------
This function will add a sequential version and changes the original PTuple expression for chunking

-----------------------------------------------------------------------------------------------
*)

        fun ASTaddchunking (env,exp) = (case prune exp 
                of e as AST.LetExp(_,_) => let
                        fun letBinds (AST.LetExp(b, e)) = let
                                val b1 = binding(b,env)
                                val c1 = letBinds(e)
                           in   
                                AST.LetExp(b1, c1)
                           end
                           | letBinds e = ASTaddchunking(env,e)
                in      
                        letBinds(e)
                end
	    | AST.IfExp(e1, e2, e3, ty) => AST.IfExp(ASTaddchunking(env,e1), ASTaddchunking(env,e2), ASTaddchunking(env,e3), ty)
	    | AST.CaseExp(e, rules, ty) => AST.CaseExp(ASTaddchunking(env,e) , casematch(rules, env), ty)
            | AST.FunExp(x, body, ty) => AST.FunExp(x, ASTaddchunking(env,body), ty)
            | AST.ApplyExp(e1, e2, ty) => AST.ApplyExp(ASTaddchunking(env,e1), ASTaddchunking(env,e2), ty)
            | AST.TupleExp[] => AST.TupleExp[]
            | AST.TupleExp (exps) => let
                        val exps' = List.map (fn e => ASTaddchunking(env, e)) exps
                in           
                        AST.TupleExp (exps')
                end
            | AST.VarExp(x, tys) => AST.VarExp(x, tys)
            | AST.PCaseExp _ => raise Fail "PCaseExp" (* FIXME *)
        (* DO WE NEED TO ANALYZE THIS? *)
	    | AST.HandleExp(e, mc, ty) =>  AST.HandleExp(e, mc, ty)   
	    | AST.RaiseExp(e, ty) => AST.RaiseExp(e, ty)
	    | AST.VarArityOpExp (oper, i, ty) => AST.VarArityOpExp (oper, i, ty)
	    | AST.RangeExp (lo, hi, optStep, ty) => raise Fail "FIXME (range construction)"
            | AST.PTupleExp[] => AST.PTupleExp[]
            | AST.PTupleExp (exps) => let
                        val exps' = List.map (fn e => ASTaddchunking(env, e)) exps
                        val test = Basis.int_gt
                        val test1 = AST.ConstExp(AST.LConst(Literal.Int(10),Basis.intTy))
                        val test2 = AST.ConstExp(AST.LConst(Literal.Int(5),Basis.intTy))
                        val applye = AST.ApplyExp(AST.VarExp(test,[]), AST.TupleExp([test1,test2]), Basis.boolTy)
                       
                in           
                        (* AST.PTupleExp (exps') *)
                        AST.IfExp(applye,AST.PTupleExp (exps'),AST.TupleExp (exps'),TypeOf.exp(AST.TupleExp(exps')))
                end
	    | AST.PArrayExp(exps, ty) => raise Fail "unexpected PArrayExp"
	    | AST.PCompExp _ => raise Fail "unexpected PCompExp"
	    | AST.PChoiceExp _ => raise Fail "unexpected PChoiceExp"
	    | AST.SpawnExp e => AST.SpawnExp(ASTaddchunking(env,e))
	    | AST.ConstExp (constexp) => AST.ConstExp (constexp)
	    | AST.SeqExp (e1,e2) => AST.SeqExp (ASTaddchunking(env,e1),ASTaddchunking(env,e2))
	    | AST.OverloadExp _ => raise Fail "unresolved overloading"
	    | AST.ExpansionOptsExp (opts, e) => AST.ExpansionOptsExp (opts, ASTaddchunking(env,e)) 
        )

        and binding (e,env) = (case e
                        of AST.ValBind(p, e) => AST.ValBind(p,ASTaddchunking(env,e))
                        | AST.PValBind(p, e) => AST.PValBind(p,ASTaddchunking(env,e))
                        | AST.FunBind(lam) => AST.FunBind(lambda(lam,env))
                        (* these should be the primitive operators *)
                        (* | AST.PrimVBind (x, _) => mysize(V.typeof x) *)
                        | e => e
                        
        )
        (* for the case e of pat => expr take the maximum of the (pat,expr) pair *)
        and casematch (rule::rest, env) = (case rule 
                                of AST.PatMatch (pat,e) => let
                                        val e1 = ASTaddchunking(env,e)
                                in      
                                        AST.PatMatch(pat,e1)::casematch(rest,env)
                                end
                                (* CondMatch is not used yet *)
                                | AST.CondMatch (pat,cond,e2) => raise Fail "unexpected AST.CondMatch"
                                )
                        | casematch ([], _) = [] 
        (* FIX ME *)
        and lambda ([], env) = [] 
                | lambda ( A.FB(f, x, e)::l, env ) = let
                        val e1 = ASTaddchunking(env,e)
                in
                        A.FB(f, x, e1)::lambda(l,env)
                end



        fun translate (env, body) = let
                val body' = ASTaddchunking(env,body)
                val _ = costAST(env,body')
        in   
                body'
        end

end