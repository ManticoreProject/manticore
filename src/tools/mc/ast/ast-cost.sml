(* ast-cost.sml
*
* COPYRIGHT (c) 2006 The Manticore Project (http://manticore.cs.uchicago.edu)
* All rights reserved.
*)

structure ASTCost : sig

        val translate : AST.exp -> AST.exp
        val costAST : AST.var * AST.exp -> int

end = struct


    structure A = AST
    structure B = Basis
    structure V = Var
    structure Ty = Types
    structure U = ASTUtil
    structure TTbl = TyCon.Tbl
    structure VTbl = Var.Tbl
    structure DTbl = DataCon.Tbl


    infixr 2 -->
    fun t1 --> t2 = Ty.FunTy (t1, t2)

    fun vexp v = AST.VarExp (v, [])

    fun dummyvar () = (Var.new ("dummy", B.unitTy))
    
    (* The size of the hash that we use to store information about function calls within a function *)
    val hashsize = 8
    
    (* unkown costs is -1 *)
    val Cunkown = ~1

    (* cut off limit for the costs *)
    val Tlimit = 1000

    fun printvar(x) = String.concat[Var.toString x, " : ", TypeUtil.schemeToString (Var.typeOf x),"\n"]
 
    (* this hash is used to save all the data constructor that we need to rewrite to add size information *)
    val dconhash : AST.var TTbl.hash_table = TTbl.mkTable (20, Fail "dconhash error")

    (* we need to check if we actually used the dcon in a cost function *)
    val useddconhash : bool TTbl.hash_table = TTbl.mkTable (20, Fail "useddconhash error")

(*
-----------------------------------------------------------------------------------------------
annotation for the variables to save the costs to it
-----------------------------------------------------------------------------------------------
*)

    (* f is the variable we want to assign a value to *)
    local
        val {setFn, getFn, peekFn, clrFn} = V.newProp (fn f => ~1)
    in
        fun clearCost f = clrFn f
        fun setCost (f, cost) = setFn (f, cost)
        fun getCost f = getFn f
        (* returns SOME var if exist otherwise NONE *) 
        fun existCost f = peekFn f
    end  

(*
-----------------------------------------------------------------------------------------------
annotation for the variables to save the size 
-----------------------------------------------------------------------------------------------
*)

    (* f is the variable we want to assign a value to *)
    local
        val {setFn, getFn, peekFn, clrFn} = V.newProp (fn f => ~1)
    in
        fun clearSize f = clrFn f
        fun setSize (f, cost) = setFn (f, cost)
        fun getSize f = getFn f
        (* returns SOME var if exist otherwise NONE *) 
        fun existSize f = peekFn f
    end

(*
-----------------------------------------------------------------------------------------------
annotation for the variables with a hash table that saves hash[fctname]=count occurences in body of f
-----------------------------------------------------------------------------------------------
*)

    (* f is the variable we want to assign a value to *)
    (* we use this to store the extra information to create the cost function, e.g. number of calls to a function and the maximum of the input arguments *)
    local
        val dummyexp = ~1
        val hash_dummy : (int * int) VTbl.hash_table = VTbl.mkTable (0, Fail "var to count")
        val {setFn, getFn, peekFn, clrFn} = V.newProp (fn f => hash_dummy)
    in
        fun clearCostfct f = clrFn f
        fun setCostfct (f, x) = let
                                        val hash : (int * int) VTbl.hash_table = VTbl.mkTable (hashsize, Fail "var to count")
                                        val _ = VTbl.insert hash (x, (1,dummyexp))
                                in
                                        setFn (f, hash)
                                end
        fun getCostfct f = getFn f
        (* returns SOME var if exist otherwise NONE *) 
        fun existCostfct f = peekFn f
        fun addCostfct (f,x) = case (existCostfct f)
                                of SOME hash => (case VTbl.find hash (x) 
                                                        of SOME (n,exp) => VTbl.insert hash (x, (n+1,exp))
                                                        | NONE => 
(* FIX ME CHECK FOR FULL HASH TABLE *)
                                                                (* if (VTbl.numItems hash = hashsize) then raise Fail "Hash Table is full increase MAX value"       
                                                                else *) VTbl.insert hash (x, (1,dummyexp))
                                                ) (** end case **)
                                | NONE => setCostfct(f,x)
        fun addcostfctargsize(f,x,argsize) = case (existCostfct f)
                                      of SOME hash => (case VTbl.find hash (x) 
(* FIX ME NEED A POLICY TO COMPARE ARGUMENTS, JUST OVERWRITING THE OLD ONE NOW *)
                                                        of SOME (n,size) => if (argsize > size ) then VTbl.insert hash (x, (n,argsize)) else ()
                                                        | NONE => raise Fail (String.concat["Can't find entry for ",printvar(x)," in hash table for function", printvar(f),"\n"])
                                                ) (** end case **)
                                     | NONE => raise Fail (String.concat["Can't find a cost hash for ", printvar(f),"\n"])
                                
    end  


(*
-----------------------------------------------------------------------------------------------
annotation for the variables to save the costfunction if one is created for the particular f
-----------------------------------------------------------------------------------------------
*)

   (* f is the variable we want to assign a value to *)
    local
        val dummylambda = U.mkFunWithParams( (Var.new("dummy",B.unitTy)),[],U.mkInt(1))
        val {setFn, getFn, peekFn, clrFn} = V.newProp (fn f => dummylambda)
    in
        fun clearCFname f = clrFn f
        fun setCFname (f, name) = setFn (f, name)
        fun getCFname f = getFn f
        (* returns SOME var if exist otherwise NONE *) 
        fun existCFname f = peekFn f
    end


    (* prune out overload nodes.
   * NOTE: we should probably have a pass that does this before
   * AST optimization.
   *)
    fun prune (AST.OverloadExp(ref(AST.Instance x))) = AST.VarExp(x, [])
      | prune (AST.OverloadExp _) = raise Fail "unresolved overloading"
      | prune e = e


(*
-----------------------------------------------------------------------------------------------
Collect user datacon in apply exp 
-----------------------------------------------------------------------------------------------
*)

        fun ASTcollectDCON (exp) = (case prune exp 
                of e as AST.LetExp(_,_) => let
                        fun letBinds (AST.LetExp(b, e)) = let
                                val b1 = binding(b)
                                val c1 = letBinds(e)
                           in   
                               ()
                           end
                           | letBinds e = ASTcollectDCON(e)
                in      
                        letBinds(e)
                end
	    | AST.IfExp(e1, e2, e3, ty) => let
                                val _ = ASTcollectDCON(e1)
                                val _ = ASTcollectDCON(e2)
                        in
                                ASTcollectDCON(e3)
                        end
	    | AST.CaseExp(e, rules, ty) => let
                                val _ = ASTcollectDCON(e)
                        in
                                casematch(rules)
                        end
            | AST.FunExp(x, body, ty) => ASTcollectDCON(body)
            | AST.ApplyExp(e1, e2, ty) => let
                        val _ = (case e1 
                                (* data constructor used, we might need to rewrite those so we collect the information *)
                                of AST.ConstExp(const as AST.DConst(dcon, tylist)) => let
                                                        val tyconname = DataCon.ownerOf dcon
                                                in
                                                        case TTbl.find dconhash (tyconname) 
                                                        of SOME n => ()
                                                        | NONE => let
                                                                        val typevar = U.dcontotype(AST.ConstExp(const))
                                                                        val sizename = String.concat[TyCon.toString (tyconname), "size"]
                                                                        val estSize = Var.new (sizename,typevar --> B.intTy)
                                                                   in
                                                                        TTbl.insert dconhash (tyconname,estSize);
                                                                        TTbl.insert useddconhash(tyconname,false)
                                                                   end
                                                end
                                | _ => ()       
                                )
                        in
                                ASTcollectDCON(e1);
                                ASTcollectDCON(e2)
                        end
            | AST.TupleExp[] => ()
            | AST.TupleExp (exps) => let
                        val _ = List.map (fn e => ASTcollectDCON(e)) exps
                in           
                        ()
                end
            | AST.VarExp(x, tys) => ()
            | AST.PCaseExp _ => raise Fail "PCaseExp" (* FIXME *)
	    | AST.HandleExp(e, mc, ty) =>  ()  
	    | AST.RaiseExp(e, ty) => ()
	    | AST.VarArityOpExp (oper, i, ty) => ()
	    | AST.RangeExp (lo, hi, optStep, ty) => raise Fail "FIXME (range construction)"
            | AST.PTupleExp[] => ()
            | AST.PTupleExp (exps) => let
                        val _ = List.map (fn e => ASTcollectDCON(e)) exps
                in           
                        ()
                end
	    | AST.PArrayExp(exps, ty) => raise Fail "unexpected PArrayExp"
	    | AST.PCompExp _ => raise Fail "unexpected PCompExp"
	    | AST.PChoiceExp _ => raise Fail "unexpected PChoiceExp"
	    | AST.SpawnExp e => ASTcollectDCON(e)
	    | AST.ConstExp (constexp) => ()
	    | AST.SeqExp (e1,e2) => let
                                val _ = ASTcollectDCON(e1)
                        in 
                                ASTcollectDCON(e2)
                        end
	    | AST.OverloadExp _ => raise Fail "unresolved overloading"
	    | AST.ExpansionOptsExp (opts, e) => ASTcollectDCON(e) 
        )

        and binding (e) = (case e
                        of AST.ValBind(p, e) => ASTcollectDCON(e)
                        | AST.PValBind(p, e) => ASTcollectDCON(e)
                        | AST.FunBind(lam) => lambda(lam)
                        (* these should be the primitive operators *)
                        (* | AST.PrimVBind (x, _) => mysize(V.typeof x) *)
                        | e => ()
                        
        )
        (* for the case e of pat => expr take the maximum of the (pat,expr) pair *)
        and casematch (rule::rest) = (case rule 
                                of AST.PatMatch (pat,e) => let
                                        val _ = ASTcollectDCON(e)
                                in      
                                       casematch(rest)
                                end
                                (* CondMatch is not used yet *)
                                | AST.CondMatch (pat,cond,e2) => raise Fail "unexpected AST.CondMatch"
                                )
                        | casematch ([]) = ()

        and lambda ([]) = ()
            | lambda ( A.FB(f, x, e)::l) = let
                                val _ = ASTcollectDCON(e)
                        in
                                lambda(l)
                        end



(*
-----------------------------------------------------------------------------------------------
A pretty printer for the cost functions
-----------------------------------------------------------------------------------------------
*)

    fun printCost (exp) = (case prune exp 
            of e as AST.LetExp(_,_) => let
                        fun printBinds (A.LetExp(b, e)) = let
                                val _ = printcost_binding(b)
                                val _ = printBinds(e)
                           in   
                                ()
                           end
                           | printBinds e = printCost(e)
        
                        val _ = TextIO.print("LetExp\n")
                        val _ = printBinds(e)
                in      
                        TextIO.print("end Let\n")
                end
	    | AST.IfExp(e1, e2, e3, ty) => let
                        val _ = TextIO.print("IfExp of type\n") 
                        val _ = TextIO.print(TypeUtil.toString ty)
                        val _ = printCost(e1)
                        val _ = printCost(e2)
                        val _ = printCost(e3)
                in      
                        TextIO.print("(** end IF **)\n")
                end
	    | AST.CaseExp(e, rules, ty) => let
                        val _ = TextIO.print("CaseExp of type\n")
                        val _ = printCost(e)
                        val _ = printcasematch(rules)
                in     
                        TextIO.print(" (** end case **) \n")
                end
            | AST.FunExp(x, body, ty) => let
                        val _ = TextIO.print(String.concat["Funexp ", printvar(x), " of type " ," \n"])
                        val _ = TextIO.print(TypeUtil.toString ty)
                        val _ = printCost(body)
                         fun checkcostfct () = (case existCostfct(x)
                                of SOME n => printhash(x)
                                | NONE => ()
                        )
                        val _ = checkcostfct()
                in      
                        TextIO.print(String.concat["Cost are", Int.toString(getCost(x)), "(** End Funexp **)\n"])  
                end
            (* FIX ME add cost of eval function *)
            | AST.ApplyExp(e1, e2, ty) => let
                        val _ = TextIO.print("ApplyExp of type ")
                        val _ = TextIO.print(TypeUtil.toString ty)
                        val _ = TextIO.print("\n")
                        val _ = printCost(e1)
                        val _ = printCost(e2)
                in
                        TextIO.print(" (** end apply **) \n")
                end
            | AST.TupleExp[] => ()
            | AST.TupleExp (exps) => let
                        val _ = TextIO.print("TupleExp not unit \n")
                        val _ = List.map (fn e => printCost(e)) exps
                in           
                        TextIO.print(" (** End Tupleexp **) \n")
                end
            | AST.VarExp(x, tys) => ( case existCost(x) 
                                    of SOME n => 
                                        TextIO.print(String.concat["Var exp ", printvar(x), " with costs ", Int.toString (n), " \n"])
                                    | NONE =>
                                        TextIO.print(String.concat["PROBLEM !! Var exp ", printvar(x), " has NO costs saved \n"])
                        )
            | AST.PCaseExp _ => raise Fail "PCaseExp" (* FIXME *)
	    | AST.HandleExp(e, mc, ty) =>  TextIO.print("HandleExp\n")
	    | AST.RaiseExp(e, ty) => TextIO.print("RaiseExp\n")
	    | AST.VarArityOpExp (oper, i, ty) => ()
	    | AST.RangeExp (lo, hi, optStep, ty) => raise Fail "FIXME (range construction)"
            | AST.PTupleExp[] => ()
            | AST.PTupleExp (exps) => let
                        val _ = TextIO.print("PTupleExp not unit \n")
                        val _ = List.map (fn e => printCost(e)) exps
                in           
                        TextIO.print(" (** End PTupleexp **) \n")
                end
	    | AST.PArrayExp(exps, ty) => raise Fail "unexpected PArrayExp"
	    | AST.PCompExp _ => raise Fail "unexpected PCompExp"
	    | AST.PChoiceExp _ => raise Fail "unexpected PChoiceExp"
	    | AST.SpawnExp e => let
                        val _ = TextIO.print("SpawnExp\n")
                in           
                        printCost(e)
                end
	    | AST.ConstExp (constexp) => const_print(constexp)
	    | AST.SeqExp (e1,e2) => let
                        val _ = TextIO.print("SeqExp\n")
                        val _ = printCost(e1)
                        val _ = printCost(e2)
                in           
                        TextIO.print(" (** End SeqExp **)\n")
                end
	    | AST.OverloadExp _ => raise Fail "unresolved overloading"
	    | AST.ExpansionOptsExp (opts, e) => let
                        val _ = TextIO.print("ExpansionoptsExp\n")
                in           
                        printCost(e)
                end
        )

        and const_print(conexpr) = (case conexpr
                of AST.DConst (dcon,tylist) => TextIO.print(String.concat["DConst exp with length ", Int.toString(length(tylist)), " and signature ", DataCon.toString(dcon), "\n"])
                | AST.LConst (lit, ty) => (case lit
                                        of Literal.Enum(word) => TextIO.print("Enum Type\n")
                                        | Literal.StateVal(word) => TextIO.print("StateVal Type\n")
                                        | Literal.Tag (str) => TextIO.print("Tag Type\n")
                                        | Literal.Int (num) => TextIO.print(String.concat["Int Type with value ", Int.toString(IntInf.toInt num) ,"\n"])
                                        | Literal.Float (num) => TextIO.print("Float Type\n")
                                        | Literal.Char (ch) => TextIO.print("Character Type\n")
                                        | Literal.String (str) => TextIO.print("String Type\n")
                                        | Literal.Bool (bool) => TextIO.print("Bool Type\n")
                                ) (** end case **)
        )

        and printcost_binding (e) = (case e
                        of AST.ValBind(p, e) => let
                                                val _ = TextIO.print("val ")
                                                val _ = printpat(p)
                                                val _ = TextIO.print(" = \n")
                                        in
                                                printCost(e)
                                        end
                        | AST.PValBind(p, e) => let
                                                val _ = TextIO.print("pval XX =")
                                        in
                                                printCost(e)
                                        end
                        | AST.FunBind(lam) => printlambda(lam)
                        | AST.PrimVBind (x, _) => TextIO.print(String.concat["Primbinding ",printvar(x), " with cost 1 \n"])
                        | AST.PrimCodeBind _ => TextIO.print("AST.PrimCodeBind with cost 0 \n")
        )

        and printpat(p) = (case p 
                        of AST.ConPat (dcon, tylist, pat) => ()	(* data-constructor application *)
                        | AST.TuplePat (patlist) => ()
                        | AST.VarPat (var) => (case existSize(var)
                                                of SOME n => TextIO.print(String.concat["Size saved with variable ", printvar(var), " is ", Int.toString(n),"\n"]) 
                                                | NONE => printCost(AST.VarExp(var,[]))
                                        )
                        | AST.WildPat (ty) => ()
                        | AST.ConstPat (const) => ()
                )

        (* for the case e of pat => expr take the maximum of the (pat,expr) pair *)
        and printcasematch (rule::rest) = (case rule 
                                of AST.PatMatch (pat,e) => let
                                        val _ = TextIO.print("PatMatch \n")
                                        val _ = printCost(e)
                                in      
                                        TextIO.print(" (** End PatMatch **) \n");
                                        printcasematch(rest)
                                end
                                (* CondMatch is not used yet *)
                                | AST.CondMatch (pat,e1,e2) => ()
                                )
                        | printcasematch ([]) = ()
        (* FIX ME *)
        and printlambda ([]) = ()
                | printlambda ( A.FB(f, x, e)::l) = let
                        (* look up cost of the function *)
                        val _ = TextIO.print(String.concat["Funbinding ", printvar(f), " to " , printvar(x)," \n"])
                        val _ = printCost(e)
                        val _ = TextIO.print(String.concat["Funbinding ", printvar(f), " has cost ", Int.toString (getCost(f))," \n (** End Funbinding **) \n"])
                        fun checkcostfct () = (case existCostfct(f)
                                of SOME n => printhash(f)
                                | NONE => ()
                        )
                        val _ = checkcostfct()
                in
                        printlambda(l)
                end

        and printhash(x) = let
                        val hash = getCostfct(x)
                        val listitems = VTbl.listItemsi hash
                        val _ = TextIO.print(String.concat["Printing the costfunction variables for ", Var.toString x])
                        fun printhash (key,(value,size)) = TextIO.print(String.concat[" Count of ", Var.toString key, " = ", Int.toString(value), " with size ",Int.toString(size) ,"\n" ])
                        (* this prints the actual cost function and its type *)
                        val _ = (case existCFname(x) 
                                of SOME n => let
                                                val lam as A.FB(f,_,_) = getCFname(x)
                                        in
                                                TextIO.print(String.concat["The type of the cost function is ", printvar(f), "\n"])
                                        end
                                | NONE => ()
                        )
                in      
                        List.map printhash listitems;
                        ()
                end

        and printtype (typeprint) = (case typeprint
                        of Ty.ErrorTy => TextIO.print("Errortype \n")
                        | Ty.MetaTy (meta) => let
                                            val printme = TypeUtil.toString(TypeUtil.prune(Ty.MetaTy(meta)))
                                            val _ = TextIO.print(printme)
                                        in
                                                TextIO.print("Metatype \n")
                                        end
                        | Ty.VarTy (_) => TextIO.print("Vartype \n")
                        | Ty.ConTy (tylist, tycon) => TextIO.print("Contype \n")
                        | Ty.FunTy (ty1, ty2) => TextIO.print("Funtype \n")
                        | Ty.TupleTy (tylist) => TextIO.print("Tupletype \n")
                )

(*
-----------------------------------------------------------------------------------------------
This function analyzes the tree and assigns costs to functions or ~1 if we need to reevaluate and maybe create a cost function
-----------------------------------------------------------------------------------------------
*)

    (* function to assign cost values to AST expressions if possible if the function is open it assigns ~1 *)
    fun costAST (pre,exp) = (case prune exp 
            of e as AST.LetExp(_,_) => let
                        fun costBinds (A.LetExp(b, e)) = let
                                val c1 = cost_binding(b,pre)
                                val c2 = costBinds(e)
                                val c = c1 + c2
                           in   
                                c
                           end
                           | costBinds e = costAST(pre,e)
                in      
                       costBinds(e)
                end
	    | AST.IfExp(e1, e2, e3, ty) => let
                        val c1 = costAST(pre,e1)
                        val c2 = costAST(pre,e2)
                        val c3 = costAST(pre,e3)
                in      
                        1 + c1 + Int.max(c2,c3)
                end
	    | AST.CaseExp(e, rules, ty) => let
                        val c1 = costAST(pre,e)
                        val c2 = casematch(rules, pre)
                in     
                        c1 + c2
                end
            (* FIX ME need function costs *)
            | AST.FunExp(x, body, ty) => let
                        val c = costAST(x,body)
                        val _ = setCost(x,c)
                in      
                        c
                end
            | AST.ApplyExp(e1, e2, ty) => let
                        val c1 = costAST(pre,e1)
                        val c2 = costAST(pre,e2)

                        val _ = (case e1 
                                (* if first one is a function then check the argument for trivial size *)
                                of AST.VarExp(x,tys) => (case existCostfct(x) 
                                                        of SOME hash => (case e2 of
                                                                        (* for now just trivial const expressions *)
                                                                        AST.ConstExp(exp) => addcostfctargsize(pre,x,const_size(exp))
                                                                        (* check if there is a size attached to the variable *)
                                                                        | AST.VarExp(var,_) => (case existSize(var) 
                                                                                                of SOME n => addcostfctargsize(pre,x,n)
                                                                                                | NONE => ()
                                                                                        ) (** end case **)
                                                                        | _ => TextIO.print(String.concat["Couldn't find a match for ", printvar(x), "\n"])
                                                                        ) (** end case **)
                                                        | NONE => TextIO.print(String.concat["Couldn't find a costfct for ", printvar(x)," check for fix costs\n"])
                                                        ) (** end case **)
                                (* data constructor used, we might need to rewrite those so we collect the information *)
                               (* | AST.ConstExp(const as AST.DConst(dcon, tylist)) => let
                                                        val tyconname = DataCon.ownerOf dcon
                                                in
                                                        case TTbl.find dconhash (tyconname) 
                                                        of SOME n => ()
                                                        | NONE => let
                                                                        val typevar = U.dcontotype(AST.ConstExp(const))
                                                                        val sizename = String.concat[TyCon.toString (tyconname), "size"]
                                                                        val estSize = Var.new (sizename,typevar --> B.intTy)
                                                                   in
                                                                        TTbl.insert dconhash (tyconname,estSize);
                                                                        TTbl.insert useddconhash(tyconname,false)
                                                                   end
                                                end
                *)
                                | _ => ()
                        ) (** end case **)
                in
                        c1 + c2 + 1
                end
            | AST.TupleExp[] => 0
            | AST.TupleExp (exps) => let
(* FIX ME : need to check for not closed expressions *)
                        val exps' = List.map (fn e => costAST(pre, e)) exps
                        fun addL(L) =
                                if L=[] 
                                then 0 
                                else hd(L) + addL(tl(L))
                in           
                        addL(exps')
                end
(* FIX ME VAR EXP *)
            | AST.VarExp(x, tys) => let
                                fun varcost () = (
                                        case existCost x
                                        of (SOME num) => num
                                        | NONE => let 
                                                val AST.TyScheme(_, tys) = V.typeOf x
                                                in
                                                        mysize(tys)
                                                end
                                ) (** end varcost **)
                                fun setvarcost (c) = (
                                        case c
                                        (*  ~1 means the cost are unknown and we add it to the hashtable*)
                                        of ~1 => addCostfct(pre,x)
                                        (* we have to check if there will be a cost function for the variable and attach it to our current cost function *)
                                        | _ => (case existCostfct(x) 
                                                of SOME n => addCostfct(pre,x)
                                                | NONE => setCost(x,c)
                                                )
                                ) (** end setvarcost **)
                                val c = varcost()
                                val _ = setvarcost(c)
                        in
                                c
                        end
            | AST.PCaseExp _ => raise Fail "PCaseExp" (* FIXME *)
	    | AST.HandleExp(e, mc, ty) =>  0      
	    | AST.RaiseExp(e, ty) => 0
	    | AST.VarArityOpExp (oper, i, ty) => 0
	    | AST.RangeExp (lo, hi, optStep, ty) => raise Fail "FIXME (range construction)"
            | AST.PTupleExp[] => 0
            | AST.PTupleExp (exps) => let
                        (* We need the maximum of all the ptuple expressions *)
(* FIX ME : need to check for not closed expressions *)
                        fun maxList(L) = 
                                if L=[] 
                                then ~1 
                                else Int.max(hd(L), maxList(tl(L)))
                        val exps' = List.map (fn e => costAST(pre, e)) exps
                in                          
                        maxList(exps')
                end
	    | AST.PArrayExp(exps, ty) => raise Fail "unexpected PArrayExp"
	    | AST.PCompExp _ => raise Fail "unexpected PCompExp"
	    | AST.PChoiceExp _ => raise Fail "unexpected PChoiceExp"
	    | AST.SpawnExp e => costAST(pre,e)
	    | AST.ConstExp (constexp) => const_cost(constexp)
	    | AST.SeqExp (e1,e2) => costAST(pre,e1) + costAST(pre,e2)
	    | AST.OverloadExp _ => raise Fail "unresolved overloading"
	    | AST.ExpansionOptsExp (opts, e) => costAST(pre,e)
        )

        and const_cost(conexpr) = (case conexpr
                of AST.DConst(_,tylist) => length(tylist)
                | AST.LConst (_) => 0
        )
        and const_size(conexpr) = (case conexpr
                of AST.DConst(dcon,tylist) => length(tylist)
                | AST.LConst (lit, ty) => (case lit
                                        of Literal.Enum(word) => 0
                                        | Literal.StateVal(word) => 0
                                        | Literal.Tag (str) => 0
                                        | Literal.Int (num) => IntInf.toInt num
                                        | Literal.Float (num) => 0 (* NO SUPPORT FOR FLOATS *)
                                        | Literal.Char (ch) => 1
                                        | Literal.String (str) => String.size(str)
                                        | Literal.Bool (bool) => 1
                                ) (** end case **)
        )

        and cost_binding (e,pre) = (case e
                        of AST.ValBind(p, e) => let
                                                val _ = cost_pat(p,e)
                                        in
                                                costAST(pre,e)
                                        end
                        | AST.PValBind(p, e) => costAST(pre,e)
                        | AST.FunBind(lam) => costlambda(lam,pre)
                        (* these should be the primitive operators *)
                        (* | AST.PrimVBind (x, _) => mysize(V.typeof x) *)
                        | AST.PrimVBind (x, _) => let 
                                        val _ = setCost(x,1)
                                in
                                        1
                                end
                        | AST.PrimCodeBind _ => 0
        )

        (* for the case e of pat => expr take the maximum of the (pat,expr) pair *)
        and casematch (rule::rest, pre) = (case rule 
                                of AST.PatMatch (pat,e) => let
                                        val c = costAST(pre,e)
                                in      
                                        Int.max(c,casematch(rest, pre))
                                end
                                (* CondMatch is not used yet *)
                                | AST.CondMatch (pat,e1,e2) => 0
                                )
                        | casematch ([], _) = 0
        (* collects cost information for AST.FB *)
        and costlambda ([], _) = 0
                | costlambda ( A.FB(f, x, e)::l, pre ) = let
                        fun printvarcost (c) = (
                                case existCost f
                                of (SOME num) => TextIO.print(String.concat["Funbinding ", printvar(f), " to " , printvar(x)," has cost of ", Int.toString(num), "\n"])
                                | NONE =>  TextIO.print(String.concat["Funbinding ", printvar(f), " to " , printvar(x), "will get cost of ", Int.toString(c), "\n"])
                        )
                        (* look up cost of the function *)
                        val _ = printvarcost(0)
                        val c = costAST(f,e)
                        val _ = setCost (f, c)
                        val _ = printvarcost(c)
                        (* do we need to create a cost function ? *)
                        val _ = case existCostfct(f) of SOME n => costfunction(f,x) 
                                                        | NONE => ()
                in
                        costlambda(l,pre)
                end

(* MISSING pmatch *)

        (* we need to save the size information to var bindings *)
        and cost_pat (pat,exp) = (case pat 
                of AST.ConPat (dcon,_,pat) => ()
                | AST.TuplePat(pats) => ()
                (* save the size to the variable if the expression is const *)
                | AST.VarPat(var) => (case exp
                                of AST.ConstExp(const) => setSize(var,const_size(const))
                                | _ => ()
                        )
                | AST.WildPat(ty) => ()
                | AST.ConstPat(con) => ()
        )

        (* Types.ty -> cost *)
        (* type const should just contain basic types like numbers and string/chars *)
        and mysize (ty1) = let
	  fun toS(Ty.ErrorTy) = 0
	    | toS (Ty.MetaTy mv) =  0
	    | toS (Ty.VarTy tv) = 0 (* FIX ME type variable *)
	    | toS (Ty.ConTy([], tyc)) = 0
	    | toS (Ty.ConTy([ty], tyc)) = 1
	    | toS (Ty.ConTy(tys, tyc)) = length(tys)
	    (* | toS (Ty.FunTy(ty1 as Ty.FunTy _, ty2)) = TextIO.print ("funType 1\n") *) (* HIGHER ORDER FUNCTION *)
	    | toS (Ty.FunTy(ty1, ty2)) = ~1 (* SHOULD BE THE RECURSIVE FUNCTION *)
	    | toS (Ty.TupleTy []) = 0
	    | toS (Ty.TupleTy tys) = length(tys)
	  in
	    toS(ty1)
	  end

        and costfunction(parent,x) = let
                        val hash = getCostfct(parent)
                        val listitems = VTbl.listItemsi hash

                        (* we have to check if the other functions that get called have costs attached to them *)
                        (* otherwise we can't create a cost function for the parent function *)
                        fun checkhash [] = true
                        | checkhash ((key,value)::rest) = (if (V.same(key,parent)) 
                                                                then checkhash(rest)
                                                                else (case existCFname(key)
                                                                        of SOME n => checkhash(rest)
                                                                        | NONE => false
                                                                )
                                )
                in      
                        if (checkhash(listitems)) then create_cost_fct(parent,x) else TextIO.print("Can't create a cost function\n")
                end

(* FIX ME: will create a simple cost function of the form cost(x) = c + n * cost(size(x)/n) + othercostfcts(const) *)
(* FIX ME: decrease by the costs of the other function calls since we already counted them *)
        and create_cost_fct (f, x) = let
                val cost = getCost(f)

                (* create the recursive function call to the cost function *)
                val hash = getCostfct(f)
                val size = VTbl.numItems hash
                (* all items in the hash, format is (key, (value,inputsize)) *)
                (* with key = fctname , value = number of calls to itself, inputsize the maximum const input *)
                val listitems = VTbl.listItemsi hash
                                
                (* create the name of the function and the type of the input argument *)
                val costname = String.concat[Var.nameOf f, "cost"]
                val _ = TextIO.print(String.concat["Variable name is ", costname, "\n"])
                val AST.TyScheme(_, argtys) = V.typeOf x
                val estCost = Var.new (costname,argtys --> B.intTy) (* compute the appropriate function type *)

                val _ = TextIO.print(String.concat["New variable name is and type is ", printvar(estCost), "\n"])
                val _ = printtype(argtys)
                
                (* create the input argument for the cost function *)
                val inputname = String.concat[Var.nameOf f, "arg"]
                val inputCostFn = Var.new (inputname, argtys)

                (* create the recursive function call to the cost function *)
                val body = U.mkInt(cost)

                (* check if the input type of the cost function is a dcon, if so we need the sizefunction from it *)
                fun comparedcontype ((tycon,sizename)::rest) = let
                                val dcon::otherdcons = !(TyCon.returnDcons(tycon))
                        in
                                if (TypeUtil.same(DataCon.resultOf(dcon),argtys)) 
                                then let
(*FIX ME mark as used has to go to the PTuple change *)
                                        val _ = TextIO.print(String.concat["Add the following tycon to the useddconlist ", TyCon.toString(tycon), "!!\n"])
                                        val _ = TTbl.insert useddconhash(tycon,true)
                                     in
                                        SOME (sizename)
                                     end
                                else let
val _ = TextIO.print(String.concat["DIDN'T Add the following tycon to the useddconlist ", TyCon.toString(tycon), "!!\n"])
in
comparedcontype(rest)
end
                end
                | comparedcontype ([]) = NONE 
                
                (* this function will a) check if all the other calls to costfunctions are closed (have a known input argument to them) and if 
                if so, extends the body of the current cost function with that call *)
                fun addconst ((key,(value,size))::rest, body) =
                        (* is this the call to the function itself ? *)
                        (if (V.same(key,f)) 
                                then let
                                                (* check if the argument is a ConType and if we have a size function for it *)
                                               fun makebody () : AST.exp = (case comparedcontype(TTbl.listItemsi dconhash)
                                                        of SOME sizefunction => let
                                                                                        (* Apply (sizefunction,inputargument) *)
                                                                                        val mksizearg = U.mkApplyExp(vexp sizefunction,[vexp inputCostFn])
                                                                                in 
                                                                                        (U.plus(body) (U.times (U.mkInt(value)) (U.mkApplyExp(vexp estCost,[U.intDiv(mksizearg,U.mkInt(value))] )) ))
                                                                                end
                                                        | NONE => (U.plus(body) (U.times (U.mkInt(value)) (U.mkApplyExp(vexp estCost,[U.intDiv(vexp inputCostFn,U.mkInt(value))] )) ))
                                                ) (** end case **)
                                        in
                                                addconst(rest,makebody())
                                        end
                                else (if (size = ~1)
                                     then false
                                     else let
                                            val costfun as AST.FB(fname,_,_) = getCFname(key)
                                            val body = (U.plus(body) (U.mkApplyExp(vexp fname,[U.mkInt(size)])))
                                        in
                                              addconst(rest,body)
                                        end
                                )
                        )
                | addconst ([], body) = let 
                                        val estCostFn = U.mkFunWithParams(estCost,[inputCostFn], body)
                                        val _ = setCFname(f,estCostFn)
                                in
                                        true
                                end

                in
                        (* attach the costfn to the function *)
                        if (addconst(listitems,body)) then let
                                        val costfct = getCFname(f) 
                                in
                                        printlambda(costfct::[])
                                end
                        else ()
                end

(*
-----------------------------------------------------------------------------------------------
This function will create cost functions for the open function costs of the previous analysis or add unknown
to the function if we can't assign costs to it
-----------------------------------------------------------------------------------------------
*)

        fun costFctsAST (exp) = (case prune exp 
                of e as AST.LetExp(_,_) => let
                        fun letBinds (AST.LetExp(b, e)) = let
                                val b1 = binding(b)
                                val c1 = letBinds(e)
                           in   
                                ()
                           end
                           | letBinds e = costFctsAST(e)
                in      
                        letBinds(e)
                end
	    | AST.IfExp(e1, e2, e3, ty) => let
                        val _ = costFctsAST(e1)
                        val _ = costFctsAST(e2)
                        val _ = costFctsAST(e3)
                        in
                                ()
                        end
	    | AST.CaseExp(e, rules, ty) => let
                        val _ = costFctsAST(e)
                        in
                                casematch(rules)
                        end
            | AST.FunExp(x, body, ty) => costFctsAST(body)
            | AST.ApplyExp(e1, e2, ty) => let
                                val _ = costFctsAST(e1)
                        in
                                costFctsAST(e2)
                        end
            | AST.TupleExp[] => ()
            | AST.TupleExp (exps) => let
                                val _ = List.map (fn e => costFctsAST(e)) exps
                        in
                                ()
                        end
            | AST.VarExp(x, tys) => (case getCost x
                        of ~1 => TextIO.print(String.concat["Variable ", printvar(x), " has cost ", Int.toString ~1," \n (** End VAR **) \n"])
                        | _ => TextIO.print(String.concat["Variable ", printvar(x), " has cost ", Int.toString (getCost x)," \n (** End VAR **) \n"])
                )
            | AST.PCaseExp _ => raise Fail "PCaseExp" (* FIXME *)
	    | AST.HandleExp(e, mc, ty) =>  ()   
	    | AST.RaiseExp(e, ty) => ()
	    | AST.VarArityOpExp (oper, i, ty) => ()
	    | AST.RangeExp (lo, hi, optStep, ty) => raise Fail "FIXME (range construction)"
            | AST.PTupleExp[] => ()
            | AST.PTupleExp (exps) => let
                                val _ = List.map (fn e => costFctsAST(e)) exps
                        in
                                ()
                        end
	    | AST.PArrayExp(exps, ty) => raise Fail "unexpected PArrayExp"
	    | AST.PCompExp _ => raise Fail "unexpected PCompExp"
	    | AST.PChoiceExp _ => raise Fail "unexpected PChoiceExp"
	    | AST.SpawnExp e => costFctsAST(e)
	    | AST.ConstExp (constexp) => ()
	    | AST.SeqExp (e1,e2) => let
                                val _ = costFctsAST(e1) 
                        in
                                costFctsAST(e2)
                        end
	    | AST.OverloadExp _ => raise Fail "unresolved overloading"
	    | AST.ExpansionOptsExp (opts, e) => costFctsAST(e)
        )

        and binding (e) = (case e
                        of AST.ValBind(p, e) => costFctsAST(e)
                        | AST.PValBind(p, e) => costFctsAST(e)
                        | AST.FunBind(lam) => lambda(lam)
                        (* these should be the primitive operators *)
                        (* | AST.PrimVBind (x, _) => mysize(V.typeof x) *)
                        | e => ()
                        
        )
        (* for the case e of pat => expr take the maximum of the (pat,expr) pair *)
        and casematch (rule::rest) = (case rule 
                                of AST.PatMatch (pat,e) => let
                                        val _ = costFctsAST(e)
                                in      
                                        casematch(rest)
                                end
                                (* CondMatch is not used yet *)
                                | AST.CondMatch (pat,cond,e2) => raise Fail "unexpected AST.CondMatch"
                                )
                        | casematch ([]) = ()
        (* check the lambdas if there is an open cost function *)
        and lambda ([]) = () 
                | lambda ( A.FB(f, x, e)::l ) = (case existCFname(f) 
                                of NONE => let
                                        (* use the function from the previous pass to check if we can create a cost function now *)
                                        val _ = case existCostfct(f) of SOME n => costfunction(f,x) 
                                                                        | NONE => ()
                                        in
                                                lambda(l)
                                        end
                                | SOME n => lambda(l)
                )


(*
-----------------------------------------------------------------------------------------------
This function will add a sequential version and changes the original PTuple expression for chunking
PtupleExp(exp) => If(Cost > Threshhold) then Ptupleexp else Tupleexp
-----------------------------------------------------------------------------------------------
*)

        fun ASTaddchunking (exp) = (case prune exp 
                of e as AST.LetExp(_,_) => let
                        fun letBinds (AST.LetExp(b, e)) = let
                                val b1 = binding(b)
                                val c1 = letBinds(e)
                           in   
                                AST.LetExp(b1, c1)
                           end
                           | letBinds e = ASTaddchunking(e)
                in      
                        letBinds(e)
                end
	    | AST.IfExp(e1, e2, e3, ty) => AST.IfExp(ASTaddchunking(e1), ASTaddchunking(e2), ASTaddchunking(e3), ty)
	    | AST.CaseExp(e, rules, ty) => AST.CaseExp(ASTaddchunking(e) , casematch(rules), ty)
            | AST.FunExp(x, body, ty) => AST.FunExp(x, ASTaddchunking(body), ty)
            | AST.ApplyExp(e1, e2, ty) => AST.ApplyExp(ASTaddchunking(e1), ASTaddchunking(e2), ty)
            | AST.TupleExp[] => AST.TupleExp[]
            | AST.TupleExp (exps) => let
                        val exps' = List.map (fn e => ASTaddchunking( e)) exps
                in           
                        AST.TupleExp (exps')
                end
            | AST.VarExp(x, tys) => AST.VarExp(x, tys)
            | AST.PCaseExp _ => raise Fail "PCaseExp" (* FIXME *)
	    | AST.HandleExp(e, mc, ty) =>  AST.HandleExp(e, mc, ty)   
	    | AST.RaiseExp(e, ty) => AST.RaiseExp(e, ty)
	    | AST.VarArityOpExp (oper, i, ty) => AST.VarArityOpExp (oper, i, ty)
	    | AST.RangeExp (lo, hi, optStep, ty) => raise Fail "FIXME (range construction)"
            | AST.PTupleExp[] => AST.PTupleExp[]
(* we have to change ptuple expressions to the if else statement *)
            | AST.PTupleExp (exps) => ptup (exps)
	    | AST.PArrayExp(exps, ty) => raise Fail "unexpected PArrayExp"
	    | AST.PCompExp _ => raise Fail "unexpected PCompExp"
	    | AST.PChoiceExp _ => raise Fail "unexpected PChoiceExp"
	    | AST.SpawnExp e => AST.SpawnExp(ASTaddchunking(e))
	    | AST.ConstExp (constexp) => AST.ConstExp (constexp)
	    | AST.SeqExp (e1,e2) => AST.SeqExp (ASTaddchunking(e1),ASTaddchunking(e2))
	    | AST.OverloadExp _ => raise Fail "unresolved overloading"
	    | AST.ExpansionOptsExp (opts, e) => AST.ExpansionOptsExp (opts, ASTaddchunking(e)) 
        )

        and binding (e) = (case e
                        of AST.ValBind(p, e) => AST.ValBind(p,ASTaddchunking(e))
                        | AST.PValBind(p, e) => AST.PValBind(p,ASTaddchunking(e))
                        | AST.FunBind(lam) => AST.FunBind(lambda(lam))
                        (* these should be the primitive operators *)
                        (* | AST.PrimVBind (x, _) => mysize(V.typeof x) *)
                        | e => e
                        
        )
        (* for the case e of pat => expr take the maximum of the (pat,expr) pair *)
        and casematch (rule::rest) = (case rule 
                                of AST.PatMatch (pat,e) => let
                                        val e1 = ASTaddchunking(e)
                                in      
                                        AST.PatMatch(pat,e1)::casematch(rest)
                                end
                                (* CondMatch is not used yet *)
                                | AST.CondMatch (pat,cond,e2) => raise Fail "unexpected AST.CondMatch"
                                )
                        | casematch ([]) = [] 
(* FIX ME we might not need to add all of these functions *)
        and lambda ([]) = [] 
                | lambda ( A.FB(f, x, e)::l) = (case existCFname(f) 
                                of SOME fct => A.FB(f, x, ASTaddchunking(e))::fct::lambda(l)
                                | NONE => A.FB(f, x, ASTaddchunking(e))::lambda(l)
                )
(* FIX ME: add support for more than tupples *)
        and ptup (exps) = 
         (case exps
           of [e1, e2] => let
                val exps' = List.map (fn e => ASTaddchunking( e)) exps
                (* The ptuple statement (| exp1, exp2 |) will change to if (cost(exp1) + cost(exp2) > T) then (| exp1, exp2 |) else ( exp1, exp2 ) *) 
                (* where cost() a function that may have to compute the cost on the fly *)

                (* replace this by the actual costs of the statement *)
                val estCost1 = Var.new ("estCost1", B.unitTy --> B.intTy) (* compute the appropriate function type *)
                val estCostFn1 = U.mkFunWithParams(estCost1,[], U.mkInt 5)
                val estCost2 = Var.new ("estCost2", B.unitTy --> B.intTy)
                val estCostFn2 = U.mkFunWithParams(estCost2, [], U.mkInt 6)
                val estCost = Var.new ("estCost", Basis.intTy)
                (* creates a fun ... and ... *)
                val bindEstCosts = AST.FunBind [estCostFn1, estCostFn2]
                val bindEstCost = AST.ValBind(AST.VarPat estCost, 
                                              U.mkMax(U.mkApplyExp(vexp estCost1, [U.unitExp]),
                                                      U.mkApplyExp(vexp estCost2, [U.unitExp])))
                val threshold = U.mkInt(Tlimit)
                val test = U.intGT(vexp estCost, threshold)
                in           
                  U.mkLetExp([bindEstCosts,bindEstCost], U.mkIfExp(test,AST.PTupleExp(exps'),AST.TupleExp(exps')))
                end
            | _ => raise Fail "only pair ptups currently supported in this branch"
          (* end case *))


(*
-----------------------------------------------------------------------------------------------
This function will check if we need to manipulate tycon and dcon in order to account for size informations
-----------------------------------------------------------------------------------------------
*)

        fun changesize (body) = let
                        fun checkdcon ((tycon,flag)::rest,returnlist ) = 
                                if (flag) then checkdcon(rest,returnlist@[tycon]) else checkdcon(rest,returnlist)
                        | checkdcon ([], returnlist) = returnlist

                        val dconlist = checkdcon(TTbl.listItemsi useddconhash,[])

                in
                        if (List.null dconlist) 
                        then body 
                        else let
                                fun infos (tycon::rest) = let
                                                val mylist = !(TyCon.returnDcons(tycon))
fun printdcons(dcon::rest) = let
                val _ = TextIO.print(String.concat[Option.getOpt (Option.map TypeUtil.toString (DataCon.argTypeOf(dcon)), ""),"\n"] )
        in
                printdcons(rest)
        end
  | printdcons([]) = ()

val _ = printdcons(mylist)
                                                in
                                                        infos(rest)
                                                end
                                | infos([]) = ()
                                val _ = infos(dconlist)
                        in
                                ASTchangesize(body,dconlist)
                        end
                end

        and ASTchangesize (exp,dconlist) : AST.exp = (case prune exp 
                of e as AST.LetExp(_,_) => let
                        fun letBinds (AST.LetExp(b, e)) = let
                                val b1 = binding_size(b,dconlist)
                                val c1 = letBinds(e)
                           in   
                                AST.LetExp(b1, c1)
                           end
                           | letBinds e = ASTchangesize(e,dconlist)
                in      
                        letBinds(e)
                end
	    | AST.IfExp(e1, e2, e3, ty) => AST.IfExp(ASTchangesize(e1,dconlist), ASTchangesize(e2,dconlist), ASTchangesize(e3,dconlist), ty)
	    | AST.CaseExp(e, rules, ty) => AST.CaseExp(ASTchangesize(e,dconlist) , casematch_size(rules,dconlist), ty)
            | AST.FunExp(x, body, ty) => AST.FunExp(x, ASTchangesize(body,dconlist), ty)
            | AST.ApplyExp(e1, e2, ty) => AST.ApplyExp(ASTchangesize(e1,dconlist), ASTchangesize(e2,dconlist), ty)
            | AST.TupleExp[] => AST.TupleExp[]
            | AST.TupleExp (exps) => let
                        val exps' = List.map (fn e => ASTchangesize(e,dconlist)) exps
                in           
                        AST.TupleExp (exps')
                end
            | AST.VarExp(x, tys) => AST.VarExp(x, tys)
            | AST.PCaseExp _ => raise Fail "PCaseExp" (* FIXME *)
	    | AST.HandleExp(e, mc, ty) =>  AST.HandleExp(e, mc, ty)   
	    | AST.RaiseExp(e, ty) => AST.RaiseExp(e, ty)
	    | AST.VarArityOpExp (oper, i, ty) => AST.VarArityOpExp (oper, i, ty)
	    | AST.RangeExp (lo, hi, optStep, ty) => raise Fail "FIXME (range construction)"
            | AST.PTupleExp[] => AST.PTupleExp[]
            | AST.PTupleExp (exps) => let
                        val exps' = List.map (fn e => ASTchangesize(e,dconlist)) exps
                in           
                        AST.PTupleExp (exps')
                end
	    | AST.PArrayExp(exps, ty) => raise Fail "unexpected PArrayExp"
	    | AST.PCompExp _ => raise Fail "unexpected PCompExp"
	    | AST.PChoiceExp _ => raise Fail "unexpected PChoiceExp"
	    | AST.SpawnExp e => AST.SpawnExp(ASTchangesize(e,dconlist))
	    | AST.ConstExp (constexp) => AST.ConstExp (constexp)
	    | AST.SeqExp (e1,e2) => AST.SeqExp (ASTchangesize(e1,dconlist),ASTchangesize(e2,dconlist))
	    | AST.OverloadExp _ => raise Fail "unresolved overloading"
	    | AST.ExpansionOptsExp (opts, e) => AST.ExpansionOptsExp (opts, ASTchangesize(e,dconlist)) 
        )

        and binding_size (e,dconlist) = (case e
                        of AST.ValBind(p, e) => AST.ValBind(p,ASTchangesize(e,dconlist))
                        | AST.PValBind(p, e) => AST.PValBind(p,ASTchangesize(e,dconlist))
                        | AST.FunBind(lam) => AST.FunBind(lambda_size(lam,dconlist))
                        (* these should be the primitive operators *)
                        (* | AST.PrimVBind (x, _) => mysize(V.typeof x) *)
                        | e => e
                        
        )
        (* for the case e of pat => expr take the maximum of the (pat,expr) pair *)
        and casematch_size (rule::rest,dconlist) = (case rule 
                                of AST.PatMatch (pat,e) => let
                                        val e1 = ASTchangesize(e,dconlist)
                                in      
                                        AST.PatMatch(pat,e1)::casematch_size(rest,dconlist)
                                end
                                (* CondMatch is not used yet *)
                                | AST.CondMatch (pat,cond,e2) => raise Fail "unexpected AST.CondMatch"
                                )
                        | casematch_size ([], _) = [] 

        and lambda_size ([],dconlist) = [] 
            | lambda_size ( A.FB(f, x, e)::l, dconlist) = A.FB(f, x, ASTchangesize(e,dconlist))::lambda_size(l,dconlist)

(*
-----------------------------------------------------------------------------------------------
The main function of this structure
-----------------------------------------------------------------------------------------------
*)

        (* add costs to basis values before calling the analysis *)
        fun preassigncosts () = let
                        val _ = setCost(B.eq,0)
                        val _ = setCost(B.neq,0)
                in            
                        ()
                end

        fun translate (body) = let      
                val _ = preassigncosts()
                val _ = ASTcollectDCON(body)
                val _ = costAST(dummyvar() , body)
                val _ = printCost(body)
                (* val _ = costFctsAST(body) *)
                (* val body' = ASTaddchunking(body) *)
                val _ = changesize(body)
                (* val _ = printCost(body') *)
        in   
                body
        end

end