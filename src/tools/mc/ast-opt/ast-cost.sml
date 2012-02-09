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
    structure D   = DelayedBasis
    structure DV  = D.Var


    infixr 2 -->
    fun t1 --> t2 = Ty.FunTy (t1, t2)

    fun vexp v = AST.VarExp (v, [])

    fun dummyvar () = (Var.new ("dummy", B.unitTy))
    
    (* The size of the hash that we use to store information about function calls within a function *)
    val hashsize = 8
    
    (* unkown costs is -1 *)
    val Cunkown = ~1

    (* can't compute cost *)
    val nocost = ~2

    (* cut off limit for the costs *)
    val Tlimit = 8000

    fun printvar(x) = String.concat[Var.toString x, " : ", TypeUtil.schemeToString (Var.typeOf x),"\n"]
 
    (* this hash is used to save all the data constructor that we need to rewrite to add size information *)
    val tyconhash : AST.var TTbl.hash_table = TTbl.mkTable (20, Fail "tyconhash error")

    (* we need to check if we actually used the dcon in a cost function *)
    val usedtyconhash : bool TTbl.hash_table = TTbl.mkTable (20, Fail "usedtyconhash error")

    (* this maps the cost function to the size function we need to use for the input argument to the cost function *)
    val costfcttosizefct : AST.var VTbl.hash_table = VTbl.mkTable (20, Fail "Error in the mapping of costfcttosizefct")
 
    (* this list saves all the functions we need to add to the tree after creating them *)
    val fctlist : AST.lambda list ref = ref[]

    (* this list saves all the functions we need to add to the tree after creating them *)
    val anonvar : AST.var ref = ref (Var.new ("dummy", B.unitTy))

    (* boolean to indicate if we actually have to change the program *)
    val changeprogram : bool ref = ref false


(*
-----------------------------------------------------------------------------------------------
helper functions
-----------------------------------------------------------------------------------------------
*)

      fun addtofctlist(list,exp) = list := [exp]@(!list)

      fun addvartoannon (exp) = anonvar := exp
 
      fun floatlog n = U.mkApplyExp (A.VarExp (DV.float_log(), []), [n])
      fun floatlog10 n = U.mkApplyExp (A.VarExp (DV.float_log10(), []), [n])
      fun floattoint n = U.mkApplyExp (A.VarExp (DV.float_toint(), []), [n])
      fun intlog n = floattoint(floatlog (n))
      fun intlog10 n = floattoint(floatlog10 (n))
      fun floatpow (n, m) = U.mkApplyExp (A.VarExp (DV.float_powint(), []), [n,m])   
      fun intpow (n, m) = floattoint(floatpow (n,m))

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
Collect user datacon in apply exp, WE SHOULD TRY AND REMOVE THIS PASS
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
                                                        case TTbl.find tyconhash (tyconname) 
                                                        of SOME n => ()
                                                        | NONE => let
                                                                        val typevar = U.dcontotype(AST.ConstExp(const))
                                                                        val sizename = String.concat[TyCon.toString (tyconname), "size"]
                                                                        val estSize = Var.new (sizename,typevar --> B.intTy)
                                                                   in
                                                                        TTbl.insert tyconhash (tyconname,estSize);
                                                                        TTbl.insert usedtyconhash(tyconname,false)
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
                        val _ = printtype(ty)
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
                        of AST.ConPat (dcon, tylist, pat) => let
                                        val _ = TextIO.print("DCON pat \n")	(* data-constructor application *)
                                        val _ = TextIO.print(String.concat["DConst pat with signature ", DataCon.toString(dcon), "\n"])
                                        val _ = printtypelist(tylist)
                                in
                                        TextIO.print("Rest of DCON pattern \n");
                                        printpat(pat);
                                        TextIO.print("END Rest of DCON pattern \n")
                                end
                        | AST.TuplePat (patlist) => let
                                        val _ = TextIO.print("TuplePat \n")
                                        val _ = List.map (fn e => printpat(e) ) patlist
                                in      
                                        TextIO.print("END TuplePat \n")
                                end
                        | AST.VarPat (var) => (case existSize(var)
                                                of SOME n => TextIO.print(String.concat["Size saved with variable ", printvar(var), " is ", Int.toString(n),"\n"]) 
                                                | NONE => printCost(AST.VarExp(var,[]))
                                        )
                        | AST.WildPat (ty) => let
                                                val _ = TextIO.print("WildPat \n")
                                        in
                                                printtype(ty)
                                        end
                        | AST.ConstPat (const) => ()
                )

        (* for the case e of pat => expr take the maximum of the (pat,expr) pair *)
        and printcasematch (rule::rest) = (case rule 
                                of AST.PatMatch (pat,e) => let
                                        val _ = TextIO.print("PatMatch \n")
                                        val _ = printpat(pat)
                                        val _ = TextIO.print(" (** End PatMatch **) \n")
                                        val _ = TextIO.print("EXP in casematch \n")
                                        val _ = printCost(e)
                                        val _ = TextIO.print("(** End EXP in casematch **)\n")
                                in      
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
                        | Ty.ConTy (tylist, tycon) => let
                                        val _ = TextIO.print("Contype with: \n")
                                in      
                                        printtypelist(tylist);
                                        TextIO.print(TyCon.toString(tycon));
                                        TextIO.print("END Contype *** \n")
                                end
                        | Ty.FunTy (ty1, ty2) => TextIO.print("Funtype \n")
                        | Ty.TupleTy (tylist) => let
                                        val _ = TextIO.print("Tupletype with: \n")
                                in      
                                        printtypelist(tylist);
                                        TextIO.print("END Tupletype *** \n")
                                end
)
        and printtypelist(ty::rest) = let     
                        val _ = printtype(ty)
                in
                        printtypelist(rest)
                end
        | printtypelist([]) = ()

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
                                                        case TTbl.find tyconhash (tyconname) 
                                                        of SOME n => ()
                                                        | NONE => let
                                                                        val typevar = U.dcontotype(AST.ConstExp(const))
                                                                        val sizename = String.concat[TyCon.toString (tyconname), "size"]
                                                                        val estSize = Var.new (sizename,typevar --> B.intTy)
                                                                   in
                                                                        TTbl.insert tyconhash (tyconname,estSize);
                                                                        TTbl.insert usedtyconhash(tyconname,false)
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
                                        (*  ~1 means the cost are unknown and we add it to the hashtable *)
                                        of ~1 => addCostfct(pre,x)
                                        | ~2 => setCost(pre,c)
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
                        (* look up cost of the function *)
                        val c = costAST(f,e)
                        val _ = (case existCost(f)
                                of SOME n => (case n 
                                                of ~2 => ()
                                                | _ => setCost (f, c)
                                              )
                                | NONE => setCost (f, c)
                                )

                        (* do we need to create a cost function ? *)
                        val _ = case existCostfct(f) of SOME n => costfunction(f,x) 
                                                        | NONE => ()
                in
                        costlambda(l,pre)
                end

        (* pmatch is not in the tree anymore at this point *)

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
	    | toS (Ty.FunTy(ty1 as Ty.FunTy _, ty2)) = nocost (* HIGHER ORDER FUNCTION *)
	    | toS (Ty.FunTy(ty1, ty2)) = Cunkown (* SHOULD BE THE RECURSIVE FUNCTION *)
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
                val estCost = Var.new (costname,B.intTy --> B.intTy) (* compute the appropriate function type *)

                val _ = TextIO.print(String.concat["New variable name is and type is ", printvar(estCost), "\n"])
                val _ = printtype(argtys)
                
                (* create the input argument for the cost function *)
                val inputname = String.concat[Var.nameOf f, "arg"]
                val inputCostFn = Var.new (inputname, Basis.intTy)

                (* create the recursive function call to the cost function *)
                val body = U.mkInt(cost)

                (* check if the input type of the cost function is a dcon, if so we need the sizefunction from it *)
                fun comparedcontype ((tycon,sizename)::rest) = let
                                val dcon::otherdcons = !(TyCon.returnDcons(tycon))
                        in
                                if (TypeUtil.same(DataCon.resultOf(dcon),argtys)) 
                                then let
(* FIX ME mark as used has to go to the PTuple change *)
                                        val _ = TextIO.print(String.concat["Add the following tycon to the useddconlist ", TyCon.toString(tycon), "!!\n"])
                                        val _ = TTbl.insert usedtyconhash(tycon,true)
                                        (* save the size function to the costfct *)
                                        val sizename = (case TTbl.find tyconhash (tycon) 
                                                        of SOME n => n
                                                        | NONE => raise Fail "Problem can't find sizefct "
                                        )
                                        val _ = VTbl.insert costfcttosizefct (estCost,sizename)
                                     in
                                        true
                                     end
                                else comparedcontype(rest)
                end
                | comparedcontype ([]) = false

                val recursiveconst =  ref 0
                
                (* this function will a) check if all the other calls to costfunctions are closed (have a known input argument to them) and if 
                if so, extends the body of the current cost function with that call *)
                fun addconst ((key,(value,size))::rest, body) =
                        (* this is the call to the function itself *)
                        (if (V.same(key,f)) 
                                then let
                                                (* check if the argument is a ConType then we have to add the statement n * fct(input/n) *)
                                               fun makebody () : AST.exp = (if (comparedcontype(TTbl.listItemsi tyconhash))
                                                                                then 
                                                                                     (*   let
                                                                                                val _ = recursiveconst := value
                                                                                        in
                                                                                                body
                                                                                        end *)


                                                                                        (U.plus(body) (U.times (U.mkInt(value)) (U.mkApplyExp(vexp estCost,[U.intDiv(vexp inputCostFn,U.mkInt(value))] )) ))


                                                                                else (U.plus(body) (U.times (U.mkInt(value)) (U.mkApplyExp(vexp estCost,[U.intDiv(vexp inputCostFn,U.mkInt(value))] )) ))
                                                ) (** end if **)
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
                                        (* We don't use this representation since it is significantly slower *)
                                        (* check if we have a recursive function *)
                                      (*  val body' = if (!recursiveconst <> 0) 
                                                then let
                                                        (* we need to create the closed form expression of the form 
                                                        f n = c * (−1 + k^( 1+log_k (n) ) ) / (-1 + k) + (a*k^(1+log_k (n)) −a*k^log_k (n) ) / (-1 + k) if k != 1 
                                                        otherwise f n = ( a*log(k) + c*log(n) ) / log(k) = a + c * log_k (n)
                                                        and we use the fact that log_b(n) = log_i (n) / log_i (k)
                                                        *)
                                                        val k = !recursiveconst
                                                        val mybody = if ( k = 1 ) 
                                                        then 
                                                                let
                                                                        (* ln(n) / ln(k) *)
                                                                        val logaritm = (U.intDiv (intlog(vexp inputCostFn)  , (intlog(U.mkInt(k))) ) )
                                                                in
                                                                        U.plus (U.times(body) (logaritm)) (U.mkInt(1))
                                                                end
                                                        else let
                                                               
                                                                val logaritm = (U.intDiv (intlog(vexp inputCostFn)  , (intlog(U.mkInt(k))) ) )
                                                                (* k^( 1+log_k (n) ) *)
                                                                val term1 =  (intpow(U.mkInt(k), (U.plus(U.mkInt(1)) (logaritm))))
                                                                val denom1 = U.plus (U.mkInt(~1)) (term1)
                                                                (* c * denom1 / (-1 + k) *)
                                                                val firstterm = U.intDiv((U.times (body) (denom1)) , (U.plus (U.mkInt(~1)) (U.mkInt(k))))
                                                
                                                                (* (a*k^(1+log_k (n)) *)
                                                                val term2 = U.times (U.mkInt(1)) (term1)
                                                                (* −a* k ^ log_k (n) *)
                                                                val term3 = U.times (U.mkInt(~1)) (intpow(U.mkInt(k), (logaritm)))
                                                                (* term2 + term3 / (-1 + k) *)
                                                                val secondterm = U.intDiv(U.plus (term2) (term3),  (U.plus (U.mkInt(~1)) (U.mkInt(k)) ))
                                                              in
                                                                (U.plus (firstterm) (secondterm))
                                                              end
                                                in
                                                        mybody
                                                end
                                        else body *)


                                        val threshold = U.mkInt(0)
                                        val test = U.intGT(vexp inputCostFn, threshold)

                                        val body = U.mkIfExp(test,body,U.mkInt(0))
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
                                        addtofctlist(fctlist,costfct);
                                        printlambda(costfct::[])
                                end
                        else ()
                end

(*
-----------------------------------------------------------------------------------------------
This function will create cost functions for the open function costs of the previous analysis or add unknown
to the function if we can't assign costs to it. This function will get called on the arguments of a
PTuple and will return the static costs
-----------------------------------------------------------------------------------------------
*)
        (* we will save all the function calls here that come out of the analysis *)
(*        val calledfcts : AST.lambda list ref = ref[]
        val functionknown : bool ref = ref true

        fun setknown (input) = functionknown := input

        fun analysePTuple(exp) = let
                        in
                                calledfcts := [];
                                functionknown := true;
                                costFctsAST(exp)
                        end

        fun costFctsAST (exp) = (case prune exp 
                of e as AST.LetExp(_,_) => let
                        fun letBinds (AST.LetExp(b, e)) = let
                                val c1 = binding(b)
                                val c2 = letBinds(e)
                           in   
                                c1 + c2
                           end
                           | letBinds e = costFctsAST(e)
                in      
                        letBinds(e)
                end
	    | AST.IfExp(e1, e2, e3, ty) => let
                        val c1 = costFctsAST(e1)
                        val c2 = costFctsAST(e2)
                        val c3 = costFctsAST(e3)
                        in
                                1 + c1 + Int.max(c2,c3)
                        end
	    | AST.CaseExp(e, rules, ty) => let
                                val c1 = costFctsAST(e)
                                val c2 = casematch(rules)
                        in
                                c1 + c2
                        end
            | AST.FunExp(x, body, ty) => getCost(x,c)
            | AST.ApplyExp(e1, e2, ty) => let
                                val c1 = costFctsAST(e1)
                                val c2 = costFctsAST(e2)
                        in
                                c1 + c2 + 1
                        end
            | AST.TupleExp[] => 0
            | AST.TupleExp (exps) => let
                                val exps' = List.map (fn e => costFctsAST(e)) exps
                                fun addL(L) =
                                        if L=[] 
                                        then 0 
                                        else hd(L) + addL(tl(L))
                        in           
                                addL(exps')
                        end
            | AST.VarExp(x, tys) => (** TEST **)
            | AST.PCaseExp _ => raise Fail "PCaseExp" (* FIXME *)
	    | AST.HandleExp(e, mc, ty) =>  0   
	    | AST.RaiseExp(e, ty) => 0
	    | AST.VarArityOpExp (oper, i, ty) => 0
	    | AST.RangeExp (lo, hi, optStep, ty) => raise Fail "FIXME (range construction)"
            | AST.PTupleExp[] => 0
            | AST.PTupleExp (exps) => let
                                val _ = List.map (fn e => costFctsAST(e)) exps
                                fun maxList(L) = 
                                        if L=[] 
                                        then ~1 
                                        else Int.max(hd(L), maxList(tl(L)))
                        in                          
                                maxList(exps')
                        end
	    | AST.PArrayExp(exps, ty) => raise Fail "unexpected PArrayExp"
	    | AST.PCompExp _ => raise Fail "unexpected PCompExp"
	    | AST.PChoiceExp _ => raise Fail "unexpected PChoiceExp"
	    | AST.SpawnExp e => costFctsAST(e)
	    | AST.ConstExp (constexp) => 0
	    | AST.SeqExp (e1,e2) => let
                                val c1 = costFctsAST(e1) 
                                val c2 = costFctsAST(e2)
                        in
                                c1 + c2
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
                        | e => 0
                        
        )
        (* for the case e of pat => expr take the maximum of the (pat,expr) pair *)
        and casematch (rule::rest) = (case rule 
                                of AST.PatMatch (pat,e) => let
                                        val c1 = costFctsAST(e)
                                in      
                                        c1 + casematch(rest)
                                end
                                (* CondMatch is not used yet *)
                                | AST.CondMatch (pat,cond,e2) => raise Fail "unexpected AST.CondMatch"
                                )
                        | casematch ([]) = 0
        (* check the lambdas if there is an open cost function *)
        and lambda ([]) = () 
                | lambda ( A.FB(f, x, e)::l ) = (case existCost(f) 
                                        of SOME n => (case n 
                                                      of ~2 => let
                                                           (* this function has unknown costs and we don't need to do anything anymore *)
                                                            val _ = setknown(false)
                                                        in
                                                                 A.FB(f, x, e)::l
                                                        end
                                                    | _ => (case existCostfct(f) 
                                                           of NONE => n + lambda(l)
                                                           | SOME fct => (case existCFname(f) 
                                                                          of NONE => let
                                                                                (* use the function from the previous pass to check if we can create a cost function now *)
                                                                                val _ = case existCostfct(f) of SOME n => costfunction(f,x) 
                                                                                                                | NONE => ()
                                                                                in
                                                                                        lambda(l)
                                                                                end
                                                                        | SOME n => let                
                                                                                
                                                                                lambda(l)
                                                        )
*)

(*
-----------------------------------------------------------------------------------------------
This function will add a sequential version and changes the original PTuple expression for chunking
PtupleExp(exp) => If(Cost > Threshhold) then Ptupleexp else Tupleexp
-----------------------------------------------------------------------------------------------
*)


        fun removeannotation (var) = (case existCost(var) 
                                      of SOME _ => let 
                                                        val _ = clearCost(var)
                                                   in
                                                        (case existCostfct(var)
                                                         of SOME _ => let
                                                                        val _ = clearCostfct(var)
                                                                      in
                                                                        (case existCFname(var)
                                                                         of SOME _ => clearCFname(var)
                                                                         | NONE => ()
                                                                        )
                                                                      end
                                                        | NONE => ()
                                                        )
                                                   end
                                        | NONE => ()
                                )


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
                        val exps' = List.map (fn e => ASTaddchunking(e)) exps
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
        and lambda ([]) = [] 
                | lambda ( A.FB(f, x, e)::l) = A.FB(f, x, ASTaddchunking(e))::lambda(l)
                
        and ptup (exps) = let
         
                val lambdas = ref[]

                (* this function will check if the expression in the ptuple is an applyexp with a function call that has a costfct attached *)
(* FIX ME how about exp with fix costs *)
                fun getcostfct (exp::rest,sizenamelist) = (case exp
                                        of AST.ApplyExp(e1, e2, ty) => (case e1
                                                                        of AST.VarExp(x,tys) =>  (case existCFname(x)
                                                                                                of SOME fctname => let
                                                                                                                  val A.FB(f, x, e) = fctname
                                                                                                                  in
                                                                                                                        (case VTbl.find costfcttosizefct (f) 
                                                                                                                                of SOME sizename => let
                                                                                                                        val applysize = U.mkApplyExp(vexp sizename, [e2])
                                                                                                                        val applycst = U.mkApplyExp(vexp f, [applysize]) 
                                                                                                                        in
                                                                                                                          lambdas := !lambdas@[applycst];
                                                                                                                          getcostfct(rest,sizenamelist@[sizename])
                                                                                                                        end
                                                                                                                                | NONE => false
                                                                                                                                )
                                                                                                                  end
                                                                                                | NONE => false
                                                                                                )
                                                                        | _ => false
                                                                        )
                                        | _ => false
                                        )
                | getcostfct ([],sizenamelist) = let


(*                                fun addfcttolist (sizename) = 

val sizename = (case TTbl.find tyconhash (tycon) 
of SOME n => n
| NONE => raise Fail "Problem can't find sizefct "
) 
val _ = TTbl.insert usedtyconhash(tycon,true)
addtofctlist(fctlist,costfct);

*)
                                in
                                        changeprogram := true;
                                        true
                                end
  
                val check = getcostfct(exps,[])
                
                in
                  if (check) 
                  then let
                        val _ = TextIO.print("*** PRINT PTUPLE CHANGES ***") 
                        val _ = printCost(AST.PTupleExp(exps))

                        fun makeif (myexp::rest) = (U.plus(myexp) (makeif(rest)))
                         | makeif ([]) = U.mkInt(0)

                        val threshold = U.mkInt(Tlimit)
                        val test = U.intGT(makeif(!lambdas), threshold)
                  in
                        U.mkIfExp(test,AST.PTupleExp(exps),AST.TupleExp(exps))

                  end
                  else let
                        val exps' = List.map (fn e => ASTaddchunking(e)) exps
                       in
                        AST.PTupleExp(exps')
                       end
                end

(*
-----------------------------------------------------------------------------------------------
This function will check if we need to manipulate tycon and dcon in order to account for size informations
-----------------------------------------------------------------------------------------------
*)
                                                
        val attachfcts : int ref = ref 1

        fun changesize (body) = let
                        fun checktycon ((tycon,flag)::rest,returnlist ) = 
                                if (flag) then checktycon(rest,returnlist@[tycon]) else checktycon(rest,returnlist)
                        | checktycon ([], returnlist) = returnlist

                        val tyconlist = checktycon(TTbl.listItemsi usedtyconhash,[])

                in
                        if (List.null tyconlist) 
                        then body 
                        else let
                                
                                (* lets try and change the existing dcon *)
                                fun changedycon (tycon::rest) = let
                                        (* changes the types and returns a new dcon *)
                                        fun newdcon (AST.DCon {id, name, owner, argTy}, newargs) = AST.DCon{id = id, name = name, owner = owner, argTy = newargs}
                                        (* set the new cons in the typeconstructor *)
                                        fun new (tyc as Types.Tyc{def=Types.DataTyc{nCons, cons}, ...},newcons) = cons := newcons

                                        fun newargs (dcon) = (case (DataCon.argTypeOf(dcon))
                                                                        of SOME ty => SOME (changedcon(ty))
                                                                        | NONE => NONE
                                                                        )
                                                        
                                        fun newdcons (dcon::rest,list) = newdcons(rest,list@[newdcon(dcon,newargs(dcon))])
                                         |  newdcons ([],list) = list

                                        fun printdcons(dcon::rest) = let
                                                                val _ = TextIO.print(String.concat[Option.getOpt (Option.map TypeUtil.toString (DataCon.argTypeOf(dcon)), ""),"\n"] )
                                                                val _ = (case (DataCon.argTypeOf(dcon))
                                                                        of SOME ty => printtype(ty)
                                                                        | NONE => ()
                                                                        )
                                                        in
                                                                printdcons(rest)
                                                        end
                                                  | printdcons([]) = ()

                                        val dcons = newdcons(!(TyCon.returnDcons(tycon)),[])
                                in
                                        new(tycon,dcons);
                                        printdcons(!(TyCon.returnDcons(tycon)));
                                        changedycon(rest)
                                end
                                | changedycon([]) = ()

                                val _ = changedycon(tyconlist)

                        in
                                (* create the corresponding size functions *)
                                createsizefcts(tyconlist); 
                                ASTchangesize(body,tyconlist)
                        end
                end

        and changedcon (ctype) = (case ctype
                        of Ty.ConTy (tylist, tycon) => Ty.TupleTy([Basis.intTy,Ty.ConTy (tylist, tycon)])
                        | Ty.TupleTy (tylist) => Ty.TupleTy(Basis.intTy::tylist)
                        | _ => raise Fail "Error rewriting the DataConstructor"
        )

        and createsizefcts(tycon::rest) = let
                                (* read out the predefined name of the sizefunction
                                   which is a var with type TYCON -> int *)
                                val estSize = (case TTbl.find tyconhash tycon
                                                of NONE => raise Fail "Missing size argument for the tycon \n"
                                                | SOME varname => varname
                                                )
                                (* create the input argument to the size function and set the correct type *)
                                val inputname = String.concat[Var.nameOf estSize, "arg"]
                                val AST.TyScheme(_, tys) = V.typeOf(estSize)
                                val argty = (case tys 
                                             of Ty.FunTy(ty,_) => ty
                                             | _ => raise Fail "No Correct type in the sizefctname"
                                        )
                                val inputestSizeFn = Var.new (inputname, argty)
                                (* get all the dcons of the current tycon *)
                                val dcons = !(TyCon.returnDcons(tycon))

                                val matches = ref[]

                                (* we have to create a size function that looks like this:
                                fun sizetree (input : tree) : int = case input 
                                                of Leaf(int * int):x(int * int) => case x(int * int)
                                                                        of (y:int, _ :int) => y:int
                                                | Node(int * tree * tree):x(int * tree * tree) => case x(int * tree * tree)
                                                                                of (y:int, _:tree,_:tree) => y:int
                                *)


                                fun makematch (dcon::rest) = let
                                                (* the variable with the size information *)
                                                val myvar =  Var.new ("_anon_", Basis.intTy)
                                                val patlist = ref [AST.VarPat (myvar)]
                                                (* type of the dcon we know the first one is an int *)
                                                val Ty.TupleTy(arg::tuplerest) = (case DataCon.argTypeOf(dcon)
                                                                                  of SOME n => n
                                                                                 | NONE => raise Fail "Error in reading out the contype if the dcon in creating size function"
                                                                                )
                                                (* this function adds the wildpat to the of exp since we just need the first argument of the dcon *)
                                                fun addwilds (type1::rest) = let
                                                                        
                                                                in
                                                                        patlist := !patlist@[AST.WildPat (type1)];
                                                                        addwilds(rest)
                                                                end
                                                | addwilds ([]) = ()
                
                                                (* create the second case exp *)
                                                val _ = addwilds(tuplerest)
                                                val matchdconvar = Var.new ("_anon_", Ty.TupleTy(arg::tuplerest))
                                                val secondpat = AST.PatMatch(AST.TuplePat(!patlist),vexp myvar)
                                                val secondcase = U.mkCaseExp(vexp matchdconvar, [secondpat])

                                        in
                                                matches := !matches@[AST.PatMatch (AST.ConPat (dcon, arg::tuplerest, AST.VarPat(matchdconvar)),secondcase)];
                                                makematch(rest)
                                        end
                                | makematch ([]) = ()

                                val _ = makematch(dcons)
                                val caseexp = U.mkCaseExp(vexp inputestSizeFn, (!matches))

                                (* create the function *)
                                val sizefct = U.mkFunWithParams(estSize,[inputestSizeFn], caseexp)
                                val _ = addtofctlist(fctlist, sizefct)

                                in
                                        
                                        createsizefcts(rest)
                                end
        | createsizefcts([]) = ()


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
                (* if the type signature is equal to one of the dcons, we have to add another size variable to the pattern *)
            (* This just works because we don't use CondMatch yet *)
	    | AST.CaseExp(e, rules, ty) => if (testvar(e))
                                           then (case rules
                                                (* this function will change the pattern in case we added size information to it and it is a recursive case
                                                we have to changecase (DCON(rec1,rec2,..) => case(x:rec1,..) => x,... to  DCON(int,rec1,rec2,..) => case(_:int,x:rec1,..) => x,... *)
                                                of AST.PatMatch (pat,patexp)::rest =>
                                                        (case pat
                                                        of AST.TuplePat (patlist) => let
                                                                        val patexp = AST.PatMatch(AST.TuplePat (AST.WildPat(Basis.intTy)::patlist),patexp)::rest
                                                                in
                                                                        AST.CaseExp(e, casematch_size(patexp,dconlist), ty)
                                                                end
                                                        | _ => raise Fail "Something wrong with the pattern in CaseExp \n"
                                                        )
                                                | _ => raise Fail "Something wrong with the pattern in CaseExp \n"
                                                )
                                           else AST.CaseExp(ASTchangesize(e,dconlist) , casematch_size(rules,dconlist), ty)
                                           
            | AST.FunExp(x, body, ty) => AST.FunExp(x, ASTchangesize(body,dconlist), ty)
            | AST.ApplyExp(e1, e2, ty) => let

                                        fun changestat (constexp, tycon) = let 
                                                        
                                                        (* read out the predefined name of the sizefunction
                                                                which is a var with type TYCON -> int *)
                                                        val sizename = (case TTbl.find tyconhash tycon
                                                                        of NONE => raise Fail "Missing size argument for the tycon \n"
                                                                        | SOME varname => varname
                                                                        )
                                                        
                                                        (* we need to change the apply statement to account for sizes e.g. 
                                                           Apply(DCON(tree,tree),(mktree(exp1),mktree(exp2))) 
                                                          => let
                                                                val (x1, x2) = (|mktree(exp1), mktree(exp2)|)

                                                                val size = sizetree(x1) + sizetree(x2) + 1
                                                             in 
                                                                Applt(DCON(int, tree, tree), (size,x1,x2))
                                                             end
                                                         *)
                                                
                                                        (* LetExp(binding,exp), create the bindings which are the val statements *)
                                                        val expressions = ref[]
                                                        val arguments = ref[]
                                                        val pattern = ref[]
                                                        val bindings = ref[]

                                                    fun createbinding(expr::rest) = let
                                                                        val patvar = Var.new ("_anon_",TypeOf.exp(expr))
                                                                        val valpat = AST.VarPat(patvar)
                                                                        (* val valbind = AST.ValBind(valpat,expr) *)
                                                                in     
                                                                        arguments := !arguments@[patvar];
                                                                        expressions := !expressions@[expr];
                                                                        pattern := !pattern@[valpat];
                                                                        createbinding(rest)
                                                                end
                                                        | createbinding([]) = let
                                                                (* create the size val *)
                                                                        val patvartemp = Var.new ("mysizetemp",Basis.intTy)
                                                                        val valpatemp = AST.VarPat(patvartemp)

                                                                        fun mksize (x::rest) = let
                                                                                                val apply = U.mkApplyExp(vexp sizename,[vexp x])
                                                                                        in
                                                                                               (U.plus(apply) (mksize(rest)))
                                                                                        end
                                                                        | mksize([]) = (U.mkInt(1))

                                                                        val expr = mksize(!arguments)
                                                                        val valbindtemp = AST.ValBind(valpatemp,expr)
                                                                        (* we want to optimize the size and say if (size > Tlimit) then Tlimit  else size *)
                                                                        val patvar = Var.new ("mysize",Basis.intTy)
                                                                        val valpat = AST.VarPat(patvar)
                                                                        val threshold = U.mkInt(Tlimit)
                                                                        val test = U.intGT(vexp patvartemp, threshold)
                                                                        val ifexp = U.mkIfExp(test,threshold,vexp patvartemp)

                                                                        val valbind = AST.ValBind(valpat,ifexp)
        
                                                                        val tupleval = AST.ValBind(AST.TuplePat(!pattern),AST.PTupleExp(!expressions))
                                                                in
                                                                        arguments := [patvar]@(!arguments);
                                                                        bindings := [tupleval,valbindtemp,valbind];
                                                                        ()
                                                                end

                                                       fun newe2 () = (case ASTchangesize(e2,dconlist) 
                                                                        of AST.TupleExp(exps) => let
                                                                                                val _ = createbinding(exps)
                                                                                                val mytuple = List.map (fn e => (vexp e)) (!arguments)
                                                                                        in

                                                                                                U.mkLetExp(!bindings,AST.ApplyExp(ASTchangesize(constexp,dconlist),AST.TupleExp(mytuple), ty))
                                                                                        end
                                                                        | AST.PTupleExp(exps) => let
                                                                                                val _ = createbinding(exps)
                                                                                                val mytuple = List.map (fn e => (vexp e)) (!arguments)
                                                                                        in

                                                                                                U.mkLetExp(!bindings,AST.ApplyExp(ASTchangesize(constexp,dconlist),AST.TupleExp(mytuple), ty))
                                                                                        end
                                                                        | _ => raise Fail "Problem creating the new dcon apply exp"
                                                        )

                                                in
                                                        newe2()
                                                end

                                        (* check if we have a dcon in apply and if we have to change it,
                                           if so, we also have to change the second argument to account for size information *)
                                        fun changeapply (e) = (case e
                                                        of AST.ConstExp(const as AST.DConst(dcon, tylist)) => let
                                                                val tyconname = DataCon.ownerOf dcon
                                                                val dconid = DataCon.idOf dcon
                                                        in
                                                                (* check if the tycon is one that we have to change *)
                                                                if dconinlist(dcon,dconlist)
                                                                then (case (DataCon.argTypeOf(dcon))
                                                                        of SOME dcontype => if (recursivedcon(dcontype,tyconname))
                                                                                        (* then change the entire statement for the NODE *)
                                                                                      then 
                                                                                        changestat(AST.ConstExp(AST.DConst(getdcon(tyconname,dconid),tylist)), tyconname)
                                                                                        (* this the simple case e.g. LEAF *)
                                                                                        else let
                                                                                                val tuple = U.mkPTupleExp([U.mkInt(1),e2])
                                                                                        in
                                                                                        AST.ApplyExp(ASTchangesize(ASTchangesize(AST.ConstExp(AST.DConst(getdcon(tyconname,dconid),tylist)),dconlist),dconlist), ASTchangesize(tuple,dconlist), ty)

                                                                                        end
                                                                        | NONE => AST.ApplyExp(ASTchangesize(AST.ConstExp(AST.DConst(getdcon(tyconname,dconid),tylist)),dconlist), ASTchangesize(e2,dconlist), ty)
                                                                )
                                                                else AST.ApplyExp(ASTchangesize(e,dconlist), ASTchangesize(e2,dconlist), ty)
                                                        end
                                                        | _ =>  AST.ApplyExp(ASTchangesize(e,dconlist), ASTchangesize(e2,dconlist), ty)
                                        )
                                in
                                        changeapply(e1)
                                end
            | AST.TupleExp[] => AST.TupleExp[]
            | AST.TupleExp (exps) => let
                        val exps' = List.map (fn e => ASTchangesize(e,dconlist)) exps
                in           
                        AST.TupleExp (exps')
                end
            | AST.VarExp(x, tys) => let 
                                        val _ = removeannotation(x)
                                    in
                                         if (testvar(AST.VarExp(x, tys))) 
                                                (* we have a match for a changed variable that comes out of a dcon case, 
                                           we have to change var to case ( _ , var) => var *)
                                         then let
                                                        val Ty.TupleTy(ty1::ty2::rest) = TypeOf.exp(AST.VarExp(x,tys))
                                                        val myvar = Var.new ("_anon_", ty2) (* compute the appropriate function type *)
                                                        val mypat = U.mkTuplePat([AST.WildPat(ty1),AST.VarPat(myvar)])
                                                        val mkpat = AST.PatMatch(mypat,vexp myvar)
                                                        val mycase = U.mkCaseExp(AST.VarExp(x,tys),[mkpat])
                                                        val _ = addvartoannon(dummyvar())
                                                   in
                                                        mycase
                                                   end
                                        else AST.VarExp(x, tys)
                                    end

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
                                        (* this function will change the expression e in case we added size information to it and
                                        we have to change case (DCON(x:type) => x) to case (DCON(int,type) => case(_:int,x:type) => x *)
                                        fun changee () = if (testvar(e)) 
                                                        (* we have a match for a changed variable that comes out of a dcon case, 
                                                           we have to change var to case ( _ , var) => var *)
                                                         then (case e 
                                                                of AST.VarExp(x,tys) => let
                                                                        val Ty.TupleTy(ty1::ty2::rest) = TypeOf.exp(AST.VarExp(x,tys))
                                                                        val myvar = Var.new ("_anon_", ty2) (* compute the appropriate function type *)
                                                                        val mypat = U.mkTuplePat([AST.WildPat(ty1),AST.VarPat(myvar)])
                                                                        val mkpat = AST.PatMatch(mypat,vexp myvar)
                                                                        val mycase = U.mkCaseExp(AST.VarExp(x,tys),[mkpat])
                                                                        val _ = addvartoannon(dummyvar())
                                                                   in
                                                                        mycase
                                                                   end
                                                                | _ => raise Fail "Error in replacing pattern in casematch"
                                                           )
                                                         else ASTchangesize(e,dconlist)
        
                                in      
                                        AST.PatMatch(pat_size(pat,dconlist),changee())::casematch_size(rest,dconlist)
                                end
                                (* CondMatch is not used yet *)
                                | AST.CondMatch (pat,cond,e2) => raise Fail "unexpected AST.CondMatch"
                                )
                        | casematch_size ([], _) = [] 

        and lambda_size ([],dconlist) = [] 
            | lambda_size ( A.FB(f, x, e)::l, dconlist) = let
                                                        val _ = removeannotation(f)
                                                    in
                                                        (* piggy bag all created functions onto the first lambda block we can find *)
                                                        if (List.null (!fctlist) ) 
                                                        then A.FB(f, x, ASTchangesize(e,dconlist))::lambda_size(l,dconlist)     
                                                        else if (!attachfcts = 10) then let
                                                                val fcts = !fctlist
                                                              in
                                                                fctlist := [];
                                                                fcts@A.FB(f, x, ASTchangesize(e,dconlist))::lambda_size(l,dconlist)
                                                              end  
                                                                else let 
                                                val _ = attachfcts := !attachfcts+1 
                                                in
                                                        A.FB(f, x, ASTchangesize(e,dconlist))::lambda_size(l,dconlist) 
                                                end
                                                    end

        and pat_size(p,dconlist) = (case p 
                        of AST.ConPat (dcon, tylist, pat) => let
                                          (* change the dcon to a new one with size information, it will be saved in the tycon so we just need to read it out of the owner of the old dcon *)
                                            fun newdcon () = let
                                                val tyconname = DataCon.ownerOf dcon
                                                val dconid = DataCon.idOf dcon
                                            in    
                                                getdcon(tyconname,dconid)
                                            end

                                            (* we need to check if we have to change the type to add size information *)
                                            fun changevartype(pattern) = case pattern 
                                                        of AST.VarPat (var as VarRep.V{ty,...}) => 
                                                                let
                                                                        val AST.TyScheme(t, tys) = V.typeOf var
                                                                        (* we have to save the var to change the next patternmatch that uses it *)
                                                                        val _ = addvartoannon(var)
                                                                        val _ = TextIO.print(String.concat["Adding the var ", V.toString(var), " to the list \n"])
                                                                in
                                                                        (* ty := AST.TyScheme(t, (newty(tys))); *)
                                                                        V.setType(var, ref (AST.TyScheme(t, (newty(tys)))));
                                                                        pattern
                                                                end
                                                        | _ => pat_size(pat,dconlist)

                                in
                                        if (dconinlist(dcon,dconlist)) then AST.ConPat (newdcon(), tylist, changevartype(pat) )
                                                                       else AST.ConPat (dcon, tylist, pat_size(pat,dconlist) )
                                end
                        | AST.TuplePat (patlist) => let
                                        val pat = List.map (fn e => pat_size(e,dconlist) ) patlist
                                in      
                                        AST.TuplePat(pat)
                                end
                        | AST.VarPat (var) => AST.VarPat (var)
                        | AST.WildPat (ty) => AST.WildPat (ty) 
                        | AST.ConstPat (const) => AST.ConstPat (const)
                )

        (* check if the dcon is in out list of dcons to add size information to them *)
        and dconinlist(dcon,dconlist) = case List.find (fn (e) => TyCon.same((DataCon.ownerOf dcon),e) ) dconlist 
                                                                of SOME _ => true
                                                                | NONE => false

        (* add the size type information to a type, used for types that belong to dcons we changed *)
        and newty(ty) = (case ty
                of Ty.TupleTy(tylist) => Ty.TupleTy([Basis.intTy]@tylist)
                | _ => Ty.TupleTy([Basis.intTy,ty])
        ) 

        (* change the dcon to the new one *)
        and getdcon (tyc as Types.Tyc{def=Types.DataTyc{nCons, cons}, ...},id) = let
                                val dcons = !cons
                        in
                                List.nth(dcons,id)
                        end

        (* check if the dataconstructor type is recursive or the default case e.g (NODE,tree) => true, (LEAF,tree) => false *)
        and recursivedcon (typeprint,tyconin) = (case typeprint
                        of Ty.ConTy (tylist, tycon) => if (TyCon.same (tycon,tyconin))
                                                       then true
                                                       else recursivetypelist(tylist,tyconin)
                        | Ty.TupleTy (tylist) => recursivetypelist(tylist,tyconin)
                        | _ => raise Fail "ERROR IN RECURSIVEDCON \n" 
        )
        (* check if the dcon is the base case (false) or a recursive case (true) *)
        and recursivetypelist(ty::rest,tycon) = if (recursivedcon(ty, tycon) ) 
                                            then true
                                            else recursivetypelist(rest,tycon)
        | recursivetypelist([], _) = false

        (* test if the saved variable matches the input, we need this to change the case exp to accomodate for the new size exp *)
        and testvar (exp) = (case exp 
                                of AST.VarExp(var, x) => 
                                        (if V.same(var,(!anonvar) )
                                                                then    let
                                                                                val _ = TextIO.print("Found match \n ")
                                                                                val _ = printCost(AST.VarExp(var, x))
                                                                        in
                                                                                true
                                                                        end
                                                                else   false
                                                        )
                                | _ => false
                        )

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
                (* val _ = costFctsAST(body) *)
                val body' = ASTaddchunking(body)
                val body'' = if (!changeprogram) then changesize(body') else body
                val _ = printCost(body'') 
                val _ = PrintAST.printExp(body'')
        in   
                body''
        end

end