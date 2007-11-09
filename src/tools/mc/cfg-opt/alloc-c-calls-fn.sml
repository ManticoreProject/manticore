(* alloc-c-calls-fn.sml
 *)

functor AllocCCallsFn (Target : TARGET_SPEC) : sig

   val transform : CFG.module -> CFG.module

  end = struct

    structure VSet = CFG.Var.Set
    structure VMap = CFG.Var.Map

    fun deleteList (vSet, vs) = List.foldl (fn (v, vSet) => VSet.delete (vSet, v)) vSet vs
    val addList = VSet.addList

    fun getCPrototype f = (case CFG.Var.typeOf f
      of CFGTy.T_CFun proto => proto
       | _ => raise Fail(concat["HeapTransferFn.getCPrototype: ", CFG.Var.toString f, " not a C function"])
      (* end case *))

    fun transform (CFG.MODULE{name, externs, code}) = let
       (* compute the free variables of an expression *)
	fun freeVarsOfExp (e, fvs) = deleteList (addList (fvs, CFG.rhsOfExp e), CFG.lhsOfExp e)
	fun freeVarsOfXfer transfer = addList (VSet.empty, CFG.varsOfXfer transfer)

       (* if the function body contains a C call that allocates, split the body at that point *)
	fun splitFunBody ([], _, _) = NONE
	  | splitFunBody (e :: es, preds, fvs) = (case e
            of CFG.E_CCall (lhs, f, args) => let
		val saveAllocPtr = CFunctions.protoHasAttr CFunctions.A_alloc (getCPrototype f)
	        in
		   if saveAllocPtr
		      then SOME (lhs, f, args, preds, es, fvs)
		      else splitFunBody (es, e :: preds, freeVarsOfExp (e, fvs))
	        end
	     | e => splitFunBody (es, e :: preds, freeVarsOfExp (e, fvs))
	    (* end case *))
	
	fun revBody (CFG.FUNC{lab, entry, body, exit}) = CFG.FUNC{lab=lab, entry=entry, body=List.rev body, exit=exit}

	fun rewriteFunc func = let
           (* keep splitting the body of the function until we have no more calls to C functions that allocate *)
	    fun loop (func as CFG.FUNC{lab, entry, body, exit}, funcs) = (case splitFunBody (body, [], freeVarsOfXfer exit)
                of NONE => revBody func :: funcs
                (* split the function func *)
		 | SOME (lhs, f, cArgs, preds, es, fvs') => let
                  (* free variables in f'' *)
		   val freeVars = VSet.listItems fvs'
                  (* a mapping to alpha-convert freeVars *)
		   val env = List.foldl (fn (v, env) => VMap.insert (env, v, CFG.Var.copy v)) VMap.empty freeVars
                   val lab' = CFG.Label.new(
			      "allocCCall",
			      CFGTy.T_Block(List.map CFG.Var.typeOf freeVars))
		   val export = (case CFG.Label.kindOf lab
				  of CFG.LK_Local{export, ...} => export
				   | _ => raise Fail "bogus label kind"
				 (* end case *))                  
                  (* f' is the new function up to and including the C call and f'' is the function after the C call *)
		   val func' = CFG.mkFunc(lab, entry, es, CFG.AllocCCall {
		                   f = f,
				   args = cArgs,
				   (* FIXME: make a new var for lhs *)
				   ret = (lab', lhs @ freeVars)
                                 }, export)
		   val func'' = CFG.mkLocalFunc (lab', CFG.Block (lhs @ freeVars), List.map (CFG.substExp env) preds, CFG.substTransfer env exit)
		   in
		       loop (func', func'' :: funcs)
		   end (* loop *)
                (* end case *))
            in
	       loop (revBody func, [])
	    end

	val module = CFG.mkModule (name, externs, code)
	in
	   Census.census module;
	   module
        end (* transform *)

  end (* AllocCCallsFn *)
