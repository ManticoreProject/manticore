(* alloc-c-calls-fn.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Calls to C functions that allocate can trigger garbage collections.  In this pass, we
 * transform such calls in CFG from the generic RHS form to the special AllocCCall control
 * transfer.
 *)

functor AllocCCallsFn (Target : TARGET_SPEC) : sig

   val transform : CFG.module -> CFG.module

  end = struct

    structure VSet = CFG.Var.Set
    structure VMap = CFG.Var.Map

    fun deleteList (vSet, vs) = VSet.difference (vSet, VSet.fromList vs)
    val addList = VSet.addList
    val fromList = VSet.fromList

    fun getCPrototype f = (case CFG.Var.typeOf f
	   of CFGTy.T_CFun proto => proto
	    | _ => raise Fail(concat["HeapTransferFn.getCPrototype: ", CFG.Var.toString f, " not a C function"])
	  (* end case *))

  (* compute the live variables of an expression *)
    fun liveVarsOfExp (e, fvs) =
	  deleteList (addList (fvs, CFGUtil.rhsOfExp e), CFG.lhsOfExp e)
    fun liveVarsOfXfer transfer =
	  deleteList (fromList (CFGUtil.varsOfXfer transfer), CFGUtil.lhsOfXfer transfer)

  (* if the function body contains a C call that allocates, split the body at that point *)
    fun splitFunBody ([], _, _) = NONE
      | splitFunBody (e :: es, preds, fvs) = (case e
	   of CFG.E_CCall (lhs, f, args) => 
	        if (CFunctions.protoHasAttr CFunctions.A_alloc (getCPrototype f))
		   then SOME (lhs, f, args, preds, es, deleteList (fvs, lhs))
		   else splitFunBody (es, e :: preds, liveVarsOfExp (e, fvs))
	    | e => splitFunBody (es, e :: preds, liveVarsOfExp (e, fvs))
	  (* end case *))
	
    fun revBody (CFG.FUNC{lab, entry, body, exit}) = 
	CFG.FUNC{lab=lab, entry=entry, body=List.rev body, exit=exit}

    fun rewriteFunc func = let
	(* keep splitting the function body until we have no more calls to C functions that allocate *)
	  fun split (func as CFG.FUNC{lab, entry, body, exit}, funcs) = (
		case splitFunBody (body, [], liveVarsOfXfer exit)
		 of NONE => revBody func :: funcs
		(* split the function func *)
		  | SOME (lhs, f, cArgs, preds, body, fvs') => let
		    (* live variables in f'' *)
		      val liveVars = VSet.listItems fvs'
		      val freshLiveVars = List.map CFG.Var.copy liveVars
		    (* a mapping to alpha-convert the split function *)
		      val env = List.foldl VMap.insert' VMap.empty (ListPair.zip (liveVars, freshLiveVars))
		      val lab' = CFG.Label.new(
				 "allocCCall",
				 CFGTy.T_Block{args = List.map CFG.Var.typeOf (lhs @ freshLiveVars)})
		    (* f' is the new function up to and including the C call and f'' is the function after the C call *)
		      val func' = CFGUtil.rewriteFunc (func, body, CFG.AllocCCall{
			      lhs = List.map CFG.Var.copy lhs,
			      f = f,
			      args = cArgs,
			      ret = (lab', liveVars)
			    })
		      val func'' = CFG.mkLocalFunc (lab',
			    CFG.Block{args = lhs @ freshLiveVars},
			    List.map (CFGUtil.substExp env) preds, CFGUtil.substTransfer env exit)
		      in
			  split (func', func'' :: funcs)
		      end (* split *)
	      (* end case *))
	  in
	    split (revBody func, [])
	  end
			       
    fun transform (CFG.MODULE{name, externs, code}) = let
	  val code = List.concat (List.map rewriteFunc code)
	  val module = CFG.mkModule (name, externs, code)
	  in
	    Census.census module;
	    module
	  end (* transform *)

  end (* AllocCCallsFn *)
