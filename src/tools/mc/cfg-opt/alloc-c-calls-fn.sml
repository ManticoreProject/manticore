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

    val deleteList (vSet, vs) = VSet.difference (vSet, VSet.fromList vs)
    val addList = VSet.addList
    val fromList = VSet.fromList

    fun getCPrototype f = (case CFG.Var.typeOf f
	   of CFGTy.T_CFun proto => proto
	    | _ => raise Fail(concat["HeapTransferFn.getCPrototype: ", CFG.Var.toString f, " not a C function"])
	  (* end case *))

(* QUESTION: isn't this a live variables computation? *)
  (* compute the free variables of an expression *)
    fun freeVarsOfExp (e, fvs) =
	  deleteList (addList (fvs, CFGUtil.rhsOfExp e), CFG.lhsOfExp e)
    fun freeVarsOfXfer transfer =
	  deleteList (fromList (CFGUtil.varsOfXfer transfer), CFGUtil.lhsOfXfer transfer)

  (* if the function body contains a C call that allocates, split the body at that point *)
    fun splitFunBody ([], _, _) = NONE
      | splitFunBody (e :: es, preds, fvs) = (case e
	   of CFG.E_CCall (lhs, f, args) => let
	       val saveAllocPtr = CFunctions.protoHasAttr CFunctions.A_alloc (getCPrototype f)
	       in
		 if saveAllocPtr
		   then SOME (lhs, f, args, preds, es, deleteList (fvs, lhs))
		   else splitFunBody (es, e :: preds, freeVarsOfExp (e, fvs))
	       end
	    | e => splitFunBody (es, e :: preds, freeVarsOfExp (e, fvs))
	  (* end case *))
	
    fun revBody (CFG.FUNC{lab, entry, body, exit}) = CFG.FUNC{lab=lab, entry=entry, body=List.rev body, exit=exit}

    fun rewriteFunc func = let
	(* keep splitting the function body until we have no more calls to C functions that allocate *)
	  fun loop (func as CFG.FUNC{lab, entry, body, exit}, funcs) = (
		case splitFunBody (body, [], freeVarsOfXfer exit)
		 of NONE => revBody func :: funcs
		(* split the function func *)
		  | SOME (lhs, f, cArgs, preds, body, fvs') => let
		    (* free variables in f'' *)
		      val freeVars = VSet.listItems fvs'
		      val freshFreeVars = List.map CFG.Var.copy freeVars
		    (* a mapping to alpha-convert the split function *)
		      val env = List.foldl VMap.insert' VMap.empty (ListPair.zip (freeVars, freshFreeVars))
		      val lab' = CFG.Label.new(
				 "allocCCall",
				 CFGTy.T_Block(List.map CFG.Var.typeOf (lhs @ freshFreeVars)))
		    (* f' is the new function up to and including the C call and f'' is the function after the C call *)
		      val func' = CFGUtil.rewriteFunc (func, body, CFG.AllocCCall{
			      lhs = List.map CFG.Var.copy lhs,
			      f = f,
			      args = cArgs,
			      ret = (lab', freeVars)
			    })
		      val func'' = CFG.mkLocalFunc (lab',
			    CFG.Block(lhs @ freshFreeVars),
			    List.map (CFGUtil.substExp env) preds, CFGUtil.substTransfer env exit)
		      in
			  loop (func', func'' :: funcs)
		      end (* loop *)
	      (* end case *))
	  in
	    loop (revBody func, [])
	  end
			       
    fun transform (CFG.MODULE{name, externs, code}) = let
	  val code = List.concat (List.map rewriteFunc code)
	  val module = CFG.mkModule (name, externs, code)
	  in
	    Census.census module;
	    module
	  end (* transform *)

  end (* AllocCCallsFn *)
