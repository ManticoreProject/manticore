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

  (* if the block body  contains a C call that allocates, split the body at that point *)
    fun splitBlockBody ([], _, _) = NONE
      | splitBlockBody (e :: es, preds, fvs) = (case e
	   of CFG.E_CCall (lhs, f, args) => 
	        if (CFunctions.protoHasAttr CFunctions.A_alloc (getCPrototype f))
		   then SOME (lhs, f, args, preds, es, deleteList (fvs, lhs))
		   else splitBlockBody (es, e :: preds, liveVarsOfExp (e, fvs))
	    | e => splitBlockBody (es, e :: preds, liveVarsOfExp (e, fvs))
	  (* end case *))
	
    fun revBody (CFG.BLK{lab, args, body, exit}) = 
	CFG.BLK{lab=lab, args=args, body=List.rev body, exit=exit}

    fun rewriteFunc (func as CFG.FUNC{lab, entry, start, body}) = let
	(* keep splitting the block until we have no more calls to C functions that allocate *)
	  fun split (block as CFG.BLK{lab, args, body, exit}, blocks) = (
		case splitBlockBody (body, [], liveVarsOfXfer exit)
		 of NONE => revBody block :: blocks
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
		    (* block' is the new function up to and including the C call
                     * and block'' is the function after the C call *)
                      val block' = CFG.BLK {lab=lab, args=args, body=body,
                                            exit=CFG.AllocCCall{
			      lhs = List.map CFG.Var.copy lhs,
			      f = f,
			      args = cArgs,
			      ret = (lab', liveVars)
			    }}
                      val block'' = CFG.BLK {lab=lab', args=lhs @ freshLiveVars, body=List.map (CFGUtil.substExp env) preds,
                                             exit=CFGUtil.substTransfer env exit}
		      in
			  split (block', block'' :: blocks)
		      end (* split *)
	      (* end case *))
          val start::rest = split (revBody start, [])
          val bodyBlocks = List.foldr (fn (b,rr) => (split (revBody b, []))@rr) rest body
	  val export = (case CFG.Label.kindOf lab
			 of CFG.LK_Func{export, ...} => export
			  | _ => raise Fail "bogus label kind"
		       (* end case *))
	  in
            CFG.mkFunc (lab, entry, start, rest, export)
	  end
			       
    fun transform (CFG.MODULE{name, externs, code}) = let
	  val code = List.map rewriteFunc code
	  val module = CFG.mkModule (name, externs, code)
	  in
	    Census.census module;
	    module
	  end (* transform *)

  end (* AllocCCallsFn *)
