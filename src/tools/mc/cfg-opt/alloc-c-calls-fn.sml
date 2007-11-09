(* alloc-c-calls-fn.sml
 *)

functor AllocCCallsFn (Target : TARGET_SPEC) : sig

   val transform : CFG.module -> CFG.module

  end = struct

    structure VSet = CFG.Var.Set

    fun deleteList (vSet, vs) = List.foldl (fn (v, vSet) => VSet.delete (vSet, v)) vSet vs
    val addList = VSet.addList

    fun getCPrototype f = (case CFG.Var.typeOf f
      of  CFGTy.T_CFun proto => proto
	| _ => raise Fail(concat["HeapTransferFn.getCPrototype: ", CFG.Var.toString f, " not a C function"])
      (* end case *))

    fun transform (CFG.MODULE{name, externs, code}) = let
       (* compute the free variables of an expression *)
	fun freeVars (e, fvs) = (case e
            of CFG.E_Var (lhs, rhs) => deleteList (addList (fvs, rhs), lhs)
	    (* end case *))

(* walk backwards through the list *)
       (* if the function body contains a C call that allocates, split the body at that point *)
	fun splitFunBody ([], prevs) = NONE
	  | splitFunBody (e :: es, prevs) = (case e
            of CFG.E_CCall (lhs, f, rhs) => let
		val saveAllocPtr = CFunctions.protoHasAttr CFunctions.A_alloc (getCPrototype f)
	        in
		   if saveAllocPtr
		      then SOME (e, rev prevs, es)
		      else splitFunBody (es, e::prevs)
	        end
	     | e => splitFunBody (es, e::prevs)
	    (* end case *))

	fun rewriteFunc  (CFG.FUNC{lab, body, exit, ...}) = raise Fail ""

	val module = CFG.mkModule (name, externs, code)
	in
	   Census.census module;
	   module
        end (* transform *)

  end (* AllocCCallsFn *)
