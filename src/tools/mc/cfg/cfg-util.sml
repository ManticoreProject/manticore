(* cfg-util.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Various utility functions for manipulating CFG terms.
 *)

structure CFGUtil : sig

  (* return the block that a label is bound to, or NONE if it is external *)
    val blockOfLabel : CFG.label -> CFG.block option

  (* project out the lhs variables of an expression *)
    val lhsOfExp : CFG.exp -> CFG.var list

  (* project out the rhs variable of an expression *)
    val rhsOfExp : CFG.exp -> CFG.var list

  (* project the list of variables in a control transfer.  Note that the order must agree with
   * the parameter order of paramsOfConv below.
   *)
    val varsOfXfer : CFG.transfer -> CFG.var list

   (* project the lhs variables of a control transfer *)
    val lhsOfXfer : CFG.transfer -> CFG.var list

  (* project the list of destination labels in a control transfer; note that this function
   * only looks at jumps.  A control-flow analysis may give better information.
   *)
    val labelsOfXfer : CFG.transfer -> CFG.label list

  (* project out the parameters of a convention *)
    val paramsOfConv : (CFG.convention * CFG.var list) -> CFG.var list

   (* rewrite a function with a new body and exit *)
    val rewriteFunc : (CFG.func * CFG.block * CFG.block list * CFG.transfer) -> CFG.func

    type substitution = CFG.var CFG.Var.Map.map

   (* variable substitution over an expression *)
    val substExp : substitution -> CFG.exp -> CFG.exp

   (* variable substitution over an transfer *)
    val substTransfer : substitution -> CFG.transfer -> CFG.transfer

  end = struct
    structure M = CFG

    datatype func = datatype CFG.func
    datatype exp = datatype CFG.exp
    datatype convention = datatype CFG.convention
    datatype transfer = datatype CFG.transfer

  (* return the block that a label is bound to, or NONE if it is external *)
    fun blockOfLabel lab = (case CFG.Label.kindOf lab
	   of CFG.LK_Func{func as CFG.FUNC{start,...}, ...} => SOME start
            | CFG.LK_Block block => SOME block
	    | _ => NONE
	  (* end case *))

  (* project out the lhs variables of an expression *)
    val lhsOfExp = CFG.lhsOfExp

  (* project out the rhs variable of an expression *)
    fun rhsOfExp (E_Var(_, ys)) = ys
      | rhsOfExp (E_Const _) = []
      | rhsOfExp (E_Cast(_, _, y)) = [y]
      | rhsOfExp (E_Label _) = []
      | rhsOfExp (E_Select(_, _, y)) = [y]
      | rhsOfExp (E_Update(_, y, z)) = [y, z]
      | rhsOfExp (E_AddrOf(_, _, y)) = [y]
      | rhsOfExp (E_Alloc(_, _, args)) = args
      | rhsOfExp (E_AllocSpecial(_, _, args)) = args
      | rhsOfExp (E_GAlloc(_, _, args)) = args
      | rhsOfExp (E_Promote(_, y)) = [y]
      | rhsOfExp (E_Prim0 p) = PrimUtil.varsOf p
      | rhsOfExp (E_Prim(_, p)) = PrimUtil.varsOf p
      | rhsOfExp (E_CCall(_, f, args)) = f::args
      | rhsOfExp (E_HostVProc _) = []
      | rhsOfExp (E_VPLoad(_, _, x)) = [x]
      | rhsOfExp (E_VPStore(_, x, y)) = [x, y]
      | rhsOfExp (E_VPAddr(_, _, x)) = [x]

  (* project the list of variables in a control transfer.  Note that the order must agree with
   * the parameter order of paramsOfConv below.
   *)
    fun varsOfXfer (StdApply{f, clos, args, ret, exh}) = f :: clos :: args @ [ret, exh]
      | varsOfXfer (StdThrow{k, clos, args}) = k :: clos :: args
      | varsOfXfer (Apply{f, clos, args}) = f:: clos :: args
      | varsOfXfer (Goto(_, args)) = args
      | varsOfXfer (If(cond, (_, args1), (_, args2))) = CondUtil.varsOf cond @ args1 @ args2
      | varsOfXfer (Switch(x, cases, dflt)) = let
	  fun f ((_, (_, args)), l) = args @ l
	  in
	    x :: (List.foldl f (case dflt of SOME(_, args) => args | _ => []) cases)
	  end
      | varsOfXfer (HeapCheck{nogc=(_, args), ...}) = args
      | varsOfXfer (HeapCheckN{nogc=(_, args), ...}) = args
      | varsOfXfer (AllocCCall{lhs, args, ret=(_, rArgs), ...}) = lhs @ args @ rArgs

   (* project the lhs variables of a control transfer *)
    fun lhsOfXfer (AllocCCall{lhs, ...}) = lhs
      | lhsOfXfer _ = []

  (* project the list of destination labels in a control transfer; note that this function
   * only looks at jumps.  A control-flow analysis may give better information.
   *)
    fun labelsOfXfer (StdApply _) = []
      | labelsOfXfer (StdThrow _) = []
      | labelsOfXfer (Apply _) = []
      | labelsOfXfer (Goto(lab, _)) = [lab]
      | labelsOfXfer (If(_, (lab1, _), (lab2, _))) = [lab1, lab2]
      | labelsOfXfer (Switch(x, cases, dflt)) = let
	  fun f ((_, (lab, _)), l) = lab :: l
	  in
	    List.foldl f (case dflt of SOME(lab, _) => [lab] | _ => []) cases
	  end
      | labelsOfXfer (HeapCheck{nogc=(lab, _), ...}) = [lab]
      | labelsOfXfer (HeapCheckN{nogc=(lab, _), ...}) = [lab]
      | labelsOfXfer (AllocCCall{ret=(lab, _), ...}) = [lab]

  (* project out the parameters of a convention *)
    val paramsOfConv = CFG.paramsOfConv

   (* rewrite a function with a new body and exit *)
    fun rewriteFunc (FUNC{lab, entry, ...}, start as M.BLK{args,...}, body, exit) = let
	  val func = FUNC{lab = lab, entry = entry, start = start, body = body}
	  val lk = (case CFG.Label.kindOf lab
		  of CFG.LK_Func{export, ...} => CFG.LK_Func{func=func, export=export}
		   | lk => lk
		(* end case *))
	  in
	    CFG.Label.setKind (lab, lk);
	    List.app (fn x => CFG.Var.setKind(x, CFG.VK_Param lab)) (paramsOfConv (entry, args));
	    func
	  end

    type substitution = CFG.var CFG.Var.Map.map

   (* substitute a variable w.r.t. an environment *)
    fun substVar (env : substitution) v = (case CFG.Var.Map.find (env, v)
	  of NONE => v
	   | SOME v' => v'
	  (* end case *))

   (* variable substitution over an expression *)
    fun substExp (env : substitution) e = let
	  val substVar = substVar env
	  in 
            case e
	     of E_Var(lhs, rhs) => CFG.mkVar (lhs, List.map substVar rhs)
	      | E_Cast(lhs, ty, rhs) => CFG.mkCast (lhs, ty, substVar rhs)
	      | E_Select(lhs, i, rhs) => CFG.mkSelect (lhs, i, substVar rhs)
	      | E_Update(i, v1, v2) => CFG.mkUpdate (i, substVar v1, substVar v2)
	      | E_AddrOf(lhs, i, rhs) => CFG.mkAddrOf (lhs, i, substVar rhs)
	      | E_Alloc(lhs, ty, rhs) => CFG.mkAlloc (lhs, ty, List.map substVar rhs)
	      | E_AllocSpecial(lhs, ty, rhs) => CFG.mkAllocSpecial (lhs, ty, List.map substVar rhs)
	      | E_GAlloc(lhs, ty, rhs) => CFG.mkGAlloc (lhs, ty, List.map substVar rhs)
	      | E_Promote(lhs, rhs) => CFG.mkPromote (lhs, substVar rhs)
	      | E_Prim0 prim => CFG.mkPrim0 (PrimUtil.map substVar prim)
	      | E_Prim(lhs, prim) => CFG.mkPrim (lhs, PrimUtil.map substVar prim)
	      | E_CCall(lhs, f, rhs) => CFG.mkCCall (lhs, substVar f, List.map substVar rhs)
	      | E_VPLoad(lhs, offset, rhs) => CFG.mkVPLoad (lhs, offset, substVar rhs)
	      | E_VPStore(offset, v1, v2) => CFG.mkVPStore (offset, substVar v1, substVar v2)
	      | E_VPAddr(lhs, offset, rhs) => CFG.mkVPAddr (lhs, offset, substVar rhs)
	      | e => e
            (* end case *)
	  end

   (* variable substitution over an transfer *)
    fun substTransfer (env : substitution) transfer = let
	  val sv = substVar env
	  fun sj (l, args) = (l, List.map sv args)
	  in 
	    case transfer
	     of StdApply{f, clos, args, ret, exh} =>
		  StdApply{f=sv f, clos=sv clos, args=List.map sv args, ret=sv ret, exh=sv exh}
	      | StdThrow{k, clos, args} => StdThrow{k=sv k, clos=sv clos, args=List.map sv args}
	      | Apply{f, clos, args} => Apply{f=sv f, clos=sv clos, args=List.map sv args}
	      | Goto jmp => Goto(sj jmp)
	      | If(cond, jmp1, jmp2) => If(CondUtil.map sv cond, sj jmp1, sj jmp2)
	      | Switch(x, cases, dflt) =>
		  Switch(sv x, List.map (fn (c, j) => (c, sj j)) cases, Option.map sj dflt)
	      | HeapCheck{hck, szb, nogc} => HeapCheck{hck=hck, szb=szb, nogc=sj nogc}
	      | HeapCheckN{hck, n, szb, nogc} => HeapCheckN{hck=hck, n=sv n, szb=szb, nogc=sj nogc}
	      | AllocCCall{lhs, f, args, ret} =>
		  AllocCCall{lhs=lhs, f=sv f, args=List.map sv args, ret=sj ret}
	     (* end case *)
	  end

  end

