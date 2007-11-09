(* cfg.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * The "control-flow graph" representation; essentially a 1st-order
 * CPS language.
 *)

structure CFG =
  struct

    datatype ty = datatype CFGTy.ty

    type tag = Word.word	(* data-constant tags *)
    type offset = IntInf.int	(* offsets into the runtime-system vproc structure *)

    datatype heap_check_kind = HCK_Local    (* local heap-limit check *)
                             | HCK_Global   (* global heap-limit check *)

  (* extended basic block *)
    datatype func = FUNC of {
	lab : label,		(* label of function *)
	entry : convention,	(* calling convention, includes parameters *)
	body : exp list,	(* body of function is straight-line sequence of bindings *)
	exit : transfer		(* control transfer out of function *)
      }

    and convention
      = StdFunc of {		(* a function that may be called from unknown sites; it uses *)
				(* the standard function-calling convention. *)
	    clos : var,		  (* closure parameter *)
	    args : var list,	  (* argument parameters *)
	    ret : var,		  (* return-continuation parameter *)
	    exh : var		  (* exception-handler-continuation parameter *)
	  }
      | StdCont of {		(* a continuation that may be thrown to from unknown sites; *)
				(* it uses the standard continuation-calling convention *)
	    clos : var,		  (* closure parameter *)
	    args : var list	  (* argument parameters *)
	  }
      | KnownFunc		(* a function/continuation for which we know all of its call sites *)
				(* and only known functions are called from those sites (Serrano's *)
				(* "T" property).  It uses a specialized calling convention. *)
	  of var list		  (* parameters *)
      | Block			(* a function/continuation for which we know all of its call sites *)
				(* and it is the only function called at those sites (Serrano's *)
				(* "X" property) *)
	  of var list		  (* parameters *)

    and exp
      = E_Var of var list * var list            (* parallel assignment *)
      | E_Const of var * Literal.literal
      | E_Cast of var * ty * var		(* typecast *)
      | E_Label of var * label
      | E_Select of (var * int * var)		(* select i'th field (zero-based) *)
      | E_Update of (int * var * var)		(* update i'th field (zero-based) *)
      | E_AddrOf of (var * int * var)		(* return address of i'th field (zero-based) *)
      | E_Alloc of var * var list
      | E_GAlloc of var * var list		(* allocate in the global heap *)
      | E_Promote of var * var			(* promote value to global heap *)
      | E_Prim of var * prim
      | E_CCall of (var list * var * var list)
    (* VProc operations *)
      | E_HostVProc of var			(* gets the hosting VProc *)
      | E_VPLoad of (var * offset * var)	(* load a value from the given byte offset *)
						(* in the vproc structure *)
      | E_VPStore of (offset * var * var)	(* store a value at the given byte offset *)
						(* in the vproc structure *)

    and transfer
      = StdApply of {f : var, clos : var, args : var list, ret : var, exh : var}
      | StdThrow of {k : var, clos : var, args : var list}
      | Apply of {f : var, args : var list}
      | Goto of jump
      | If of (var * jump * jump)
      | Switch of (var * (tag * jump) list * jump option)
      | HeapCheck of {hck : heap_check_kind, szb : word, nogc : jump}
      | AllocCCall of {f : var, args : var list, ret : jump}            (* jump to ret after calling f(args) *)

    and var_kind
      = VK_None
      | VK_Let of exp
      | VK_Param of func

    and label_kind
      = LK_None			(* for initialization purposes *)
      | LK_Extern of string	(* external label; e.g., a C function *)
      | LK_Local of {		(* local to module *)
	    func : func,	    (* the function that this label names *)
	    export : string option  (* optional export name. *)
	  }

    withtype var = (var_kind, ty) VarRep.var_rep
	 and label = (label_kind, ty) VarRep.var_rep
         and prim = var Prim.prim
         and jump = (label * var list)
	 and cfun = label CFunctions.c_fun

    datatype module = MODULE of {
	name : Atom.atom,
	externs : cfun list,
	code : func list	(* first function is initialization *)
      }

    fun labelKindToString (LK_None) = "None"
      | labelKindToString (LK_Extern s) = "Extern " ^ s
      | labelKindToString (LK_Local{export = NONE, ...}) = "Local"
      | labelKindToString (LK_Local{export = SOME s, ...}) = "Export " ^ s

    structure Label = VarFn (
      struct
	type kind = label_kind
	type ty = ty
	val defaultKind = LK_None
	val kindToString = labelKindToString
	val tyToString = CFGTy.toString
      end)

    fun varKindToString VK_None = "None"
      | varKindToString (VK_Let _) = "Let"
      | varKindToString (VK_Param _) = "Param"

    structure Var = VarFn (
      struct
	type kind = var_kind
	type ty = ty
	val defaultKind = VK_None
	val kindToString = varKindToString
	val tyToString = CFGTy.toString
      end)

  (* return the function that a label is bound to, or NONE if it is external *)
    fun funcOfLabel lab = (case Label.kindOf lab
	   of LK_Local{func, ...} => SOME func
	    | _ => NONE
	  (* end case *))

  (* project out the lhs variables of an expression *)
    fun lhsOfExp (E_Var(xs, _)) = xs
      | lhsOfExp (E_Const(x, _)) = [x]
      | lhsOfExp (E_Cast(x, _, _)) = [x]
      | lhsOfExp (E_Label(x, _)) = [x]
      | lhsOfExp (E_Select(x, _, _)) = [x]
      | lhsOfExp (E_Update(_, _, _)) = []
      | lhsOfExp (E_AddrOf(x, _, _)) = [x]
      | lhsOfExp (E_Alloc(x, _)) = [x]
      | lhsOfExp (E_GAlloc(x, _)) = [x]
      | lhsOfExp (E_Promote(x, _)) = [x]
      | lhsOfExp (E_Prim(x, _)) = [x]
      | lhsOfExp (E_CCall(res, _, _)) = res
      | lhsOfExp (E_HostVProc x) = [x]
      | lhsOfExp (E_VPLoad(x, _, _)) = [x]
      | lhsOfExp (E_VPStore _) = []

  (* project out the rhs variable of an expression *)
    fun rhsOfExp (E_Var(_, ys)) = ys
      | rhsOfExp (E_Const _) = []
      | rhsOfExp (E_Cast(_, _, y)) = [y]
      | rhsOfExp (E_Label _) = []
      | rhsOfExp (E_Select(_, _, y)) = [y]
      | rhsOfExp (E_Update(_, y, z)) = [y, z]
      | rhsOfExp (E_AddrOf(_, _, y)) = [y]
      | rhsOfExp (E_Alloc(_, args)) = args
      | rhsOfExp (E_GAlloc(_, args)) = args
      | rhsOfExp (E_Promote(_, y)) = [y]
      | rhsOfExp (E_Prim(_, p)) = PrimUtil.varsOf p
      | rhsOfExp (E_CCall(_, f, args)) = f::args
      | rhsOfExp (E_HostVProc _) = []
      | rhsOfExp (E_VPLoad(_, _, x)) = [x]
      | rhsOfExp (E_VPStore(_, x, y)) = [x, y]

  (* project the list of variables in a control transfer.  Note that the order must agree with
   * the parameter order of paramsOfConv below.
   *)
    fun varsOfXfer (StdApply{f, clos, args, ret, exh}) = f :: clos :: args @ [ret, exh]
      | varsOfXfer (StdThrow{k, clos, args}) = k :: clos :: args
      | varsOfXfer (Apply{f, args}) = f::args
      | varsOfXfer (Goto(_, args)) = args
      | varsOfXfer (If(x, (_, args1), (_, args2))) = x :: args1 @ args2
      | varsOfXfer (Switch(x, cases, dflt)) = let
	  fun f ((_, (_, args)), l) = args @ l
	  in
	    x :: (List.foldl f (case dflt of SOME(_, args) => args | _ => []) cases)
	  end
      | varsOfXfer (HeapCheck{nogc=(_, args), ...}) = args
      | varsOfXfer (AllocCCall{ret=(_, args), ...}) = args

  (* project the list of destination labels in a control transfer; note that this function
   * only looks at jumps.  A control-flow analysis may give better information.
   *)
    fun labelsOfXfer (StdApply _) = []
      | labelsOfXfer (StdThrow _) = []
      | labelsOfXfer (Apply _) = []
      | labelsOfXfer (Goto(lab, _)) = [lab]
      | labelsOfXfer (If(x, (lab1, _), (lab2, _))) = [lab1, lab2]
      | labelsOfXfer (Switch(x, cases, dflt)) = let
	  fun f ((_, (lab, _)), l) = lab :: l
	  in
	    List.foldl f (case dflt of SOME(lab, _) => [lab] | _ => []) cases
	  end
      | labelsOfXfer (HeapCheck{nogc=(lab, _), ...}) = [lab]
      | labelsOfXfer (AllocCCall{ret=(lab, _), ...}) = [lab]

  (* project out the parameters of a convention *)
    fun paramsOfConv (StdFunc{clos, args, ret, exh}) = clos :: args @ [ret, exh]
      | paramsOfConv (StdCont{clos, args}) = clos::args
      | paramsOfConv (KnownFunc params) = params
      | paramsOfConv (Block params) = params

  (* smart constructors that set the kind field of the lhs variables *)
    fun mkExp e = (
	  List.app (fn x => Var.setKind(x, VK_Let e)) (lhsOfExp e);
	  e)
    fun mkVar arg = mkExp(E_Var arg)
    fun mkConst arg = mkExp(E_Const arg)
    fun mkCast arg = mkExp(E_Cast arg)
    fun mkLabel arg = mkExp(E_Label arg)
    fun mkSelect arg = mkExp(E_Select arg)
    fun mkUpdate arg = mkExp(E_Update arg)
    fun mkAddrOf arg = mkExp(E_AddrOf arg)
    fun mkAlloc arg = mkExp(E_Alloc arg)
    fun mkGAlloc arg = mkExp(E_GAlloc arg)
    fun mkPromote arg = mkExp(E_Promote arg)
    fun mkWrap (x, y) = mkExp(E_Alloc(x, [y]))
    fun mkUnwrap (x, y) = mkExp(E_Select(x, 0, y))
    fun mkPrim arg = mkExp(E_Prim arg)
    fun mkCCall arg = mkExp(E_CCall arg)
    fun mkHostVProc arg = mkExp(E_HostVProc arg)
    fun mkVPLoad arg = mkExp(E_VPLoad arg)
    fun mkVPStore arg = mkExp(E_VPStore arg)

    fun mkFunc (l, conv, body, exit, export) = let
	    val func = FUNC{lab = l, entry = conv, body = body, exit = exit}
	    in
	      Label.setKind (l, LK_Local{func = func, export = export});
	      List.app (fn x => Var.setKind(x, VK_Param func)) (paramsOfConv conv);
	      func
	    end
    fun mkLocalFunc (l, conv, body, exit) = mkFunc (l, conv, body, exit, NONE)
    fun mkExportFunc (l, conv, body, exit, name) = mkFunc (l, conv, body, exit, SOME name)

    fun mkCFun arg = (
	  Label.setKind (#var arg, LK_Extern(#name arg));
	  CFunctions.CFun arg)

    fun mkModule (name, externs, code) = MODULE{
	    name = name,
	    externs = externs,
	    code = code
	  }

    fun substVar env v = (case Var.Map.find (env, v)
            of NONE => v
	     | SOME v' => v'
            (* end case *))

    fun substExp env e = let
        val substVar = substVar env
        in 
            (case e
	      of E_Var (lhs, rhs) => mkVar (lhs, List.map substVar rhs)
	       | E_Cast (lhs, ty, rhs) => mkCast (lhs, ty, substVar rhs)
	       | E_Select (lhs, i, rhs) => mkSelect (lhs, i, substVar rhs)
	       | E_Update (i, v1, v2) => mkUpdate (i, substVar v1, substVar v2)
	       | E_AddrOf (lhs, i, rhs) => mkAddrOf (lhs, i, substVar rhs)
	       | E_Alloc (lhs, rhs) => mkAlloc (lhs, List.map substVar rhs)
	       | E_GAlloc (lhs, rhs) => mkGAlloc (lhs, List.map substVar rhs)
	       | E_Promote (lhs, rhs) => mkPromote (lhs, substVar rhs)
	       | E_Prim (lhs, prim) => mkPrim (lhs, PrimUtil.map substVar prim)
	       | E_CCall (lhs, f, rhs) => mkCCall (lhs, substVar f, List.map substVar rhs)
	       | E_VPLoad (lhs, offset, rhs) => mkVPLoad (lhs, offset, substVar rhs)
	       | E_VPStore (offset, v1, v2) => mkVPStore (offset, substVar v1, substVar v2)
	       | e => e
            (* end case *))
         end

    fun substTransfer env transfer = let
        val sv = substVar env
	fun sj (l, args) = (l, List.map sv args)
        in 
           (case transfer
	     of StdApply{f, clos, args, ret, exh} => StdApply{f=sv f, clos=sv clos, args=List.map sv args, ret=sv ret, exh=sv exh}
	      | StdThrow{k, clos, args} => StdThrow{k=sv k, clos=sv clos, args=List.map sv args}
	      | Apply{f, args} => Apply{f=sv f, args=List.map sv args}
	      | Goto jmp => Goto (sj jmp)
	      | If(v, jmp1, jmp2) => If(sv v, sj jmp1, sj jmp2)
	      | Switch(x, cases, dflt) => Switch(sv x, List.map (fn (c, j) => (c, sj j)) cases, Option.map sj dflt)
	      | HeapCheck{hck, szb, nogc} => HeapCheck{hck=hck, szb=szb, nogc=sj nogc}
	      | AllocCCall{f, args, ret} => AllocCCall{f=sv f, args=List.map sv args, ret=sj ret}
	    (* end case *))
         end

  end
