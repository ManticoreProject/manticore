(* cfg.sml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
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

    datatype heap_check_kind
      = HCK_Local		(* local heap-limit check *)
      | HCK_Global		(* global heap-limit check *)

  (* functions and continuations *)
    datatype func = FUNC of {
	lab : label,		(* label of function *)
	entry : convention,	(* calling convention, includes parameters *)
	start : block,		(* special start block *)
	body : block list	(* body of function is straight-line sequence of bindings *)
      }

    and block = BLK of {
        lab : label,            (* label of block *)
	args : var list,	(* argument list *)
	body : exp list,	(* body of function is straight-line sequence of bindings *)
	exit : transfer		(* control transfer out of function *)
      }

    and convention
      = StdFunc of {		(* a function that may be called from unknown sites; it uses *)
				(* the standard function-calling convention. *)
	    clos : var,		  (* closure parameter *)
	    ret : var,		  (* return-continuation parameter *)
	    exh : var		  (* exception-handler-continuation parameter *)
	  }
      | StdCont of {		(* a continuation that may be thrown to from unknown sites; *)
				(* it uses the standard continuation-calling convention *)
	    clos : var		  (* closure parameter *)
	  }
      | KnownFunc of {		(* a function/continuation for which we know all of its call sites *)
				(* and only known functions are called from those sites (Serrano's *)
				(* "T" property).  It uses a specialized calling convention. *)
	    clos : var		  (* closure parameter *)
          }

    and exp
      = E_Var of var list * var list            (* parallel assignment *)
      | E_Const of var * Literal.literal * ty
      | E_Cast of var * ty * var		(* typecast *)
      | E_Label of var * label
      | E_Select of (var * int * var)		(* select i'th field (zero-based) *)
      | E_Update of (int * var * var)		(* update i'th field (zero-based) *)
      | E_AddrOf of (var * int * var)		(* return address of i'th field (zero-based) *)
      | E_Alloc of var * ty * var list
      | E_GAlloc of var * ty * var list		(* allocate in the global heap *)
      | E_Promote of var * var			(* promote value to global heap *)
      | E_Prim0 of prim				(* primop w/o any results *)
      | E_Prim of var * prim
      | E_CCall of (var list * var * var list)
    (* VProc operations *)
      | E_HostVProc of var			(* gets the hosting VProc *)
      | E_VPLoad of (var * offset * var)	(* load a value from the given byte offset *)
						(* in the vproc structure *)
      | E_VPStore of (offset * var * var)	(* store a value at the given byte offset *)
						(* in the vproc structure *)
      | E_VPAddr of (var * offset * var)	(* address of given byte offset in the vproc *)
						(* structure *)

    and transfer
      = StdApply of {f : var, clos : var, args : var list, ret : var, exh : var}
      | StdThrow of {k : var, clos : var, args : var list}
      | Apply of {f : var, clos : var, args : var list}
      | Goto of jump
      | If of (cond * jump * jump)
      | Switch of (var * (tag * jump) list * jump option)
      | HeapCheck of {hck : heap_check_kind, szb : word, nogc : jump}
      | AllocCCall of { (* a CCall that does allocation *)
	    lhs : var list,
	    f : var,
	    args : var list,
	    ret : jump            (* jump to ret after calling f(args) *)
	  }

    and var_kind
      = VK_None			(* for initialization purposes *)
      | VK_Let of exp		(* let-bound variable *)
      | VK_Param of label	(* function/block parameter *)

    and label_kind
      = LK_None			(* for initialization purposes *)
      | LK_Extern of string	(* external label; e.g., a C function *)
      | LK_Func of {		(* local to module *)
	    func : func,	    (* the function that this label names *)
	    export : string option  (* optional export name. *)
	  }
      | LK_Block of block	(* labels a block in a function *)

    withtype var = (var_kind, ty) VarRep.var_rep
	 and label = (label_kind, ty) VarRep.var_rep
         and cond = var Prim.cond
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
      | labelKindToString (LK_Func{export = NONE, ...}) = "Func"
      | labelKindToString (LK_Func{export = SOME s, ...}) = "ExportFunc " ^ s
      | labelKindToString (LK_Block _) = "Block"

    structure Label = VarFn (
      struct
	type kind = label_kind
	type ty = ty
	val defaultKind = LK_None
	val kindToString = labelKindToString
	val tyToString = CFGTyUtil.toString
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
	val tyToString = CFGTyUtil.toString
      end)

  (* project out the lhs variables of an expression *)
    fun lhsOfExp (E_Var(xs, _)) = xs
      | lhsOfExp (E_Const(x, _, _)) = [x]
      | lhsOfExp (E_Cast(x, _, _)) = [x]
      | lhsOfExp (E_Label(x, _)) = [x]
      | lhsOfExp (E_Select(x, _, _)) = [x]
      | lhsOfExp (E_Update(_, _, _)) = []
      | lhsOfExp (E_AddrOf(x, _, _)) = [x]
      | lhsOfExp (E_Alloc(x, _, _)) = [x]
      | lhsOfExp (E_GAlloc(x, _, _)) = [x]
      | lhsOfExp (E_Promote(x, _)) = [x]
      | lhsOfExp (E_Prim0 _) = []
      | lhsOfExp (E_Prim(x, _)) = [x]
      | lhsOfExp (E_CCall(res, _, _)) = res
      | lhsOfExp (E_HostVProc x) = [x]
      | lhsOfExp (E_VPLoad(x, _, _)) = [x]
      | lhsOfExp (E_VPStore _) = []
      | lhsOfExp (E_VPAddr(x, _, _)) = [x]

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
    fun mkWrap (x, y) = mkExp(E_Alloc(x, CFGTy.T_Tuple(false, [Var.typeOf y]), [y]))
    fun mkUnwrap (x, y) = mkExp(E_Select(x, 0, y))
    fun mkPrim0 arg = mkExp(E_Prim0 arg)
    fun mkPrim arg = mkExp(E_Prim arg)
    fun mkCCall arg = mkExp(E_CCall arg)
    fun mkHostVProc arg = mkExp(E_HostVProc arg)
    fun mkVPLoad arg = mkExp(E_VPLoad arg)
    fun mkVPStore arg = mkExp(E_VPStore arg)
    fun mkVPAddr arg = mkExp(E_VPAddr arg)

    fun mkBlock (l, args, body, exit) = let
	  val blk = BLK{lab = l, args = args, body = body, exit = exit}
	  in
	    List.app (fn x => Var.setKind(x, VK_Param l)) args;
	    blk
	  end

  (* project out the parameters of a convention *)
    fun paramsOfConv (StdFunc{clos, ret, exh}, params) = clos :: params @ [ret, exh]
      | paramsOfConv (StdCont{clos}, params) = clos::params
      | paramsOfConv (KnownFunc{clos}, params) = clos::params

    fun mkBlock (lab, args, body, exit) = let
        val block = BLK{lab=lab, args=args, body=body, exit=exit}
    in
        Label.setKind (lab, LK_Block block);
        block
    end

    fun mkFunc (l, conv, start as BLK{args,...}, body, export) = let
	  val func = FUNC{lab = l, entry = conv, start = start, body = body}
	  val params = paramsOfConv (conv, args)
	  in
	    Label.setKind (l, LK_Func{func = func, export = export});
	    List.app (fn x => Var.setKind(x, VK_Param l)) params;
	    func
	  end
    fun mkLocalFunc (l, conv, start, body) = mkFunc (l, conv, start, body, NONE)
    fun mkExportFunc (l, conv, start, body, name) = mkFunc (l, conv, start, body, SOME name)

    fun mkCFun arg = (
	  Label.setKind (#var arg, LK_Extern(#name arg));
	  CFunctions.CFun arg)

    fun mkModule (name, externs, code) = MODULE{
	    name = name,
	    externs = externs,
	    code = code
	  }

  end
