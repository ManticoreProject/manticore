(* interp-cfg.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure InterpCFG : sig

    type code_map

    val runtime : unit -> code_map

    datatype value
      = UNIT
      | ENUM of word
      | RAW of raw_value
      | WRAP of raw_value
      | TUPLE of value list
      | LABEL of CFG.label

    and raw_value = INT of IntInf.int

  (* set to true to enable execution tracing *)
    val traceFlg : bool ref

  (* convert a value to a string; the conversion is limited by the depth parameter *)
    val fmt : int -> value -> string

    val load : code_map -> CFG.module -> value
    val applyClos : code_map -> (value * value) -> value
    val applyFunc : code_map -> (CFG.func * value) -> value

  end = struct

    structure P = Prim
    structure VMap = CFG.Var.Map

    datatype value
      = UNIT
      | ENUM of word
      | RAW of raw_value
      | WRAP of raw_value
      | TUPLE of value list
      | LABEL of CFG.label

    and raw_value = INT of IntInf.int

    type env = value VMap.map

    fun set (env, x, v) = VMap.insert(env, x, v)

  (* convert a value to a string; the conversion is limited by the depth parameter *)
    fun fmt depth v = let
	  fun rawToString (INT i) = IntInf.toString i
	  fun toS (_, UNIT, l) = "<>" :: l
	    | toS (_, ENUM w, l) = "#" :: Word.fmt StringCvt.DEC w :: l
	    | toS (_, RAW rv, l) = rawToString rv :: l
	    | toS (_, WRAP rv, l) = "[" :: rawToString rv :: "]" :: l
	    | toS (0, TUPLE _, l) = "<...>" :: l
	    | toS (k, TUPLE vs, l) = let
		fun f ([], l) = l
		  | f ([v], l) = toS(k-1, v, l)
		  | f (v::vs, l) = toS(k-1, v, "," :: f (vs, l))
		in
		  "<" :: f(vs, ">"::l)
		end
	    | toS (_, LABEL lab, l) = "$" :: CFG.Label.toString lab :: l
	  in
	    concat (toS (Int.max(depth, 0), v, []))
	  end

    fun toBool (ENUM 0w0) = false
      | toBool (ENUM 0w1) = true
      | toBool v = raise Fail(concat["toBool(", fmt 2 v, ")"])

    fun fromBool false = ENUM 0w0
      | fromBool true = ENUM 0w1

    datatype code
      = EXTERN_FN of value list  -> value list
      | CFG_FN of CFG.func

    type code_map = code CFG.Label.Tbl.hash_table

  (* apply a primop *)
    local
      fun toInt (env, x) = (case VMap.find(env, x)
	     of SOME(RAW(INT i)) => i
	      | _ => raise Fail("toInt " ^ CFG.Var.toString x)
	    (* end case *))
      fun toB (env, x) = (case VMap.find(env, x)
	     of SOME v => toBool v
	      | NONE => raise Fail("toB " ^ CFG.Var.toString x)
	    (* end case *))
(* FIXME: truncate any excess bits *)
      fun toI32 x = RAW(INT x)
      fun toI64 x = RAW(INT x)
    in
    fun evalPrim (env, p) = (case p
	   of P.BNot x => fromBool(not(toB(env, x)))
	    | P.BEq(x, y) => fromBool(toB(env, x) = toB(env, y))
	    | P.BNEq(x, y) => fromBool(toB(env, x) <> toB(env, y))
	    | P.I32Add(x, y) => toI32(IntInf.+(toInt(env, x), toInt(env, y)))
	    | P.I32Sub(x, y) => toI32(IntInf.-(toInt(env, x), toInt(env, y)))
	    | P.I32Mul(x, y) => toI32(IntInf.*(toInt(env, x), toInt(env, y)))
	    | P.I32Div(x, y) => toI32(IntInf.quot(toInt(env, x), toInt(env, y)))
	    | P.I32Mod(x, y) => toI32(IntInf.rem(toInt(env, x), toInt(env, y)))
	    | P.I32Neg x => toI32(~(toInt(env, x)))
	    | P.I32Eq(x, y) => fromBool(toInt(env, x) = toInt(env, y))
	    | P.I32NEq(x, y) => fromBool(toInt(env, x) <> toInt(env, y))
	    | P.I32Lt(x, y) => fromBool(IntInf.<(toInt(env, x), toInt(env, y)))
	    | P.I32Lte(x, y) => fromBool(IntInf.<=(toInt(env, x), toInt(env, y)))
	    | P.I32Gt(x, y) => fromBool(IntInf.>(toInt(env, x), toInt(env, y)))
	    | P.I32Gte(x, y) => fromBool(IntInf.>=(toInt(env, x), toInt(env, y)))
	    | P.I64Add(x, y) => toI64(IntInf.+(toInt(env, x), toInt(env, y)))
	    | P.I64Sub(x, y) => toI64(IntInf.-(toInt(env, x), toInt(env, y)))
	    | P.I64Mul(x, y) => toI64(IntInf.*(toInt(env, x), toInt(env, y)))
	    | P.I64Div(x, y) => toI64(IntInf.quot(toInt(env, x), toInt(env, y)))
	    | P.I64Mod(x, y) => toI64(IntInf.rem(toInt(env, x), toInt(env, y)))
	    | P.I64Neg x => toI64(~(toInt(env, x)))
	    | P.I64Eq(x, y) => fromBool(toInt(env, x) = toInt(env, y))
	    | P.I64NEq(x, y) => fromBool(toInt(env, x) <> toInt(env, y))
	    | P.I64Lt(x, y) => fromBool(IntInf.<(toInt(env, x), toInt(env, y)))
	    | P.I64Lte(x, y) => fromBool(IntInf.<=(toInt(env, x), toInt(env, y)))
	    | P.I64Gt(x, y) => fromBool(IntInf.>(toInt(env, x), toInt(env, y)))
	    | P.I64Gte(x, y) => fromBool(IntInf.>=(toInt(env, x), toInt(env, y)))
	    | _ => raise Fail "unsupported primop"
(*
	    | P.F32Add of 'var * 'var
	    | P.F32Sub of 'var * 'var
	    | P.F32Mul of 'var * 'var
	    | P.F32Div of 'var * 'var
	    | P.F32Neg of 'var
	    | P.F32Eq of 'var * 'var
	    | P.F32NEq of 'var * 'var
	    | P.F32Lt of 'var * 'var
	    | P.F32Lte of 'var * 'var
	    | P.F32Gt of 'var * 'var
	    | P.F32Gte of 'var * 'var
	    | P.F64Add of 'var * 'var
	    | P.F64Sub of 'var * 'var
	    | P.F64Mul of 'var * 'var
	    | P.F64Div of 'var * 'var
	    | P.F64Neg of 'var
	    | P.F64Eq of 'var * 'var
	    | P.F64NEq of 'var * 'var
	    | P.F64Lt of 'var * 'var
	    | P.F64Lte of 'var * 'var
	    | P.F64Gt of 'var * 'var
	    | P.F64Gte of 'var * 'var
*)
	  (* end case *))
    end (* local *)

  (* raised in response to the invoking of the default exception handler *)
    exception Uncaught of value

  (* raised in response to the invoking of the top-level return continuation *)
    exception Return of value

    local
      fun new name = CFG.Label.newWithKind(name, CFG.LK_Extern name, CFGTy.T_Any)
    in
    val uncaughtLab = new "uncaught"
    val returnLab = new "return"
    end

  (* allocate and initialize a code map that has definitions for the
   * runtime hooks.
   *)
    fun runtime () = let
	  val cMap = CFG.Label.Tbl.mkTable(128, Fail "code map")
	  fun ins (label, f) = CFG.Label.Tbl.insert cMap (label, EXTERN_FN f)
	  in
	    ins (uncaughtLab, fn [v] => raise Uncaught v);
	    ins (returnLab, fn [v] => raise Return v);
	    cMap
	  end

  (* create a return-continuation closure that invokes the return
   * runtime function.
   *)
    fun returnCont cMap = let
	  fun newV name = CFG.Var.new(name, CFGTy.T_Any)
	  val ty = CFGTy.T_StdCont{clos = CFGTy.T_Any, arg = CFGTy.T_Any}
	  val lab = CFG.Label.new("returnCont", ty)
	  val closParam = newV "clos"
	  val argParam = newV "arg"
	  val f = newV "f"
	  val self = newV "self"
	  val func = CFG.FUNC{
		  lab = lab,
		  entry = CFG.StdCont{clos = closParam, arg = argParam},
		  body = [
		      CFG.mkLabel(f, returnLab),
		      CFG.mkCCall([], f, [argParam]),
		      CFG.mkLabel(self, lab)
		    ],
		  exit = CFG.StdThrow{k = self, clos = closParam, arg = argParam}
		}
	  in
	    CFG.Label.Tbl.insert cMap (lab, CFG_FN func);
	    TUPLE[LABEL lab]
	  end

  (* create an exception-handler-continuation closure that invokes the uncaught exception
   * runtime function.
   *)
    fun exnHandler cMap = let
	  fun newV name = CFG.Var.new(name, CFGTy.T_Any)
	  val ty = CFGTy.T_StdCont{clos = CFGTy.T_Any, arg = CFGTy.T_Any}
	  val lab = CFG.Label.new("exnHandler", ty)
	  val closParam = newV "clos"
	  val argParam = newV "arg"
	  val f = newV "f"
	  val self = newV "self"
	  val func = CFG.FUNC{
		  lab = lab,
		  entry = CFG.StdCont{clos = closParam, arg = argParam},
		  body = [
		      CFG.mkLabel(f, uncaughtLab),
		      CFG.mkCCall([], f, [argParam]),
		      CFG.mkLabel(self, lab)
		    ],
		  exit = CFG.StdThrow{k = self, clos = closParam, arg = argParam}
		}
	  in
	    CFG.Label.Tbl.insert cMap (lab, CFG_FN func);
	    TUPLE[LABEL lab]
	  end

  (* tracing *)
    val traceFlg = ref false

    local
      val v2s = fmt 1
      fun prl l = print(concat l)
    in
    fun trace curBlock = if !traceFlg
	  then let
	    val curBlock = CFG.Label.toString curBlock ^ ": "
	    val lineLabel = StringCvt.padRight #" " 20 curBlock
	    fun arg2str env x = (case VMap.find(env, x)
		   of SOME v => v2s v
		    | NONE => 
(print "env = {"; print (String.concatWith "," (List.map CFG.Var.toString (VMap.listKeys env))); print "}\n";
raise Fail(concat[
			  "Error in ", curBlock, "unbound variable ", CFG.Var.toString x
			])
)
		  (* end case *))
	    fun args2str env [] = "()"
	      | args2str env [x] = concat["(", arg2str env x, ")"]
	      | args2str env (first::rest) =
		  concat("(" :: arg2str env first
		    :: (List.foldr (fn (x, l) => "," :: arg2str env x :: l) [")"] rest))
	    fun lhsV2str x = concat[
		    lineLabel, "let ", CFG.Var.toString x, " = "
		  ]
	    fun lhs2str lhs = let
		  val lhs = (case lhs
			 of [] => ["do "]
			  | [x] => ["let ", CFG.Var.toString x, " = "]
			  | (x::rest) => "let " :: CFG.Var.toString x
			      :: (List.foldr (fn (x, l) => "," :: CFG.Var.toString x :: l) [") = "] rest)
			(* end case *))
		  in
		    concat(lineLabel :: lhs)
		  end
	    fun traceExp (env, exp) = (case exp
		   of CFG.E_Var(lhs, rhs) =>
			prl [lhs2str lhs, args2str env rhs, "\n"]
		    | CFG.E_Label(x, lab) =>
			prl [lhsV2str x, "$", CFG.Label.toString lab, "\n"]
		    | CFG.E_Literal(x, Literal.Bool b) => 
			prl [lhsV2str x, Bool.toString b, "\n"]
		    | CFG.E_Literal(x, Literal.Int n) => 
			prl [lhsV2str x, IntInf.toString n, "\n"]
		    | CFG.E_Literal(x, Literal.Float f) => raise Fail "float"
		    | CFG.E_Select(x, i, y) => 
			prl [lhsV2str x, "#", Int.toString i, "(", arg2str env y, ")", "\n"]
		    | CFG.E_Alloc(x, []) => 
			prl [lhsV2str x, "<>\n"]
		    | CFG.E_Alloc(x, ys) => 
			prl [lhsV2str x, "alloc", args2str env ys, "\n"]
		    | CFG.E_Wrap(x, y) =>
			prl [lhsV2str x, "wrap(", arg2str env y, ")\n"]
		    | CFG.E_Unwrap(x, y) => 
			prl [lhsV2str x, "unwrap(", arg2str env y, ")\n"]
		    | CFG.E_Prim(x, p) =>
			prl [lhsV2str x, PrimUtil.fmt (arg2str env) p, "\n"]
		    | CFG.E_CCall(res, f, args) =>
			prl [lhs2str res, "ccall ", arg2str env f, " ", args2str env args, "\n"]
		  (* end case *))
	    fun traceExit (env, xfer, CFG.FUNC{lab, ...}) = let
		  val lab = CFG.Label.toString lab
		  in
		    case xfer
		     of CFG.StdApply _ => prl [lineLabel, "StdApply ===> ", lab, "\n"]
		      | CFG.StdThrow _ => prl [lineLabel, "StdThrow ===> ", lab, "\n"]
		      | CFG.Apply _ => prl [lineLabel, "Apply ===> ", lab, "\n"]
		      | CFG.Goto _ => prl [lineLabel, "Goto ===> ", lab, "\n"]
		      | CFG.If(x, _, _) => prl [lineLabel, "If(", arg2str env x, ") ===> ", lab, "\n"]
		      | CFG.Switch(x, _, _) =>
			  prl [lineLabel, "Switch(", arg2str env x, ") ===> ", lab, "\n"]
		      | CFG.HeapCheck _ => prl [lineLabel, "HeapCheck ===> ", lab, "\n"]
		    (* end case *)
		  end
	    in
	      {traceExp = traceExp, traceExit = traceExit}
	    end
	  else {traceExp = fn _ => (), traceExit = fn _ => ()}
    end (* local *)

    fun applyClos cMap (closure as TUPLE[_, LABEL lab], arg : value) = let
	  val CFG_FN(func as CFG.FUNC{entry, body, exit, ...}) = CFG.Label.Tbl.lookup cMap lab
	  val findFunc = CFG.Label.Tbl.find cMap
	  fun evalFunc (CFG.FUNC{lab, body, exit, ...}, argEnv) = let
		val {traceExp=trace, traceExit} = trace lab
		fun error msg = raise Fail(concat(
		      "Error in " :: CFG.Label.toString lab :: ": " :: msg))
		fun valueOf (env, x) = (case VMap.find(env, x)
		       of SOME v => v
			| NONE => error["unbound variable ", CFG.Var.toString x]
		      (* end case *))
		fun valueOf' env x = valueOf(env, x)
		fun evalExp (exp, env : env) = (
		      trace (env, exp);
		      case exp
		       of CFG.E_Var(lhs, rhs) =>
			    ListPair.foldl
			      (fn (x, y, env) => set(env, x, valueOf(env, y)))
				env (lhs, rhs)
			| CFG.E_Label(x, lab) => set(env, x, LABEL lab)
			| CFG.E_Literal(x, Literal.Bool b) => set(env, x, fromBool b)
			| CFG.E_Literal(x, Literal.Int n) => set(env, x, RAW(INT n))
			| CFG.E_Literal(x, Literal.Float f) => raise Fail "float"
			| CFG.E_Select(x, i, y) => (case valueOf(env, y)
			     of TUPLE vs => set(env, x, List.nth(vs, i))
			      | _ => error["expected tuple"]
			    (* end case *))
			| CFG.E_Alloc(x, []) => set(env, x, UNIT)
			| CFG.E_Alloc(x, ys) => set(env, x, TUPLE(List.map (valueOf' env) ys))
			| CFG.E_Wrap(x, y) => (case valueOf(env, y)
			     of RAW y' => set(env, x, WRAP y')
			      | _ => error["expected wrapped value"]
			    (* end case *))
			| CFG.E_Unwrap(x, y) => (case valueOf(env, y)
			     of WRAP y' => set(env, x, RAW y')
			      | _ => error["expected raw value"]
			    (* end case *))
			| CFG.E_Prim(x, p) => set(env, x, evalPrim(env, p))
			| CFG.E_CCall(lhs, f, args) => (case valueOf(env, f)
			     of LABEL lab => (case findFunc lab
				   of SOME(EXTERN_FN f) => let
					val res = f(List.map (fn y => valueOf(env, y)) args)
					in
					  ListPair.foldl
					    (fn (x, y, env) => set(env, x, y))
					      env (lhs, res)
					end
				    | _ => raise Fail "C fucntion not extern"
				  (* end case *))
			      | _ => raise Fail "C function not label"
			    (* end case *))
		      (* end case *))
		fun evalExit (xfer, env0) = let
		      fun goto (f, env) = (traceExit(env0, xfer, f); evalFunc(f, env))
		      fun toFunc f = (case valueOf(env0, f)
			     of LABEL lab => (case findFunc lab
				   of SOME(CFG_FN f) => f
				    | NONE => raise Fail "undefined label"
				  (* end case *))
			      | _ => raise Fail "not label"
			    (* end case *))
		      fun initEnv binds = List.foldl
			    (fn ((param, arg), newEnv) =>
			      VMap.insert (newEnv, param, valueOf(env0, arg))
			    ) VMap.empty binds
		      fun evalJump (lab, args) = (case findFunc lab
			     of SOME(CFG_FN(f as CFG.FUNC{entry=CFG.Block params, ...})) => let
				  val env = initEnv (ListPair.zip (params, args))
				    in
				      goto (f, env)
				    end
			      | _ => raise Fail "undefined label"
			    (* end case *))
		      in
			case xfer
			 of CFG.StdApply{f, clos, arg, ret, exh} => (case toFunc f
			       of (f as CFG.FUNC{entry=CFG.StdFunc params, ...}) => let
				    val env = initEnv [
					    (#clos params, clos),
					    (#arg params, arg),
					    (#ret params, ret),
					    (#exh params, exh)
					  ]
				    in
				      goto (f, env)
				    end
				| _ => raise Fail "not standard function entry"
			      (* end case *))
			  | CFG.StdThrow{k, clos, arg} => (case toFunc k
			       of (k as CFG.FUNC{entry=CFG.StdCont params, ...}) => let
				    val env = initEnv [
					    (#clos params, clos),
					    (#arg params, arg)
					  ]
				    in
				      goto (k, env)
				    end
				| _ => raise Fail "not standard continuation entry"
			      (* end case *))
			  | CFG.Apply{f, args} => (case toFunc f
			       of (f as CFG.FUNC{entry=CFG.KnownFunc params, ...}) => let
				    val env = initEnv (ListPair.zip (params, args))
				    in
				      goto (f, env)
				    end
				| _ => raise Fail "not known function entry"
			      (* end case *))
			  | CFG.Goto jmp => evalJump jmp
			  | CFG.If(x, j1, j2) => if toBool(valueOf(env0, x))
			      then evalJump j1
			      else evalJump j2
			  | CFG.Switch(x, cases, dflt) => let
			      val arg = (case valueOf(env0, x)
				     of ENUM w => Word.toIntX w
				      | RAW(INT i) => Int.fromLarge i
				    (* end case *))
			      fun match [] = (case dflt
				     of SOME jmp => evalJump jmp
				      | NONE => raise Fail "switch failure"
				    (* end case *))
				| match ((i, jmp)::r) =
				    if (i = arg) then evalJump jmp else match r
			      in
				match cases
			      end
			  | CFG.HeapCheck{nogc, ...} => evalJump nogc
			(* end case *)
		      end (* end evalExit *)
		in
		  evalExit (exit, List.foldl evalExp argEnv body)
		end
	  val CFG.StdFunc params = entry
	  val env = List.foldl VMap.insert' VMap.empty [
		  (#clos params, closure),
		  (#arg params, arg),
		  (#ret params, returnCont cMap),
		  (#exh params, exnHandler cMap)
		]
	  in
	    evalFunc (func, env)
	  end
	    handle Return v => v

    fun applyFunc cMap (CFG.FUNC{lab, ...}, arg) = applyClos cMap (TUPLE[UNIT, LABEL lab], arg)

    fun load cMap (CFG.MODULE{code as init::_, ...}) = let
	  fun register (func as CFG.FUNC{lab, ...}) =
		CFG.Label.Tbl.insert cMap (lab, CFG_FN func)
	  in
	  (* register the functions in the module *)
	    List.app register code;
	  (* evaluate the module's initialization code *)
	    applyFunc cMap (init, UNIT)
	  end

  end
