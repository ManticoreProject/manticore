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

    val fmt : int -> value -> string

    val load : code_map -> CFG.module -> value
    val apply : code_map -> (CFG.func * value) -> value

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
		fun f ([], l) = ">" :: l
		  | f ([v], l) = toS(k-1, v, ">"::l)
		  | f (v::vs, l) = f (vs, toS(k-1, v, ">,"::l))
		in
		  "<" :: f(vs, l)
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
      = EXTERN_FN of value list  -> value
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

    fun exnHandler (handler : value -> value) = let
	  val ty = CFGTy.T_StdCont{clos = CFGTy.T_Any, arg = CFGTy.T_Any}
	  val lab = CFG.Label.new(Atom.atom "exnHandler", CFG.Local, ty)
	  val func = CFG.FUNC{
		  lab = lab,
		  entry = CFG.StdCont{clos = closParam, arg = argParam},
		  body = [
		      CFG.mkLabel(f, handlerLab),
		      CFG.mkCCall(res, x, [argParam])
		    ],
		  exit = ??
		}
	  in
	    ?
	  end

    fun apply cMap (CFG.FUNC{lab, entry, body, exit}, arg) = let
	  val findFunc = CFG.Label.Tbl.find cMap
	  fun evalFunc (CFG.FUNC{lab, body, exit, ...}, argEnv) = let
		fun error msg = raise Fail(concat(
		      "Error in " :: CFG.Label.toString lab :: ": " :: msg))
		fun valueOf (env, x) = (case VMap.find(env, x)
		       of SOME v => v
			| NONE => error["unbound variable ", CFG.Var.toString x]
		      (* end case *))
		fun valueOf' env x = valueOf(env, x)
		fun evalExp (exp, env : env) = (case exp
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
			| CFG.E_CCall(x, f, args) => raise Fail "ccall"
		      (* end case *))
		fun evalExit (xfer, env) = let
		      fun toFunc f = (case valueOf(env, f)
			     of LABEL lab => (case findFunc lab
				   of SOME(CFG_FN f) => f
				    | NONE => ??
				  (* end case *))
			      | _ => ??
			    (* end case *))
		      fun initEnv binds = List.foldl
			    (fn ((param, arg), newEnv) =>
			      VMap.insert (newEnv, param, valueOf(env, arg))
			    ) VMap.empty binds
		      fun evalJump (lab, args) = (case findFunc lab
			     of SOME(CFG_FN(f as CFG.FUNC{entry=CFG.Block params, ...})) => let
				  val env = initEnv (ListPair.zip (params, args))
				    in
				      evalFunc (f, env)
				    end
			      | _ => ??
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
				      evalFunc (f, env)
				    end
				| _ => ??
			      (* end case *))
			  | CFG.StdThrow{k, clos, arg} => (case toFunc k
			       of (k as CFG.FUNC{entry=CFG.StdCont params, ...}) => let
				    val env = initEnv [
					    (#clos params, clos),
					    (#arg params, arg)
					  ]
				    in
				      evalFunc (k, env)
				    end
				| _ => ??
			      (* end case *))
			  | CFG.Apply{f, args} => (case toFunc f
			       of (f as CFG.FUNC{entry=CFG.KnownFunc params, ...}) => let
				    val env = initEnv (ListPair.zip (params, args))
				    in
				      evalFunc (f, env)
				    end
				| _ => ??
			      (* end case *))
			  | CFG.Goto jmp => evalJump jmp
			  | CFG.If(x, j1, j2) => if toBool(valueOf(env, x))
			      then evalJump j1
			      else evalJump j2
			  | CFG.Switch(x, cases, dflt) => raise Fail "switch"
			  | CFG.HeapCheck{nogc, ...} => evalJump nogc
			(* end case *)
		      end
		in
		  evalExit (exit, List.foldl evalExp argEnv body)
		end
	  in
	    ?
	  end

    fun load cMap (CFG.MODULE{code as init::_, funcs}) = let
	  fun register (func as CFG.FUNC{lab, ...}) =
		CFG.Label.Tbl.insert cMap (lab, CFG_FN func)
	  in
	  (* register the functions in the module *)
	    List.app register code;
	  (* evaluate the module's initialization code *)
	    apply cMap (init, UNIT)
	  end

  end
