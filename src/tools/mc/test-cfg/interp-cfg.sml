(* interp-cfg.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure InterpCFG =
  struct

    structure P = Prim

    datatype value
      = UNIT
      | ENUM of word
      | RAW of raw_value
      | WRAP of raw_value
      | TUPLE of value list
      | LABEL of CFG.label

    and raw_value = INT of IntInf.int

    fun toBool (env, x) = (case CFG.Var.Map.find(env, x)
	   of SOME(ENUM 0w0) => false
	    | SOME(ENUM 0w1) => true
	    | _ => raise Fail("toBool " ^ CFG.Var.toString x)
	  (* end case *))
    fun fromBool false = ENUM 0w0
      | fromBool true = ENUM 0w1

  (* convert a value to a string; the conversion is limited by the depth parameter *)
    fun fmt depth = let
	  fun toS (_, UNIT, l) = "<>" :: l
	    | toS (_, BOOL b, l) = Bool.toString b :: l
	    | toS (_, RAW rv, l) = rawToString r :: l
	    | toS (_, WRAP rv, l) = "[" :: rawToString r :: "]" :: l
	    | toS (0, TUPLE _, l) = "<...>" :: l
	    | toS (k, TUPLE vs, l) = let
		fun f ([], l) = ">" :: l
		  | f ([v], l) = toS(k-1, v, ">"::l)
		  | f (v::vs, l) = f (vs, toS(k-1, v, ">,"::l))
		in
		  "<" :: f(vs, l)
		end
	    | toS (_, LABEL lab, l) = "$" :: CFG.Label.toString l :: l
	  in
	    concat (toS (Int.max(depth, 0), []))
	  end

    type env = value CFG.Var.Map.map

    datatype code
      = EXTERN_FN of value list  -> value
      | CFG_FN of CFG.func

    type code_map = code CFG.Label.Tbl.hash_table

    fun set (env, x, v) = CFG.Var.Map.insert(env, x, v)

  (* apply a primop *)
    local
      fun toInt (env, x) = (case CFG.Var.Map.find(env, x)
	     of SOME(RAW(INT i)) => i
	      | _ => raise Fail("toInt " ^ CFG.Var.toString x)
	    (* end case *))
(* FIXME: truncate any excess bits *)
      fun toI32 x = RAW(INT x)
      fun toI64 x = RAW(INT x)
    in
    fun evalPrim (env, p) = (case p
	   of P.BNot x => fromBool(not(toBool(env, x)))
	    | P.BEq(x, y) => fromBool(toBool(env, x) = toBool(env, y))
	    | P.BNEq(x, y) => fromBool(toBool(env, x) <> toBool(env, y))
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
	  val ty = CFGTy.StdCont{clos = CFGTy.T_Any, arg = CFGTy.T_Any}
	  val lab = CFG.Label.new(Atom.atom "exnHandler", ty)
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
	  fun evalFunc (CFG.FUNC{lab, body, exit, ...}, argEnv) = let
		fun error msg = raise Fail(concat(
		      "Error in " :: CFG.Label.toString lab :: ": " :: msg))
		fun valueOf (env, x) = (case CFG.Var.Map.find(env, x)
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
			| CFG.E_Literal(x, Literal.Bool b) => set(env, x, BOOL b)
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
		      in
			case xfer
			 of StdApply{f, clos, arg, ret, exh}
			  | StdThrow{k, clos, arg}
			  | Apply{f, args}
			  | Goto jmp
			  | If(x, j1, j2) => if toBool(valueOf(env, x))
			      then evalJump(env, j1)
			      else evalJump(env, j2)
			  | Switch(x, cases, dflt) =>
			  | HeapCheck{nogc, ...} => evalJump(env, nogc)
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
