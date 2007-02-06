(* interp-cfg.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure InterpCFG =
  struct

    structure P = Prim

    datatype value
      = BOOL of bool
      | RAW of raw_value
      | LABEL of CFG.label
      | WRAP of raw_value
      | TUPLE of value list

    and raw_value = INT of IntInf.int

    type env = value CFG.Var.Map.map

    fun set (env, x, v) = CFG.Var.Map.insert(env, x, v)

  (* apply a primop *)
    local
      fun toBool (env, x) = (case CFG.Var.Map.find(env, x)
	     of SOME(BOOL b) => b
	      | _ => raise Fail("toBool " ^ CFG.Var.toString x)
	    (* end case *))
      fun toInt (env, x) = (case CFG.Var.Map.find(env, x)
	     of SOME(RAW(INT i)) => i
	      | _ => raise Fail("toInt " ^ CFG.Var.toString x)
	    (* end case *))
(* FIXME: truncate an excess bits *)
      fun toI32 x = RAW(INT x)
      fun toI64 x = RAW(INT x)
    in
    fun evalPrim (env, p) = (case p
	   of P.BNot x => BOOL(not(toBool(env, x)))
	    | P.BEq(x, y) => BOOL(toBool(env, x) = toBool(env, y))
	    | P.BNEq(x, y) => BOOL(toBool(env, x) <> toBool(env, y))
	    | P.I32Add(x, y) => toI32(IntInf.+(toInt(env, x), toInt(env, y)))
	    | P.I32Sub(x, y) => toI32(IntInf.-(toInt(env, x), toInt(env, y)))
	    | P.I32Mul(x, y) => toI32(IntInf.*(toInt(env, x), toInt(env, y)))
	    | P.I32Div(x, y) => toI32(IntInf.quot(toInt(env, x), toInt(env, y)))
	    | P.I32Mod(x, y) => toI32(IntInf.rem(toInt(env, x), toInt(env, y)))
	    | P.I32Neg x => toI32(~(toInt(env, x)))
	    | P.I32Eq(x, y) => BOOL(toInt(env, x) = toInt(env, y))
	    | P.I32NEq(x, y) => BOOL(toInt(env, x) <> toInt(env, y))
	    | P.I32Lt(x, y) => BOOL(IntInf.<(toInt(env, x), toInt(env, y)))
	    | P.I32Lte(x, y) => BOOL(IntInf.<=(toInt(env, x), toInt(env, y)))
	    | P.I32Gt(x, y) => BOOL(IntInf.>(toInt(env, x), toInt(env, y)))
	    | P.I32Gte(x, y) => BOOL(IntInf.>=(toInt(env, x), toInt(env, y)))
	    | P.I64Add(x, y) => toI64(IntInf.+(toInt(env, x), toInt(env, y)))
	    | P.I64Sub(x, y) => toI64(IntInf.-(toInt(env, x), toInt(env, y)))
	    | P.I64Mul(x, y) => toI64(IntInf.*(toInt(env, x), toInt(env, y)))
	    | P.I64Div(x, y) => toI64(IntInf.quot(toInt(env, x), toInt(env, y)))
	    | P.I64Mod(x, y) => toI64(IntInf.rem(toInt(env, x), toInt(env, y)))
	    | P.I64Neg x => toI64(~(toInt(env, x)))
	    | P.I64Eq(x, y) => BOOL(toInt(env, x) = toInt(env, y))
	    | P.I64NEq(x, y) => BOOL(toInt(env, x) <> toInt(env, y))
	    | P.I64Lt(x, y) => BOOL(IntInf.<(toInt(env, x), toInt(env, y)))
	    | P.I64Lte(x, y) => BOOL(IntInf.<=(toInt(env, x), toInt(env, y)))
	    | P.I64Gt(x, y) => BOOL(IntInf.>(toInt(env, x), toInt(env, y)))
	    | P.I64Gte(x, y) => BOOL(IntInf.>=(toInt(env, x), toInt(env, y)))
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

    fun load (CFG.MODULE{code, funcs}) = let
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
		in
		  evalExit (exit, List.foldl evalExp argEnv body)
		end
	  in
	    ??
	  end

  end
