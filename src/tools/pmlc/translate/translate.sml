(* translate.sml
 *
 * COPYRIGHT (c) 2013 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure Translate : sig

    structure Sxml : SXML

    val translate : Sxml.Program.t -> BOM.program

  end = struct

    structure S = PMLFrontEnd.Sxml
    structure B = BuildBOM
    structure V = Vector

  (***** Translation environment *****)

    datatype env = E of {
	varEnv : var_bind VMap.map,	(* map from AST variables to BOM variables *)
	exh : BOM.var			(* current exception handler continuation *)
      }

    fun lookupVar (E{varEnv, ...}, x) = (case VMap.find(varEnv, x)
	   of SOME x' => x'
	    | NONE => raise Fail(concat["lookupVar(_, ", Var.toString x, ")"])
	  (* end case *))

    fun newHandler _ = ??
    fun handlerOf (E{exh, ...}) = exh


  (***** Translation functions *****)

    fun transTy (env, ty) =

    fun newVar (env, x, ty) = ??

    fun transExp (e, env) = let
	  val {decs, result} = S.Exp.dest e
	  in
	  end

    and transDec (env, d, k : env -> BOM.exp) = (case d
	   of S.Dec.Fun{decs, ...} => let
		fun bind ({var, ty, lambda}, (fns, env)) = let
		      val (var', env) = newVar(env, var, ty)
		      in
			((var', lambda)::fns, env)
		      end
		val (fns, env) = V.foldr bind ([], env) decs
		in
		  B.mkFun(List.map (transLambda env) fns, k env)
		end
	    | S.Dec.MonoVal{var, ty, exp} => let
		val (var', env) = newVar(env, var, ty)
		in
		  transPrimExp (env, var', exp, k)
		end
	    | _ => raise Fail "impossible"
	  (* end case *))

    and transLambda env (f, lambda) = let
	  val {arg, argType, body, ...} = S.Lambda.dest lambda
	  val (param, env) = newVar(env, arg, argType)
	  val (exh, env) = newHandler env
	  in
	    B.mkLambda(f, [param], [exh], transExp(body, env))
	  end

  (* translate a PrimExp.t term.  These occur as the rhs of the a MonoVal binding; the
   * translated lhs variable is passed as an argument.
   *)
    and transPrimExp (env, lhs, e, k : env -> BOM.exp) = let
	  fun mkLet e' = B.mkLet([lhs], e', k env)
	  fun mkStmt rhs = B.mkStmt([lhs], rhs, k env)
	  in
	    case e
	     of S.PrimExp.App{func, arg} => let
		  val func' = transVarExp(env, func)
		  val arg' = transVarExp(env, arg)
		  in
		    mkLet(B.mkApply(func', [arg'], [handlerOf env]))
		  end
	      | S.PrimExp.Case{test, cases=S.Cases.Con cases, default} => let
		  val test' = transVarExp(env, test)
		  val default' = Option.map (fn e => transExp(e, env)) default
		  val cases' = (case cases
			 of S.Cases.Con rules => ??
(* for cases on int, word, char, ... *)
			  | S.Cases.Word(sz, rules) => ??
			(* end case *))
		  in
		    mkLet(B.mkCase(test', cases', default'))
		  end
(* QUESTION: are there primitive constructors that have targs?  No! *)
	      | S.PrimExp.ConApp{con, arg, ...} => let
		  val con' = ??
		  in
		    mkStmt(B.mkDCon(con', [transVarExp(env, arg)]))
		  end
	      | S.PrimExp.Const c => let
		  val lit = (case c
			 of S.Const.IntInf i =>
			  | S.Const.Null => ?? (* C TYpe *)
			  | S.Const.Real flt => 
			  | S.Const.Word w => 
			  | S.Const.WordVector v => ?? (* strings *)
			(* end case *))
		  in
		    mkStmt(B.mkConst(BV.typeOf lhs, lit))
		  end
	      | S.PrimExp.Handle{try, catch=(x, ty), handler} => let
		  val (x', handlerEnv) = newVar(env, x, ty)
		  val (exh, tryEnv) = newHandler env
		  in
		    mkLet(B.mkCont(
		      B.mkLambda(exh, [x'], [], transExp(handler, handlerEnv)),
		      transExp(try, tryEnv)))
		  end
	      | S.PrimExp.Lambda lambda => B.mkFun([transLmabda env (lhs, lambda)], k env)
(* These could be BOM HLOps? *)
	      | S.PrimExp.PrimApp{prim, targs, args} =>
	      | S.PrimExp.Profile info => k env (* ignore for now *)
(* QUESTION: what does the "extend" flag mean? *)
	      | S.PrimExp.Raise{exn, ...} =>
		  B.mkThrow (handlerOf env, [transVarExp(env, exn)])
	      | S.PrimExp.Select{offset, tuple} =>
		  mkStmt(B.mkSelect(offset, transVarExp(env, tuple)))
	      | S.PrimExp.Tuple args =>
		  mkStmt(B.mkAlloc(
		    BV.typeOf lhs,  (* type *)
		    V.foldr (fn (x, xs) => transVarExp(env, x)::xs) [] args))
	      | S.PrimExp.Var x => mkLet(B.mkRet[transVarExp(env, x)])
	    (* end case *)
	  end

    and transVarExp (env, S.VarExp.T{var, ...}) = lookupVar(env, var)

(*
                     datatypes: {cons: {arg: Type.t option,
                                        con: Con.t} vector,
                                 tycon: Tycon.t,
                                 tyvars: Tyvar.t vector} vector,
*)
    fun transDatatype ({tycon, tyvars, cons}, env) = 

(* QUESTION: what does the overflow option mean?  Used to keep Overflow exn from being
 * shaken out.
 *)
    fun translate (S.Program.T{datatypes, overflow, body}) = let
	  val env0 = ??
	  val env = V.foldl transDatatype env0 datatypes
	  val body' = transExp (body, env)
	  in
	    ??
	  end

  end
