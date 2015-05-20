(* translate.sml
 *
 * COPYRIGHT (c) 2013 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure Translate : sig

(*    structure Sxml : SXML*)
    val translate : PMLFrontEnd.Sxml.Program.t -> BOM.program

  end = struct

    structure S = PMLFrontEnd.Sxml
    structure V = Vector
    structure BV = BOMVar
    structure P = Prim

    structure VMap = BV.Map

    structure TyCacheKV = struct
        type ord_key = S.Type.dest
        fun compare(t1: S.Type.dest, t2: S.Type.dest) = (case (t1, t2)
           of (S.Type.Con (ltc1, tys1), S.Type.Con (ltc2, tys2)) => (case String.compare (S.Tycon.toString ltc1, S.Tycon.toString ltc2)
               of EQUAL => V.collate compare (V.map S.Type.dest tys1, V.map S.Type.dest tys2)
                | relation => relation
               (* end case *))
            | (S.Type.Con _, S.Type.Var _) => LESS
            | (S.Type.Var v1, S.Type.Var v2) => String.compare (S.Tyvar.toString v1, S.Tyvar.toString v2)
            | (S.Type.Var _, S.Type.Con _) => GREATER
        (* end case *))
    end
    structure TyCache = RedBlackMapFn (TyCacheKV)
    type tycache_t = BOMTy.t TyCache.map (* map from AST types to BOM types *)


    structure TycMapKV = struct
        type ord_key = S.Tycon.t
        fun compare(tyc1: S.Tycon.t, tyc2: S.Tycon.t) = String.compare (Layout.toString (S.Tycon.layout tyc1), Layout.toString (S.Tycon.layout tyc1))
    end
    structure TycMap = RedBlackMapFn (TycMapKV)
    type tycmap_t = BOMTyc.t TycMap.map (* map from AST tycs to BOM datatype constructors *)


    structure VarCacheKV = struct
        type ord_key = (S.Var.t)
        (* TODO(wings) this compare function is terrible but should work *)
        val compare = fn(v1, v2) => String.compare (Layout.toString (S.Var.layout v1), Layout.toString (S.Var.layout v2))
    end
    structure VarCache = RedBlackMapFn (VarCacheKV)
    type varcache_t = BOM.var VarCache.map (* map from AST variables to BOM variables *)

  (***** Translation environment *****)

    datatype var_bind
      = Lambda of (BOMTy.t -> BOM.lambda) (* used for primops and high-level ops *)
      | Var of BOM.var
      | EqOp			(* either "=" or "<>" *)


    fun cacheTyDest (cache, sxmltyd, bomty) = TyCache.insert(cache, sxmltyd, bomty)
    fun cacheTy (cache, sxmlty, bomty) = cacheTyDest(cache, S.Type.dest sxmlty, bomty)

    fun peekTyDest (cache, sxmltyd) = TyCache.find(cache, sxmltyd)
    fun peekTy (cache, sxmlty) = peekTyDest (cache, S.Type.dest sxmlty)

    fun lookupTyDest (cache, sxmltyd) = (case peekTyDest(cache, sxmltyd)
	   of SOME bomty => bomty
	    | NONE => raise Fail(concat["lookupTyDest(cache, <S.Type.dest>) = NONE"])
	  (* end case *))
    fun lookupTy (cache, sxmlty) = (case peekTyDest(cache, S.Type.dest sxmlty)
	   of SOME bomty => bomty
	    | NONE => raise Fail(concat["lookupTy(cache, ", Layout.toString (S.Type.layout sxmlty), ") = NONE"])
	  (* end case *))

    fun exhTy (tyCache) = BOMTy.T_Cont[lookupTy(tyCache, S.Type.exn)]


    val tupleTyc = BOMTyc.new("tuple", 0) (* TODO(wings): is this right? *)
    val exnTyc = BOMTyc.new("exn", 0)
    val exnTy0 = BOMTy.T_Con (exnTyc, [])
    val exhTy0 = BOMTy.T_Cont [exnTy0] (* TODO(wings): construct from SXML's S.Type.exn *)

(*
    datatype env = E of {
	varEnv : (BOMTy.t) PMLFrontEnd.Plist.t,	(* map from AST variables to BOM variables *)
	exh : BOM.var			(* current exception handler continuation *)
      }
*)

    datatype env = E of {
        tyCache : tycache_t,	(* map from AST types to BOM types *)
        tycMap : tycmap_t,	(* map from AST tycs to BOM datatype constructors *)
        varCache : varcache_t,	(* map from AST variables to BOM variables *)
        varEnv : BOMTy.t VMap.map,	(* map from BOM variables to their BOM types *)
        binds : (*var_bind*)BOM.exp VMap.map, (* map from BOM variables to their bound expressions *)
        exh : BOM.var			(* current exception handler continuation *)
      }

    fun newHandler (E{tyCache, tycMap, varCache, varEnv, binds, ...}) = let
	  val exh = BV.new("_exh", exhTy (tyCache))
	  val env = E{tyCache = tyCache, tycMap=tycMap, varCache=varCache, varEnv = varEnv, binds=binds, exh = exh}
	  in
	    (exh, env)
	  end
    
    fun handlerOf (E{exh, ...}) = exh

    fun writeBind (E{tyCache, tycMap, varCache, varEnv, binds, exh}, x, x') (*: (env * S.Type.t * BOMTy.t) -> env*) = E{tyCache=tyCache, tycMap=tycMap, varCache=varCache, varEnv=varEnv, binds=VMap.insert(binds, x, x'), exh=exh}

    fun lookupBind (E{tyCache, tycMap, varCache, varEnv, binds, exh}, x) : BOM.exp = (case VMap.find(binds, x)
	   of SOME x' => x'
	    | NONE => raise Fail(concat["lookupBind(env, ", BV.toString x, ") = NONE"])
	  (* end case *))

    fun writeVarTy (E{tyCache, tycMap, varCache, varEnv, binds, exh}, var, ty) = E{tyCache=tyCache, tycMap=tycMap, varCache=varCache, varEnv=VMap.insert(varEnv, var, ty), binds=binds, exh=exh}

    fun lookupVarTy (E{tyCache, tycMap, varCache, varEnv, binds, exh}, bomvar) = (case VMap.find(varEnv, bomvar)
	   of SOME bomty => bomty
	    | NONE => raise Fail(concat["lookupVarTy(env, ", BV.toString bomvar, ") = NONE"])
	  (* end case *))

    fun lookupVar (E{tyCache, tycMap, varCache, varEnv, binds, exh}, sxmlvar : S.Var.t) : BOM.var = (case VarCache.find(varCache, sxmlvar)
	   of SOME bomvar => bomvar
	    | NONE => raise Fail(concat["lookupVar(env, ", Layout.toString (S.Var.layout sxmlvar), ") = NONE"])
	  (* end case *))

    fun cacheVarTy (varEnv, bomvar, bomty) = VMap.insert(varEnv, bomvar, bomty)

    fun writeVar (E{tyCache, tycMap, varCache, varEnv, binds, exh}, sxmlvar, bomvar) = E{tyCache=tyCache, tycMap=tycMap, varCache=VarCache.insert(varCache, sxmlvar, bomvar), varEnv=varEnv, binds=binds, exh=exh}

    fun cacheVar (cache, sxmlvar, bomvar) = VarCache.insert(cache, sxmlvar, bomvar)

  (***** Translation functions *****)

(*
    datatypes: {cons: {arg: Type.t option,
                    con: Con.t} vector,
             tycon: Tycon.t,
             tyvars: Tyvar.t vector} vector,
*)
    fun transTy (env as E{tyCache, tycMap, varCache, varEnv, binds, exh}, ty (*as {tycon, tyvars, cons}*)) = 
    let
	  (* convert an uncached type from the MLton represenation to a BOM type *)
	  fun tr(tyCache: tycache_t, dest: S.Type.dest) = (case dest
	    of S.Type.Var _ => raise Fail "Type variable found in SXML"
		| (S.Type.Con(tyc, args)) => let
		    val (tyCache': tycache_t, args': BOMTy.t list) = V.foldl (fn (ty, (tyCache, tys)) => let val (tyCache', ty') = tr (tyCache, S.Type.dest ty) in (tyCache', ty' :: tys) end) (tyCache, []) args
		    val nArgs = length args';
		    fun singleArg () = if nArgs = 1 then List.hd args' else raise Fail (concat["Expected exactly one argument to tycon ", S.Type.Tycon.toString tyc, ", got ", Int.toString nArgs])
		    fun noArgs () = if nArgs = 0 then () else raise Fail (concat["Expected zero arguments to tycon ", S.Type.Tycon.toString tyc, ", got ", Int.toString nArgs])
		    val ty' = if S.Tycon.equals(tyc, S.Tycon.arrow)
			then let (* function type *)
			    val [domTy, rngTy] = args'
			    in
			      BOMTy.T_Fun([domTy], [exhTy (tyCache)], [rngTy])
			    end
			else if S.Tycon.equals(tyc, S.Tycon.array)
			  then BOMTy.T_Con (BOMTyc.arrayTyc, [singleArg ()])
			else if S.Tycon.equals(tyc, S.Tycon.bool)
			  then let (* convert bools to a Datatype of two DCons *)
			      val booltyc = BOMTyc.new("bool", 0)
				in
				  BOMDataCon.new booltyc ("true", []);
				  BOMDataCon.new booltyc ("false", []);
				  BOMTy.T_Con (booltyc, [])
				end
			else if S.Tycon.equals(tyc, S.Tycon.cpointer)
			  then BOMTy.T_Con(BOMTyc.addrTyc, [singleArg ()])
			else if S.Tycon.equals(tyc, S.Tycon.exn)
			  then (noArgs (); exhTy (tyCache))
			else if S.Tycon.equals(tyc, S.Tycon.intInf)
			  then (noArgs (); raise Fail "TODO(wings): BOMTy.T_Bignum")
			else if S.Tycon.equals(tyc, S.Tycon.list)
			  then let (* convert each specialization of 'a list to a Datatype *)
			    val dataTyc = BOMTyc.new(S.Tycon.toString tyc, 1)
			    in
			      (* XXX(wings): should data constructor names be obtained from somewhere else? *)
			      BOMDataCon.new dataTyc ((*S.Con.toString con1*)"nil", []);
			      BOMDataCon.new dataTyc ((*S.Con.toString con2*)"::", [singleArg ()]);
			      BOMTy.T_Con (dataTyc, []) (* XXX(wings): should this tyc have an arg? *)
			    end
			else if S.Tycon.equals(tyc, S.Tycon.reff)
			  then BOMTy.T_Con(BOMTyc.arrayTyc, [singleArg()]) (* XXX(wings): should we really treat refs as one-item arrays? *)
			else if S.Tycon.equals(tyc, S.Tycon.thread)
			  then (noArgs (); BOMTy.T_Con(BOMTyc.vprocTyc, []))
			else if S.Tycon.equals(tyc, S.Tycon.tuple)
			  then BOMTy.T_Con(tupleTyc, args')
			else if S.Tycon.equals(tyc, S.Tycon.vector)
			  then BOMTy.T_Con (BOMTyc.vectorTyc, [singleArg ()])
			else if S.Tycon.equals(tyc, S.Tycon.weak)
			  then raise Fail "Weak pointer type found in SXML"
			else BOMTy.T_Con(TycMap.lookup(tycMap, tyc), args')
			(*else raise Fail (concat["Failed to translate SXML tycon ", S.Type.Tycon.toString tyc, " with ", Int.toString nArgs," arguments"])*)
		    in
		      (*print (concat["caching ty ", Layout.toString (S.Type.layout ty), "\n"]); *)(cacheTy (tyCache', ty, ty'), ty')
		    end
	      (* end case *))
	  in (case peekTy (tyCache, ty)
		 of SOME ty' => (env, ty')
		  | NONE => let
		      val (tyCache', ty') = tr (tyCache, S.Type.dest ty)
		    in
		      (E{tyCache=tyCache', tycMap=tycMap, varCache=varCache, varEnv=varEnv, binds=binds, exh=exh}, ty')
		    end
	      (* end case *))
	  end

     (*XXX(wings): should we set kind? one of:
        VK_None
      | VK_Let of exp
      | VK_RHS of rhs
      | VK_Param
      | VK_Fun of lambda
      | VK_Cont of lambda
      | VK_CFun of c_fun
      *)
    fun newVar (env, x, ty) = let
      val (env', ty') = transTy (env, ty)
      val x' = BV.new (S.Var.toString x, ty')
      val env' = writeVar (env, x, x') (* construct explicit map from x to x' so x can be used to look up x' and x' can be used to look up the type *)
      in
	(x', writeVarTy (env', x', ty'))
      end

    fun transExp (e, env) : BOM.exp = let
	  val {decs, result} = S.Exp.dest e
	  fun transDecs (env, decs, result) = 
	     (case decs of
		[] => (case result of
		  S.VarExp.T{targs, var} => lookupBind(env, lookupVar(env, var))
		  (* end case *))
		| d::rest => transDec (env, d, fn (env) => transDecs(env, rest, result))
	     (* end case *))
	  in
	    transDecs(env, decs, result)
	  end

    and transDec (env, d, k : env -> BOM.exp) = (case d
	   of S.Dec.Exception{arg, con} => raise Fail "exception declaraion?"
	    | S.Dec.Fun{decs, ...} => let
		fun bind ({var, ty, lambda}, (fns, env)) = let
		  val (var', env) = newVar(env, var, ty)
		  in
		    ((var', lambda)::fns, env)
		  end
		val (fns, env) = V.foldr bind ([], env) decs
		in
		  BOM.mkFun(List.map (transLambda env) fns, k env)
		end
	    | S.Dec.MonoVal{var, ty, exp} => let
		val (var', env) = newVar(env, var, ty)
		in
		  transPrimExp (env, var', exp, k)
		end
	    | S.Dec.BOM{bom} => raise Fail "BOM declaration"
	    | S.Dec.PolyVal _ => raise Fail "Polymorphic declaration found in SXML"
	  (* end case *))

    and transLambda env (f, lambda) = let
	  val {arg, argType, body, ...} = S.Lambda.dest lambda
	  val (param, env) = newVar(env, arg, argType)
	  val (exh, env) = newHandler env
	  in
	    BOM.mkLambda{f = f, params = [param], exh = [exh], body = transExp(body, env)}
	  end

  (* translate a constant word value of a given size into a (Literal.literal * BOMTy.t) pair *)
  (* XXX(wings): make sure this handles [un]signedness properly *)
    and transConst (sz, word) : (Literal.literal * BOMTy.t) = let
	  val lit = Literal.Int (S.Atoms.WordX.toIntInf word)
	  val ty = (if sz = S.Atoms.WordSize.word8
	      then BOMTy.T_Raw RawTypes.Int8
	    else if sz = S.Atoms.WordSize.word16
	      then BOMTy.T_Raw RawTypes.Int16
	    else if sz = S.Atoms.WordSize.word32
	      then BOMTy.T_Raw RawTypes.Int32
	    else if sz = S.Atoms.WordSize.word64
	      then BOMTy.T_Raw RawTypes.Int64
	    else raise Fail "TODO(wings): BOMTy.T_Bignum")
	in
	  (lit, ty)
	end

    and transDCon (E{tyCache, tycMap, varCache, varEnv, binds, exh}, con, targs) : BOMDataCon.t = let
	val tycon = PMLFrontEnd.conTycon con
	val ty : S.Type.dest = S.Type.Con (tycon, targs)
	val ty' = lookupTyDest (tyCache, ty)
	val tyName = "..." (* XXX(wings): stringify ty? (* Layout.toString (S.Type.layout ty) *) *)
	val (name, dcons) = case ty'
	  of BOMTy.T_Con (tyc, params) => (BOMTyc.nameOf tyc, BOMTyc.consOf tyc)
	   | _ => raise Fail (concat ["Failed to translate data constructor for ", tyName, ".", S.Con.toString con])
      in
	case List.find (fn (dcon) => BOMDataCon.nameOf dcon = S.Con.toString con) dcons
	  of SOME x => x
	   | NONE => raise Fail (concat ["Failed to translate data constructor for ", tyName, ".", S.Con.toString con])
      end

  (* translate a PrimExp.t term.  These occur as the rhs of the a MonoVal binding; the
   * translated lhs variable is passed as an argument.
   *)
    and transPrimExp (env, lhs, e, k : env -> BOM.exp) : BOM.exp = let
	  fun mkLet e' = BOM.mkLet([lhs], e', k env)
	  fun mkStmt rhs = BOM.mkStmt([lhs], rhs, k env)
	  in
	    case e
	     of S.PrimExp.App{func, arg} => let
		  val func' = transVarExp(env, func)
		  val arg' = transVarExp(env, arg)
		  in
		    mkLet(BOM.mkApply(func', [arg'], [handlerOf env]))
		  end
	      | S.PrimExp.Case{test, cases, default} => let
		  val test' = transVarExp(env, test)
		  val default' = Option.map (fn (e, region) => transExp(e, env)) default
		  val cases' = (case cases
			(* for cases on inductive datatypes, ... *)
			 of S.Cases.Con (rules) => V.map (fn (pat as S.Pat.T{arg, con, targs}, exp) => let
				 (*Pat.t = T of {
					arg: (Var.t * Type.t) option,
					con: Con.t,
					targs: Type.t vector
				}*)
				  val dcon=transDCon (env, con, targs)
				  val vars=(case arg
				    of NONE => []
				     | SOME (var, sxmlty) => [lookupVar(env, var)] (* XXX(wings): unpack tuple into multiple vars? *)
				   (* end case *))
				in
				  (* P_DCon of data_con * var list *)
				  (BOM.P_DCon (dcon, vars), transExp(exp, env))
				end) rules
			(* for cases on int, word, char, ... *)
			  | S.Cases.Word (sz, rules) => V.map (fn (word, exp) => (BOM.P_Const(transConst(sz, word)), transExp(exp, env))) rules
			(* end case *))
		  in
		    mkLet(BOM.mkCase(test', V.foldl (fn(x, y) => x::y) [] cases', default'))
		  end
		(* QUESTION: are there primitive dcons that have targs?  No! *)
	      | S.PrimExp.ConApp{con, arg, targs} => let
		  val con' = transDCon (env, con, targs)
		  in
		    (* translate dcon and tupled args *)
		    mkStmt(BOM.E_DCon(con', case arg
		       of SOME arg' => (* XXX(wings): unpack typaram tuple into its own list? *) [transVarExp(env, arg')]
		        | NONE => []))
		  end
	      | S.PrimExp.Const c => let
	        (* XXX(wings): verify that this produces the correct endianness. will fail obviously if at all, but whether it does is architecture-dependent. *)
		    fun bytesIn (w) = let
				fun lit(n) = S.Atoms.WordX.fromChar (Char.chr n)
				val byte = Word8.fromInt (S.Atoms.WordX.toInt (S.Atoms.WordX.andb(w, (lit 255))))
				val rest = S.Atoms.WordX.rshift(w, (lit 8), {signed=false})
			  in
			    if S.Atoms.WordX.equals(rest, lit 0)
			    then [byte]
			    else byte::(bytesIn rest)
			  end
		  val lit = (case c
			 of S.Const.IntInf i => Literal.Int i
			  | S.Const.Null => Literal.StateVal 0w0 (* XXX(wings): there is no Addr literal but it seems like genLit in codegen-fn.sml will treat this properly *)
			  | S.Const.Real flt => Literal.Float (FloatLit.fromBytes (Word8Vector.fromList (bytesIn (valOf (S.Atoms.RealX.castToWord flt))))) (* XXX(wings): this is terrible. *)
			  | S.Const.Word w => Literal.Int (S.Atoms.WordX.toIntInf w)
			  | S.Const.WordVector v => Literal.String (S.Atoms.WordXVector.toString v) (* XXX(wings): can this corrupt strings? see word-x-vector.fun *)
			(* end case *))
		  in
		    mkStmt(BOM.E_Const(lit, BV.typeOf lhs))
		  end
	      | S.PrimExp.Handle{try, catch=(x, ty), handler} => let
		  val (x', handlerEnv) = newVar(env, x, ty)
		  val (exh, tryEnv) = newHandler env
		  in
		    mkLet(BOM.mkCont(
		      BOM.mkLambda{f = exh, params = [x'], exh = [], body = transExp(handler, handlerEnv)},
		      transExp(try, tryEnv)))
		  end
	      | S.PrimExp.Lambda lambda => BOM.mkFun([transLambda env (lhs, lambda)], k env)
		(* XXX: These could be BOM HLOps? *)
	      | S.PrimExp.PrimApp{prim, targs, args} => let
		fun mkCFun arg = let
		    val cf = CFunctions.CFun arg
		  in
		    BV.setKind(#var arg, BV.VK_CFun cf);
		    cf
		  end
		  (* TODO(wings): get return type from C function *)
		  fun expFromRhs (rhs, ret) = let
		      val var = BV.new("_", ret)
		    in
		      BOM.mkStmt([var], rhs, BOM.mkRet[var])
		  end
		val args = V.foldl (fn (argi, rest) => transVarExp(env,argi)::rest) [] args
		fun argn(n) = List.nth(args, n)
		val unitTy = BOMTy.T_Con(tupleTyc, [])
		val (rhs, ret) = 
		  if S.Prim.equals (prim, S.Prim.array) then raise Fail "array" else
		  if S.Prim.equals (prim, S.Prim.arrayLength) then raise Fail "arrayLength" else
		  if S.Prim.equals (prim, S.Prim.assign) then (BOM.E_Prim (P.AdrStore(argn 0, argn 1)), unitTy) else
		  if S.Prim.equals (prim, S.Prim.bogus) then raise Fail "bogus" else
		  if S.Prim.equals (prim, S.Prim.bug) then (BOM.E_CCall(BV.new("_", unitTy), []), unitTy) else
		  if S.Prim.equals (prim, S.Prim.deref) then (BOM.E_Prim (P.AdrLoad (argn 0)), BV.typeOf (argn 0)) else
		  if S.Prim.equals (prim, S.Prim.reff) then raise Fail "ref" else
		  if S.Prim.equals (prim, S.Prim.cpointerSub) then raise Fail "cpointerSub" else
		raise Fail ("failed to translate PrimApp " ^ S.Prim.toString prim)
		in
		  expFromRhs (rhs, ret)
		end
	      | S.PrimExp.Profile info => k env (* ignore for now *)
		(* QUESTION: what does the "extend" flag mean? *)
	      | S.PrimExp.Raise{exn, ...} =>
		  BOM.mkThrow (handlerOf env, [transVarExp(env, exn)])
	      | S.PrimExp.Select{offset, tuple} =>
		  mkStmt(BOM.E_Select(offset, transVarExp(env, tuple)))
	      | S.PrimExp.Tuple args =>
		  mkStmt(BOM.E_Alloc(
		    BV.typeOf lhs,  (* type *)
		    V.foldr (fn (x, xs) => transVarExp(env, x)::xs) [] args))
	      | S.PrimExp.Var x => mkLet(BOM.mkRet[transVarExp(env, x)])
	    (* end case *)
	  end

    and transVarExp (env, S.VarExp.T{var, ...}) = lookupVar(env, var)

(*
Datatype.t = {cons: {arg: Type.t option,
                       con: Con.t} vector,
                tycon: Tycon.t,
                tyvars: Tyvar.t vector}
*)

    fun transDatatype ({cons, tycon, tyvars}, env) = let
        (*val () = print ("transDatatype " ^ S.Tycon.toString tycon ^ "\n")*)
        (* XXX(wings): remove nullary dcon count *)
        fun isNullary {arg, con} = not (isSome arg)
        val nNullary = V.foldl (fn (elem, n) => n + (if isNullary elem then 1 else 0)) 0 cons
        val dataTyc = BOMTyc.new(S.Tycon.toString tycon, V.length tyvars)
        val E{tyCache, tycMap, varCache, varEnv, binds, exh} = env;
        val tyCache' = cacheTyDest(tyCache, S.Type.Con(tycon, V.fromList []), BOMTy.T_Con(dataTyc, [](*TODO(wings): convert these lazily so we can handle type arguments instead of assuming none!*)))
        val env = E{tyCache=tyCache', tycMap=TycMap.insert(tycMap, tycon, dataTyc), varCache=varCache, varEnv=varEnv, binds=binds, exh=exh}
        val env = V.foldl (fn({con: S.Con.t, arg: S.Type.t option}, env) => (case arg
              of SOME a => let
                   val (env, a') = transTy (env, a)
                 in
                   (* this mutates dataTyc's cons which is a ref-cell, and returns the data_con it stores: *)
                   BOMDataCon.new dataTyc (S.Con.toString con, [a']);
                   (* XXX(wings): unpack typaram tuple into its own list? *)
                   env
                 end
               | NONE => (BOMDataCon.new dataTyc (S.Con.toString con, [(* no arguments *)]); env)
               (* end case *))) env cons
        in
            env
        end

(* QUESTION: what does the overflow option mean?  Used to keep Overflow exn from being
 * shaken out.
 *)
    fun translate (S.Program.T{datatypes, overflow, body}) = let
	  val env = E{tyCache = TyCache.empty, tycMap=TycMap.empty, varCache = VarCache.empty, varEnv = VMap.empty, binds = VMap.empty, exh = BV.new("_exh", exhTy0) }
	  (* initially translate all the datatypes listed by MLton *)
	  val env as E{tycMap=tycMap, ...} = V.foldl transDatatype env datatypes
	  val body = transExp (body, env)
	  (* val argTy = BOMTy.T_Raw RawTypes.Int32 *)
	  (* val arg = BV.new("_arg", argTy) *)
	  val mainFun = BOM.FB{
		  f = BV.new("main", BOMTy.T_Fun([(* argTy *)], [(* BOMTy.exhTy *)], [(* TypeOf.exp body *)])),
		  params = [(* arg *)],
		  exh = [(* exh *)],
		  body = body
		}
	  in
	    BOM.PROGRAM{exnTyc=exnTyc, dataTycs=TycMap.listItems tycMap, hlops=[], externs=[], body=mainFun}
	  end
	end
