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
    structure BExp = S.CoreBOM.Exp
    structure BSExp = S.CoreBOM.SimpleExp

  (***** Define environment map types/structures *****)

    structure VMap = BV.Map

    structure TyCacheKV = struct
        type ord_key = S.Type.dest
        fun compare(t1: S.Type.dest, t2: S.Type.dest) = (case (t1, t2)
           of (S.Type.Con (ltc1, tys1), S.Type.Con (ltc2, tys2)) => (case Word.compare (S.Tycon.hash ltc1, S.Tycon.hash ltc2)
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
        fun compare(tyc1: S.Tycon.t, tyc2: S.Tycon.t) = Word.compare (S.Tycon.hash tyc1, S.Tycon.hash tyc2)
    end
    structure TycMap = RedBlackMapFn (TycMapKV)
    type tycmap_t = BOMTyc.t TycMap.map (* map from AST tycs to BOM datatype constructors *)

    structure VarCacheKV = struct
        type ord_key = (S.Var.t)
        fun compare(v1, v2) = Word.compare (S.Var.hash v1, S.Var.hash v2)
    end
    structure VarCache = RedBlackMapFn (VarCacheKV)
    type varcache_t = BOM.var VarCache.map (* map from AST variables to BOM variables *)

    datatype var_bind
      = Lambda of (BOMTy.t -> BOM.lambda) (* used for primops and high-level ops *)
      | Var of BOM.var
      | DCon of BOM.data_con
      (*| EqOp                        (* either "=" or "<>" *)*)

    structure BOMValKV = struct
        type ord_key = (S.CoreBOM.Val.t)
        fun compare(v1, v2) = Stamp.compare (S.CoreBOM.Val.stampOf v1, S.CoreBOM.Val.stampOf v2)
    end
    structure BOMValMap = RedBlackMapFn (BOMValKV)
    type bomvalmap_t = var_bind BOMValMap.map (* map from CoreBOM (frontend) values to (middle-end) BOM variables *)

  (***** Translation environment *****)

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


    val boolTyc = BOMTyc.new("bool", 0)
      val falseCon = BOMDataCon.new boolTyc ("false", []);
      val trueCon = BOMDataCon.new boolTyc ("true", []);
    val exnTyc = BOMTyc.new("exn", 0)
    val exnTy0 = BOMTy.T_Con (exnTyc, [])
    val exhTy0 = BOMTy.T_Cont [exnTy0] (* TODO(wings): construct from SXML's S.Type.exn *)

    val progExterns : BOM.var CFunctions.c_fun list ref = ref []

    datatype env = E of {
        tyCache : tycache_t, (* map from AST types to BOM types *)
        tycMap : tycmap_t, (* map from AST tycs to BOM datatype constructors *)
        varCache : varcache_t, (* map from AST variables to BOM variables *)
        varEnv : BOMTy.t VMap.map, (* map from BOM variables to their BOM types *)
        bomValMap : bomvalmap_t, (* map from CoreBOM (frontend) values to BOM variables *)
        binds : (*var_bind*)BOM.exp VMap.map, (* map from BOM variables to their bound expressions *)
        exh : BOM.var (* current exception handler continuation *)
      }

    fun newHandler (E{tyCache, tycMap, varCache, varEnv, bomValMap, binds, exh=oldExh}) = let
          val exh = BV.new("_exh", exhTy (tyCache))
          val env = E{tyCache = tyCache, tycMap=tycMap, varCache=varCache, varEnv=varEnv, bomValMap=bomValMap, binds=binds, exh=exh}
          in
            (exh, env)
          end

    fun handlerOf (E{exh, ...}) = exh

    fun writeBind (E{tyCache, tycMap, varCache, varEnv, bomValMap, binds, exh}, x, x') = E{tyCache=tyCache, tycMap=tycMap, varCache=varCache, varEnv=varEnv, bomValMap=bomValMap, binds=VMap.insert(binds, x, x'), exh=exh}

    fun lookupBind (E{binds, ...}, x) : BOM.exp = (case VMap.find(binds, x)
           of SOME x' => x'
            | NONE => raise Fail(concat["lookupBind(env, ", BV.toString x, ") = NONE"])
          (* end case *))

    fun writeVarTy (E{tyCache, tycMap, varCache, varEnv, bomValMap, binds, exh}, var, ty) = E{tyCache=tyCache, tycMap=tycMap, varCache=varCache, varEnv=VMap.insert(varEnv, var, ty), bomValMap=bomValMap, binds=binds, exh=exh}

    fun lookupVarTy (E{varEnv, ...}, bomvar) = (case VMap.find(varEnv, bomvar)
           of SOME bomty => bomty
            | NONE => raise Fail(concat["lookupVarTy(env, ", BV.toString bomvar, ") = NONE"])
          (* end case *))

    fun lookupVar (E{varCache, ...}, sxmlvar : S.Var.t) : BOM.var = (case VarCache.find(varCache, sxmlvar)
           of SOME bomvar => bomvar
            | NONE => raise Fail(concat["lookupVar(env, ", Layout.toString (S.Var.layout sxmlvar), ") = NONE"])
          (* end case *))

    fun writeVar (E{tyCache, tycMap, varCache, varEnv, bomValMap, binds, exh}, sxmlvar, bomvar) = E{tyCache=tyCache, tycMap=tycMap, varCache=VarCache.insert(varCache, sxmlvar, bomvar), varEnv=varEnv, bomValMap=bomValMap, binds=binds, exh=exh}

    fun lookupBOMVal (E{bomValMap, ...}, bomVal : S.CoreBOM.Val.t) : var_bind = (case BOMValMap.find(bomValMap, bomVal)
           of SOME bomvarbind => bomvarbind
            | NONE => raise Fail(concat["lookupBOMVal(env, ", S.CoreBOM.ValId.toString (S.CoreBOM.Val.idOf bomVal), ") = NONE"])
          (* end case *))

    fun writeBOMVal (E{tyCache, tycMap, varCache, varEnv, bomValMap, binds, exh}, bomVal, bomvar) = E{tyCache=tyCache, tycMap=tycMap, varCache=varCache, varEnv=varEnv, bomValMap=BOMValMap.insert(bomValMap, bomVal, bomvar), binds=binds, exh=exh}




    fun newVarBOMValWithTy (env, bomVal, ty) = let
      val var = BV.new (S.CoreBOM.ValId.toString
        (S.CoreBOM.Val.idOf bomVal) ^ ".", ty)
      val env' = writeVarTy (env, var, ty)
      in
        (var, writeBOMVal (env', bomVal, Var var))
      end


    (* from manticore.lex *)
    fun mkFloat s = let
      val ss = Substring.full s
	  val (isNeg, rest) = (case Substring.getc ss
		 of SOME(#"-", r) => (true, r)
		  | SOME(#"~", r) => (true, r)
		  | _ => (false, ss)
		(* end case *))
	  val (whole, rest) = Substring.splitl Char.isDigit rest
	  val rest = Substring.triml 1 rest (* remove "." *)
	  val (frac, rest) = Substring.splitl Char.isDigit rest
	  val exp = if Substring.isEmpty rest
		then 0
		else let
		  val rest = Substring.triml 1 rest (* remove "e" or "E" *)
		  in
		    #1(valOf(Int.scan StringCvt.DEC Substring.getc rest))
		  end
	  in
	    FloatLit.float{
		isNeg = isNeg,
		whole = Substring.string whole,
		frac = Substring.string frac,
		exp = exp
	      }
	  end


    fun transCFunTy env (S.CoreBOM.CProto.T (S.CoreBOM.CFunction.T {convention, kind, target, prototype, ...}, attrs)) = let
      fun transCTy cty = (case cty
         of S.CType.CPointer => CFunctions.PointerTy
          | S.CType.Int8 => CFunctions.BaseTy RawTypes.Int8
          | S.CType.Int16 => CFunctions.BaseTy RawTypes.Int16
          | S.CType.Int32 => CFunctions.BaseTy RawTypes.Int32
          | S.CType.Int64 => CFunctions.BaseTy RawTypes.Int64
          | S.CType.Objptr => raise Fail "objptr?"
          | S.CType.Real32 => CFunctions.BaseTy RawTypes.Float32
          | S.CType.Real64 => CFunctions.BaseTy RawTypes.Float64
          | S.CType.Word8 => CFunctions.BaseTy RawTypes.UInt8
          | S.CType.Word16 => CFunctions.BaseTy RawTypes.UInt16
          | S.CType.Word32 => CFunctions.BaseTy RawTypes.UInt32
          | S.CType.Word64 => CFunctions.BaseTy RawTypes.UInt64
        (* end case *))
      (* convert argument and return types from MLton's C-function repr to BOM's *)
      val (argTys, retTy) = prototype
      val retTy' = (case retTy
         of SOME t => transCTy t
          | NONE => CFunctions.VoidTy
        (* end case *))
      val argTys' = Vector.foldl (fn (x, rest) => transCTy x :: rest) [] argTys

      (* destringify attributes *)
      val attrs' = List.mapPartial (fn(attr) => case attr
         of "pure" => SOME CFunctions.A_pure (* technically, we shouldn't ever see this case since MLton puts purity into the kind *)
          | "alloc" => SOME CFunctions.A_alloc
          | "noreturn" => SOME CFunctions.A_noreturn
          | _ => (print ("unknown extern function attribute: " ^ attr ^ "\n"); NONE)) attrs
      (* MLton reifies purity in kind whereas PML stores it in an attribute *)
      val attrs' = if kind = S.CFunction.Kind.Pure then CFunctions.A_pure::attrs' else attrs'

      (* build the appropriate BOM type and make a variable with it to place in the env *)
      val ty = BOMTy.T_CFun (CFunctions.CProto (retTy', argTys', attrs'))
    in
      (ty, retTy', argTys', attrs')
    end

    fun transCoreBOMTy env coreBomTy: (env * BOMTy.t) = let
      val _ = print ("transCoreBOMTy " ^ Layout.toString (S.CoreBOM.BOMType.layout coreBomTy) ^ "\n")
      fun transCoreBOMTy' ty = #2 (transCoreBOMTy env ty)
      val bomTy =
        (case coreBomTy
          of S.CoreBOM.BOMType.Param p => BOMTy.T_Param (raise Fail "TODO(wings): translate type param")
           | S.CoreBOM.BOMType.TyCon { con=con, args=args } => BOMTy.T_Con (raise Fail "TODO(wings): translate tycon")
           | S.CoreBOM.BOMType.Con { dom=dom, rng=rng } => raise Fail "TODO(wings): translate con"
           | S.CoreBOM.BOMType.Record fields => BOMTy.T_Record (raise Fail "TODO(wings): translate fields")
           | S.CoreBOM.BOMType.Tuple bool_type_ts => BOMTy.T_Record (raise Fail "TODO(wings): translate bool_type_ts")
           | S.CoreBOM.BOMType.Fun { dom=dom, cont=cont, rng=rng} => BOMTy.T_Fun
             (List.map transCoreBOMTy' dom,
             List.map transCoreBOMTy' cont,
             List.map transCoreBOMTy' rng)
           | S.CoreBOM.BOMType.BigNum => raise Fail "TODO(wings): BOMTy.T_Bignum"
           | S.CoreBOM.BOMType.Exn => exnTy0
           | S.CoreBOM.BOMType.Any => BOMTy.T_Any
           | S.CoreBOM.BOMType.VProc => BOMTy.vprocTy
           | S.CoreBOM.BOMType.Array t => BOMTy.arrayTy (transCoreBOMTy' t)
           | S.CoreBOM.BOMType.Vector t => BOMTy.vectorTy (transCoreBOMTy' t)
           | S.CoreBOM.BOMType.Cont tys => BOMTy.T_Cont (List.map transCoreBOMTy' tys)
           | S.CoreBOM.BOMType.CFun cproto => #1 (transCFunTy env cproto)
           | S.CoreBOM.BOMType.Addr t => BOMTy.addrTy (transCoreBOMTy' t)
           | S.CoreBOM.BOMType.Raw rawTy => BOMTy.T_Raw (case rawTy
             of S.CoreBOM.RawTy.Int8 => BOMTy.Int8
              | S.CoreBOM.RawTy.Int16 => BOMTy.Int16
              | S.CoreBOM.RawTy.Int32 => BOMTy.Int32
              | S.CoreBOM.RawTy.Int64 => BOMTy.Int64

              | S.CoreBOM.RawTy.UInt8 => BOMTy.UInt8
              | S.CoreBOM.RawTy.UInt16 => BOMTy.UInt16
              | S.CoreBOM.RawTy.UInt32 => BOMTy.UInt32
              | S.CoreBOM.RawTy.UInt64 => BOMTy.UInt64

              | S.CoreBOM.RawTy.Float32 => BOMTy.Float32
              | S.CoreBOM.RawTy.Float64 => BOMTy.Float64

              | S.CoreBOM.RawTy.Vec128 => BOMTy.Vec128
              | S.CoreBOM.RawTy.Vec256 => BOMTy.Vec256
              | S.CoreBOM.RawTy.Vec512 => BOMTy.Vec512)
           | S.CoreBOM.BOMType.Error => raise Fail "translating error type"
           (* end case *))
      in
        (env, bomTy)
      end

    fun newVarBOMVal (env, bomVal) = let
      val (env', ty) = transCoreBOMTy env (S.CoreBOM.Val.typeOf bomVal)
      in
        newVarBOMValWithTy (env', bomVal, ty)
      end

    fun newVarBOMVals (e, bomVals) = foldl
      (fn (bomVal, (vars, env)) => let
        val (var, env') = newVarBOMVal (env, bomVal)
        in
          (var::vars, env')
        end) ([], e) bomVals

    fun transBOMExp (e, env) : BOM.exp = let
      val (e, tys) = (S.CoreBOM.Exp.node e, List.map (fn ty => #2 ((transCoreBOMTy env) ty)) (S.CoreBOM.Exp.typeOf e))
      in
        case e
        of BExp.Let (vals, rhs, e') => let
             val (vars, env) = newVarBOMVals (env, vals)
             in
               BOM.mkLet (vars, transBOMRhs (rhs, env), transBOMExp (e', env))
             end
         | BExp.FunExp (fundefs, e') => let
           val (lambdas, env) = transBOMFuns (fundefs, env)
           in
             BOM.mkFun (lambdas, transBOMExp (e', env))
           end
         | BExp.ContExp (contVal, argVals, (*=*) body, (*in*) e') => let
             fun convertTy (bomVal, (env, tys)) = let
               val (env, ty) = transCoreBOMTy env (S.CoreBOM.Val.typeOf bomVal)
               in
                 (env, ty::tys)
               end
             val (env, argTys) = List.foldl convertTy (env, []) argVals
             val (paramVars, env) = newVarBOMVals (env, argVals)
             val contTy = BOMTy.T_Cont (argTys)
             val (contVar, env) = newVarBOMValWithTy (env, contVal, contTy)
             val lambda = BOM.mkLambda {f=contVar, params=paramVars,
               exh=[handlerOf env], body=transBOMExp (body, env)}
             in
               BOM.mkCont (lambda, transBOMExp (e', env))
             end
         | BExp.If (cond, then_, else_) => let
           val then_ = transBOMExp(then_, env)
           val else_ = transBOMExp(else_, env)
           in
             transBOMCond (cond, env, fn primcond => BOM.mkIf (primcond, then_, else_))
           end
           (* TODO(wings): verify that "do" outputs both expressions *)
         | BExp.Do (se, e') => transBOMSimpleExp(se, env, fn [se_var] =>
             transBOMExp (e', env))
         | BExp.Case (se, caserules) => transBOMSimpleExp(se, env, fn [se_var] => let
             val (cases, def) = transBOMCaseRules (env, caserules)
             in
               BOM.mkCase (se_var, cases, def)
             end)
         | BExp.Typecase (typaram, tycaserules) => BOM.mkTypecase (raise Fail "TODO(wings): bomexp.typecase")
         | BExp.Apply (func, conts, args) => let
             val Var func = lookupBOMVal (env, func)
             in transBOMSimpleExps (conts, env, fn conts_vars =>
                 transBOMSimpleExps (args, env, fn args_vars =>
                     BOM.mkApply (func, conts_vars, args_vars), []), [])
             end
           (* TODO(wings): ensure that throw/return folds are correct *)
         | BExp.Throw (exncon, ses) => let
           val Var exncon = (print "lookup exncon\n"; lookupBOMVal (env, exncon))
           fun k args [arg] = BOM.mkThrow (exncon, arg::args)

           val k' = List.foldl (fn (se, k) =>
             let
               fun k'' args [arg] = transBOMSimpleExp (se, env, k (arg::args))
             in
               k''
             end) k ses
           in
             case ses
               of [] => BOM.mkThrow (exncon, [])
                | se::rest => transBOMSimpleExp(se, env, k' [])
           end
         | BExp.Return ses => let
           fun k args [arg] = BOM.mkRet (arg::args)
           val k' = List.foldl (fn (se, k) =>
             let
               fun k'' args [arg] = transBOMSimpleExp (se, env, k (arg::args))
             in
               k''
             end) k ses
           in
             case ses
               of [] => BOM.mkRet []
                | se::rest => transBOMSimpleExp(se, env, k' [])
           end
      end

    and transBOMRhs (rhs, env): BOM.exp = case rhs
      of BExp.Composite e => transBOMExp (e, env)
       | BExp.Simple se => transBOMSimpleExp (se, env, BOM.mkRet)

    and transBOMPrim (prim, env, k: BOM.prim -> BOM.exp): BOM.exp = let
      fun extractSExps prim = case prim
        of Prim.I32Add (s1, s2) => [s1, s2]
         | Prim.I32Sub (s1, s2) => [s1, s2]
         | Prim.I32Mul (s1, s2) => [s1, s2]
         | Prim.I32Div (s1, s2) => [s1, s2]
         | Prim.I32Mod (s1, s2) => [s1, s2]
         | Prim.I32LSh (s1, s2) => [s1, s2]
         | Prim.I32Neg (s1) => [s1]
         | Prim.I64Add (s1, s2) => [s1, s2]
         | Prim.I64Sub (s1, s2) => [s1, s2]
         | Prim.I64Mul (s1, s2) => [s1, s2]
         | Prim.I64Div (s1, s2) => [s1, s2]
         | Prim.I64Mod (s1, s2) => [s1, s2]
         | Prim.I64LSh (s1, s2) => [s1, s2]
         | Prim.I64Neg (s1) => [s1]
         | Prim.U64Mul (s1, s2) => [s1, s2]
         | Prim.U64Div (s1, s2) => [s1, s2]
         | Prim.U64Rem (s1, s2) => [s1, s2]
         | Prim.F32Add (s1, s2) => [s1, s2]
         | Prim.F32Sub (s1, s2) => [s1, s2]
         | Prim.F32Mul (s1, s2) => [s1, s2]
         | Prim.F32Div (s1, s2) => [s1, s2]
         | Prim.F32Neg (s1) => [s1]
         | Prim.F32Sqrt (s1) => [s1]
         | Prim.F32Abs (s1) => [s1]
         | Prim.F64Add (s1, s2) => [s1, s2]
         | Prim.F64Sub (s1, s2) => [s1, s2]
         | Prim.F64Mul (s1, s2) => [s1, s2]
         | Prim.F64Div (s1, s2) => [s1, s2]
         | Prim.F64Neg (s1) => [s1]
         | Prim.F64Sqrt (s1) => [s1]
         | Prim.F64Abs (s1) => [s1]
       (* conversions *)
         | Prim.I32ToI64X (s1) => [s1]		(* int -> long conversion with sign extension *)
         | Prim.I32ToI64 (s1) => [s1]		(* unsigned int -> long conversion *)
         | Prim.I64ToI32 (s1) => [s1]                (* int -> long conversion *)
         | Prim.I32ToF32 (s1) => [s1]		(* int -> float conversion *)
         | Prim.I32ToF64 (s1) => [s1]		(* int -> double conversion *)
         | Prim.I64ToF32 (s1) => [s1]		(* long -> float conversion *)
         | Prim.I64ToF64 (s1) => [s1]		(* long -> double conversion *)
         | Prim.F64ToI32 (s1) => [s1]                (* double -> int conversion *)
       (* address arithmetic *)
         | Prim.AdrAddI32 (s1, s2) => [s1, s2]
         | Prim.AdrAddI64 (s1, s2) => [s1, s2]
         | Prim.AdrSubI32 (s1, s2) => [s1, s2]
         | Prim.AdrSubI64 (s1, s2) => [s1, s2]
       (* loads from addresses *)
         | Prim.AdrLoadI8 (s1) => [s1]
         | Prim.AdrLoadU8 (s1) => [s1]
         | Prim.AdrLoadI16 (s1) => [s1]
         | Prim.AdrLoadU16 (s1) => [s1]
         | Prim.AdrLoadI32 (s1) => [s1]
         | Prim.AdrLoadI64 (s1) => [s1]
         | Prim.AdrLoadF32 (s1) => [s1]
         | Prim.AdrLoadF64 (s1) => [s1]
         | Prim.AdrLoadAdr (s1) => [s1]
         | Prim.AdrLoad (s1) => [s1]                   (* load a uniform value from the given address *)
       (* stores to addresses *)
         | Prim.AdrStoreI8 (s1, s2) => [s1, s2]
         | Prim.AdrStoreI16 (s1, s2) => [s1, s2]
         | Prim.AdrStoreI32 (s1, s2) => [s1, s2]
         | Prim.AdrStoreI64 (s1, s2) => [s1, s2]
         | Prim.AdrStoreF32 (s1, s2) => [s1, s2]
         | Prim.AdrStoreF64 (s1, s2) => [s1, s2]
         | Prim.AdrStoreAdr (s1, s2) => [s1, s2]
         | Prim.AdrStore (s1, s2) => [s1, s2]           (* store a uniform value at the given address *)
       (* array load operations *)
         | Prim.ArrLoadI32 (s1, s2) => [s1, s2]
         | Prim.ArrLoadI64 (s1, s2) => [s1, s2]
         | Prim.ArrLoadF32 (s1, s2) => [s1, s2]
         | Prim.ArrLoadF64 (s1, s2) => [s1, s2]
         | Prim.ArrLoad (s1, s2) => [s1, s2]	(* load a uniform value *)
       (* array store operations *)
         | Prim.ArrStoreI32 (s1, s2, s3) => [s1, s2, s3]
         | Prim.ArrStoreI64 (s1, s2, s3) => [s1, s2, s3]
         | Prim.ArrStoreF32 (s1, s2, s3) => [s1, s2, s3]
         | Prim.ArrStoreF64 (s1, s2, s3) => [s1, s2, s3]
         | Prim.ArrStore (s1, s2, s3) => [s1, s2, s3] (* store a uniform value *)
       (* atomic operations *)
         | Prim.I32FetchAndAdd (s1, s2) => [s1, s2]
         | Prim.I64FetchAndAdd (s1, s2) => [s1, s2]
         | Prim.CAS (s1, s2, s3) => [s1, s2, s3]	(* compare and swap; returns old value *)
       (* memory-system operations *)
         | Prim.Pause => []				(* yield processor to allow memory operations to be seen *)
         | Prim.FenceRead => []			(* memory fence for reads *)
         | Prim.FenceWrite => []			(* memory fence for writes *)
         | Prim.FenceRW => []				(* memory fence for both reads and writes *)
       (* allocation primitives *)
         | Prim.AllocPolyVec (s1, s2) => [s1, s2]     (* AllocPolyVec (n, xs): allocate in the local heap a vector
					    * v of length n s.t. v[i] := l[i] for 0 <= i < n *)
         | Prim.AllocIntArray (s1) => [s1]           (* allocates an array of ints in the local heap *)
         | Prim.AllocLongArray (s1) => [s1]          (* allocates an array of longs in the local heap *)
         | Prim.AllocFloatArray (s1) => [s1]         (* allocates an array of floats in the local heap *)
         | Prim.AllocDoubleArray (s1) => [s1]        (* allocates an array of doubles in the local heap *)

       fun applyVars prim vars = case (prim, vars)
         of (Prim.I32Add _, [v1, v2]) => Prim.I32Add (v1, v2)
          | (Prim.I32Sub _, [v1, v2]) => Prim.I32Sub (v1, v2)
          | (Prim.I32Mul _, [v1, v2]) => Prim.I32Mul (v1, v2)
          | (Prim.I32Div _, [v1, v2]) => Prim.I32Div (v1, v2)
          | (Prim.I32Mod _, [v1, v2]) => Prim.I32Mod (v1, v2)
          | (Prim.I32LSh _, [v1, v2]) => Prim.I32LSh (v1, v2)
          | (Prim.I32Neg _, [v1]) => Prim.I32Neg (v1)
          | (Prim.I64Add _, [v1, v2]) => Prim.I64Add (v1, v2)
          | (Prim.I64Sub _, [v1, v2]) => Prim.I64Sub (v1, v2)
          | (Prim.I64Mul _, [v1, v2]) => Prim.I64Mul (v1, v2)
          | (Prim.I64Div _, [v1, v2]) => Prim.I64Div (v1, v2)
          | (Prim.I64Mod _, [v1, v2]) => Prim.I64Mod (v1, v2)
          | (Prim.I64LSh _, [v1, v2]) => Prim.I64LSh (v1, v2)
          | (Prim.I64Neg _, [v1]) => Prim.I64Neg (v1)
          | (Prim.U64Mul _, [v1, v2]) => Prim.U64Mul (v1, v2)
          | (Prim.U64Div _, [v1, v2]) => Prim.U64Div (v1, v2)
          | (Prim.U64Rem _, [v1, v2]) => Prim.U64Rem (v1, v2)
          | (Prim.F32Add _, [v1, v2]) => Prim.F32Add (v1, v2)
          | (Prim.F32Sub _, [v1, v2]) => Prim.F32Sub (v1, v2)
          | (Prim.F32Mul _, [v1, v2]) => Prim.F32Mul (v1, v2)
          | (Prim.F32Div _, [v1, v2]) => Prim.F32Div (v1, v2)
          | (Prim.F32Neg _, [v1]) => Prim.F32Neg (v1)
          | (Prim.F32Sqrt _, [v1]) => Prim.F32Sqrt (v1)
          | (Prim.F32Abs _, [v1]) => Prim.F32Abs (v1)
          | (Prim.F64Add _, [v1, v2]) => Prim.F64Add (v1, v2)
          | (Prim.F64Sub _, [v1, v2]) => Prim.F64Sub (v1, v2)
          | (Prim.F64Mul _, [v1, v2]) => Prim.F64Mul (v1, v2)
          | (Prim.F64Div _, [v1, v2]) => Prim.F64Div (v1, v2)
          | (Prim.F64Neg _, [v1]) => Prim.F64Neg (v1)
          | (Prim.F64Sqrt _, [v1]) => Prim.F64Sqrt (v1)
          | (Prim.F64Abs _, [v1]) => Prim.F64Abs (v1)
        (* conversions *)
          | (Prim.I32ToI64X _, [v1]) => Prim.I32ToI64X (v1)		(* int -> long conversion with sign extension *)
          | (Prim.I32ToI64 _, [v1]) => Prim.I32ToI64 (v1)		(* unsigned int -> long conversion *)
          | (Prim.I64ToI32 _, [v1]) => Prim.I64ToI32 (v1)                (* int -> long conversion *)
          | (Prim.I32ToF32 _, [v1]) => Prim.I32ToF32 (v1)		(* int -> float conversion *)
          | (Prim.I32ToF64 _, [v1]) => Prim.I32ToF64 (v1)		(* int -> double conversion *)
          | (Prim.I64ToF32 _, [v1]) => Prim.I64ToF32 (v1)		(* long -> float conversion *)
          | (Prim.I64ToF64 _, [v1]) => Prim.I64ToF64 (v1)		(* long -> double conversion *)
          | (Prim.F64ToI32 _, [v1]) => Prim.F64ToI32 (v1)                (* double -> int conversion *)
        (* address arithmetic *)
          | (Prim.AdrAddI32 _, [v1, v2]) => Prim.AdrAddI32 (v1, v2)
          | (Prim.AdrAddI64 _, [v1, v2]) => Prim.AdrAddI64 (v1, v2)
          | (Prim.AdrSubI32 _, [v1, v2]) => Prim.AdrSubI32 (v1, v2)
          | (Prim.AdrSubI64 _, [v1, v2]) => Prim.AdrSubI64 (v1, v2)
        (* loads from addresses *)
          | (Prim.AdrLoadI8 _, [v1]) => Prim.AdrLoadI8 (v1)
          | (Prim.AdrLoadU8 _, [v1]) => Prim.AdrLoadU8 (v1)
          | (Prim.AdrLoadI16 _, [v1]) => Prim.AdrLoadI16 (v1)
          | (Prim.AdrLoadU16 _, [v1]) => Prim.AdrLoadU16 (v1)
          | (Prim.AdrLoadI32 _, [v1]) => Prim.AdrLoadI32 (v1)
          | (Prim.AdrLoadI64 _, [v1]) => Prim.AdrLoadI64 (v1)
          | (Prim.AdrLoadF32 _, [v1]) => Prim.AdrLoadF32 (v1)
          | (Prim.AdrLoadF64 _, [v1]) => Prim.AdrLoadF64 (v1)
          | (Prim.AdrLoadAdr _, [v1]) => Prim.AdrLoadAdr (v1)
          | (Prim.AdrLoad _, [v1]) => Prim.AdrLoad (v1)                   (* load a uniform value from the given address *)
        (* stores to addresses *)
          | (Prim.AdrStoreI8 _, [v1, v2]) => Prim.AdrStoreI8 (v1, v2)
          | (Prim.AdrStoreI16 _, [v1, v2]) => Prim.AdrStoreI16 (v1, v2)
          | (Prim.AdrStoreI32 _, [v1, v2]) => Prim.AdrStoreI32 (v1, v2)
          | (Prim.AdrStoreI64 _, [v1, v2]) => Prim.AdrStoreI64 (v1, v2)
          | (Prim.AdrStoreF32 _, [v1, v2]) => Prim.AdrStoreF32 (v1, v2)
          | (Prim.AdrStoreF64 _, [v1, v2]) => Prim.AdrStoreF64 (v1, v2)
          | (Prim.AdrStoreAdr _, [v1, v2]) => Prim.AdrStoreAdr (v1, v2)
          | (Prim.AdrStore _, [v1, v2]) => Prim.AdrStore (v1, v2)           (* store a uniform value at the given address *)
        (* array load operations *)
          | (Prim.ArrLoadI32 _, [v1, v2]) => Prim.ArrLoadI32 (v1, v2)
          | (Prim.ArrLoadI64 _, [v1, v2]) => Prim.ArrLoadI64 (v1, v2)
          | (Prim.ArrLoadF32 _, [v1, v2]) => Prim.ArrLoadF32 (v1, v2)
          | (Prim.ArrLoadF64 _, [v1, v2]) => Prim.ArrLoadF64 (v1, v2)
          | (Prim.ArrLoad _, [v1, v2]) => Prim.ArrLoad (v1, v2)	(* load a uniform value *)
        (* array store operations *)
          | (Prim.ArrStoreI32 _, [v1, v2, v3]) => Prim.ArrStoreI32 (v1, v2, v3)
          | (Prim.ArrStoreI64 _, [v1, v2, v3]) => Prim.ArrStoreI64 (v1, v2, v3)
          | (Prim.ArrStoreF32 _, [v1, v2, v3]) => Prim.ArrStoreF32 (v1, v2, v3)
          | (Prim.ArrStoreF64 _, [v1, v2, v3]) => Prim.ArrStoreF64 (v1, v2, v3)
          | (Prim.ArrStore _, [v1, v2, v3]) => Prim.ArrStore (v1, v2, v3) (* store a uniform value *)
        (* atomic operations *)
          | (Prim.I32FetchAndAdd _, [v1, v2]) => Prim.I32FetchAndAdd (v1, v2)
          | (Prim.I64FetchAndAdd _, [v1, v2]) => Prim.I64FetchAndAdd (v1, v2)
          | (Prim.CAS _, [v1, v2, v3]) => Prim.CAS (v1, v2, v3)	(* compare and swap; returns old value *)
        (* memory-system operations *)
          | (Prim.Pause, []) => Prim.Pause				(* yield processor to allow memory operations to be seen *)
          | (Prim.FenceRead, []) => Prim.FenceRead			(* memory fence for reads *)
          | (Prim.FenceWrite, []) => Prim.FenceWrite			(* memory fence for writes *)
          | (Prim.FenceRW, []) => Prim.FenceRW				(* memory fence for both reads and writes *)
        (* allocation primitives *)
          | (Prim.AllocPolyVec _, [v1, v2]) => Prim.AllocPolyVec (v1, v2)     (* Prim.AllocPolyVec (n, xs): allocate in the local heap a vector
					     * v of length n s.t. v[i] := l[i] for 0 <= i < n *)
          | (Prim.AllocIntArray _, [v1]) => Prim.AllocIntArray (v1)           (* allocates an array of ints in the local heap *)
          | (Prim.AllocLongArray _, [v1]) => Prim.AllocLongArray (v1)          (* allocates an array of longs in the local heap *)
          | (Prim.AllocFloatArray _, [v1]) => Prim.AllocFloatArray (v1)         (* allocates an array of floats in the local heap *)
          | (Prim.AllocDoubleArray _, [v1]) => Prim.AllocDoubleArray (v1)        (* allocates an array of doubles in the local heap *)
          | (_, _) => raise Fail "internal error: prim applied to wrong number of arguments"

        val ses = extractSExps prim
        val prim = transBOMSimpleExps (ses, env, fn vars => k (applyVars prim vars), [])
      in
        prim
      end

    and transBOMCond (cond, env, k: BOM.cond -> BOM.exp): BOM.exp = let
      fun extractSExps cond = case cond
        of Prim.isBoxed (s1) => [s1]
         | Prim.isUnboxed (s1) => [s1]
         | Prim.I32isSet (s1) => [s1]
         | Prim.I32TAS (s1) => [s1]

         | Prim.Equal (s1, s2) => [s1, s2]
         | Prim.NotEqual (s1, s2) => [s1, s2]
         | Prim.EnumEq (s1, s2) => [s1, s2]
         | Prim.EnumNEq (s1, s2) => [s1, s2]
         | Prim.I32Eq (s1, s2) => [s1, s2]
         | Prim.I32NEq (s1, s2) => [s1, s2]
         | Prim.I32Lt (s1, s2) => [s1, s2]
         | Prim.I32Lte (s1, s2) => [s1, s2]
         | Prim.I32Gt (s1, s2) => [s1, s2]
         | Prim.I32Gte (s1, s2) => [s1, s2]
         | Prim.U32Lt (s1, s2) => [s1, s2]
         | Prim.I64Eq (s1, s2) => [s1, s2]
         | Prim.I64NEq (s1, s2) => [s1, s2]
         | Prim.I64Lt (s1, s2) => [s1, s2]
         | Prim.I64Lte (s1, s2) => [s1, s2]
         | Prim.I64Gt (s1, s2) => [s1, s2]
         | Prim.I64Gte (s1, s2) => [s1, s2]
         | Prim.U64Lt (s1, s2) => [s1, s2]
         | Prim.F32Eq (s1, s2) => [s1, s2]
         | Prim.F32NEq (s1, s2) => [s1, s2]
         | Prim.F32Lt (s1, s2) => [s1, s2]
         | Prim.F32Lte (s1, s2) => [s1, s2]
         | Prim.F32Gt (s1, s2) => [s1, s2]
         | Prim.F32Gte (s1, s2) => [s1, s2]
         | Prim.F64Eq (s1, s2) => [s1, s2]
         | Prim.F64NEq (s1, s2) => [s1, s2]
         | Prim.F64Lt (s1, s2) => [s1, s2]
         | Prim.F64Lte (s1, s2) => [s1, s2]
         | Prim.F64Gt (s1, s2) => [s1, s2]
         | Prim.F64Gte (s1, s2) => [s1, s2]
         | Prim.AdrEq (s1, s2) => [s1, s2]
         | Prim.AdrNEq (s1, s2) => [s1, s2]

         | Prim.BCAS (s1, s2, s3) => [s1, s2, s3]

       fun applyVars cond vars = case (cond, vars)
         of (Prim.isBoxed _, [s1]) => Prim.isBoxed (s1)
          | (Prim.isUnboxed _, [s1]) => Prim.isUnboxed (s1)
          | (Prim.I32isSet _, [s1]) => Prim.I32isSet (s1)
          | (Prim.I32TAS _, [s1]) => Prim.I32TAS (s1)

          | (Prim.Equal _, [s1, s2]) => Prim.Equal (s1, s2)
          | (Prim.NotEqual _, [s1, s2]) => Prim.NotEqual (s1, s2)
          | (Prim.EnumEq _, [s1, s2]) => Prim.EnumEq (s1, s2)
          | (Prim.EnumNEq _, [s1, s2]) => Prim.EnumNEq (s1, s2)
          | (Prim.I32Eq _, [s1, s2]) => Prim.I32Eq (s1, s2)
          | (Prim.I32NEq _, [s1, s2]) => Prim.I32NEq (s1, s2)
          | (Prim.I32Lt _, [s1, s2]) => Prim.I32Lt (s1, s2)
          | (Prim.I32Lte _, [s1, s2]) => Prim.I32Lte (s1, s2)
          | (Prim.I32Gt _, [s1, s2]) => Prim.I32Gt (s1, s2)
          | (Prim.I32Gte _, [s1, s2]) => Prim.I32Gte (s1, s2)
          | (Prim.U32Lt _, [s1, s2]) => Prim.U32Lt (s1, s2)
          | (Prim.I64Eq _, [s1, s2]) => Prim.I64Eq (s1, s2)
          | (Prim.I64NEq _, [s1, s2]) => Prim.I64NEq (s1, s2)
          | (Prim.I64Lt _, [s1, s2]) => Prim.I64Lt (s1, s2)
          | (Prim.I64Lte _, [s1, s2]) => Prim.I64Lte (s1, s2)
          | (Prim.I64Gt _, [s1, s2]) => Prim.I64Gt (s1, s2)
          | (Prim.I64Gte _, [s1, s2]) => Prim.I64Gte (s1, s2)
          | (Prim.U64Lt _, [s1, s2]) => Prim.U64Lt (s1, s2)
          | (Prim.F32Eq _, [s1, s2]) => Prim.F32Eq (s1, s2)
          | (Prim.F32NEq _, [s1, s2]) => Prim.F32NEq (s1, s2)
          | (Prim.F32Lt _, [s1, s2]) => Prim.F32Lt (s1, s2)
          | (Prim.F32Lte _, [s1, s2]) => Prim.F32Lte (s1, s2)
          | (Prim.F32Gt _, [s1, s2]) => Prim.F32Gt (s1, s2)
          | (Prim.F32Gte _, [s1, s2]) => Prim.F32Gte (s1, s2)
          | (Prim.F64Eq _, [s1, s2]) => Prim.F64Eq (s1, s2)
          | (Prim.F64NEq _, [s1, s2]) => Prim.F64NEq (s1, s2)
          | (Prim.F64Lt _, [s1, s2]) => Prim.F64Lt (s1, s2)
          | (Prim.F64Lte _, [s1, s2]) => Prim.F64Lte (s1, s2)
          | (Prim.F64Gt _, [s1, s2]) => Prim.F64Gt (s1, s2)
          | (Prim.F64Gte _, [s1, s2]) => Prim.F64Gte (s1, s2)
          | (Prim.AdrEq _, [s1, s2]) => Prim.AdrEq (s1, s2)
          | (Prim.AdrNEq _, [s1, s2]) => Prim.AdrNEq (s1, s2)

          | (Prim.BCAS _, [s1, s2, s3]) => Prim.BCAS (s1, s2, s3)
          | (_, _) => raise Fail "internal error: cond applied to wrong number of arguments"

        val ses = extractSExps cond
        val cond = transBOMSimpleExps (ses, env, fn vars => k (applyVars cond vars), [])
      in
        cond
      end

    (* TODO(wings): factor out varAccum argument *)
    (* TODO(wings): does env need to be threaded through here? *)
    and transBOMSimpleExps (ses, env, k: BOM.var list -> BOM.exp, varAccum): BOM.exp = case ses
        of [] => k varAccum
         | se::rest => transBOMSimpleExp (se, env, fn [var] => transBOMSimpleExps (rest, env, k, var::varAccum))

    and transBOMSimpleExp (se, env, k: BOM.var list -> BOM.exp): BOM.exp = let
      val (se, (env, ty)) = (BSExp.node se, transCoreBOMTy env (BSExp.typeOf se))
      fun transOffset x = x
      in
        case se
          of BSExp.PrimOp prim => let
               val newVar = [BV.new ("prim", ty)]
               in
                 transBOMPrim (prim, env, fn prim => BOM.mkStmt(newVar, BOM.E_Prim prim, k newVar))
               end
           | BSExp.HostVproc => let
               val newVar = [BV.new ("host_vproc", ty)]
               in
                 BOM.mkStmt (newVar, BOM.E_HostVProc, k newVar)
               end
           | BSExp.VpLoad (offset, se') => let
               val newVar = [BV.new ("vproc_load", ty)]
               val k' = fn [var_vp] => let
                 val vpField = transOffset offset
                 in
                   BOM.mkStmt (newVar, BOM.E_VPLoad (vpField, var_vp), k newVar)
                 end
               in
                 transBOMSimpleExp (se', env, k')
               end
           | BSExp.VpAddr (offset, se') => let
               val newVar = [BV.new ("vproc_addr", ty)]
               val k' = fn [var_vp] => let
                 val vpField = transOffset offset
                 in
                   BOM.mkStmt (newVar, BOM.E_VPAddr (vpField, var_vp), k newVar)
                 end
               in
                 transBOMSimpleExp (se', env, k')
               end
           | BSExp.VpStore (offset, se', se'') => let
               val newVar = [BV.new ("vproc_store", ty)]
               val k' = fn [var_vp] => let
                 val vpField = transOffset offset
                 val k'' = fn [var_val] =>
                   BOM.mkStmt (newVar, BOM.E_VPStore (vpField, var_vp, var_val), k newVar)
                 in
                   transBOMSimpleExp (se'', env, k'')
                 end
               in
                 transBOMSimpleExp (se', env, k')
               end
           | BSExp.AllocId (bomVal, se') => let
               (* TODO(wings): this is almost surely completely wrong *)
               val (newVar, env) = newVarBOMVal (env, bomVal)
               val k' = fn [var_tuple] =>
                 BOM.mkStmt ([newVar], BOM.E_Alloc (ty, [var_tuple]), k [newVar])
               in
                 transBOMSimpleExp (se', env, k')
               end
           | BSExp.RecAccess (offset, se', maybeSe'') => let
             val newVar = [BV.new ("rec_field", ty)]
             val field = transOffset offset
             val k' =
               case maybeSe''
                 of SOME storeSe =>
                   (fn [var_rec] => let
                     val k'' = fn [var_fieldval] =>
                       BOM.mkStmt (newVar,
                         BOM.E_Update (IntInf.toInt field, var_rec, var_fieldval), k newVar)
                     in
                       transBOMSimpleExp (storeSe, env, k'')
                     end)
                  | NONE =>
                   (fn [var_rec] =>
                     BOM.mkStmt (newVar,
                       BOM.E_Select (IntInf.toInt field, var_rec), k newVar))
             in
               transBOMSimpleExp (se', env, k')
             end
           | BSExp.Promote se' => let
               val newVar = [BV.new ("promoted", ty)]
               val k' = fn [var_topromote] =>
                 BOM.mkStmt (newVar, BOM.E_Promote (var_topromote), k newVar)
               in
                 transBOMSimpleExp (se', env, k')
               end
           | BSExp.TypeCast (bomTy, se') => raise Fail "typecast"
           | BSExp.MLString intVec => let
               val newVar = [BV.new ("str", ty)]
               val str = Literal.String (raise Fail "intVec")
               in
                 BOM.mkStmt (newVar, BOM.E_Const (str, ty), k newVar)
               end
           | BSExp.Lit lit => let
               val newVar = [BV.new ("lit", ty)]
               val const = transBOMLiteral (env, lit)
               in
                 BOM.mkStmt (newVar, BOM.E_Const const, k newVar)
               end
           | BSExp.Val bomVal => let
               val _ = print "s.exp.val bomval\n"
               in
                 case lookupBOMVal (env, bomVal)
                   of Lambda x => raise Fail "lambda"
                    | Var v => k [v]
                    | DCon con => let
                        val newVar = [BV.new ("dcon", ty)]
                        in
                          BOM.mkStmt (newVar, BOM.E_DCon(con, []), k newVar)
                        end
               end
     end

    and transBOMFuns (fundefs, env) = let
      val (env, fundef_vars) = List.foldl (fn (fundef, (env, fns)) => let
        val S.CoreBOM.FunDef.Def (attrs, func, domVals, contVals, retTys, body) =
          fundef
        fun convertTy (bomVal, (env, tys)) = let
          val (env, ty) = transCoreBOMTy env (S.CoreBOM.Val.typeOf bomVal)
          in
            (env, ty::tys)
          end
        val (env, domTys) = List.foldl convertTy (env, []) contVals
        val (env, contTys) = List.foldl convertTy (env, []) contVals
        val retTys = List.map (fn ty => #2 ((transCoreBOMTy env) ty)) retTys
        val funcTy = BOMTy.T_Fun (domTys, contTys, retTys)
        val (var', env) = newVarBOMValWithTy(env, func, funcTy)
        in
          (env, (fundef, var')::fns)
        end) (env, []) fundefs
      val lambdas = List.map (fn (fundef, var) => let
        val S.CoreBOM.FunDef.Def (attrs, func, domVals, contVals, retTys, body) =
          fundef
        (* TODO(wings): attrs? *)
        val (params, env) = newVarBOMVals (env, domVals)
        val (exh, env) = newVarBOMVals (env, contVals)
        in
          BOM.mkLambda {f=var, params=params, exh=exh, body=transBOMExp (body, env)}
        end
      ) fundef_vars
      in
        (lambdas, env)
      end

    and transBOMLiteral (env, lit) = (case S.CoreBOM.Literal.valOf lit
        of S.CoreBOM.Literal.Int n => Literal.Int n
         | S.CoreBOM.Literal.Float x => Literal.Float (mkFloat (Real.toString x))
         | S.CoreBOM.Literal.String s => Literal.String s
         | S.CoreBOM.Literal.NullVP => raise Fail "TODO(wings): translate NullVP literal",
       #2 (transCoreBOMTy env (S.CoreBOM.Literal.typeOf lit)))

    (* TODO(wings): verify that patterns get placed into environment properly *)
    and transBOMCaseRules (env, caserules): ((BOM.pat * BOM.exp) list * BOM.exp option) = let
        val def = ref NONE
        fun transRule (rule, rest) =
          case rule
            of S.CoreBOM.CaseRule.LongRule (conVal, argVals, e) => let
                 val (argVars, env) = newVarBOMVals (env, argVals)
                 val DCon con = lookupBOMVal (env, conVal)
                 in
                   (BOM.P_DCon(con, argVars), transBOMExp(e, env))::rest
                 end
             | S.CoreBOM.CaseRule.LiteralRule (lit, e) => let
                 val const = transBOMLiteral (env, lit)
                 in
                   (BOM.P_Const const, transBOMExp(e, env))::rest
                 end
             | S.CoreBOM.CaseRule.DefaultRule (argVal, e) => (case !def
               of NONE => (def := SOME (let
                  val (var_, env) = newVarBOMVal (env, argVal)
                  in
                    transBOMExp (e, env)
                  end); rest)
                | SOME _ => raise Fail "multiple defaults in BOM case statement")
        val pats_exps = List.foldl transRule [] caserules
      in
        (pats_exps, !def)
      end

  (***** Translation functions *****)

(*
    datatypes: {cons: {arg: Type.t option,
                    con: Con.t} vector,
             tycon: Tycon.t,
             tyvars: Tyvar.t vector} vector,
*)
    fun transTy (env as E{tyCache, tycMap, varCache, varEnv, bomValMap, binds, exh}, ty (*as {tycon, tyvars, cons}*)) =
    let
          (*val _ = print (concat["SXML.Type->BOMTy: ", Layout.toString (S.Type.layout ty), "\n"])*)
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
                          then BOMTy.arrayTy (singleArg ())
                        else if S.Tycon.equals(tyc, S.Tycon.bool)
                          then (noArgs (); BOMTy.T_Con (boolTyc, []))
                        else if S.Tycon.equals(tyc, S.Tycon.cpointer)
                          then BOMTy.addrTy (singleArg ())
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
                          then BOMTy.arrayTy (singleArg()) (* XXX(wings): should we really treat refs as one-item arrays? *)
                        else if S.Tycon.equals(tyc, S.Tycon.thread)
                          then (noArgs (); BOMTy.vprocTy)
                        else if S.Tycon.equals(tyc, S.Tycon.tuple)
                          then BOMTy.tupleTy args'
                        else if S.Tycon.equals(tyc, S.Tycon.vector)
                          then BOMTy.vectorTy (singleArg ())
                        else if S.Tycon.equals(tyc, S.Tycon.weak)
                          then raise Fail "Weak pointer type found in SXML"
                        else BOMTy.T_Con(TycMap.lookup(tycMap, tyc), args')
                        (*else raise Fail (concat["Failed to translate SXML tycon ", S.Type.Tycon.toString tyc, " with ", Int.toString nArgs," arguments"])*)
                    in
                      (cacheTy (tyCache', ty, ty'), ty')
                    end
              (* end case *))
          in (case peekTy (tyCache, ty)
                 of SOME ty' => (env, ty')
                  | NONE => let
                      val (tyCache', ty') = tr (tyCache, S.Type.dest ty)
                    in
                      (E{tyCache=tyCache', tycMap=tycMap, varCache=varCache, varEnv=varEnv, bomValMap=bomValMap, binds=binds, exh=exh}, ty')
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
      val x' = BV.new (S.Var.toString x ^ ".", ty')
      val env' = writeVar (env, x, x') (* construct explicit map from x to x' so x can be used to look up x' and x' can be used to look up the type *)
      in
        (x', writeVarTy (env', x', ty'))
      end

    fun transSExp (e, env) : BOM.exp = let
          val {decs, result} = S.Exp.dest e
          fun transDecs (env, decs, result as S.VarExp.T{targs, var}) =
             (case decs of
                [] => (*lookupBind(env,*) BOM.mkRet [lookupVar(env, var)] (*)*)
                | d::rest => transDec (env, d, fn (env) => ((*print "k called\n";*) transDecs(env, rest, result)))
             (* end case *))
          in
            transDecs(env, decs, result)
          end

    and transDec (env, d, k : env -> BOM.exp) = ( case d
           of S.Dec.Exception{arg, con} => raise Fail "Exception declaration found in SXML"
            | S.Dec.Fun{decs, ...} => let
                (*val _ = print "transDec Fun\n"*)
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
                (*val _ = print "transDec MonoVal\n"*)
                val (var', env) = newVar(env, var, ty)
                in
                  transPrimExp (env, var', ty, exp, k)
                end
            | S.Dec.BOM{bom=bomdecs} => let
                (*val _ = print "transDec BOM\n"*)
                val (env, k) = Vector.foldl (fn (bomdec, (env, k: env -> BOM.exp)) => (case bomdec
                   of S.CoreBOM.Definition.Datatype dataTypeDef => (env, k)
                    | S.CoreBOM.Definition.HLOp (attrs, valid, exp) => raise Fail "Polymorphic HLOps found in SXML"
                    | S.CoreBOM.Definition.Fun fundefs => let
                       val (lambdas, env) = transBOMFuns (fundefs, env)
                       in
                         (env, fn env' => BOM.mkFun(lambdas, k env'))
                       end
                    | S.CoreBOM.Definition.Extern (bomVal, S.CoreBOM.CProto.T (cfun, attrs)) => let
                       (* translate the C function; place in env and global externs *)
                       val (cfun, env') = transCFun (S.CoreBOM.CProto.T (cfun, attrs), env)
                       (* references to cfuns elsewhere in the program are via BOMVal,
                       so put a mapping from it to the cfun's var into the env *)
                       val env' = writeBOMVal (env', bomVal, Var (CFunctions.varOf cfun))
                       in
                         progExterns := cfun :: !progExterns; (env', k)
                     end
                  (* end case *))) (env, k) bomdecs
                in
                  k env
                end
            | S.Dec.PolyVal _ => raise Fail "Polymorphic declaration found in SXML"
          (* end case *))

    (* translate a C function, put it in the global list of externs, return it and
    a modified environment in which it's bound *)
    and transCFun (S.CoreBOM.CProto.T (cfun as S.CoreBOM.CFunction.T {convention, kind, target, prototype, ...}, attrs), env) = let
          val name = (case target
             of S.CoreBOM.CFunction.Target.Direct name => name
              | S.CoreBOM.CFunction.Target.Indirect => raise Fail "indirect function encountered"
            (* end case *))

          (* build the appropriate BOM type and make a variable with it to place in the env *)
          val (ty, retTy', argTys', attrs') = transCFunTy env (S.CoreBOM.CProto.T(cfun, attrs))
          val var = BV.new ("extern " ^ name, ty)
          val env' = writeVarTy (env, var, ty) (* construct explicit map from x to x' so x can be used to look up x' and x' can be used to look up the type *)
          val cfun = BOM.mkCFun { var = var, name = name, retTy = retTy',
                                  argTys = argTys', varArg = false, attrs = attrs' }
          in
            (cfun, env')
          end

    and transLambda env (f, lambda) = let
          val {arg, argType, body, ...} = S.Lambda.dest lambda
          val (param, env) = newVar(env, arg, argType)
          val (exh, env) = newHandler env
          in
            BOM.mkLambda{f = f, params = [param], exh = [exh], body = transSExp(body, env)}
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

    (* TODO(wings): I have no idea how to make this work when maybeSxmlty is NONE, because conTycon seems to fail consistently on bool... *)
    and transDCon (env as E{tyCache, tycMap, varCache, varEnv, bomValMap, binds, exh}, con, maybeSxmlty, targs) : BOMDataCon.t = let
        (*val boolCons = PMLFrontEnd.tyconCons (S.Tycon.bool)*)
        val dest: S.Type.dest = case maybeSxmlty
          of NONE => S.Type.Con (PMLFrontEnd.conTycon con, targs)
           | SOME sxmlty => S.Type.dest sxmlty
        val tyName = case maybeSxmlty
          of NONE => "<unknown type>"
           | SOME sxmlty => Layout.toString (S.Type.layout sxmlty)
        val _ = print ("transDcon for ty " ^ tyName ^ "\n")
        val tycon = case maybeSxmlty
          of NONE => PMLFrontEnd.conTycon con
           | SOME sxmlty => S.Type.tycon sxmlty
        val bomTy = if S.Tycon.equals (tycon, S.Tycon.bool) then BOMTy.T_Con(boolTyc, []) else lookupTyDest (tyCache, dest)
        val (name, dcons) = case bomTy
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
    and transPrimExp (env, lhs, sxmlty, e, k : env -> BOM.exp) : BOM.exp = let
          fun mkLet e' = BOM.mkLet([lhs], e', k env)
          fun mkStmt rhs = BOM.mkStmt([lhs], rhs, k env)
          val (env, _) = transTy(env, sxmlty)
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
                  val default' = Option.map (fn (e, region) => transSExp(e, env)) default
                  val cases' = (case cases
                        (* for cases on inductive datatypes, ... *)
                         of S.Cases.Con (rules) => V.map (fn (pat as S.Pat.T{arg, con, targs}, exp) => let
                                 (*Pat.t = T of {
                                        arg: (Var.t * Type.t) option,
                                        con: Con.t,
                                        targs: Type.t vector
                                }*)
                                  val dcon=transDCon (env, con, NONE, targs)
                                  val vars=(case arg
                                    of NONE => []
                                     | SOME (var, sxmlty) => [lookupVar(env, var)] (* XXX(wings): unpack tuple into multiple vars? *)
                                   (* end case *))
                                in
                                  (* P_DCon of data_con * var list *)
                                  (BOM.P_DCon (dcon, vars), transSExp(exp, env))
                                end) rules
                        (* for cases on int, word, char, ... *)
                          | S.Cases.Word (sz, rules) => V.map (fn (word, exp) => (BOM.P_Const(transConst(sz, word)), transSExp(exp, env))) rules
                        (* end case *))
                  in
                    mkLet(BOM.mkCase(test', V.foldl (fn(x, y) => x::y) [] cases', default'))
                  end
                (* QUESTION: are there primitive dcons that have targs?  No! *)
              | S.PrimExp.ConApp{con, arg, targs} => let
                  val con' = transDCon (env, con, SOME sxmlty, targs)
                  in
                    (* translate dcon and tupled args *)
                    mkStmt(BOM.E_DCon(con', case arg
                       of SOME arg' => (* XXX(wings): unpack typaram tuple into its own list? *) [transVarExp(env, arg')]
                        | NONE => []))
                  end
              | S.PrimExp.Const c => let
                  val lit = (case c
                         of S.Const.IntInf i => Literal.Int i
                          | S.Const.Null => Literal.StateVal 0w0 (* XXX(wings): there is no Addr literal but it seems like genLit in codegen-fn.sml will treat this properly *)
                          | S.Const.Real flt => Literal.Float (mkFloat (S.Atoms.RealX.toString flt))
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
                      BOM.mkLambda{f = exh, params = [x'], exh = [], body = transSExp(handler, handlerEnv)},
                      transSExp(try, tryEnv)))
                  end
              | S.PrimExp.Lambda lambda => BOM.mkFun([transLambda env (lhs, lambda)], k env)
                (* XXX: These could be BOM HLOps? *)
              | S.PrimExp.PrimApp{prim, targs, args} => let
                  fun expFromRhs (rhs, ret) = let
                      val var = BV.new("rhs", ret)
                    in
                      BOM.mkStmt([var], rhs, BOM.mkRet[var])
                  end
                val args = V.foldl (fn (argi, rest) => transVarExp(env,argi)::rest) [] args
                fun argn(n) = List.nth(args, n)
                val unitTy = BOMTy.unitTy
                val boolTy = BOMTy.T_Con(boolTyc, [])
                fun r2e (rhs, ty): BOM.exp = mkLet(expFromRhs (rhs, ty))
                val exp =
                  if S.Prim.equals (prim, S.Prim.array) then raise Fail "array" else
                  if S.Prim.equals (prim, S.Prim.arrayLength) then raise Fail "arrayLength" else
                  if S.Prim.equals (prim, S.Prim.assign) then r2e (BOM.E_Prim (P.AdrStore(argn 0, argn 1)), unitTy) else
                  if S.Prim.equals (prim, S.Prim.bogus) then raise Fail "bogus" else
                  if S.Prim.equals (prim, S.Prim.bug) then r2e (BOM.E_CCall(BV.new("_ccall_bug", unitTy), []), unitTy) else
                  if S.Prim.equals (prim, S.Prim.deref) then r2e (BOM.E_Prim (P.AdrLoad (argn 0)), BV.typeOf (argn 0)) else
                  if S.Prim.equals (prim, S.Prim.reff) then r2e (BOM.E_Alloc (BV.typeOf (argn 0), [argn 0]), BOMTy.arrayTy(BV.typeOf (argn 0))) else

                  if S.Prim.equals (prim, S.Prim.equal) then BOM.mkIf (Prim.Equal (argn 0, argn 1),
                                                                        expFromRhs (BOM.E_Const (Literal.trueLit, boolTy), boolTy),
                                                                        expFromRhs (BOM.E_Const (Literal.falseLit, boolTy), boolTy)) else

                  if S.Prim.equals (prim, S.Prim.touch) then r2e (BOM.E_Promote (argn 0), unitTy) (* TODO: the backend needs a notion of 'touch' *) else
                  (*raise Fail "ref" else *)
                  if S.Prim.equals (prim, S.Prim.cpointerSub) then raise Fail "cpointerSub" else
                raise Fail ("failed to translate PrimApp " ^ S.Prim.toString prim)
                in
                  exp
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
              | S.PrimExp.BOMVal bomVal => case lookupBOMVal(env, bomVal)
                   of DCon con => mkStmt(BOM.E_DCon(con, []))
                    | Var v => mkLet(BOM.mkRet[v])
                    | Lambda lam => raise Fail "S.PrimExpo.BOMVal lambda"
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
            val () = print ("transDatatype " ^ S.Tycon.toString tycon ^ " '" ^ Int.toString (V.length tyvars) ^ "\n")
        (* XXX(wings): remove nullary dcon count *)
        fun isNullary {arg, con} = not (isSome arg)
        val nNullary = V.foldl (fn (elem, n) => n + (if isNullary elem then 1 else 0)) 0 cons
        val dataTyc = BOMTyc.new(S.Tycon.toString tycon, V.length tyvars)
        val E{tyCache, tycMap, varCache, varEnv, bomValMap, binds, exh} = env;
        val tyCache' = cacheTyDest(tyCache, S.Type.Con(tycon, V.fromList []), BOMTy.T_Con(dataTyc, [](* TODO(wings): convert these lazily so we can handle type arguments instead of assuming none! *)))
        val env = E{tyCache=tyCache', tycMap=TycMap.insert(tycMap, tycon, dataTyc), varCache=varCache, varEnv=varEnv, bomValMap=bomValMap, binds=binds, exh=exh}
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
          val env = E{tyCache = TyCache.empty, tycMap=TycMap.empty, varCache = VarCache.empty, varEnv = VMap.empty, bomValMap=BOMValMap.empty, binds = VMap.empty, exh = BV.new("_exh", exhTy0) }
          (* initially translate all the datatypes listed by MLton *)
          val env as E{tycMap=tycMap, ...} = V.foldl transDatatype env datatypes
          val body = transSExp (body, env)
          (* val argTy = BOMTy.T_Raw RawTypes.Int32 *)
          (* val arg = BV.new("_arg", argTy) *)
          val mainFun = BOM.FB{
                  f = BV.new("main", BOMTy.T_Fun([(* argTy *)], [(* BOMTy.exhTy *)], [(* TypeOf.exp body *)])),
                  params = [(* arg *)],
                  exh = [(* exh *)],
                  body = body
                }
          val program = BOM.PROGRAM{exnTyc = exnTyc, dataTycs = TycMap.listItems tycMap, hlops = [], externs = !progExterns, body = mainFun}
          in(* raise Fail "hi";*)
            PrintBOM.print (program); program (*raise Fail "done"*)
          end
        end
