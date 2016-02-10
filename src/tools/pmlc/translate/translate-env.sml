    structure S = PMLFrontEnd.Sxml
    structure V = Vector
    structure BV = BOMVar
    structure P = Prim
    structure BExp = S.CoreBOM.Exp
    structure BSExp = S.CoreBOM.SimpleExp
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
    type tycache_t = BOMTy.t TyCache.map

    structure TycMapKV = struct
        type ord_key = S.Tycon.t
        fun compare(tyc1: S.Tycon.t, tyc2: S.Tycon.t) = Word.compare (S.Tycon.hash tyc1, S.Tycon.hash tyc2)
    end
    structure TycMap = RedBlackMapFn (TycMapKV)
    type tycmap_t = BOMTyc.t TycMap.map

    structure MLTycMapKV = struct
        type ord_key = S.CoreBOM.TyCon.t
        fun compare(tyc1: S.CoreBOM.TyCon.t, tyc2: S.CoreBOM.TyCon.t) = S.CoreBOM.TyCon.compare (tyc1, tyc2)
    end
    structure MLTycMap = RedBlackMapFn (MLTycMapKV)
    type shadowtycmap_t = (S.Tycon.t * S.PrimConDef.t vector) MLTycMap.map

    structure DConMapKV = struct
        type ord_key = S.Con.t
        fun compare(con1: S.Con.t, con2: S.Con.t) = Word.compare (S.Con.hash con1, S.Con.hash con2)
    end
    structure DConMap = RedBlackMapFn (DConMapKV)
    type dconmap_t = BOM.data_con DConMap.map

    structure VarCacheKV = struct
        type ord_key = (S.Var.t)
        fun compare(v1, v2) = Word.compare (S.Var.hash v1, S.Var.hash v2)
    end
    structure VarCache = RedBlackMapFn (VarCacheKV)
    type varcache_t = BOM.var VarCache.map

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
    type bomvalmap_t = var_bind BOMValMap.map

  (***** Translation environment *****)

    datatype env = E of {
        tyCache: tycache_t, (* BOM types for SXML types *)
        tycMap: tycmap_t, (* BOM tycs for SXML tycons *)
        varCache: varcache_t, (* BOM variables for SXML variables *)
        varEnv: BOMTy.t VMap.map, (* BOM types of BOM variables *)
        bomValMap: bomvalmap_t, (* BOM variables' corresponding CoreBOM values *)
        dconMap: dconmap_t, (* BOM data cons for SXML data cons *)
        shadowTycs: shadowtycmap_t, (* "shadow" ML datatypes for CoreBOM tycons *)
        exh: BOM.var (* current exception handler continuation *)
      }

    fun writeTyDest (E{
      tyCache,
      tycMap,
      varCache,
      varEnv,
      bomValMap,
      dconMap,
      shadowTycs,
      exh
    }, sxmltyd, bomty) =
    E{
      tyCache=TyCache.insert(tyCache, sxmltyd, bomty),
      tycMap=tycMap,
      varCache=varCache,
      varEnv=varEnv,
      bomValMap=bomValMap,
      dconMap=dconMap,
      shadowTycs=shadowTycs,
      exh=exh
    }
    fun writeTy (env, sxmlty, bomty) = writeTyDest(env, S.Type.dest sxmlty, bomty)

    fun peekTyDest (E{tyCache, ...}, sxmltyd) = TyCache.find(tyCache, sxmltyd)
    fun peekTy (env, sxmlty) = peekTyDest (env, S.Type.dest sxmlty)

    fun lookupTyDest (env, sxmltyd) =
      case peekTyDest(env, sxmltyd)
        of SOME bomty => bomty
         | NONE => raise Fail(concat["lookupTyDest(env, <S.Type.dest>) = NONE"])
    fun lookupTy (env, sxmlty) =
      case peekTyDest(env, S.Type.dest sxmlty)
        of SOME bomty => bomty
         | NONE => raise Fail(concat["lookupTy(env, ", Layout.toString (S.Type.layout sxmlty), ") = NONE"])

    fun newEnv exh = E{tyCache = TyCache.empty, tycMap=TycMap.empty, varCache = VarCache.empty, varEnv = VMap.empty, bomValMap=BOMValMap.empty, dconMap=DConMap.empty, shadowTycs=MLTycMap.empty, exh=exh}

    fun handlerOf (E{exh, ...}) = exh

    (* extract the exception type constructor, of which there's hopefully exactly one *)
    fun exnTyc (env as E{tycMap=tycMap, ...}) = let
        val [exnTyc] = TycMap.listItems (TycMap.mapPartiali (fn (tyc, bomTyc) => if S.Tycon.equals(tyc, S.Tycon.exn) then SOME bomTyc else NONE) tycMap)
      in
        exnTyc
      end
    fun exnTy (env) = BOMTy.T_Con (exnTyc(env), [])
    fun exhTy (env) = BOMTy.T_Cont [exnTy(env)]

    fun newHandler (env as E{
      tyCache,
      tycMap,
      varCache,
      varEnv,
      bomValMap,
      dconMap,
      shadowTycs,
      exh
    }) =
    let
      val newExh = BV.new("_exh", exhTy (env))
      val env = E{
        tyCache=tyCache,
        tycMap=tycMap,
        varCache=varCache,
        varEnv=varEnv,
        bomValMap=bomValMap,
        dconMap=dconMap,
        shadowTycs=shadowTycs,
        exh=newExh
      }
    in
      (newExh, env)
    end

    fun writeVarTy (E{
      tyCache,
      tycMap,
      varCache,
      varEnv,
      bomValMap,
      dconMap,
      shadowTycs,
      exh
    }, var, ty) = 
    E{
      tyCache=tyCache,
      tycMap=tycMap,
      varCache=varCache,
      varEnv=VMap.insert(varEnv, var, ty),
      bomValMap=bomValMap,
      dconMap=dconMap,
      shadowTycs=shadowTycs,
      exh=exh
    }
    fun lookupVarTy (E{varEnv, ...}, bomVar) = (case VMap.find(varEnv, bomVar)
           of SOME bomty => bomty
            | NONE => raise Fail(concat["lookupVarTy(env, ", BV.toString bomVar, ") = NONE"])
          (* end case *))

    fun writeVar (E{
      tyCache,
      tycMap,
      varCache,
      varEnv,
      bomValMap,
      dconMap,
      shadowTycs,
      exh
    }, sxmlVar, bomVar) = 
    E{
      tyCache=tyCache,
      tycMap=tycMap,
      varCache=VarCache.insert(varCache, sxmlVar, bomVar),
      varEnv=varEnv,
      bomValMap=bomValMap,
      dconMap=dconMap,
      shadowTycs=shadowTycs,
      exh=exh
    }
    fun lookupVar (E{varCache, ...}, sxmlVar) =
      case VarCache.find(varCache, sxmlVar)
        of SOME bomVar => bomVar
         | NONE => raise Fail(concat["lookupVar(env, ", Layout.toString (S.Var.layout sxmlVar), ") = NONE"])

    fun writeTyc (E{
      tyCache,
      tycMap,
      varCache,
      varEnv,
      bomValMap,
      dconMap,
      shadowTycs,
      exh
    }, tyc, bomTyc) = 
    E{
      tyCache=tyCache,
      tycMap=TycMap.insert(tycMap, tyc, bomTyc),
      varCache=varCache,
      varEnv=varEnv,
      bomValMap=bomValMap,
      dconMap=dconMap,
      shadowTycs=shadowTycs,
      exh=exh
    }
    fun lookupTyc(E{tycMap, ...}, tyc) = (case TycMap.find(tycMap, tyc)
          of SOME bomTyc => bomTyc
           | NONE => raise Fail (concat["lookupTyc(", S.Tycon.toString tyc, ") = NONE"])
         (* end case *))

    fun printTycs (E{tycMap, ...}) = print ("tycs: "
      ^ String.concatWith " " (List.map (fn (tyc) =>
        Layout.toString (S.Tycon.layout tyc)
        ^ ":hash=0x"
        ^ Word.toString (S.Tycon.hash tyc))
          (TycMap.listKeys tycMap))
      ^ "\n")

    fun writeBOMVal (E{
      tyCache,
      tycMap,
      varCache,
      varEnv,
      bomValMap,
      dconMap,
      shadowTycs,
      exh
    }, bomVal, bomVarBind) = 
    E{
      tyCache=tyCache,
      tycMap=tycMap,
      varCache=varCache,
      varEnv=varEnv,
      bomValMap=BOMValMap.insert(bomValMap, bomVal, bomVarBind),
      dconMap=dconMap,
      shadowTycs=shadowTycs,
      exh=exh
    }
    fun lookupBOMVal (E{bomValMap, ...}, bomVal : S.CoreBOM.Val.t) : var_bind =
      case BOMValMap.find(bomValMap, bomVal)
        of SOME bomVarBind => bomVarBind
         | NONE => raise Fail(concat["lookupBOMVal(env, ",
             S.CoreBOM.ValId.toString (S.CoreBOM.Val.idOf bomVal), ") = NONE"])

    fun writeDCon (E{
      tyCache,
      tycMap,
      varCache,
      varEnv,
      bomValMap,
      dconMap,
      shadowTycs,
      exh
    }, mlDCon, bomDCon) =
    E{
      tyCache=tyCache,
      tycMap=tycMap,
      varCache=varCache,
      varEnv=varEnv,
      bomValMap=bomValMap,
      dconMap=DConMap.insert(dconMap, mlDCon, bomDCon),
      shadowTycs=shadowTycs,
      exh=exh
    }
    fun lookupDCon (E{dconMap, ...}, mlDCon) =
      case DConMap.find(dconMap, mlDCon)
        of SOME bomDCon => bomDCon
         | NONE => raise Fail(concat["lookupDCon(env, ", S.Con.toString mlDCon, ") = NONE"])

    fun writeMLTyc (E{
      tyCache,
      tycMap,
      varCache,
      varEnv,
      bomValMap,
      dconMap,
      shadowTycs,
      exh
    }, bomTyc, def) = 
    E{
      tyCache=tyCache,
      tycMap=tycMap,
      varCache=varCache,
      varEnv=varEnv,
      bomValMap=bomValMap,
      dconMap=dconMap,
      shadowTycs=MLTycMap.insert(shadowTycs, bomTyc, def),
      exh=exh
    }
    fun lookupMLTyc (E{shadowTycs, ...}, bomTyc): (S.Tycon.t * S.PrimConDef.t vector) =
      case MLTycMap.find(shadowTycs, bomTyc)
        of SOME def => def
         | NONE => raise Fail(concat["lookupMLTyc(env, ", Layout.toString (S.CoreBOM.TyCon.layout (bomTyc)), ") = NONE"])

    fun printMLTycs (E{shadowTycs, ...}) = print ("ml tycs: "
      ^ String.concatWith " " (List.map (fn (mlTyc, _) =>
        S.Tycon.toString mlTyc ^ ":id=0x" ^ Word.toString (S.Tycon.hash mlTyc))
        (MLTycMap.listItems shadowTycs))
      ^ "\n")

    fun newVarBOMValWithTy (env, bomVal, ty) = let
      val var = BV.new (S.CoreBOM.ValId.toString
        (S.CoreBOM.Val.idOf bomVal) ^ ".", ty)
      val env' = writeVarTy (env, var, ty)
      in
        (var, writeBOMVal (env', bomVal, Var var))
      end
