(* translate-env.sml
 *
 * COPYRIGHT (c) 2016 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure Env : sig

    type env

    datatype var_bind
      = Lambda of (BOMTy.t -> BOM.lambda) (* used for primops and high-level ops *)
      | Var of BOMVar.t
      | DCon of BOMDataCon.t
      (*| EqOp                        (* either "=" or "<>" *)*)

  (* create a new environment.  The variable is the current exception handler continuation *)
    val newEnv : BOMVar.t -> env

    val exnTyc : env -> BOMTyc.t
    val exnTy : env -> BOMTy.t
    val exhTy : env -> BOMTy.t

  (* return the current exn handler *)
    val handlerOf : env -> BOMVar.t

    val newHandler : env -> BOMVar.t * env

    val bindTyDest : env * PMLFrontEnd.Sxml.Type.dest * BOMTy.t -> env

    val bindTy : env * PMLFrontEnd.Sxml.Type.t * BOMTy.t -> env

    val peekTy : env * PMLFrontEnd.Sxml.Type.t -> BOMTy.t option

    val bindMLTyc : env * PMLFrontEnd.Sxml.CoreBOM.TyCon.t * (PMLFrontEnd.Sxml.Tycon.t * PMLFrontEnd.Sxml.PrimConDef.t vector)
	  -> env

    val lookupMLTyc : env * PMLFrontEnd.Sxml.CoreBOM.TyCon.t
	  -> (PMLFrontEnd.Sxml.Tycon.t * PMLFrontEnd.Sxml.PrimConDef.t vector)

    val bindTyc : env * PMLFrontEnd.Sxml.Tycon.t * BOMTyc.t -> env

    val lookupTyc : env * PMLFrontEnd.Sxml.Tycon.t -> BOMTyc.t

  (* return a list of the BOM type constructors in the environment (which will be datatypes) *)
    val listTycs : env -> BOMTyc.t list

    val bindDCon : env * PMLFrontEnd.Sxml.Con.t * BOMDataCon.t -> env

    val lookupDCon : env * PMLFrontEnd.Sxml.Con.t -> BOMDataCon.t

    val bindVar : env * PMLFrontEnd.Sxml.Var.t * BOMVar.t -> env

    val bindVarTy : env * BOMVar.t * BOMTy.t -> env

    val lookupVar : env * PMLFrontEnd.Sxml.Var.t -> BOMVar.t

    val bindBOMVal : env * PMLFrontEnd.Sxml.CoreBOM.Val.t * var_bind -> env

    val lookupBOMVal : env * PMLFrontEnd.Sxml.CoreBOM.Val.t -> var_bind

    val newVarBOMValWithTy : env * PMLFrontEnd.Sxml.CoreBOM.Val.t * BOMTy.t -> BOMVar.t * env

  (* debugging support *)
    val printTycs : env -> unit

  end = struct

    structure S = PMLFrontEnd.Sxml
    structure V = Vector
    structure BV = BOMVar
    structure P = Prim
    structure BExp = S.CoreBOM.Exp
    structure BSExp = S.CoreBOM.SimpleExp
    structure VMap = BV.Map

    structure TyCacheKV =
      struct
        type ord_key = S.Type.dest
        fun compare (t1: S.Type.dest, t2: S.Type.dest) = (case (t1, t2)
               of (S.Type.Con (ltc1, tys1), S.Type.Con (ltc2, tys2)) => (
		    case Word.compare (S.Tycon.hash ltc1, S.Tycon.hash ltc2)
		      of EQUAL => V.collate compare (V.map S.Type.dest tys1, V.map S.Type.dest tys2)
		       | relation => relation
		     (* end case *))
		| (S.Type.Con _, S.Type.Var _) => LESS
		| (S.Type.Var v1, S.Type.Var v2) =>
		    String.compare (S.Tyvar.toString v1, S.Tyvar.toString v2)
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
        type ord_key = S.Var.t
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
        type ord_key = S.CoreBOM.Val.t
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

  (* return a list of the BOM type constructors in the environment (which will be datatypes) *)
    fun listTycs (E{tycMap, ...}) = TycMap.listItems tycMap

    fun bindTyDest (env, sxmltyd, bomty) = let
	  val E{tyCache, tycMap, varCache, varEnv, bomValMap, dconMap, shadowTycs, exh} = env
	  in
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
	  end
    fun bindTy (env, sxmlty, bomty) = bindTyDest(env, S.Type.dest sxmlty, bomty)

    fun peekTyDest (E{tyCache, ...}, sxmltyd) = TyCache.find(tyCache, sxmltyd)
    fun peekTy (env, sxmlty) = peekTyDest (env, S.Type.dest sxmlty)

    fun lookupTyDest (env, sxmltyd) = (case peekTyDest(env, sxmltyd)
	   of SOME bomty => bomty
	    | NONE => raise Fail(concat["lookupTyDest(env, <S.Type.dest>) = NONE"])
	  (* end case *))
    fun lookupTy (env, sxmlty) = (case peekTyDest(env, S.Type.dest sxmlty)
	   of SOME bomty => bomty
	    | NONE => raise Fail(concat["lookupTy(env, ", Layout.toString (S.Type.layout sxmlty), ") = NONE"])
	  (* end case *))

    fun newEnv exh = E{
	    tyCache = TyCache.empty,
	    tycMap=TycMap.empty,
	    varCache = VarCache.empty,
	    varEnv = VMap.empty,
	    bomValMap=BOMValMap.empty,
	    dconMap=DConMap.empty,
	    shadowTycs=MLTycMap.empty,
	    exh=exh
	  }

    fun handlerOf (E{exh, ...}) = exh

    (* extract the exception type constructor, of which there's hopefully exactly one *)
    fun exnTyc (env as E{tycMap=tycMap, ...}) = let
	  val [exnTyc] = TycMap.listItems (
		TycMap.mapPartiali
		  (fn (tyc, bomTyc) => if S.Tycon.equals(tyc, S.Tycon.exn) then SOME bomTyc else NONE)
		    tycMap)
	  in
	    exnTyc
	  end
    fun exnTy env = BOMTy.T_Con (exnTyc env, [])
    fun exhTy env = BOMTy.T_Cont [exnTy env]

    fun newHandler env = let
	  val E{tyCache, tycMap, varCache, varEnv, bomValMap, dconMap, shadowTycs, exh} = env
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

    fun bindVarTy (env, var, ty) = let
	  val E{tyCache, tycMap, varCache, varEnv, bomValMap, dconMap, shadowTycs, exh} = env
	  in
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
	  end
    fun lookupVarTy (E{varEnv, ...}, bomVar) = (case VMap.find(varEnv, bomVar)
           of SOME bomty => bomty
            | NONE => raise Fail(concat["lookupVarTy(env, ", BV.toString bomVar, ") = NONE"])
	  (* end case *))

    fun bindVar (env, sxmlVar, bomVar) = let
	  val E{tyCache, tycMap, varCache, varEnv, bomValMap, dconMap, shadowTycs, exh} = env
	  in
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
	  end
    fun lookupVar (E{varCache, ...}, sxmlVar) = (case VarCache.find(varCache, sxmlVar)
	   of SOME bomVar => bomVar
	    | NONE => raise Fail(concat[
		  "lookupVar(env, ", Layout.toString (S.Var.layout sxmlVar), ") = NONE"
		])
	  (* end case *))

    fun bindTyc (env, tyc, bomTyc) = let
	  val E{tyCache, tycMap, varCache, varEnv, bomValMap, dconMap, shadowTycs, exh} = env
	  in
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
	  end
    fun lookupTyc(E{tycMap, ...}, tyc) = (case TycMap.find(tycMap, tyc)
          of SOME bomTyc => bomTyc
           | NONE => raise Fail (concat["lookupTyc(", S.Tycon.toString tyc, ") = NONE"])
         (* end case *))

    fun printTycs (E{tycMap, ...}) = let
	  val keys = TycMap.listKeys tycMap
	  fun fmt tyc = concat[
		  Layout.toString (S.Tycon.layout tyc),
		  ":hash=0x", Word.toString (S.Tycon.hash tyc)
		]
	  in
	    print (concat ["tycs: ", String.concatWithMap " " fmt keys, "\n"])
	  end

    fun bindBOMVal (env, bomVal, bomVarBind) = let
	  val E{tyCache, tycMap, varCache, varEnv, bomValMap, dconMap, shadowTycs, exh} = env
	  in
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
	  end
    fun lookupBOMVal (E{bomValMap, ...}, bomVal : S.CoreBOM.Val.t) : var_bind = (
	  case BOMValMap.find(bomValMap, bomVal)
	   of SOME bomVarBind => bomVarBind
	    | NONE => raise Fail(concat["lookupBOMVal(env, ",
		S.CoreBOM.ValId.toString (S.CoreBOM.Val.idOf bomVal), ") = NONE"])
         (* end case *))

    fun bindDCon (env, mlDCon, bomDCon) = let
	  val E{tyCache, tycMap, varCache, varEnv, bomValMap, dconMap, shadowTycs, exh} = env
	  in
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
	  end
    fun lookupDCon (E{dconMap, ...}, mlDCon) = (case DConMap.find(dconMap, mlDCon)
	   of SOME bomDCon => bomDCon
	    | NONE => raise Fail(concat["lookupDCon(env, ", S.Con.toString mlDCon, ") = NONE"])
          (* end case *))

    fun bindMLTyc (env, bomTyc, def) = let
	  val E{tyCache, tycMap, varCache, varEnv, bomValMap, dconMap, shadowTycs, exh} = env
	  in
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
	  end

    fun lookupMLTyc (E{shadowTycs, ...}, bomTyc) : (S.Tycon.t * S.PrimConDef.t vector) = (
	  case MLTycMap.find(shadowTycs, bomTyc)
	   of SOME def => def
	    | NONE => raise Fail(concat[
		  "lookupMLTyc(env, ", Layout.toString (S.CoreBOM.TyCon.layout bomTyc), ") = NONE"
		])
	  (* end case *))

    fun printMLTycs (E{shadowTycs, ...}) = let
	  val tycs = MLTycMap.listItems shadowTycs
	  fun fmt (mlTyc, _) = concat[
		  S.Tycon.toString mlTyc,":id=0x", Word.toString (S.Tycon.hash mlTyc)
	        ]
	  in
	    print (concat [
		"ml tycs: ", String.concatWithMap " " fmt tycs, "\n"
	      ])
	  end

    fun newVarBOMValWithTy (env, bomVal, ty) = let
	  val var = BV.new (S.CoreBOM.ValId.toString(S.CoreBOM.Val.idOf bomVal) ^ ".", ty)
          val env' = bindVarTy (env, var, ty)
	  in
	    (var, bindBOMVal (env', bomVal, Var var))
	  end

  end
