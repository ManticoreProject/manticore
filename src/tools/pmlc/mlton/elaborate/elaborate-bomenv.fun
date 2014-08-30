signature ENV_MAP_PARAMS = sig
  type key
  type value
  val compare: key * key -> order
end

signature ENV_MAP = sig
  include ENV_MAP_PARAMS

  structure Key: ORD_KEY
  structure Map: ORD_MAP
  sharing type key = Key.ord_key

  type t = value Map.map

  val lookupThis: t * key -> value option
  val extendThis: t * key * value -> t
  val empty: t

  (* TODO: for easier debugging *)
  (* val keyToString: key -> string *)
  (* val valToString: value -> string *)

end

functor BOMEnv (S: ELABORATE_BOMENV_STRUCTS): ELABORATE_BOMENV = struct
  open S

  structure AstBOM = Ast.AstBOM

  functor EnvMap (S: ENV_MAP_PARAMS): ENV_MAP = struct
      open S

      structure Key = struct
        type ord_key = key
        val compare = compare
      end

      structure Map = RedBlackMapFn (Key)

      type t = value Map.map

      val lookupThis = Map.find
      val extendThis = Map.insert
      val empty = Map.empty

      (* fun toString (map: t) =  *)
      (*   let  *)
      (*     val elements =  *)
    end


  structure TyAlias = struct
    type t = {
      params: CoreBOM.TyParam.t list,
      ty: CoreBOM.BomType.t
    }

    fun arity ({params = params, ...}: t) = length params

    fun applyToArgs ({params, ty}: t, args) =
      ListPair.foldr
        (fn (toSwap, swapFor, ty) =>
          CoreBOM.BomType.applyArg (ty, toSwap, swapFor))
        ty
        (params, args)

    val error: t = {
      params = [],
      ty = CoreBOM.BomType.makeRegion (
        CoreBOM.BomType.Error,
        Region.bogus)
    }
  end


  structure TypeDefn = struct
    datatype t
      = Alias of TyAlias.t
      | Con of CoreBOM.TyCon.t

    fun arity defn =
      case defn of
        Alias alias => TyAlias.arity alias
    (* TODO: other case *)

    fun applyToArgs (defn, args) =
      case defn of
        Alias alias => TyAlias.applyToArgs (alias, args)
    (* TODO: other case *)

    val error = Alias TyAlias.error
  end


  structure TyEnvMap = EnvMap (struct
    type key = CoreBOM.TyId.t
    type value = TypeDefn.t
    val compare = CoreBOM.TyId.compare
  end)


  structure TyParamEnvMap = EnvMap (struct
    type key = AstBOM.TyParam.t
    type value = CoreBOM.TyParam.t
    val compare = AstBOM.TyParam.compare
  end)


  datatype t = T of {
    tyEnv: TyEnvMap.t,
    tyParamEnv: TyParamEnvMap.t,
    currentModule: CoreBOM.ModuleId.t
  }


  structure TyParamEnv = struct
    type env = t

    open TyParamEnvMap


    fun extendThis (self: t, newEl: Key.ord_key) =
      let
        val key = newEl
        val value = CoreBOM.TyParam.fromAst (newEl: AstBOM.TyParam.t)
      in
        Map.insert (self, key, value)
      end

    local
      fun getTyParamEnv (T env: env): t = #tyParamEnv env
    in
      fun lookup (env, param) = lookupThis (getTyParamEnv env, param)

      fun extend(T {tyEnv, tyParamEnv, currentModule}, newParam) =
        T {
          tyEnv = tyEnv,
          tyParamEnv = extendThis (tyParamEnv, newParam),
          currentModule = currentModule
        }

      fun getParams (env: env) =
         let
           val items = Map.listItems (getTyParamEnv env)
           val getHash = CoreBOM.TyParam.hash
         in
           ListMergeSort.sort
             (fn (x, y) => (getHash x) < (getHash y))
             items
         end


    end
  end

  structure TyEnv = struct
    type env = t

    open TyEnvMap

    local
      fun getTyEnv (T env: env): t = #tyEnv env
      fun maybeQualify (ty: CoreBOM.TyId.t, T env: env) =
        CoreBOM.TyId.maybeQualify(ty, #currentModule env)
    in
      fun lookup (env, ty) =
        (print (String.concat [
          "Looking up ",
          CoreBOM.TyId.toString ty,
          "\n"
        ])
        ; lookupThis (getTyEnv env, maybeQualify (ty, env)))
      fun extend (env as T {tyEnv, tyParamEnv, currentModule}: env,
          bomId,
          newTy): env =
        let
          val qualifiedId = maybeQualify (bomId, env)
        in
          (print (String.concat [
            "Extending env for bomid: ",
            CoreBOM.TyId.toString qualifiedId,
            "\n"
          ])
            ; T {
              tyEnv = extendThis (tyEnv, qualifiedId, newTy),
              tyParamEnv = tyParamEnv,
              currentModule = currentModule
            })
        end
    end
  end

  val empty = T {
    tyEnv = TyEnv.empty,
    tyParamEnv = TyParamEnv.empty,
    currentModule = CoreBOM.ModuleId.bogus
  }

  fun emptyNamed name = T {
    tyEnv = TyEnv.empty,
    tyParamEnv = TyParamEnv.empty,
    currentModule = name
  }

  fun setName (T {tyEnv, tyParamEnv, currentModule}, name) =
    T {
      tyEnv = tyEnv,
      tyParamEnv = tyParamEnv,
      currentModule = name
    }

  fun setName' (env, name) =
    setName (env, CoreBOM.ModuleId.fromBomId name)

end
