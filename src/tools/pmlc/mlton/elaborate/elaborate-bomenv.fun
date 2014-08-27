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
    end


  structure TyAlias = struct
    type t = {
      params: CoreBOM.TyParam.t list,
      ty: CoreBOM.BomType.t
    }

    fun arity ({params = params, ...}: t) = length params

    fun applyToArgs ({params = params, ty = ty}: t, args) =
      ListPair.foldr
        (fn (toSwap, swapFor, ty') =>
          CoreBOM.BomType.applyArg (ty', toSwap, swapFor))
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

  datatype t = T of {tyEnv: TyEnvMap.t, tyParamEnv: TyParamEnvMap.t}


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
      val lookup = lookupThis o (fn (env, param) => (getTyParamEnv env, param))

      fun extend(T {tyEnv = tyEnv', tyParamEnv = tyParamEnv'}, newParam) =
        T {tyEnv = tyEnv', tyParamEnv = extendThis (tyParamEnv', newParam)}

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
    in
      val lookup  = lookupThis o (fn (env, ty) => (getTyEnv env, ty))
      fun extend (T {tyEnv = tyEnv, tyParamEnv = tyParamEnv}: env,
          bomId,
          newTy): env =
        T {tyEnv = extendThis (tyEnv, bomId, newTy), tyParamEnv = tyParamEnv}
    end
  end

  val empty = T {tyEnv = TyEnv.empty, tyParamEnv = TyParamEnv.empty}

end
