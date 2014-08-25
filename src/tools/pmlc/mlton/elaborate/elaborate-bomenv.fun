signature ENV_MAP_PARAMS = sig
  type key
  type value
  val compare: key * key -> order
  val make: key -> value
end


functor BOMEnv (S: ELABORATE_BOMENV_STRUCTS): ELABORATE_BOMENV = struct
  open S

  structure AstBOM = Ast.AstBOM

  functor EnvMap (S: ENV_MAP_PARAMS):
    sig
      include ENV_MAP_PARAMS

      structure Key: ORD_KEY
      structure Map: ORD_MAP
      sharing Map.Key = Key
      sharing type key = Key.ord_key

      type t = value Map.map

      val lookupThis: value Map.map * key -> value option
      val extendThis: value Map.map * key -> value Map.map
      val empty: value Map.map

    end = struct
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



  structure TypeDefn = struct
    datatype t
      = TyAlias of {
          params: CoreBOM.TyParam.t list,
          ty: CoreBOM.BomType.t
      }
      | TyCon of CoreBOM.TyCon.t
  end


  structure TyEnvMap = EnvMap (struct
    type key = AstBOM.BomId.t
    type value = TypeDefn.t
    val compare = AstBOM.BomId.compare
  end)

  type tyEnv = typeDefn TyEnvMap.map

  structure TyParamEnvMap = EnvMap (struct
    type key = AstBOM.TyParam.t
    type value = CoreBOM.TyParam.t
    val compare = AstBOM.TyParam.compare
  end)

  type tyParamEnv = CoreBOM.TyParam.t TyParamEnvMap.map

  datatype env = T of {tyEnv: tyEnv, tyParamEnv: tyParamEnv}

  structure TyParamEnv = struct
    (* structure Key: ORD_KEY = struct *)
    (*   type ord_key = AstBOM.TyParam.t *)
    (*   val compare = AstBOM.TyParam.compare *)
    (* end *)

    (* structure Map = RedBlackMapFn (Key) *)

    (* type t = CoreBOM.TyParam.t Map.map *)

    (* val lookup = Map.find *)
    (* val new = fn () => Map.empty *)

    local
      structure EnvMap = TyParamEnvMap
    in
      open EnvMap
    end

    fun extendThis (self: t, newEl: Key.ord_key) =
      let
        val key = newEl
        val value = CoreBOM.TyParam.fromAst (newEl: AstBOM.TyParam.t)
      in
        Map.insert (self, key, value)
      end

    local
      fun getTyParamEnv (env: env) = #tyParamEnv env
    in
      val lookup = lookupThis o getTyParamEnv

      fun extend({tyEnv = tyEnv', tyParamEnv = tyParamEnv'}, newParam) =
        {tyEnv = tyEnv', tyParamEnv = extendThis (tyParamEnv', newParam)}

    end
  end

  structure TyEnv = struct
    type t = tyEnv

  (* TODO: make this handle other kinds of IDs *)
    local
      structure EnvMap = TyEnvMap
    in
      open EnvMap
    end

    local
      fun getTyEnv (env: env) = #tyEnv env
    in
      val lookup  = lookupThis o getTyEnv
      fun extend ({tyEnv = tyEnv', tyParamEnv = tyParamEnv'}, newTy): env =
        {tyEnv = extendThis (tyEnv', newTy), tyParamEnv = tyParamEnv'}
    end
  end


  (* (* FIXME: get the order of this delcaration right *) *)
  (* datatype t *)
  (*   = T of {HLE: HLOpEnv.t, TE: TypeEnv.t, VE: ValEnv.t} *)

  (* (* structure Type = struct *) *)
  (* (*   datatype t  *) *)
  (* (*     = TypeVar of Tyvar.t *) *)
  (* (*     | MLType of Env.Type.t *) *)
  (* (*     |   *) *)
  (* (* end *) *)

  (* structure Scheme = struct *)
  (*   datatype t = T of {typarams: CoreBOM.TyParam.t list, ty: CoreBOM.BOMType.t} *)

  (*   fun getCoreBOM.TyParams (T {typarams=typarams', ...}) = typarams' *)
  (*   fun getTy (T {ty=ty',...}) = ty' *)
  (*   fun new (typarams: CoreBOM.TyParam.t list, ty: CoreBOM.BomType.t) *)
  (*   fun generalizes (scheme: t, ty: CoreBOM.BomType.t): bool = *)
  (* end *)

  (* datatype t = T of {tyParamE: TyParamEnv.t, tyE: TyEnv.t} *)

  (* fun getTyParamEnv (T {tyParamE = tyParamE', tyE = tyE'}) = tyParamE' *)
  (* fun getTyEnv (T {tyParamE = tyParamE', tyE = tyE'}) = tyE' *)

  (* fun setTyParamEnv (T {tyParamE = tyParamE', tyE = tyE'}, newEnv) = *)
  (*   T {tyParamE = newEnv, tyE = tyE'} *)
  (* fun setTyEnv (T {tyParamE = tyParamE', tyE = tyE'}, newEnv) = *)
  (*   T {tyParamE = tyParamE', tyE = newEnv} *)

  (* val empty = T {tyParamE = TyParamEnv.empty, tyE = TyEnv.empty} *)

  (* fun extendTyParamEnv (T {tyParamE = tyParamE', tyE = tyE'}, newParam) = *)
  (*   T {tyParamE = TyParamEnv.extend (tyParamE', newParam), tyE = tyE'} *)

  (* fun extendTyEnv (T {tyParamE = tyParamE', tyE = tyE'}, newId, newTy) = *)
  (*   T {tyParamE = tyParamE', tyE = TyEnv.extend (tyE', newId, newTy)} *)


  (* fun lookupTyParam (env: t, tyParam) = *)
  (*   TyParamEnv.lookup ((getTyParamEnv env), tyParam) *)
  type t = env
end
