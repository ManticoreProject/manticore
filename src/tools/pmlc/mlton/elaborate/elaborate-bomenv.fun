signature ENV_MAP_PARAMS = sig
  type key
  type value
  val compare: key * key -> order
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

      val lookup: value Map.map * key -> value option
      val extend: value Map.map * key * value -> value Map.map
      val empty: value Map.map

      (* Why is this a syntax error? *)
      (* sharing type t = value Map.map *)
    end = struct
      open S

      structure Key = struct
        type ord_key = key
        val compare = compare
      end

      structure Map = RedBlackMapFn (Key)

      type t = value Map.map

      val lookup = Map.find
      val extend = Map.insert
      val empty = Map.empty
    end

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
      structure EnvMap = EnvMap (struct
        type key = AstBOM.TyParam.t
        type value = CoreBOM.TyParam.t
        val compare = AstBOM.TyParam.compare
      end)
    in
      open EnvMap
    end

    fun extend (self: t, newEl: Key.ord_key) =
      let
        val key = newEl
        val value = CoreBOM.TyParam.fromAst (newEl: AstBOM.TyParam.t)
      in
        Map.insert (self, key, value)
      end
    end

  structure TyEnv = struct
  (* TODO: make this handle other kinds of IDs *)
    (* structure Key: ORD_KEY = struct *)
    (*   type ord_key = AstBOM.BomId.t *)
    (*   val compare = AstBOM.BomId.compare *)
    (* end *)

    (* structure Map = RedBlackMapFn (Key) *)

    (* type t = CoreBOM.BomType.t Map.map *)

    (* val lookup = Map.find *)
    (* val new = fn () => Map.empty *)
    (* val extend = Map.insert *)
    local
      structure EnvMap = EnvMap (struct
        type key = AstBOM.BomId.t
        type value = CoreBOM.BomType.t
        val compare = AstBOM.BomId.compare
      end)
    in
      open EnvMap
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

  datatype t = T of {tyParamE: TyParamEnv.t, tyE: TyEnv.t}

  fun getTyParamEnv (T {tyParamE = tyParamE', tyE = tyE'}) = tyParamE'
  fun getTyEnv (T {tyParamE = tyParamE', tyE = tyE'}) = tyE'

  fun setTyParamEnv (T {tyParamE = tyParamE', tyE = tyE'}, newEnv) =
    T {tyParamE = newEnv, tyE = tyE'}
  fun setTyEnv (T {tyParamE = tyParamE', tyE = tyE'}, newEnv) =
    T {tyParamE = tyParamE', tyE = newEnv}

  val empty = T {tyParamE = TyParamEnv.empty, tyE = TyEnv.empty}

end
