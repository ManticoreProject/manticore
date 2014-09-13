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
  val listKeys: t -> key list
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
      (* val listItems = Map.listItems *)
      val empty = Map.empty

      fun listKeys envMap =
        map (fn (k, _) => k) (Map.listItemsi envMap)

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
      if length params = length args then
        SOME (ListPair.foldr
          (fn (toSwap, swapFor, ty) =>
            CoreBOM.BomType.applyArg (ty, toSwap, swapFor))
          ty
          (params, args))
      else
        NONE

    val error: t = {
      params = [],
      ty = CoreBOM.BomType.Error
    }

    fun equal (alias, alias') =
      (arity alias = arity alias') andalso
        CoreBOM.BomType.equal (#ty alias, #ty alias')
    fun equals (aliass, aliass') =
      (length aliass = length aliass') andalso
        ListPair.allEq equal (aliass, aliass')

    local
      fun boolToOpt (comparison: ('a * 'a) -> bool) (left, right) =
        if comparison (left, right) then
          SOME left
        else
          NONE
    in
      val equal' = boolToOpt equal
      val equals' = boolToOpt equals
    end
  end


  structure TypeDefn = struct
    datatype def
      = Alias of TyAlias.t
      | Con of CoreBOM.TyCon.t

    type t = {def: def, uid: int}

    local
      val counter = Counter.new 0

      fun def (tyDefn: t) = #def tyDefn
      fun uid (tyDefn: t) = #uid tyDefn
      fun app2 f (x, y) = (f x, f y)
      fun new def = {
        def = def,
        uid = Counter.next counter
      }
    in
      fun arity defn =
        case def defn of
          Alias alias => TyAlias.arity alias
        | Con con => CoreBOM.TyCon.arity con

      fun applyToArgs (defn, args) =
        case def defn of
          Alias alias => TyAlias.applyToArgs (alias, args)
        | Con con => CoreBOM.TyCon.applyToArgs (con, args)

      fun isCon defn =
        case def defn of
          Con con => SOME defn
        | _ => NONE

      fun getCon defn =
        case def defn of
          Con con => SOME con
        | _ => NONE

      (* TODO: handle typarams correctly *)
      val compare = Int.compare o (app2 uid)
      val newCon = new o Con
      val newAlias = new o Alias
    end

    val error = {
      def = Alias TyAlias.error,
      uid = ~1
    }
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

  structure ValEnvMap = EnvMap (struct
    type key = CoreBOM.ValId.t
    type value = TyAlias.t
    val compare = CoreBOM.ValId.compare
  end)


  datatype t = T of {
    tyEnv: TyEnvMap.t,
    tyParamEnv: TyParamEnvMap.t,
    valEnv: ValEnvMap.t,
    currentModule: CoreBOM.ModuleId.t
  }

  fun modifyTyEnv (T {tyEnv, tyParamEnv, valEnv, currentModule}, f) =
    T {
      tyEnv = f tyEnv,
      valEnv = valEnv,
      tyParamEnv = tyParamEnv,
      currentModule = currentModule
    }

  fun modifyValEnv (T {tyEnv, tyParamEnv, valEnv, currentModule}, f) =
    T {
      valEnv = f valEnv,
      tyEnv = tyEnv,
      tyParamEnv = tyParamEnv,
      currentModule = currentModule
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

      fun extend(T {tyEnv, tyParamEnv, valEnv, currentModule}, newParam) =
        T {
          tyEnv = tyEnv,
          tyParamEnv = extendThis (tyParamEnv, newParam),
          valEnv = valEnv,
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

  local
    fun maybeQualify maybeQualFn (id, T env: t) =
      maybeQualFn (id, #currentModule env)
    fun getEnv envSelector (T env: t) =
      envSelector env
    fun trace (ss: string list) =
      print (String.concat (ss@["\n"]))
    fun lookup (lookupThis, getEnv, maybeQualify, idToString) (env, id) =
      (trace ["Looking up ", idToString id]
      ; lookupThis (getEnv env, maybeQualify (id, env)))
    fun extend (maybeQualify, extendThis, idToString, modifyEnv) (
        env, id, newVal) =
      let
        val qualifiedId = maybeQualify (id, env)
      in
        (trace [
          "Extending env for bomid: ",
          idToString qualifiedId
        ]
        ; modifyEnv (env, fn thisEnv => extendThis (thisEnv, qualifiedId, newVal)))
      end

  in
    structure TyEnv = struct
      type env = t

      open TyEnvMap

      local
        val getEnv = getEnv #tyEnv
        val maybeQualify = maybeQualify CoreBOM.TyId.maybeQualify
      in
        val lookup =
          lookup (lookupThis, getEnv, maybeQualify, CoreBOM.TyId.toString)
        val extend =
          extend (maybeQualify, extendThis, CoreBOM.TyId.toString,
            modifyTyEnv)

        fun lookupCon (env, tyId) =
          case lookup (env, tyId) of
            SOME tyDefn => TypeDefn.getCon tyDefn
          | NONE => NONE

        fun printKeys env =
          print (
            String.concat ["[", (
            String.concatWith ", "
              (map CoreBOM.TyId.toString (listKeys (getEnv env)))), "]\n"])

      end
    end


    structure ValEnv = struct
      type env = t

      open ValEnvMap

      local
        val getEnv = getEnv #valEnv
        val maybeQualify = maybeQualify CoreBOM.ValId.maybeQualify
      in
        val lookup =
          lookup (lookupThis, getEnv, maybeQualify, CoreBOM.ValId.toString)
        val extend =
          extend (maybeQualify, extendThis, CoreBOM.ValId.toString,
            modifyValEnv)
      end
    end

  end

  val empty = T {
    tyEnv = TyEnv.empty,
    tyParamEnv = TyParamEnv.empty,
    valEnv = ValEnv.empty,
    currentModule = CoreBOM.ModuleId.bogus
  }

  fun emptyNamed name = T {
    tyEnv = TyEnv.empty,
    tyParamEnv = TyParamEnv.empty,
    valEnv = ValEnv.empty,
    currentModule = name
  }

  fun setName (T {tyEnv, tyParamEnv, valEnv, currentModule}, name) =
    T {
      tyEnv = tyEnv,
      tyParamEnv = tyParamEnv,
      valEnv = ValEnv.empty,
      currentModule = name
    }

  fun setName' (env, name) =
    setName (env, CoreBOM.ModuleId.fromBomId name)

  (* fun setValEnv (T {tyEnv, tyParamEnv, valEnv, currentModule}, valEnv') = *)
  (*   T { *)
  (*     tyEnv = tyEnv, *)
  (*     tyParamEnv = tyParamEnv, *)
  (*     valEnv = valEnv' *)
  (*     currentModule = currentModule *)
  (*   } *)

  (* fun getValEnv T {valEnv,...} = valEnv *)

end
