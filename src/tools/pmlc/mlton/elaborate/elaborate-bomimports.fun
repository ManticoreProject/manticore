functor ElaborateBOMImports (S: ELABORATE_BOMIMPORTS_STRUCTS) = struct
  open S
  structure BOM = Ast.BOM
  structure MLType = Env.Type
  structure MLScheme = Env.Scheme

  fun elaborateBomImport (import, {env: Env.t, bomEnv: BOMEnv.t}) =
    let
      fun elaborateType (ty, lookup) = Env.elaborateType (ty,
        Lookup.fromEnv env)
      fun resolveId doResolve (mlId, maybeId) =
        CoreBOM.ValId.fromBOMId (case maybeId of
          SOME bomId => bomId
        | NONE => doResolve mlId)
      (* remove qualifying module, make a BOMId *)
      fun resolveVid vid =
        BOM.BomId.fromId o Longvid.short vid
    in
      case import of
        BOM.Import.Val (vid, ty, maybeId) =>
          let
            val ty' = elaborateType ty
            (* FIXME: not sure what to do with this scheme *)
            (* FIXME *)
            val (vid', maybeScheme) = Env.lookupLongvid (env, vid)
            val success = ref true
            val _ =
              case maybeScheme of
                (* FIXME: preError? *)
                (* FIXME: real error message? *)
                SOME scheme => MLType.unify (ty', MLScheme.ty scheme, {
                  error = fn (l, r) => Control.error (Layout.seq [l, r]),
                  preError = fn () => success := false})
              | NONE => success := false
            val newTy = CoreBOM.BOMType.MLType ty'
            val newId = resolve (resolveVid vid')
          in
            if !success then
              (* If it worked (vid was bound to a type that could
              unify with ty), put the new ty into our env and bind our
              new valId to it *)
              BOMEnv.ValEnv.extend (BOMEnv.TyEnv.extend (bomEnv, newTy),
                (* we never have typarams on a val from ML code *)
                newId, CoreBOM.Val.new (newId, newTy, []))
            else
              (* Otherwise, return the env unchanged (errors have
              already been logged above) *)
              bomEnv
          end
      | BOM.Import.Datatype (tys, tyc, maybeId, cons) => raise Fail "not implemented"
      | BOM.Import.Exn (tyc, maybeTy, maybeId) => raise Fail "not implemented"
    end
end
