functor ElaborateBOMImports (S: ELABORATE_BOMIMPORTS_STRUCTS) = struct
  open S
  structure BOM = Ast.BOM
  structure MLType = Env.TypeEnv.Type
  structure MLScheme = Env.TypeEnv.Scheme
  structure Tycon = Env.TypeEnv.Tycon
  (* structure TypeOps = Env.TypeEnv.Type.Ops *)


  fun translateType (mlType: MLType.t): CoreBOM.BOMType.t =
    (* We map tycons to recursive calls *)
    case (MLType.deConOpt mlType) of
      (* SOME (Tycon.bool, tyvec) => (* FIXME *) *)
      NONE => raise Fail "Bad type."
    | _ => raise Fail "Not implemented."


  fun elaborateBomImport (import, {env: Env.t, bomEnv: BOMEnv.t}) =
    let
      fun elaborateMLType (ty, lookup) = ElaborateCore.elaborateType (ty,
        ElaborateCore.Lookup.fromEnv env)
      local
        fun resolve doResolve (mlId, maybeId): CoreBOM.BOMId.t =
          case maybeId of
            SOME bomId => CoreBOM.BOMId.fromAst bomId
          | NONE => doResolve mlId
      in
        fun resolveValId (doResolve) (idPair): CoreBOM.ValId.t  =
          CoreBOM.ValId.fromBOMId' (resolve doResolve idPair)
        (* fun resolveTyId = resolve() CoreBOM.TyId.fromBOMId *)
      end
    in
      case import of
        BOM.Import.Val (vid, ty, maybeId) =>
          let
            val ty' = elaborateMLType (ty, env)
            (* FIXME: not sure what to do with this scheme *)
            (* FIXME *)
            val (vid', maybeScheme) = Env.lookupLongvid (env, vid)
            val success = ref true
            val _ =
              case maybeScheme of
                (* FIXME: preError? *)
                (* FIXME: real error message? *)
                SOME scheme => MLType.unify (ty', #instance (MLScheme.instantiate
                    scheme), {
                  (* FIXME: real region *)
                  error = fn (l, r) => Control.error (Region.bogus, l, r),
                  preError = fn () => success := false})
               (* FIXME: error message *)
              | NONE => success := false
            (* FIXME: PUT A REAL TYPE HERE *)
            val newTy = CoreBOM.BOMType.Error
            (* QUESTION: Will we ever create a new type via a val0
             import? *)
            (* remove qualifying module, make a BOMId *)
            val newValId = resolveValId CoreBOM.BOMId.fromLongvid (vid, maybeId)
          in
            if !success then
              (* If it worked (vid was bound to a type that could
              unify with ty), put the new ty into our env and bind our
              new valId to it *)
              (* we never have typarams on a val from ML code *)
              BOMEnv.ValEnv.extend (bomEnv, newValId,
                CoreBOM.Val.new (newValId, newTy, []))
            else
              (* Otherwise, return the env unchanged (errors have
              already been logged above) *)
              bomEnv
          end
      | BOM.Import.Datatype (tys, tyc, maybeId, cons) => raise Fail "not implemented"
      | BOM.Import.Exn (tyc, maybeTy, maybeId) => raise Fail "not implemented"
    end
end
