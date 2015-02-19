functor ElaborateBOMImports (S: ELABORATE_BOMIMPORTS_STRUCTS) = struct
  open S
  structure BOM = Ast.BOM
  structure MLType = Env.TypeEnv.Type
  structure MLScheme = Env.TypeEnv.Scheme
  structure Tycon = Env.TypeEnv.Tycon
  (* structure TypeOps = Env.TypeEnv.Type.Ops *)


  fun translateType (mlTyEnv: BOMEnv.MLTyEnv.t)
      (mlType: MLType.t): CoreBOM.BOMType.t =
    let
      fun applyCon (tyc: Tycon.t, tyvec: MLType.t vector): CoreBOM.BOMType.t =
        case BOMEnv.MLTyEnv.lookupThis (mlTyEnv, tyc) of
          SOME bomTyc =>
            (case (CoreBOM.TyCon.applyToArgs' (bomTyc, Vector.map
              (translateType mlTyEnv) tyvec)) of
              SOME bomTy => bomTy
            | NONE => raise Fail "Bad application.")
        | NONE => raise Fail "Unmapped type."

      and translateCon (mlType: MLType.t): CoreBOM.BOMType.t =
          case (MLType.deConOpt mlType) of
            SOME (tyc, tyvec) => applyCon (tyc, tyvec)
          | NONE => raise Fail "Bad type."
    in
        (* FIXME: return the right thing *)
      CoreBOM.BOMType.Error
    end


  fun elaborateBomImport (import, {env: Env.t, bomEnv: BOMEnv.t}, mlTyEnv) =
    let
      fun elaborateMLType ty = ElaborateCore.elaborateType (ty,
        ElaborateCore.Lookup.fromEnv env)
      local
          (* FIXME: this can probably be removed *)
        fun resolve doResolve (mlId, maybeId): CoreBOM.BOMId.t =
          case maybeId of
            SOME bomId => CoreBOM.BOMId.fromAst bomId
          | NONE => doResolve mlId
        (* fun resolveToBOMId doResolve idPair = resolve doResolve idPair *)
      in
        fun resolveValId (doResolve) (idPair): CoreBOM.ValId.t  =
          CoreBOM.ValId.fromBOMId' (resolve doResolve idPair)
        (* val resolveValId = CoreBOM.ValId.fromBOMId' o resolveToBOMId *)
        (* val resolveTyId = CoreBOM.TyId.fromBOMId' o resolveToBOMId *)
        (* fun resolveTyId doResolve idPair: CoreBOM.TyId.t =  *)
        (*   CoreBOM.TyId.fromBOMId' (resolve doResolve idPair) *)
        (* fun resolveTyId = resolve() CoreBOM.TyId.fromBOMId *)
      end
    in
      case import of
        BOM.Import.Val (vid, ty, maybeId) =>
          let
            val ty' = elaborateMLType ty
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
            (if !success then
              (* If it worked (vid was bound to a type that could
              unify with ty), put the new ty into our env and bind our
              new valId to it *)
              (* we never have typarams on a val from ML code *)
              BOMEnv.ValEnv.extend (bomEnv, newValId,
                CoreBOM.Val.new (newValId, newTy, []))
            else
              (* Otherwise, return the env unchanged (errors have
              already been logged above) *)
              bomEnv, mlTyEnv)
          end
      | BOM.Import.Datatype (tyargs, tyc, maybeId, cons) =>
          let
            (* Translate the ML types from the AST representation *)
            val tyargs' = Vector.map elaborateMLType tyargs
            val maybeTyStr =
              case Env.lookupLongtycon (env, tyc) of
                SOME tyStr => SOME (tyStr)
              | NONE => NONE
            val tyId =
              CoreBOM.TyId.fromBOMId' (case maybeId of
                SOME astId => CoreBOM.BOMId.fromAst astId
              | NONE => CoreBOM.BOMId.fromLongtycon tyc)

            val maybeTycs =
              case maybeTyStr of
                (* Only datatypes can be imported, and we ignore their
                constructors since they must be explicitly imported *)
                SOME (tyStr) =>
                  (* Apply the tycon to the provided arguments *)
                  SOME (tyStr, CoreBOM.TyCon.TyC {
                    (* FIXME: how do I get typarams out of this? *)
                    id = tyId, definition = ref [], params = []})
               (* FIXME: better error handling *)
              | _ => NONE

          (* FIXME: deal with the constructors *)
          in
            case maybeTycs of
              SOME (tyStr, bomTyc) =>
                let
                    (* FIXME: error handling *)
                  val (Env.TypeStr.Datatype {tycon,...}) =
                    Env.TypeStr.node tyStr
                  (* First, we put the new tyc into the mapping *)
                  val mlTyEnv' = BOMEnv.MLTyEnv.extendThis (mlTyEnv,
                    tycon, bomTyc)
                  val bomEnv' = BOMEnv.TyEnv.extend (bomEnv, tyId,
                    BOMEnv.TypeDefn.newCon bomTyc)

                  (* Apply it to the constructors we were given *)
                  val mlTy = Env.TypeStr.apply (tyStr, tyargs')
                  (* No params by this point *)
                  val bomTy = {params = [], ty = translateType mlTyEnv' mlTy}

                  (* THEN translate it back to a BOM type *)
                  val bomEnv' = BOMEnv.TyEnv.extend (bomEnv', tyId,
                    BOMEnv.TypeDefn.newAlias bomTy)
                in
                  (bomEnv', mlTyEnv')
                end
            | NONE => (bomEnv, mlTyEnv)
          end

            (* NOTE: do we want to reject all but the datatype type strs? *)
      | BOM.Import.Exn (tyc, maybeTy, maybeId) => raise Fail "not implemented"
    end
end
