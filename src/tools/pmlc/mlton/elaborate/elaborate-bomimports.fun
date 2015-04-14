functor ElaborateBOMImports (S: ELABORATE_BOMIMPORTS_STRUCTS): ELABORATE_BOMIMPORTS
  = struct
  open S
  (* structure Ast = ElaborateCore.Ast *)
  (* structure Env = ElaborateCore.Env *)
  (* structure BOMEnv = ElaborateBOMCore.BOMEnv *)
  (* structure CoreBOM = ElaborateBOMCore.CoreBOM *)
  structure BOM = Ast.BOM
  structure MLType = Env.TypeEnv.Type
  structure MLScheme = Env.TypeEnv.Scheme
  structure MLTycon = Env.TypeEnv.Tycon
  structure MLKind = Env.TypeStr.Kind
  structure BOMExport = Ast.BOMExport

  (* structure TypeOps = Env.TypeEnv.Type.Ops *)


  local
    fun printMLObj doLayout (obj, msg) =
      print (msg ^ (Layout.toString (doLayout obj)) ^ "\n")
  in
    val printMLTy = printMLObj MLType.layout
  end

  val translateType = BOMEnv.MLTyEnv.translateType'
  fun elaborateMLType' env ty = ElaborateCore.elaborateType (ty,
    ElaborateCore.Lookup.fromEnv env)

  (* fun translateInt intSize = *)
  (*   Vector.fromList [ *)
  (*     CoreBOM.BOMType.Raw (case (Bits.toInt (MLTycon.IntSize.bits intSize)) of *)
  (*       8 => CoreBOM.RawTy.Int8 *)
  (*     | 16 => CoreBOM.RawTy.Int16 *)
  (*     | 32 => CoreBOM.RawTy.Int32 *)
  (*     | 64 => CoreBOM.RawTy.Int64 *)
  (*     | n => raise Fail (String.concatWith " " [ *)
  (*         "Compiler bug: unexpected int size:", Int.toString n, "\n"]))] *)

  (* fun translateType (mlTyEnv: BOMEnv.MLTyEnv.t) *)
  (*     (mlType: MLType.t): CoreBOM.BOMType.t = *)
  (*   let *)
  (*     fun applyCon (tyc: MLTycon.t, tyvec: MLType.t vector): CoreBOM.BOMType.t = *)
  (*       case BOMEnv.MLTyEnv.lookupThis (mlTyEnv, tyc) of *)
  (*         SOME bomTyc => *)
  (*           (case (bomTyc (Vector.map (translateType mlTyEnv) tyvec)) of *)
  (*             SOME bomTy => bomTy *)
  (*           | NONE => raise Fail "Bad application.") *)
  (*       | NONE => raise Fail "Unmapped type." *)

  (*     and translateCon (mlType: MLType.t): CoreBOM.BOMType.t = *)
  (*       let *)
  (*           (* DEBUG *) *)
  (*         val _ = printMLTy (mlType, "unwrapping ty: ") *)
  (*       in *)
  (*         case (MLType.deConOpt mlType) of *)
  (*           SOME (tyc, tyvec) => applyCon (tyc, tyvec) *)
  (*         | NONE => raise Fail "Bad type." *)
  (*       end *)
  (*   in *)
  (*     translateCon mlType *)
  (*   end *)


  fun elaborateBOMExport (export, tyEnvs as {env: Env.t, bomEnv: BOMEnv.t},
      mlTyEnv) =
    let
      val elaborateMLType = elaborateMLType' env
    in
      case BOMExport.node export of
        BOMExport.Datatype (tyvars, tycon, bomLongId, bomTys) =>
          raise Fail "Not implemented"
      | BOMExport.TypBind (tyvars, tycon, bomTy) =>
          let
            (* This is all of the "elaboration" they do on tyvars *)
            val tyvars' = Vector.map MLType.var tyvars
            val bomTy' = ElaborateBOMCore.elaborateBOMType (bomTy, tyEnvs)
            (* FIXME: error handling *)
            (* FIXME: we'll need a second env here *)
            val (SOME mlTy): MLType.t option = BOMEnv.PrimTyEnv.lookupBOM bomTy'
            val kind = Vector.length tyvars
            (* TODO: check kind matches *)
            (* We follow the lead of ElaborateCore.elabTypBind *)
            val mlTyStr = Env.TypeStr.def (MLScheme.make {canGeneralize = true,
               ty = mlTy, tyvars = tyvars}, MLKind.Arity kind)
            (* Extend ML Type environment in place *)
            val _ = Env.extendTycon (env, tycon, mlTyStr, {forceUsed = false,
              isRebind = false})
          in
            (bomEnv, mlTyEnv)
          end
      | BOMExport.Val (valId, mlTy, bomTy) =>
          raise Fail "Not implemented."
      (* FIXME: placeholder *)
        (* (bomEnv, mlTyEnv) *)
    end


  fun elaborateBOMImport (import, {env: Env.t, bomEnv: BOMEnv.t}, mlTyEnv) =
    let
      (* fun elaborateLType ty = ElaborateCore.elaborateType (ty, *)
      (*   ElaborateCore.Lookup.fromEnv env) *)
      val elaborateMLType = elaborateMLType' env
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

      fun extendEnvs (tyargs, tyc, maybeId) =
        let
          (* Translate the ML types from the AST representation *)
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
                (* FIXME: this is probably WRONG for typarams *)
                SOME (tyStr, CoreBOM.TyCon.new (tyId, List.tabulate (
                    Vector.length tyargs, fn _ => CoreBOM.TyParam.new ())))
              | _ => NONE
        in
          case maybeTycs of
            SOME (tyStr, bomTyc) =>
              let
                  (* FIXME: error handling *)
                val (Env.TypeStr.Datatype {tycon,...}) =
                  Env.TypeStr.node tyStr
                (* First, we put the new tyc into the mapping *)
                val mlTyEnv' = BOMEnv.MLTyEnv.extendThis (mlTyEnv,
                  tycon, fn args => CoreBOM.TyCon.applyToArgs' (bomTyc, args))
                val bomEnv' = BOMEnv.TyEnv.extend (bomEnv, tyId,
                  BOMEnv.TypeDefn.newCon bomTyc)

                (* Apply it to the constructors we were given *)
                val mlTy = Env.TypeStr.apply (tyStr, tyargs)
                (* No params by this point *)
                val bomTy = translateType mlTyEnv' mlTy

                val bomEnv' = BOMEnv.TyEnv.extend (bomEnv', tyId,
                  BOMEnv.TypeDefn.newAlias {params = [], ty = bomTy})
              in
                SOME (bomEnv', mlTyEnv', bomTyc, bomTy)
              end
            | NONE => NONE
        end
      end

      fun unwrapMLArrow (mlTyEnv, mlTy) =
        case MLType.deArrowOpt mlTy of
          (* MLton doesn't distinguish between arrow and constructors
            here, so we have to handle this as a special case *)
          SOME (dom, rng) => CoreBOM.BOMType.Con {
            dom = translateType mlTyEnv dom,
            rng = translateType mlTyEnv rng
          }
        | NONE => translateType mlTyEnv mlTy

      (* FIXME: note that we pull from the env in scope *)
      (* FIXME: better error handling *)
      fun unwrapMLCon (longcon, tyargs) =
        case Env.lookupLongcon (env, longcon) of
          (con, SOME tyScheme) => MLScheme.apply (tyScheme, tyargs)
        | _  => raise Fail "Unmapped longcon"

      fun translateCon (mlTyEnv, bomResultTy, tyargs, longcon, maybeTy, maybeId) =
        let
          val mlTy = unwrapMLCon (longcon, tyargs)
          val newValId = resolveValId CoreBOM.BOMId.fromLongcon (longcon, maybeId)
          val bomConTy = unwrapMLArrow (mlTyEnv, mlTy)

          (* DEBUG *)
          val _ = printMLTy (mlTy, "translating: ")

          val _ =
            if CoreBOM.BOMType.equal (bomResultTy,
              case bomConTy of
                CoreBOM.BOMType.Con {rng,...} => rng
              | tycon as (CoreBOM.BOMType.TyCon tycon') => tycon
              (* Special case for exns because it's not worth
              factoring them out *)
              | CoreBOM.BOMType.Exn => CoreBOM.BOMType.Exn
              | CoreBOM.BOMType.Error => raise Fail "Con wasn't found in env."
              | _ => raise Fail "Type is not a con.")
            then ()
            else raise Fail "Bad con application."
        in
            (* FIXME: never any params in imports? *)
          (newValId, CoreBOM.Val.new (newValId, bomConTy, []))
        end

      fun translateImportCon (mlTyEnv, bomResultTy, tyargs) importCon =
        let
            (* FIXME: let's ignore the maybeTy for now *)
          val BOM.ImportCon.T (longcon, maybeTy, maybeId) =
            BOM.ImportCon.node importCon
        in
          translateCon (mlTyEnv, bomResultTy, tyargs, longcon, maybeTy, maybeId)
        end

    in
      case BOM.Import.node import of
        BOM.Import.Val (vid, ty, maybeId) =>
          let
            val ty' = elaborateMLType ty
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
            val newTy = translateType mlTyEnv ty'
            (* QUESTION: Will we ever create a new type via a val
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
            fun extendBOMEnv ((valId, bomVal), bomEnv) =
              BOMEnv.ValEnv.extend (bomEnv, valId, bomVal)

            val tyargs' = Vector.map elaborateMLType tyargs

             (* FIXME: error handling *)
            val SOME (bomEnv', mlTyEnv', bomTyc, bomTy) =
              extendEnvs (tyargs', tyc, maybeId)
            val cons' = map (translateImportCon (mlTyEnv', bomTy, tyargs')) cons

            (* Add the constructors to the tycon *)
            val _ = (fn (CoreBOM.TyCon.TyC {definition,...}) =>
              definition := map (fn (valId, bomVal) =>  CoreBOM.ConsDef (
                CoreBOM.ValId.truncateToBOMId valId,
                SOME (CoreBOM.Val.typeOf bomVal))) cons') bomTyc

            val bomEnv' = foldl (fn ((valId, bomVal), bomEnv) =>
             BOMEnv.ValEnv.extend (bomEnv, valId, bomVal)) bomEnv' cons'

          in
            (bomEnv', mlTyEnv')
          end

      | BOM.Import.Exn (longcon, maybeTy, maybeId) =>
          let
            (* FIXME: this is broken b/c of the way tycons are handled *)
            val (newValId, newVal) = translateCon (mlTyEnv,
              CoreBOM.BOMType.Exn, Vector.fromList [], longcon, maybeTy,
                maybeId)
          in
            (BOMEnv.ValEnv.extend (bomEnv, newValId, newVal), mlTyEnv)
          end

    end
end
