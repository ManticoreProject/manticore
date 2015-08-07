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
  structure CoreML = Env.CoreML
  structure ABOMExport = Ast.BOMExport
  structure BOMExport = CoreML.BOMExport

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


  (* creating a new (possibly polymorphic) ML val/tycon from one exported by BOM *)
  fun elaborateBOMExport (export, tyEnvs as {env: Env.t, bomEnv: BOMEnv.t}) =
    let
      val elaborateMLType = elaborateMLType' env
      val mkBOMExport = CoreML.Dec.BOMExport
    in
      case ABOMExport.node export of
        ABOMExport.Datatype (tyvars, tycon, bomLongId, bomTys, primConDefs) =>
          (* TODO(wings): this case is mostly a blind adaptation of the TypBind case and is fairly wrong; finish it *)
          let
            (* elaborate tyvars with a simple conversion *)
            val tyvars' = Vector.map MLType.var tyvars
            (* the name (to be used in ML) of the BOM definitions being exported *)
            val tyId = CoreBOM.TyId.fromLongId bomLongId

            (* then we try to find the pre-elaborated tycon in the environment before elaborating its
            type parameters (in bomTys) and applying the tycon to them *)

            (*val bomCon = CoreBOM.TyCon.new(tyId, [(*params*)])*)
            (*BOM.Type.T_Con (tycon, map (fn (bomTy) => (ElaborateBOMCore.elaborateBOMType (bomTy, bomEnv))) bomTys)*)
            val bomTyCon = case BOMEnv.TyEnv.lookup (bomEnv, tyId)
              of SOME x => x
               | NONE => raise Fail "internal error: did not find tyId in environment!"
            val bomTy = case BOMEnv.TypeDefn.applyToArgs (bomTyCon, [] (*Vector.foldl (op::) [] tyvars'*))
              of SOME x => x
               | NONE => raise Fail "internal error: arity mismatch applying empty arguments to tycon!"
            val bomCon = (case bomTy(*ElaborateBOMCore.elaborateBOMType (bomTy, bomEnv)*)
              of CoreBOM.BOMType.TyCon {con=bomCon,...} => bomCon
               | _ => raise Fail "internal error: did not find TyCon BOMType when looking up _datatype in BOM environment"
              (* end case *))
            val kind = MLKind.Arity (Vector.length tyvars)
            (* TODO: check kind matches *)
            val mlTycon = Env.newTycon (Ast.Tycon.toString tycon,
              kind, MLTycon.AdmitsEquality.Never, Ast.Tycon.region tycon)
            (* Extend ML Type environment in place *)
            val _ = Env.extendTycon (env, tycon, Env.TypeStr.tycon (mlTycon,
             kind), {forceUsed = false, isRebind = false})
          in
            (SOME (BOMExport.Datatype (mlTycon, bomCon, ())), tyEnvs)
          end
          (*raise Fail "Not implemented"*)
      | ABOMExport.TypBind (tyvars, tycon, bomTy) =>
          let
            (* This is all of the "elaboration" they do on tyvars *)
            val tyvars' = Vector.map MLType.var tyvars
            (* FIXME: error handling *)
            val bomCon = (case ElaborateBOMCore.elaborateBOMType (bomTy, bomEnv)
              of CoreBOM.BOMType.TyCon {con=bomCon,...} => bomCon
(*               | CoreBOM.BOMType.Raw rawTy => BOM.BOMType.Raw BOM.BOMType.Int8*)
               | ty => raise Fail ("FIXME: expected tycon but found primitive type: " ^ Layout.toString (BOM.BOMType.layout bomTy) ^ "...")
              (* end case *))
            val kind = MLKind.Arity (Vector.length tyvars)
            (* TODO: check kind matches *)
            val mlTycon = Env.newTycon (Ast.Tycon.toString tycon,
              kind, MLTycon.AdmitsEquality.Never, Ast.Tycon.region tycon)
            (* Extend ML Type environment in place *)
            val _ = Env.extendTycon (env, tycon, Env.TypeStr.tycon (mlTycon,
             kind), {forceUsed = false, isRebind = false})
          in
            (SOME (BOMExport.TypBind (mlTycon, bomCon)), tyEnvs)
          end
      | ABOMExport.Val (mlValId, mlTy, bomValId) =>
          let
            (* FIXME: error handling *)
            val mlTy' = ElaborateCore.elaborateType (mlTy,
              ElaborateCore.Lookup.fromEnv env)
            in
              case BOM.ValueId.node bomValId of
                 BOM.ValueId.HLOpQId hlopqid => raise Fail "TODO(wings): elaborate HLOp exports"
               | BOM.ValueId.LongId bomLongId =>
                   let
                     (*val _ = print "BOMEnv.TyEnv: "
                     val _ = BOMEnv.TyEnv.printKeys bomEnv
                     val _ = print "\n ValEnv: "
                     val _ = BOMEnv.ValEnv.printKeys bomEnv
                     val _ = print "\n"*)
                     val valId = CoreBOM.ValId.fromLongId (bomLongId)
                     val bomVal =
                       case BOMEnv.ValEnv.lookup (bomEnv, valId) of
                          SOME bv => bv
                        | NONE => raise Fail ("failed to find referenced BOM name: " ^ CoreBOM.ValId.toString valId)
                     val mlVar = Ast.Vid.toVar mlValId
                     val mlVar' = ElaborateCore.Var.fromAst mlVar
                     (* FIXME: rebind? *)
                     val _ = Env.extendVar (env, mlVar, mlVar',
                       MLScheme.fromType mlTy', {isRebind = false})
                  in
                    (SOME (BOMExport.ValBind (mlVar', mlTy', bomVal)), tyEnvs)
                end
          end
    end


  (* importing a monomorphization (or monomorphic) ML val/tycon into the BOM environment *)
  fun elaborateBOMImport (import, {env: Env.t, bomEnv: BOMEnv.t}) =
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
        fun resolveValId (doResolve) (idPair): CoreBOM.ValId.t =
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
                val bomEnv' = (fn bomEnv' => BOMEnv.TyEnv.extend (bomEnv', tyId,
                  BOMEnv.TypeDefn.newCon bomTyc)) (BOMEnv.MLTyEnv.extend (
                  bomEnv, tycon, fn args => CoreBOM.TyCon.applyToArgs' (
                    bomTyc, args)))

                (* Apply it to the constructors we were given *)
                val mlTy = Env.TypeStr.apply (tyStr, tyargs)
                (* No params by this point *)
                val bomTy = translateType bomEnv' mlTy

                val bomEnv' = BOMEnv.TyEnv.extend (bomEnv', tyId,
                  BOMEnv.TypeDefn.newAlias {params = [], ty = bomTy})
              in
                  SOME (bomTyc, bomTy, {env = env, bomEnv = bomEnv'})
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

      fun translateCon (bomEnv, bomResultTy, tyargs, longcon, maybeTy, maybeId) =
        let
          val mlTy = unwrapMLCon (longcon, tyargs)
          val newValId = resolveValId CoreBOM.BOMId.fromLongcon (longcon, maybeId)
          val bomConTy = unwrapMLArrow (bomEnv, mlTy)

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

      fun translateImportCon (bomEnv, bomResultTy, tyargs) importCon =
        let
            (* FIXME: let's ignore the maybeTy for now *)
          val BOM.ImportCon.T (longcon, maybeTy, maybeId) =
            BOM.ImportCon.node importCon
        in
          translateCon (bomEnv, bomResultTy, tyargs, longcon, maybeTy, maybeId)
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
            val newTy = translateType bomEnv ty'
            (* QUESTION: Will we ever create a new type via a val
             import? *)
            (* remove qualifying module, make a BOMId *)
            val newValId = resolveValId CoreBOM.BOMId.fromLongvid (vid, maybeId)
          in
              raise Fail "TODO(wings): BOM.Import.Val elaboration not implemented"
            (* (if !success then *)
            (*   (* If it worked (vid was bound to a type that could *)
            (*   unify with ty), put the new ty into our env and bind our *)
            (*   new valId to it *) *)
            (*   (* we never have typarams on a val from ML code *) *)
            (*   BOMEnv.ValEnv.extend (bomEnv, newValId, *)
            (*     CoreBOM.Val.new (newValId, newTy, [])) *)
            (* else *)
            (*   (* Otherwise, return the env unchanged (errors have *)
            (*   already been logged above) *) *)
            (*   bomEnv) *)
          end

      | BOM.Import.Datatype (tyargs, tyc, maybeId, cons) =>
          let
            fun extendBOMEnv ((valId, bomVal), bomEnv) =
              BOMEnv.ValEnv.extend (bomEnv, valId, bomVal)

            val tyargs' = Vector.map elaborateMLType tyargs
            (* FIXME: will we need to return the env here for any reason? *)
             (* FIXME: error handling *)
            val SOME (bomTyc, bomTy, {env, bomEnv}) =
              extendEnvs (tyargs', tyc, maybeId)
            val cons' = map (translateImportCon (bomEnv, bomTy, tyargs')) cons

            (* Add the constructors to the tycon *)
            val _ = (fn (CoreBOM.TyCon.TyC {definition,...}) =>
              definition := map (fn (valId, bomVal) =>  CoreBOM.ConsDef (
                CoreBOM.ValId.truncateToBOMId valId,
                SOME (CoreBOM.Val.typeOf bomVal))) cons') bomTyc

            val bomEnv' = foldl (fn ((valId, bomVal), bomEnv) =>
             BOMEnv.ValEnv.extend (bomEnv, valId, bomVal)) bomEnv cons'

          in
            raise Fail "TODO(wings): BOM.Import.Datatype elaboration not implemented"
            (* bomEnv' *)
          end

      | BOM.Import.Exn (longcon, maybeTy, maybeId) =>
          let
            (* FIXME: this is broken b/c of the way tycons are handled *)
            val (newValId, newVal) = translateCon (bomEnv,
              CoreBOM.BOMType.Exn, Vector.fromList [], longcon, maybeTy,
                maybeId)
          in
            raise Fail "TODO(wings): BOM.Import.Exn elaboration not implemented"
            (* BOMEnv.ValEnv.extend (bomEnv, newValId, newVal) *)
          end

    end
end
