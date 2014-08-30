functor ElaborateBOMCore(S: ELABORATE_BOMCORE_STRUCTS) = struct
  open S

  structure AstBOM = Ast.AstBOM

  fun app3 f (x, y, z) = (f x, f y, f z)

  fun elaborateBomType (astTy: AstBOM.BomType.t,
      tyEnvs as {env = env:Env.t, bomEnv = bomEnv: BOMEnv.t}): CoreBOM.BomType.t =
    let
      fun error (msg: string) = (Control.error (
        AstBOM.BomType.region astTy,
        AstBOM.BomType.layout astTy,
        Layout.str ("Error checking BomType: " ^ msg))
        ; CoreBOM.BomType.errorFromAst astTy)
      fun check (x: 'a option, msg: string) (f: 'a -> CoreBOM.BomType.t) =
        case x of
          SOME y => f y
        | NONE => error msg
      fun doElaborate ty = elaborateBomType (ty, tyEnvs)
      fun keepRegion newNode = CoreBOM.BomType.keepRegion (
        (fn _ => newNode), AstBOM.BomType.dest astTy)

      fun defnArityMatches (input as (defn, tyArgs)) =
        if (BOMEnv.TypeDefn.arity defn) = (length tyArgs) then
          SOME input
        else
          NONE

      val passThrough = fn () => CoreBOM.BomType.fromAst astTy
    in
      case AstBOM.BomType.node astTy of
        AstBOM.BomType.Param tyParam =>
          check
            (BOMEnv.TyParamEnv.lookup (bomEnv, tyParam), "typaram not found")
            (fn _ => CoreBOM.BomType.fromAst astTy)
      | AstBOM.BomType.Tuple tys =>
          keepRegion (CoreBOM.BomType.Tuple (map doElaborate tys))
      | AstBOM.BomType.Fun funTys =>
          keepRegion (CoreBOM.BomType.Fun (let
            val (dom, cont, rng) = app3 (map doElaborate) funTys
          in
            {dom=dom, cont=cont, rng=rng}
          end))
      | AstBOM.BomType.Any => passThrough ()
      | AstBOM.BomType.VProc => passThrough ()
      | AstBOM.BomType.Cont maybeTyArgs =>
          keepRegion (CoreBOM.BomType.Cont (map doElaborate (
            CoreBOM.TyArgs.flattenFromAst maybeTyArgs)))
      | AstBOM.BomType.Addr ty =>
          keepRegion (CoreBOM.BomType.Addr (doElaborate ty))
      | AstBOM.BomType.Raw ty => passThrough ()
      | AstBOM.BomType.LongId (longTyId, maybeTyArgs) =>
          let
            val tyArgs = map doElaborate (CoreBOM.TyArgs.flattenFromAst maybeTyArgs)
            val tyId = CoreBOM.TyId.fromLongTyId longTyId
          in
            check
             (BOMEnv.TyEnv.lookup (bomEnv, tyId), "type not found")
             (fn defn =>
               check
                 (defnArityMatches (defn, tyArgs), "arity mismatch")
                  BOMEnv.TypeDefn.applyToArgs)

            (* case (CoreBOM.TyId.fromLongTyId longTyId) of *)
            (*   (tyId as CoreBOM.TyId.BomTy bomTy) => *)
            (*    check *)
            (*      (BOMEnv.TyEnv.lookup (bomEnv, tyId), "type not found") *)
            (*      (fn defn => *)
            (*        check *)
            (*          (defnArityMatches (defn, tyArgs), "arity mismatch") *)
            (*           BOMEnv.TypeDefn.applyToArgs) *)
            (* | _ => error "not implemented" (* FIXME *) *)
          end
      |  _ => error "not implemented" (* FIXME *)
    end


  fun elaborateBomDec (dec: AstBOM.Definition.t,
      {env = env:Env.t, bomEnv = bomEnv: BOMEnv.t}) =
    case AstBOM.Definition.node dec of
      AstBOM.Definition.TypeDefn (bomId, maybeTyParams, bomTy) =>
        let
          fun error msg = (Control.error (
            AstBOM.BomType.region bomTy,
            AstBOM.BomType.layout bomTy,
            Layout.str msg)
            ; BOMEnv.TypeDefn.error)
          fun checkArityMatches (typeDefn, ty) =
            let
              val defnArity = BOMEnv.TypeDefn.arity typeDefn
              val tyArity = CoreBOM.BomType.arity ty
            in
              if defnArity = tyArity then
                typeDefn
              else
                error "arity mismatch"
            end

          val tyParams = CoreBOM.TyParam.flattenFromAst maybeTyParams
          val envWithTyParams: BOMEnv.t = foldr
            (fn (tyP: AstBOM.TyParam.t, bEnv)
              => BOMEnv.TyParamEnv.extend (bEnv, tyP))
            bomEnv
            tyParams
          val newTy = elaborateBomType (
            bomTy, {env = env, bomEnv = envWithTyParams})
          (* alias is the only kind we can get from this *)
          val newTyAlias = checkArityMatches (
            BOMEnv.TypeDefn.Alias ({
              params = BOMEnv.TyParamEnv.getParams envWithTyParams,
              ty = newTy
             }),
             newTy)

          val newId = CoreBOM.TyId.fromAstBomId bomId

          val newEnv = BOMEnv.TyEnv.extend (bomEnv, newId, newTyAlias)
        in
          (CoreML.Dec.BomDec, newEnv)
        end
    (* TODO: the other cases *)



    (* (CoreML.Dec.BomDec, bomEnv) *)
end
