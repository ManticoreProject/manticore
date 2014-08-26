functor ElaborateBOMCore(S: ELABORATE_BOMCORE_STRUCTS) = struct
  open S

  structure AstBOM = Ast.AstBOM

  fun app3 f (x, y, z) = (f x, f y, f z)


  fun elaborateBomType (astTy: AstBOM.BomType.t,
      tyEnvs as {env = env':Env.t, bomEnv = bomEnv': BOMEnv.t}): CoreBOM.BomType.t =
    let
      fun check (f: 'a -> CoreBOM.BomType.t) (x: 'a option) =
        case x of
          SOME y => f y
        | NONE => ((Control.error (
              AstBOM.BomType.region astTy,
              AstBOM.BomType.layout astTy,
              Layout.str "Error checking BomType."));
            CoreBOM.BomType.errorFromAst astTy)
      fun doElaborate ty = elaborateBomType (ty, tyEnvs)
      fun keepRegion newNode = CoreBOM.BomType.keepRegion (
        (fn _ => newNode), AstBOM.BomType.dest astTy)
      val passThrough = fn () => CoreBOM.BomType.fromAst astTy
    in
      case AstBOM.BomType.node astTy of
        AstBOM.BomType.Param tyParam =>
          check
            (* (fn x => CoreBOM.BomType.keepRegion ((fn _ => AstBOM.BomType.Param x), *)
            (*   AstBOM.BomType.dest astTy)) *)
            (fn _ => CoreBOM.BomType.fromAst astTy)
            (BOMEnv.TyParamEnv.lookup (bomEnv', tyParam))
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
            val tyArgs = CoreBOM.flattenFromAst maybeTyArgs
          in
            case (CoreBOM.TyId.fromLongTyId longTyId) of
              tyId as BomTy bomTy => BOMEnv.TyEnv.lookup (bomEnv, tyId)
            | _ => CoreBOM.BomType.errorFromAst astTy (* FIXME *)
          end
      |  _ => CoreBOM.BomType.errorFromAst astTy (* FIXME *)
    end

  fun checkArityMatches (typeDefn, bomTy) =
  (* TODO: write this *)
    (* if (BOMEnv.TypeDefn.arity typeDefn) = (CoreBOM.BomType.arity bomTy) then  *)
    (*   typeDefn *)
    (* else  *)
    (*   BOMEnv.TypeDefn.error *)
    typeDefn


  fun elaborateBomDec (dec: AstBOM.Definition.t,
      {env = env':Env.t, bomEnv = bomEnv': BOMEnv.t}) =
    case AstBOM.Definition.node dec of
      AstBOM.Definition.TypeDefn (bomId, maybeTyParams, bomTy) =>
        let
          val tyParams = CoreBOM.TyParam.flattenFromAst maybeTyParams
          val newBomEnv: BOMEnv.t = foldr
            (fn (tyP: AstBOM.TyParam.t, bEnv)
              => BOMEnv.TyParamEnv.extend (bEnv, tyP))
            bomEnv'
            tyParams
          val newTy = elaborateBomType (bomTy, {env = env', bomEnv = newBomEnv})
          (* alias is the only kind we can get from this *)
          val newTyAlias = checkArityMatches (
            BOMEnv.TypeDefn.TyAlias ({
              params = BOMEnv.TyParamEnv.getParams newBomEnv,
              ty = newTy
             }),
             newTy)

          val newId = CoreBOM.TyId.fromAstBomId bomId

          val newEnv = BOMEnv.TyEnv.extend (bomEnv', newId, newTyAlias)
        in
          (CoreML.Dec.BomDec, newEnv)
        end
    (* TODO: the other cases *)



    (* (CoreML.Dec.BomDec, bomEnv') *)
end
