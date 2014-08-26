functor ElaborateBOMCore(S: ELABORATE_BOMCORE_STRUCTS) = struct
  open S

  structure AstBOM = Ast.AstBOM

  fun app3 f (x, y, z) = (f x, f y, f z)


  fun elaborateBomType (astTy: AstBOM.BomType.t,
      tyEnvs as {env = env:Env.t, bomEnv = bomEnv: BOMEnv.t}): CoreBOM.BomType.t =
    let
      fun check (f: 'a -> CoreBOM.BomType.t) (x: 'a option, msg: string) =
        case x of
          SOME y => f y
        | NONE => ((Control.error (
              AstBOM.BomType.region astTy,
              AstBOM.BomType.layout astTy,
              Layout.str ("Error checking BomType: " ^ msg)));
            CoreBOM.BomType.errorFromAst astTy)
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
            (* (fn x => CoreBOM.BomType.keepRegion ((fn _ => AstBOM.BomType.Param x), *)
            (*   AstBOM.BomType.dest astTy)) *)
            (fn _ => CoreBOM.BomType.fromAst astTy)
            (BOMEnv.TyParamEnv.lookup (bomEnv, tyParam), "typaram not found")
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
          in
            case (CoreBOM.TyId.fromLongTyId longTyId) of
              (tyId as CoreBOM.TyId.BomTy bomTy) =>
               check
                 (fn defn =>
                   check
                     BOMEnv.TypeDefn.applyToArgs
                     (defnArityMatches (defn, tyArgs), "arity mismatch"))
                 (BOMEnv.TyEnv.lookup (bomEnv, tyId), "type not found")
            | _ => CoreBOM.BomType.errorFromAst astTy (* FIXME *)
          end
      |  _ => CoreBOM.BomType.errorFromAst astTy (* FIXME *)
    end

  (* fun elaborateBomType (astTy: AstBOM.BomType.t, *)
  (*     tyEnvs as {env = env:Env.t, bomEnv = bomEnv: BOMEnv.t}): BOMEnv.TypeDefn.t = *)
  (*   let *)
  (*     (* fun check (f: a -> CoreBOM.BomType.t) (x: a option) = *) *)
  (*     (*   case x of *) *)
  (*     (*     SOME y => f y *) *)
  (*     (*   | NONE => ((Control.error ( *) *)
  (*     (*         AstBOM.BomType.region astTy, *) *)
  (*     (*         AstBOM.BomType.layout astTy, *) *)
  (*     (*         Layout.str "Error checking BomType.")); *) *)
  (*     (*       CoreBOM.BomType.errorFromAst astTy) *) *)
  (*     (* fun doElaborate ty = elaborateBomType (ty, tyEnvs) *) *)
  (*     (* fun keepRegion newNode = CoreBOM.BomType.keepRegion ( *) *)
  (*     (*   (fn _ => newNode), AstBOM.BomType.dest astTy) *) *)
  (*     (* val passThrough = fn () => CoreBOM.BomType.fromAst astTy *) *)
  (*     (* val newTy = CoreBOM.BomType.fromAst astTy *) *)

  (*     fun aliasOverParams tyParams = BOMEnv.TypeDefn.TyAlias ({ *)
  (*       params = tyParams, *)
  (*       ty = CoreBOM.BomType.fromAst astTy *)
  (*     }) *)

  (*     fun flattenToError maybeResult = *)
  (*       case maybeResult of *)
  (*         SOME result => result *)
  (*       | NONE => BOMEnv.TypeDefn.error *)

  (*     fun ifTyParamFound (tyParam, resultFn) = *)
  (*       case BOMEnv.TyParamEnv.lookup tyParam of *)
  (*         SOME tyParam' => SOME (resultFn tyParam') *)
  (*       | NONE => NONE *)
  (*   in *)
  (*     flattenToError (case AstBOM.BomType.node astTy of *)
  (*       AstBOM.BomType.Param tyParam => *)
  (*         ifTyParamFound (tyParam, fn tyParam' => aliasOverParams [tyParam']) *)
  (*     (* | AstBOM.BomType.Tuple tys =>  *) *)
  (*     (* | AstBOM.BomType.Fun funTys => *) *)
  (*     (* | AstBOM.BomType.Any => passThrough () *) *)
  (*     (* | AstBOM.BomType.VProc => passThrough () *) *)
  (*     (* | AstBOM.BomType.Cont maybeTyArgs => *) *)
  (*     (* | AstBOM.BomType.Addr ty => *) *)
  (*     (* | AstBOM.BomType.Raw ty => passThrough () *) *)
  (*     (* | AstBOM.BomType.LongId (longTyId, maybeTyArgs) => *) *)
  (*     |  _ => NONE (* FIXME *)) *)
  (*   end *)

  fun elaborateBomDec (dec: AstBOM.Definition.t,
      {env = env:Env.t, bomEnv = bomEnv: BOMEnv.t}) =
    case AstBOM.Definition.node dec of
      AstBOM.Definition.TypeDefn (bomId, maybeTyParams, bomTy) =>
        let
          fun checkArityMatches (typeDefn, ty) =
            let
              val defnArity = BOMEnv.TypeDefn.arity typeDefn
              val tyArity = CoreBOM.BomType.arity ty
            in
              if defnArity = tyArity then
                typeDefn
              else
                ((Control.error (
                  AstBOM.BomType.region bomTy,
                  AstBOM.BomType.layout bomTy,
                  Layout.str (String.concat [
                    "Arity mismatch: ",
                    Int.toString defnArity,
                    " vs ",
                    Int.toString tyArity
                  ])));         (* TODO: make this conform with other errors *)
                BOMEnv.TypeDefn.error)
            end

          val tyParams = CoreBOM.TyParam.flattenFromAst maybeTyParams
          val newBomEnv: BOMEnv.t = foldr
            (fn (tyP: AstBOM.TyParam.t, bEnv)
              => BOMEnv.TyParamEnv.extend (bEnv, tyP))
            bomEnv
            tyParams
          val newTy = elaborateBomType (bomTy, {env = env, bomEnv = newBomEnv})
          (* alias is the only kind we can get from this *)
          val newTyAlias = checkArityMatches (
            BOMEnv.TypeDefn.Alias ({
              params = BOMEnv.TyParamEnv.getParams newBomEnv,
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
