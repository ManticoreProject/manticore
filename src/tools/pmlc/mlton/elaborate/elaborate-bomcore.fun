functor ElaborateBOMCore(S: ELABORATE_BOMCORE_STRUCTS) = struct
  open S

  structure AstBOM = Ast.AstBOM

  fun app3 f (x, y, z) = (f x, f y, f z)
  fun error (getRegion, getLayout, errorVal, element) msg =
    (Control.error (getRegion element, getLayout element, Layout.str msg)
    ; errorVal)
  fun check (error: string -> 'b) (x: 'a option, msg: string) (f: 'a -> 'b) =
    case x of
      SOME y => f y
    | NONE => error msg


  fun elaborateBomType (astTy: AstBOM.BomType.t,
      tyEnvs as {env = env:Env.t, bomEnv = bomEnv: BOMEnv.t}): CoreBOM.BomType.t =
    let
      val error: string -> CoreBOM.BomType.t =
        error (AstBOM.BomType.region, AstBOM.BomType.layout,
          CoreBOM.BomType.Error,
          astTy)

      (* Need to put whole body here to get around value restriction *)
      fun check (x: 'a option, msg: string) (f: 'a -> CoreBOM.BomType.t) =
        case x of
          SOME y => f y
        | NONE => error  msg
      fun doElaborate ty = elaborateBomType (ty, tyEnvs)
      (* fun keepRegion newNode = CoreBOM.BomType.keepRegion ( *)
      (*   (fn _ => newNode), AstBOM.BomType.dest astTy) *)

      fun defnArityMatches (input as (defn, tyArgs)) =
        if (BOMEnv.TypeDefn.arity defn) = (length tyArgs) then
          SOME input
        else
          NONE

      fun recordLabelsOkay fields =
        let
          fun fieldToIndex field =
            case field of
              AstBOM.Field.Immutable (index, _) => index
            | AstBOM.Field.Mutable (index, _) => index


          fun loop (labels, lastLabel) =
            case labels of
              l::ls =>
                if l > lastLabel then
                  loop (ls, l)
                else
                  false
            | [] => true

          val _ = print (
            String.concat (map (Layout.toString o AstBOM.Field.layout)
              fields))
        in
          if loop (map (fieldToIndex o AstBOM.Field.node) fields,
            IntInf.fromInt ~1)
          then
            SOME fields
          else
            NONE
        end


    in
      case AstBOM.BomType.node astTy of
        AstBOM.BomType.Param tyParam =>
          check
            (BOMEnv.TyParamEnv.lookup (bomEnv, tyParam), "unbound typaram")
            (fn tyParam => CoreBOM.BomType.Param tyParam)
      | AstBOM.BomType.Tuple tys =>
          CoreBOM.BomType.Tuple (map doElaborate tys)
      | AstBOM.BomType.Fun funTys =>
          CoreBOM.BomType.Fun (let
            val (dom, cont, rng) = app3 (map doElaborate) funTys
          in
            {dom=dom, cont=cont, rng=rng}
          end)
      | AstBOM.BomType.Any => CoreBOM.BomType.Any
      | AstBOM.BomType.VProc => CoreBOM.BomType.VProc
      | AstBOM.BomType.Cont maybeTyArgs =>
          CoreBOM.BomType.Cont (map doElaborate (
            CoreBOM.TyArgs.flattenFromAst maybeTyArgs))
      | AstBOM.BomType.Addr ty =>
          CoreBOM.BomType.Addr (doElaborate ty)
      | AstBOM.BomType.Raw ty => CoreBOM.BomType.Raw (
          CoreBOM.RawTy.fromAst ty)
      | AstBOM.BomType.LongId (longTyId, maybeTyArgs) =>
          let
            val tyArgs = map doElaborate (CoreBOM.TyArgs.flattenFromAst maybeTyArgs)
            val tyId = CoreBOM.TyId.fromLongTyId longTyId
          in
            check
             (BOMEnv.TyEnv.lookup (bomEnv, tyId), "undefined type")
             (fn defn =>
               check
                 (BOMEnv.TypeDefn.applyToArgs (defn, tyArgs), "arity mismatch")
                  (fn x => x))
          end
      | AstBOM.BomType.Record fields =>
          check
            (recordLabelsOkay fields, "labels must be strictly increasing")
            (fn fields => CoreBOM.BomType.Record (map (fn field' =>
              elaborateField (field', tyEnvs)) fields))
    end
  and elaborateField (astField: AstBOM.Field.t,
     tyEnvs as {env = env:Env.t, bomEnv = bomEnv: BOMEnv.t}): CoreBOM.Field.t =
    let
      val (constructor, index, astTy) =
        case AstBOM.Field.node astField of
          AstBOM.Field.Immutable (index, astTy) =>
            (CoreBOM.Field.Immutable, index, astTy)
        | AstBOM.Field.Mutable (index, astTy) =>
            (CoreBOM.Field.Mutable, index, astTy)
    in
      CoreBOM.Field.wrap (constructor (index, elaborateBomType (astTy, tyEnvs)))
    end

  fun instanceTyToTy (tyId: AstBOM.LongTyId.t, tyArgs: AstBOM.TyArgs.t):
      AstBOM.BomType.t =
    let
      val wholeRegion = Region.append (
        AstBOM.LongTyId.region tyId,
        AstBOM.TyArgs.region tyArgs)
    in
      AstBOM.BomType.makeRegion (
        AstBOM.BomType.LongId (tyId, SOME tyArgs),
        wholeRegion)
    end

  fun extendEnvForTyParams (bomEnv: BOMEnv.t, tyParams: AstBOM.TyParam.t list) =
    foldl
      (fn (tyP: AstBOM.TyParam.t, bEnv)
        => BOMEnv.TyParamEnv.extend (bEnv, tyP))
      bomEnv
      tyParams

  fun extendEnvForTyParams' (bomEnv, maybeTyParams: AstBOM.TyParams.t option) =
    extendEnvForTyParams (bomEnv, CoreBOM.TyParam.flattenFromAst maybeTyParams)

  fun checkTyAliasArity (ty, params, error): BOMEnv.TyAlias.t =
    if (CoreBOM.BomType.arity ty) = (length params) then
      {ty = ty, params = params}
    else
      error "arity mismatch"


  fun varPatToTy (pat, tyEnvs) =
    let
      val error = error (AstBOM.VarPat.region, AstBOM.VarPat.layout,
        CoreBOM.BomType.Error, pat)
      val check = check error
      val maybeTy =
        case AstBOM.VarPat.node pat of
          AstBOM.VarPat.Var (id, maybeTy) => maybeTy
        | AstBOM.VarPat.Wild maybeTy => maybeTy
    in
      check
        (maybeTy, "varpat missing type annotation")
        (fn ty => elaborateBomType (ty, tyEnvs))
    end


  fun extendEnvForFun (funDef: AstBOM.FunDef.t,
      tyEnvs as {env = env:Env.t, bomEnv = bomEnv: BOMEnv.t}) =
    let
      val AstBOM.FunDef.Def (
          _, id, maybeTyParams, domPats, contPats, rngTys, _) =
        AstBOM.FunDef.node funDef
      val envWithTyParams = extendEnvForTyParams' (bomEnv, maybeTyParams)
      val tyEnvs' = {env = env, bomEnv = envWithTyParams}
      fun patsToTys pats = map
        (fn pat => varPatToTy (pat, tyEnvs'))
        pats
      val domTys = patsToTys domPats
      val contTys = patsToTys contPats
      val rngTys' = map (fn ty => elaborateBomType (ty, tyEnvs')) rngTys
      val funTy = CoreBOM.BomType.Fun {
          dom = domTys,
          cont = contTys,
          rng = rngTys'
        }
      val newTyAlias = checkTyAliasArity (funTy,
        BOMEnv.TyParamEnv.getParams envWithTyParams,
        error (AstBOM.FunDef.region, AstBOM.FunDef.layout,
          {ty = CoreBOM.BomType.Error, params = []}, funDef))
    in
      ({
        env = env,
        bomEnv = BOMEnv.ValEnv.extend (bomEnv, CoreBOM.ValId.fromAstBomId id,
          newTyAlias)
      }, funTy)
    end

  fun dataTypeDefToTyIdAndParams dtDef =
    let
      val (tyId, tyParams) =
        (fn AstBOM.DataTypeDef.ConsDefs (astId, maybeTyParams, _) =>
          (CoreBOM.TyId.fromAstBomId astId,
          CoreBOM.TyParam.flattenFromAst maybeTyParams)) (
          AstBOM.DataTypeDef.node dtDef)
    in
      (tyId, tyParams)
    end


  fun extendEnvForDataTypeDef (dtDef: AstBOM.DataTypeDef.t,
      tyEnvs as {env:Env.t, bomEnv: BOMEnv.t}) =
    let
      val (tyId, tyParams) = dataTypeDefToTyIdAndParams dtDef
    in
      {
        env = env,
        bomEnv = BOMEnv.TyEnv.extend (bomEnv,
          tyId,
          BOMEnv.TypeDefn.newCon (CoreBOM.TyCon.TyC {
              id = tyId,
              definition = ref [],
              params = map CoreBOM.TyParam.fromAst tyParams
            }))
      }
    end


  fun elaborateDataConsDef (dtCon: AstBOM.DataConsDef.t,
      datatypeTy: CoreBOM.BomType.t,
      tyEnvs as {env:Env.t, bomEnv: BOMEnv.t}):
      (CoreBOM.DataConsDef.t * BOMEnv.t) =
    let
      val AstBOM.DataConsDef.ConsDef (astId, maybeTy) =
        AstBOM.DataConsDef.node dtCon
      val params = CoreBOM.BomType.uniqueTyParams datatypeTy
      val (maybeArgTy: CoreBOM.BomType.t option, valTy: BOMEnv.TyAlias.t) =
        case (maybeTy: AstBOM.BomType.t option) of
          SOME (argTy: AstBOM.BomType.t) =>
            let
              val argTy' = elaborateBomType (argTy, tyEnvs)
            in
              (SOME argTy', {
                params = params,
                ty = CoreBOM.BomType.Fun {
                    dom = [argTy'],
                    cont = [],
                    rng = [datatypeTy]
                  }
                })
            end
        | NONE =>
            (NONE, {params = params, ty = datatypeTy})
    in
      (CoreBOM.DataConsDef.wrap (CoreBOM.DataConsDef.ConsDef (
        CoreBOM.BomId.fromAst astId, maybeArgTy)),
      BOMEnv.ValEnv.extend (bomEnv, CoreBOM.ValId.fromAstBomId astId, valTy))
    end


  fun elaborateDataConsDefs (dtCons: AstBOM.DataConsDef.t list,
      datatypeTy: CoreBOM.BomType.t, tyEnvs as {env:Env.t, bomEnv: BOMEnv.t}) =
    foldr (fn (newAstCon, (oldEnv, oldCons)) =>
      let
        val (newCon, newEnv) = elaborateDataConsDef (
          newAstCon, datatypeTy, {env=env, bomEnv=oldEnv})
      in
        (newEnv, newCon::oldCons)
      end) (bomEnv, []) dtCons


  fun elaborateDataTypeDef (dtDef: AstBOM.DataTypeDef.t,
      tyEnvs as {env:Env.t, bomEnv: BOMEnv.t}) =
    let
      val error = error (AstBOM.DataTypeDef.region, AstBOM.DataTypeDef.layout,
        tyEnvs, dtDef)
      val check = check error

      val (tyId, tyParams) = dataTypeDefToTyIdAndParams dtDef
      val SOME (tyConOfDatatype) = BOMEnv.TyEnv.lookupCon (bomEnv, tyId)
      val envWithTyParams = extendEnvForTyParams (bomEnv, tyParams)
      val newEnvs =
        case AstBOM.DataTypeDef.node dtDef of
          AstBOM.DataTypeDef.ConsDefs (_, _, consDefs) =>
            let
              val (newEnv, dtCons) = elaborateDataConsDefs (consDefs,
                CoreBOM.TyCon.toBomTy tyConOfDatatype,
                {env = env, bomEnv = envWithTyParams})
              val CoreBOM.TyCon.TyC {definition=definition,params=params,...} =
                tyConOfDatatype
            in
              definition := dtCons
              ; {
                env = env,
                bomEnv = newEnv
              }
            end
    in
      newEnvs
    end

  fun elaborateFunDef (funDef: AstBOM.FunDef.t, funTy: CoreBOM.BomType.t,
      tyEnvs as {env:Env.t, bomEnv: BOMEnv.t}) =
    let
        (* TODO: find the appropriate error value here *)
      val error = error (AstBOM.FunDef.region, AstBOM.FunDef.layout,
        (), funDef)
      val check = check error
    (* TODO: extend the val env to hold the function parameters *)
    (* TODO: typecheck the body with the params in scope *)
    (* TODO: check the body has the same type as the range type *)
    in
      ()
    end

  fun elaborateBomDec (dec: AstBOM.Definition.t,
      tyEnvs as {env = env:Env.t, bomEnv = bomEnv: BOMEnv.t}) =
    case AstBOM.Definition.node dec of
      AstBOM.Definition.Datatype dtdefs =>
        let
          val envWithTys = foldl extendEnvForDataTypeDef tyEnvs dtdefs
          val envWithDefs = foldl elaborateDataTypeDef envWithTys dtdefs
        in
          (CoreML.Dec.BomDec, #bomEnv envWithDefs)
        end
    | AstBOM.Definition.DatatypeAlias (bomId, maybeTyParams, longTyId) =>
        let
          val error = error (AstBOM.LongTyId.region, AstBOM.LongTyId.layout,
            BOMEnv.TypeDefn.error, longTyId)
          val check = check error
          val tyId = CoreBOM.TyId.fromAstBomId bomId

          val tyConDefn =
            (* TODO: can't get this to compile if the last line extends env *)
            check
              (BOMEnv.TyEnv.lookup (bomEnv,
                CoreBOM.TyId.fromLongTyId longTyId): BOMEnv.TypeDefn.t option,
                  "undefined type")
                (fn tyDefn: BOMEnv.TypeDefn.t => check
                  ((BOMEnv.TypeDefn.isCon tyDefn): BOMEnv.TypeDefn.t option,
                    "not a datatype")
                  (fn x => x))
        in
          (CoreML.Dec.BomDec, BOMEnv.TyEnv.extend (bomEnv, tyId, tyConDefn))
        end

    | AstBOM.Definition.TypeDefn (bomId, maybeTyParams, bomTy) =>
        let
          val error = error (AstBOM.BomType.region, AstBOM.BomType.layout,
            BOMEnv.TypeDefn.error, bomTy)
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

          val envWithTyParams: BOMEnv.t = extendEnvForTyParams' (
            bomEnv, maybeTyParams)
          val newTy = elaborateBomType (
            bomTy, {env = env, bomEnv = envWithTyParams})
          (* alias is the only kind we can get from this *)
          val newTyAlias = checkArityMatches (
            BOMEnv.TypeDefn.newAlias ({
              params = BOMEnv.TyParamEnv.getParams envWithTyParams,
              ty = newTy
             }),
             newTy)

          val newId = CoreBOM.TyId.fromAstBomId bomId

          val newEnv = BOMEnv.TyEnv.extend (bomEnv, newId, newTyAlias)
        in
          (CoreML.Dec.BomDec, newEnv)
        end
    | AstBOM.Definition.Fun funDefs =>
        let
        (* TODO: add a distinct FunTy.t in CoreBOM that we can return
        from this function. it doesn't need to be wrapped *)

          val (envWithFns, funTys) =
            foldr (fn (funDef, (oldEnv, oldTys)) =>
                let
                  val (newEnv, newTy) = extendEnvForFun (funDef, oldEnv)
                in
                  (newEnv, newTy::oldTys)
                end) (tyEnvs, []) funDefs

          val _ = ListPair.map
            (fn (funDef, funTy) => elaborateFunDef (funDef, funTy, envWithFns))
            (funDefs, funTys)
        (* TODO: check the body *)
        in
          (CoreML.Dec.BomDec, #bomEnv envWithFns)
        end
    | AstBOM.Definition.InstanceType instanceTy =>
        let
          val ty = elaborateBomType (instanceTyToTy instanceTy, tyEnvs)
        (* TODO: deal with extending the environment *)
        in
          (CoreML.Dec.BomDec, bomEnv)
        end
    | _ => raise Fail "not implemented"
    (* TODO: the other cases *)

    (* (CoreML.Dec.BomDec, bomEnv) *)
end
