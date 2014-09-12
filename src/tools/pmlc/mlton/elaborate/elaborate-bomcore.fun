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
        error (AstBOM.BomType.region,
          AstBOM.BomType.layout,
          CoreBOM.BomType.errorFromAst astTy,
          astTy)

      (* Need to put whole body here to get around value restriction *)
      fun check (x: 'a option, msg: string) (f: 'a -> CoreBOM.BomType.t) =
        case x of
          SOME y => f y
        | NONE => error  msg
      fun doElaborate ty = elaborateBomType (ty, tyEnvs)
      fun keepRegion newNode = CoreBOM.BomType.keepRegion (
        (fn _ => newNode), AstBOM.BomType.dest astTy)

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
            (fn tyParam => keepRegion (CoreBOM.BomType.Param tyParam))
      | AstBOM.BomType.Tuple tys =>
          keepRegion (CoreBOM.BomType.Tuple (map doElaborate tys))
      | AstBOM.BomType.Fun funTys =>
          keepRegion (CoreBOM.BomType.Fun (let
            val (dom, cont, rng) = app3 (map doElaborate) funTys
          in
            {dom=dom, cont=cont, rng=rng}
          end))
      | AstBOM.BomType.Any => keepRegion (CoreBOM.BomType.Any)
      | AstBOM.BomType.VProc => keepRegion (CoreBOM.BomType.VProc)
      | AstBOM.BomType.Cont maybeTyArgs =>
          keepRegion (CoreBOM.BomType.Cont (map doElaborate (
            CoreBOM.TyArgs.flattenFromAst maybeTyArgs)))
      | AstBOM.BomType.Addr ty =>
          keepRegion (CoreBOM.BomType.Addr (doElaborate ty))
      | AstBOM.BomType.Raw ty => keepRegion (CoreBOM.BomType.Raw (
          CoreBOM.RawTy.fromAst ty))
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
            (fn fields => keepRegion (CoreBOM.BomType.Record (map
              (fn field' => elaborateField (field', tyEnvs)) fields)))
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
      (CoreBOM.Field.keepRegion ((fn _ =>
        constructor (index, elaborateBomType (astTy, tyEnvs))),
       AstBOM.Field.dest astField))
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
      val error = error (
        AstBOM.VarPat.region,
        AstBOM.VarPat.layout,
        CoreBOM.BomType.makeRegion (
          CoreBOM.BomType.Error,
          AstBOM.VarPat.region pat),
        pat)
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


  fun extendEnvForFun (fundef: AstBOM.FunDef.t,
      tyEnvs as {env = env:Env.t, bomEnv = bomEnv: BOMEnv.t}) =
    let
      val AstBOM.FunDef.Def (
          _, id, maybeTyParams, domPats, contPats, rngTys, _) =
        AstBOM.FunDef.node fundef
      val envWithTyParams = extendEnvForTyParams' (bomEnv, maybeTyParams)
      val tyEnvs' = {env = env, bomEnv = envWithTyParams}
      fun patsToTys pats = map
        (fn pat => varPatToTy (pat, tyEnvs'))
        pats
      val domTys = patsToTys domPats
      val contTys = patsToTys contPats
      val rngTys' = map (fn ty => elaborateBomType (ty, tyEnvs')) rngTys
      val funTy = CoreBOM.BomType.makeRegion (
        CoreBOM.BomType.Fun {
          dom = domTys,
          cont = contTys,
          rng = rngTys'
        }, AstBOM.FunDef.region fundef)
      val newTyAlias = checkTyAliasArity (funTy,
        BOMEnv.TyParamEnv.getParams envWithTyParams,
        error (AstBOM.FunDef.region,
        AstBOM.FunDef.layout,
        {ty = CoreBOM.BomType.keepRegion (
            fn _ => CoreBOM.BomType.Error,
            CoreBOM.BomType.dest funTy),
          params = []},
        fundef))
    in
      {
        env = env,
        bomEnv = BOMEnv.ValEnv.extend (bomEnv, CoreBOM.ValId.fromAstBomId id,
          newTyAlias)
      }
    end

  fun dataTypeDefToTyIdAndParams dtDef =
    (fn (astId, maybeTyParams) => (CoreBOM.TyId.fromAstBomId astId,
        CoreBOM.TyParam.flattenFromAst maybeTyParams))
    (case AstBOM.DataTypeDef.node dtDef of
        AstBOM.DataTypeDef.ConsDefs (bomId, maybeTyParams, _) =>
          (bomId, maybeTyParams)
     | AstBOM.DataTypeDef.SimpleDef (bomId, maybeTyParams, _) =>
          (bomId, maybeTyParams))

  fun extendEnvForDataTypeDef (dtDef: AstBOM.DataTypeDef.t,
      tyEnvs as {env:Env.t, bomEnv: BOMEnv.t}) =
    let
      val (tyId, tyParams) = dataTypeDefToTyIdAndParams dtDef
    in
      {
        env = env,
        bomEnv = BOMEnv.TyEnv.extend (bomEnv,
          tyId,
          BOMEnv.TypeDefn.Con (CoreBOM.TyCon.makeRegion (
            CoreBOM.TyCon.TyC {
              id = tyId,
              definition = ref [],
              params = map CoreBOM.TyParam.fromAst tyParams
            }, AstBOM.DataTypeDef.region dtDef)))
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
      (* val valId = CoreBOM.ValId.fromAstBomId astId *)
      val (maybeArgTy: CoreBOM.BomType.t option, valTy: BOMEnv.TyAlias.t) =
      (* TODO: handle it correctly when the con isn't parameterized *)
        case (maybeTy: AstBOM.BomType.t option) of
          SOME (argTy: AstBOM.BomType.t) =>
            let
              val argTy' = elaborateBomType (argTy, tyEnvs)
            in
              (SOME argTy', {
                params = params,
                ty = CoreBOM.BomType.makeRegion (
                  CoreBOM.BomType.Fun {
                    dom = [argTy'],
                    cont = [],
                    rng = [datatypeTy]
                  }, AstBOM.DataConsDef.region dtCon)
                })
            end
        | NONE =>
            (NONE, {params = params, ty = datatypeTy})
    in
      (CoreBOM.DataConsDef.makeRegion (CoreBOM.DataConsDef.ConsDef (
        CoreBOM.BomId.fromAst astId, maybeArgTy),
        AstBOM.DataConsDef.region dtCon),
      BOMEnv.ValEnv.extend (bomEnv,
        CoreBOM.ValId.fromAstBomId astId, valTy))
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
      val error = error (
        AstBOM.DataTypeDef.region,
        AstBOM.DataTypeDef.layout,
        tyEnvs,
        dtDef)
      val check = check error

      val (tyId, tyParams) = dataTypeDefToTyIdAndParams dtDef
      val SOME (BOMEnv.TypeDefn.Con tyConOfDatatype) =
        BOMEnv.TyEnv.lookup (bomEnv, tyId)
      val envWithTyParams = extendEnvForTyParams (bomEnv, tyParams)
      val newEnvs =
        case AstBOM.DataTypeDef.node dtDef of
          AstBOM.DataTypeDef.ConsDefs (_, _, consDefs) =>
            let
              val (newEnv, dtCons) = elaborateDataConsDefs (consDefs,
                CoreBOM.TyCon.toBomTy tyConOfDatatype,
                {env = env, bomEnv = envWithTyParams})
              val CoreBOM.TyCon.TyC {definition=definition,params=params,...} =
                CoreBOM.TyCon.node tyConOfDatatype
            in
              definition := dtCons
              ; {
                env = env,
                bomEnv = newEnv
              }
            end
        | AstBOM.DataTypeDef.SimpleDef (_, _, longTyId) =>
          (* TODO: make this type equal to the type of the longTyId *)
            check
              (BOMEnv.TyEnv.lookup (bomEnv,
                CoreBOM.TyId.fromLongTyId longTyId), "undefined type")
                (fn tyDefn => check
                  (BOMEnv.TypeDefn.isCon tyDefn, "not a datatype")
                  (fn tyDefn => {
                  env = env,
                  bomEnv = BOMEnv.TyEnv.extend (bomEnv, tyId, tyDefn)
                }))
    in
      newEnvs
    end


  (* fun elaborateFunDef (fundef: AstBOM.FunDef.t, *)
  (*     tyEnvs as {env = env:Env.t, bomEnv = bomEnv: BOMEnv.t}) = *)
  (*   let *)


  fun elaborateBomDec (dec: AstBOM.Definition.t,
      tyEnvs as {env = env:Env.t, bomEnv = bomEnv: BOMEnv.t}) =
    case AstBOM.Definition.node dec of
      AstBOM.Definition.Datatype dtdefs =>
        let
          val envWithTys = foldl extendEnvForDataTypeDef tyEnvs dtdefs
          val envWithDefs = foldl elaborateDataTypeDef envWithTys dtdefs
        (* TODO: add value constructors *)
        in
          (CoreML.Dec.BomDec, #bomEnv envWithDefs)
        end

    | AstBOM.Definition.TypeDefn (bomId, maybeTyParams, bomTy) =>
        let
          val error = error (
            AstBOM.BomType.region,
            AstBOM.BomType.layout,
            BOMEnv.TypeDefn.error,
            bomTy)
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
    | AstBOM.Definition.Fun fundefs =>
        let
          val envWithFns = foldl extendEnvForFun tyEnvs fundefs
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
