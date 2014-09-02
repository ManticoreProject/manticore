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
                 (defnArityMatches (defn, tyArgs), "arity mismatch")
                  BOMEnv.TypeDefn.applyToArgs)
          end
      | AstBOM.BomType.Record fields =>
          check
            (recordLabelsOkay fields, "labels must be strictly increasing")
            (fn fields => keepRegion (CoreBOM.BomType.Record (map
              (fn field' => elaborateField (field', tyEnvs)) fields)))
      (* |  _ => error "not implemented" (* FIXME *) *)
    end
  and elaborateField (astField: AstBOM.Field.t,
     tyEnvs as {env = env:Env.t, bomEnv = bomEnv: BOMEnv.t}): CoreBOM.Field.t =
  (* TODO: do field ids need to be unique? mix mutable and immutable? *)
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


  fun elaborateBomDec (dec: AstBOM.Definition.t,
      tyEnvs as {env = env:Env.t, bomEnv = bomEnv: BOMEnv.t}) =
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
