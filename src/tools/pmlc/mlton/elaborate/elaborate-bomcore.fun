functor ElaborateBOMCore(S: ELABORATE_BOMCORE_STRUCTS) = struct
  open S

  structure AstBOM = Ast.AstBOM

  type funtype = {
    dom: CoreBOM.BomType.t list,
    cont: CoreBOM.BomType.t list,
    rng: CoreBOM.BomType.t list
  }

  fun app3 f (x, y, z) = (f x, f y, f z)
  fun error (getRegion, getLayout, errorVal, element) msg =
    (Control.error (getRegion element, getLayout element, Layout.str msg)
    ; errorVal)
  fun check (error: string -> 'b) (x: 'a option, msg: string) (f: 'a -> 'b) =
    case x of
      SOME y => f y
    | NONE => error msg

  fun elaborateBomType (astTy: AstBOM.BomType.t,
						tyEnvs as {env, bomEnv}): CoreBOM.BomType.t =
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
	  val wrappedElaborate = CoreBOM.BomType.wrapTuple o map (fn ty =>
		elaborateBomType (ty, tyEnvs))

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

          (* TODO: make into a fold *)
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
            val (dom, cont, rng) = app3 wrappedElaborate funTys
          in
            {dom=dom, cont=cont, rng=rng}
          end)
      | AstBOM.BomType.Any => CoreBOM.BomType.Any
      | AstBOM.BomType.VProc => CoreBOM.BomType.VProc
      | AstBOM.BomType.Cont maybeTyArgs =>
          CoreBOM.BomType.Cont (wrappedElaborate maybeTyArgs)
      | AstBOM.BomType.Addr ty =>
          CoreBOM.BomType.Addr (doElaborate ty)
      | AstBOM.BomType.Raw ty => CoreBOM.BomType.Raw (
          CoreBOM.RawTy.fromAst ty)
      | AstBOM.BomType.LongId (longTyId, maybeTyArgs) =>
          let
            val tyArgs = map doElaborate maybeTyArgs
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
      constructor (index, elaborateBomType (astTy, tyEnvs))
    end

  fun instanceTyToTy (tyId: AstBOM.LongTyId.t, tyArgs):
      AstBOM.BomType.t =
    let
      val wholeRegion = foldr Region.append
        (AstBOM.LongTyId.region tyId)
        (map AstBOM.BomType.region tyArgs)
    in
      AstBOM.BomType.makeRegion (
        AstBOM.BomType.LongId (tyId, tyArgs),
        wholeRegion)
    end

  fun extendEnvForTyParams (bomEnv: BOMEnv.t, tyParams: AstBOM.TyParam.t list) =
    foldl
      (fn (tyP: AstBOM.TyParam.t, bEnv)
        => BOMEnv.TyParamEnv.extend (bEnv, tyP))
      bomEnv
      tyParams

  (* fun extendEnvForTyParams (bomEnv, maybeTyParams) = *)
  (*   extendEnvForTyParams (bomEnv, CoreBOM.TyParam.flattenFromAst maybeTyParams) *)

  fun checkValArity (valId, ty, params, error): CoreBOM.Val.t =
    if (CoreBOM.BomType.arity ty) = (length params) then
      CoreBOM.Val.new (valId, ty, params)
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
      val envWithTyParams = extendEnvForTyParams (bomEnv, maybeTyParams)
      val tyEnvs' = {env = env, bomEnv = envWithTyParams}
      fun patsToTys pats = CoreBOM.BomType.wrapTuple (map
        (fn pat => varPatToTy (pat, tyEnvs'))
        pats)
      val domTys = patsToTys domPats
      val contTys = patsToTys contPats
      val rngTys' = CoreBOM.BomType.wrapTuple (map (fn ty => elaborateBomType (
		ty, tyEnvs')) rngTys)
      val funTy = {
          dom = domTys,
          cont = contTys,
          rng = rngTys'
        }
      val valId = CoreBOM.ValId.fromAstBomId id
      val newVal = checkValArity (valId, CoreBOM.BomType.Fun funTy,
        BOMEnv.TyParamEnv.getParams envWithTyParams, error (
          AstBOM.FunDef.region, AstBOM.FunDef.layout, CoreBOM.Val.error,
          funDef))
    in
      ({
        env = env,
        bomEnv = BOMEnv.ValEnv.extend (envWithTyParams, valId, newVal)
      }, newVal)
    end


  fun elaborateFunDefs (funDefs, tyEnvs as {env, bomEnv}) =
    let
      val (envWithFns, funVals) =
        foldr (fn (funDef, (oldEnv, oldVals)) =>
            let
              val (newEnv, newVal) = extendEnvForFun (funDef, oldEnv)
            in
              (newEnv, newVal::oldVals)
            end) (tyEnvs, []) funDefs

      val _ = ListPair.map
        (fn (funDef, funVal) => elaborateFunDef (funDef, funVal, envWithFns))
        (funDefs, funVals)
    in
      envWithFns
    end
  and elaborateFunDef (funDef: AstBOM.FunDef.t, funVal: CoreBOM.Val.t,
      tyEnvs as {env, bomEnv}) =
    let
        (* TODO: find the appropriate error value here *)
      val check = check (error (AstBOM.FunDef.region, AstBOM.FunDef.layout,
        (), funDef))
      val CoreBOM.BomType.Fun {dom, cont, rng} = CoreBOM.Val.typeOf funVal
      val AstBOM.FunDef.Def (maybeAttrs, _, _, domPats, contPats, _, exp) =
        AstBOM.FunDef.node funDef
      val envForBody = ListPair.foldrEq (fn (pat, ty, bomEnv) =>
        bindVarPat (pat, ty, {env = env, bomEnv = bomEnv})) bomEnv (
          [domPats, contPats], [dom, cont])
      val bodyExp = elaborateExp (exp, {env=env, bomEnv=envForBody})
    in
     (* TODO: handle noreturn *)
      check (CoreBOM.BomType.equal' (CoreBOM.Exp.typeOf bodyExp, rng),
        "function body doesn't agree with range type")
        (fn _ => ())
    end
  (* and wrapTuple tys = *)
  (*   case tys of *)
  (*     [] => CoreBOM.BomType.NoReturn *)
  (*   | [ty] => ty *)
  (*   | tys => CoreBOM.BomType.Tuple tys *)
  and elaborateSimpleExp (sExp, tyEnvs as {env, bomEnv}): CoreBOM.Exp.t =
    let
      fun checkForErrorVal errorVal = check (error (AstBOM.SimpleExp.region,
        AstBOM.SimpleExp.layout, errorVal, sExp))
      val checkVal = checkForErrorVal CoreBOM.Val.error
      val checkTy = checkForErrorVal CoreBOM.BomType.Error
	  val checkExp = checkForErrorVal CoreBOM.Exp.error
      fun elaborateTupleExp (dom, rng, sExps) =
        let
          (* TODO: handle noreturn correctly *)
          val argumentTy = CoreBOM.BomType.wrapTuple (map (fn sExp =>
			CoreBOM.Exp.typeOf (elaborateSimpleExp (sExp, tyEnvs))) sExps)
        in
        (* check that the argument matches the range of the constructor *)
          checkForErrorVal CoreBOM.BomType.Error (
            CoreBOM.BomType.equal' (dom, argumentTy),
            "invalid constructor argument") (fn _ => rng)
        end
      fun elaborateVpExp (index, procExp) =
        (* TODO: do something useful with the index *)
        checkTy (CoreBOM.BomType.equal' (CoreBOM.Exp.typeOf (
		  elaborateSimpleExp (procExp, tyEnvs)), CoreBOM.BomType.VProc),
        "argument to vproc operation must be a vproc") (fn x => x)
    in
      case AstBOM.SimpleExp.node sExp of
        AstBOM.SimpleExp.Id longValId => checkExp (BOMEnv.ValEnv.lookup (
		  bomEnv, CoreBOM.ValId.fromLongValueId longValId),
		  "undefined value identifier") (fn value => CoreBOM.Exp.new (
		    CoreBOM.Exp.Val value, CoreBOM.Val.typeOf value))
      | AstBOM.SimpleExp.HostVproc =>  CoreBOM.Exp.new (CoreBOM.Exp.HostVproc,
		  CoreBOM.BomType.VProc)
      | AstBOM.SimpleExp.Promote sExp' =>
		  CoreBOM.Exp.newWithType (CoreBOM.Exp.Promote, elaborateSimpleExp (
			sExp', tyEnvs))
      | AstBOM.SimpleExp.TypeCast (ty, sExp) =>
          (* make sure we only typecast Any *)
          checkExp (
			let
			  val (expNode, expTy) = CoreBOM.Exp.dest (elaborateSimpleExp (sExp,
				tyEnvs))
			in
		      if CoreBOM.BomType.strictEqual (CoreBOM.BomType.Any, expTy) then
				SOME expNode
			  else
				NONE
			end, "only 'any' can be typecast")
          (fn expNode => CoreBOM.Exp.new (expNode, elaborateBomType (
			ty, tyEnvs)))
      (* | AstBOM.SimpleExp.Literal (* TODO: what do these become?*) *)
      | AstBOM.SimpleExp.AllocId (longValId, sExps) =>
          let
            (* make sure longValId is bound to a con, find its domain
            and range *)
            val CoreBOM.BomType.Con {dom, rng} = checkForErrorVal
              (CoreBOM.BomType.Con {
                dom = CoreBOM.BomType.Error,
                rng = CoreBOM.BomType.Error
              }) (CoreBOM.BomType.isCon (CoreBOM.Val.typeOf (checkVal (
                BOMEnv.ValEnv.lookup (bomEnv, CoreBOM.ValId.fromLongValueId
                longValId), "undefined value identifier") (fn x => x))),
              "value identifier is not a constructor") (fn x => x)
          in
            elaborateTupleExp (dom, rng, sExps)
          end
      | AstBOM.SimpleExp.AllocType (tyArgs, sExps) =>
          let
            val tyArgs' = map (fn tyArg => elaborateBomType (tyArg, tyEnvs))
              tyArgs
            (* the range is always a tuple *)
            val rng = CoreBOM.BomType.Tuple tyArgs'
            (* if we only have one tyarg, then the domain is that
            type, otherwise, we wrap it in a tuple *)
            val dom =
              case tyArgs' of
                [tyArg] => tyArg
              | tyArgs => rng
          in
            elaborateTupleExp (dom, rng, sExps)
          end
      | AstBOM.SimpleExp.VpLoad (index, procExp) =>
          let
            val _ = elaborateVpExp (index, procExp)
          in
            (* for now, we return Any *)
            CoreBOM.BomType.Any
          end
      | AstBOM.SimpleExp.VpStore (index, procExp, valExp) =>
          let
            val _ = elaborateVpExp (index, procExp)
          in
            CoreBOM.BomType.unit
          end
      | AstBOM.SimpleExp.VpAddr (index, procExp) =>
          let
            val _ = elaborateVpExp (index, procExp)
          in
            CoreBOM.BomType.Addr CoreBOM.BomType.Any
          end
      | AstBOM.SimpleExp.AtIndex (index, recordSExp, maybeStoreSExp) =>
          let
            (* make sure recordSExp evaluates to a record *)
            val fieldTys = checkForErrorVal []
              ((case elaborateSimpleExp (recordSExp, tyEnvs) of
                CoreBOM.BomType.Record fields => SOME fields
              | _ => NONE),
             (* TODO: phrase this better *)
              "argument to index access expression is not a record") (fn x => x)

            (* make sure the record is defined at the specified index *)
            val fieldTy = checkForErrorVal CoreBOM.Field.bogus
              (List.find (fn fieldTy => CoreBOM.Field.index fieldTy = index)
                fieldTys, "no such index") (fn x => x)

            (* if the rhs is a store expression, find out the type *)
            val maybeStoreTy =
              case maybeStoreSExp of
                SOME sExp => SOME (elaborateSimpleExp (sExp, tyEnvs))
              | NONE => NONE
          in
            checkTy (
              case (fieldTy, maybeStoreTy) of
                  (* make sure only mutable fields are mutated *)
                (CoreBOM.Field.Immutable (_, ty), NONE) => SOME ty
              | (CoreBOM.Field.Immutable (_, _), SOME ty) => NONE
              | (CoreBOM.Field.Mutable (_, ty), NONE) => SOME ty
              | (CoreBOM.Field.Mutable (_, ty), SOME ty') =>
                  (* if a field is mutated, rhs and lhs types must match *)
                  checkForErrorVal (SOME CoreBOM.BomType.Error) (
                    CoreBOM.BomType.equal' (ty, ty'),
                  "assignment type does not match field type")
                  (* and assignments always evaluate to unit *)
                  (fn _ => SOME CoreBOM.BomType.unit),
              "immutable record in assignment expression")
              (fn x => x)
          end

      | _ => raise Fail "not implemented"
    end
  and elaborateRHS (rhs, tyEnvs) =
    case AstBOM.RHS.node rhs of
      AstBOM.RHS.Composite exp => elaborateExp (exp, tyEnvs)
    | AstBOM.RHS.Simple sExp => [elaborateSimpleExp (sExp, tyEnvs)]
  and bindVarPat (varPat, rhsTy, tyEnvs as {env, bomEnv}) =
    let
      val check = check (error (AstBOM.VarPat.region, AstBOM.VarPat.layout,
        CoreBOM.BomType.Error, varPat))
      fun checkTyBinding (maybeTy, rhsTy) =
        case maybeTy of
          SOME ty =>
            check (
              CoreBOM.BomType.equal' (elaborateBomType (ty, tyEnvs), rhsTy),
              "type constraint does not match rhs") (fn x => x)
        | NONE => rhsTy
      val (bind, maybeTy) =
        case AstBOM.VarPat.node varPat of
          AstBOM.VarPat.Wild maybeTy => ((fn _ => tyEnvs), maybeTy)
        | AstBOM.VarPat.Var (bomId, maybeTy) => (fn rhsTy =>
            let
              val valId = CoreBOM.ValId.fromAstBomId bomId
            in
              {env = env, bomEnv = BOMEnv.ValEnv.extend (bomEnv, valId,
                CoreBOM.Val.new (valId, rhsTy, []))}
            end, maybeTy)
      in
        #bomEnv (bind (checkTyBinding (maybeTy, rhsTy)))
      end
  and elaborateExp (exp: AstBOM.Exp.t, tyEnvs as {env, bomEnv}): CoreBOM.Exp.t =
    let
      fun errorForErrorVal errorVal = error (AstBOM.Exp.region,
        AstBOM.Exp.layout, errorVal, exp)
      fun checkForErrorVal errorVal = check (errorForErrorVal errorVal)
    in
      case AstBOM.Exp.node exp of
        AstBOM.Exp.Return sExps =>
          let
            val (tys, exps) = ListPair.unzip (map (fn sExp => elaborateSimpleExp (
              sExp, tyEnvs)) sExps)
          in
            (tys, CoreBOM.Exp.Return exps)
          end
      | AstBOM.Exp.If (sExp, left, right) =>
          let
           val check = checkForErrorVal [CoreBOM.BomType.Error]
          (* TODO: figure out where bool is, check that this is bool *)
            val condTy = elaborateSimpleExp (sExp, tyEnvs)
            fun doElaborate exp = elaborateExp (exp, tyEnvs)
          in
            check
              (CoreBOM.BomType.equals' (doElaborate left, doElaborate right),
              "types of if branches do not agree")
              (fn x => x)
          end
      | AstBOM.Exp.Let (varPats, rhs, exp) =>
          let
            val check = checkForErrorVal bomEnv
            val rhsTys = elaborateRHS (rhs, tyEnvs)
            val newEnvs = {
              env = env,
              bomEnv = check (if length rhsTys = length varPats then
                  SOME bomEnv
                else NONE,
                "left and right side of let binding are of different lengths")
                (fn bomEnv => ListPair.foldrEq (fn (varPat, rhsTy, oldEnv) =>
                  bindVarPat (varPat, rhsTy, {env = env, bomEnv = oldEnv}))
                  bomEnv (varPats, rhsTys))
              }
          in
            elaborateExp (exp, newEnvs)
          end
      | AstBOM.Exp.Do (sExp, exp) =>
          (elaborateSimpleExp (sExp, tyEnvs)
          ; elaborateExp (exp, tyEnvs))
      | AstBOM.Exp.Throw (bomId, sExps) =>
          (* TODO: this will give an unhelpful message if bomId isn't a cont *)
          (* check arguments match the type of the cont. to be thrown *)
          checkForErrorVal [] (CoreBOM.BomType.equal' (
            (* find the type of the cont. to throw *)
            (checkForErrorVal CoreBOM.BomType.Error (BOMEnv.ValEnv.lookup
              (bomEnv, CoreBOM.ValId.fromAstBomId bomId),
              "unbound value identifier") (fn value =>
                CoreBOM.Val.typeOf value),
            (* find the type of the arguments, wrap in a cont. *)
            CoreBOM.BomType.Cont (map (fn sExp =>
                elaborateSimpleExp (sExp, tyEnvs)) sExps))),
            (* throw elaborates to noreturn *)
          "throw arguments do not match continuation type") (fn _ => [])
      | AstBOM.Exp.FunExp (funDefs, exp) =>
          let
            val envWithFns = elaborateFunDefs (funDefs, tyEnvs)
          in
            elaborateExp (exp, envWithFns)
          end
      | _ => raise Fail "not implemented"
    end

  fun dataTypeDefToTyIdAndParams dtDef =
    let
      val (tyId, tyParams) =
        ((fn AstBOM.DataTypeDef.ConsDefs (astId, maybeTyParams, _) =>
          (CoreBOM.TyId.fromAstBomId astId, maybeTyParams))
          (AstBOM.DataTypeDef.node dtDef))
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
      val valId = CoreBOM.ValId.fromAstBomId astId
      val (maybeArgTy: CoreBOM.BomType.t option, valTy: CoreBOM.BomType.t) =
        case (maybeTy: AstBOM.BomType.t option) of
          SOME (argTy: AstBOM.BomType.t) =>
            let
              val argTy = elaborateBomType (argTy, tyEnvs)
            in
              (SOME argTy, CoreBOM.BomType.Con {dom = argTy, rng = datatypeTy})
            end
        | NONE => (NONE, datatypeTy)
    in
      (CoreBOM.DataConsDef.ConsDef (
        CoreBOM.BomId.fromAst astId, maybeArgTy),
      BOMEnv.ValEnv.extend (bomEnv, valId, CoreBOM.Val.new (valId, valTy,
        params)))
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
              val CoreBOM.TyCon.TyC {definition, params, ...} =
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

  fun elaborateBomDec (dec: AstBOM.Definition.t, tyEnvs as {env, bomEnv}) =
    case AstBOM.Definition.node dec of
      AstBOM.Definition.Datatype dtdefs =>
        let
          val envWithTys = foldl extendEnvForDataTypeDef tyEnvs dtdefs
          val envWithDefs = foldl elaborateDataTypeDef envWithTys dtdefs
        in
          (CoreML.Dec.BomDecs [], #bomEnv envWithDefs)
        end
    | AstBOM.Definition.DatatypeAlias (bomId, longTyId) =>
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
          (CoreML.Dec.BomDecs [], BOMEnv.TyEnv.extend (bomEnv, tyId, tyConDefn))
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

          val envWithTyParams: BOMEnv.t = extendEnvForTyParams (
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
          (CoreML.Dec.BomDecs [], newEnv)
        end
    | AstBOM.Definition.Fun funDefs =>
        let
          val envWithFns = elaborateFunDefs (funDefs, tyEnvs)
        in
          (CoreML.Dec.BomDecs [], #bomEnv envWithFns)
        end
    | AstBOM.Definition.InstanceType instanceTy =>
        let
          val ty = elaborateBomType (instanceTyToTy instanceTy, tyEnvs)
        (* TODO: deal with extending the environment *)
        in
          (CoreML.Dec.BomDecs [], bomEnv)
        end
    | _ => raise Fail "not implemented"
    (* TODO: the other cases *)

    (* (CoreML.Dec.BomDec, bomEnv) *)
end
