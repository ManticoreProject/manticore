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
  structure BOMImport = CoreML.BOMImport

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

  (* creating a new (maybe polymorphic) ML val/tycon from one exported by BOM *)
  fun elaborateBOMExport (export, tyEnvs as {env: Env.t, bomEnv: BOMEnv.t}) =
    let
      val elaborateMLType = elaborateMLType' env
      val mkBOMExport = CoreML.Dec.BOMExport
      (* get the ML tycon created by makeMLDatatype for the given BOM tycon *)
      fun findMLTycon env bomLongId = let
        val (strids, id) = BOM.LongId.split bomLongId
        val idSymbol = Ast.Symbol.fromString ("__bomtycon_" ^ BOM.LongId.Id.toString
          id)
        val bomTyconSymbols = idSymbol :: List.map
          BOM.LongId.Strid.toSymbol strids
        val bomTycon = Ast.Longtycon.fromSymbols (bomTyconSymbols,
          BOM.LongId.region bomLongId)
        val mlTyconTypestr: Env.TypeStr.t =
          case Env.lookupLongtycon(env, bomTycon) of
             SOME typestr => typestr
           | NONE => raise Fail ("failed to find referenced BOM datatype: "
             ^ BOM.LongId.toString bomLongId)
        val SOME mlTycon = Env.TypeStr.toTyconOpt mlTyconTypestr
      in
        mlTycon
      end
    in
      case ABOMExport.node export of
        ABOMExport.Datatype (tyvars, asttycon, bomLongId, bomTys, primConDefs) =>
          (* make let bindings for all the names and types linking them to the
          ML type for this BOM type which was originally created by
          makeMLDatatype *)
          let
            (* elaborate tyvars with a simple conversion *)
            val tyvars' = Vector.map MLType.var tyvars
            val kind = MLKind.Arity (Vector.length tyvars)

            (* the name (for use in ML) of the BOM definitions being exported *)
            val tyId = CoreBOM.TyId.fromLongId bomLongId

            (* find the corresponding ML tycon, created by makeMLDatatype *)
            val mlTycon = findMLTycon env bomLongId

            val resultMLTy: CoreML.Type.t =
              MLType.con (mlTycon, Vector.map MLType.var tyvars)

            (* TODO(wings): insert a type alias into the env? *)

            (* put all dcons into the environment and translate them to CoreML *)
            val (primConDefs, cons) =
              ListPair.unzip (List.map ((fn astPrimConDef =>
                let
                  val BOM.PrimConDef.T (mlConName, maybeTy, bomValLongId) =
                    BOM.PrimConDef.node astPrimConDef
                  val bomVal =
                    case BOMEnv.ValEnv.lookup (bomEnv, CoreBOM.ValId.fromLongId
                      bomValLongId) of
                       SOME bv => bv
                     | NONE => raise Fail ("failed to find referenced BOM data\
                       \ constructor: " ^ BOM.LongId.toString bomValLongId)

                  (* make an ML variable with the ML name we want to bind *)
                  val mlVarSymbol = Ast.Symbol.fromString (Ast.Con.toString
                    mlConName)
                  val mlConBind = Ast.Vid.toCon (Ast.Vid.fromSymbol (mlVarSymbol,
                    Ast.Con.region mlConName))
                  val mlVar = Ast.Vid.toVar (Ast.Vid.fromSymbol (mlVarSymbol,
                    Ast.Con.region mlConName))
                  val mlVar' = ElaborateCore.Var.fromAst mlVar

                  (* find the corresponding ML con, created by makeMLDatatype *)
                  val (strids, id) = BOM.LongId.split bomValLongId
                  val bomConSymbols = Ast.Symbol.fromString ("__bomcon_" ^
                    BOM.LongId.Id.toString id) :: List.map
                    BOM.LongId.Strid.toSymbol strids
                  val bomCon = Ast.Longcon.fromSymbols (bomConSymbols,
                    BOM.LongId.region bomValLongId)
                  val (mlCon, maybeArgScheme) = Env.lookupLongcon(env, bomCon)

                  (* TODO(wings): ensure the con's actual type (i.e. conMLTy)
                  unifies with the possibly ascribed type (i.e. maybeTy) *)

                  (* TODO(wings): handle type arguments (???) *)

                  val maybeArgMLTy = Option.map MLScheme.ty maybeArgScheme

                  val conMLTy =
                    case maybeArgMLTy of
                       SOME argMLTy => MLType.arrow (argMLTy, resultMLTy)
                     | NONE => resultMLTy

                  (* mutate the environment to bind constructor variable *)
                  (*val _ = Env.extendVal (env, mlVar, mlVar',
                    MLScheme.fromType resultMLTy, {isRebind = false})*)
                  val _ = Env.extendCon (env, mlConBind, mlCon,
                    MLScheme.fromType resultMLTy, {isRebind = false})
                in
                  (CoreML.PrimConDef.T (mlCon, maybeArgMLTy,
                    resultMLTy, mlVar', bomVal),
                    {con=mlCon,
                     name=mlConName,
                     arg=maybeArgMLTy,
                     ty=conMLTy})
                end)
              ) primConDefs)

            (* find the tycon in the environment and apply it to type
            parameters *)
            val bomTyCon = case BOMEnv.TyEnv.lookup (bomEnv, tyId)
              of SOME x => x
               | NONE => raise Fail ("internal error: "
                 ^ "did not find tyId for _datatype export in BOM environment")
            (* TODO(wings): apply type variables? *)
            val bomTy = case BOMEnv.TypeDefn.applyToArgs (bomTyCon, []
              (*Vector.foldl (op::) [] tyvars'*))
              of SOME x => x
               | NONE => raise Fail ("internal error: "
                 ^ "arity mismatch applying empty arguments to tycon!")
            val bomTycon = case bomTy
              of CoreBOM.BOMType.TyCon {con=bomTycon,...} => bomTycon
               | _ => raise Fail ("internal error: "
                 ^ "did not find TyCon for _datatype export in BOM environment")

            (* Extend ML Type environment in place *)
            val _ = Env.extendTycon (env, asttycon,
              Env.TypeStr.def (MLScheme.make {canGeneralize = false,
                ty = resultMLTy,
                tyvars = tyvars},
              kind), {forceUsed = false, isRebind = false})
          in
            (SOME (BOMExport.Datatype (mlTycon, bomTycon, primConDefs)), tyEnvs)
          end
      | ABOMExport.TypBind (tyvars, asttycon, bomTy) =>
          let
            (* This is all of the "elaboration" they do on tyvars *)
            val tyvars' = Vector.map MLType.var tyvars
            (* FIXME: error handling *)

            val kind = MLKind.Arity (Vector.length tyvars)
            (* TODO: check kind matches *)

            val elaborated = ElaborateBOMCore.elaborateBOMType (bomTy, bomEnv)
            val (bomExport, mlTycon) = (case elaborated
              of CoreBOM.BOMType.TyCon {con=bomCon,...} =>
                let

                  (* find the corresponding ML tycon, created by makeMLDatatype *)
                  val CoreBOM.TyC {id=tyid, ...} = bomCon
                  val symbols = case tyid
                    of CoreBOM.TyId.BOMTy bomId =>
                      [BOM.Symbol.fromString (CoreBOM.BOMId.toString (bomId))]
                    | CoreBOM.TyId.QBOMTy (moduleId, bomId) =>
                      let
                        val moduleSym = BOM.Symbol.fromString
                          (CoreBOM.BOMId.toString
                            (CoreBOM.ModuleId.toBOMId moduleId))
                        val bomSym = BOM.Symbol.fromString
                          (CoreBOM.BOMId.toString (bomId))
                      in
                        [moduleSym, bomSym]
                      end
                  val mlTycon = findMLTycon env (BOM.LongId.fromSymbols(symbols, Region.bogus))
                in
                  (SOME (BOMExport.TypBind (mlTycon, bomCon)), mlTycon)
                end
               (* | CoreBOM.BOMType.Raw rawTy => BOM.BOMType.Raw BOM.BOMType.Int8 *)
               | CoreBOM.BOMType.Raw rawTy => let
                   fun intTy bits = (NONE, CoreML.Tycon.int
                     (CoreML.IntSize.fromBits bits))
                   fun uintTy bits = (NONE, CoreML.Tycon.word
                     (CoreML.WordSize.fromBits bits))
                   fun floatTy fltsize = (NONE, CoreML.Tycon.real fltsize)
                 in case rawTy
                   of CoreBOM.RawTy.Int8 => intTy Bits.inWord8
                    | CoreBOM.RawTy.Int16 => intTy Bits.inWord16
                    | CoreBOM.RawTy.Int32 => intTy Bits.inWord32
                    | CoreBOM.RawTy.Int64 => intTy Bits.inWord64

                    | CoreBOM.RawTy.UInt8 => uintTy Bits.inWord8
                    | CoreBOM.RawTy.UInt16 => uintTy Bits.inWord16
                    | CoreBOM.RawTy.UInt32 => uintTy Bits.inWord32
                    | CoreBOM.RawTy.UInt64 => uintTy Bits.inWord64

                    | CoreBOM.RawTy.Float32 => floatTy (CoreML.RealSize.R32)
                    | CoreBOM.RawTy.Float64 => floatTy (CoreML.RealSize.R64)

                    | CoreBOM.RawTy.Vec128 => raise Fail "VecNNN types!"
                    | CoreBOM.RawTy.Vec256 => raise Fail "VecNNN types!"
                    | CoreBOM.RawTy.Vec512 => raise Fail "VecNNN types!"
                 end
               | CoreBOM.BOMType.Error => raise Fail
                   ("attempting to export unknown BOM type: "
                   ^ Layout.toString (BOM.BOMType.layout bomTy) ^ "...")
               | ty => raise Fail
                   ("TODO(wings): exporting unhandled type: "
                   ^ Layout.toString (BOM.BOMType.layout bomTy) ^ "...")
              (* end case *))

            (* Extend ML Type environment in place *)
            val _ = Env.extendTycon (env, asttycon, Env.TypeStr.tycon (mlTycon,
             kind), {forceUsed = false, isRebind = false})
          in
            (bomExport, tyEnvs)
          end
      | ABOMExport.Val (mlValId, mlTy, bomValId) =>
          let
            (* FIXME: error handling *)
            val mlTy' = ElaborateCore.elaborateType (mlTy,
              ElaborateCore.Lookup.fromEnv env)
            in
              (* TODO(wings): typecheck BOM values against ascribed ML type! *)
              case BOM.ValueId.node bomValId of
                 BOM.ValueId.HLOpQId hlopqid =>
                   raise Fail "TODO(wings): elaborate HLOp exports"
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
                        | NONE => raise Fail
                            ("failed to find referenced BOM name: "
                            ^ CoreBOM.ValId.toString valId)
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


  (* importing a monomorphization (or monomorphic) ML val/tycon into the BOM
  environment *)
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

      fun extendEnvs (astMlTy, tyargs, tyc, kind, maybeId) =
        let
          val typeStr = Env.TypeStr.tycon (tyc, kind)
          val tyId =
            CoreBOM.TyId.fromBOMId' (case maybeId of
              SOME astId => CoreBOM.BOMId.fromAst astId
            | NONE => let
                val symbol = Ast.Symbol.fromString (CoreML.Tycon.toString tyc)
                val bomLongcon = Ast.Longcon.fromSymbols ([symbol],
                  Ast.Type.region astMlTy)
              in
                CoreBOM.BOMId.fromLongcon bomLongcon
              end)
val _ = print ("binding datatype with name " ^ CoreBOM.TyId.toString tyId ^ "\n")
          (* Only datatypes can be imported, and we ignore their
          constructors since they must be explicitly imported *)
          val bomTyc =
            (* Apply the tycon to the provided arguments *)
            (* FIXME: this is probably WRONG for typarams *)
            CoreBOM.TyCon.new (tyId, List.tabulate (
                Vector.length tyargs, fn _ => CoreBOM.TyParam.new ()))
        in
          let
            (* First, we put the new tyc into the mapping *)
            val bomEnv' = (fn bomEnv' => BOMEnv.TyEnv.extend (bomEnv', tyId,
              BOMEnv.TypeDefn.newCon bomTyc)) (BOMEnv.MLTyEnv.extend (
              bomEnv, tyc, fn args => CoreBOM.TyCon.applyToArgs' (
                bomTyc, args)))

            (* Apply it to the constructors we were given *)
            val mlTy = Env.TypeStr.apply (typeStr, tyargs)
            (*(* No params by this point *)
            val bomTy = translateType bomEnv' mlTy*)
            val bomTy = CoreBOM.BOMType.TyCon {con=bomTyc, args=[(*TODO(wings): args*)]}

(*            val bomEnv' = BOMEnv.TyEnv.extend (bomEnv', tyId,
              BOMEnv.TypeDefn.newAlias {params = [], ty = bomTy})*)
          in
            (bomTyc, bomTy, {env = env, bomEnv = bomEnv'})
          end
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
          (con, SOME tyScheme) => (con, MLScheme.apply (tyScheme, tyargs))
        | _  => raise Fail ("Unmapped longcon " ^ Ast.Longcon.toString longcon)

      fun translateCon (bomEnv, bomResultTy, tyargs, longcon, maybeArgTy, maybeId) =
        let
          val newValId = resolveValId CoreBOM.BOMId.fromLongcon (longcon, maybeId)
          val (mlCon, mlTy) = unwrapMLCon (longcon, tyargs)
          (* grab the actual argument type *)
          val (maybeDomMlTy, rngMlTy) = case MLType.deArrowOpt mlTy of
            SOME (dom, rng) => (SOME dom, rng)
          | NONE => (NONE, mlTy)

          (* convert the type of the con to a BOM type *)
          val bomConTy = unwrapMLArrow (bomEnv, mlTy)

          (* DEBUG *)
          val _ = printMLTy (mlTy, "translating: ")

          val _ =
            if CoreBOM.BOMType.equal (bomResultTy,
              case (bomConTy, maybeArgTy) of
                (CoreBOM.BOMType.Con {dom, rng}, SOME argTy) =>
                  let
                    (* check that the import has the right argument type *)
                    val SOME domMlTy = maybeDomMlTy
                    val argMlTy: CoreML.Type.t = elaborateMLType argTy
                    val _ = ElaborateCore.unify
                      (argMlTy, domMlTy, fn x => x, fn (l, l') =>
                       (BOM.Import.region import,
                        Layout.str "constructor imported with incorrect argument type",
                        Layout.align [Layout.seq [Layout.str "expects: ", l'],
                               Layout.seq [Layout.str "but got: ", l],
                               Layout.seq [Layout.str "in import of ",
                                 Layout.str (Ast.Longcon.toString longcon)]]))
                  in
                    rng
                  end
              | (CoreBOM.BOMType.Con _, NONE) => raise Fail
                "argument type not provided when importing constructor"
              | (_, SOME argTy) => raise Fail
                "argument type specified when importing nullary constructor"
              | (CoreBOM.BOMType.TyCon tycon, NONE) => bomConTy
              (* Special case for exns because it's not worth
              factoring them out *)
              | (CoreBOM.BOMType.Exn, NONE) => CoreBOM.BOMType.Exn
              | (CoreBOM.BOMType.Error, NONE) => raise Fail "Con wasn't found in env."
              | _ => raise Fail "Type is not a con.")
            then ()
            else raise Fail "Bad con application."
        in
            (* FIXME: never any params in imports? *)
          (newValId, CoreBOM.Val.new (newValId, bomConTy, []), mlCon, maybeDomMlTy, rngMlTy)
        end

      fun translateImportCon (bomEnv, bomResultTy, tyargs) importCon =
        let
            (* FIXME: let's ignore the maybeTy for now *)
          val BOM.ImportCon.T (longcon, maybeArgTy, maybeId) =
            BOM.ImportCon.node importCon
        in
          translateCon (bomEnv, bomResultTy, tyargs, longcon, maybeArgTy, maybeId)
        end

    in
      case BOM.Import.node import of
        BOM.Import.Val (valId, ty, maybeId) =>
          let
            val ty' = elaborateMLType ty
            val (valId', maybeScheme) = Env.lookupLongvid (env, valId)
            val success =
              case maybeScheme of
                SOME scheme =>
                  let
                    val success = ref true
                  in
                    (MLType.unify (ty', #instance (MLScheme.instantiate
                      scheme), {
                    error = fn (l, r) => Control.error (BOM.Import.region import,
                      Layout.str "value imported with incorrect type",
                      Layout.align [Layout.seq [Layout.str "actual type is: ", r],
                        Layout.seq [Layout.str "but imported with type: ", l],
                        Layout.seq [Layout.str "in import of ",
                          Ast.Longvid.layout valId]]),
                    preError = fn () => success := false }); !success)
                      (*(ElaborateCore.unify
                       (ty', #instance (MLScheme.instantiate scheme),
                       fn () => success := false,
                       fn (l, r) => (BOM.Import.region import,
                         Layout.str "value imported with incorrect type",
                         Layout.align [Layout.seq [Layout.str "actual type is: ", r],
                           Layout.seq [Layout.str "but imported with type: ", l],
                           Layout.seq [Layout.str "in import of ",
                             Ast.Longvid.layout valId]])); !success)*)
                  end
              | NONE => false
(*
		_import val can be applied to:
			dcons, for which it should work like _datatype
			extern funs, for which it might already work
			bom funs, for which it should get handled in translate
*)

            val bomTy = translateType bomEnv ty'
            (* remove qualifying module, make a BOMId *)
            val bomValId = resolveValId CoreBOM.BOMId.fromLongvid (valId, maybeId)

            (* we never have typarams on a val from ML code *)
            val bomVal = CoreBOM.Val.new (bomValId, bomTy, [])

            (* put the new ty into our env and bind our new valId to it *)
            val _ = BOMEnv.ValEnv.extend (bomEnv, bomValId, bomVal)
          in
            (SOME (BOMImport.Val (raise Fail "TODO(wings): do something meaningful in _import val", bomVal, bomTy)), {bomEnv=bomEnv, env=env})
          end

      | BOM.Import.Datatype (astMlTy(*args, tyc*), maybeId, cons) =>
          let
            val mlTy = elaborateMLType (astMlTy)
            val (tyargs, mlTyc) = (case CoreML.Type.deConOpt mlTy
                     of SOME (tyc, tyargs) => (tyargs, tyc)
                      | _ => raise Fail
                       ("attempting to import unknown type: "
                       ^ Layout.toString (Ast.Type.layout astMlTy))
                    (* end case *))
            val kind = MLKind.Arity (Vector.length tyargs)
            (*val tyargs' = Vector.map elaborateMLType tyargs*)
            (* FIXME: will we need to return the env here for any reason? *)
            (* TODO(wings): use MLton error reporting for these two cases *)
            val (bomTyc, bomTy, {env, bomEnv}) =
              extendEnvs (astMlTy, tyargs, mlTyc, kind, maybeId)

            (* TODO(wings): check that we map every con exactly once *)
            val cons' = map (translateImportCon (bomEnv, bomTy, tyargs)) cons

            (* Add the constructors to the tycon *)
            val _ = (fn (CoreBOM.TyCon.TyC {definition,...}) =>
              definition := map (fn (valId, bomVal, mlCon, maybeArgMlTy, resultMlTy) => CoreBOM.ConsDef (
                CoreBOM.ValId.truncateToBOMId valId,
                SOME (CoreBOM.Val.typeOf bomVal))) cons') bomTyc

            val bomEnv = foldl (fn ((valId, bomVal, mlCon, maybeArgMlTy, resultMlTy), bomEnv) =>
             BOMEnv.ValEnv.extend (bomEnv, valId, bomVal)) bomEnv cons'

            (* create a list of primcondefs for the import *)
            val pcds = map (fn (valId, bomVal, mlCon, maybeArgMlTy, resultMlTy) => let
              (*dummy var*)
              val mlVar = Ast.Vid.toVar (Ast.Vid.fromSymbol (Ast.Symbol.fromString ("__TODO_remove_dummy"), Region.bogus))
              val mlVar' = ElaborateCore.Var.fromAst mlVar
              in
                CoreML.PrimConDef.T (mlCon, maybeArgMlTy, resultMlTy, mlVar', bomVal)
              end) cons'
          in
            (SOME (BOMImport.Datatype (mlTyc, bomTyc, pcds)), {bomEnv=bomEnv, env=env})
          end

      | BOM.Import.Exn (longcon, maybeArgTy, maybeId) =>
          let
            val newValId = resolveValId CoreBOM.BOMId.fromLongcon (longcon, maybeId)

            (* find the ML exception being imported *)
            val (mlCon, mlTy) = unwrapMLCon (longcon, Vector.fromList [])

            (* get its argument type *)
            val (maybeArgMlTy, resultMlTy) =
              case MLType.deArrowOpt mlTy of
                (* MLton doesn't distinguish between arrow and constructors
                  here, so we have to handle this as a special case *)
                SOME (dom, rng) => (SOME dom, rng)
              | NONE => (NONE, mlTy)

            (* ensure the result is an exception type *)
            val _ = ElaborateCore.unify
              (resultMlTy, MLType.con (MLTycon.exn, Vector.fromList []), fn x => x, fn (l, l') =>
               (BOM.Import.region import,
                Layout.str "non-exception value imported as exception",
                Layout.align [Layout.seq [Layout.str "expected exception constructor"],
                       Layout.seq [Layout.str "but got value of type: ", l],
                       Layout.seq [Layout.str "in import of ",
                         Layout.str (Ast.Longcon.toString longcon)]]))

            (* create the BOM type to use for the exn con, checking whether the
            provided arg type matches the exn con's arg's actual type *)
            val bomConTy = case (maybeArgMlTy, maybeArgTy) of
                (NONE, NONE) => CoreBOM.BOMType.Exn
                (* TODO(wings): use MLton error reporting for these two cases *)
              | (SOME _, NONE) => raise Fail "argument type not provided when importing exception"
              | (NONE, SOME _) => raise Fail "argument type specified when importing nullary exception"
              | (SOME argMlTy, SOME argTy) =>
                 let
                   val domMlTy: CoreML.Type.t = elaborateMLType argTy
                   val _ = ElaborateCore.unify
                     (domMlTy, argMlTy, fn x => x, fn (l, l') =>
                      (BOM.Import.region import,
                       Layout.str "exception imported with incorrect argument type",
                       Layout.align [Layout.seq [Layout.str "expects: ", l'],
                              Layout.seq [Layout.str "but got: ", l],
                              Layout.seq [Layout.str "in import of ",
                                Layout.str (Ast.Longcon.toString longcon)]]))
                   val dom: CoreBOM.BOMType.t = translateType bomEnv domMlTy
                 in
                   CoreBOM.BOMType.Con {
                     dom = dom,
                     rng = CoreBOM.BOMType.Exn
                   }
                 end

            val newVal = CoreBOM.Val.new (newValId, bomConTy, [])
(*            val (newValId, newVal) = translateCon (bomEnv,
              CoreBOM.BOMType.Exn, Vector.fromList [], longcon, maybeTy,
                maybeId)*)
            val bomEnv = BOMEnv.ValEnv.extend (bomEnv, newValId, newVal)
          in
            (* TODO(wings): pass optional type of con's arg rather than type of con *)
            (SOME (BOMImport.Exception (mlCon, newVal, SOME bomConTy)), {bomEnv=bomEnv, env=env})
          end

    end
end
