functor ElaborateBOMCore(S: ELABORATE_BOMCORE_STRUCTS) = struct
  open S

  structure AstBOM = Ast.AstBOM

  fun elaborateBomDec (dec: AstBOM.Definition.t,
      {env = env':Env.t, bomEnv = bomEnv': BOMEnv.t}) =
    case dec of
      TypeDefn (bomId, maybeTyParams, bomTy) =>
        let
          val tyParams = Option.getOpt (maybeTyParams, [])
          val newBomEnv = foldr
            (fn (tyP, bEnv) => BOMEnv.extendTyParamEnv (bEnv, tyP))
            tyParams



    (* (CoreML.Dec.BomDec, bomEnv') *)
end
