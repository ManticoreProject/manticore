signature ELABORATE_BOMCORE_STRUCTS =
  sig
    structure Ast: AST
    structure CoreBOM: CORE_BOM
    structure Env: ELABORATE_ENV
    structure Decs: DECS
    structure BOMEnv: ELABORATE_BOMENV
    structure CoreML: CORE_ML
    sharing Env.Ast = CoreBOM.Ast = Ast
    sharing Ast.Tyvar = CoreML.Tyvar
    sharing CoreML = Decs.CoreML = Env.CoreML
    sharing Decs = Env.Decs
    sharing BOMEnv.CoreBOM = CoreBOM
    sharing BOMEnv.Env = Env
  end

signature ELABORATE_BOMCORE =
  sig
    include ELABORATE_BOMCORE_STRUCTS
    (* structure BOM: AST_BOM sharing Ast.BOM = BOM *)


    (* need to return a Decs.t for elaborate-modules.fun  *)
    val elaborateBOMDec:
      (Ast.BOM.Definition.t * {env: Env.t, bomEnv: BOMEnv.t})
        -> (Decs.dec * BOMEnv.t)

    (* Elaborate a BOM type. Needs to be exposed so that
    exports can be elaborated *)
    val elaborateBOMType: Ast.BOM.BOMType.t * {env: Env.t, bomEnv: BOMEnv.t}
        -> CoreBOM.BOMType.t

  end
