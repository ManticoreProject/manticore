signature ELABORATE_BOMCORE_STRUCTS =
  sig
    structure Ast: AST
    structure CoreBOM: CORE_BOM
    structure Env: ELABORATE_ENV
    structure Decs: DECS
    structure BOMEnv: ELABORATE_BOMENV
    structure CoreML: CORE_ML
    sharing Ast = Env.Ast
    sharing Ast.Tyvar = CoreML.Tyvar
    sharing CoreML = Decs.CoreML = Env.CoreML
    sharing Decs = Env.Decs
    sharing CoreBOM.BOM = Ast.BOM = BOMEnv.BOM
    sharing BOMEnv.CoreBOM = CoreBOM
  end

signature ELABORATE_BOMCORE =
  sig
    include ELABORATE_BOMCORE_STRUCTS
    structure BOM: AST_BOM


    (* need to return a Decs.t for elaborate-modules.fun  *)
    val elaborateBomDec:
      (BOM.Definition.t * {env: Env.t, bomEnv: BOMEnv.t})
        -> (Decs.dec * BOMEnv.t)
  end
