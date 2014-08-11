signature ELABORATE_BOMCORE_STRUCTS =
  sig
    structure Ast: AST
    structure CoreBOM: CORE_BOM
    structure Env: ELABORATE_ENV
    structure BOMEnv: ELABORATE_BOMENV
    sharing Ast = Env.Ast
    sharing Ast.Tyvar = CoreML.TyVar
    sharing CoreML = Decs.CoreML = Env.CoreML
    sharing Decs = Env.Decs
  end

signature ELABORATE_BOMCORE =
  sig
    include ELABORATE_BOMCORE_STRUCTS
    structure AstBOM: AST_BOM


    val elaborateBomDec:
      AstBOM.Definition.t * {env: Env.t, bomEnv: BOMEnv.t} -> CoreBOM.Decs.t

  end
