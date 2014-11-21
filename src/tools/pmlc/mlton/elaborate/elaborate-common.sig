signature ELABORATE_COMMON =
  sig
    structure Ast: AST
    structure CoreML: CORE_ML
    structure CoreBOM: CORE_BOM
    structure Decs: DECS
    structure Env: ELABORATE_ENV
    structure BOMEnv: ELABORATE_BOMENV
    sharing Ast = Env.Ast
    sharing Ast.Tyvar = CoreML.Tyvar
    sharing CoreML = Decs.CoreML = Env.CoreML
    sharing Decs = Env.Decs
    sharing Ast.BOM = CoreBOM.BOM = BOMEnv.BOM
    sharing BOMEnv.CoreBOM = CoreBOM
  end
