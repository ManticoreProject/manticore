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
    sharing CoreBOM.Ast = Ast
    sharing BOMEnv.CoreBOM = CoreBOM
    sharing BOMEnv.Env = Env
  end
