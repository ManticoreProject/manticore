signature ELABORATE_BOMIMPORTS_STRUCTS =
  sig
    structure Ast: AST
    structure CoreBOM: CORE_BOM
    structure Env: ELABORATE_ENV
    structure BOMEnv: ELABORATE_BOMENV
    structure CoreML: CORE_ML
    sharing Ast = Env.Ast
    sharing Ast.Tyvar = CoreML.Tyvar
    sharing CoreML = Env.CoreML
    sharing CoreBOM.BOM = Ast.BOM = BOMEnv.BOM
    sharing BOMEnv.CoreBOM = CoreBOM
  end

signature ELABORATE_BOMIMPORTS =
  sig
    include ELABORATE_BOMIMPORTS_STRUCTS
    structure BOM: AST_BOM

    (* TODO: do we need to keep any of this? *)
    val elaborateBomImports:
      (BOM.Import.t * {env: Env.t * bomEnv: BOMEnv.t}) -> BOMEnv.t
  end
