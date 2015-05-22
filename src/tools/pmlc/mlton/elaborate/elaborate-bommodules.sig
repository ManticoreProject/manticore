signature ELABORATE_BOMMODULES_STRUCTS =
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

signature ELABORATE_BOMMODULES =
  sig
    include ELABORATE_BOMMODULES_STRUCTS
    structure AstBOM: AST_BOM


    val elaboratePrimDataType:
      AstBOM.PrimDataType.t * {env: Env.t, bomEnv: BOMEnv.t} -> Decs.t
    val elaboratePrimTycon:
      AstBOM.PrimTycon.t * {env: Env.t, bomEnv: BOMEnv.t} -> Decs.t
    val elaboratePrimVal:
      AstBOM.PrimVal.t * {env: Env.t, bomEnv: BOMEnv.t} -> Decs.t
  end
