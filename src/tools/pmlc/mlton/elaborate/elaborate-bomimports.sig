(* This module handles the elaboration of _import statements --- given
an _import statement, it enriches the BOM environment with type
information from the ML-side values and returns the new environment
--- in addition to BOM's export statements, namely _datatype, _type,
and _val. *)

signature ELABORATE_BOMIMPORTS_STRUCTS =
  sig
    structure Ast: AST
    structure CoreBOM: CORE_BOM
    structure ElaborateCore: ELABORATE_CORE
    structure ElaborateBOMCore: ELABORATE_BOMCORE
    structure Env: ELABORATE_ENV
    structure BOMEnv: ELABORATE_BOMENV
    structure CoreML: CORE_ML
    sharing Ast.Tyvar = CoreML.Tyvar
    sharing CoreML = Env.CoreML
    sharing ElaborateBOMCore.Ast = BOMEnv.Ast = CoreBOM.Ast = Env.Ast = Ast
    sharing ElaborateBOMCore.CoreBOM = CoreBOM
    sharing ElaborateBOMCore.BOMEnv = BOMEnv
    sharing ElaborateCore.Env = ElaborateBOMCore.Env = BOMEnv.Env = Env
    sharing ElaborateCore.Ast = ElaborateBOMCore.Ast = Ast
  end

signature ELABORATE_BOMIMPORTS =
  sig
    include ELABORATE_BOMIMPORTS_STRUCTS

    (* TODO: do we need to keep any of this? *)
    val elaborateBOMImport:
      (Ast.BOM.Import.t * {env: Env.t, bomEnv: BOMEnv.t} *  BOMEnv.MLTyEnv.t)
           -> (BOMEnv.t * BOMEnv.MLTyEnv.t)

    val elaborateBOMExport:
      (Ast.BOMExport.t * {env: Env.t, bomEnv: BOMEnv.t} * BOMEnv.MLTyEnv.t)
           -> (BOMEnv.t * BOMEnv.MLTyEnv.t)
  end
