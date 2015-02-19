(* This module handles the elaboration of _import statements --- given
an _import statement, it enriches the BOM environment with type
information from the ML-side values and returns the new environment. *)

signature ELABORATE_BOMIMPORTS_STRUCTS =
  sig
    structure Ast: AST
    structure CoreBOM: CORE_BOM
    structure ElaborateCore: ELABORATE_CORE
    structure Env: ELABORATE_ENV
    structure BOMEnv: ELABORATE_BOMENV
    structure CoreML: CORE_ML
    sharing Ast = Env.Ast
    sharing Ast.Tyvar = CoreML.Tyvar
    sharing CoreML = Env.CoreML
    sharing CoreBOM.BOM = Ast.BOM = BOMEnv.BOM
    sharing BOMEnv.CoreBOM = CoreBOM
    sharing ElaborateCore.Env = BOMEnv.Env = Env
    sharing ElaborateCore.Ast = Ast
  end

signature ELABORATE_BOMIMPORTS =
  sig
    include ELABORATE_BOMIMPORTS_STRUCTS
    (* structure BOM: AST_BOM *)

    (* TODO: do we need to keep any of this? *)
    val elaborateBOMImport:
      (Ast.BOM.Import.t * {env: Env.t, bomEnv: BOMEnv.t} *  BOMEnv.MLTyEnv.t)
           -> (BOMEnv.t * BOMEnv.MLTyEnv.t)
  end
