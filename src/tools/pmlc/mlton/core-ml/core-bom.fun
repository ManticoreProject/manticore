functor CoreBOM (S: CORE_BOM_STRUCTS) : CORE_BOM =
  struct
  open S

  structure AstBOM = structure Ast.AstBOM

  (* open AstBOM *)


  structure TyParam = struct
    open Ast.Tyvar
  end





  structure BomType = struct
    open AstBOM.BomType

    fun fromAst astType = astType

    fun arity ty =
      case node ty of
        Param => 1
      |
  end

  (* structure BomId = struct  *)
  (*   open AstBOM.BomId *)
  (* end  *)

  (* ... *)

  end
