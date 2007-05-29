structure TypeOf : sig

    val exp : AST.exp -> Types.ty
    val pat : AST.pat -> Types.ty

  end = struct

    structure Ty = Types
    structure B = Basis

    fun exp (AST.LetExp(_, e)) = exp e
      | exp (AST.IfExp(_, _, _, ty)) = ty
      | exp (AST.CaseExp(_, _, ty)) = ty
      | exp (AST.ApplyExp(_, _, ty)) = ty
      | exp (AST.TupleExp es) = Ty.TupleTy(List.map exp es)
      | exp (AST.RangeExp(_, _, _, ty)) = B.parrayTy ty
      | exp (AST.PTupleExp of exp list
      | exp (AST.PArrayExp of exp list
      | exp (AST.ComprehendExp of (exp * (pat * exp) list * exp option)
      | exp (AST.SpawnExp of exp
      | exp (AST.ConstExp of const
      | exp (AST.VarExp of var * ty list
      | exp (AST.SeqExp of (exp * exp)
      | exp (AST.OverloadExp of overload_var ref
