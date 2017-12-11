(* cond-contract.sml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure CondContract : sig

    datatype result = FALSE | TRUE | UNKNOWN

    val contract : BOM.cond -> result

  end = struct

    structure B = BOM
    structure BV = B.Var
    structure BTy = BOMTy
    structure P = Prim

    datatype result = FALSE | TRUE | UNKNOWN

    (* TODO fold floats *)
    datatype binding
      = Var of B.var
      | Prim of B.prim
      | Int of IntInf.int
      | Enum of word

    fun same (Var x, Var y) = if BV.same(x, y) then TRUE else UNKNOWN
      | same (Int a, Int b) = if (a = b) then TRUE else FALSE
      | same (Enum a, Enum b) = if (a = b) then TRUE else FALSE
      | same _ = UNKNOWN
      
    fun diff ab = (case same ab
        of UNKNOWN => UNKNOWN
         | TRUE => FALSE
         | FALSE => TRUE
        (* end case *))
      
    fun less (Var x, Var y) = if BV.same(x, y) then FALSE else UNKNOWN
      | less (Int a, Int b) = if (a < b) then TRUE else FALSE
      | less _ = UNKNOWN 
      
    fun great (Var x, Var y) = if BV.same(x, y) then FALSE else UNKNOWN
      | great (Int a, Int b) = if (a > b) then TRUE else FALSE
      | great _ = UNKNOWN 
      
    fun lessEq (Var x, Var y) = if BV.same(x, y) then TRUE else UNKNOWN
      | lessEq (Int a, Int b) = if (a <= b) then TRUE else FALSE
      | lessEq _ = UNKNOWN 
      
    fun greatEq (Var x, Var y) = if BV.same(x, y) then TRUE else UNKNOWN
      | greatEq (Int a, Int b) = if (a >= b) then TRUE else FALSE
      | greatEq _ = UNKNOWN 

    fun bind x = (case BV.kindOf x
      of B.VK_RHS(B.E_Const(Literal.Int n, _)) => Int n
       | B.VK_RHS(B.E_Const(Literal.Enum n, _)) => Enum n
       | B.VK_RHS(B.E_Prim p) => Prim p
       | _ => Var x
     (* end case *))

    fun contract cond = let
      fun delete () = CondUtil.app Census.decUseCnt cond
      fun withDel f ab = (case f ab
        of UNKNOWN => UNKNOWN
         | res => (delete (); res)
        (* end case *))
        
      val neq = withDel diff
      val eq = withDel same
      val lt = withDel less
      val ltEq = withDel lessEq
      val gt = withDel great
      val gtEq = withDel greatEq
      
      in
      if not (CondUtil.isPure cond) 
      then UNKNOWN
      else
        case CondUtil.map bind cond
         of P.isBoxed(Enum x) => (delete (); FALSE)
          | P.isUnboxed(Enum x) => (delete (); TRUE)
          | P.Equal(a, b) => eq(a, b)
          | P.NotEqual(a, b) => neq(a, b)
          | P.EnumEq(a, b) => eq(a, b)
          | P.EnumNEq(a, b) => neq(a, b)
          | P.I32Eq(a, b) => eq(a, b)
          | P.I32NEq(a, b) => neq(a, b)
          | P.I32Lt(a, b) => lt(a, b)
          | P.I32Lte(a, b) => ltEq(a, b)
          | P.I32Gt(a, b) => gt(a, b)
          | P.I32Gte(a, b) => gtEq(a, b)
          | P.U32Lt(a, b) => lt(a, b)
          | P.I64Eq(a, b) => eq(a, b)
          | P.I64NEq(a, b) => neq(a, b)
          | P.I64Lt(a, b) => lt(a, b)
          | P.I64Lte(a, b) => ltEq(a, b)
          | P.I64Gt(a, b) => gt(a, b)
          | P.I64Gte(a, b) => gtEq(a, b)
          | P.U64Lt(a, b) => lt(a, b)
          | P.F32Eq(a, b) => eq(a, b)
          | P.F32NEq(a, b) => neq(a, b)
          | P.F64Eq(a, b) => eq(a, b)
          | P.F64NEq(a, b) => neq(a, b)
          | P.AdrEq(a, b) => eq(a, b)
          | P.AdrNEq(a, b) => neq(a, b)
          | _ => UNKNOWN
        (* end case *)
      end

  end
