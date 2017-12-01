(* prim-contract.sml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure PrimContract : sig

  (* result of primop contraction *)
    datatype const_fold_result
      = FAIL
      | OK of ((BOM.var list  * BOM.rhs) list * BOMUtil.subst)

    val contract : (BOMUtil.subst * BOM.var * BOM.prim) -> const_fold_result

  end = struct

    structure B = BOM
    structure BV = B.Var
    structure BTy = BOMTy
    structure P = Prim
    structure IL = IntegerLit
    structure L = Literal
    structure U = BOMUtil
    structure CA = ConstArithGlueFn (
                    structure S = SignedWrappingArith
                    structure U = UnsignedWrappingArith
                    structure B = BitwiseConstArith)

  (* result of primop contraction *)
    datatype const_fold_result
      = FAIL
      | OK of ((B.var list * B.rhs) list * BOMUtil.subst)

    datatype 'a arith_result
      = NEW of 'a
      | SAME

    datatype binding
      = Var of B.var
      | Prim of B.prim
      | Int of IL.integer
      | Enum of word

    fun bind x = (case BV.kindOf x
	  of B.VK_RHS(B.E_Const(Literal.Int n, _)) => Int n
	   | B.VK_RHS(B.E_Const(Literal.Enum n, _)) => Enum n
	   | B.VK_RHS(B.E_Prim p) => Prim p
	   | _ => Var x
	 (* end case *))
      

    fun contract (env, x, p) = let
        
        (* common cases *)
        fun replRHS rhs = OK ([ ([x], rhs) ], env)
        
        and replPrim p = replRHS (B.E_Prim p)
        
        and const (lit, ty) = B.E_Const(lit, ty)
        
        and handleRes res = (case res
          of SAME => FAIL
           | NEW (Int c)  => replRHS( const(L.Int c, BV.typeOf x) )
           | NEW (Enum c) => replRHS( const(L.Enum c, BV.typeOf x) )
           | NEW (Prim p) => replPrim p
           | NEW (Var y)  => OK ([], U.extend(env, x, y))
          (* end case *))
        
        (* algSimp are operation specific simplifications 
           we handle the case of two constant ints immediately.
        *)
        fun try width algSimp arith ab = (case ab
            of (Int a, Int b) => handleRes (NEW (Int (arith (width, a, b))))
             | ab             => handleRes (algSimp ab)
            (* end case *))
            
        fun try1 width arith a = (case a
            of Int a => handleRes (NEW (Int (arith (width, a))))
             | _     => handleRes SAME
            (* end case *))
            
        fun try_chk0 width algSimp arith ab = (case ab
            of (_, Int 0) => handleRes SAME  (* don't do anything if divisor is 0 *)
             | ab     => try width algSimp arith ab
            (* end case *))
        
        (****************************
          algebraic simplifications.
        *****************************)
          
        fun NOTHING _ = SAME
          
        fun add (other, Int 0) = NEW other
          | add (Int 0, other) = NEW other
          | add _ = SAME
          
        fun mul (_, Int 0) = NEW (Int 0)
          | mul (Int 0, _) = NEW (Int 0)
          | mul (other, Int 1) = NEW other
          | mul (Int 1, other) = NEW other
          | mul _ = SAME
          (* NOTE: could turn -1 * x into Negate x. get type from x *)
          
        fun sub (other, Int 0) = NEW other
          | sub (Var a, Var b) = if BV.same (a, b) then NEW (Int 0) else SAME
          | sub _ = SAME
          (* NOTE: could turn 0 - b into Negate b. get type from x *)
          
        fun divv (Int 0, _) = NEW (Int 0)
          | divv (any, Int 1) = NEW any
          | divv _ = SAME
          
        fun andd (Int 0, _) = NEW (Int 0)
          | andd (_, Int 0) = NEW (Int 0)
          | andd _ = SAME
          
        fun or (Int 0, any) = NEW any
          | or (any, Int 0) = NEW any
          | or _ = SAME
          
        fun xor (Var x, Var y) = if BV.same(x, y) then NEW (Int 0) else SAME
          | xor _ = SAME
          
        (* TODO: could reduce verbosity in the case below by introducing a
           try-function specialized to 32 bit, etc. *)
          
        (****************************)
    in
        if not (PrimUtil.isPure p)
        then FAIL
        else case PrimUtil.map bind p
            (* signed 32-bit arith *)
         of   P.I32Add  ab => try 32 add CA.sAdd ab
            | P.I32Sub  ab => try 32 sub CA.sSub ab
            | P.I32Mul  ab => try 32 mul CA.sMul ab
            | P.I32Div  ab => try_chk0 32 divv CA.sQuot ab
            | P.I32Mod  ab => try_chk0 32 NOTHING CA.sRem ab
            | P.I32Neg  a  => try1 32 CA.sNeg a
            
            (* unsigned 32-bit arith *)
            | P.U32Mul  ab => try 32 mul CA.uMul ab
            | P.U32Div  ab => try_chk0 32 divv CA.uDiv ab
            | P.U32Rem  ab => try_chk0 32 NOTHING CA.uMod ab
            
            (* 32-bit bitwise ops *)
            | P.I32AndB ab => try 32 andd CA.bAnd ab
            | P.I32OrB  ab => try 32 or CA.bOr ab
            | P.I32XorB ab => try 32 xor CA.bXor ab
            | P.I32NotB a  => try1 32 CA.bNot a
            | P.I32LSh  ab => try 32 NOTHING CA.uShL ab
            | P.I32RSh  ab => try 32 NOTHING CA.uShR ab  (*right (logical) shift*)
            
            (* signed 64-bit arith *)
            | P.I64Add  ab => try 64 add CA.sAdd ab
            | P.I64Sub  ab => try 64 sub CA.sSub ab
            | P.I64Mul  ab => try 64 mul CA.sMul ab
            | P.I64Div  ab => try_chk0 64 divv CA.sQuot ab
            | P.I64Mod  ab => try_chk0 64 NOTHING CA.sRem ab
            | P.I64Neg  a  => try1 64 CA.sNeg a
            
            (* unsigned 64-bit arith *)
            | P.U64Mul  ab => try 64 mul CA.uMul ab
            | P.U64Div  ab => try_chk0 64 divv CA.uDiv ab
            | P.U64Rem  ab => try_chk0 64 NOTHING CA.uMod ab
            
            (* 64-bit bitwise ops *)
            | P.I64AndB ab => try 64 andd CA.bAnd ab
            | P.I64OrB  ab => try 64 or CA.bOr ab
            | P.I64XorB ab => try 64 xor CA.bXor ab
            | P.I64NotB a  => try1 64 CA.bNot a
            | P.I64LSh  ab => try 64 NOTHING CA.uShL ab
            | P.I64RSh  ab => try 64 NOTHING CA.uShR ab  (*right (logical) shift*)
            
            | _ => FAIL
    end

  end
