(* amd64-types.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

functor AMD64TypesFn (
	structure Spec : TARGET_SPEC
  ) : ARCH_TYPES = struct

    structure M = CFG
    structure Ty = CFGTy

    val wordSzB = IntInf.toInt Spec.ABI.wordSzB
    val wordAlignB = IntInf.toInt Spec.ABI.wordAlignB

    fun alignedRawTySzB Ty.T_Vec128 = 16
      | alignedRawTySzB _ = wordAlignB
    fun alignedTySzB ty = (case ty
	  of M.T_Raw rt => alignedRawTySzB rt
	   | _ => wordAlignB
	(* esac *))

    fun sizeOfRawTyB rt = (case rt
	  of Ty.T_Byte => 1
	   | Ty.T_Short => 2
	   | Ty.T_Int => 4
	   | Ty.T_Long => 8
	   | Ty.T_Float => 4
	   | Ty.T_Double => 8
	   | Ty.T_Vec128 => 16
	(* escac *))
    fun szOfB ty =
	(case ty
	  of M.T_Raw rt => sizeOfRawTyB rt
	   | _ => wordSzB
	(* esac *))
    fun szOf ty = szOfB ty * 8

    fun szOfIx (ty, i) = (case ty
        of ( Ty.T_Tuple (_, tys) ) => szOf (List.nth (tys, i))
         | ( Ty.T_OpenTuple tys ) => szOf (List.nth (tys, i))
	 | _ => raise Fail "attempt to select from an opaque type"
        (* end case *))

  end (* AMD64TypesFn *)
