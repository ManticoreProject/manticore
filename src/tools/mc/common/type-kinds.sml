(* type-kinds.sml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * A kind system for BOM, CPS, and CFG types.
 *)

structure TypeKinds : sig

    eqtype kind

    val kBoxed : kind		(* heap pointer *)
    val kUnboxed : kind		(* tagged integer *)
    val kMLType	 : kind		(* ML type: either K_BOXED or K_UNBOXED *)
    val kNotMLType : kind	(* bit pattern outside K_MLTY *)
    val kUniform : kind		(* types that the GC understands: either K_MLTY or K_NOTML *)
    val kRaw : kind		(* hardware integers and floats *)
    val kVar : kind		(* types that a variable can have *)
    val kMem : kind		(* types that can be stored in memory *)
    val kType : kind		(* type (any of the above kinds) *)

  (* convert to string *)
    val toString : kind -> string

  (* isKind k1 k2 --- returns true if k2 is a subkind of k1 *)
    val isKind : kind -> kind -> bool

  end = struct

    type kind = word

  (* we use a bit pattern to represent kinds so that the isKind test is
   * fast.
   *)
    val kBoxed		= 0wx01		(* 000001 *)
    val kUnboxed	= 0wx02		(* 000010 *)
    val kMLType		= 0wx03		(* 000011 *)
    val kNotMLType	= 0wx04		(* 000100 *)
    val kUniform	= 0wx07		(* 000111 *)
    val kRaw		= 0wx08		(* 001000 *)
    val kVar		= 0wx0f		(* 001111 *)
    val kMem		= 0wx1f		(* 011111 *)
    val kType		= 0wx3f		(* 111111 *)

    fun toString k = (case k
	   of 0wx01 => "BOXED"
	    | 0wx02 => "UNBOXED"
	    | 0wx03 => "MLTY"
	    | 0wx04 => "NOTML"
	    | 0wx07 => "UNIFORM"
	    | 0wx08 => "RAW"
	    | 0wx0f => "VAR"
	    | 0wx1f => "MEM"
	    | 0wx3f => "TYPE"
	    | _ => raise Fail("bogus kind rep 0x" ^ Word.toString k)
	  (* end case *))

  (* isKind k1 k2 --- returns true if k2 is a subkind of k1 *)
    fun isKind (k1 : kind) k2 = (Word.andb(k1, k2) = k2)

  end
