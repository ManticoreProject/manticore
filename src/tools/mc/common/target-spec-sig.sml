(* target-spec-sig.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Target OS/architecture specific information.
 *)

signature TARGET_SPEC =
  sig

    val targetOS : string
    val targetArch : string

    val wordSzB : word		(* number of bytes in a pointer-sized word *)
    val wordAlignB : word	(* byte alignment of pointers *)

  end

