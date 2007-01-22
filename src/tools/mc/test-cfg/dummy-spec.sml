(* dummy-spec.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Dummy Target OS/architecture specific information.
 *)

structure DummySpec : TARGET_SPEC =
  struct

    val targetOS = "dummy"
    val targetArch = "dummy"

    val wordSzB = 0w4
    val wordAlignB = 0w4

  end

