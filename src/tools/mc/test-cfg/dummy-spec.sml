(* dummy-spec.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Dummy Target OS/architecture specific information.
 *)

structure DummySpec : TARGET_SPEC =
  struct

    val archName = "dummy"
    val abiName = "dummy"
    val osName = "dummy"

    val wordSzB = 0w4
    val wordAlignB = 0w4

    val boolSzB = 0w4

  (* representation of True and False *)
    val trueRep : IntegerLit.integer = 1
    val falseRep : IntegerLit.integer = 0

    val spillAreaSzB = 0w1024

  end

