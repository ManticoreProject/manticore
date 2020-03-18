(* This tests the correctness of pointerEq, which is meant to
    support the = operator on ref cells in SML, which is compared
    via pointer equality. *)

val a = Ref.new 13
val b = Ref.new 13
val c = a

val _ = if not (Ref.pointerEq (a, b))
          then print "ok.\n"
          else print "bug!\n"

val _ = if Ref.pointerEq (a, a)
          then print "ok.\n"
          else print "bug!\n"

val _ = if Ref.pointerEq (a, c)
          then print "ok.\n"
          else print "bug!\n"

val _ = if (Ref.get a) = (Ref.get b)
          then print "ok.\n"
          else print "bug!\n"
