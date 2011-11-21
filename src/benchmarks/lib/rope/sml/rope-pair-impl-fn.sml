functor RopePairImplFn (
  structure R : ROPE
) : ROPE_PAIR = struct

exception UnequalLengths

structure R = R
type 'a rope = 'a R.rope

fun zip _ = raise Fail "todo"
fun zipEq _ = raise Fail "todo"
fun unzip _ = raise Fail "todo"
fun map _ = raise Fail "todo"
fun mapEq _ = raise Fail "todo"

end
