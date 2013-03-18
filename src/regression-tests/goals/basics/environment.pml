fun f b k =
    if b then (fn () => k())
    else (fn () => b)

val res = f true (f false (fn () => raise Fail ""))
val final = res()
val _ = print (Int.toString (if final then 1 else 0))

fun f b k =
    if b then (fn () => (fn () => b)())
    else (fn () => b)

val res = f true (f false (fn () => raise Fail ""))
val final = res()

val _ = print (Int.toString (if final then 1 else 0))
