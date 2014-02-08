val x = Rope.tabulateSequential (fn x => x) (1, 30)

val px = PArray.fromRope x

fun fib i = if i <= 2
            then 1
            else fib (i-1) + fib(i-2)

val mapped = PArray.map (fn x => (fib 30; x+1)) px


val vtos = PArray.toString Int.toString ","

val _ = Print.printLn(vtos px)
val _ = Print.printLn(vtos mapped)


