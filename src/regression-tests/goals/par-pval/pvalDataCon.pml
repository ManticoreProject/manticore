
datatype foo = Foo of int * int * int

fun f() = Foo(1, 2, 3)

fun dummy () = let pval Foo(a, b, c) = f()
                   val  Foo(a', b', c') = f()
               in a + b + c + a' + b' + c'
               end



val _ = Print.printLn (Int.toString (dummy()))
