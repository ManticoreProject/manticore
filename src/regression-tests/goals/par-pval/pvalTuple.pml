


fun dummy () = let pval (a, (b, c)) = (1, (2, 3))
                   val  (a', (b', c')) = (4, (5, 6))
               in a + b + c + a' + b' + c'
               end



val _ = Print.printLn (Int.toString (dummy()))
