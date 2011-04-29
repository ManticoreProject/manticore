(* stress testing FT tuple unzipping *)

val arr = [| (n, (n+1, (n+2, n+3), n+4, n+5)) | n in [| 1 to 10 |] |]

val sums = [| a+b+c+d+e+f | (a, (b, (c, d), e, f)) in arr |]

val _ = Print.printLn "done."
