structure L = List

val par = [| 10, 11, 12, 13, 14, 15, 16, 17, 18 |]

val x0 = par!0
val x4 = par!4
val x8 = par!8

val list = L.CONS(x0, L.CONS(x4, L.CONS(x8, nil)))

fun catw (sep, ss) =
  (case ss
     of nil => ""
      | L.CONS(s, nil) => s
      | L.CONS(s, ss) => s ^ sep ^ (catw (sep, ss))
    (* end case *))

val s = catw (",", List.map Int.toString list)

val _ = Print.printLn ("[" ^ s ^ "]")




