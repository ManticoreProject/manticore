

fun f c msg = (print msg; c)

val prT = f true
val prF = f false

(* check order *)
val _ = prT "1" andalso prT "2" andalso prT "3"
val _ = print "\n"

val _ = prF "a" orelse prF "b" orelse prF "c"
val _ = print "\n"


(* check short circuiting *)
val _ = prF "yes" andalso prT "no" andalso prT "no"
val _ = print "\n"

val _ = prT "yes" orelse prT "no" orelse prT "no"
val _ = print "\n"
