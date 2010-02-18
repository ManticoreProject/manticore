datatype ('a, 'b) either = LEFT of 'a | RIGHT of 'b

fun f x =
    (case x
      of LEFT (x, y) => print (Int.toString (x + y))
       | RIGHT b => if b then print "hi" else print "there")

val _ = f (LEFT (2, 3))
val _ = f (RIGHT false)

