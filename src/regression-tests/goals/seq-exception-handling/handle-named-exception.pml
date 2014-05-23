
exception E

val _ = print "prior to exception\n"
val _ = (raise E) handle E => ()
val _ = print "after excption\n"

