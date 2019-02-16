
exception E

val _ = print "prior to exception\n"
val _ = (raise E) handle E => print "inside handler\n"
val _ = print "after excption\n"
