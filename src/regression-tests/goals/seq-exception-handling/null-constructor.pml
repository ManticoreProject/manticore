exception E

val _ = print "prior to exception\n"
val ASDF = raise E
val _ = print "after exception\n"

