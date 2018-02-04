(* adapted from a MLton benchmark *)


fun even' 0 = true
 | even' i = odd' (i-1)
and odd'  0 = false
 | odd'  i = even' (i-1)
 
fun even i = even' (Int.abs i)
fun odd i  = odd' (Int.abs i)


val _ = if (even 500000000) andalso not (odd 500000000)
            then Print.print "ok\n"
         else Print.print "ERROR\n"
