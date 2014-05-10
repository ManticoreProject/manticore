fun f () = f()

val x = if false andalso f()
        then print "impossible\n"
	else print "correct answer\n"

