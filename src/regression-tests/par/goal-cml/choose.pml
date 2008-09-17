val ch1 = channel();
val ch2 = channel();

fun t (x, ch, i) = if (i = 0)
    then () 
    else (
	send(ch, x);
	t(x, ch, i-1))
;

fun server () = 
    sync (choose(wrap (recvEvt(ch1), let fun f (x) = (print (itos x^"\n"); server()) in f end),
		 wrap (recvEvt(ch2), let fun f (x) = (print (itos x^"\n"); server()) in f end)))
;

val _ = spawn(server());
val _ = spawn (t(0, ch1, 10));
t(1, ch2, 10)
