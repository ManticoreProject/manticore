fun pick3 (ev1, ev2, ev3) = choose(ev1, choose(ev2, ev3));
fun f x = (1, x);
fun g x = (2, x);
fun sync2 (ev1, ev2) = sync(choose(wrap(ev1, f), wrap(ev2, g)));
()

