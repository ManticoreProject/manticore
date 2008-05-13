val c: int = 13;
fun c () = ();
val c: unit -> unit = c;
fun d y = (y, y);
fun id x = x;

val (f, hd::tail) = (id, d :: nil);

val (s,_) = f (hd "hello world\n");

val _ = print (s)
