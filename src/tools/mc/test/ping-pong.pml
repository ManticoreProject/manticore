val ch = channel();

fun ping i = if i = 0 then () else (send(ch, i); ping(recv ch));

fun pong () = let
      val i = recv ch
      in
	print("received " ^ itos i ^ "\n");
	send (ch, i-1);
	if (i = 1) then () else pong()
      end;

val b = gettimeofday ();
val _ = (spawn (ping 10); pong());
val e = gettimeofday ();

print ("time elapsed: "^dtos (e-b)^"\n")
