val ch = channel();

fun ping i = if i = 0 then () else (send(ch, i); ping(recv ch));

fun pong () = let
      val i = recv ch
      in
	print("received " ^ Int.toString i ^ "\n");
	send (ch, i-1);
	if (i = 1) then () else pong()
      end;

val b = Time.now ();
val _ = (spawn (ping 1000); pong());
val e = Time.now ();

val _ = print ("time elapsed: "^dtos (e-b)^"\n")
