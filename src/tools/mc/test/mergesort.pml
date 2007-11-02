(* Test the mergesort example from the CML book.
 *) 

fun split (inCh, outCh1, outCh2) = let
    fun loop (v, outCh1, outCh2) = (case v
        of NONE => ( send (outCh1, NONE), send (outCh2, NONE) )
	 | SOME x => ( send (outCh1, v); loop (recv inCh, outCh2, outCh1) )
         (* end case *))
    in
       loop (recv inCh, outCh1, outCh2)
    end
;

fun merge (inCh1, inCh2, outCh) = let
    fun merge' (x, y) = (case (x, y)
        of (NONE, NONE) => send (outCh, NONE)
	 | (NONE, SOME _) => ( send (outCh, y); merge' (NONE, recv inCh2) )
	 | (SOME _, NONE) => ( send (outCh, x); merge' (recv inCh1, NONE) )
	 | (SOME xVal, SOME yVal) => if xVal <= yVal
           then ( send (outCh, x); merge' (recv inCh1, y) )
	   else ( send (outCh, y); merge' (x, recv inCh2) )
        (* end case *))
    in
        merge' (recv inCh1, recv inCh2)
    end
;

fun mergesort () = let
    val ch = channel ()
    val x1 = recv ch
    in
       spawn (
        (case x1
          of NONE => send (ch, NONE)
	   | SOME x => let
             val x2 = recv ch
             in
		 (case x2
                   of NONE => ( send (ch, x1); send (ch, NONE) )
		    | SOME x => let
                      val ch1 = mergesort ()
                      val ch2 = mergesort ()
		      in
			  send (ch, x1);
			  send (ch, x2);
			  split (ch, ch1, ch2);
			  merge (ch1, ch2, ch)
		      end
                 (* end case *))
             end
         (* end case *)) );
         ch
    end
;

val n = 10;

fun test () = let
    val inCh1 = channel ()
    val inCh2 = channel ()
    val outCh1 = channel ()
    val outCh2 = channel ()

    fun inLoop (i, inCh, x) = if i < n
        then ( send (inCh, SOME (i*2 + (if x then 1 else 0))); 
               print ( "sent " ^ itos i ^ "\n"); 
               inLoop (i+1, inCh, x) )
        else send (inCh, NONE)

    fun outLoop (i, outCh) = (case recv outCh
        of NONE => print "outLoop done\n"
	 | SOME x => ( print ( "out loop iteration=" ^ itos x ^ " on thread " ^ itos i ^ "\n" ); outLoop (i, outCh) )
        (* end case *))

    in
        spawn (inLoop (0, inCh1, true));
	spawn (inLoop (0, inCh2, false));
	spawn (outLoop (1, outCh1));
        merge (inCh1, inCh2, outCh1)
    end
;

test ()
