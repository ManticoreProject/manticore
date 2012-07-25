(* mergesort.pml
 * 
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Mergesort process network (borrowed from Concurrent Programming in ML Section 4.1).
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
    in
       spawn (let
        val x1 = recv ch
        in case x1
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
			          send (ch1, x1);
				  send (ch2, x2);
				  split (ch, ch1, ch2);
				  merge (ch1, ch2, ch)
			       end
                      (* end case *))
                  end
              (* end case *)
           end);
         ch
    end
;

val n = 10;

fun doit () = let
    val ch = mergesort ()
    fun f i = n - i
    val ls = tab (f, 0, n, 1)
    fun sendLs ls = (case ls
        of nil => send (ch, NONE)
	 | x :: xs => (send (ch, SOME x); sendLs xs)
        (* end case *))
    fun recvLs () = (case (recv ch)
        of NONE => ()
	 | SOME i => (print ((Int.toString i)^"\n"); recvLs ())
        (* end case *))
    in
       	print "start sorting\n";
	sendLs ls;
	print "done sending\n";
	recvLs ();
	print "done sorting\n"
    end
;

val _ = doit ()
