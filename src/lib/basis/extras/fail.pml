(* fail.pml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 *)

structure Fail = struct

(* This function prints a message on the way to failing.     *)
(* This is useful because the messages in the Fail exception *)
(*   are not currently printed when raised.                  *)
  fun fail module = let
    fun fm func info = let
      val msg = "Fail in " ^ module ^ "." ^ func ^ ": " ^ info
      in
        (Print.printLn msg; raise Fail msg)
      end
    in
      fm
    end

end
