(*** Utility Functions ***)

  fun parString a =
    let val len = plen a
        fun build (curr, acc) =
          if curr=len
          then rev acc
          else build (curr+1, (itos (a!curr)) :: acc)
    in
        "[|" ^ (concatWith (",", build (0, nil))) ^ "|]"
    end;

  fun printPar(a) =
    (print (parString(a));
     print "\n");

(* A test to see if ranges are correctly built when necessary. *)

  val r1 = [| 1 to 20 |];
  val r2 = [| 1 to 20 by 2 |];
  val r3 = [| 1 to 20 by 500 |];
  val r4 = [| 20 to 2 by ~1 |];

val _ =  app (printPar, r1::r2::r3::r4::nil)

