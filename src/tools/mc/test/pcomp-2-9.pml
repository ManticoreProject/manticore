(*** Utility Functions ***)

fun listmax (ns:int list) =
  let fun m (ns:int list, mx:int) =
            (case ns
               of nil => mx
                | (n::ns) => m (ns, if (n>mx) then n else mx))
  in
    case ns
      of n::ns => m (ns, n)
       | nil => ~111111 (* error *)
  end;

fun catw (sep, ss) =
  (case ss
     of nil => ""
      | s::nil => s
      | s::ss  => s ^ sep ^ (catw (sep, ss))
    (* end case *));

fun parString a =
  let val len = plen a
      fun build (curr, acc) =
        if curr=len
        then rev acc
        else build (curr+1, (Int.toString (a!curr)) :: acc)
  in
      "[|" ^ (catw (",", build (0, nil))) ^ "|]"
  end;

fun printPar(a) =
  (print "result:  ";
   print (parString(a));
   print "\n");

(* Tests (machine-generated) *)

val _ = print "testing parallel comprehension with 2 pbinds\n";
val parcomp =
 [| listmax(a::b::nil) |
      a in [|20,89,90,43,45,75,3,11,30,4|],
      b in [|63,4,44,58,85,75,25,52,32,19|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|63,89,90,58,85,75,25,52,32,19|]\n\n";


val _ = print "testing parallel comprehension with 3 pbinds\n";
val parcomp =
 [| listmax(a::b::c::nil) |
      a in [|22,22,77,81,44,78,45,7,23,9|],
      b in [|86,98,1,88,8,57,52,23,11,2|],
      c in [|99,79,77,38,11,90,60,60,83,21|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|99,98,77,88,44,90,60,60,83,21|]\n\n";


val _ = print "testing parallel comprehension with 4 pbinds\n";
val parcomp =
 [| listmax(a::b::c::d::nil) |
      a in [|76,56,85,77,58,68,78,77,60,28|],
      b in [|25,11,63,35,98,69,37,78,64,66|],
      c in [|19,92,18,8,50,61,18,80,37,60|],
      d in [|35,39,33,28,92,49,83,95,24,51|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|76,92,85,77,98,69,83,95,64,66|]\n\n";


val _ = print "testing parallel comprehension with 5 pbinds\n";
val parcomp =
 [| listmax(a::b::c::d::e::nil) |
      a in [|39,42,96,12,66,45,56,81,40,22|],
      b in [|96,76,2,70,0,44,40,59,61,34|],
      c in [|17,52,49,10,90,26,25,40,63,84|],
      d in [|63,51,64,62,29,73,35,73,19,82|],
      e in [|27,92,61,34,88,86,60,21,83,14|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|96,92,96,70,90,86,60,81,83,84|]\n\n";


val _ = print "testing parallel comprehension with 6 pbinds\n";
val parcomp =
 [| listmax(a::b::c::d::e::f::nil) |
      a in [|15,62,68,92,55,89,47,9,18,14|],
      b in [|89,34,19,40,12,98,7,61,84,76|],
      c in [|88,37,87,27,4,87,88,67,13,0|],
      d in [|64,50,26,12,11,97,48,24,5,45|],
      e in [|9,64,95,50,5,65,37,87,62,38|],
      f in [|43,6,14,95,2,43,98,24,16,11|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|89,64,95,95,55,98,98,87,84,76|]\n\n";


val _ = print "testing parallel comprehension with 7 pbinds\n";
val parcomp =
 [| listmax(a::b::c::d::e::f::g::nil) |
      a in [|86,65,77,82,76,21,79,18,82,31|],
      b in [|72,87,83,19,36,43,6,63,61,43|],
      c in [|10,14,55,79,49,44,42,49,16,3|],
      d in [|10,99,2,18,58,72,46,56,69,28|],
      e in [|39,57,59,14,17,21,15,25,65,80|],
      f in [|72,88,94,51,81,43,76,79,81,27|],
      g in [|25,48,83,16,39,27,79,89,7,69|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|86,99,94,82,81,72,79,89,82,80|]\n\n";


val _ = print "testing parallel comprehension with 8 pbinds\n";
val parcomp =
 [| listmax(a::b::c::d::e::f::g::h::nil) |
      a in [|51,11,69,30,54,66,74,66,4,31|],
      b in [|45,7,2,98,91,91,93,26,31,41|],
      c in [|96,21,53,61,90,57,15,37,28,71|],
      d in [|86,4,72,60,23,68,47,68,73,3|],
      e in [|90,90,69,91,32,96,24,66,85,66|],
      f in [|9,94,80,17,97,16,55,49,84,28|],
      g in [|81,0,15,34,22,47,66,38,61,16|],
      h in [|40,89,30,41,53,50,74,51,63,73|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|96,94,80,98,97,96,93,68,85,73|]\n\n";


val _ = print "testing parallel comprehension with 9 pbinds\n";
val parcomp =
 [| listmax(a::b::c::d::e::f::g::h::i::nil) |
      a in [|71,24,51,13,77,88,2,67,73,79|],
      b in [|77,26,10,17,10,83,89,56,23,58|],
      c in [|9,56,23,73,10,22,19,92,76,90|],
      d in [|28,45,87,0,82,52,60,14,23,81|],
      e in [|49,77,93,12,47,41,17,20,45,44|],
      f in [|77,3,43,8,56,81,49,85,88,3|],
      g in [|83,42,70,89,37,81,0,20,7,54|],
      h in [|51,31,55,57,88,42,73,37,76,47|],
      i in [|15,4,38,63,91,47,19,19,20,15|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|83,77,93,89,91,88,89,92,88,90|]\n\n";

()
