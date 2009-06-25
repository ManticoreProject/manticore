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

val parcomp =
 [| listmax(a::b::nil) |
      a in [|4|],
      b in [|4|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|4|]\n";


val parcomp =
 [| listmax(a::b::nil) |
      a in [|3,8|],
      b in [|0,2|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|3,8|]\n";


val parcomp =
 [| listmax(a::b::nil) |
      a in [|4,8,3|],
      b in [|3,3,6|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|4,8,6|]\n";


val parcomp =
 [| listmax(a::b::nil) |
      a in [|6,5,6,3|],
      b in [|7,2,3,7|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|7,5,6,7|]\n";


val parcomp =
 [| listmax(a::b::nil) |
      a in [|4,0,6,0,5|],
      b in [|3,5,3,5,8|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|4,5,6,5,8|]\n";


val parcomp =
 [| listmax(a::b::nil) |
      a in [|6,0,1,6,6,2|],
      b in [|7,9,0,5,1,3|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|7,9,1,6,6,3|]\n";


val parcomp =
 [| listmax(a::b::nil) |
      a in [|7,7,0,8,7,6,0|],
      b in [|2,6,5,1,1,3,7|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|7,7,5,8,7,6,7|]\n";


val parcomp =
 [| listmax(a::b::nil) |
      a in [|5,4,4,9,6,9,8,7|],
      b in [|9,8,3,3,5,1,8,8|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|9,8,4,9,6,9,8,8|]\n";


val parcomp =
 [| listmax(a::b::nil) |
      a in [|9,7,5,1,5,5,9,7,3|],
      b in [|2,2,8,4,4,5,6,5,0|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|9,7,8,4,5,5,9,7,3|]\n";


val parcomp =
 [| listmax(a::b::nil) |
      a in [|3,0,4,3,0,3,5,3,6,7|],
      b in [|8,0,0,3,4,6,9,0,1,1|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|8,0,4,3,4,6,9,3,6,7|]\n";


val parcomp =
 [| listmax(a::b::nil) |
      a in [|4,6,7,5,2,1,1,2,3,1,2|],
      b in [|8,9,3,8,0,2,7,2,7,7,8|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|8,9,7,8,2,2,7,2,7,7,8|]\n";


val parcomp =
 [| listmax(a::b::nil) |
      a in [|7,8,5,5,5,8,4,3,6,2,7,1|],
      b in [|9,8,7,6,2,4,4,0,0,1,4,7|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|9,8,7,6,5,8,4,3,6,2,7,7|]\n";


val parcomp =
 [| listmax(a::b::nil) |
      a in [|5,4,1,4,2,7,4,8,3,4,5,7,8|],
      b in [|6,1,0,5,3,3,9,2,1,6,4,6,1|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|6,4,1,5,3,7,9,8,3,6,5,7,8|]\n";


val parcomp =
 [| listmax(a::b::c::nil) |
      a in [|6|],
      b in [|8|],
      c in [|8|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|8|]\n";


val parcomp =
 [| listmax(a::b::c::nil) |
      a in [|0,3|],
      b in [|9,1|],
      c in [|1,0|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|9,3|]\n";


val parcomp =
 [| listmax(a::b::c::nil) |
      a in [|7,1,3|],
      b in [|8,8,0|],
      c in [|6,0,0|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|8,8,3|]\n";


val parcomp =
 [| listmax(a::b::c::nil) |
      a in [|8,8,9,2|],
      b in [|0,1,5,7|],
      c in [|3,5,9,7|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|8,8,9,7|]\n";


val parcomp =
 [| listmax(a::b::c::nil) |
      a in [|5,7,5,3,1|],
      b in [|4,2,8,5,6|],
      c in [|3,0,6,1,5|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|5,7,8,5,6|]\n";


val parcomp =
 [| listmax(a::b::c::nil) |
      a in [|4,8,4,1,4,8|],
      b in [|5,9,2,3,9,2|],
      c in [|7,4,3,3,8,2|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|7,9,4,3,9,8|]\n";


val parcomp =
 [| listmax(a::b::c::nil) |
      a in [|5,8,6,7,1,0,0|],
      b in [|2,5,0,8,0,2,6|],
      c in [|0,3,5,8,8,8,6|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|5,8,6,8,8,8,6|]\n";


val parcomp =
 [| listmax(a::b::c::nil) |
      a in [|4,3,5,8,5,9,9,5|],
      b in [|6,9,6,5,3,4,3,9|],
      c in [|8,6,4,2,7,3,3,4|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|8,9,6,8,7,9,9,9|]\n";


val parcomp =
 [| listmax(a::b::c::nil) |
      a in [|9,9,4,2,2,5,3,4,0|],
      b in [|9,2,7,1,1,9,9,5,1|],
      c in [|7,1,2,7,2,9,2,7,7|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|9,9,7,7,2,9,9,7,7|]\n";


val parcomp =
 [| listmax(a::b::c::nil) |
      a in [|8,1,2,6,0,4,8,6,7,3|],
      b in [|4,0,6,9,6,7,8,7,6,9|],
      c in [|6,1,9,1,5,7,2,6,5,0|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|8,1,9,9,6,7,8,7,7,9|]\n";


val parcomp =
 [| listmax(a::b::c::nil) |
      a in [|0,8,7,6,1,2,5,8,7,0,1|],
      b in [|1,9,3,3,1,7,1,9,9,7,0|],
      c in [|4,8,6,5,1,9,1,6,1,8,5|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|4,9,7,6,1,9,5,9,9,8,5|]\n";


val parcomp =
 [| listmax(a::b::c::nil) |
      a in [|1,2,4,4,3,4,9,7,9,4,8,0|],
      b in [|8,2,0,5,1,7,9,0,9,9,9,4|],
      c in [|0,7,0,4,8,3,4,9,4,3,3,2|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|8,7,4,5,8,7,9,9,9,9,9,4|]\n";


val parcomp =
 [| listmax(a::b::c::nil) |
      a in [|8,7,2,8,5,0,9,7,4,1,0,3,2|],
      b in [|0,6,3,9,3,5,5,3,5,7,4,7,8|],
      c in [|6,6,5,0,0,5,6,2,7,1,3,6,5|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|8,7,5,9,5,5,9,7,7,7,4,7,8|]\n";


val parcomp =
 [| listmax(a::b::c::d::nil) |
      a in [|2|],
      b in [|5|],
      c in [|5|],
      d in [|6|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|6|]\n";


val parcomp =
 [| listmax(a::b::c::d::nil) |
      a in [|4,8|],
      b in [|2,7|],
      c in [|3,5|],
      d in [|6,5|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|6,8|]\n";


val parcomp =
 [| listmax(a::b::c::d::nil) |
      a in [|8,0,0|],
      b in [|4,2,8|],
      c in [|9,0,6|],
      d in [|9,6,0|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|9,6,8|]\n";


val parcomp =
 [| listmax(a::b::c::d::nil) |
      a in [|2,6,7,8|],
      b in [|6,3,4,4|],
      c in [|5,0,6,6|],
      d in [|9,0,8,7|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|9,6,8,8|]\n";


val parcomp =
 [| listmax(a::b::c::d::nil) |
      a in [|2,5,6,6,4|],
      b in [|5,8,0,5,2|],
      c in [|9,2,6,2,0|],
      d in [|8,3,1,1,1|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|9,8,6,6,4|]\n";


val parcomp =
 [| listmax(a::b::c::d::nil) |
      a in [|4,4,1,4,1,5|],
      b in [|5,4,4,5,7,1|],
      c in [|3,4,3,6,7,7|],
      d in [|9,7,7,8,4,5|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|9,7,7,8,7,7|]\n";


val parcomp =
 [| listmax(a::b::c::d::nil) |
      a in [|1,6,3,7,0,8,8|],
      b in [|1,7,0,2,9,3,2|],
      c in [|7,8,7,8,7,7,0|],
      d in [|8,5,2,9,1,0,4|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|8,8,7,9,9,8,8|]\n";


val parcomp =
 [| listmax(a::b::c::d::nil) |
      a in [|8,3,1,5,8,2,8,7|],
      b in [|1,5,9,4,7,1,3,6|],
      c in [|7,3,1,4,2,5,7,4|],
      d in [|3,0,6,1,8,9,4,5|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|8,5,9,5,8,9,8,7|]\n";


val parcomp =
 [| listmax(a::b::c::d::nil) |
      a in [|3,8,0,1,6,0,1,3,4|],
      b in [|9,6,6,0,5,1,4,3,1|],
      c in [|7,0,0,4,2,1,9,9,5|],
      d in [|4,1,9,1,2,7,7,9,5|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|9,8,9,4,6,7,9,9,5|]\n";


val parcomp =
 [| listmax(a::b::c::d::nil) |
      a in [|8,0,8,1,5,5,0,6,8,3|],
      b in [|5,9,7,0,8,8,3,3,9,5|],
      c in [|3,1,7,8,2,4,9,9,4,5|],
      d in [|8,6,6,3,6,6,4,7,0,6|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|8,9,8,8,8,8,9,9,9,6|]\n";


val parcomp =
 [| listmax(a::b::c::d::nil) |
      a in [|9,8,9,0,8,3,0,6,0,3,5|],
      b in [|1,4,9,1,8,6,1,6,6,2,2|],
      c in [|5,2,9,0,8,1,3,1,3,7,8|],
      d in [|0,3,4,6,0,5,8,0,6,5,6|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|9,8,9,6,8,6,8,6,6,7,8|]\n";


val parcomp =
 [| listmax(a::b::c::d::nil) |
      a in [|3,5,0,9,7,0,6,6,4,6,9,9|],
      b in [|6,7,0,1,0,0,1,8,3,5,3,7|],
      c in [|0,1,2,0,3,2,1,4,8,8,6,9|],
      d in [|6,2,8,3,1,7,3,1,3,0,0,1|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|6,7,8,9,7,7,6,8,8,8,9,9|]\n";


val parcomp =
 [| listmax(a::b::c::d::nil) |
      a in [|6,0,0,2,9,9,4,3,9,3,1,1,6|],
      b in [|9,5,3,3,6,9,6,9,3,6,4,4,5|],
      c in [|5,3,3,9,4,0,0,9,4,3,9,2,5|],
      d in [|2,4,3,1,0,4,2,9,1,1,6,5,2|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|9,5,3,9,9,9,6,9,9,6,9,5,6|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::nil) |
      a in [|8|],
      b in [|1|],
      c in [|2|],
      d in [|8|],
      e in [|7|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|8|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::nil) |
      a in [|8,1|],
      b in [|3,1|],
      c in [|8,5|],
      d in [|2,3|],
      e in [|1,0|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|8,5|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::nil) |
      a in [|4,9,9|],
      b in [|5,9,2|],
      c in [|2,2,8|],
      d in [|0,6,0|],
      e in [|3,8,2|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|5,9,9|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::nil) |
      a in [|9,5,9,0|],
      b in [|8,2,6,4|],
      c in [|2,8,8,4|],
      d in [|1,7,2,0|],
      e in [|9,5,2,8|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|9,8,9,8|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::nil) |
      a in [|1,9,5,4,1|],
      b in [|9,1,9,5,1|],
      c in [|1,3,3,4,2|],
      d in [|4,2,2,3,8|],
      e in [|5,7,8,8,1|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|9,9,9,8,8|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::nil) |
      a in [|0,1,8,2,4,7|],
      b in [|1,1,5,5,9,4|],
      c in [|6,3,5,6,6,1|],
      d in [|2,0,9,0,5,8|],
      e in [|5,6,4,3,8,0|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|6,6,9,6,9,8|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::nil) |
      a in [|5,3,8,8,7,7,6|],
      b in [|5,1,4,9,0,7,8|],
      c in [|9,4,7,7,9,3,7|],
      d in [|6,4,0,7,5,9,3|],
      e in [|4,7,0,3,6,2,2|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|9,7,8,9,9,9,8|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::nil) |
      a in [|1,6,8,9,8,7,4,3|],
      b in [|8,3,4,5,1,6,2,5|],
      c in [|8,8,6,9,6,4,2,5|],
      d in [|5,5,2,0,2,1,5,1|],
      e in [|0,4,3,3,6,2,3,6|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|8,8,8,9,8,7,5,6|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::nil) |
      a in [|8,8,6,6,8,4,8,2,1|],
      b in [|4,0,2,5,4,2,7,8,8|],
      c in [|8,2,3,1,5,6,4,5,9|],
      d in [|9,3,4,6,5,3,6,0,9|],
      e in [|5,2,7,3,8,1,4,1,8|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|9,8,7,6,8,6,8,8,9|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::nil) |
      a in [|9,1,8,3,1,1,2,8,0,5|],
      b in [|9,3,6,4,6,3,9,6,4,1|],
      c in [|3,8,3,2,1,0,5,0,2,9|],
      d in [|3,7,9,7,1,9,7,0,2,1|],
      e in [|7,5,5,7,3,9,9,2,3,7|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|9,8,9,7,6,9,9,8,4,9|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::nil) |
      a in [|8,7,5,4,3,8,7,2,0,8,1|],
      b in [|2,7,5,9,7,5,8,5,9,7,0|],
      c in [|1,7,7,2,4,3,7,8,9,6,1|],
      d in [|4,9,4,9,3,3,1,7,4,1,4|],
      e in [|8,6,9,3,7,2,3,4,8,5,4|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|8,9,9,9,7,8,8,8,9,8,4|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::nil) |
      a in [|1,1,7,7,4,5,6,3,7,7,5,5|],
      b in [|7,6,7,7,5,3,3,2,4,7,7,2|],
      c in [|8,2,7,7,6,7,0,9,2,4,4,5|],
      d in [|4,8,6,3,1,4,2,7,1,5,4,7|],
      e in [|4,1,8,1,3,4,0,4,2,4,2,6|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|8,8,8,7,6,7,6,9,7,7,7,7|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::nil) |
      a in [|5,9,9,1,4,5,3,9,3,7,6,8,3|],
      b in [|9,9,1,5,8,7,6,0,7,8,0,4,3|],
      c in [|1,3,7,7,2,3,2,3,4,8,2,3,9|],
      d in [|8,3,7,3,1,7,8,3,6,8,3,7,8|],
      e in [|4,7,8,8,5,4,7,6,8,5,2,6,2|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|9,9,9,8,8,7,8,9,8,8,6,8,9|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::nil) |
      a in [|6|],
      b in [|7|],
      c in [|6|],
      d in [|2|],
      e in [|0|],
      f in [|8|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|8|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::nil) |
      a in [|2,2|],
      b in [|6,2|],
      c in [|8,0|],
      d in [|2,6|],
      e in [|7,4|],
      f in [|8,0|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|8,6|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::nil) |
      a in [|7,1,0|],
      b in [|7,5,2|],
      c in [|7,0,7|],
      d in [|4,0,4|],
      e in [|3,5,7|],
      f in [|1,8,1|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|7,8,7|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::nil) |
      a in [|9,4,6,1|],
      b in [|8,3,0,5|],
      c in [|0,4,3,6|],
      d in [|4,3,0,5|],
      e in [|5,6,3,8|],
      f in [|3,4,3,0|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|9,6,6,8|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::nil) |
      a in [|8,7,3,9,7|],
      b in [|5,5,5,9,0|],
      c in [|7,5,5,6,4|],
      d in [|5,8,7,3,0|],
      e in [|8,5,0,5,4|],
      f in [|8,0,3,5,3|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|8,8,7,9,7|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::nil) |
      a in [|2,3,3,5,0,7|],
      b in [|6,9,5,8,1,7|],
      c in [|6,1,0,3,5,1|],
      d in [|7,2,1,9,1,8|],
      e in [|6,7,3,3,1,6|],
      f in [|5,0,4,2,4,3|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|7,9,5,9,5,8|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::nil) |
      a in [|1,3,0,3,2,5,2|],
      b in [|6,1,1,8,8,1,6|],
      c in [|7,3,2,8,7,6,7|],
      d in [|9,2,3,0,9,4,6|],
      e in [|8,1,9,1,5,4,2|],
      f in [|3,9,7,6,2,8,9|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|9,9,9,8,9,8,9|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::nil) |
      a in [|7,1,7,1,7,9,1,7|],
      b in [|1,0,3,7,9,1,3,5|],
      c in [|8,6,0,1,7,3,2,0|],
      d in [|1,4,6,3,8,7,8,5|],
      e in [|8,8,5,4,6,6,4,1|],
      f in [|1,7,2,2,1,4,8,5|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|8,8,7,7,9,9,8,7|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::nil) |
      a in [|9,1,1,6,9,6,6,1,8|],
      b in [|8,6,0,0,4,0,9,1,1|],
      c in [|9,9,8,2,8,1,8,8,4|],
      d in [|4,0,9,4,3,2,7,9,9|],
      e in [|3,6,8,2,8,0,7,3,4|],
      f in [|9,2,9,6,4,0,3,0,0|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|9,9,9,6,9,6,9,9,9|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::nil) |
      a in [|3,8,0,9,8,9,9,4,8,5|],
      b in [|3,2,6,6,6,1,4,5,6,1|],
      c in [|9,0,0,3,8,8,6,0,2,1|],
      d in [|4,9,8,8,5,4,6,3,5,9|],
      e in [|8,0,0,5,4,5,4,2,5,8|],
      f in [|9,8,8,7,3,2,1,3,1,8|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|9,9,8,9,8,9,9,5,8,9|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::nil) |
      a in [|5,8,1,5,3,7,5,3,7,0,2|],
      b in [|6,4,4,9,3,5,6,1,1,5,5|],
      c in [|5,0,9,4,1,4,8,4,2,7,7|],
      d in [|3,5,9,8,6,7,0,1,2,9,7|],
      e in [|1,0,3,5,1,1,1,0,6,5,9|],
      f in [|0,9,0,6,8,1,2,7,6,4,7|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|6,9,9,9,8,7,8,7,7,9,9|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::nil) |
      a in [|9,6,3,6,3,4,8,3,4,1,8,4|],
      b in [|4,8,9,9,1,9,9,4,4,1,4,9|],
      c in [|3,5,4,3,6,7,3,3,6,5,8,3|],
      d in [|3,0,4,4,5,8,2,6,0,4,0,5|],
      e in [|0,2,5,5,5,7,6,6,9,5,6,3|],
      f in [|8,0,0,7,1,5,8,9,1,1,1,1|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|9,8,9,9,6,9,9,9,9,5,8,9|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::nil) |
      a in [|7,3,1,9,4,1,8,8,5,1,0,3,3|],
      b in [|2,8,5,6,9,2,1,7,7,4,8,6,1|],
      c in [|8,4,5,7,9,1,6,8,3,5,6,5,4|],
      d in [|4,5,8,5,3,0,8,7,9,6,0,0,5|],
      e in [|4,5,7,1,0,7,3,9,7,0,1,2,3|],
      f in [|8,0,6,1,5,8,9,1,0,7,0,9,8|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|8,8,8,9,9,8,9,9,9,7,8,9,8|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::g::nil) |
      a in [|9|],
      b in [|9|],
      c in [|1|],
      d in [|8|],
      e in [|6|],
      f in [|9|],
      g in [|0|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|9|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::g::nil) |
      a in [|3,5|],
      b in [|8,4|],
      c in [|1,7|],
      d in [|1,9|],
      e in [|9,3|],
      f in [|9,8|],
      g in [|6,3|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|9,9|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::g::nil) |
      a in [|7,2,8|],
      b in [|8,6,6|],
      c in [|6,1,3|],
      d in [|5,7,6|],
      e in [|7,1,3|],
      f in [|9,4,4|],
      g in [|8,7,9|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|9,7,9|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::g::nil) |
      a in [|1,6,3,3|],
      b in [|4,9,8,4|],
      c in [|6,0,2,9|],
      d in [|7,0,7,3|],
      e in [|6,2,6,5|],
      f in [|8,5,6,0|],
      g in [|9,2,2,6|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|9,9,8,9|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::g::nil) |
      a in [|3,4,9,0,9|],
      b in [|0,5,1,9,1|],
      c in [|6,5,0,3,0|],
      d in [|1,0,0,0,8|],
      e in [|4,6,5,4,9|],
      f in [|6,3,2,4,8|],
      g in [|3,7,5,8,2|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|6,7,9,9,9|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::g::nil) |
      a in [|5,0,3,9,7,5|],
      b in [|3,6,0,8,6,2|],
      c in [|8,4,6,5,7,7|],
      d in [|9,8,0,9,1,8|],
      e in [|9,9,1,6,4,0|],
      f in [|0,9,2,3,6,7|],
      g in [|2,2,9,1,0,4|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|9,9,9,9,7,8|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::g::nil) |
      a in [|4,1,2,9,8,2,1|],
      b in [|0,7,1,5,6,7,4|],
      c in [|7,1,5,2,3,2,8|],
      d in [|8,9,6,9,3,5,6|],
      e in [|3,8,1,3,4,5,6|],
      f in [|4,3,8,2,9,8,6|],
      g in [|7,4,2,3,6,2,0|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|8,9,8,9,9,8,8|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::g::nil) |
      a in [|9,1,1,4,1,0,0,4|],
      b in [|3,7,3,6,4,1,4,3|],
      c in [|8,5,6,1,0,4,0,4|],
      d in [|6,8,8,4,0,5,8,8|],
      e in [|8,2,1,5,1,0,0,3|],
      f in [|6,2,3,1,0,3,2,2|],
      g in [|9,7,1,6,9,3,2,1|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|9,8,8,6,9,5,8,8|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::g::nil) |
      a in [|1,3,0,6,1,5,8,9,0|],
      b in [|4,9,4,3,8,7,7,5,8|],
      c in [|5,1,9,1,0,1,3,4,0|],
      d in [|0,2,3,4,0,9,3,8,0|],
      e in [|8,2,6,5,2,5,4,5,1|],
      f in [|9,8,7,9,9,0,4,2,4|],
      g in [|7,3,1,4,6,2,5,5,0|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|9,9,9,9,9,9,8,9,8|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::g::nil) |
      a in [|6,0,0,2,8,7,3,7,6,6|],
      b in [|9,0,4,3,6,1,8,1,8,4|],
      c in [|9,5,9,3,5,7,7,6,8,0|],
      d in [|7,4,5,7,2,6,6,0,4,9|],
      e in [|0,2,6,0,9,1,1,5,0,8|],
      f in [|7,9,3,4,6,5,0,7,2,7|],
      g in [|5,0,1,9,3,2,2,5,0,9|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|9,9,9,9,9,7,8,7,8,9|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::g::nil) |
      a in [|5,4,8,0,6,5,7,3,0,3,8|],
      b in [|9,3,4,3,1,5,7,9,6,3,8|],
      c in [|4,7,9,5,2,9,1,9,8,3,1|],
      d in [|5,8,4,8,3,7,0,0,0,9,0|],
      e in [|7,7,7,5,3,7,9,7,2,1,5|],
      f in [|6,9,6,0,6,7,6,7,6,1,4|],
      g in [|5,4,5,9,7,4,7,6,7,4,8|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|9,9,9,9,7,9,9,9,8,9,8|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::g::nil) |
      a in [|7,0,3,7,4,0,1,8,4,0,6,1|],
      b in [|4,4,0,9,1,4,7,4,4,3,6,8|],
      c in [|8,9,0,3,8,7,5,5,5,0,5,9|],
      d in [|4,2,2,8,0,7,0,0,4,8,3,0|],
      e in [|6,3,6,1,5,7,9,6,8,1,4,3|],
      f in [|0,2,8,1,9,3,9,7,5,0,8,3|],
      g in [|4,0,9,3,0,8,7,2,1,7,4,5|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|8,9,9,9,9,8,9,8,8,8,8,9|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::g::nil) |
      a in [|1,2,7,1,7,2,4,8,8,8,2,1,3|],
      b in [|8,3,1,1,3,0,9,4,9,5,0,6,6|],
      c in [|3,7,4,6,7,0,1,3,5,3,2,3,4|],
      d in [|4,3,0,6,5,8,9,7,4,2,4,3,4|],
      e in [|6,9,0,3,8,5,4,5,9,3,7,7,2|],
      f in [|3,4,1,3,0,6,6,9,9,9,7,4,5|],
      g in [|3,3,0,7,0,2,3,9,4,0,9,7,0|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|8,9,7,7,8,8,9,9,9,9,9,7,6|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::g::h::nil) |
      a in [|0|],
      b in [|6|],
      c in [|0|],
      d in [|7|],
      e in [|5|],
      f in [|0|],
      g in [|8|],
      h in [|3|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|8|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::g::h::nil) |
      a in [|2,8|],
      b in [|4,9|],
      c in [|9,6|],
      d in [|0,8|],
      e in [|9,4|],
      f in [|5,9|],
      g in [|2,1|],
      h in [|6,5|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|9,9|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::g::h::nil) |
      a in [|3,7,3|],
      b in [|2,6,8|],
      c in [|3,7,0|],
      d in [|7,0,8|],
      e in [|2,6,6|],
      f in [|7,2,1|],
      g in [|2,9,4|],
      h in [|5,5,2|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|7,9,8|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::g::h::nil) |
      a in [|8,7,6,8|],
      b in [|9,6,7,9|],
      c in [|4,5,7,6|],
      d in [|1,1,8,5|],
      e in [|1,8,5,3|],
      f in [|5,9,4,4|],
      g in [|9,0,3,7|],
      h in [|3,9,7,4|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|9,9,8,9|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::g::h::nil) |
      a in [|7,3,1,6,3|],
      b in [|8,7,2,6,8|],
      c in [|2,5,8,6,5|],
      d in [|1,8,5,6,5|],
      e in [|5,2,7,0,2|],
      f in [|4,4,4,4,8|],
      g in [|7,5,0,0,7|],
      h in [|1,5,7,8,6|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|8,8,8,8,8|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::g::h::nil) |
      a in [|9,9,4,0,5,3|],
      b in [|2,7,3,1,1,7|],
      c in [|6,9,1,5,5,4|],
      d in [|4,4,9,9,9,2|],
      e in [|5,2,6,6,6,0|],
      f in [|6,3,3,5,0,5|],
      g in [|8,6,6,5,3,3|],
      h in [|3,3,7,4,9,8|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|9,9,9,9,9,8|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::g::h::nil) |
      a in [|4,1,5,3,0,7,1|],
      b in [|0,1,1,2,4,9,3|],
      c in [|8,7,1,8,7,0,4|],
      d in [|3,3,5,6,1,2,7|],
      e in [|1,5,6,7,7,5,3|],
      f in [|4,2,7,2,8,9,1|],
      g in [|5,2,0,3,5,9,3|],
      h in [|3,8,9,2,0,9,4|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|8,8,9,8,8,9,7|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::g::h::nil) |
      a in [|0,7,8,5,5,4,6,0|],
      b in [|1,6,3,8,8,8,1,7|],
      c in [|8,6,0,5,9,2,8,8|],
      d in [|9,2,8,6,9,1,3,2|],
      e in [|6,9,6,3,5,3,5,5|],
      f in [|4,9,9,9,4,6,0,0|],
      g in [|7,9,4,4,9,3,7,3|],
      h in [|5,5,0,4,9,2,6,8|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|9,9,9,9,9,8,8,8|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::g::h::nil) |
      a in [|7,8,0,1,3,7,5,5,5|],
      b in [|2,8,9,4,4,6,6,4,3|],
      c in [|8,0,8,9,5,7,5,5,7|],
      d in [|0,4,2,1,1,9,7,6,2|],
      e in [|9,4,1,9,9,4,4,2,4|],
      f in [|5,1,8,2,0,8,5,0,4|],
      g in [|5,5,6,6,4,3,6,6,9|],
      h in [|5,0,1,5,0,7,2,1,0|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|9,8,9,9,9,9,7,6,9|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::g::h::nil) |
      a in [|0,3,7,2,2,8,1,2,4,0|],
      b in [|3,0,3,7,8,2,0,6,5,8|],
      c in [|0,2,3,8,5,4,8,1,6,2|],
      d in [|1,2,7,0,2,9,1,2,6,9|],
      e in [|0,0,9,0,1,4,6,8,2,3|],
      f in [|2,4,6,9,8,8,8,3,7,8|],
      g in [|4,5,8,7,5,1,6,7,7,8|],
      h in [|3,2,7,5,8,0,7,5,0,9|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|4,5,9,9,8,9,8,8,7,9|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::g::h::nil) |
      a in [|2,7,3,8,9,3,1,5,6,8,2|],
      b in [|4,0,7,1,1,6,8,4,5,1,9|],
      c in [|4,3,2,5,9,4,3,5,0,5,9|],
      d in [|4,9,6,5,8,3,8,0,0,9,4|],
      e in [|0,9,2,4,4,9,7,7,9,4,6|],
      f in [|7,5,1,5,7,7,8,6,7,0,0|],
      g in [|8,4,1,3,1,3,4,2,6,9,4|],
      h in [|0,3,4,0,0,6,8,4,2,1,7|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|8,9,7,8,9,9,8,7,9,9,9|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::g::h::nil) |
      a in [|5,3,5,4,0,3,1,7,1,0,0,9|],
      b in [|6,5,1,3,4,7,8,1,9,2,0,5|],
      c in [|6,3,3,1,0,1,4,2,6,3,9,3|],
      d in [|5,8,0,7,7,5,4,9,8,3,0,9|],
      e in [|0,7,8,3,9,7,7,1,2,9,9,7|],
      f in [|2,7,1,5,4,3,9,2,3,3,7,8|],
      g in [|0,4,6,6,0,5,3,0,9,9,5,3|],
      h in [|2,0,1,0,1,9,8,1,5,5,4,2|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|6,8,8,7,9,9,9,9,9,9,9,9|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::g::h::nil) |
      a in [|4,7,8,6,5,5,1,3,9,3,9,0,3|],
      b in [|4,8,3,0,4,8,7,5,6,7,0,2,9|],
      c in [|6,7,9,6,6,3,0,1,7,7,7,5,1|],
      d in [|8,9,3,0,8,2,4,8,6,6,4,8,5|],
      e in [|3,8,8,4,7,3,0,3,2,1,8,0,9|],
      f in [|4,2,0,2,4,6,6,1,6,9,3,2,3|],
      g in [|2,7,1,8,6,1,8,5,0,6,1,8,7|],
      h in [|8,8,7,7,0,2,9,3,3,8,1,4,3|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|8,9,9,8,8,8,9,8,9,9,9,8,9|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::g::h::i::nil) |
      a in [|1|],
      b in [|3|],
      c in [|0|],
      d in [|8|],
      e in [|6|],
      f in [|4|],
      g in [|0|],
      h in [|7|],
      i in [|5|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|8|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::g::h::i::nil) |
      a in [|7,1|],
      b in [|7,4|],
      c in [|5,9|],
      d in [|0,6|],
      e in [|9,2|],
      f in [|4,3|],
      g in [|9,1|],
      h in [|8,6|],
      i in [|6,3|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|9,9|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::g::h::i::nil) |
      a in [|0,7,8|],
      b in [|7,6,6|],
      c in [|2,9,1|],
      d in [|3,0,9|],
      e in [|7,3,8|],
      f in [|2,4,8|],
      g in [|3,7,8|],
      h in [|3,8,2|],
      i in [|1,3,3|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|7,9,9|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::g::h::i::nil) |
      a in [|4,3,5,1|],
      b in [|1,2,3,5|],
      c in [|6,1,9,4|],
      d in [|9,0,8,6|],
      e in [|6,3,7,3|],
      f in [|3,9,7,6|],
      g in [|7,1,1,3|],
      h in [|1,0,4,3|],
      i in [|8,6,9,9|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|9,9,9,9|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::g::h::i::nil) |
      a in [|3,9,4,7,0|],
      b in [|4,1,6,4,8|],
      c in [|0,2,0,0,1|],
      d in [|4,3,0,8,5|],
      e in [|8,8,6,1,1|],
      f in [|1,8,9,6,7|],
      g in [|6,4,9,3,1|],
      h in [|7,9,9,9,0|],
      i in [|1,2,6,6,8|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|8,9,9,9,8|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::g::h::i::nil) |
      a in [|5,3,8,3,3,6|],
      b in [|7,1,1,5,2,5|],
      c in [|8,9,1,0,0,9|],
      d in [|7,5,9,9,9,4|],
      e in [|4,3,6,6,8,9|],
      f in [|7,2,2,8,8,1|],
      g in [|2,1,5,8,9,7|],
      h in [|3,5,7,5,4,6|],
      i in [|0,8,4,8,7,0|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|8,9,9,9,9,9|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::g::h::i::nil) |
      a in [|2,1,6,8,4,3,6|],
      b in [|0,3,6,6,1,8,7|],
      c in [|5,2,0,6,9,2,8|],
      d in [|0,9,5,1,3,9,4|],
      e in [|7,2,8,6,1,1,5|],
      f in [|4,7,1,2,5,1,3|],
      g in [|1,6,2,3,2,0,1|],
      h in [|3,9,7,2,9,5,4|],
      i in [|4,3,3,9,7,6,7|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|7,9,8,9,9,9,8|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::g::h::i::nil) |
      a in [|2,0,2,9,1,1,0,0|],
      b in [|5,4,2,5,6,2,1,8|],
      c in [|9,5,8,5,0,1,9,7|],
      d in [|7,3,2,1,1,5,6,0|],
      e in [|1,1,9,8,4,3,5,4|],
      f in [|3,0,2,5,7,0,0,4|],
      g in [|3,1,2,4,5,5,3,2|],
      h in [|2,6,8,5,0,1,4,8|],
      i in [|7,8,3,4,0,8,9,6|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|9,8,9,9,7,8,9,8|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::g::h::i::nil) |
      a in [|1,0,5,9,1,6,4,6,5|],
      b in [|1,5,3,3,5,8,8,3,2|],
      c in [|8,4,1,7,2,4,7,9,5|],
      d in [|4,6,4,9,2,5,8,1,8|],
      e in [|7,1,0,5,3,2,2,6,2|],
      f in [|3,3,7,9,6,3,2,4,0|],
      g in [|3,3,7,8,0,8,1,3,5|],
      h in [|3,7,1,0,5,5,9,4,6|],
      i in [|0,1,8,1,1,8,7,0,0|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|8,7,8,9,6,8,9,9,8|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::g::h::i::nil) |
      a in [|5,3,5,5,8,7,7,2,1,7|],
      b in [|3,9,4,0,1,0,0,1,5,9|],
      c in [|8,3,0,1,0,2,8,6,7,9|],
      d in [|6,7,9,2,8,1,6,3,8,7|],
      e in [|9,5,8,9,1,4,9,9,9,1|],
      f in [|7,2,5,8,2,1,7,4,8,9|],
      g in [|1,2,5,9,3,4,5,2,3,2|],
      h in [|8,9,3,0,0,7,1,8,5,1|],
      i in [|1,6,2,7,4,9,9,0,4,8|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|9,9,9,9,8,9,9,9,9,9|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::g::h::i::nil) |
      a in [|4,9,8,4,7,0,3,8,0,1,4|],
      b in [|0,3,9,8,0,7,7,4,9,4,6|],
      c in [|3,6,7,1,9,7,7,0,5,8,1|],
      d in [|0,4,4,7,0,7,6,0,3,7,1|],
      e in [|6,1,9,6,3,4,4,6,7,7,9|],
      f in [|0,0,8,3,5,7,8,2,5,9,3|],
      g in [|2,9,9,1,6,5,4,6,2,2,3|],
      h in [|6,2,8,8,3,7,5,2,2,0,4|],
      i in [|8,7,8,2,6,9,6,0,3,8,8|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|8,9,9,8,9,9,8,8,9,9,9|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::g::h::i::nil) |
      a in [|8,9,7,2,6,7,1,7,7,0,9,0|],
      b in [|3,7,8,4,7,8,6,6,3,2,6,3|],
      c in [|8,0,6,2,5,1,6,5,8,9,2,7|],
      d in [|2,9,3,7,7,2,9,0,3,9,5,4|],
      e in [|2,5,8,9,2,9,7,7,0,3,2,7|],
      f in [|4,6,4,4,4,4,1,6,7,7,8,1|],
      g in [|6,6,5,0,9,9,4,4,4,5,7,2|],
      h in [|5,1,8,1,1,7,4,6,1,2,5,4|],
      i in [|1,4,3,4,6,8,0,0,3,8,6,6|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|8,9,8,9,9,9,9,7,8,9,9,7|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::g::h::i::nil) |
      a in [|2,5,7,6,9,6,6,3,3,8,9,2,2|],
      b in [|2,4,6,6,0,3,6,1,6,3,6,1,1|],
      c in [|7,4,9,2,9,0,4,5,1,0,1,2,5|],
      d in [|5,6,5,1,2,6,0,1,0,0,2,1,2|],
      e in [|1,9,6,4,9,7,6,8,4,9,1,0,6|],
      f in [|6,6,3,1,7,6,6,6,1,3,6,5,3|],
      g in [|8,9,5,4,1,0,6,8,9,3,0,6,4|],
      h in [|8,0,8,2,2,1,4,0,2,8,3,9,9|],
      i in [|9,4,7,3,9,5,4,5,1,7,2,2,3|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|9,9,9,6,9,7,6,8,9,9,9,9,9|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::g::h::i::j::nil) |
      a in [|6|],
      b in [|0|],
      c in [|4|],
      d in [|0|],
      e in [|2|],
      f in [|3|],
      g in [|3|],
      h in [|7|],
      i in [|0|],
      j in [|4|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|7|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::g::h::i::j::nil) |
      a in [|0,8|],
      b in [|0,5|],
      c in [|3,0|],
      d in [|0,8|],
      e in [|3,1|],
      f in [|0,6|],
      g in [|8,5|],
      h in [|0,0|],
      i in [|0,1|],
      j in [|9,1|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|9,8|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::g::h::i::j::nil) |
      a in [|8,4,7|],
      b in [|7,1,2|],
      c in [|6,2,7|],
      d in [|1,3,2|],
      e in [|6,4,0|],
      f in [|6,3,5|],
      g in [|8,7,0|],
      h in [|1,8,0|],
      i in [|1,7,0|],
      j in [|6,0,9|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|8,8,9|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::g::h::i::j::nil) |
      a in [|3,8,8,1|],
      b in [|4,0,0,6|],
      c in [|3,4,2,2|],
      d in [|8,7,6,8|],
      e in [|0,4,5,5|],
      f in [|9,7,9,2|],
      g in [|8,8,3,6|],
      h in [|4,5,1,0|],
      i in [|1,3,0,6|],
      j in [|1,0,5,3|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|9,8,9,8|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::g::h::i::j::nil) |
      a in [|2,8,5,8,7|],
      b in [|8,4,0,3,0|],
      c in [|5,1,0,2,9|],
      d in [|2,7,1,4,3|],
      e in [|4,5,4,8,4|],
      f in [|5,4,2,3,2|],
      g in [|3,1,9,1,4|],
      h in [|0,9,8,5,3|],
      i in [|1,1,6,1,2|],
      j in [|6,0,7,0,4|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|8,9,9,8,9|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::g::h::i::j::nil) |
      a in [|5,6,7,7,2,5|],
      b in [|9,9,0,1,9,5|],
      c in [|0,0,9,2,7,5|],
      d in [|6,0,7,8,1,9|],
      e in [|0,6,1,4,1,2|],
      f in [|8,9,4,8,7,1|],
      g in [|4,8,1,9,8,5|],
      h in [|9,4,7,6,2,3|],
      i in [|1,5,6,7,9,9|],
      j in [|0,5,1,9,4,6|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|9,9,9,9,9,9|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::g::h::i::j::nil) |
      a in [|9,0,0,3,7,5,0|],
      b in [|5,1,1,0,8,0,4|],
      c in [|9,6,9,0,2,0,5|],
      d in [|4,3,9,3,5,7,7|],
      e in [|2,9,2,4,4,6,9|],
      f in [|5,4,2,2,6,5,1|],
      g in [|3,0,1,8,4,0,1|],
      h in [|3,1,0,6,2,2,8|],
      i in [|5,1,1,8,6,0,7|],
      j in [|6,8,1,1,0,3,9|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|9,9,9,8,8,7,9|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::g::h::i::j::nil) |
      a in [|6,2,6,5,3,3,7,4|],
      b in [|0,9,4,7,4,9,9,4|],
      c in [|8,5,7,6,2,2,5,2|],
      d in [|4,5,3,2,9,3,1,4|],
      e in [|1,2,1,2,4,4,2,1|],
      f in [|9,9,3,6,1,3,1,8|],
      g in [|8,6,9,4,8,9,9,6|],
      h in [|7,7,5,9,5,8,3,4|],
      i in [|2,6,6,4,3,6,9,3|],
      j in [|1,5,1,8,7,6,6,9|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|9,9,9,9,9,9,9,9|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::g::h::i::j::nil) |
      a in [|0,0,7,9,0,0,3,8,4|],
      b in [|9,4,5,2,4,6,2,5,2|],
      c in [|4,6,7,8,4,4,5,1,0|],
      d in [|4,5,0,9,7,5,6,9,7|],
      e in [|2,3,6,1,5,0,4,5,9|],
      f in [|4,7,6,0,1,0,8,9,1|],
      g in [|0,3,8,3,3,8,5,3,7|],
      h in [|8,4,4,1,7,5,6,3,6|],
      i in [|1,4,7,1,1,0,0,3,5|],
      j in [|3,5,3,2,4,5,6,4,3|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|9,7,8,9,7,8,8,9,9|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::g::h::i::j::nil) |
      a in [|1,0,9,9,2,8,8,4,4,5|],
      b in [|3,3,5,6,2,8,4,1,5,1|],
      c in [|1,0,1,8,1,9,7,1,2,7|],
      d in [|1,0,4,8,8,3,0,0,7,1|],
      e in [|5,7,1,5,5,6,1,0,4,0|],
      f in [|5,9,1,4,1,2,9,1,4,4|],
      g in [|0,9,1,3,1,5,8,5,6,5|],
      h in [|0,6,0,6,1,8,2,9,7,4|],
      i in [|4,4,5,0,7,1,6,1,0,2|],
      j in [|0,6,4,5,6,0,5,8,8,8|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|5,9,9,9,8,9,9,9,8,8|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::g::h::i::j::nil) |
      a in [|1,0,1,9,4,2,9,3,2,0,3|],
      b in [|9,4,1,6,7,5,8,2,2,1,3|],
      c in [|1,3,4,5,5,0,7,6,2,2,0|],
      d in [|7,8,1,3,9,5,8,1,5,6,7|],
      e in [|6,5,1,4,6,1,4,2,1,0,1|],
      f in [|1,9,5,9,2,0,9,1,0,0,6|],
      g in [|4,9,1,5,4,1,4,1,8,6,0|],
      h in [|2,5,5,9,9,6,0,6,0,6,3|],
      i in [|8,8,1,7,3,1,7,3,3,4,1|],
      j in [|1,5,1,0,6,8,1,5,4,0,6|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|9,9,5,9,9,8,9,6,8,6,7|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::g::h::i::j::nil) |
      a in [|4,4,8,3,5,6,0,1,4,1,1,4|],
      b in [|2,1,0,1,5,3,6,8,9,1,3,5|],
      c in [|8,6,9,4,0,2,0,8,1,1,8,8|],
      d in [|3,7,9,6,1,0,5,3,2,9,3,8|],
      e in [|6,3,5,6,1,7,3,5,6,5,7,0|],
      f in [|4,8,8,2,5,7,6,3,3,6,2,6|],
      g in [|0,5,9,7,4,2,5,3,9,0,5,5|],
      h in [|4,8,0,7,5,4,0,9,1,8,0,1|],
      i in [|6,6,7,0,8,7,2,9,5,6,4,0|],
      j in [|3,4,7,5,2,4,7,5,8,2,5,8|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|8,8,9,7,8,7,7,9,9,9,8,8|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::g::h::i::j::nil) |
      a in [|3,0,1,5,0,4,5,5,4,5,8,6,7|],
      b in [|8,3,5,2,9,7,0,5,8,0,1,4,7|],
      c in [|0,7,1,5,8,9,9,4,2,3,1,5,9|],
      d in [|7,9,8,7,3,4,0,1,7,0,8,1,4|],
      e in [|3,2,3,0,3,8,6,4,7,2,0,9,9|],
      f in [|0,3,5,2,4,1,6,9,5,1,8,5,5|],
      g in [|6,4,0,9,6,8,2,3,7,2,9,3,1|],
      h in [|6,8,4,3,3,9,8,0,2,3,7,4,5|],
      i in [|5,0,7,9,3,0,1,8,7,7,6,4,9|],
      j in [|1,6,3,8,1,6,1,4,3,6,6,6,1|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|8,9,8,9,9,9,9,9,8,7,9,9,9|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::g::h::i::j::k::nil) |
      a in [|0|],
      b in [|3|],
      c in [|6|],
      d in [|6|],
      e in [|9|],
      f in [|2|],
      g in [|7|],
      h in [|7|],
      i in [|0|],
      j in [|2|],
      k in [|6|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|9|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::g::h::i::j::k::nil) |
      a in [|3,6|],
      b in [|9,1|],
      c in [|9,8|],
      d in [|9,3|],
      e in [|0,3|],
      f in [|5,5|],
      g in [|9,6|],
      h in [|4,0|],
      i in [|1,7|],
      j in [|4,2|],
      k in [|8,3|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|9,8|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::g::h::i::j::k::nil) |
      a in [|1,8,6|],
      b in [|9,3,7|],
      c in [|1,8,9|],
      d in [|6,8,7|],
      e in [|9,1,7|],
      f in [|4,2,4|],
      g in [|3,2,9|],
      h in [|2,2,8|],
      i in [|5,6,3|],
      j in [|4,3,5|],
      k in [|4,0,1|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|9,8,9|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::g::h::i::j::k::nil) |
      a in [|6,6,6,3|],
      b in [|3,9,4,4|],
      c in [|5,2,0,7|],
      d in [|2,3,5,2|],
      e in [|5,5,0,0|],
      f in [|8,8,3,6|],
      g in [|2,4,0,6|],
      h in [|6,8,6,4|],
      i in [|9,7,9,8|],
      j in [|8,8,5,3|],
      k in [|7,2,0,2|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|9,9,9,8|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::g::h::i::j::k::nil) |
      a in [|5,2,3,4,1|],
      b in [|9,9,6,2,6|],
      c in [|9,6,1,3,8|],
      d in [|4,2,8,9,8|],
      e in [|7,5,0,0,8|],
      f in [|8,9,3,0,1|],
      g in [|8,3,1,0,1|],
      h in [|8,7,6,0,8|],
      i in [|3,0,4,2,5|],
      j in [|6,4,6,3,9|],
      k in [|6,8,5,9,7|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|9,9,8,9,9|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::g::h::i::j::k::nil) |
      a in [|8,0,8,6,6,6|],
      b in [|4,9,4,5,3,9|],
      c in [|1,1,8,8,2,2|],
      d in [|1,1,0,9,7,9|],
      e in [|0,7,2,3,4,0|],
      f in [|7,6,2,8,2,4|],
      g in [|5,6,7,4,0,5|],
      h in [|7,3,6,4,9,0|],
      i in [|4,2,1,3,0,7|],
      j in [|2,2,9,7,8,9|],
      k in [|0,0,3,0,3,8|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|8,9,9,9,9,9|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::g::h::i::j::k::nil) |
      a in [|7,0,1,5,8,2,0|],
      b in [|2,5,9,4,8,7,6|],
      c in [|2,2,8,8,0,7,4|],
      d in [|0,0,4,6,7,7,9|],
      e in [|3,9,5,5,8,5,7|],
      f in [|2,0,6,4,4,9,7|],
      g in [|5,3,9,2,7,1,0|],
      h in [|7,5,5,1,7,4,3|],
      i in [|9,3,6,2,4,6,5|],
      j in [|6,7,7,2,3,8,4|],
      k in [|9,4,1,5,4,8,4|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|9,9,9,8,8,9,9|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::g::h::i::j::k::nil) |
      a in [|6,9,0,3,0,5,1,6|],
      b in [|4,8,2,0,4,4,1,4|],
      c in [|4,4,7,3,3,7,4,6|],
      d in [|2,5,9,9,2,5,4,9|],
      e in [|8,3,2,8,5,6,8,7|],
      f in [|1,5,3,3,2,8,2,2|],
      g in [|1,0,2,9,4,9,1,3|],
      h in [|0,2,7,4,6,3,2,5|],
      i in [|3,0,4,6,1,5,0,5|],
      j in [|4,7,3,6,4,3,0,5|],
      k in [|7,4,1,8,1,1,3,0|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|8,9,9,9,6,9,8,9|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::g::h::i::j::k::nil) |
      a in [|8,5,4,6,7,0,6,1,7|],
      b in [|5,1,8,3,6,7,2,7,1|],
      c in [|8,3,2,8,6,5,5,6,9|],
      d in [|0,4,7,9,1,1,9,6,2|],
      e in [|9,4,3,5,9,4,6,2,3|],
      f in [|3,4,2,8,6,3,0,8,5|],
      g in [|4,8,7,0,6,4,8,5,3|],
      h in [|0,5,8,0,9,5,4,0,3|],
      i in [|7,2,8,8,1,6,4,2,9|],
      j in [|5,9,3,6,7,4,9,5,3|],
      k in [|4,5,8,6,4,6,4,8,5|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|9,9,8,9,9,7,9,8,9|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::g::h::i::j::k::nil) |
      a in [|7,1,7,2,5,7,6,5,3,1|],
      b in [|5,9,4,3,4,2,1,8,7,5|],
      c in [|8,0,9,0,1,7,8,3,5,2|],
      d in [|3,0,8,5,4,8,7,8,7,6|],
      e in [|1,2,8,3,8,5,6,3,1,1|],
      f in [|2,7,6,6,4,3,8,2,5,8|],
      g in [|6,1,5,5,1,1,7,8,0,5|],
      h in [|3,9,5,8,5,1,3,5,6,4|],
      i in [|0,5,4,0,0,4,4,5,9,3|],
      j in [|3,8,1,7,7,6,3,0,4,4|],
      k in [|2,7,3,7,9,1,2,4,4,9|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|8,9,9,8,9,8,8,8,9,9|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::g::h::i::j::k::nil) |
      a in [|2,2,7,6,1,0,0,5,9,6,3|],
      b in [|5,3,8,8,6,3,0,4,8,0,6|],
      c in [|8,7,9,8,5,4,7,1,5,5,4|],
      d in [|9,5,1,9,6,0,6,1,7,2,8|],
      e in [|6,1,2,1,6,6,4,4,3,4,5|],
      f in [|5,2,6,3,0,6,6,0,1,5,6|],
      g in [|2,1,8,5,9,2,7,8,3,5,1|],
      h in [|5,2,0,8,5,0,8,1,1,4,7|],
      i in [|8,1,4,6,0,3,4,8,3,0,9|],
      j in [|6,0,9,5,0,5,1,9,1,8,1|],
      k in [|7,4,7,2,9,7,8,7,8,9,5|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|9,7,9,9,9,7,8,9,9,9,9|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::g::h::i::j::k::nil) |
      a in [|6,2,2,7,8,3,9,2,1,5,2,9|],
      b in [|9,7,2,5,7,0,2,5,2,7,4,7|],
      c in [|2,6,3,7,1,1,3,1,1,8,9,5|],
      d in [|9,8,4,2,9,8,9,2,0,0,7,6|],
      e in [|8,1,6,0,4,2,1,3,8,1,9,7|],
      f in [|8,6,2,3,2,1,4,0,5,6,3,6|],
      g in [|3,3,1,0,5,1,5,0,4,9,7,8|],
      h in [|0,9,9,5,8,1,6,8,7,1,7,6|],
      i in [|2,6,4,9,8,5,7,3,7,3,5,8|],
      j in [|2,5,1,3,3,2,5,3,0,0,5,7|],
      k in [|7,5,4,2,3,7,8,5,4,8,4,5|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|9,9,9,9,9,8,9,8,8,9,9,9|]\n";


val parcomp =
 [| listmax(a::b::c::d::e::f::g::h::i::j::k::nil) |
      a in [|9,4,2,6,1,0,3,4,3,6,6,8,7|],
      b in [|9,4,1,3,5,0,2,7,4,3,2,9,5|],
      c in [|2,3,0,2,8,8,2,7,4,0,2,3,7|],
      d in [|1,3,8,6,9,9,4,7,0,7,0,1,5|],
      e in [|8,1,7,9,2,4,2,6,0,6,7,4,0|],
      f in [|0,5,1,9,1,6,5,9,8,2,1,2,2|],
      g in [|2,2,8,7,7,0,3,8,9,8,9,9,6|],
      h in [|1,2,4,7,0,9,2,7,1,3,8,1,7|],
      i in [|9,6,2,2,2,1,1,6,0,9,6,4,0|],
      j in [|2,7,9,8,5,6,7,9,9,2,1,4,8|],
      k in [|1,7,0,6,6,3,9,9,6,5,3,2,3|] |]
;
val _ = printPar(parcomp);
val _ = print "expected [|9,7,9,9,9,9,9,9,9,9,9,9,8|]\n";

()
