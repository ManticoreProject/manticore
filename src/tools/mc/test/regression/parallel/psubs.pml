val par = [| 10, 11, 12, 13, 14, 15, 16, 17, 18 |];

val x0 = par!0;
val x4 = par!4;
val x8 = par!8;

val list = x0 :: x4 :: x8 :: nil;

fun catw (sep, ss) =
  (case ss
     of nil => ""
      | s :: nil => s
      | s :: ss => s ^ sep ^ catw (sep, ss)
    (* end case *))
;

fun map (f, xs) = 
  (case xs
     of nil => nil
      | x :: xs => f x :: map (f, xs)
    (* end case *))
;

fun itosToString ns = "[" ^ (catw (",", (map (itos, ns)))) ^ "]";

val _ = print ("Elements 0, 4 and 8 of par are " 
       ^ itosToString list 
       ^ " (expected [10,14,18]).\n")



