(* There seems to be a problem with pattern matching. *)

datatype XO = X | O;

fun toString n =
  case n 
   of X => "X"
    | _ => "O";

(print (toString X);
 print "\n";
 print (toString O);
 print "\n")
