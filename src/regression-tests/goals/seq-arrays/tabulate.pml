fun id (n : int) = n;

val a = Array.tabulate (10, id);

fun pr n = let
  val s = Int.toString (Array.sub (a, n))
  in
    Print.print (s ^ "\n")
  end;

val _ = Array.app pr a
