fun id (n : int) = n;

val a = Array64.tabulate (10, id);

fun pr n = let
  val s = Int.toString (Array64.sub (a, n))
  in
    Print.print (s ^ "\n")
  end;

val _ = Array64.app (pr, a)
