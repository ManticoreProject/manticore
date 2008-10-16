val a = Array64.array (20, 1)

fun pr n =
  if n >= 20 then
    ()
  else let
    val s = Int.toString (Array64.sub (a, n))
    in
      Print.print (Int.toString n ^ "\t" ^ s ^ "\n");
      pr (n+1)
    end

val _ = pr 0

