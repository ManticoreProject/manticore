fun incr n = n+1

fun tos parr = let
  val len = lengthP parr
  fun lp (i, acc) = 
    if (i < 0) then
      String.concat ("[|" :: acc)
    else let
      val s = Int.toString(parr!i)
      in
        lp (i-1, s::","::acc)
      end
  in
    if (len < 0) then
      raise Fail "bug"
    else if (len = 0) then
      "[||]"
    else let
      val s = Int.toString(parr!(len-1)) :: ["|]"]
      in
        lp (len-2, s)
      end
  end

val c0 = [| incr n | n in [| 1 to 10 |] |]
val c1 = [| incr n | n in [| 1 to 10 by 1 |] |]
val c2 = [| incr n | n in [| 10 to 1 |] |]
val c3 = [| incr n | n in [| 10 to 1 by ~1 |] |]

val c4 = [| incr n | n in [| 1 to 10 by 2 |] |]
val c5 = [| incr n | n in [| 10 to 1 by ~2 |] |]
val c6 = [| incr n | n in [| 1 to 10 by ~2 |] |]
val c7 = [| incr n | n in [| 10 to 1 by 2 |] |]

val c8 = [| incr n | n in [| 99 to 109 by 3 |] |]
val c9 = [| incr n | n in [| 109 to 99 by ~3 |] |]
val cA = [| incr n | n in [| ~9 to ~1 |] |]
val cB = [| incr n | n in [| ~1 to ~9 by ~1 |] |]

val all = [c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, cA, cB]

fun compose (f, g) = (fn x => f (g x))
val _ = List.app (compose (Print.printLn, tos)) all

val _ = Print.printLn "done."



