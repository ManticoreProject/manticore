val vec = [| 1 to 10 |]
val square = [| [| 1 to 10 |] | r in vec |]

fun squareLen (arr : int parray parray) : int = let
  val n = PArray.length arr
  fun lp (i, tot) = 
    if (i >= n) then tot
    else let
      val m = PArray.length (arr!i)
      in
        lp (i+1, m+tot)
      end
  in
    lp (0, 0)
  end

val _ = Print.printLn ("square's first line: " ^ PArray.tos_int (square!0))
val _ = Print.printLn ("square's last line: " ^ PArray.tos_int (square!9))
val _ = Print.printLn ("total number of elements in square: " ^ Int.toString (squareLen square))

val _ = Print.printLn "done."
