structure Fib =
  struct
    fun fib i = (case i
       of 0 => 0
	| 1 => 1
	| n => fib(i-1) + fib(i-2)
      (* end case *))
  end

fun println s = (print s; print "\n")

fun main _ = let
  val f10 = itos (Fib.fib 10)
  in
    println f10
  end
