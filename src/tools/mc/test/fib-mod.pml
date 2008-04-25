structure Fib =
  struct
    fun fib i = (case i
       of 0 => 0
	| 1 => 1
	| n => fib(i-1) + fib(i-2)
      (* end case *))
  end

