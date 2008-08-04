structure UnitTesting =
  struct

    fun fib n = if n < 2 then n else fib(n-1) + fib(n-2)

    fun validate s f = if f() then print "success\n" else fail s

  end
