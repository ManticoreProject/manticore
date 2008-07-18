structure UnitTesting =
  struct

    fun validate s f = if f() then print "success\n" else fail s

  end
