structure IntBinaryMap =
  struct

    type ord_key = int
    type 'a map = (int * 'a) list

    val empty = []
    fun insert (mp, x, v) = (x, v) :: mp
    fun find (mp, x) = let
	   fun f mp = (case mp
                of nil => NONE
		 | (x', v') :: mp => if x' = x
                           then SOME v'
		           else f mp
                (* end case *))
           in
	      f mp
	   end
  end

structure TestIntBinaryMap =
  struct

    structure I = IntBinaryMap

    val x = I.empty
    val y = I.insert(x, 4, "t1")
    val y = I.insert(y, 5, "t3")
    val z = (case I.find(y, 4)
	      of SOME s => s = "t1"
	       | _ => fail "test")

  end
