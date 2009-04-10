structure ListMergesort =
  struct

    fun merge (x,y) = (
	  case (x,y)
	   of (nil,y) => y
	    | (x,nil) => x
	    | ((hx::tx),
	       (hy::ty)) => 
	      if hx < hy then hx :: merge(tx,(hy::ty))
	      else hy :: merge((hx::tx),ty)
          (* end case *))

    fun split xs = (
	  case xs
	   of (nil) => (nil,nil)
	    | (a::nil) => (a::nil,nil)
	    | (a::b::t) => let val (x,y) = split(t) in (a::x,b::y) end
          (* end case *))

    fun mergesort xs = (
	  case xs
	   of (nil) => nil
	    | (x::nil) => x::nil
	    | (l) =>
	      let
		  val (l1,l2) = split(l)
	      in
		  merge(mergesort(l1),mergesort(l2))
	      end
          (* end case *))

  end

structure Main = struct

    fun randomList n = List.tabulate(n, fn _ => Rand.range(0, 10000) 0w43)

    fun timeit n = let
	  fun thd () = let
		val t0 = Time.now()
		val ls = ListMergesort.mergesort(randomList n)
		val t = Time.-(Time.now(), t0)
		in
		  TextIO.print(concat[
		      Int.toString n, " number of elements ",
		      Time.toString t, " seconds\n"
		    ])
		end
	  in
	    thd ()
	  end

    fun main _ = (timeit 262144; OS.Process.success)

  end
