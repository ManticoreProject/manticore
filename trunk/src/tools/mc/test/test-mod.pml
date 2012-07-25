structure Main =
  struct
    fun fib i = (case i
       of 0 => 0
	| 1 => 1
	| n => fib(i-1) + fib(i-2)
      (* end case *))

    structure T =
      struct
        fun f i = (
	    print (Int.toString (fib i)^"\n");
	    print (Int.toString (Fib.fib i + TreeAdd.treeAdd (TreeAdd.mkTree 10))^"\n"))
      end
    fun g () = T.f 10 + 1
  end

fun main _ = Main.g()
