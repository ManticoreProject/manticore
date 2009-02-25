fun fib (i : int) = (case i
       of 0 => (0 : int)
	| 1 => (1 : int)
	| n => fib(i-1) + fib(i-2)
      (* end case *))

val globalBFS = GlobalBFSScheduler.workGroup()
val x = ImplicitThread.runWithGroup(globalBFS, fn () =>
	   let 
	       val fut = LazyFuture.delay(fn () => fib 20, false)
	       val () = LazyFuture.run fut
	       val x = fib 20
	   in
	       x + LazyFuture.force fut
	   end)
val () = pml_assert(x = fib 20 * 2)

fun pfib (i : int) = (case i
       of 0 => (0 : int)
	| 1 => (1 : int)
	| n => let
	      val fut = LazyFuture.delay(fn () => pfib(i-1), false)
	      val () = LazyFuture.run fut
	      in
	        pfib(i-2) + LazyFuture.force fut
	      end
      (* end case *))

val x = ImplicitThread.runWithGroup(globalBFS, fn () => pfib 20)
val () = pml_assert(x = fib 20)

val () = Print.printLn "Finished futures tests"

