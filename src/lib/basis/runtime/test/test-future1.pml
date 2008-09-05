structure TestFuture1 =
  struct

    structure F = Future1

    fun fib n = if n < 2 then n else fib(n-1) + fib(n-2)

    fun cancel1Fut () = 
	let fun f1 () = fib 100
	    fun f2 () = fib 20
	    val fut1 = F.future f1
	    val fut2 = F.future f2
	    val x = F.touch fut2
	in
	    F.cancel fut1;
	    x
	end

(*    val _ = Print.printLn("cancel1Fut()="^Int.toString (cancel1Fut()))*)

    fun cancel2Futs () = 
	let fun f1 () = 
		let fun f () = fib 40
		    val fut1 = F.future f
		    val fut2 = F.future f
		in
		    F.touch fut1 + F.touch fut2
		end
	    fun f2 () = fib 20
	    val fut1 = F.future f1
	    val x = fib 20
	in
	    F.cancel fut1;
	    x
	end

(*    val _ = Print.printLn("cancel2Futs()="^Int.toString (cancel2Futs()))*)

    fun futFib n =
	if n < 2 then n else let
	    fun f () = futFib (n-1)
	    val f1 = Future1.future f
	    in
	      futFib(n-2) + Future1.touch f1
	    end

    fun cancelNFuts () = 
	let fun f1 () = futFib 15
	    val fut1 = F.future f1
	    val x = fib 29
	in
	    F.cancel fut1;
	    x
	end

    val _ = Print.printLn("cancelNFuts()="^Int.toString (cancelNFuts()))

  end
