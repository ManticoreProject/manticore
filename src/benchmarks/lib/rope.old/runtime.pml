structure Runtime =
  struct

    datatype splitting_strategy = datatype SplittingStrategy.splitting_strategy

    val splittingStrategy = SplittingStrategy.default

    fun forkjoin (f, g) = (| f (), g () |)

  (* failwith : string -> 'a *)
  (* using this for the moment so we can observe the exception message at runtime *)
    fun failwith msg = (Print.printLn msg; (raise Fail msg))

    fun parMap f xs =
	let fun mp xs =
		(case xs
		  of nil => nil
		   | x :: nil => f x :: nil
		   | x :: xs => 
		     let
			 val (x', xs') = (| f x, mp xs |)
		     in
			 x' :: xs'
		     end
		(* end case *))
	in
	    mp xs
	end

    val numVProcs = VProc.numVProcs ()
    fun numAvailProcs () = numVProcs

#ifndef SEQUENTIAL
    val otherHungryProcs = WorkStealing.isLocalDequeEmpty
  (* returns the unique integer id of the worker or processor on which the calling context is executing *)
  (* FIXME: get the real worker id from the work stealing scheduler... the impl. below is OK until we *)
  (* start using a multiprogrammed work stealing scheduler. *)
    fun workerId () = VProcExtras.id (VProcExtras.host ())
#else
    fun otherHungryProcs () = false
    fun workerId () = 0
#endif

  end
