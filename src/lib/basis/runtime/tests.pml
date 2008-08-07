structure Tests =
  struct

    structure PT = PrimTypes

  (* control *)
    _primcode(
      define @test1 (x : PT.unit / exh : PT.exh) : PT.unit =
	cont act (s : PT.signal) =
	  case s
	   of STOP => do Control.@forward(STOP / exh)
		      return(UNIT)
	    | PT.PREEMPT (k : PT.fiber) => 
	      do ccall M_Print("Seems to have worked\n")
	      do Control.@forward(STOP / exh)
	      return(UNIT)
	  end
	cont k (x : PT.unit) = do Control.@forward(PT.PREEMPT(k) / exh)
			       return(UNIT)
	do ccall M_Print("Testing run and forward\n")
	do Control.@run(act, k / exh)
	return(UNIT)
      ;

      define @test2 (x : PT.unit / exh : PT.exh) : PT.unit =
	fun lp (i : int / exh : PT.exh) : () =
	    if I32Gte(i, 2)
	       then return()
	    else
		let _ : PT.unit = Control.@yield(/exh)
		apply lp(I32Add(i,1) / exh)
	do apply lp (0 / exh)
	return(UNIT)
      ;

    )
(*
    val _ = Print.printLn "control test: going to sleep"
    val t : unit -> unit = _prim(@test2)
    val _ = t()
    val _ = Print.printLn "control test: waking"
*)
  (* thread operations *)
    _primcode (

      define @ex(x : PT.unit / exh : PT.exh) : PT.unit =
        let flg : ![PT.bool] = alloc(FALSE)
            do ThreadOps.@sleep(I64Mul(I32ToI64(1000000), 2), flg / exh)
        return(UNIT)
      ;

    )

    val _ = Print.printLn "sleep test: going to sleep"
    val ex : unit -> unit = _prim(@ex)
    val _ = ex()
    val _ = Print.printLn "sleep test: woke up"

  (* scheduler utils *)
(*    val _ = Print.printLn(Int.toString(UnitTesting.fib(30)))*)

  end
