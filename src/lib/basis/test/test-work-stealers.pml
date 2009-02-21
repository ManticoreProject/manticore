structure TestWorkStealers =
  struct

    structure PT = PrimTypes
    structure FLS = FiberLocalStorage

    fun fib n = if n < 2 then n else fib(n-1) + fib(n-2)

    _primcode (

      define @test(x : PT.unit / exh : PT.exh) : PT.unit =
        let nVps : int = SchedulerUtils.@num-vprocs(/ exh)
        let cnt : ![int] = alloc(nVps)
	let cnt : ![int] = promote(cnt)
	fun f1 (x : PT.unit / exh : PT.exh) : PT.unit =
	    do print_ppt()
	    let fib : fun([int] / PT.exh -> [int]) = pmlvar fib
	    let x : [int] = apply fib(alloc(35) / exh)
	    do print_ppt()
	    let i : int = I32FetchAndAdd(&0(cnt), ~1)
	    return(UNIT)
	let k : PT.fiber = Control.@fiber(f1 / exh)
	fun f2 (vp : vproc / exh : PT.exh) : () =
            let fls : FLS.fls = FLS.@new(UNIT/ exh)
	    VProcQueue.@enqueue(fls, k / exh)
	do SchedulerUtils.@for-other-vprocs(f2 / exh)

        do FLS.@set-pinned(/ exh)

	fun wait () : () =
	    if I32Eq(SELECT(0, cnt), 1)
	       then return()
	    else
do print_ppt()
                do SchedulerAction.@yield(/ exh)
		apply wait()
	do apply wait ()
	return(UNIT)
      ;

    )

    val test : unit -> unit = _prim(@test)
    val _ = test()
    val _ = printMsg("test succeeded")

  end
