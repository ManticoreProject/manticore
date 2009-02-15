structure LazyFuture : LAZY_FUTURE =
  struct

#define STOLEN_OFF     0
#define IVAR_OFF       1
#define THUNK_OFF      2
#define CANCELABLE_OFF 3

    _primcode(

      typedef future = ![
		 bool,                                (* true iff the future has been stolen*)
		 IVar.ivar,                           (* state *)
		 fun(unit / exh -> any),              (* thunk *)
		 Option.option                        (* cancelable *)
      ];

    (* create a future *)
      define @delay (f : fun(unit / exh -> any) / exh : exh) : future =
	let ivar : IVar.ivar = IVar.@ivar(/ exh)
	let fut : future = alloc(false, ivar, f, Option.NONE)
	let fut : future = promote(fut)
	return(fut)
      ;

    (* evaluate the future without synchronization *)
      define @eval (fut : future / exh : exh) : any =
	let f : fun(unit / exh -> any) = SELECT(THUNK_OFF, fut)
	let x : any = apply f (UNIT / exh)
	return(x)
      ;

    (* evaluate the future and seed the ivar with the result *)
      define @steal (fut : future / exh : exh) : () =
        let stolen : bool = CAS(ADDR_OF(STOLEN_OFF, fut), false, true)
	if stolen
	   then
	    return()
	else
	    let x : any = @eval(fut / exh)
            let nilThk : fun(unit / exh -> any) = (fun(unit / exh -> any))$0
	    do UPDATE(THUNK_OFF, fut, nilThk)                  (* prevent a space leak *)
	    IVar.@put(SELECT(IVAR_OFF, fut), x / exh)
      ;

    (* place the future on the ready queue *)
      define @run (fut : future / exh : exh) : unit =
        cont k (x : unit) =
          do @steal(fut / exh)
          SchedulerAction.@stop(/ exh)
        let thd : ImplicitThread.thread = ImplicitThread.@thread(k, SELECT(CANCELABLE_OFF, fut) / exh)
	do ImplicitThread.@spawn(thd / exh)
	return(UNIT)
      ;

  (* synchronize on completion of the future *)
      define @force (fut : future / exh : exh) : any =
	do @steal(fut / exh)
	IVar.@get(SELECT(IVAR_OFF, fut) / exh)
      ;

    (* make the future cancelable *)
      define @cancelable (fut : future / exh : exh) : future =
	let c : Cancelation.cancelable = Cancelation.@new(/ exh)
        let fut : future = alloc(false, SELECT(IVAR_OFF, fut), SELECT(THUNK_OFF, fut), Option.SOME(c))
	return(fut)
      ;

  (* cancel the future *)
      define @cancel (fut : future / exh : exh) : unit =
        case SELECT(CANCELABLE_OFF, fut)
	 of Option.NONE =>
	    (* QUESTION: is this an error? *)
	    return(UNIT)
	  | Option.SOME(c : Cancelation.cancelable) =>
	    do Cancelation.@cancel(c / exh)
            return(UNIT)
        end
      ;

    )

    type 'a future = _prim(future)

    val delay : (unit -> 'a) -> 'a future = _prim(@delay)
    val cancelable : 'a future -> 'a future = _prim(@cancelable)
    val run : 'a future -> unit = _prim(@run)
    val force : 'a future -> 'a = _prim(@force)
    val cancel : 'a future -> unit = _prim(@cancel)

  end
