(* eager-future.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Components for building futures with eager semantics.
 *)

structure EagerFuture : FUTURE =
  struct

    _primcode(

#define IVAR_OFF         0
#define CANCELABLE_OFF   1

      typedef future = [
                ImplicitThreadIVar.ivar,                (* value *)
		Option.option                           (* cancelable *)
             ];

      define @alloc(ivar : ImplicitThreadIVar.ivar, c : Option.option / exh : exh) : future =
	let fut : future = alloc(ivar, c)
	return(fut)
      ;

      define @future-no-cancelation (f : fun(unit / exh -> any) / exh : exh) : future =
	let ivar : ImplicitThreadIVar.ivar = ImplicitThreadIVar.@empty-ivar(/ exh)       
	let fut : future = @alloc(ivar, Option.NONE / exh)

	cont k (x : unit) = return(fut)
	let thd : ImplicitThread.thread = ImplicitThread.@thread(k, Option.NONE / exh)
	do ImplicitThread.@spawn(thd / exh)

	let v : any = apply f (UNIT / exh)

	let isMigrated : bool = ImplicitThread.@remove-thread(thd / exh)
	if isMigrated
	   then
	    (* slow clone *)
	    do ImplicitThreadIVar.@put(SELECT(IVAR_OFF, fut), v / exh)
	    SchedulerAction.@stop()
	else
	    (* fast clone *)
	    let ivar : ImplicitThreadIVar.ivar = ImplicitThreadIVar.@ivar(v / exh)
	    @alloc(ivar, Option.NONE / exh)
      ;

      define @future-with-cancelation (f : fun(unit / exh -> any) / exh : exh) : future =
	let ivar : ImplicitThreadIVar.ivar = ImplicitThreadIVar.@empty-ivar(/ exh)
	let c : Cancelation.cancelable = Cancelation.@new(/ exh)
	let fut : future = @alloc(ivar, Option.SOME(c) / exh)

	cont k (x : unit) = return(fut)
	let thd : ImplicitThread.thread = ImplicitThread.@thread(k, Option.NONE / exh)
	do ImplicitThread.@spawn(thd / exh)

	cont k' (x : unit) = 
	  let v : any = apply f (UNIT / exh)

	  let isMigrated : bool = ImplicitThread.@remove-thread(thd / exh)
	  if isMigrated
	     then
	      (* slow clone *)
	      do ImplicitThreadIVar.@put(SELECT(IVAR_OFF, fut), v / exh)
	      let _ : unit = SchedulerAction.@stop()
              return(fut)
	  else
	      (* fast clone *)
	      let ivar : ImplicitThreadIVar.ivar = ImplicitThreadIVar.@ivar(v / exh)
	     @alloc(ivar, Option.NONE / exh)

        let thd : ImplicitThread.thread = ImplicitThread.@thread(k', Option.SOME(c) / exh)
        do ImplicitThread.@run-out-of-scheduler(thd / exh)

        return(fut)
      ;

      define @future (arg : [fun(unit / exh -> any), bool] / exh : exh) : future =
	if SELECT(1, arg)
	   then @future-with-cancelation(SELECT(0, arg) / exh)
	else @future-no-cancelation(SELECT(0, arg) / exh)
      ;

      define @touch (fut : future / exh : exh) : any =
	ImplicitThreadIVar.@get(SELECT(IVAR_OFF, fut) / exh)
      ;

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

    type 'a thunk = unit -> 'a
    type 'a future = _prim(future)
    val future : ('a thunk * bool) -> 'a future = _prim(@future)
    val touch : 'a future -> 'a = _prim(@touch)
    val cancel : 'a future -> unit = _prim(@cancel)

  end
