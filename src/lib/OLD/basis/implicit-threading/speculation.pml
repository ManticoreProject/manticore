(* speculation.pml
 * 
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Speculative parallelism.
 *)

structure Speculation :
  sig

  (* parallel-or: evaluate the two thunks in potentially in parallel, and return the result of whichever 
   * finishes first.
   *) 
    val por : ((unit -> 'a Option.option) * (unit -> 'a Option.option)) -> 'a Option.option

  end = struct

    structure PT = PrimTypes
    structure O = Option
    structure C = Cancelation


    _primcode (

#define EMPTY_ST     $0
#define DONE_ST      $1
#define FULL_ST      $2

      typedef por = ![any];

      define @por (f1 : fun(unit / exh -> O.option),
		   f2 : fun(unit / exh -> O.option)
		  / exh : exh) : O.option =
      (* lazy promotion *)
        let por : por = alloc(EMPTY_ST)

        fun markFull () : () =
            let por : por = promote(por)
	    cont exit () = return()
	    let v1 : any = CAS(&0(por), EMPTY_ST, FULL_ST)
	    do if Equal(v1, EMPTY_ST)
		  then throw exit ()
		  else return()
	    let v1 : any = CAS(&0(por), DONE_ST, FULL_ST)
	    do if Equal(v1, DONE_ST)
		  then throw exit ()
		  else return()
	    let _ : unit = SchedulerAction.@stop()
            return()

	fun markEmpty () : () =
            let por : por = promote(por)
	    let v1 : any = CAS(&0(por), EMPTY_ST, DONE_ST)
	    do if Equal(v1, EMPTY_ST)
		  then let _ : unit = SchedulerAction.@stop()
                       return()
		  else return()
	    if Equal(SELECT(0, por), FULL_ST)
	       then let _ : unit = SchedulerAction.@stop()
                    return()
	       else return()

      (* capture the ite of the original thread *)
        let ite : ImplicitThread.ite = FLS.@get-ite(/ exh)

      (* resume the original thread *)
        cont retK (x : O.option) = 
          cont k (z : unit) = return(x)
          let thd : ImplicitThread.thread = ImplicitThread.@alloc(ite, k / exh)
          do ImplicitThread.@run-out-of-scheduler(thd / exh)
          let e : exn = Fail(@"Speculation.@por: impossible")
          throw exh(e)

	fun handler (sibling : C.cancelable, f : fun(unit / exh -> O.option)) : PT.fiber =
	    cont k (unit : unit) = 
		 let x : O.option = apply f(UNIT / exh)
		 case x
		  of O.NONE => 
		     do apply markEmpty()
		     throw retK(O.NONE)
		   | O.SOME(v : any) =>
		     do apply markFull()
		     do C.@cancel(sibling / exh)
		     throw retK(x)
		 end
	    return(k)

        let c1 : C.cancelable = C.@new(/ exh)
	let c2 : C.cancelable = C.@new(/ exh)

        let k2 : PT.fiber = apply handler(c1, f2)
        let t1 : ImplicitThread.thread = ImplicitThread.@thread(k2, O.SOME(c2) / exh)
	do ImplicitThread.@spawn(t1 / exh)

	let k1 : PT.fiber = apply handler(c2, f1)
        let t1 : ImplicitThread.thread = ImplicitThread.@thread(k1, O.SOME(c1) / exh)
	do ImplicitThread.@run-out-of-scheduler(t1 / exh)
        let e : exn = Fail(@"Speculation.@por: impossible")
        throw exh(e)
      ;

      define @por-w (arg : [fun(unit / exh -> O.option), fun(unit / exh -> O.option)] / exh : exh) : O.option =
	@por(#0(arg), #1(arg) / exh)
      ;
  
    )

    val por : ((unit -> 'a Option.option) * (unit -> 'a Option.option)) -> 'a Option.option = _prim(@por-w)

  end
