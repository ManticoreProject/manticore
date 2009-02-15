(* p-or.pml
 * 
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Parallel or operator.
 *)

structure POr : sig

    type 'a por_thunk = unit -> 'a Option.option
  (* takes two thunks and returns the first to evaluate. the other thunk is canceled *)
    val pOr : ('a por_thunk * 'a por_thunk) -> 'a

  end = struct

    structure PT = PrimTypes
    structure O = Option
    structure C = Cancelation
    structure WorkStealing = Cilk5WorkStealing

    #define EMPTY_ST     $0
    #define DONE_ST      $1
    #define FULL_ST      $2

    type 'a por_thunk = unit -> 'a O.option

    _primcode(

      typedef p_or_state = any;
      typedef p_or = ![p_or_state];
      typedef por_thunk = fun(PT.unit / PT.exh -> O.option);

      define @p-or (f1 : por_thunk, f2 : por_thunk / exh : PT.exh) : O.option =
	cont retK (x : O.option) = return(x)
	let por : p_or = alloc(EMPTY_ST)
	let por : p_or = promote(por)

	fun markFull () : () =
	    cont exit () = return()
	    let v1 : p_or_state = CAS(&0(por), EMPTY_ST, FULL_ST)
	    do if Equal(v1, EMPTY_ST)
		  then throw exit ()
		  else return()
	    let v1 : por_state = CAS(&0(por), DONE_ST, FULL_ST)
	    do if Equal(v1, DONE_ST)
		  then throw exit ()
		  else return()
	    Control.stop(/ exh)

	fun markEmpty () : () =
	    let v1 : por_state = CAS(&0(por), EMPTY_ST, DONE_ST)
	    do if Equal(v1, EMPTY_ST)
		  then Control.@stop(/ exh)
		  else return()
	    if Equal(SELECT(0, por), FULL_ST)
	       then Control.@stop(/ exh)
	       else return()

       (* return by stopping -- necessary cleaning up cancelation wrapper *)
	cont returnK (x : O.option) =
	     cont k (_ : PT.unit) = throw retK(x)
	     do WorkStealing.@push-tl(k / exh)
	     do Control.@stop(/ exh)
	     return(NONE)

	fun handlerK (sibling : C.cancelable, f : por_thunk / exh : PT.exh) : PT.fiber =
	    cont k (_ : PT.unit) = 
		 let x : O.option = apply f(UNIT / exh)
		 case x
		  of O.NONE => 
		     do apply markEmpty()
		     throw returnK(O.NONE)
		   | O.SOME(v : any) =>
		     do apply markFull()
		     let _ : PT.unit = C.@cancel(sibling / exh)
		     throw returnK(x)
		  end
	    let k : PT.fiber = (PT.fiber)k
	    return(k)

	let c1 : C.cancelable = C.@new(tag(pOrCancelable) / exh)
	let c2 : C.cancelable = C.@new(tag(pOrCancelable) / exh)

	let k2 : PT.fiber = apply handlerK(c1, f2 / exh)
	let k2 : PT.fiber = C.@wrap(c2, k2 / exh)
	do WorkStealing.@push-tl(k2 / exh)

	let k1 : PT.fiber = apply handlerK(c2, f1 / exh)
	let k1 : PT.fiber = C.@wrap(c1, k1 / exh)
	throw k1(UNIT)
      ;

      define @por-wrap (arg : [por_thunk, por_thunk] / exh : PT.exh) : any =
	@por(#0(arg), #1(arg) / exh)
      ;

    )

    val pOr : ('a por_thunk * 'a por_thunk) -> 'a = _prim(@por-wrap)

  end
