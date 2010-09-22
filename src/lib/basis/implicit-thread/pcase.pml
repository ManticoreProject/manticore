(* pcase.pml
 *
 * COPYRIGHT (c) 2010 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Underlying support for parallel case expressions.
 * See JFP paper submission (in final/jfp-datapar) for details.
 *)

structure PCase = struct

  _primcode (

    define @dispatch (_ : unit / exh : exh) : any =
	let _ : unit = SchedulerAction.@stop ()
	return (UNIT)
      ;

    define @pcase-wrapper (f : fun ([fun (any / exh -> any), fun (exn / exh -> any)] / exh -> any) / exh : exh) : any =
	cont retK (x : any) = return (x)
	fun throwReturn (x : any / exh : exh) : any = throw retK (x)
	fun throwExn (e : exn / _ : exh) : any = throw exh (e)
	let arg : [fun (any / exh -> any), fun (exn / exh -> any)] = alloc (throwReturn, throwExn)
	apply f (arg / exh)
      ;

  )

  val dispatch : unit -> 'a = _prim (@dispatch)

  val pcaseWrapper : ((('a -> 'b) * (exn -> 'c)) -> 'd) -> 'a  = _prim (@pcase-wrapper)

end
