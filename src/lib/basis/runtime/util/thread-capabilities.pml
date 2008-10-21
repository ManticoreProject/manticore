(* thread-capabilities.pml
 *
 * Thread capabilities specify properties of an individual thread.
 *)

structure ThreadCapabilities =
  struct

    structure PT = PrimTypes
    structure FLS = FiberLocalStorage

    type capability = _prim( [FLS.fls_tag, any] )

    _primcode(

      define @new (tg : FLS.fls_tag, v : any / exh : PT.exh) : capability =
	let cap : capability = alloc(tg, v)
	return(cap)
      ;

      define @add (fls : FLS.fls, cap : capability / exh : PT.exh) : FLS.fls =
	let fls : FLS.fls = FLS.@add(fls, #0(cap), #1(cap) / exh)
	return(fls)
      ;

      define @init (tg : FLS.fls_tag, init : fun(PT.unit / PT.exh -> any) / exh : PT.exh) : capability =
	let c : SetOnceMem.set_once_mem = SetOnceMem.@new(init / exh)
	let cap : capability = @new(tg, c / exh)
	return(cap)
      ;

    (* get the value of a thread capability *)
      define @get-from-fls (tg : FLS.fls_tag / exh : exh) : any =
	let fls : FLS.fls = FLS.@get( / exh)
	let x : Option.option = FLS.@find(fls, tg / exh)
	let x : any =
		     case x
		      of Option.NONE => 
		  (* FIXME: throw an exception here *)
			do assert(PT.false)
			return($0)
		      | Option.SOME (c : SetOnceMem.set_once_mem) =>
			let x : any = SetOnceMem.@get(c / exh)
			return(x)
		    end  
         return(x)
      ;


    )

  end
