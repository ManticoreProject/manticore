
structure TestWorkStealing =
  struct

    structure PT = PrimTypes
    structure FLS = FiberLocalStorage
    structure IVar = WorkStealingIVar

    fun t () = false
    fun failDeadlock () = UnitTesting.validate "deadlock" t
    fun failAns () = UnitTesting.validate "answer" t

    _primcode(

      (* fibonacci function as would result from the pval transformation *)
      define @fib (n : PT.ml_int / exh : PT.exh) : PT.ml_int =
	fun fib (n : [int] / exh : PT.exh) : [int] =
	    let raw : int = #0(n)
	    if I32Lte(raw, 1)
	       then
		let wlit : [int] = alloc(raw)
		return(wlit)
	       else
		 let raw : int = #0(n)

		 cont retK (x : [int]) = return(x)
		 let iv : IVar.ivar = IVar.@ivar( / exh)

		 fun bodyP (selFn : fun (PT.unit / PT.exh -> [int]) / exh : PT.exh) : [int] =
		     let a : [int] = alloc(I32Sub(raw,2))
		     let q : [int] = apply fib(a / exh)
		     let p : [int] = apply selFn(UNIT / exh)
		     let p : int = #0(p)
		     let q : int = #0(q)
		     let r : [int] = alloc(I32Add(p,q))
		     return(r)           
		 cont k (unt : PT.unit) =
		      fun f (unt : PT.unit / exh : PT.exh) : [int] =
			  let v : any = IVar.@get(iv / exh)
			  let v : [int] = ([int])v
			  return(v)
		      let x : [int] = apply bodyP(f / exh)
		      throw retK(x)

		 do Cilk5WorkStealing.@push-tl(k / exh)

		 (*eval the body *)
		 let a : [int] = alloc(I32Sub(raw,1))
		 let p : [int] = apply fib(a / exh)

		 let x : PT.bool = Cilk5WorkStealing.@pop-tl( / exh)
		 do if x
		    then return()
		    else 
			 do IVar.@put(iv, (any)p / exh)
			 let unt : PT.unit = Control.@stop(/ exh)
			 return()

		fun f (x : PT.unit / exh : PT.exh) : [int] =
		    return(p)

		apply bodyP (f / exh)

	let n : [int] = apply fib (n / exh)
	return(n)
      ;

    )

    val fib : int -> int = _prim(@fib)
    val _ = Print.printLn(Int.toString(fib 27))

end
