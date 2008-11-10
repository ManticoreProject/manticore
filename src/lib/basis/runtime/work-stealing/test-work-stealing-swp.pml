structure TestWorkStealingSWP =
  struct

    structure W = WorkStealing
    structure PT = PrimTypes
    structure IVar = WorkStealingIVar

    fun sfib n = if n < 2 then n else sfib(n-1) + sfib(n-2)

_primcode(
  
(* test local deque operations *)
define @test-ld (x : unit / exh : exh) : bool =
  cont k (x : unit) = return(true)
  cont k' (x : unit) = return(false)
(* put the return continuation in the local deque *)
  let id : int = VProc.@id(host_vproc / exh)
  let localDequeGlobalList : any = ccall M_WSAllocLocalDeque(id)
  let localDeque : W.local_deque = ccall M_WSGetLocalDeque(localDequeGlobalList)
  do W.@local-deque-push-tl(localDeque, k' / exh)
  do W.@local-deque-push-tl(localDeque, k / exh)

(* trigger GCs *)
  let sfib : fun(PT.ml_int / PT.exh -> PT.ml_int) = pmlvar sfib
  let arg : PT.ml_int = alloc(30)
  let x : PT.ml_int = apply sfib(arg / exh)
  let x : PT.ml_int = apply sfib(arg / exh)
  let x : PT.ml_int = apply sfib(arg / exh)

(* k should have survived the GCs *)
  let k : PT.fiber = W.@local-deque-pop-tl(localDeque / exh)
  let k' : PT.fiber = W.@local-deque-pop-tl(localDeque / exh)
  throw k(UNIT)
;

(* loops until a successful steal occurs *)
define @test-steal (x : unit / exh : exh) : bool =
  cont k (x : unit) = return(true)
  cont k' (x : unit) = return(false)

  do W.@push-tl(k / exh)

  fun infinitelp (i : int) : bool = 
(*do ccall M_PrintInt(i)*)
(*do print_ppt()*)
(*do VProc.@handle-messages(/ exh)*)
(*do vpstore(ATOMIC, host_vproc, false)*)
      let m : bool = vpload(ATOMIC, host_vproc)
      if m
         then throw k'(enum(0))
      else apply infinitelp (I32Add(i, 1))
  apply infinitelp (0)  
;

      (* fibonacci function as would result from the pval transformation *)
      define @fib-t (n : PT.ml_int / exh : PT.exh) : PT.ml_int =
	fun fib (n : [int] / exh : PT.exh) : [int] =
	    let raw : int = #0(n)
	    if I32Lte(raw, 1)
	       then
		let wlit : [int] = alloc(raw)
		return(wlit)
	       else
		 let raw : int = #0(n)

                 fun spawnFn (k : PT.fiber / exh : exh) : () = W.@push-tl(k / exh)
		 let iv : IVar.ivar = IVar.@ivar(spawnFn / exh)

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
		      return(x)

		 do W.@push-tl(k / exh)

		 (*eval the body *)
		 let a : [int] = alloc(I32Sub(raw,1))
		 let p : [int] = apply fib(a / exh)

		 let notStolen : PT.bool = W.@pop-tl( / exh)
		 do if notStolen
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

    val testLd : unit -> bool = _prim(@test-ld)
    val testSteal : unit -> bool = _prim(@test-steal)
    val pfib : int -> int = _prim(@fib-t)

  end
