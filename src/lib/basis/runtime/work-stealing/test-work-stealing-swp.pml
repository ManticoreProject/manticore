structure TestWorkStealingSWP =
  struct

    structure W = WorkStealing
    structure PT = PrimTypes

    fun fib n = if n < 2 then n else fib(n-1) + fib(n-2)

_primcode(
  
define @test-ld (x : unit / exh : exh) : bool =
  cont k (x : unit) = return(true)
(* put the return continuation in the local deque *)
  let id : int = VProc.@id(host_vproc / exh)
  let localDequeGlobalList : any = ccall M_WSAllocLocalDeque(id)
  let localDeque : W.local_deque = ccall M_WSGetLocalDeque(localDequeGlobalList)
  do W.@local-deque-push-tl(localDeque, k / exh)

(* trigger GCs *)
  let fib : fun(PT.ml_int / PT.exh -> PT.ml_int) = pmlvar fib
  let arg : PT.ml_int = alloc(30)
  let x : PT.ml_int = apply fib(arg / exh)
  let x : PT.ml_int = apply fib(arg / exh)
  let x : PT.ml_int = apply fib(arg / exh)

(* k should have survived the GCs *)
  let k : PT.fiber = W.@local-deque-pop-tl(localDeque / exh)
  throw k(UNIT)
;

)

    val testLd : unit -> bool = _prim(@test-ld)
    val x = testLd()

  end
