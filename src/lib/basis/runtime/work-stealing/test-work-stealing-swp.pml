structure TestWorkStealingSWP =
  struct

    structure W = WorkStealing
    structure PT = PrimTypes

    fun fib n = if n < 2 then n else fib(n-1) + fib(n-2)

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
  let fib : fun(PT.ml_int / PT.exh -> PT.ml_int) = pmlvar fib
  let arg : PT.ml_int = alloc(30)
  let x : PT.ml_int = apply fib(arg / exh)
  let x : PT.ml_int = apply fib(arg / exh)
  let x : PT.ml_int = apply fib(arg / exh)

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

)

    val testLd : unit -> bool = _prim(@test-ld)
    val testSteal : unit -> bool = _prim(@test-steal)

  end
