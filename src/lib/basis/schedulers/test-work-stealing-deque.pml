structure D = WorkStealingDeque

#define DUMMY_ELT enum(2):any

_primcode (

define @test (x : unit / exh : exh) : unit =
    let self : vproc = host_vproc

    let deque : deque = D.@new-from-atomic (self, 2)
    do @push-new-end-from-atomic (self, deque, DUMMY_ELT)
    do @push-new-end-from-atomic (self, deque, DUMMY_ELT)
    let x : elt = @pop-new-end-from-atomic (self, deque)
    do assert (Equal (x, DUMMY_ELT))
    do @push-new-end-from-atomic (self, deque, DUMMY_ELT)
    let x : elt = @pop-new-end-from-atomic (self, deque)
    do assert (Equal (x, DUMMY_ELT))
    let x : elt = @pop-new-end-from-atomic (self, deque)
    do assert (Equal (x, DUMMY_ELT))

    let deque : deque = D.@new-from-atomic (self, 2)
    do @push-new-end-from-atomic (self, deque, DUMMY_ELT)
    do @push-new-end-from-atomic (self, deque, DUMMY_ELT)
    let x : elt = @pop-old-end-from-atomic (self, deque)
    do assert (Equal (x, DUMMY_ELT))
    do @push-new-end-from-atomic (self, deque, DUMMY_ELT)
    let x : elt = @pop-old-end-from-atomic (self, deque)
    do assert (Equal (x, DUMMY_ELT))
    let x : elt = @pop-old-end-from-atomic (self, deque)
    do assert (Equal (x, DUMMY_ELT))

    return ()
  ;

)

val test : unit -> unit = _prim (@test)
val _ = test ()
