structure D = WorkStealingDeque

#define DUMMY_ELT enum(2):any
#define DUMMY_ELT2 enum(3):any

_primcode (

define @check-elt (x : Option.option, y : any) : () =
    case x
     of Option.NONE =>
	do assert(false)
	return ()
      | Option.SOME (x : any) =>
	assert (Equal (x, y))
    end
  ;

define @test (x : unit / exh : exh) : unit =
    let self : vproc = host_vproc

    let workGroupId : long = 0:long

    let deque : D.deque = D.@new-from-atomic (self, workGroupId, 3)

    do D.@push-new-end-from-atomic (self, deque, DUMMY_ELT )
    let x : Option.option = D.@pop-new-end-from-atomic (self, deque )
    do @check-elt (x, DUMMY_ELT)
    do D.@push-new-end-from-atomic (self, deque, DUMMY_ELT )
    let x : Option.option = D.@pop-new-end-from-atomic (self, deque )
    do @check-elt (x, DUMMY_ELT)
    let isEmpty : bool = D.@is-empty-from-atomic (self, deque)
    do assert (isEmpty)

    do D.@push-new-end-from-atomic (self, deque, DUMMY_ELT2 )
    do D.@push-new-end-from-atomic (self, deque, DUMMY_ELT )
    let x : Option.option = D.@pop-new-end-from-atomic (self, deque )
    do @check-elt (x, DUMMY_ELT)
    do D.@push-new-end-from-atomic (self, deque, DUMMY_ELT )
    let x : Option.option = D.@pop-new-end-from-atomic (self, deque )
    do @check-elt (x, DUMMY_ELT)
    let x : Option.option = D.@pop-new-end-from-atomic (self, deque )
    do @check-elt (x, DUMMY_ELT2)
    let isEmpty : bool = D.@is-empty-from-atomic (self, deque)
    do assert (isEmpty)


    do D.@push-new-end-from-atomic (self, deque, DUMMY_ELT )
    do D.@push-new-end-from-atomic (self, deque, DUMMY_ELT2 )
    let x : Option.option = D.@pop-old-end-from-atomic (self, deque )
    do @check-elt (x, DUMMY_ELT)
    do D.@push-new-end-from-atomic (self, deque, DUMMY_ELT )
    let x : Option.option = D.@pop-old-end-from-atomic (self, deque )
    do @check-elt (x, DUMMY_ELT2)
    let x : Option.option = D.@pop-old-end-from-atomic (self, deque )
    do @check-elt (x, DUMMY_ELT)
    let isEmpty : bool = D.@is-empty-from-atomic (self, deque)
    do assert (isEmpty)


    do D.@push-new-end-from-atomic (self, deque, DUMMY_ELT )
    do D.@push-new-end-from-atomic (self, deque, DUMMY_ELT )
    let x : Option.option = D.@pop-old-end-from-atomic (self, deque )
    do @check-elt (x, DUMMY_ELT)
    do D.@push-new-end-from-atomic (self, deque, DUMMY_ELT )
    let x : Option.option = D.@pop-new-end-from-atomic (self, deque )
    do @check-elt (x, DUMMY_ELT)
    let x : Option.option = D.@pop-old-end-from-atomic (self, deque )
    do @check-elt (x, DUMMY_ELT)
    let isEmpty : bool = D.@is-empty-from-atomic (self, deque)
    do assert (isEmpty)

    do D.@release-from-atomic (self, deque)

    let claimed : bool = D.@claim-from-atomic (self, deque)
    do assert (claimed)

    do D.@push-new-end-from-atomic (self, deque, DUMMY_ELT )
    do D.@push-new-end-from-atomic (self, deque, DUMMY_ELT )
    let x : Option.option = D.@pop-new-end-from-atomic (self, deque )
    do @check-elt (x, DUMMY_ELT)
    do D.@push-new-end-from-atomic (self, deque, DUMMY_ELT )
    let x : Option.option = D.@pop-old-end-from-atomic (self, deque )
    do @check-elt (x, DUMMY_ELT)
    let x : Option.option = D.@pop-new-end-from-atomic (self, deque )
    do @check-elt (x, DUMMY_ELT)
    let isEmpty : bool = D.@is-empty-from-atomic (self, deque)
    do assert (isEmpty)

    do D.@push-new-end-from-atomic (self, deque, DUMMY_ELT )
    let x : Option.option = D.@pop-old-end-from-atomic (self, deque )
    do @check-elt (x, DUMMY_ELT)
    do D.@push-new-end-from-atomic (self, deque, DUMMY_ELT )
    let x : Option.option = D.@pop-new-end-from-atomic (self, deque )
    do @check-elt (x, DUMMY_ELT)
    do assert (isEmpty)

    let deque : D.deque = D.@double-size-from-atomic (self, workGroupId, deque)
    do D.@push-new-end-from-atomic (self, deque, DUMMY_ELT )
    do D.@push-new-end-from-atomic (self, deque, DUMMY_ELT )
    let x : Option.option = D.@pop-new-end-from-atomic (self, deque )
    do @check-elt (x, DUMMY_ELT)
    do D.@push-new-end-from-atomic (self, deque, DUMMY_ELT )
    let x : Option.option = D.@pop-old-end-from-atomic (self, deque )
    do @check-elt (x, DUMMY_ELT)
    let x : Option.option = D.@pop-new-end-from-atomic (self, deque )
    do @check-elt (x, DUMMY_ELT)
    do D.@push-new-end-from-atomic (self, deque, DUMMY_ELT )
    do D.@push-new-end-from-atomic (self, deque, DUMMY_ELT )
    do D.@push-new-end-from-atomic (self, deque, DUMMY_ELT )
    do D.@push-new-end-from-atomic (self, deque, DUMMY_ELT )
    do D.@push-new-end-from-atomic (self, deque, DUMMY_ELT )
    let isEmpty : bool = D.@is-empty-from-atomic (self, deque)
    do assert (BNot(isEmpty))
    let isFull : bool = D.@is-full-from-atomic (self, deque)
    do assert (isFull)

    do D.@release-from-atomic (self, deque)

    return (UNIT)
  ;

)

val test : unit -> unit = _prim (@test)
val _ = test ()
