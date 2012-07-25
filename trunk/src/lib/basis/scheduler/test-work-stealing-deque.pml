structure D = WorkStealingDeque
structure A = Array

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

extern int M_NumDequeRoots (void *) __attribute__((pure));
extern void *M_AddDequeEltsToRoots (void*, void*);

define @check-roots (self : vproc, deque : D.deque / exh : exh) : () =
    let nRoots : int = ccall M_NumDequeRoots (deque)
    let nElts : int = D.@num-elts (deque)
    do assert (I32Eq (nRoots, nElts))

    let x : A.array = A.@array (1000, enum(0):any / exh)
    let x2 : A.array = ccall M_AddDequeEltsToRoots (self, x)

    return ()
  ;

define @test (x : unit / exh : exh) : unit =
    let self : vproc = host_vproc

    let workGroupId : long = 1000000:long

    let deque : D.deque = D.@new-in-atomic (self, workGroupId, 3)

    do D.@push-new-end-in-atomic (self, deque, DUMMY_ELT )
    let x : Option.option = D.@pop-new-end-in-atomic (self, deque )
    do @check-elt (x, DUMMY_ELT)
    do @check-roots (self, deque / exh)
    do D.@push-new-end-in-atomic (self, deque, DUMMY_ELT )
    let x : Option.option = D.@pop-new-end-in-atomic (self, deque )
    do @check-roots (self, deque / exh)
    do @check-elt (x, DUMMY_ELT)
    let isEmpty : bool = D.@is-empty-in-atomic (self, deque)
    do assert (isEmpty)

    do D.@push-new-end-in-atomic (self, deque, DUMMY_ELT2 )
    do D.@push-new-end-in-atomic (self, deque, DUMMY_ELT )
    do @check-roots (self, deque / exh)
    let x : Option.option = D.@pop-new-end-in-atomic (self, deque )
    do @check-elt (x, DUMMY_ELT)
    do D.@push-new-end-in-atomic (self, deque, DUMMY_ELT )
    let x : Option.option = D.@pop-new-end-in-atomic (self, deque )
    do @check-elt (x, DUMMY_ELT)
    let x : Option.option = D.@pop-new-end-in-atomic (self, deque )
    do @check-elt (x, DUMMY_ELT2)
    let isEmpty : bool = D.@is-empty-in-atomic (self, deque)
    do @check-roots (self, deque / exh)
    do assert (isEmpty)


    do D.@push-new-end-in-atomic (self, deque, DUMMY_ELT )
    do D.@push-new-end-in-atomic (self, deque, DUMMY_ELT2 )
    let x : Option.option = D.@pop-old-end-in-atomic (self, deque )
    do @check-roots (self, deque / exh)
    do @check-elt (x, DUMMY_ELT)
    do D.@push-new-end-in-atomic (self, deque, DUMMY_ELT )
    let x : Option.option = D.@pop-old-end-in-atomic (self, deque )
    do @check-elt (x, DUMMY_ELT2)
    let x : Option.option = D.@pop-old-end-in-atomic (self, deque )
    do @check-elt (x, DUMMY_ELT)
    let isEmpty : bool = D.@is-empty-in-atomic (self, deque)
    do assert (isEmpty)

    do D.@push-new-end-in-atomic (self, deque, DUMMY_ELT )
    do D.@push-new-end-in-atomic (self, deque, DUMMY_ELT )
    let x : Option.option = D.@pop-old-end-in-atomic (self, deque )
    do @check-elt (x, DUMMY_ELT)
    do D.@push-new-end-in-atomic (self, deque, DUMMY_ELT )
    let x : Option.option = D.@pop-new-end-in-atomic (self, deque )
    do @check-elt (x, DUMMY_ELT)
    let x : Option.option = D.@pop-old-end-in-atomic (self, deque )
    do @check-elt (x, DUMMY_ELT)
    let isEmpty : bool = D.@is-empty-in-atomic (self, deque)
    do assert (isEmpty)

    do D.@push-new-end-in-atomic (self, deque, DUMMY_ELT )
    do D.@push-new-end-in-atomic (self, deque, DUMMY_ELT )
    let x : Option.option = D.@pop-new-end-in-atomic (self, deque )
    do @check-elt (x, DUMMY_ELT)
    do D.@push-new-end-in-atomic (self, deque, DUMMY_ELT )
    let x : Option.option = D.@pop-old-end-in-atomic (self, deque )
    do @check-elt (x, DUMMY_ELT)
    let x : Option.option = D.@pop-new-end-in-atomic (self, deque )
    do @check-elt (x, DUMMY_ELT)
    let isEmpty : bool = D.@is-empty-in-atomic (self, deque)
    do assert (isEmpty)

    do D.@push-new-end-in-atomic (self, deque, DUMMY_ELT )
    let x : Option.option = D.@pop-old-end-in-atomic (self, deque )
    do @check-elt (x, DUMMY_ELT)
    do D.@push-new-end-in-atomic (self, deque, DUMMY_ELT )
    let x : Option.option = D.@pop-new-end-in-atomic (self, deque )
    do @check-elt (x, DUMMY_ELT)
    do assert (isEmpty)
(*
    let deque : D.deque = D.@double-size-in-atomic (self, workGroupId, deque)
    do D.@push-new-end-in-atomic (self, deque, DUMMY_ELT )
    do D.@push-new-end-in-atomic (self, deque, DUMMY_ELT )
    let x : Option.option = D.@pop-new-end-in-atomic (self, deque )
    do @check-elt (x, DUMMY_ELT)
    do D.@push-new-end-in-atomic (self, deque, DUMMY_ELT )
    let x : Option.option = D.@pop-old-end-in-atomic (self, deque )
    do @check-elt (x, DUMMY_ELT)
    let x : Option.option = D.@pop-new-end-in-atomic (self, deque )
    do @check-elt (x, DUMMY_ELT)
    do D.@push-new-end-in-atomic (self, deque, DUMMY_ELT )
    do D.@push-new-end-in-atomic (self, deque, DUMMY_ELT )
    do D.@push-new-end-in-atomic (self, deque, DUMMY_ELT )
    do D.@push-new-end-in-atomic (self, deque, DUMMY_ELT )
    do D.@push-new-end-in-atomic (self, deque, DUMMY_ELT )
    do @check-roots (self, deque / exh)
    let isEmpty : bool = D.@is-empty-in-atomic (self, deque)
    do assert (BNot(isEmpty))
    let isFull : bool = D.@is-full-in-atomic (self, deque)
    do assert (isFull)
*)
    do D.@release-in-atomic (self, deque)

    return (UNIT)
  ;

)

val test : unit -> unit = _prim (@test)
val _ = test ()
