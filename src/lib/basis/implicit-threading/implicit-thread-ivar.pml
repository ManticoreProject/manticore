(* implicit-thread-ivar.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Synchronous, write-once memory for implicit threads.
 *)

#define BLOCKED_LIST_OFF           0
#define VALUE_OFF                  1
#define SPIN_LOCK_OFF              2

#define EMPTY_VAL                  $0

structure ImplicitThreadIVar = 
  struct

    structure PT = PrimTypes

    _primcode (

       typedef ivar = ![
	         List.list,      (* list of blocked implicit threads *)
	         any,            (* value *)
	         int             (* spin lock *)
	        ];

    (* create an ivar. 
     * NOTE: we postpone promoting the ivar until the "put" operation. this strategy has strong 
     * synergy with the software-polling version of work stealing. for this implementation, we can
     * avoid promoting the ivar in the common case, and pay for the promotion only when a steal occurs.
     *)
      define @ivar (v : any / exh : exh) : ivar =
	let x : ivar = alloc (nil, v, 0)
	return (x)
      ;

      define @empty-ivar (/ exh : exh) : ivar =
	@ivar(EMPTY_VAL / exh)
      ;

      define @get (ivar : ivar / exh : exh) : any =
(*
FIXME: add "isPromoted" primop to the compiler.
NOTE: supposing we have inLocalHeap, 
        if Not(isPromoted(ivar))
	   then return(SELECT(VALUE_OFF, ivar))
	else 
*)
        let ivar : ivar = promote(ivar)
        let vp : vproc = SchedulerAction.@atomic-begin()
	let readFlag : int = I32FetchAndAdd(&SPIN_LOCK_OFF(ivar), 1)
	let value : any = SELECT(VALUE_OFF, ivar)
	if Equal(value, EMPTY_VAL)
	   then
	      cont k (x : unit) = return(SELECT(VALUE_OFF, ivar))
	      let thd : ImplicitThread.thread = ImplicitThread.@capture(k / exh)
	      fun loop () : () = 
		 let blocks : List.list = SELECT(BLOCKED_LIST_OFF, ivar)
		 let newBlocks : List.list = List.CONS(thd, blocks)
		 let newBlocks : List.list = promote(newBlocks)
		 let blocked : any = CAS(&BLOCKED_LIST_OFF(ivar), blocks, newBlocks)
		 if Equal(blocked, blocks)
		   then return () 
		   else apply loop ()
	      do apply loop()
	      let x : int = I32FetchAndAdd(&SPIN_LOCK_OFF(ivar), ~1)
	      let x : PT.unit = SchedulerAction.@stop-from-atomic(vp)
	      let e : exn = Fail(@"ImplicitThreadIVar.@get: impossible")
              throw exh(e)
	else
	    let x : int = I32FetchAndAdd(&SPIN_LOCK_OFF(ivar), ~1)
            do SchedulerAction.@atomic-end(vp)
	    return (value)
      ;

      define @put (ivar : ivar, x : any / exh : exh) : () = 
        let ivar : ivar = promote(ivar)
	let x : any = promote((any)x)
        let vp : vproc = SchedulerAction.@atomic-begin()
	let oldValue : any = CAS (&VALUE_OFF(ivar), EMPTY_VAL, x)
      (* wait for blocking fibers *)
	let blocked : List.list = 
	   if Equal(oldValue, EMPTY_VAL)
	      then
		fun spin () : () =
		    if I32Eq (SELECT(SPIN_LOCK_OFF,ivar), 0)
		       then return ()
		       else apply spin ()
		do apply spin ()
		let blockedFibers : List.list = SELECT(BLOCKED_LIST_OFF, ivar)
		return(blockedFibers)
	   else
	       do assert(NotEqual(oldValue, EMPTY_VAL))
	       return(nil)
        fun unblock (thd : ImplicitThread.thread / exh : exh) : () =
	    ImplicitThread.@spawn(thd / exh)
        do PrimList.@app(unblock, blocked / exh)
        do SchedulerAction.@atomic-end(vp)
        return()
      ;

      define @poll (ivar : ivar / exh : exh) : Option.option =
	if Equal(SELECT(VALUE_OFF, ivar), EMPTY_VAL)
	   then
	    return (Option.NONE)
	else
	    return (Option.SOME(SELECT(VALUE_OFF, ivar)))
      ;

    )

  end
