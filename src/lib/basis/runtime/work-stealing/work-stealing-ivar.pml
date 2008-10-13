(* work-stealing-ivar.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Synchronous memory for work stealing.
 *)

#define BLOCKED_LIST_OFF           0
#define VALUE_OFF                  1
#define SPIN_LOCK_OFF              2

#define EMPTY_VAL              enum(0)

structure WorkStealingIVar 
(*:
  sig

      type 'a ivar
	 
      val ivar : unit -> 'a ivar
      val get : 'a ivar -> 'a
      val put : ('a ivar * 'a) -> unit

  end *) = struct

    structure PT = PrimTypes
    structure FLS = FiberLocalStorage

    _primcode (
         typedef ivar2 =
              ![
		 List.list,      (* list of blocked fibers *)
		 any,            (* value *)
		 int             (* spin lock *)
	      ];
    )

(* FIXME: try renaming ivar2 to ivar *)
    type 'a ivar = _prim (ivar2)

    _primcode (

    (* create an ivar. 
     * NOTE: we postpone promoting the ivar until the "put" operation. this strategy has strong 
     * synergy with the software-polling version of work stealing. for this implementation, we can
     * avoid promoting the ivar in the common case, and pay for the promotion only when a steal occurs.
     *)
      define @ivar ( / exh : PT.exh) : ivar =
	let x : ivar = alloc (nil, enum(0):any, 0)
	return (x)
      ;  
 
      define @get (ivar : ivar / exh : PT.exh) : any =
	let readFlag : int = I32FetchAndAdd(&SPIN_LOCK_OFF(ivar), 1)
	let value : any = SELECT(VALUE_OFF, ivar)
	if Equal(value, EMPTY_VAL)
	   then
	      cont k (x : any) = return (x)
	      fun loop () : () = 
		 let blocks : List.list = SELECT(BLOCKED_LIST_OFF, ivar)
		 let newBlocks : List.list = List.CONS(k, blocks)
		 let newBlocks : List.list = promote(newBlocks)
		 let blocked : any = CAS(&BLOCKED_LIST_OFF(ivar), blocks, newBlocks)
		 if Equal(blocked, blocks)
		   then return () 
		   else apply loop ()
	      do apply loop()
	      let x : int = I32FetchAndAdd(&SPIN_LOCK_OFF(ivar), ~1)
	      let x : PT.unit = Control.@stop(/ exh)
	      return(value)
	else
	    let x : int = I32FetchAndAdd(&SPIN_LOCK_OFF(ivar), ~1)
	    return (value)
      ;

      define @put (ivar : ivar, x : any / exh : PT.exh) : () = 
        let ivar : ivar = promote(ivar)
	let x : any = promote((any)x)
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
      (* push any blocked fibers on the local deque *)
	fun push (blockedK : any / exh : PT.exh) : () =
	    let blockedK : cont(any) = (cont(any))blockedK
	    cont k (unt : PT.unit) = throw blockedK(x)
            let x : PT.unit = Control.@unblock(k, Option.NONE / exh)
            return()
  	do PrimList.@app(push, blocked / exh)
	return()
      ;

      define @put-wrap (arg : [ivar, any] / exh : PT.exh) : PT.unit =
	do @put(#0(arg), #1(arg) / exh)
	return(UNIT)
      ;

      define @ivar-wrap (unt : PT.unit / exh : PT.exh) : ivar =
	@ivar(/ exh)
      ;

    )

    val ivar : unit -> 'a ivar = _prim(@ivar-wrap)
    val get : 'a ivar -> 'a = _prim(@get)
    val put : ('a ivar * 'a) -> unit = _prim(@put-wrap)

  end
