(* set-once-mem.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Set-once synchronous memory.
 *)

#define EMPTY_CELL    $0
#define LOCKED_CELL   $1

structure SetOnceMem =
  struct

    structure PT = PrimTypes

    _primcode(

      (* set-once memory cell; a cell follows the protocol below
       * 
       * -new-> EMPTY_CELL -set-> LOCKED_CELL -> INITIALIZED --\
       *                                                ^      |
       *                                                \-set-|
       *)
      typedef set_once_mem = ![any];

    (* create a set-once memory cell *)
      define @new(x : PT.unit / exh : PT.exh) : set_once_mem =
	let c : set_once_mem = alloc(EMPTY_CELL)
	let c : set_once_mem = promote(c)
	return(c)
      ;

    (* take a set-once variable and an initializer function, and return the contents of the memory
     * cell. the initializer function gets evaluated the first time only. 
     *)
      define @set (c : set_once_mem, init : fun(PT.unit / PT.exh -> any) / exh : PT.exh) : any =
	fun getIt (/ exh : PT.exh) : any = 
	    if Equal(#0(c), EMPTY_CELL)
	       then (* see if we need to set the cell *)
		    let oldValue : any = CAS(&0(c), EMPTY_CELL, LOCKED_CELL)
		    if Equal(oldValue, EMPTY_CELL)
		       then (* it's our job to set the cell *)
			    let v : any = apply init(UNIT / exh)
			    let v : any = promote(v)
			    do #0(c) := v
			    return(v)
		    else (* another fiber has set the cell; spin until the value is set *)
			 apply getIt(/exh)
	    else if Equal(#0(c), LOCKED_CELL)
		    then apply getIt(/exh)
	    else return(#0(c))
	apply getIt(/exh)
      ;
    )

  end
