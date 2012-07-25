(* set-once-mem.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Set-once synchronous memory. Because this implementation spin waits, client code must
 * guarantee that the thunk quickly evaluates to completion.
 *)

#define EMPTY_CELL    $0
#define LOCKED_CELL   $1

#define STATE_OFF  0
#define THUNK_OFF  1

structure SetOnceMem =
  struct

    structure PT = PrimTypes

    _primcode(

      typedef thunk = fun (PT.unit / PT.exh -> any);

      (* set-once memory cell; a cell follows the protocol below
       * 
       * -new-> EMPTY_CELL -set-> LOCKED_CELL -> INITIALIZED --\
       *                                                ^      |
       *                                                \-set-|
       *)
      typedef set_once_mem = ![any, thunk];

    (* take an initializer thunk, and create a set-once memory cell *)
      define @new(init : thunk / exh : PT.exh) : set_once_mem =
	let c : set_once_mem = alloc(EMPTY_CELL, init)
	let c : set_once_mem = promote(c)
	return(c)
      ;

    (* obtain the value of the set-once cell; NOTE: this code spin waits .*)
      define @get (c : set_once_mem / exh : PT.exh) : any =
	fun getIt (/ exh : PT.exh) : any = 
	    if Equal(SELECT(STATE_OFF, c), EMPTY_CELL)
	       then (* see if we need to set the cell *)
		    let oldValue : any = CAS(&STATE_OFF(c), EMPTY_CELL, LOCKED_CELL)
		    if Equal(oldValue, EMPTY_CELL)
		       then (* it's our job to set the cell *)
                            let init : thunk = SELECT(THUNK_OFF, c)
			    let v : any = apply init(UNIT / exh)
			    let v : any = promote(v)
			    do UPDATE(STATE_OFF, c, v)
			    return(v)
		    else (* another fiber has set the cell; spin until the value is set *)
			 apply getIt(/exh)
	    else if Equal(SELECT(STATE_OFF, c), LOCKED_CELL)
		    then apply getIt(/exh)
	    else return(SELECT(STATE_OFF, c))
	apply getIt(/exh)
      ;
    )

  end
