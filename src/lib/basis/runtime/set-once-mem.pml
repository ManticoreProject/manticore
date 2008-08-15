#define EMPTY_CELL    $0
#define LOCKED_CELL   $1

structure SetOnceMem =
  struct

    (* set-once synchronous memory
     *   type 'a set_once_mem
     * create a set-once memory cell
     *   val new : unit -> 'a set_once_mem
     * take a set-once variable and an initializer function, and return
     * the contents of the memory cell. the initializer function gets
     * evaluated the first time only.
     *   val set : ('a set_once_mem * (unit -> 'a)) -> 'a
     *)

  structure PT = PrimTypes

_primcode(

  (* set-once memory cell; a cell follows the protocol below
   * 
   * -new-> EMPTY_CELL -set-> LOCKED_CELL -> INITIALIZED --\
   *                                                ^      |
   *                                                \-set-|
   *)
  typedef set_once_mem = ![any];

  define @new(x : PT.unit / exh : PT.exh) : set_once_mem =
    let c : set_once_mem = alloc(EMPTY_CELL)
    let c : set_once_mem = promote(c)
    return(c)
  ;

  define @set(c : set_once_mem, init : fun(PT.unit / PT.exh -> any) / exh : PT.exh) : any =
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
                else (* another fiber has set the cell *)
		     apply getIt(/exh)
        else if Equal(#0(c), LOCKED_CELL)
                then apply getIt(/exh)
        else return(#0(c))
    apply getIt(/exh)
  ;
)

  end
