(* bom-tyc.sml
 *
 * COPYRIGHT (c) 2015 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * BOM type constructors.
 *)

structure BOMTyc : sig

    type t

  (* the tyc's name *)
    val nameOf : t -> string

  (* unique string rep *)
    val toString : t -> string

  (* return the arity of the tyc *)
    val arityOf : t -> int

  (* is a tyc a datatype? *)
    val isDataTyc : t -> bool

  (* return the constructors of a datatype ([] for non-datatype tycs) *)
    val consOf : t -> BOMRep.data_con list

  (* curried equality test *)
    val isTyc : t -> t -> bool

  (* are two tycs the same? *)
    val same : t * t -> bool

  (* compare two tycs *)
    val compare : t * t -> order

  (* hash value for a tyc *)
    val hash : t -> word

  (* properties and flags *)
    val newProp : (t -> 'a) -> {
            clrFn : t -> unit,          (* remove the property from the tyc *)
            getFn : t -> 'a,            (* get the property from the tyc *)
            peekFn : t -> 'a option,    (* does the tyc have the property? *)
            setFn : t * 'a -> unit      (* set the property *)
          }
    val newFlag : unit -> {
            getFn : t -> bool,  	(* get the flag value *)
            setFn : t * bool -> unit    (* set the flag value; false removes it *)
          }

  (* builtin type constructors *)
    val arrayTyc : t
    val vectorTyc : t
    val addrTyc : t
    val vprocTyc : t

  end = struct

    datatype t = datatype BOMRep.tyc

    fun new (name, arity) = Tyc{
	    name = name,
	    stamp = Stamp.new(),
	    arity = arity,
	    cons = ref[],
	    props = PropList.newHolder()
	  }

    fun nameOf (Tyc{name, ...}) = name
    fun toString (Tyc{name, stamp, ...}) = name ^ Stamp.toString stamp
    fun arityOf (Tyc{arity, ...}) = arity
    fun isDataTyc (Tyc{cons, ...}) = null(!cons)
    fun consOf (Tyc{cons, ...}) = !cons

    fun isTyc (Tyc{stamp, ...}) (Tyc{stamp=stamp', ...}) = Stamp.same(stamp, stamp')

    fun same (Tyc{stamp, ...}, Tyc{stamp=stamp', ...}) = Stamp.same(stamp, stamp')

    fun compare (Tyc{stamp, ...}, Tyc{stamp=stamp', ...}) = Stamp.compare(stamp, stamp')

    fun hash (Tyc{stamp, ...}) = Stamp.hash stamp

    fun newProp initVal = PropList.newProp (fn (Tyc{props, ...}) => props, initVal)

    fun newFlag () = PropList.newFlag (fn (Tyc{props, ...}) => props)

  (* builtin type constructors *)
    val arrayTyc = new ("array", 1)
    val vectorTyc = new ("vector", 1)
    val addrTyc = new ("addrOf", 1)
    val vprocTyc = new ("vproc", 0)

  end
