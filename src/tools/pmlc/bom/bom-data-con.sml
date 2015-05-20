(* bom-data-con.sml
 *
 * COPYRIGHT (c) 2015 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure BOMDataCon : sig

    type t

    type tyc = BOMRep.tyc

  (* create a new data constructor for the tyc *)
    val new : tyc -> (string * BOMRep.ty list) -> t

  (* the constructor's name *)
    val nameOf : t -> string

  (* unique string rep *)
    val toString : t -> string

  (* return the datatype that this constructor belongs to *)
    val tycOf : t -> tyc

  (* return the domain of the type constructor; nil for nullary constructors *)
    val domainOf : t -> BOMRep.ty list

  (* return the range of the type constructor *)
    val rangeOf : t -> BOMRep.ty

  (* are two constructors the same? *)
    val same : t * t -> bool

  (* compare two data constructors *)
    val compare : t * t -> order

  (* compute a hash of a data constructor *)
    val hash : t -> word

  (* per constructor properties *)
    val newProp : (t -> 'a) -> {
	    clrFn : t -> unit,
	    getFn : t -> 'a,
	    peekFn : t -> 'a option,
	    setFn : (t * 'a) -> unit
	  }
    val newFlag : unit -> {
	    getFn : t -> bool,
	    setFn : t * bool -> unit
	  }
  
  end = struct

    datatype t = datatype BOMRep.data_con

    datatype tyc = datatype BOMRep.tyc

  (* create a new data constructor for the tyc *)
    fun new (tyc as Tyc{cons, ...}) (name, argTy) = let
	  val dc = DCon{
		  name = name,
		  stamp = Stamp.new(),
		  argTy = argTy,
		  myTyc = tyc,
		  props = PropList.newHolder()
		}
	  in
(* perhaps we should sort this constructors by name? *)
	    cons := !cons @ [dc];
	    dc
	  end

  (* the constructor's name *)
    fun nameOf (DCon{name, ...}) = name

    fun toString (DCon{name, stamp, ...}) = name

    fun tycOf (DCon{myTyc, ...}) = myTyc
    fun domainOf (DCon{argTy, ...}) = argTy
    fun rangeOf (DCon{myTyc, ...}) = BOMTy.T_Con(myTyc, [])

    fun same (DCon{stamp=a, ...}, DCon{stamp=b, ...}) = Stamp.same(a, b)
    fun compare (DCon{stamp=a, ...}, DCon{stamp=b, ...}) = Stamp.compare(a, b)
    fun hash (DCon{stamp, ...}) = Stamp.hash stamp

    fun newProp initVal = PropList.newProp (fn (DCon{props, ...}) => props, initVal)

    fun newFlag () = PropList.newFlag (fn (DCon{props, ...}) => props)

  end
