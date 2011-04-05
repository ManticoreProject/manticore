(* ty-con.sml
ame *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Based on CMSC 22610 Sample code (Winter 2007)
 *
 * Utility operations on type constructors.
 *)

structure TyCon : sig

  (* create a new abstract type constructor *)
    val newAbsTyc : (Atom.atom * int * bool) -> Types.tycon

  (* create a new datatype tyc; it will have an empty constructor list *)
    val newDataTyc : (Atom.atom * AST.tyvar list) -> Types.tycon

  (* return the name of a type constructor *)
    val nameOf : Types.tycon -> Atom.atom

  (* return the stamp of a type constructor *)
    val stampOf : Types.tycon -> Stamp.stamp

  (* return a string representation *)
    val toString : Types.tycon -> string

  (* return true if two type constructors are the same *)
    val same : Types.tycon * Types.tycon -> bool

  (* compare two type constructors *)
    val compare : Types.tycon * Types.tycon -> order

  (* return the arity of a type constructor *)
    val arityOf : Types.tycon -> int

  (* dictionaries keyed by type constructors *)
    structure Map : ORD_MAP where type Key.ord_key = Types.tycon

  (* hash tables keyed by type constructors *)
    structure Tbl : MONO_HASH_TABLE where type Key.hash_key = Types.tycon

  (* per-type properties *)
    val newProp : (Types.tycon -> 'a) -> {
	    clrFn : Types.tycon -> unit,
	    getFn : Types.tycon -> 'a,
	    peekFn : Types.tycon -> 'a option,
	    setFn : (Types.tycon * 'a) -> unit
	  }
    val newFlag : unit -> {
	    getFn : Types.tycon -> bool,
	    setFn : Types.tycon * bool -> unit
	  }

  (* equality type property *)
    val isEqTyc : Types.tycon -> bool
    val markEqTyc : Types.tycon -> unit

  end = struct

    datatype tycon = datatype Types.tycon

  (* per-type properties *)
    fun propsOf (Tyc{props, ...}) = props
    fun newProp mkProp = PropList.newProp (propsOf, mkProp)
    fun newFlag () = PropList.newFlag propsOf

  (* equality type property.  This property is set for abstract types when they
   * are defined and for datatypes after their constructors are checked (see
   * typechecker.sml).
   *)
    local
      val {getFn, setFn} = newFlag ()
    in
    val isEqTyc = getFn
    fun markEqTyc tyc = setFn(tyc, true)
    end

    fun newTyc (name, arity, params, def) = Tyc{
	    name = name,
	    stamp = Stamp.new(),
	    arity = arity,
	    params = params,
	    props = PropList.newHolder(),
	    def = def
	  }

  (* create a new abstract type constructor *)
    local
      val params = Vector.fromList(List.map Atom.atom ["'a", "'b", "'c", "'d", "'e"])
    in
    fun newAbsTyc (name, arity, eq) = let
	  fun mkParam i = TyVar.new(Vector.sub(params, i))
	  val tyc = newTyc (name, arity, List.tabulate(arity, mkParam), Types.AbsTyc)
	  in
	    if eq then markEqTyc tyc else ();
	    tyc
	  end
    end

  (* create a new datatype tyc; it will have an empty constructor list *)
    fun newDataTyc (name, params) = 
	  newTyc (name, List.length params, params, Types.DataTyc{nCons = ref 0, cons = ref[]})

  (* return the name of a type constructor *)
    fun nameOf (Tyc{name, ...}) = name

(* FIXME: should include type parameters! *)
    fun toString (Tyc{name, stamp, ...}) = (Atom.toString name) ^ (Stamp.toString stamp)

    fun stampOf (Tyc{stamp, ...}) = stamp

  (* return true if two type constructors are the same *)
    fun same (tyc1, tyc2) = Stamp.same(stampOf tyc1, stampOf tyc2)

  (* compare two type constructors *)
    fun compare (tyc1, tyc2) = Stamp.compare(stampOf tyc1, stampOf tyc2)

  (* return the arity of a type constructor *)
    fun arityOf (Tyc{arity, ...}) = arity


    structure Map = BinaryMapFn (
      type ord_key = tycon
      val compare = compare)

    structure Tbl = HashTableFn (
      struct
	type hash_key = tycon
	fun hashVal (Tyc{stamp, ...}) = Stamp.hash stamp
	val sameKey = same
      end)

  end
