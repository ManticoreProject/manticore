(* ft-ty-con.sml 
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Based on CMSC 22610 Sample code (Winter 2007)
 *
 * Utility operations on type constructors.
 *)

structure FTTyCon : sig

  (* create a new abstract type constructor *)
    val newAbsTyc : (Atom.atom * int * bool * Types.tycon) -> FTTypes.tycon

  (* create a new datatype tyc; it will have an empty constructor list *)
    val newDataTyc : (Atom.atom * FLAST.tyvar list * Types.tycon) -> FTTypes.tycon

  (* return the name of a type constructor *)
    val nameOf : FTTypes.tycon -> Atom.atom

  (* return the stamp of a type constructor *)
    val stampOf : FTTypes.tycon -> Stamp.stamp

  (* return a string representation *)
    val toString : FTTypes.tycon -> string

  (* return true if two type constructors are the same *)
    val same : FTTypes.tycon * FTTypes.tycon -> bool

  (* compare two type constructors *)
    val compare : FTTypes.tycon * FTTypes.tycon -> order

  (* return the arity of a type constructor *)
    val arityOf : FTTypes.tycon -> int

  (* dictionaries keyed by type constructors *)
    structure Map : ORD_MAP where type Key.ord_key = FTTypes.tycon

  (* hash tables keyed by type constructors *)
    structure Tbl : MONO_HASH_TABLE where type Key.hash_key = FTTypes.tycon

  (* per-type properties *)
    val newProp : (FTTypes.tycon -> 'a) -> {
	    clrFn : FTTypes.tycon -> unit,
	    getFn : FTTypes.tycon -> 'a,
	    peekFn : FTTypes.tycon -> 'a option,
	    setFn : (FTTypes.tycon * 'a) -> unit
	  }
    val newFlag : unit -> {
	    getFn : FTTypes.tycon -> bool,
	    setFn : FTTypes.tycon * bool -> unit
	  }

  (* equality type property *)
    val isEqTyc : FTTypes.tycon -> bool
    val markEqTyc : FTTypes.tycon -> unit

  end = struct

    datatype tycon = datatype FTTypes.tycon

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

    fun newTyc (name, arity, params, def, interface) = Tyc {
	    name = name,
	    stamp = Stamp.new(),
	    arity = arity,
	    params = params,
	    props = PropList.newHolder(),
	    def = def,
	    interface = interface
	  }

  (* create a new abstract type constructor *)
    local
      val params = Vector.fromList(List.map Atom.atom ["'a", "'b", "'c", "'d", "'e"])
    in
      fun newAbsTyc (name, arity, eq, interface) = let
        fun mkParam i = TyVar.new(Vector.sub(params, i))
        val params = List.tabulate (arity, mkParam)
	val a = FTTypes.AbsTyc
	val tyc = newTyc (name, arity, params, a, interface)
        in
	  if eq then markEqTyc tyc else ();
	  tyc
	end
    end (* local *)

  (* create a new datatype tyc; it will have an empty constructor list *)
    fun newDataTyc (name, params, interface) = let
      val pLen = List.length params
      val d = FTTypes.DataTyc {nCons = ref 0, cons = ref []}
      in
        newTyc (name, pLen, params, d, interface)
      end

  (* return the name of a type constructor *)
    fun nameOf (Tyc{name, ...}) = name

(* FIXME: should include type parameters! *)
    fun toString (Tyc{name, stamp, ...}) = Atom.toString name

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
