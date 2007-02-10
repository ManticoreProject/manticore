(* var-fn.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

signature VAR_PARAMS =
  sig
    type kind
    type ty

    val defaultKind : kind

    val kindToString : kind -> string
    val tyToString : ty -> string

  end

signature VAR =
  sig
    type kind
    type ty
    type var = (kind, ty) VarRep.var_rep

    val new : (Atom.atom * ty) -> var
    val newWithKind : (Atom.atom * kind * ty) -> var
    val copy : var -> var

    val nameOf : var -> Atom.atom
    val kindOf : var -> kind
    val setKind : (var * kind) -> unit
    val typeOf : var -> ty

    val useCount : var -> int

    val same : (var * var) -> bool
    val compare : (var * var) -> order
    val hash : var -> word

    val toString : var -> string

  (* per-variable properties *)
    val newProp : (var -> 'a) -> {
	    clrFn : var -> unit,
	    getFn : var -> 'a,
	    peekFn : var -> 'a option,
	    setFn : (var * 'a) -> unit
	  }
    val newFlag : unit -> {
	    getFn : var -> bool,
	    setFn : var * bool -> unit
	  }

    structure Set : ORD_SET where type Key.ord_key = var
    structure Map : ORD_MAP where type Key.ord_key = var
    structure Tbl : MONO_HASH_TABLE where type Key.hash_key = var

  end

functor VarFn (VP : VAR_PARAMS) : VAR =
  struct

    datatype var_rep = datatype VarRep.var_rep

    type kind = VP.kind
    type ty = VP.ty
    type var = (kind, ty) VarRep.var_rep

    fun newWithKind (name, k, ty) = V{
	    name = name,
	    id = Stamp.new(),
	    kind = ref k,
	    useCnt = ref 0,
	    ty = ty,
	    props = PropList.newHolder()
	  }

    fun new (name, ty) = newWithKind (name, VP.defaultKind, ty)

    fun copy (V{name, kind, ty, ...}) = newWithKind (name, !kind, ty)

    fun nameOf (V{name, ...}) = name

    fun kindOf (V{kind, ...}) = !kind
    fun setKind (V{kind, ...}, k) = kind := k

    fun typeOf (V{ty, ...}) = ty

    fun useCount (V{useCnt, ...}) = !useCnt

    fun same (V{id=a, ...}, V{id=b, ...}) = Stamp.same(a, b)
    fun compare (V{id=a, ...}, V{id=b, ...}) = Stamp.compare(a, b)
    fun hash (V{id, ...}) = Stamp.hash id

    fun toString (V{name, id, ...}) = Atom.toString name ^ Stamp.toString id

    fun propsOf (V{props, ...}) = props

  (* per-variable properties *)
    fun newProp mkProp = PropList.newProp (propsOf, mkProp)
    fun newFlag () = PropList.newFlag propsOf

    structure Key =
      struct
	type ord_key = var
	val compare = compare
      end
    structure Map = RedBlackMapFn (Key)
    structure Set = RedBlackSetFn (Key)

    structure Tbl = HashTableFn (struct
	type hash_key = var
	val hashVal = hash
	val sameKey = same
      end)

  end
