(* meta-var.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Based on CMSC 22610 Sample code (Winter 2007)
 *
 * Meta-variable utility code.
 *)

structure MetaVar : sig

  (* create a fresh meta variable at the given lambda depth *)
    val new : int -> Types.meta

  (* are two meta-variables the same? *)
    val same : Types.meta * Types.meta -> bool

  (* return true if the first meta-variable is bound at a deeper lambda depth *)
    val isDeeper : Types.meta * Types.meta -> bool

  (* instantiate a meta variable; raises Fail if already instantiated *)
    val instantiate : Types.meta * Types.ty -> unit

  (* finite maps on meta variables *)
    structure Map : ORD_MAP where type Key.ord_key = Types.meta

  end = struct

    datatype meta = datatype Types.meta

  (* create a fresh meta variable *)
    fun new d = MVar{info = ref(Types.UNIV d), stamp = Stamp.new()}

  (* are two meta-variables the same? *)
    fun same (MVar{stamp=a, ...}, MVar{stamp=b, ...}) = Stamp.same(a, b)

  (* return true if the first meta-variable is bound at a deeper lambda depth *)
    fun isDeeper (MVar{info=ref(Types.UNIV d1), ...}, MVar{info=ref(Types.UNIV d2), ...}) =
	  d1 > d2
      | isDeeper _ = raise Fail "isDeeper"

  (* instantiate a meta variable; raises Fail if already instantiated *)
    fun instantiate (MVar{info, ...}, ty) = (case !info
	   of Types.INSTANCE _ =>  raise Fail "instantiate"
	    | _ => info := Types.INSTANCE ty
	  (* end case *))

    structure Map = RedBlackMapFn (
      struct
	type ord_key = meta
	fun compare (MVar{stamp = a, ...}, MVar{stamp = b, ...}) = Stamp.compare(a, b)
      end)

  end
