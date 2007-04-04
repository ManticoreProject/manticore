(* meta-var.sml
 *
 * COPYRIGHT (c) 2007 John Reppy (http://www.cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Based on CMSC 22610 Sample code (Winter 2007)
 *
 * Meta-variable utility code.
 *)

structure MetaVar : sig

  (* create a fresh meta variable at the given lambda depth *)
    val new : int -> AST.meta

  (* are two meta-variables the same? *)
    val same : AST.meta * AST.meta -> bool

  (* return true if the first meta-variable is bound at a deeper lambda depth *)
    val isDeeper : AST.meta * AST.meta -> bool

  (* instantiate a meta variable; raises Fail if already instantiated *)
    val instantiate : AST.meta * AST.ty -> unit

  (* finite maps on meta variables *)
    structure Map : ORD_MAP where type Key.ord_key = AST.meta

  end = struct

    datatype meta = datatype AST.meta

  (* create a fresh meta variable *)
    fun new d = MVar{info = ref(AST.UNIV d), stamp = Stamp.new()}

  (* are two meta-variables the same? *)
    fun same (MVar{stamp=a, ...}, MVar{stamp=b, ...}) = Stamp.same(a, b)

  (* return true if the first meta-variable is bound at a deeper lambda depth *)
    fun isDeeper (MVar{info=ref(AST.UNIV d1), ...}, MVar{info=ref(AST.UNIV d2), ...}) =
	  d1 > d2
      | isDeeper _ = raise Fail "isDeeper"

  (* instantiate a meta variable; raises Fail if already instantiated *)
    fun instantiate (MVar{info, ...}, ty) = (case !info
	   of AST.UNIV _ => info := AST.INSTANCE ty
	    | _ => raise Fail "instantiate"
	  (* end case *))

    structure Map = RedBlackMapFn (
      struct
	type ord_key = meta
	fun compare (MVar{stamp = a, ...}, MVar{stamp = b, ...}) = Stamp.compare(a, b)
      end)

  end
