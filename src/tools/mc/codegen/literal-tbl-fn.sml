(* literal-tbl-fn.sml
 * 
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Hash tables that map literals to labels.
 *)

signature LITERAL_TABLE =
  sig
    
    type lit                 (* representation of the literal *)
    type lit_tbl             (* hash table mapping literals to labels *)

    val new : unit -> lit_tbl
    val addLit : (lit_tbl * lit) -> Label.label
    val appi : ((lit * Label.label) -> unit) -> lit_tbl -> unit    

  end (* LITERAL_TABLE *)

functor LiteralTblFn (
    A : sig
	type lit
	val labelPrefix : string
	val hash : lit -> word
	val same : (lit * lit) -> bool
      end
  ) : LITERAL_TABLE = struct

    structure Tbl = HashTableFn (struct
	type hash_key = A.lit
	val hashVal = A.hash
	val sameKey = A.same
      end)

    type lit = A.lit
    type lit_tbl = Label.label Tbl.hash_table

    val newLabel = Label.label A.labelPrefix

    fun new () = Tbl.mkTable (32, Fail "LiteralTable")

  fun addLit (tbl, lit) = (case Tbl.find tbl lit
	 of NONE => let
	      val lab = newLabel()
	      in
		Tbl.insert tbl (lit, lab);
		lab
	    end
	  | (SOME lab) => lab
	(* end case *))

    val appi = Tbl.appi

  end (* LiteralTblFn *)
