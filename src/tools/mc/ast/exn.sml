(* exn.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Support for exceptions in the typechecker and AST.
 *)

structure Exn : sig

  (* the exception type *)
    val exnTyc : Types.tycon

  (* create a new exception constructor. *)
    val new : (Atom.atom * AST.ty option) -> AST.dcon

  (* is this dcon an exception constructor? *)
    val isExn : AST.dcon -> bool

  (* return the list of currently defined exceptions *)
    val listExceptions : unit -> AST.dcon list

  end = struct

    val exnTyc = TyCon.newDataTyc (BasisNames.exn, [])

    val new = DataCon.new exnTyc

  (* is this dcon an exception constructor? *)
    fun isExn dc = TyCon.same (DataCon.ownerOf dc, exnTyc)

    fun listExceptions () =
      (case exnTyc
	of Types.Tyc{def=Types.DataTyc{cons, ...}, ...} => !cons
	 | _ => raise Fail "AbsTyc")

  end
