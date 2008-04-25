(* mlb-parse-tree.sml
 *
 * COPYRIGHT (c) 2008 John Reppy (http://www.cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Parse-tree representation of MLB files.
 *)

structure MLBParseTree =
  struct

    type id = Atom.atom

  (* a term marked with a source-map span *)
    type 'a mark = {span : Error.span, tree : 'a}

    datatype bas_exp
      = MarkBasExp of bas_exp mark
      | DecBasExp of bas_dec                       (* basic *)
      | IdBasExp of id                             (* basis identifier *)
      | LocalDeclBasExp of (bas_dec * bas_exp)     (* local declaration *)

    and bas_dec
      = MarkBasDec of bas_dec mark
      | BasisBasDec of bas_bind                    (* basis *)
      | LocalBasDec of (bas_dec * bas_dec)         (* local *)
      | OpenBasDec of bas_dec list                 (* open n >= 1 *)
      | StructureBasDec of mod_bind                (* basis structure binding *)
      | SignatureBasDec of mod_bind                (* basis signature binding *)
      | FunctorBasDec of mod_bind                  (* basis functor binding *)
      | SeqBasDec of bas_dec list                  (* sequential *)
      | ImportBasDec of id                         (* import ML basis or source *)

  (* binds bases *)
    and bas_bind
      = MarkBasBind of bas_bind mark
      | BindBasBind of (id * bas_exp) list

  (* binds structures, signatures and functors *)
    and mod_bind
      = MarkModBind of mod_bind mark
      | BindModBind of (id * id) list

  end (* MLBParseTree *)
