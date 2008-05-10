(* program-parse-tree.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Parse-tree representation of Manticore + inline BOM programs.
 *)

structure ProgramParseTree =
  struct

  (* a term marked with a source-map span *)
    type 'a mark = {span : Error.span, tree : 'a}
  (* qualified names *)
    type 'a path = (Atom.atom list * 'a) mark

  (* parse tree with variables represented as qualified names *)
    structure PML1 = PMLParseTreeFn (
            type 'a mark = 'a mark
            type ty_binder = Atom.atom
	    type ty_use = Atom.atom path
	    type con_binder = Atom.atom
	    type con_use = Atom.atom path
	    type var_binder = Atom.atom
	    type var_use = Atom.atom path
	    type op_id = Atom.atom
	    type mod_binder = Atom.atom
	    type mod_use = Atom.atom path
	    type sig_id = Atom.atom)

    structure Var = VarFn (
           struct
	     type kind = unit
	     type ty = unit
	     val defaultKind = ()
	     fun kindToString _ = ""
	     fun tyToString _ = ""
           end)
    type var = (unit, unit) VarRep.var_rep

 (* parse tree with variables represented as AST variables *)
    structure PML2 = PMLParseTreeFn (
            type 'a mark = 'a mark
            type ty_binder = var
	    type ty_use = var
	    type con_binder = var
	    type con_use = var
	    type var_binder = var
	    type var_use = var
	    type op_id = var
	    type mod_binder = var
	    type mod_use = var
	    type sig_id = var)

  end (* ProgramParseTree *)
