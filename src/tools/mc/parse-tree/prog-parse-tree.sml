(* program-parse-tree.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Parse-tree representation of Manticore + inline BOM programs.
 *)

structure ProgramParseTree =
  struct

  (* qualified names *)
    type 'a path = (Atom.atom list * 'a)  Error.mark

    type qid = Atom.atom path

  (* PML parse tree with variables represented as qualified names *)
    structure PML1 = PMLParseTreeFn (
	type ty_bind = Atom.atom
	type ty_use = qid
	type con_bind = Atom.atom
	type con_use = qid
	type var_bind = Atom.atom
	type var_use = qid
	type op_id = Atom.atom
	type mod_bind = Atom.atom
	type mod_use = qid
	type sig_id = Atom.atom)

    structure Var = VarFn (
      struct
	type kind = unit
	type ty = unit
	val defaultKind = ()
	fun kindToString _ = ""
	fun tyToString _ = ""
      end)
    type var = Var.var

 (* PML parse tree with variables represented as flat names *)
    structure PML2 = PMLParseTreeFn (
	type ty_bind = var
	type ty_use = var
	type con_bind = var
	type con_use = var
	type var_bind = var
	type var_use = var
	type op_id = var
	type mod_bind = var
	type mod_use = var
	type sig_id = var)

  end (* ProgramParseTree *)
