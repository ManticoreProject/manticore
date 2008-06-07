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
	type ty_binder = Atom.atom
	type ty_use = qid
	type con_binder = Atom.atom
	type con_use = qid
	type var_binder = Atom.atom
	type var_use = qid
	type op_id = Atom.atom
	type mod_binder = Atom.atom
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

  (* BOM parse tree with variables represented as qualified names *)
    structure BOM1 = BOMParseTreeFn (
        type var = qid
	type ty_con = qid
	type ty_def = qid
	type prim = Atom.atom
	type dcon = qid
	type pml_var = qid)

  (* BOM parse tree with variables represented flat names *)
    structure BOM2 = BOMParseTreeFn (
        type var = var
	type ty_con = var
	type ty_def = var
	type prim = var
	type dcon = var
	type pml_var = var)

  end (* ProgramParseTree *)
