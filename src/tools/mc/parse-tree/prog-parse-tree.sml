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

    fun qidToString ({tree=(prefix, p), ...} : qid) : string = 
      String.concatWith "." (List.map Atom.toString (prefix @ [p]))

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
	type sig_id = Atom.atom
	val var_bindToString = Atom.toString
	val var_useToString = qidToString
	val ty_bindToString = Atom.toString
	val ty_useToString = qidToString)

    structure Var = struct
      local
	  structure V = VarFn (
            struct
	      type kind = unit
	      type ty = unit
	      val defaultKind = ()
	      fun kindToString _ = ""
	      fun tyToString _ = ""
	    end)
      in
      open V
      val {getFn = getErrorStream  : var -> Error.err_stream option, setFn = setErrorStream, ...} 
        = newProp (fn _ => NONE)
      end
    end

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
	type sig_id = var
	val var_bindToString = Var.toString
	val var_useToString = Var.toString
	val ty_bindToString = Var.toString
	val ty_useToString = Var.toString)

  end (* ProgramParseTree *)
