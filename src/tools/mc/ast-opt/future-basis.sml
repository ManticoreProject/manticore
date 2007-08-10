(* future-basis.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * This file includes definitions of future, touch, and cancel, for use
 * in transformations of intermediate languages.
 *)

structure FutureBasis (* : sig

  end *) =

  struct
  
    structure A = AST
    structure T = Types
    
    val tv = TyVar.new (Atom.atom "'a")
	     
    val futureTyc = TyCon.newAbsTyc (Atom.atom "future", 1, false)
		    
    (* future : AST.exp -> AST.exp *)
    fun future e =
  
  end
