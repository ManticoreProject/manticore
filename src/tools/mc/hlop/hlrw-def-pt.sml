(* hlrw-def-pt.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure HLRWDefPT = struct

     datatype pattern = Call of Atom.atom * pattern list
                      | Var of Atom.atom

     datatype rewrite = Rewrite of { label  : Atom.atom,
                                     lhs    : pattern,
                                     rhs    : pattern,
                                     weight : IntInf.int }

     type file = rewrite list

end (* HLRWDefPT *)
