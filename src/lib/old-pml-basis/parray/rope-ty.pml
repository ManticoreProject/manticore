(* rope-ty.pml  
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Polymorphic rope datatype.
 *)

structure RopeTy = struct

  structure Seq = Seq

  datatype 'a rope
    = Leaf of 'a Seq.seq
    | Cat  of int * int * 'a rope * 'a rope

end
