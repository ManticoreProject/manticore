(* ft-synth-ops.sml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Our flattening transformation includes type-indexed families of operators.
 * All these operators may be statically synthesized out of simple components.
 * This module performs the synthesis.
 * Supporting documents in 
 * /path/to/manti-papers/papers/notes/amsft
 *)

structure FTSynthOps = struct

  structure F = FLAST
  structure T = FTTypes

  datatype oper
    = Cat
    | Map of oper
    | Unzip
    | ID
    | Compose of oper * oper
    | CrossCompose of oper list

  fun mk (r : T.repr_ty) : oper = 
   (case r
      of T.FunTy (t1, t2) => ID
       | T.TupleTy ts => Compose (CrossCompose (List.map mk ts), Unzip)
       | T.FlatArrayTy (t', n) => Compose (Cat, Map (mk t'))
       | T.ConTy _ => if isGround r then ID else raise Fail "todo"
       | _ => raise Fail "todo")

  and isGround _ = raise Fail "todo"

  fun flatten (r : T.ty) : F.exp = raise Fail "todo"

end
