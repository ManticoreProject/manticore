(* nesting-tree-types.sml
 *
 * COPYRIGHT (c) 2010 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Types for the flattening transformation.
 * Supporting documents in 
 * /path/to/manti-papers/papers/notes/amsft
 *)
 
structure NestingTreeTypes = struct

  datatype ty
    = Lf
    | Nd of ty

(* same : ty * ty -> bool *)
  fun same (Lf, Lf) = true
    | same (Nd t, Nd u) = same (t, u)
    | same _ = false

(* toString : ty -> string *)
  fun toString Lf = "Lf"
    | toString (Nd t) = "Nd(" ^ toString t ^ ")"

(* toNat : ty -> int *)
  fun toNat Lf = 0
    | toNat (Nd n) = 1 + toNat n

(* fromNat : int -> ty *)
(* pre: int argument non-negative *)
  fun fromNat n =
    if n < 0 then raise Fail "fromNat"
    else let 
      fun lp i = if i=0 then Lf else Nd (lp (i-1))
      in
        lp n
      end

(* deeper : ty * ty -> bool *)
  fun deeper (n, n') = (toNat n > toNat n')

end
