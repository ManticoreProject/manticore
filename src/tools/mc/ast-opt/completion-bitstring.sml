(* completion-bitstring.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

(* A "completion bitstring" is a string of 0s and 1s indicating which computations
 * in a pcase must be finished in order for a given branch to be matched.
 *
 * Consider the expression
 *
 *   pcase f(1) & g(2) & h(3)
 *    of ? & NONE & ? => x
 *     | nil & ? & ? => y
 *     | nil & SOME(false) & 6 => z
 *
 * The completion bitstrings for the three arms of this pcase are
 * 010, 100, and 111 respectively.
 *)

structure CompletionBitstring : sig

  datatype bit = Zero | One
  type t = bit list

  (* some common useful operations *)
  val eq       : t * t -> bool
  val compare  : t * t -> order
  val toString : t -> string

  (* c1 < c2 if everywhere c1 is 1, c2 is 1. *)
  (* If one thinks of c1 and c2 as bit-vector sets, this is the subset relationship. *)
  val sub : t * t -> bool
 
  (* Given a list of ppats, return a bitstring with 0s for the ?s and 1s elsewhere. *)
  val fromPPats : AST.ppat list -> t

  (* allOnes produces a bitstring consisting of all ones of given length. *)
  val allOnes : int -> t

end = struct

  datatype bit = Zero | One

  type t = bit list

  fun bitEq (b1:bit, b2:bit) = (b1=b2)

  fun eq (c1, c2) = 
    if (List.length c1) <> (List.length c2) then
      raise Fail "UnequalLengths"
    else 
      ListPair.all bitEq (c1, c2)
      
  fun toString cb = concat (map (fn Zero => "0" | One => "1") cb)

  (* c1 < c2 if everywhere c1 is 1, c2 is 1. *)
  (* If one thinks of c1 and c2 as bit-vector sets, this is the subset relationship. *)
  fun sub (c1, c2) = let
    fun s ([], []) = true
      | s (One::t1, One::t2) = s (t1, t2)
      | s (Zero::t1, _::t2) = s (t1, t2)
      | s (One::t1, Zero::t2) = false
      | s _ = let
          val c1s = toString c1
	  val c2s = toString c2
	  val msg = "bug: comparing unequal length bitstrings " ^ c1s ^ ", " ^ c2s 
          in
            raise Fail msg
            (* unequal length lists should be screened out below *)
	  end
    in
      if (length c1) <> (length c2) then
        raise Fail "UnequalLengths"
      else s (c1, c2)
    end		   

  fun compare (c1, c2) = 
    if (length c1) <> (length c2) then
      raise Fail "Unequal Lengths"
    else 
      String.compare (toString c1, toString c2)

  (* Given a list of ppats, return a bitstring with 0s for the ?s and 1s elsewhere. *)
  fun fromPPats ps = map (fn AST.NDWildPat _ => Zero | _ => One) ps

  (* allOnes produces a bitstring consisting of all ones of given length. *)
  fun allOnes n = List.tabulate (n, fn _ => One) 

end
