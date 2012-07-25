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
  val length   : t -> int
  val compare  : t * t -> order
  val invert   : t -> t
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

  fun bitEq (b1: bit, b2: bit) : bool = (b1=b2)

  fun eq (c1: t, c2: t) : bool = let
    fun lp (b::bs, c::cs) = bitEq (b, c) andalso lp (bs, cs)
      | lp (_::_, []) = raise Fail "unequal lengths"
      | lp ([], _::_) = raise Fail "unequal lengths"
      | lp ([], []) = true
    in
      lp (c1, c2) (* note: ListPair.allEq returns false on unequal lengths *)
    end
      
  val length : t -> int = List.length

  val toString : t -> string = String.concat o (List.map (fn Zero => "0" | One => "1"))

(* c1 < c2 if everywhere c1 is 1, c2 is 1. *)
(* If one thinks of c1 and c2 as bit-vector sets, this is the subset relationship. *)
  fun sub (c1: t, c2: t) : bool = let
    fun s ([],       [])       = true
      | s (One::t1,  Zero::t2) = false
      | s (One::t1,  One::t2)  = s (t1, t2)
      | s (Zero::t1, _::t2)    = s (t1, t2)
      | s (_::_,     [])       = raise Fail "unequal lengths"
      | s ([],       _::_)     = raise Fail "unequal lengths"
    in
      s (c1, c2)
    end		   

(* lexicographic order comparison *)
  fun compare (c1: t, c2: t) : order = 
    if (length c1) <> (length c2) then
      raise Fail "Unequal Lengths"
    else 
      String.compare (toString c1, toString c2)

(* Given a list of ppats, return a bitstring with 0s for the ?s and 1s elsewhere. *)
  val fromPPats : AST.ppat list -> t = List.map (fn AST.NDWildPat _ => Zero | _ => One)

(* allOnes produces a bitstring consisting of all ones of given length. *)
  fun allOnes (n: int) : t = List.tabulate (n, fn _ => One) 

(* toggle all the 0s and 1s *)
  val invert : t -> t = List.map (fn One => Zero | Zero => One)

end
