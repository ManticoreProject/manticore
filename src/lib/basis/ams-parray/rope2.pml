(* rope2.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Ropes with a pair of sequences at each leaf.
 * These could be the target of flattening a rope of tuples.
 *)

(* FIXME UNFINISHED *)

structure Rope2 = struct

  val fail = Fail.fail "Rope2"

  structure R = Rope

  structure S = Rope.S (* VectorSeq *) (* ArraySeq *) (* ListSeq *)

  datatype option = datatype Option.option

  type 'a seq = 'a S.seq

(* ***** UTILITIES ***** *)

(* ***** ROPES ***** *)

  datatype ('a, 'b) rope2
    = CAT2 of (int * (* depth *)
	       int * (* length *)
	       ('a, 'b) rope2 *
	       ('a, 'b) rope2)
    | LEAF2 of (int * (* length *)
		'a seq *
		'b seq)

  val maxLeafSize = MaxLeafSize.sz

  val empty = LEAF2 (0, S.empty, S.empty)

  fun isLeaf r =
   (case r
      of LEAF2 _ => true
       | _ => false)

  fun isBalanced r =
   (case r
      of LEAF2 _ => true
       | CAT2 (depth, len, _, _) => (depth <= Int.ceilingLg len + 2)
     (* end case *))

(* singleton : 'a * 'b -> ('a, 'b) rope2 *)
  fun singleton (x, y) = LEAF2 (1, S.singleton x, S.singleton y)

(* length : 'a rope2 -> int *)
  fun length r =
   (case r
      of LEAF2 (n, _, _) => n
       | CAT2 (_, n, _, _) => n
     (* esac *))

(* isEmpty : 'a rope2 -> bool *)
  fun isEmpty r = (length r = 0)

(* depth : 'a rope2 -> int *)
(* The depth of a leaf is 0. *)
  fun depth r =
   (case r
      of LEAF2 _ => 0
       | CAT2 (depth, _, _, _) => depth
     (* esac *))

(* inBounds : 'a rope * int -> bool *)
(* Is the given int a valid index of the given rope? *)
  fun inBounds (r, i) = i >= 0 andalso i < length r

(* subInBounds : ('a, 'b) rope * int -> 'a * 'b *)
(* pre: inBounds (r, i) *)
  fun subInBounds (r, i) = 
   (case r
      of LEAF2 (_, s1, s2) => (S.sub(s1,i), S.sub(s2,i))
       | CAT2 (depth, len, r1, r2) =>
	   if i < length r1 then 
             subInBounds(r1, i)
	   else 
             subInBounds(r2, i - length r1)       
     (* esac *))

(* sub : ('a, 'b) rope * int -> 'a * 'b *)
(* subscript; returns r[i] *)
  fun sub (r, i) = 
    if inBounds (r, i) 
    then subInBounds(r, i)
    else fail "sub" "subscript out of bounds"

(* mapP : ('a * 'b -> 'c) -> ('a, 'b) rope2 -> 'c rope *)
(* post : the output has the same shape as the input *)
  fun mapP (f, rope) = let
    fun m r =
     (case r
        of LEAF2 (len, s1, s2) => R.LEAF (len, S.map2 (f, s1, s2))
	 | CAT2 (dpt, len, r1, r2) => R.CAT (| dpt, len, m r1, m r2 |)
       (* esac *))
    in
      m rope
    end
          
end
