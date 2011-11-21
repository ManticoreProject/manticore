(* rope-permute-sig.sml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Operations to permute the elements of a rope
 * 
 * Authors:
 *   Mike Rainey (mrainey@cs.uchicago.edu)
 *   Adam Shaw (ams@cs.uchicago.edu)
 *
 *)

signature ROPE_PERMUTE =
  sig

    type 'a rope

    (* given a destination sequence and an (index, value) pair sequence, write *)
    (* elements from the pair sequence into the destination sequence. for each element*)
    (* (i,v) in the sequence of index-value pairs, the value v is written into position*)
    (* i of the destination sequence *)
    (* e.g., scatter ([1,2,3,4], [(2,5)(3,6)]) ==> [1,2,5,6] *)
    val scatter : 'a rope * (int * 'a) rope -> 'a rope
    (* given a sequence of values and a sequence of indices, which can be different *)
    (* of lengths, return a sequence which is the same length as the indices sequence *)
    (* and of the same type as the values sequence. for each position in the indices *)
    (* sequence it reads the value at that index in the values sequence. *)
    (* e.g., gather ([0,1,2,3,4,5,6,7], [3,5,2,6]) ==> [3,5,2,6] *)
    val gather : 'a rope * int rope -> 'a rope
    (* given a sequence and an integer, rotate the sequence around by n positions to the right *)
    (* if the integer is negative, the sequence is rotated to the left. *)
    (* e.g., rotate ([1,2,3,4,5,6,7], 3) ==> [5,6,7,1,2,3,4] *)
    val rotate : 'a rope * int -> 'a rope

  end
