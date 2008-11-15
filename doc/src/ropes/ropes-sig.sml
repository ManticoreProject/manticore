(* ropes-sig.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * A prototype implementation of ropes in SML.
 *)

signature ROPES = 
  sig

    structure S : SEQ

    type 'a rope
    type 'a seq = 'a S.seq

  (* O(c)-time operations *)

    val maxLeafSize : int
    val empty       : 'a rope
    val isEmpty     : 'a rope -> bool
    val isLeaf      : 'a rope -> bool
    val length      : 'a rope -> int
    val depth       : 'a rope -> int

  (* O(log n)-time operations *)
  
    val concat      : 'a rope * 'a rope -> 'a rope
    val sub         : 'a rope * int -> 'a
  (* given a rope r and an index i, split the rope into r[0, ..., i] and r[i+1, ..., length r] *)
    val splitAt     : 'a rope * int -> ('a rope * 'a rope)

  (* O(n)-time operations *)

    val toSeq      : 'a rope -> 'a seq
    val fromSeq    : 'a seq -> 'a rope

  end
