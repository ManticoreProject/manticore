(* ropes-sig.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Ropes for Standard ML.
 * 
 * Authors:
 *   Mike Rainey (mrainey@cs.uchicago.edu)
 *   Adam Shaw (ams@cs.uchicago.edu)
 *
 * We have based our implementation on the original paper by Boehm et. al.

@article{bap:ropes,
    author = {Hans-J. Boehm and Russ Atkinson and Michael Plass},
    title = {Ropes: an alternative to strings},
    journal = {Software---Practice \& Experience},
    volume = 25,
    number = 12,
    year = 1995,
    issn = {0038-0644},
    pages = {1315--1330},
    publisher = {John Wiley \& Sons, Inc.},
    address = {New York} 
    }

 *)

signature ROPES = 
  sig

    structure S : SEQ

    type 'a rope
    type 'a seq = 'a S.seq

    val maxLeafSize : int

  (* constant-time operations *)

    val empty       : 'a rope
    val isEmpty     : 'a rope -> bool
    val isLeaf      : 'a rope -> bool
    val length      : 'a rope -> int
    val depth       : 'a rope -> int

  (* amortized log-time operations; the time is O(n log n) when rebalancing is necessary. *)
  
    val concat      : 'a rope * 'a rope -> 'a rope
    val sub         : 'a rope * int -> 'a
  (* given a rope r and an index i, split the rope into r[0, ..., i] and r[i+1, ..., length r] *)
    val splitAt     : 'a rope * int -> ('a rope * 'a rope)

    val take       : 'a rope * int -> 'a rope
    val drop       : 'a rope * int -> 'a rope
  (* cut the rope r into r[0, ..., n-1] and r[n, ..., length r] *)
    val cut        : 'a rope * int -> 'a rope * 'a rope

  (* linear-time operations *)

    val toSeq      : 'a rope -> 'a seq
    val fromSeq    : 'a seq -> 'a rope

    val rev        : 'a rope -> 'a rope
    val map        : ('a -> 'b) -> 'a rope -> 'b rope
    val foldl      : ('a * 'b -> 'b) -> 'b -> 'a rope -> 'b
    val foldr      : ('a * 'b -> 'b) -> 'b -> 'a rope -> 'b
    val filter     : ('a -> bool) -> 'a rope -> 'a rope

  end
