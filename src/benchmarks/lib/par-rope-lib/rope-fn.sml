(* rope-fn.sml
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
 * We have based our implementation on the paper "Ropes: An Alternative to Strings" 
 * by Boehm et al. (1995). See rope-fn.sml for more detailed commentary.
 *)

functor RopeFn (

    structure S : SEQ

    val maxLeafSize : int

    structure RT : RUNTIME

  ) :> ROPE = struct

    structure S = S
    type 'a seq = 'a S.seq

    structure R = RopeImplFn (
                    structure S = S
		    val maxLeafSize = maxLeafSize
		    structure RT = RT)

    datatype rope = datatype R.rope

    val maxLeafSize = R.maxLeafSize

    val empty = R.empty
    val isEmpty = R.isEmpty
    val isLeaf = R.isLeaf
    val length = R.length
    val depth = R.depth

    val append = R.append
    val sub = R.sub
    val update = R.update
    val delete = R.delete
    val take = R.take
    val drop = R.drop
    val splitAt = R.splitAt
    val cut = R.cut

    val concat = R.concat
    val rev = R.rev
    val map = R.map
    val mapPartial = R.mapPartial
    val reduce = R.reduce
    val foldl = R.foldl
    val foldli = R.foldli
    val foldr = R.foldr
    val foldri = R.foldri
    val filter = R.filter
    val partition = R.partition
    val app = R.app
    val appi = R.appi
    val find = R.find
    val exists = R.exists
    val all = R.all

    val fromSeq = R.fromSeq
    val toSeq = R.toSeq
    val fromList = R.fromList
    val toList = R.toList
    val fromString = R.fromString

    val singleton = R.singleton
    val tabulate = R.tabulate
	    
    val toString = R.toString

    val randomRope = R.randomRope

    structure Pair = R.Pair
    structure Scan = R.Scan
    structure Permute = R.Permute

  end
