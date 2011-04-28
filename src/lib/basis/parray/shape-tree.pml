(* shape-tree.pml  
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * A implementation of nesting trees and flattened arrays in Manticore.
 *)

structure ShapeTree = struct

  val fail = Fail.fail "ShapeTree"

  (* ***** NESTING TREES ***** *)

  (* The shape tree datatype and some basic operations. *)
    datatype shape_tree
      = Lf of int * int (* lower bound inclusive, upper bound exclusive *)
      | Nd of shape_tree list

  (* same : shape_tree * shape_tree -> bool *)
    fun same (t1, t2) = (case (t1, t2)
      of (Lf (lo1, hi1), Lf (lo2, hi2)) => (lo1 = lo2) andalso (hi1 = hi2)
       | (Nd ts1, Nd ts2) => ListPair.allEq same (ts1, ts2)
       | _ => false
      (* end case *))

  (* minIdx : shape_tree -> int *)
    fun minIdx nt = (case nt 
      of Lf (i, _) => i
       | Nd ts => minIdx (List.hd ts)
      (* end case *))

  (* maxIdx : shape_tree -> int *)
    fun maxIdx nt = (case nt
      of Lf (_, i) => i
       | Nd ts => let
           fun lp ts = (case ts
             of t::nil => maxIdx t
	      | _::tl  => lp tl
	      | nil => fail "maxIdx" "empty Nd"
             (* end case *))
	   in
	     lp ts
	   end
     (* end case *))

  (* span : shape_tree -> int * int *)
  (* returns lower bound incl, upper bound excl *)
    fun span t = (minIdx t, maxIdx t)
                
  (* incrBy : int -> shape_tree -> shape_tree *)
    fun incrBy i = let
      fun incr nt = (case nt
        of Lf (lo, hi) => Lf (lo+i, hi+i)
	 | Nd ts => Nd (List.map (incrBy i) ts)
        (* end case *))
      in
	incr
      end

    val itos = Int.toString

  (* toString : shape_tree -> string *)
    fun toString t = 
     (case t
       of Lf (lo, hi) => 
	    String.concat ["Lf(", itos lo, ",", itos hi, ")"]
	| Nd (ts) => let
            val s = String.concatWith "," (List.map toString ts)
            in
              String.concat ["Nd[", s, "]"]
            end
       (* end case *))

end
