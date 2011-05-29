(* shape.pml  
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * A implementation of nesting trees and flattened arrays in Manticore.
 *)

structure Shape = struct

  val fail = Fail.fail "Shape"

  (* ***** NESTING TREES ***** *)

  (* The shape tree datatype and some basic operations. *)
    datatype shape
      = Lf of int * int (* lower bound inclusive, upper bound exclusive *)
      | Nd of shape list

  (* same : shape * shape -> bool *)
    fun same (t1, t2) = (case (t1, t2)
      of (Lf (lo1, hi1), Lf (lo2, hi2)) => (lo1 = lo2) andalso (hi1 = hi2)
       | (Nd ts1, Nd ts2) => ListPair.allEq same (ts1, ts2)
       | _ => false
      (* end case *))

  (* minIdx : shape -> int *)
    fun minIdx nt = (case nt 
      of Lf (i, _) => i
       | Nd ts => minIdx (List.hd ts)
      (* end case *))

  (* maxIdx : shape -> int *)
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

  (* span : shape -> int * int *)
  (* returns lower bound incl, upper bound excl *)
    fun span t = (minIdx t, maxIdx t)
                
  (* incrBy : int -> shape -> shape *)
    fun incrBy i = let
      fun incr nt = (case nt
        of Lf (lo, hi) => Lf (lo+i, hi+i)
	 | Nd ts => Nd (List.map (incrBy i) ts)
        (* end case *))
      in
	incr
      end

    val itos = Int.toString

  (* toString : shape -> string *)
    fun toString t = (case t
      of Lf (lo, hi) => 
	   String.concat ["Lf(", itos lo, ",", itos hi, ")"]
       | Nd (ts) => let
           val s = String.concatWith "," (List.map toString ts)
           in
             String.concat ["Nd[", s, "]"]
           end
      (* end case *))

  (* buildNode : shape list -> shape *)
  (* Collects shapes together into a node, adjusting indices as needed. *)
  (* ex: [Lf(0,1),Lf(0,1)] --> Nd[Lf(0,1),Lf(1,2)] *)
  (* ex: [Lf(0,2),Lf(0,3),Lf(0,1)] --> Nd[Lf(0,2),Lf(2,5),Lf(5,6)] *)
  (* ex: [Nd[Lf(0,1)],Nd[Lf(0,1),Lf(1,2)]] --> Nd[Nd[Lf(0,1)],Nd[Lf(1,2),Lf(2,3)]] *)
  (* It is expected, but not checked, that *)
  (* - each shape in the list has min index 0, and *)
  (* - each shape in the list is of the same depth. *)
    fun buildNode shapes = let
      fun lp ss = (case ss
        of (s1::s2::t) => let
             val s2' = incrBy (maxIdx s1) s2
             in
               s1::(lp (s2'::t))
             end
         | _ => ss
        (* end case *))
      in
        Nd (lp shapes)
      end

  (* regularShape : (int * int * int) list -> shape *)
  (* Computes the shape given triples representing dimensions in a regular array. *)
    fun regularShape triples = let
      fun copy (n, x) = List.tabulate (n, fn i => x)
      fun lp ts = (case ts
        of nil => fail "regularShape" "empty arg"
	 | t::nil => Lf (0, Range.nElts t)
	 | t::ts => let
             val s = lp ts
             val n = Range.nElts t
             in
	       buildNode (copy (n, s))
             end)
      in
        lp triples
      end

end
