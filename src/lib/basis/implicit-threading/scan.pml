(* scan.pml  
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Implementation of parallel prefix + scan, for now.
 * More scans coming.
 *
 * Follows the algorithms in Blelloch's "Prefix Sums and their Applications."
 * Adapted for ropes here.
 * (Nov 1990, CMS-CS-90-190)
 *)

structure Scan = struct

    structure S = ListSeq
    structure R = Ropes

    datatype option = datatype Option.option

    type 'a seq = 'a S.seq

  (* We need a rope that can store a datum at every node. *)
  (* This is different from our other rope type. *)
  (* That datum will be an accumulator. *)
    datatype 'a scan_rope
      = Cat of ('a *      (* datum *)
		int *     (* depth *)
		int *     (* length *)
		'a scan_rope * (* left subtree *)
		'a scan_rope)  (* right subtree *)
      | Leaf of ('a *     (* datum *)
		 int *    (* length *)
		 'a seq)  (* data *)

  (* ***** UTILITIES ***** *)

  (* failwith : string -> 'a *)
  (* using this for the moment so we can observe the exception message at runtime *)
    fun failwith msg = (Print.printLn msg; (raise Fail msg))

  (* ***** prefix+ ***** *)
    fun datumOf x =
     (case x
        of (Cat (d, _, _, _, _)) => d
	 | (Leaf (d, _, _)) => d
       (* end case *))
  
  (* seqscan : int -> int seq -> int seq * int *)
  (* Does a prefix scan starting from the given seed value. *)
  (* Returns the scanned sequence and the total. *)
    fun seqscan seed seq = let
      fun lp (nums, acc) =
           (case (nums, acc)
	      of (n::nil, k::_) => (S.fromList (List.rev acc), k+n)
	       | (n::ns, k::acc) => lp (ns, k+n::k::acc)
	       | (nil, _) => failwith "BUG: sequence emptied out"
	       | (_, nil) => failwith "BUG")
      val l = S.toList seq
      in
        case l
          of nil => (S.empty, 0)
	   | _ => lp (l, seed::nil)
      end

  (* seqsum : Num 'a => 'a seq -> 'a *)
    fun seqsum s = let
      fun plus (a, b) = a + b
      in
        S.foldl plus 0 s
      end

  (* upsweep : R.rope -> scan_rope *)
    fun upsweep t = let
      fun lp r = 
       (case r 
	  of (R.LEAF (len, s)) => Leaf (seqsum s, len, s)
	   | (R.CAT (d, len, rL, rR)) => let
               val (uL, uR) = (| lp rL, lp rR |)
               in 
                 Cat (datumOf uL + datumOf uR, d, len, uL, uR)
	       end)
      in
        lp t
      end   

    fun downsweep t = let
      (* FIXME It seems odd that I'm underscoring the datums here... *)
      (* ...think about this more. *)
      fun lp (c, r) =
       (case r
          of (Cat (_, d, len, cL, cR)) => let
               val nL = datumOf cL
	       in
                 R.CAT (| d, len, lp (c, cL), lp (c+nL, cR) |)
               end
	   | (Leaf (_, len, s)) => let
               val (scanned, _) = seqscan c s
               in
                 R.LEAF (len, scanned)
               end)
      in
        lp (0, t)
      end
(*
  (* toString : ('a -> string) -> 'a rope -> string *)
    fun toString show r = let
      fun copies thing n = List.tabulate (n, fn _ => thing)
      val rootString = "C<"
      val spaces = copies " "
      val indenter = String.concat (spaces (String.size rootString))
      val indent = List.map (fn s => indenter ^ s) 
      fun build r =
       (case r
	 of LEAF (_, xs) => let 
              fun b args = 
               (case args
	         of (nil, acc) => "]" :: acc
		  | (x::nil, acc) => b (nil, show x :: acc)
		  | (x::xs, acc) => b (xs, "," :: show x ::acc)
	         (* end case *))
              in
		(String.concat(List.rev(b (S.toList xs, ("["::nil))))) :: nil
              end
	  | CAT (_, _, r1, r2) => let 
              val ss1 = build r1
	      val ss2 = build r2
	      in
	        (indent ss1) @ (rootString :: (indent ss2))
	      end	
         (* end case *))
      in
        String.concatWith "\n" (build r @ ("\n"::nil))
      end
*)

  fun prefixPlus r = downsweep (upsweep r)

  end
