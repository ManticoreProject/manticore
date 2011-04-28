(* array-seq.pml  
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 *)

structure ArraySeq = struct

  val fail = Fail.fail "ArraySeq"

  structure A = Array

  datatype 'a seq 
    = Empty
    | NonEmpty of 'a A.array

  fun min (m, n) = if m < n then m else n

  val empty = Empty

  fun singleton s = NonEmpty (A.array (1, s))

  fun null s = (case s
    of Empty => true
     | _ => false
    (* end case *))

  fun length s = (case s
    of Empty => 0
     | NonEmpty a => A.length a
    (* end case *))

  fun sub (s, n) = (case s
    of Empty => fail "sub" "Empty"
     | NonEmpty a => A.sub (a, n)
    (* end case *))

  fun tabulate (n, f) =
    if (n=0) then Empty
    else NonEmpty (A.tabulate (n, f))

  fun concat (x, y) = let
    val xn = length x
    val yn = length y
    fun elt i = if (i<xn) then sub(x,i) else sub (y,i-xn)
    in
      tabulate (xn+yn, elt)      
    end

  fun take (s, n) = let
    val len = length s
    in
      if n >= len then s
      else tabulate (n, fn i => sub (s, i))
    end

  fun drop (s, n) = let
    val len = length s
    in
      if n >= len then 
        Empty
      else 
        tabulate (len-n, fn i => sub(s,i+n))
    end

  fun splitAt (s, i) = (take (s, i+1), drop (s, i+1))

  fun fromList xs = 
   (if List.null xs then Empty
    else let
      val len = List.length xs
      val a = A.array (len, List.hd xs)
      fun lp (xs, i) = (case xs
        of nil => NonEmpty a
	 | h::t => (A.update (a, i, h); lp (t, i+1))
        (* end case *))
      in
        lp (xs, 0)
      end)

  fun fromListRev (xs, n) =
  (* I don't know what the int arg "n" is for. I'm just matching the interface *) 
  (* as needed and, for now, forging ahead. -ams *)
    if List.null xs then Empty
    else let
      val len = List.length xs
      val a = A.array (len, List.hd xs)
      fun lp (xs, i) = (case xs
        of nil => NonEmpty a
	 | h::t => (A.update (a, i, h); lp (t, i-1))
        (* end case *))
      in
        lp (xs, len-1)
      end

  fun toList s = (case s
    of Empty => []
     | NonEmpty a => let
         val len = A.length a
	 fun lp (i, acc) = 
           if i < 0 then acc
	   else lp (i-1, A.sub(a,i)::acc)
         in
           lp (len-1, nil)
         end
    (* end case *))

  fun rev s = (case s
    of Empty => Empty
     | NonEmpty a => let
	 val n = A.length a
	 val a' = A.tabulate (n, fn i => A.sub (a, n-1-i))
         in 
	   NonEmpty a'
         end
    (* end case *))

  fun app (f, s) = (case s
    of Empty => ()
     | NonEmpty a => let
         val n = A.length a
         fun f' i = f (A.sub (a, i))
	 fun lp i = if (i>=n) then () 
		    else (f' i; lp (i+1))
         in
           lp 0
         end
    (* end case *))

  fun map (f, s) = (case s
    of Empty => Empty
     | NonEmpty a => let
	 val a' = A.tabulate (A.length a, fn i => f (A.sub(a,i)))
         in
           NonEmpty a'
         end
    (* end case *))

  fun map2 (f, s1, s2) = (case (s1, s2)
    of (Empty, _) => Empty
     | (_, Empty) => Empty
     | (NonEmpty a1, NonEmpty a2) => let
	 fun f' i = f (A.sub (a1, i), A.sub (a2, i))
         in
	   NonEmpty (A.tabulate (min (A.length a1, A.length a2), f'))
         end
    (* end case *))

  fun map2Eq (f, s1, s2) = (case (s1, s2)
    of (Empty, Empty) => Empty
     | (NonEmpty a1, NonEmpty a2) => let
         val n1 = A.length a1
	 val n2 = A.length a2
         in
           if (n1 <> n2) then 
             fail "map2Eq" "unequal lengths (1)"
	   else let
             fun f' i = f (A.sub (a1,i), A.sub (a2, i))
             in
               NonEmpty (A.tabulate (n1, f'))
	     end
         end
     | _ => fail "map2Eq" "unequal lengths (2)"
    (* end case *))

  fun foldr (f, z, s) = (case s
    of Empty => z
     | NonEmpty a => let
         val n = A.length a
         fun lp (i, acc) = 
           if (i<0) then acc
	   else lp (i-1, f (sub(s,i), acc))
         in
	   lp (n-1, z)
         end
    (* end case *))

  fun foldl (f, z, s) = (case s
    of Empty => z
     | NonEmpty a => let
         val n = A.length a
         fun lp (i, acc) = 
           if (i>=n) then acc
	   else lp (i+1, f (sub(s,i), acc))
         in
	   lp (0, z)
         end
    (* end case *))

  fun reduce (oper, unit, s) = foldl (oper, unit, s)

  fun cut (s, n) = (take (s, n), drop (s, n))

  fun filter (pred, s) = (case s
    of Empty => Empty
     | NonEmpty a => let
         val n = length s
         fun lp (i, acc) = 
           if i < 0 then
             fromList acc
           else let
             val x = sub (s, i)
             val acc' = if pred(x) then x::acc else acc
	     in
               lp (i-1, acc')
	     end
          in
            lp (n-1, [])
          end
    (* end case *))

  fun zip (s1, s2) = 
    tabulate (min (length s1, length s2),
	      fn i => (sub (s1, i), sub (s2, i)))			       

  fun unzip s = (case s
    of Empty => (Empty, Empty)
     | NonEmpty a => let
         val n = A.length a
	 val (x, y) = A.sub (a, 0)
	 val xs = A.array (n, x)
	 val ys = A.array (n, y)
         fun lp i = 
           if (i >= n) then (NonEmpty xs, NonEmpty ys)
	   else let
             val (x, y) = A.sub (a, i)
	     val _ = A.update (xs, i, x)
	     val _ = A.update (ys, i, y)
             in
               lp (i+1)
	     end
         in
	   lp 1
         end
    (* end case *))

  fun update (s, i, x) = (case s
    of Empty => fail "update" "Empty"
     | NonEmpty a => (A.update (a, i, x); s)
    (* end case *))

(* short-circuits on finding false *)
  fun all pred s = (case s
    of Empty => true
     | NonEmpty a => let 
         val n = A.length a
	 fun lp i =
           if (i >= n) then true
	   else pred (A.sub (a, i)) andalso lp (i+1)
         in
	   lp 0
         end
    (* end case *))

(* short-circuits on finding true *)
  fun exists pred s = (case s 
    of Empty => false
     | NonEmpty a => let
         val n = A.length a
         fun lp i = 
           if (i >= n) then false
	   else pred (A.sub (a, i)) orelse lp (i+1)
	 in
	   lp 0
         end
    (* end case *))

  fun toArray s = (case s
    of Empty => fail "toArray" "Empty"
     | NonEmpty a => a
    (* end case *)) 

end
