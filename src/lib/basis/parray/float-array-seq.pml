(* float-array-seq.pml  
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure FloatArraySeq = struct

  val fail = Fail.fail "FloatArraySeq"

  structure A = FloatArray

  type seq = A.array

  fun tabulate (_, f : int -> float) = fail "tabulate" "todo"

  fun sum (a : A.array) : float = fail "sum" "todo"

  fun prefixPlusScan (f : float, a : A.array) : A.array = fail "prefixPlusScan" "todo"

  val empty = A.array (0, 0.0)

  fun singleton s = A.array (1, s)

  fun null s = (A.length s = 0)

  val length = A.length

  fun sub (s, n) = A.sub (s, n)

  fun concat (x, y) = let
    val xn = length x
    val yn = length y
    fun elt i =
      if i < xn then
        sub (x, i)
      else
	sub (y, i-xn)
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
      if n >= len then empty
      else tabulate (len-n, fn i => sub (s, i+n))
    end

  fun splitAt (s, i) = (take (s, i+1), drop (s, i+1))

  fun fromList xs = 
    if List.null xs then empty
    else let
      val len = List.length xs
      val x = List.hd xs
      val a = A.array (len, x)
      fun lp (xs, i) = 
        if List.null xs then a
	else let
          val h = List.hd xs
          val t = List.tl xs
          val _ = A.update (a, i, h)
          in
            lp (t, i+1)
          end
    in
      lp (xs, 0)
    end

  fun toList s = let
    val len = length s
    fun lp (i, acc) = 
      if i < 0 then acc
      else lp (i-1, A.sub(s,i)::acc)
    in
      lp (len-1, nil)
    end

  fun rev s = 
    if null s then empty
    else let
      val len = length s
      val x = sub (s, 0)
      val a = A.array (len, x)
      fun lp (up, down) = 
        if up >= len then a
	else let
          val _ = A.update (a, down, A.sub (s, up))
          in
            lp (up+1, down-1)
          end
      in
        lp (0, len-1)
      end

(* map : (float -> float) * seq -> seq *)
  fun map (f, s) = 
    if null s then empty
    else let
      val len = length s
      val init = f (sub (s, 0))
      val b = A.array (len, init)
      fun lp i =
        if i >= len then b
        else let
          val _ = A.update (b, i, f (A.sub (s, i)))
          in
            lp (i+1)
          end
      in
        lp 1 (* we already did 0 *)
      end

(* mapPoly : (float -> 'a) * seq -> 'a Array.seq *)
  fun mapPoly (f, s) =
    if null s then 
      fail "mapPoly" "null s"
    else let
      val len = length s
      val init = f (sub (s, 0))
      val b = Array.array (len, init)
      fun lp i =
        if i >= len then b
        else let
          val _ = Array.update (b, i, f (A.sub (s, i)))
          in
            lp (i+1)
          end
      in
        lp 1 (* we already did 0 *)
      end

  fun map2 (f, s1, s2) = 
    if null s1 then s1
    else if null s2 then s2
    else let
      fun min (m, n) = if m < n then m else n
      val minlen = min (length s1, length s2)
      val init = f (sub (s1, 0), sub (s2, 0))
      val b = A.array (minlen, init)
      fun lp i =
        if i >= minlen then b
        else let
          val _ = A.update (b, i, f (sub (s1, i), sub (s2, i)))
          in
            lp (i+1)
	  end
      in
        lp 1 (* already did 0 *)
      end

  fun foldr (f, z, s) = let
    val len = length s
    fun lp (i, acc) = 
      if i < 0 then acc
      else lp (i-1, f (sub (s,i), acc))
    in
      lp (len-1, z)
    end       

  fun foldl (f, z, s) = let
    val len = length s
    fun lp (i, acc) =
      if i >= len then acc
      else lp (i+1, f (sub(s,i), acc))
    in
      lp (0, z)
    end       

  fun reduce (oper, unit, s) = foldl (oper, unit, s)

  fun cut (s, n) = (take (s, n), drop (s, n))

  fun filter (pred, s) =
    if null s then empty
    else let
      val len = length s
      fun lp (i, acc) = 
        if i < 0 then
          fromList acc
        else let
          val x = sub (s, i)
          in
            if pred x then lp (i-1, x::acc)
	    else lp (i-1, acc) 
	  end
      in
        lp (len-1, nil)
      end

  val tabulate = tabulate

(*
  fun zip (s1, s2) = let
    fun min (m, n) = if m < n then m else n
    val len = min (length s1, length s2)
    in
      if len = 0 then empty 
      else let
        val init = (sub(s1,0), sub(s2,0))
	val b = A.array (len, init)
        fun lp i = 
          if i >= len then b
	  else let
            val pair = (sub(s1,i), sub(s2,i))
	    val _ = A.update (b, i, pair)
            in
              lp (i+1)
            end
        in
          lp 1 (* did 0 already *)
        end
    end
*)
(*
  fun unzip s = 
    if null s then (empty, empty)
    else let
      val len = length s
      val (x, y) = sub (s, 0)
      val a = A.array (len, x)
      val b = A.array (len, y)
      fun lp i = 
        if i >= len then (a, b)
	else let
          val (x, y) = sub (s, i)
          val _ = A.update (a, i, x)
	  val _ = A.update (b, i, y)
          in
            lp (i+1)
          end
      in
        lp 1 (* did 0 already *)
      end
*)

  val update = A.update

(* short-circuits on finding false *)
  fun any pred s = let
    val len = length s
    fun lp i =
      if i >= len then true
      else pred (sub (s, i)) andalso lp (i+1)
    in
      lp 0
    end

(* short-circuits on finding true *)
  fun exists pred s = let
    val len = length s
    fun lp i =
      if i >= len then false
      else pred (sub (s, i)) orelse lp (i+1)
    in
      lp 0
    end

(* concatList : seq list -> seq *)
  fun concatList ss = let
    fun plus (a, b) = a + b
    val lens = List.map length ss
    val totalLen = List.foldr plus 0 lens
    val a = A.array (totalLen, 0.0)
    fun copy (from, into, start) = let
      val len = length from
      fun lp i = 
        if i >= len then ()
	else let
          val _ = A.update (into, i+start, A.sub(from,i))
          in
            lp (i+1)
	  end
      in
	lp 0
      end
    fun copyAll (arrs, lens, start) = 
     (case (arrs, lens)
        of (nil, nil) => ()
	 | (s::ss, n::ns) => let
             val _ = copy (s, a, start)
             in
	       copyAll (ss, ns, start+n)
             end
	 | _ => fail "concatList.copyAll" "bug"
        (* end case *))
    in
      copyAll (ss, lens, 0)
    end

(* prefixPlusScan : float * seq -> seq *)
  val prefixPlusScan = prefixPlusScan

(* sum : seq -> float *)
  val sum = sum

end
