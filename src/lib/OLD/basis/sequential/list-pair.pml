(* list-pair.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure ListPair = struct

(* zip : 'a list * 'b list -> ('a * 'b) list *)
  fun zip (xs, ys) = let
    fun lp (xs, ys, acc) =
     (case (xs, ys)
        of (nil, _) => List.rev acc
	 | (_, nil) => List.rev acc
	 | (x::xs, y::ys) => lp (xs, ys, (x,y)::acc)
       (* end case *))
    in
      lp (xs, ys, nil)
    end

(* zipEq : 'a list * 'b list -> ('a * 'b) list *)
(* raises an exception if lists are not of the same length *)
  fun zipEq (xs, ys) = let
    fun lp (xs, ys, acc) = 
     (case (xs, ys)
        of (nil, nil) => List.rev acc
	 | (nil, _) => (raise Fail "unequal lengths")
	 | (_, nil) => (raise Fail "unequal lengths")
	 | (x::xs, y::ys) => lp (xs, ys, (x,y)::acc)
       (* end case *))
    in
      lp (xs, ys, nil)
    end

(* unzip : ('a * 'b) list -> 'a list * 'b list *)
  fun unzip pairs = let
    fun lp (ps, accL, accR) =
     (case ps
        of nil => (accL, accR)
	 | (x,y)::t => lp (t, x::accL, y::accR)
       (* end case *))
    in
      lp (List.rev pairs, nil, nil)
    end

(* map : ('a * 'b -> 'c) -> 'a list * 'b list -> 'c list *)
  fun map f (xs, ys) = let
    fun lp arg =
     (case arg
        of (nil, _, acc) => List.rev acc
	 | (_, nil, acc) => List.rev acc
	 | (x::xs, y::ys, acc) => lp (xs, ys, f(x,y)::acc)
       (* end case *))
    in
      lp (xs, ys, nil)
    end

(* mapEq : ('a * 'b -> 'c) -> 'a list * 'b list -> 'c list *)
(* raises an exception if lists are not of same length *)
  fun mapEq f (xs, ys) = let
    fun lp (xs, ys, acc) =
     (case (xs, ys)
        of (nil, nil) => List.rev acc
	 | (nil, _) => (raise Fail "unequal lengths")
	 | (_, nil) => (raise Fail "unequal lengths")
	 | (x::xs, y::ys) => lp (xs, ys, f(x,y)::acc)
       (* end case *))
    in
      lp (xs, ys, nil)
    end

(* foldl : ('a * 'b * 'c -> 'c) -> 'c -> 'a list * 'b list -> 'c *)
  fun foldl f z (xs, ys) = let
    fun lp (xs, ys, acc) =
     (case (xs, ys)
        of (nil, _) => acc
	 | (_, nil) => acc
	 | (x::xs, y::ys) => lp (xs, ys, f(x,y,acc))
       (* end case *))
    in
      lp (xs, ys, z)
    end

(* foldlEq : ('a * 'b * 'c -> 'c) -> 'c -> 'a list * 'b list -> 'c *)
(* raises an exception if lists are not of same length *)
  fun foldlEq f z (xs, ys) = let
    fun lp (xs, ys, acc) =
     (case (xs, ys)
        of (nil, nil) => acc
	 | (nil, _) => (raise Fail "unequal lengths")
	 | (_, nil) => (raise Fail "unequal lengths")
	 | (x::xs, y::ys) => lp (xs, ys, f(x,y,acc))
       (* end case *))
    in
      lp (xs, ys, z)
    end

(* foldr : ('a * 'b * 'c -> 'c) -> 'c -> 'a list * 'b list -> 'c *)
  fun foldr f z (xs, ys) = let
    fun lp (xs, ys) = 
     (case (xs, ys)
        of (nil, _) => z
	 | (_, nil) => z
	 | (x::xs, y::ys) => f (x, y, lp (xs, ys))
       (* end case *))
    in
      lp (xs, ys)
    end

(* foldrEq : ('a * 'b * 'c -> 'c) -> 'c -> 'a list * 'b list -> 'c *)
(* raises an exception if lists are not of same length *)
  fun foldrEq f z (xs, ys) = let
    fun lp (xs, ys) = 
     (case (xs, ys)
        of (nil, nil) => z
	 | (nil, _) => (raise Fail "unequal lengths")
	 | (_, nil) => (raise Fail "unequal lengths")
	 | (x::xs, y::ys) => f (x, y, lp (xs, ys))
       (* end case *))
    in
      lp (xs, ys)
    end

(* all : ('a * 'b -> bool) -> 'a list * 'b list -> bool *)
  fun all pred (xs, ys) = let
    fun lp (xs, ys) =
     (case (xs, ys)
        of (nil, _) => true
	 | (_, nil) => true
	 | (x::xs, y::ys) => pred (x, y) andalso lp (xs, ys)
       (* end case *))
    in
      lp (xs, ys)
    end

(* exists : ('a * 'b -> bool) -> 'a list * 'b list -> bool *)
  fun exists pred (xs, ys) = let
    fun lp (xs, ys) = 
     (case (xs, ys)
        of (nil, _) => false
	 | (_, nil) => false
	 | (x::xs, y::ys) => pred (x, y) orelse lp (xs, ys)
       (* end case *))
    in
      lp (xs, ys)
    end

(* allEq : ('a * 'b -> bool) -> 'a list * 'b list -> bool *)
(* raises an exception if lists are not of same length *)
  fun allEq pred (xs, ys) = let
    fun lp (xs, ys) =
     (case (xs, ys)
        of (nil, nil) => true
	 | (nil, _) => (raise Fail "unequal lengths")
	 | (_, nil) => (raise Fail "unequal lengths")
	 | (x::xs, y::ys) => pred (x, y) andalso lp (xs, ys)
       (* end case *))
    in
      lp (xs, ys)
    end

end
