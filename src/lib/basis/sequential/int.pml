(* int.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)


structure Int =
  struct

    structure PT = PrimTypes

    type int = int

    _primcode (

      extern void *M_IntToString (int) __attribute__((alloc,pure));
      extern void *M_IntFromString (void *) __attribute__((alloc,pure));
      extern int M_CeilingLg (int) __attribute__((pure));
      extern int M_FloorLg (int) __attribute__((pure));

      define inline @to-string (n : ml_int / exh : exh) : ml_string =
	  let res : ml_string = ccall M_IntToString (unwrap(n))
	    return (res)
      ;

      define inline @from-string (s : ml_string / exh : exh) : Option.option =
	  let res : Option.option = ccall M_IntFromString (s)
	    return (res)
      ;

      define inline @ceiling-lg(n : ml_int / exh : exh) : ml_int =
	let res : int = ccall M_CeilingLg(unwrap(n))
	return (alloc(res))
      ;

      define inline @floor-lg(n : ml_int / exh : exh) : ml_int =
	let res : int = ccall M_FloorLg(unwrap(n))
	return (alloc(res))
      ;

    )

    val toString : int -> string = _prim(@to-string)
    val fromString : string -> int Option.option = _prim(@from-string)

(* FIXME: why is this function here? It is not part of the INT API *)
    val ceilingLg : int -> int = _prim(@ceiling-lg)
    val floorLg : int -> int = _prim(@floor-lg)

  (* abs : int -> int *)
    fun abs n = if n < 0 then ~n else n

    fun max (x, y) = if x < y then y else x
    fun min (x, y) = if x < y then x else y

    fun compare (x, y) = 
      if x = y then EQUAL 
      else if x < y then LESS 
      else GREATER

    fun sign n =
      if n < 0 then ~1
      else if n = 0 then 0
      else 1

    fun sameSign (m, n) = (sign(m) = sign(n))

(* FIXME: why is this function here? It is not part of the INT API *)
  (* fib : int -> int *)
  (* Compute the nth Fibonacci number, where *)
  (*   fib 0 is 0, fib 1 is 1, fib 2 is 1, etc. *)
  (* Raises an exception on negative arguments. *)
    fun fib n = let
      fun ff args =
       (case args
	  of (0, u, p) => u
	   | (n, u, p) => ff (n-1, u+p, u)
          (* end case *))
      in
	if n < 0 then (raise Fail "fib: given negative arg")
	else if n = 0 then 1
        else if n = 1 then 0
	else ff (n, 0, 1)
      end

  end
