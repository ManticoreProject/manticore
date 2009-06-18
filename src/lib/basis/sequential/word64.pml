(* word64.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure Word64 =
  struct

    _primcode (

      extern void *M_Word64ToString (long) __attribute__((alloc,pure));

      typedef word = long;
      typedef ml_word = [word];

      define @add (arg : [ml_word, ml_word] / exh : exh) : ml_word =
	  return (alloc (I64Add (unwrap (#0(arg)), unwrap (#1(arg)))))
	;

      define @sub (arg : [ml_word, ml_word] / exh : exh) : ml_word =
	  return (alloc (I64Sub (unwrap (#0(arg)), unwrap (#1(arg)))))
	;

      define @mul (arg : [ml_word, ml_word] / exh : exh) : ml_word =
	  return (alloc (U64Mul (unwrap (#0(arg)), unwrap (#1(arg)))))
	;

      define @udiv (arg : [ml_word, ml_word] / exh : exh) : ml_word =
	  return (alloc (U64Div (unwrap (#0(arg)), unwrap (#1(arg)))))
	;

      define @lsh (arg : [ml_word, ml_word] / exh : exh) : ml_word =
	  return (alloc (I64LSh (unwrap (#0(arg)), unwrap (#1(arg)))))
	;

      define @same (arg : [ml_word, ml_word] / exh : exh) : bool =
	  return (I64Eq (unwrap (#0(arg)), unwrap (#1(arg))))
	;

      define @less-than (arg : [ml_word, ml_word] / exh : exh) : bool =
	  return (U64Lt (unwrap (#0(arg)), unwrap (#1(arg))))
	;

      define @from-int (x : ml_int / exh : exh) : ml_word =
	  return (alloc (I32ToI64 (unwrap (x))))
	;

      define inline @to-string (n : ml_word / exh : exh) : ml_string =
	  let res : ml_string = ccall M_Word64ToString (unwrap(n))
	    return (res)
      ;

    )

    type word = long

    val add : (word * word) -> word = _prim (@add)
    val sub : (word * word) -> word = _prim (@sub)
    val mul : (word * word) -> word = _prim (@mul)
    val udiv : (word * word) -> word = _prim (@udiv)
    val lsh : (word * word) -> word = _prim (@lsh)
    val same : (word * word) -> bool = _prim (@same)
    val lessThan : (word * word) -> bool = _prim (@less-than)
    val fromInt : int -> word = _prim (@from-int)
    val toString : word -> string = _prim(@to-string)

    fun compare (x, y) = 
	if same (x, y) then EQUAL 
	else if lessThan (x, y) then LESS 
	else GREATER

    fun floorLg x =
	let fun lp (x, i) =
		if same (x, 1) then
		    i
		else
		    lp (udiv (x, 2), add (i, 1))
	in
	    lp (x, 0)
	end

    fun ceilingLg x = 
	let val lg = floorLg x
	in
	    add (lg, (case compare (sub (x, lsh (1, lg)), 0)
			      of GREATER => 1
			       | _ => 0))
	end

  end

