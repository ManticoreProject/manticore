(* initial-basis.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * This file defines HLOps that are bound to the builtin operators.  It is loaded
 * prior to compilation to seed the initial environment.
 *)

_primcode (

(* int operators *)
  define inline @int-add (arg : [ml_int, ml_int] / _ : exh) : ml_int =
      let res : ml_int = wrap(Int32Add(unwrap(#0 arg), unwrap(#1 arg)))
        return (res)
  ;
  define inline @int-sub (arg : [ml_int, ml_int] / _ : exh) : ml_int =
      let res : ml_int = wrap(Int32Sub(unwrap(#0 arg), unwrap(#1 arg)))
        return (res)
  ;
  define inline @int-mul (arg : [ml_int, ml_int] / _ : exh) : ml_int =
      let res : ml_int = wrap(Int32Mul(unwrap(#0 arg), unwrap(#1 arg)))
        return (res)
  ;
  define inline @int-div (arg : [ml_int, ml_int] / _ : exh) : ml_int =
      let b = #1 arg
        if (In32Eq(b, 0))
	  then RAISE
	  else
	    let res : ml_int = wrap(Int32Div(unwrap(#0 arg), unwrap(#1 arg)))
	      return (res)
  ;
  define inline @int-mod (arg : [ml_int, ml_int] / _ : exh) : ml_int =
      let res : ml_int = wrap(Int32Mod(unwrap(#0 arg), unwrap(#1 arg)))
        return (res)
  ;
  define inline @int-gt (arg : [ml_int, ml_int] / _ : exh) : bool =
      let res : bool = Int32Gt(unwrap(#0 arg), unwrap(#1 arg)))
        return (res)
  ;
  define inline @int-gte (arg : [ml_int, ml_int] / _ : exh) : bool =
      let res : bool = Int32Gte(unwrap(#0 arg), unwrap(#1 arg)))
        return (res)
  ;
  define inline @int-lt (arg : [ml_int, ml_int] / _ : exh) : bool =
      let res : bool = Int32Lt(unwrap(#0 arg), unwrap(#1 arg)))
        return (res)
  ;
  define inline @int-lte (arg : [ml_int, ml_int] / _ : exh) : bool =
      let res : bool = Int32Lte(unwrap(#0 arg), unwrap(#1 arg)))
        return (res)
  ;
  define inline @int-neg (arg : ml_int / _ : exh) : ml_int =
      let res : ml_int = wrap(Int32Neg(unwrap arg))
        return (res)
  ;

(* long operators *)
  define inline @long-add (arg : [ml_long, ml_long] / _ : exh) : ml_long =
      let res : ml_long = wrap(Int64Add(unwrap(#0 arg), unwrap(#1 arg)))
        return (res)
  ;
  define inline @long-sub (arg : [ml_long, ml_long] / _ : exh) : ml_long =
      let res : ml_long = wrap(Int64Sub(unwrap(#0 arg), unwrap(#1 arg)))
        return (res)
  ;
  define inline @long-mul (arg : [ml_long, ml_long] / _ : exh) : ml_long =
      let res : ml_long = wrap(Int64Mul(unwrap(#0 arg), unwrap(#1 arg)))
        return (res)
  ;
  define inline @long-div (arg : [ml_long, ml_long] / _ : exh) : ml_long =
      let b = #1 arg
        if (In64Eq(b, 0))
	  then RAISE
	  else
	    let res : ml_long = wrap(Int64Div(unwrap(#0 arg), unwrap(#1 arg)))
	      return (res)
  ;
  define inline @long-mod (arg : [ml_long, ml_long] / _ : exh) : ml_long =
      let res : ml_long = wrap(Int64Mod(unwrap(#0 arg), unwrap(#1 arg)))
        return (res)
  ;
  define inline @long-gt (arg : [ml_long, ml_long] / _ : exh) : bool =
      let res : bool = Int64Gt(unwrap(#0 arg), unwrap(#1 arg)))
        return (res)
  ;
  define inline @long-gte (arg : [ml_long, ml_long] / _ : exh) : bool =
      let res : bool = Int64Gte(unwrap(#0 arg), unwrap(#1 arg)))
        return (res)
  ;
  define inline @long-lt (arg : [ml_long, ml_long] / _ : exh) : bool =
      let res : bool = Int64Lt(unwrap(#0 arg), unwrap(#1 arg)))
        return (res)
  ;
  define inline @long-lte (arg : [ml_long, ml_long] / _ : exh) : bool =
      let res : bool = Int64Lte(unwrap(#0 arg), unwrap(#1 arg)))
        return (res)
  ;
  define inline @long-neg (arg : ml_long / _ : exh) : ml_long =
      let res : ml_long = wrap(Int64Neg(unwrap arg))
        return (res)
  ;

(* float operations *)
  define inline @float-add (arg : [ml_float, ml_float] / _ : exh) : ml_float =
      let res : ml_float = wrap(Float32Add(unwrap(#0 arg), unwrap(#1 arg)))
        return (res)
  ;
  define inline @float-sub (arg : [ml_float, ml_float] / _ : exh) : ml_float =
      let res : ml_float = wrap(Float32Sub(unwrap(#0 arg), unwrap(#1 arg)))
        return (res)
  ;
  define inline @float-mul (arg : [ml_float, ml_float] / _ : exh) : ml_float =
      let res : ml_float = wrap(Float32Mul(unwrap(#0 arg), unwrap(#1 arg)))
        return (res)
  ;
  define inline @float-div (arg : [ml_float, ml_float] / _ : exh) : bool =
      let res : ml_float = wrap(Float32Div(unwrap(#0 arg), unwrap(#1 arg)))
        return (res)
  ;
  define inline @float-gt (arg : [ml_float, ml_float] / _ : exh) : bool =
      let res : bool = Float32Gt(unwrap(#0 arg), unwrap(#1 arg)))
        return (res)
  ;
  define inline @float-gte (arg : [ml_float, ml_float] / _ : exh) : bool =
      let res : bool = Float32Gte(unwrap(#0 arg), unwrap(#1 arg)))
        return (res)
  ;
  define inline @float-lt (arg : [ml_float, ml_float] / _ : exh) : bool =
      let res : bool = Float32Lt(unwrap(#0 arg), unwrap(#1 arg)))
        return (res)
  ;
  define inline @float-lte (arg : [ml_float, ml_float] / _ : exh) : ml_float =
      let res : bool = Float32Lte(unwrap(#0 arg), unwrap(#1 arg)))
        return (res)
  ;
  define inline @float-neg (arg : ml_float / _ : exh) : ml_float =
      let res : ml_float = wrap(Float32Neg(unwrap arg))
        return (res)
  ;

(* double operations *)
  define inline @double-add (arg : [ml_double, ml_double] / _ : exh) : ml_double =
      let res : ml_double = wrap(Float64Add(unwrap(#0 arg), unwrap(#1 arg)))
        return (res)
  ;
  define inline @double-sub (arg : [ml_double, ml_double] / _ : exh) : ml_double =
      let res : ml_double = wrap(Float64Sub(unwrap(#0 arg), unwrap(#1 arg)))
        return (res)
  ;
  define inline @double-mul (arg : [ml_double, ml_double] / _ : exh) : ml_double =
      let res : ml_double = wrap(Float64Mul(unwrap(#0 arg), unwrap(#1 arg)))
        return (res)
  ;
  define inline @double-div (arg : [ml_double, ml_double] / _ : exh) : bool =
      let res : ml_double = wrap(Float64Div(unwrap(#0 arg), unwrap(#1 arg)))
        return (res)
  ;
  define inline @double-gt (arg : [ml_double, ml_double] / _ : exh) : bool =
      let res : bool = Float64Gt(unwrap(#0 arg), unwrap(#1 arg)))
        return (res)
  ;
  define inline @double-gte (arg : [ml_double, ml_double] / _ : exh) : bool =
      let res : bool = Float64Gte(unwrap(#0 arg), unwrap(#1 arg)))
        return (res)
  ;
  define inline @double-lt (arg : [ml_double, ml_double] / _ : exh) : bool =
      let res : bool = Float64Lt(unwrap(#0 arg), unwrap(#1 arg)))
        return (res)
  ;
  define inline @double-lte (arg : [ml_double, ml_double] / _ : exh) : ml_double =
      let res : bool = Float64Lte(unwrap(#0 arg), unwrap(#1 arg)))
        return (res)
  ;
  define inline @double-neg (arg : ml_double / _ : exh) : ml_double =
      let res : ml_double = wrap(Float64Neg(unwrap arg))
        return (res)
  ;

(* string operations *)
  extern void *M_StringConcat2 (void *, void *) __attribute__((pure,alloc));
  
  define inline @string-concat2 (arg : [ml_string, ml_string] / _ : exh) : ml_string =
      let res : ml_string = ccall M_StringConcat2 (#0(arg), #1(arg))
	return (res)
  ;

(* list operations *)
  (* ?? list-append ?? *)

)

val int_add : int * int -> int = _prim(@int-add)
