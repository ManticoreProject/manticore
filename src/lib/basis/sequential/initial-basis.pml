(* initial-basis.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * This file defines HLOps that are bound to the builtin operators.  It is loaded
 * prior to compilation to seed the initial environment.
 *)

_primcode (

   #define RAISE_DIV(exh) let e : exn = Div (* in *) throw exh (e)

  typedef exh = cont(exn);
  typedef unit = enum(0);
  typedef string_len = int;
  typedef string_data = any;
  typedef ml_string = [string_data, string_len];
  typedef ml_int = [int];
  typedef ml_long = [long];
  typedef ml_float = [float];
  typedef ml_double = [double];

(* logging events *)
  typedef log_event = int;

(* int operators *)
  define inline @int-add (arg : [ml_int, ml_int] / _ : exh) : ml_int =
      let res : ml_int = wrap(I32Add(unwrap(#0(arg)), unwrap(#1(arg))))
        return (res)
  ;
  define inline @int-sub (arg : [ml_int, ml_int] / _ : exh) : ml_int =
      let res : ml_int = wrap(I32Sub(unwrap(#0(arg)), unwrap(#1(arg))))
        return (res)
  ;
  define inline @int-mul (arg : [ml_int, ml_int] / _ : exh) : ml_int =
      let res : ml_int = wrap(I32Mul(unwrap(#0(arg)), unwrap(#1(arg))))
        return (res)
  ;

  define inline @int-div (arg : [ml_int, ml_int] / exh : exh) : ml_int =
      let b : int = unwrap(#1(arg))
        if I32Eq(b, 0)
	  then RAISE_DIV(exh)
	  else
	    let res : ml_int = wrap(I32Div(unwrap(#0(arg)), b))
	      return (res)
  ;

  define inline @int-mod (arg : [ml_int, ml_int] / _ : exh) : ml_int =
      let res : ml_int = wrap(I32Mod(unwrap(#0(arg)), unwrap(#1(arg))))
        return (res)
  ;
  define inline @int-gt (arg : [ml_int, ml_int] / _ : exh) : bool =
      let res : bool = I32Gt(unwrap(#0(arg)), unwrap(#1(arg)))
        return (res)
  ;
  define inline @int-gte (arg : [ml_int, ml_int] / _ : exh) : bool =
      let res : bool = I32Gte(unwrap(#0(arg)), unwrap(#1(arg)))
        return (res)
  ;
  define inline @int-lt (arg : [ml_int, ml_int] / _ : exh) : bool =
      let res : bool = I32Lt(unwrap(#0(arg)), unwrap(#1(arg)))
        return (res)
  ;
  define inline @int-lte (arg : [ml_int, ml_int] / _ : exh) : bool =
      let res : bool = I32Lte(unwrap(#0(arg)), unwrap(#1(arg)))
        return (res)
  ;
  define inline @int-neg (arg : ml_int / _ : exh) : ml_int =
      let res : ml_int = wrap(I32Neg(unwrap(arg)))
        return (res)
  ;

(* long operators *)
  define inline @long-add (arg : [ml_long, ml_long] / _ : exh) : ml_long =
      let res : ml_long = wrap(I64Add(unwrap(#0(arg)), unwrap(#1(arg))))
        return (res)
  ;
  define inline @long-sub (arg : [ml_long, ml_long] / _ : exh) : ml_long =
      let res : ml_long = wrap(I64Sub(unwrap(#0(arg)), unwrap(#1(arg))))
        return (res)
  ;
  define inline @long-mul (arg : [ml_long, ml_long] / _ : exh) : ml_long =
      let res : ml_long = wrap(I64Mul(unwrap(#0(arg)), unwrap(#1(arg))))
        return (res)
  ;
  define inline @long-div (arg : [ml_long, ml_long] / exh : exh) : ml_long =
      let b : long = unwrap(#1(arg))
        if I64Eq(b, 0)
	  then RAISE_DIV(exh)
	  else
	    let res : ml_long = wrap(I64Div(unwrap(#0(arg)), b))
	      return (res)
  ;
  define inline @long-mod (arg : [ml_long, ml_long] / _ : exh) : ml_long =
      let res : ml_long = wrap(I64Mod(unwrap(#0(arg)), unwrap(#1(arg))))
        return (res)
  ;
  define inline @long-gt (arg : [ml_long, ml_long] / _ : exh) : bool =
      let res : bool = I64Gt(unwrap(#0(arg)), unwrap(#1(arg)))
        return (res)
  ;
  define inline @long-gte (arg : [ml_long, ml_long] / _ : exh) : bool =
      let res : bool = I64Gte(unwrap(#0(arg)), unwrap(#1(arg)))
        return (res)
  ;
  define inline @long-lt (arg : [ml_long, ml_long] / _ : exh) : bool =
      let res : bool = I64Lt(unwrap(#0(arg)), unwrap(#1(arg)))
        return (res)
  ;
  define inline @long-lte (arg : [ml_long, ml_long] / _ : exh) : bool =
      let res : bool = I64Lte(unwrap(#0(arg)), unwrap(#1(arg)))
        return (res)
  ;
  define inline @long-neg (arg : ml_long / _ : exh) : ml_long =
      let res : ml_long = wrap(I64Neg(unwrap(arg)))
        return (res)
  ;

(* float operations *)
  define inline @float-add (arg : [ml_float, ml_float] / _ : exh) : ml_float =
      let res : ml_float = wrap(F32Add(unwrap(#0(arg)), unwrap(#1(arg))))
        return (res)
  ;
  define inline @float-sub (arg : [ml_float, ml_float] / _ : exh) : ml_float =
      let res : ml_float = wrap(F32Sub(unwrap(#0(arg)), unwrap(#1(arg))))
        return (res)
  ;
  define inline @float-mul (arg : [ml_float, ml_float] / _ : exh) : ml_float =
      let res : ml_float = wrap(F32Mul(unwrap(#0(arg)), unwrap(#1(arg))))
        return (res)
  ;
  define inline @float-div (arg : [ml_float, ml_float] / _ : exh) : ml_float =
      let res : ml_float = wrap(F32Div(unwrap(#0(arg)), unwrap(#1(arg))))
        return (res)
  ;
  define inline @float-gt (arg : [ml_float, ml_float] / _ : exh) : bool =
      let res : bool = F32Gt(unwrap(#0(arg)), unwrap(#1(arg)))
        return (res)
  ;
  define inline @float-gte (arg : [ml_float, ml_float] / _ : exh) : bool =
      let res : bool = F32Gte(unwrap(#0(arg)), unwrap(#1(arg)))
        return (res)
  ;
  define inline @float-lt (arg : [ml_float, ml_float] / _ : exh) : bool =
      let res : bool = F32Lt(unwrap(#0(arg)), unwrap(#1(arg)))
        return (res)
  ;
  define inline @float-lte (arg : [ml_float, ml_float] / _ : exh) : bool =
      let res : bool = F32Lte(unwrap(#0(arg)), unwrap(#1(arg)))
        return (res)
  ;
  define inline @float-neg (arg : ml_float / _ : exh) : ml_float =
      let res : ml_float = wrap(F32Neg(unwrap(arg)))
        return (res)
  ;

(* double operations *)
  define inline @double-add (arg : [ml_double, ml_double] / _ : exh) : ml_double =
      let res : ml_double = wrap(F64Add(unwrap(#0(arg)), unwrap(#1(arg))))
        return (res)
  ;
  define inline @double-sub (arg : [ml_double, ml_double] / _ : exh) : ml_double =
      let res : ml_double = wrap(F64Sub(unwrap(#0(arg)), unwrap(#1(arg))))
        return (res)
  ;
  define inline @double-mul (arg : [ml_double, ml_double] / _ : exh) : ml_double =
      let res : ml_double = wrap(F64Mul(unwrap(#0(arg)), unwrap(#1(arg))))
        return (res)
  ;
  define inline @double-div (arg : [ml_double, ml_double] / _ : exh) : ml_double =
      let res : ml_double = wrap(F64Div(unwrap(#0(arg)), unwrap(#1(arg))))
        return (res)
  ;
  define inline @double-gt (arg : [ml_double, ml_double] / _ : exh) : bool =
      let res : bool = F64Gt(unwrap(#0(arg)), unwrap(#1(arg)))
        return (res)
  ;
  define inline @double-gte (arg : [ml_double, ml_double] / _ : exh) : bool =
      let res : bool = F64Gte(unwrap(#0(arg)), unwrap(#1(arg)))
        return (res)
  ;
  define inline @double-lt (arg : [ml_double, ml_double] / _ : exh) : bool =
      let res : bool = F64Lt(unwrap(#0(arg)), unwrap(#1(arg)))
        return (res)
  ;
  define inline @double-lte (arg : [ml_double, ml_double] / _ : exh) : bool =
      let res : bool = F64Lte(unwrap(#0(arg)), unwrap(#1(arg)))
        return (res)
  ;
  define inline @double-neg (arg : ml_double / _ : exh) : ml_double =
      let res : ml_double = wrap(F64Neg(unwrap(arg)))
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

#ifndef SEQUENTIAL
(* primitive types for parallelism *)
  typedef fiber_local_storage = [
      bool,		(* flag for pinning the fiber to a vproc *)
      any		(* == AssocList.assoc_list;  dictionary *)
    ];
  typedef thread_id = fiber_local_storage;
(* support for local atomicity *)
  define inline @atomic-begin () : () =
      do vpstore (ATOMIC, host_vproc, true)
	return ()
  ;
  define inline @atomic-end () : () =
      let vp : vproc = host_vproc
      do vpstore (ATOMIC, vp, false)
(*
      let pending = vpload (SIG_PENDING, vp)
	if pending
	  then
	    do vpstore (SIG_PENDING, vp, false)
	    ???
	  else return (UNIT)
*)
	return ()
  ;

#else
  typedef thread_id = unit;
#endif

#ifdef ENABLE_LOGGING

  #include "log-events.def"

  extern void M_LogEvent0 (void*, int);
  extern void M_LogEvent1 (void*, int, int);
  extern void M_LogEventPtr (void*, int, void*);

  define inline @log-event0 (evt : log_event) : () =
    do ccall M_LogEvent0 (host_vproc, evt)
    return ()
  ;

  define inline @log-event1 (evt : log_event, aux : int) : () =
    do ccall M_LogEvent1 (host_vproc, evt, aux)
    return ()
  ;

  define inline @log-event-ptr (evt : log_event, ptr : any) : () =
    do ccall M_LogEventPtr (host_vproc, evt, ptr)
    return ()
  ;

#else

  define inline @log-event0 (evt : log_event) : () = return ();
  define inline @log-event1 (evt : log_event, aux : int) : () = return ();
  define inline @log-event-ptr (evt : log_event, ptr : any) : () = return ();

#endif  /* !ENABLE_LOGGING */

)
