(* prim-list.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Some hooks into the List module, which we make available to inline BOM code.
 *)

structure PrimList =
  struct

    _primcode (

(* FIXME: if the bindings below are declared "inline", the compiler complains that the hlops are
 * unbound. *)

      typedef list = List.list;

      define (* inline *) @length-w = List.length;
      define (* inline *) @length (xs : list / exh : exh) : int =
	  let l : ml_int = @length-w (xs / exh)
	  return (unwrap(l))
	;

      define (* inline *) @app-w = List.app;
      define (* inline *) @app (f : fun(any / exh -> ), ls : list / exh : exh) : () =
	  fun f1 (x : any / exh : exh) : unit =
	      do apply f (x / exh)
	      return (UNIT)
	(* app is curried *)
	  let app : fun(list / exh -> unit) = @app-w (f1 / exh)
	  let _ : unit = apply app (ls / exh)
	  return ()
	;

      define (* inline *) @rev = List.rev;

      define (* inline *) @map-w = List.map;
      define (* inline *) @map (f : fun(any / exh -> any), ls : list / exh : exh) : list =
	  let map : fun (list / exh -> any) = @map-w (f / exh)
	  let x : any = apply map (ls / exh)
	  return (x)
	;

      define (* inline *) @append-w = List.append;
      define (* inline *) @append (l1 : list, l2 : list / exh : exh) : list =
	  let x : list = @append-w (alloc (l1, l2) / exh)
	  return (x)
	;

    )


  end
