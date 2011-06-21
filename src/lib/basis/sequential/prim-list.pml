(* prim-list.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Some hooks into the List module that we make available to inline BOM code.
 *)

structure PrimList =
  struct

    local

      _primcode (
(* FIXME: if these wrapper hlops below are declared "inline", the compiler complains that the hlops are
 * unbound. *)
	define (* inline *) @length-w = List.length;
	define (* inline *) @app-w = List.app;
	define (* inline *) @map-w = List.map;
	define (* inline *) @append-w = List.append;
	define (* inline *) @filter-w = List.filter;
	define (* inline *) @partition-w = List.partition;
      )

    in

    _primcode (

      typedef list = List.list;

      define @null = List.null;

      define (* inline *) @length (xs : list / exh : exh) : int =
	  let l : ml_int = @length-w (xs / exh)
	  return (#0(l))
	;

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

      define (* inline *) @map (f : fun(any / exh -> any), ls : list / exh : exh) : list =
	  let map : fun (list / exh -> any) = @map-w (f / exh)
	  let x : any = apply map (ls / exh)
	  return (x)
	;

      define (* inline *) @append (l1 : list, l2 : list / exh : exh) : list =
	  let x : list = @append-w (alloc (l1, l2) / exh)
	  return (x)
	;

      define (* inline *) @filter (f : fun(any / exh -> bool), ls : list / exh : exh) : list =
	  let filter : fun (list / exh -> list) = @filter-w (f / exh)
          apply filter (ls / exh)
	;

      define (* inline *) @partition (f : fun(any / exh -> bool), ls : list / exh : exh) : [list,list] =
	  let partition : fun (list / exh -> [list,list]) = @partition-w (f / exh)
          apply partition (ls / exh)
	;

    )

    end (* local *)

  end
