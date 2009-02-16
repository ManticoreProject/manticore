(* fls.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Implicit local memory for fibers.
 *)

structure FLS (* :
  sig

    _prim(

    (* environment of an implicit thread *)
      typedef ite = [
                PrimStk.stk,                  (* work-group stack *)
		Option.option                 (* current cancelable *)
      ];

    (* fiber-local storage *)
      typedef fls = [
	  int,                              (* if this value is a valid vproc id, the thread is pinned to that vproc *)
	  Option.option                     (* implicit-thread environment (ITE) *)
      ];

    (* create fls *)
      define @new (x : unit / exh : exh) : fls;
    (* set the fls on the host vproc *)
      define inline @set (fls : fls / exh : exh) : unit;
    (* get the fls from the host vproc *)
      define inline @get ( / exh : exh) : fls;

    (* find the implicit-thread environment *)
      define @get-ite (/ exh : exh) : ite;
      define @find-ite (/ exh : exh) : Option.option;
    (* set the implicit-thread environment *)
      define @set-ite (ite : ite / exh : exh) : ();

    (* set the fls as pinned to the given vproc *)
      define @pin-to (fls : fls, vprocId : int / exh : exh) : fls;
    (* pin the current fls to the given vproc *)
      define @pin (vprocId : int / exh : exh) : ();

    )

  end *) = struct

#define VPROC_OFF              0
#define ITE_OFF                1

    _primcode (

    (* environment of an implicit thread *)
      typedef ite = [
                PrimStk.stk,                  (* work-group stack *)
		Option.option                 (* current cancelable *)
      ];

      (* fiber-local storage *)
	typedef fls = [
	    int,                              (* if this value is a valid vproc id, the thread is pinned to that vproc *)
(* FIXME: using an option type here adds an unnecessary level of indirection *)
	    Option.option                     (* implicit-thread environment (ITE) *)
	];

	define @alloc (vprocId : int, ite : Option.option / exh : exh) : fls =
	  let fls : fls = alloc(vprocId, ite)
	  return(fls)
	;

      (* create fls *)
	define @new (x : unit / exh : exh) : fls =
	  let fls : fls = @alloc(~1, Option.NONE / exh)
	  return(fls)
	;

      (* set the fls on the host vproc *)
	define inline @set (fls : fls / exh : exh) : unit =
	  do assert(NotEqual(fls, nil))
	  do vpstore (CURRENT_FG, host_vproc, fls)
	  return(UNIT)
	;

      (* get the fls from the host vproc *)
	define inline @get ( / exh : exh) : fls =
	  let fls : fls = vpload (CURRENT_FG, host_vproc)
	  do assert(NotEqual(fls, nil))
	  return(fls)
	;

	define @vproc-id (fls : fls / exh : exh) : int =
	  return(SELECT(VPROC_OFF, fls))
	;

      (* find the implicit-thread environment *)

	define @find-ite (/ exh : exh) : Option.option =
	  let fls : fls = @get(/ exh)
	  return(SELECT(ITE_OFF, fls))
	;

	define @get-ite (/ exh : exh) : ite =
	  let fls : fls = @get(/ exh)
	  case SELECT(ITE_OFF, fls)
	   of Option.NONE =>
	      let e : exn = Fail(@"FLS.ite: nonexistant implicit threading environment")
	      throw exh(e)
	    | Option.SOME(ite : ite) =>
	      return(ite)
	  end
	;

      (* set the implicit-thread environment *)
	define @set-ite (ite : ite / exh : exh) : () =
	  let fls : fls = @get(/ exh)
	  let vProcId : int = @vproc-id(fls / exh)
	  let fls : fls = @alloc(vProcId, Option.SOME(ite) / exh)
	  let _ : unit = @set(fls / exh)  
	  return()
	;

      (* set the fls as pinned to the given vproc *)
	define @pin-to (fls : fls, vprocId : int / exh : exh) : fls =
	  let fls : fls = @alloc(vprocId, SELECT(ITE_OFF, fls) / exh)
	  return(fls)
	;

      (* pin the current fls to the given vproc *)
	define @pin (vprocId : int / exh : exh) : () =
	  let fls : fls = @get(/ exh)
	  let fls : fls = @pin-to(fls, vprocId / exh)
	  let _ : unit = @set(fls / exh)
	  return()
	;

    )

  end
