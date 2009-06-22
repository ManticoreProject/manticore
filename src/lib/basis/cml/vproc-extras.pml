(* vproc-extras.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * This is an ML-level interface to vprocs.  Its purpose is for testing and benchmarking
 * applications that need control of the layout of threads on vprocs.
 *)

structure VProcExtras (* : sig

    type vproc

  (* return a list of all of the vprocs in the system *)
    val vprocs : unit -> vproc list

  (* return the host vproc *)
    val host : unit -> vproc

  (* return the integer ID of a vproc *)
    val id : vproc -> int

  (* are two vprocs the same? *)
    val same : (vproc * vproc) -> bool

  (* spawn a thread on a specific vproc *)
    val spawnOn : (unit -> unit) -> vproc -> Threads.thread_id

  end *) = struct

    _primcode (
	typedef ml_vproc = [vproc];

	define inline @vprocs (_ : unit / _ : exh) : List.list =
	    VProc.@all-vprocs()
	  ;

	define inline @host (_ : unit / _ : exh) : [vproc] =
	    let vp : ml_vproc = alloc(host_vproc)
	    return (vp)
	  ;

	define inline @id (vp : ml_vproc / _ : exh) : ml_int =
	    let n : int = VProc.@vproc-id (#0(vp))
	    let m : ml_int = wrap(n)
	    return (m)
	  ;

	define inline @same (arg : [ml_vproc, ml_vproc] / _ : exh) : bool =
	    let a : vproc = #0(#0(arg))
	    let b : vproc = #0(#1(arg))
	    if Equal(a, b) then return(true) else return(false)
	  ;

	define inline @remoteSpawn (arg : [ml_vproc, fun(unit / exh -> unit)] / exh : exh) : FLS.fls =
	    let vp : vproc = #0(#0(arg))
	    let f : fun(unit / exh -> unit) = #1(arg)
	    let tid : FLS.fls = Threads.@remote-spawn (vp, f / exh)
	    return (tid)
	  ;
      )

    type vproc = _prim(ml_vproc)

    val vprocs : unit -> vproc list = _prim (@vprocs)
    val host : unit -> vproc = _prim(@host)
    val id : vproc -> int = _prim(@id)
    val same : (vproc * vproc) -> bool = _prim(@same)

    val remoteSpawn : (vproc * (unit -> unit)) -> Threads.thread_id = _prim(@remoteSpawn)

    fun spawnOn f vp = remoteSpawn (vp, f)

  end
