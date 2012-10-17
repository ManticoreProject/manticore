(* vproc-utils.pml
 *)

structure VProcUtils = struct

    structure PT = PrimTypes
    _primcode (

    (* hooks into the C runtime system (parallel-rt/vproc/vproc.c) *)
      extern int GetNumHWNodes () __attribute__((pure));

      typedef ml_vproc = [vproc];

    (* returns the total number of nodes *)
      define inline @num-nodes () : int =
	  let n : int = ccall GetNumHWNodes()
	  return (n)
	;

      define inline @num-nodes-w (_ : unit / exh : exh) : ml_int =
	  let n : int = @num-nodes ()
          return (alloc (n))
	;

    (* returns the node id of the given vproc *)
      define inline @node-id (vp : vproc) : int =
	  let id : int = vpload(NODE_ID, vp)
	  return (id)
	;

    )

    type vproc = _prim(ml_vproc)

    val numNodes : unit -> int = _prim (@num-nodes-w)

    local
      _primcode (
	define inline @node (_ : unit / _ : exh) : ml_int =
	    let n : int = @node-id (host_vproc)
	    let m : ml_int = alloc(n)
	    return (m)
	  ;
      )
    in
    val node : unit -> int = _prim(@node)
    end

  end
