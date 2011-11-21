(* virtual-maps.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Virtual maps are maps between different processor topologies.
 *)

structure VirtualMaps :
  sig

    val vproc : Topologies.topologies -> VProc.vproc

  (* create a processor topology specialized to our 8-core benchmarking machine. *)
    val octo : unit -> SimpleTopology1.topology

  (* creates a tree topology of the given depth, and maps it onto the given depth-two tree topology *)
    val treeToDepth2Tree : (int * SimpleTopology1.topology) -> TreeTopology.topology

  end = struct

    val vproc = VProc.fromId o Topologies.self

  (* create a processor topology specialized to our 8-core benchmarking machine. *)
    fun octo () = let
	  fun mk (vps, self, sibling, others) = (
	        case (vps, self, sibling)
		 of (nil, SOME self, SOME sibling) => (self, sibling, others)
		  | (vp1 :: vp2 :: vps, _, _) => 
		      if VProc.same(vp1, VProc.host())
		         then mk(vps, SOME vp1, SOME vp2, others)
		      else if VProc.same(vp2, VProc.host())
		         then mk(vps, SOME vp2, SOME vp1, others)
		      else mk(vps, (vp1, vp2) :: others)
	  val (self, sibling, others) = mk(VProc.vprocs(), NONE, NONE, nil)
          in
	    (VProc.id self, VProc.id sibling, List.map (fn (vp1, vp2) => (VProc.id vp1, VProc.id vp2)) others)
          end

  (* creates a tree topology of the given depth, and maps it onto the given depth-two tree topology *)
    fun treeToDepth2Tree (depth, depth2Tree) = let
	  fun mk (depth, self, depth2Tree) = 
	        if depth <= 0
		   then LEAF self
		else let
	           val depth' = depth - 1
		   val l = mapToDepth2Tree(depth', self, depth2Tree)
		   val r = mapToDepth2Tree(depth', 
					   SimpleTopology1.self(SimpleTopology1.sibling depth2Tree), 
					   SimpleTopology1.next depth2Tree)
		   in
	              ND(self, l, r)
                   end
          in 
	    (mk(depth, SimpleTopology1.self depth2Tree, depth2Tree), TOP)
	  end

  end
