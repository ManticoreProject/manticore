(* 
 *  Spill heuristics should match the following signature.
 *)
signature RA_SPILL_HEURISTICS =
sig
   structure G : RA_GRAPH (* = RAGraph *)
                 where type C.CellSet.cellset = RAGraph.C.CellSet.cellset
                   and type 'a C.ColorTable.hash_table = 'a RAGraph.C.ColorTable.hash_table
                   and type 'a C.HashTable.hash_table = 'a RAGraph.C.HashTable.hash_table
                   and type C.SortedCells.sorted_cells = RAGraph.C.SortedCells.sorted_cells
                   and type C.cell = RAGraph.C.cell
                   and type C.cellColor = RAGraph.C.cellColor
                   and type C.cellkind = RAGraph.C.cellkind
                   and type C.cellkindDesc = RAGraph.C.cellkindDesc
                   and type C.cellkindInfo = RAGraph.C.cellkindInfo
                   and type 'a PPtHashTable.hash_table = 'a RAGraph.PPtHashTable.hash_table
                   and type 'a SpillLocHashTable.hash_table = 'a RAGraph.SpillLocHashTable.hash_table
                   and type interferenceGraph = RAGraph.interferenceGraph
                   and type move = RAGraph.move
                   and type moveKind = RAGraph.moveKind
                   and type moveStatus = RAGraph.moveStatus
                   and type node = RAGraph.node
                   and type nodeStatus = RAGraph.nodeStatus
                   and type spillLoc = RAGraph.spillLoc
                   and type trailInfo = RAGraph.trailInfo

   exception NoCandidate

   val mode : G.mode

   val init : unit -> unit

   val chooseSpillNode : 
       { graph          : G.interferenceGraph,
         spillWkl       : G.node list,
         hasBeenSpilled : int -> bool
       } ->
       { spillWkl : G.node list,
         node     : G.node option,
         cost     : real
       }
end
