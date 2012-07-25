(*
 * Abstract view a flowgraph required by the new register allocator.
 * In order to allow different representation to share the same 
 * register allocator core, each representation should implement the
 * following interface to talk to the new RA.
 *
 * -- Allen
 *)

signature RA_FLOWGRAPH =
sig

   structure I     : INSTRUCTIONS
   structure C     : CELLS  
   structure G     : RA_GRAPH (* = RAGraph *)
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
   structure Spill : RA_SPILL
     (* sharing Spill.I = I *)
     where type I.addressing_mode = I.addressing_mode
       and type I.ea = I.ea
       and type I.instr = I.instr
       and type I.instruction = I.instruction
       and type I.operand = I.operand
     (* sharing I.C = C *)

   type flowgraph

   val mode : G.mode

    (* Dump the flograph to a stream *)
   val dumpFlowgraph : string * flowgraph * TextIO.outstream -> unit

    (* Dump the flograph to a stream *)
   val annotations : flowgraph -> Annotations.annotations ref

    (*
     * Interface for communicating with the new register allocator.
     * It is expected that the services will cache enough information
     * during build so that the rebuild and spill phases can be execute
     * quickly.
     *)
   val services : flowgraph ->
       { build   : G.interferenceGraph * CellsBasis.cellkind-> 
                      G.move list, (* build the graph *)
         spill   : {copyInstr    : Spill.copyInstr,
                    spill        : Spill.spill,
                    spillSrc     : Spill.spillSrc,
                    spillCopyTmp : Spill.spillCopyTmp,
                    reload       : Spill.reload,
                    reloadDst    : Spill.reloadDst,
                    renameSrc    : Spill.renameSrc,
                    graph        : G.interferenceGraph,
                    nodes        : G.node list,
                    cellkind     : CellsBasis.cellkind
                   } -> G.move list,
                     (* spill/rebuild the graph *)
         programPoint : {block:int, instr:int} -> G.programPoint,
         blockNum     : G.programPoint -> int,
         instrNum     : G.programPoint -> int
       }

end
