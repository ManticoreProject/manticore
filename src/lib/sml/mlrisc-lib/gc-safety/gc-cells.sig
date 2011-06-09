(*
 * This module provides few helper functions for annotating virtual registers
 * with gc type information. 
 *)

signature GC_CELLS =
sig

   structure C  : CELLS
   structure GC : GC_TYPE
   structure CB : CELLS_BASIS (* = CellsBasis *)
                  where type CellSet.cellset = CellsBasis.CellSet.cellset
                    and type 'a ColorTable.hash_table = 'a CellsBasis.ColorTable.hash_table
                    and type 'a HashTable.hash_table = 'a CellsBasis.HashTable.hash_table
                    and type SortedCells.sorted_cells = CellsBasis.SortedCells.sorted_cells
                    and type cell = CellsBasis.cell
                    and type cellColor = CellsBasis.cellColor
                    and type cellkind = CellsBasis.cellkind
                    and type cellkindDesc = CellsBasis.cellkindDesc
                    and type cellkindInfo = CellsBasis.cellkindInfo

   (* Generate a virtual register and update the gc info at the same time. *)
   val newCell   : CB.cellkind -> GC.gctype -> CB.cell
   val setGCType : CB.cell * GC.gctype -> unit
   val getGCType : CB.cell -> GC.gctype

   (* Prettty print gc type *)
   val printType : CB.cell -> string

   val GCLIVEOUT : (CB.cell * GC.gctype) list Annotations.property

end
