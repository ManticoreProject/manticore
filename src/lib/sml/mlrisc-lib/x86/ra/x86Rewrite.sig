signature X86REWRITE = sig
  structure I  : X86INSTR
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
  val rewriteUse : I.instruction * CB.cell * CB.cell -> I.instruction
  val rewriteDef : I.instruction * CB.cell * CB.cell -> I.instruction
  val frewriteUse : I.instruction * CB.cell * CB.cell -> I.instruction
  val frewriteDef : I.instruction * CB.cell * CB.cell -> I.instruction
end

