signature HPPA_MILLICODE = sig
  structure I : HPPAINSTR
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

  val divu : {rs:CB.cell, rt:CB.cell, rd:CB.cell} -> I.instruction list
  val mulo : {rs:CB.cell, rt:CB.cell, rd:CB.cell} -> I.instruction list
  val divo : {rs:CB.cell, rt:CB.cell, rd:CB.cell} -> I.instruction list
  val mulu : {rs:CB.cell, rt:CB.cell, rd:CB.cell} -> I.instruction list
  val cvti2s : {rs:CB.cell, fd:CB.cell} -> I.instruction list
  val cvti2d : {rs:CB.cell, fd:CB.cell} -> I.instruction list
  val cvti2q : {rs:CB.cell, fd:CB.cell} -> I.instruction list
end

