(* arch-spill-instr.sig
 *
 * COPYRIGHT (c) 2002 Bell Labs, Lucent Technologies
 *
 * Architecture specific instructions to emit when spilling an instruction.
 *)

(* TODO: Some day, all these interface functions will be sensitive to
 * the size being spilled or reloaded --- but today is not the day!
 *)
signature ARCH_SPILL_INSTR = sig
  structure I : INSTRUCTIONS
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
  
  val spillToEA :
      CB.cellkind ->
         CB.cell * I.ea -> 
            {code:I.instruction list, proh:CB.cell list, newReg:CB.cell option}

  val reloadFromEA :
      CB.cellkind ->
         CB.cell * I.ea ->
            {code:I.instruction list, proh:CB.cell list, newReg:CB.cell option}	   

  val spill : 
      CB.cellkind -> 
         I.instruction * CB.cell * I.ea -> 
	    {code:I.instruction list, proh:CB.cell list, newReg:CB.cell option}
  val reload : 
      CB.cellkind ->
         I.instruction * CB.cell * I.ea -> 
		 {code:I.instruction list, proh:CB.cell list, newReg:CB.cell option}
end