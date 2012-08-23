(* alphaPseudoInstr.sig --- alpha pseudo instructions *)

signature ALPHA_PSEUDO_INSTR = 
sig
   structure I : ALPHAINSTR
   structure T : MLTREE
                 where type Basis.cond = I.T.Basis.cond
                   and type Basis.div_rounding_mode = I.T.Basis.div_rounding_mode
                   and type Basis.ext = I.T.Basis.ext
                   and type Basis.fcond = I.T.Basis.fcond
                   and type Basis.rounding_mode = I.T.Basis.rounding_mode
                   and type Constant.const = I.T.Constant.const
                   and type ('s,'r,'f,'c) Extension.ccx = ('s,'r,'f,'c) I.T.Extension.ccx
                   and type ('s,'r,'f,'c) Extension.fx = ('s,'r,'f,'c) I.T.Extension.fx
                   and type ('s,'r,'f,'c) Extension.rx = ('s,'r,'f,'c) I.T.Extension.rx
                   and type ('s,'r,'f,'c) Extension.sx = ('s,'r,'f,'c) I.T.Extension.sx
                   and type I.div_rounding_mode = I.T.I.div_rounding_mode
                   and type Region.region = I.T.Region.region
                   and type ccexp = I.T.ccexp
                   and type fexp = I.T.fexp
                   (* and type labexp = I.T.labexp *)
                   and type mlrisc = I.T.mlrisc
                   and type oper = I.T.oper
                   and type rep = I.T.rep
                   and type rexp = I.T.rexp
                   and type stm = I.T.stm
   structure C : ALPHACELLS
     (* sharing C = I.C *)
     (* sharing I.T = T *)
   structure CB: CELLS_BASIS (* = CellsBasis *)
                 where type CellSet.cellset = CellsBasis.CellSet.cellset
                   and type 'a ColorTable.hash_table = 'a CellsBasis.ColorTable.hash_table
                   and type 'a HashTable.hash_table = 'a CellsBasis.HashTable.hash_table
                   and type SortedCells.sorted_cells = CellsBasis.SortedCells.sorted_cells
                   and type cell = CellsBasis.cell
                   and type cellColor = CellsBasis.cellColor
                   and type cellkind = CellsBasis.cellkind
                   and type cellkindDesc = CellsBasis.cellkindDesc
                   and type cellkindInfo = CellsBasis.cellkindInfo
  
   type reduceOpnd = I.operand -> CB.cell

   val divlv : {ra:CB.cell, rb:I.operand, rc:CB.cell} * reduceOpnd -> I.instruction list
   val divl  : {ra:CB.cell, rb:I.operand, rc:CB.cell} * reduceOpnd -> I.instruction list
   val divlu : {ra:CB.cell, rb:I.operand, rc:CB.cell} * reduceOpnd -> I.instruction list
   val remlv : {ra:CB.cell, rb:I.operand, rc:CB.cell} * reduceOpnd -> I.instruction list
   val reml  : {ra:CB.cell, rb:I.operand, rc:CB.cell} * reduceOpnd -> I.instruction list
   val remlu : {ra:CB.cell, rb:I.operand, rc:CB.cell} * reduceOpnd -> I.instruction list
   val divqv : {ra:CB.cell, rb:I.operand, rc:CB.cell} * reduceOpnd -> I.instruction list
   val divq  : {ra:CB.cell, rb:I.operand, rc:CB.cell} * reduceOpnd -> I.instruction list
   val divqu : {ra:CB.cell, rb:I.operand, rc:CB.cell} * reduceOpnd -> I.instruction list
   val remqv : {ra:CB.cell, rb:I.operand, rc:CB.cell} * reduceOpnd -> I.instruction list
   val remq  : {ra:CB.cell, rb:I.operand, rc:CB.cell} * reduceOpnd -> I.instruction list
   val remqu : {ra:CB.cell, rb:I.operand, rc:CB.cell} * reduceOpnd -> I.instruction list

   val cvtls : {opnd:I.operand, fd:CB.cell} * reduceOpnd -> I.instruction list
   val cvtlt : {opnd:I.operand, fd:CB.cell} * reduceOpnd -> I.instruction list
   val cvtqs : {opnd:I.operand, fd:CB.cell} * reduceOpnd -> I.instruction list
   val cvtqt : {opnd:I.operand, fd:CB.cell} * reduceOpnd -> I.instruction list

   val cvtsl : {mode:T.rounding_mode, fs:CB.cell, rd:CB.cell} -> I.instruction list
   val cvttl : {mode:T.rounding_mode, fs:CB.cell, rd:CB.cell} -> I.instruction list
   val cvtsq : {mode:T.rounding_mode, fs:CB.cell, rd:CB.cell} -> I.instruction list
   val cvttq : {mode:T.rounding_mode, fs:CB.cell, rd:CB.cell} -> I.instruction list
end 

