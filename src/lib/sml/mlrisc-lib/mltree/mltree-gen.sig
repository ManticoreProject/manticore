(*
 * This module provides various generic MLTREE transformations.
 * Basically, we want to support various non built-in datatype widths.
 * This module handles the translation. 
 *
 * -- Allen
 *)
signature MLTREEGEN =
sig

   structure T : MLTREE
   structure Size : MLTREE_SIZE (* where T = T *)
                    where type T.Basis.cond = T.Basis.cond
                      and type T.Basis.div_rounding_mode = T.Basis.div_rounding_mode
                      and type T.Basis.ext = T.Basis.ext
                      and type T.Basis.fcond = T.Basis.fcond
                      and type T.Basis.rounding_mode = T.Basis.rounding_mode
                      and type T.Constant.const = T.Constant.const
                      and type ('s,'r,'f,'c) T.Extension.ccx = ('s,'r,'f,'c) T.Extension.ccx
                      and type ('s,'r,'f,'c) T.Extension.fx = ('s,'r,'f,'c) T.Extension.fx
                      and type ('s,'r,'f,'c) T.Extension.rx = ('s,'r,'f,'c) T.Extension.rx
                      and type ('s,'r,'f,'c) T.Extension.sx = ('s,'r,'f,'c) T.Extension.sx
                      and type T.I.div_rounding_mode = T.I.div_rounding_mode
                      and type T.Region.region = T.Region.region
                      and type T.ccexp = T.ccexp
                      and type T.fexp = T.fexp
                      (* and type T.labexp = T.labexp *)
                      and type T.mlrisc = T.mlrisc
                      and type T.oper = T.oper
                      and type T.rep = T.rep
                      and type T.rexp = T.rexp
                      and type T.stm = T.stm

   val condOf : T.ccexp -> T.Basis.cond
   val fcondOf : T.ccexp -> T.Basis.fcond

   (* 
    * Perform simplification
    *)
   val compileRexp : T.rexp -> T.rexp
   val compileFexp : T.fexp -> T.fexp
   val compileStm  : T.stm  -> T.stm list
  
   (*
    * Simulate conditional expression. 
    *)
   val compileCond : 
       {exp : T.ty * T.ccexp * T.rexp * T.rexp,
        an  : Annotations.annotations,
        rd  : CellsBasis.cell
       } -> T.stm list

   val compileFcond : 
       {exp : T.fty * T.ccexp * T.fexp * T.fexp,
        an  : Annotations.annotations,
        fd  : CellsBasis.cell
       } -> T.stm list


end
