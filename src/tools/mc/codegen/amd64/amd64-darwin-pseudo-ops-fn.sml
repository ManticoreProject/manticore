(* amd64-darwin-pseudo-ops-fn.sml
 * 
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * The AMD64 pseudo operations for Linux, which uses the gas syntax.
 *)

functor AMD64PseudoOpsFn (
    structure T : MLTREE
    structure MLTreeEval : MLTREE_EVAL (* where T = T *)
          where type T.cond = T.cond
          where type T.fcond = T.fcond
          where type T.rounding_mode = T.rounding_mode
          where type T.div_rounding_mode = T.div_rounding_mode
          where type T.ext = T.ext
          where type T.stm = T.stm
          where type T.rexp = T.rexp
          where type T.rep = T.rep
          where type T.oper = T.oper
          where type T.fexp = T.fexp
          where type T.ccexp = T.ccexp
          where type T.mlrisc = T.mlrisc
          where type T.Constant.const = T.Constant.const
          and type ('s,'r,'f,'c) T.Extension.ccx = ('s,'r,'f,'c) T.Extension.ccx
          and type ('s,'r,'f,'c) T.Extension.fx = ('s,'r,'f,'c) T.Extension.fx
          and type ('s,'r,'f,'c) T.Extension.rx = ('s,'r,'f,'c) T.Extension.rx
          and type ('s,'r,'f,'c) T.Extension.sx = ('s,'r,'f,'c) T.Extension.sx
          and type T.I.div_rounding_mode = T.I.div_rounding_mode
          and type T.Region.region = T.Region.region

  ) : PSEUDO_OPS_BASIS = AMD64DarwinPseudoOps (
    structure T = T
    structure MLTreeEval = MLTreeEval)
