(* llvm-print-util.sml
 *
 * COPYRIGHT (c) 2015 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Misc utility functions/types used by the LLVM Printer.
 *)

functor LLVMPrintUtil (structure Spec : TARGET_SPEC) : sig

    (* CFG representation utilities *)

    type label
    type var
    type linkage

    val cvtLabel : CFG.label -> label

    val linkageOf : CFG.label -> linkage

    val cvtVar : CFG.var -> var

    (* basicBlock, func *)
    val lab2name : label -> string

    (* %basicBlock, @func *)
    val lab2FullName : label -> string

    val var2s : var -> string
    val link2s : linkage -> string


    (* C Function utilities *)

    type c_attr
    val attrOfC : CFunctions.attribute -> c_attr
    val cAttr2s : c_attr -> string

  end = struct

  structure C = CFG
  structure CV = CFG.Var
  structure CL = CFG.Label
  structure CT = CFGTy
  structure CF = CFunctions
  structure S = String
  structure Type = AMD64TypesFn (structure Spec = Spec)


  type label = C.label_kind * string
  type var = CFGTy.ty * string
  type linkage = string
  type c_attr = string  

  (* internal functions *)

  val i2s = Int.toString

  fun mantiName2LLVM(name, id) =  
                   name ^
                   "_" ^
                   (StringCvt.padLeft #"0" 4 (Word.toString (Stamp.hash id)))


  (* end of internal functions *)


  fun cvtLabel (label) = let
      val kind = CL.kindOf label
    in
      (case kind
      of C.LK_Func { export = NONE,
                     func = C.FUNC { lab = VarRep.V{ name, id, ... }, ... } } => (kind, mantiName2LLVM(name, id))
                     
       | C.LK_Func { export = SOME s, ... } => (kind, s)
       | C.LK_Extern s => (kind, s)
       | C.LK_Block (C.BLK { lab = VarRep.V{ name, id, ... }, ... }) => (kind, mantiName2LLVM(name, id))
       (* end case *))
    end

    fun cvtVar (cfgV as VarRep.V{ name, id, ... }) = (CFG.Var.typeOf cfgV, mantiName2LLVM(name, id))

    fun linkageOf (label) = (case CL.kindOf label
        of C.LK_Func { export = NONE, ... } => "internal"
         | C.LK_Func { export = SOME _, ... } => "external"
         | _ => raise Fail ("linkageOf is only valid for manticore functions.")
         (* end case *))

      
    fun attrOfC (a : CF.attribute) = (case a
          of CF.A_pure => "readonly"
           | CF.A_noreturn => "noreturn"
           (* alloc/malloc attribute in C doesn't seem to translate over to LLVM IR *)
           | _ => ""
          (* end case *)) 


    fun lab2FullName (kind, llname) = (case kind
      of C.LK_Block _ => "%" ^ llname
       | _ => "@" ^ llname
       (* end case *))

    fun lab2name (_, llname) = llname

    fun var2s (_, x) = x

    fun link2s x = x
    fun cAttr2s x = x

end
