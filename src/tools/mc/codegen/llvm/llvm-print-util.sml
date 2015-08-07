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
    type ty

    val cvtLabel : CFG.label -> label

    val linkageOf : CFG.label -> linkage

    val cvtVar : CFG.var -> var

    val cvtType : CFGTy.ty -> ty

    (* basicBlock, func *)
    val lab2name : label -> string

    (* %basicBlock, @func *)
    val lab2FullName : label -> string

    val var2s : var -> string
    val link2s : linkage -> string
    val ty2s : ty -> string


    (* C Function utilities *)

    type c_ty
    type c_attr

    val typeOfC : CFunctions.c_type -> c_ty
    val attrOfC : CFunctions.attribute -> c_attr

    val cAttr2s : c_attr -> string
    val cTy2s : c_ty -> string

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
  type ty = string
  type c_ty = string
  type c_attr = string

  

  (* internal functions *)

  val i2s = Int.toString

  fun mantiName2LLVM(name, id) =  
                   name ^
                   "_" ^
                   (StringCvt.padLeft #"0" 4 (Word.toString (Stamp.hash id)))


  (* end of internal functions *)


  fun cvtType_INPROGRESS (t : CT.ty) : string = (case t
      of CT.T_Any => "i8*"    (* the void* of llvm *)

       (* TODO(kavon): a RawType.T_Vec128 should really be represented as an LLVM vector type if we want SSE,
                       however, that requires knowing what the type of its elements are. For example,
                       in LLVM this Vec128 might be a <4 x i32> or <2 x i64>. *)
       | CT.T_Raw rawTy => "i" ^ (i2s ((RawTypes.sizeOf rawTy) * 8))

       | CT.T_Addr cfgTy => (cvtType cfgTy) ^ "*"


       | CT.T_Tuple (isMutable, memberTys) => 
       (* TODO(kavon): this brings up an interesting point. 
          we want to use GEP instructions in order to please the alias analysis and other optimization passes,
          so we also want to use array or structure types wherever we can instead of i8* and inttoptr/ptrtoint
          , however, the types must all be the same for arrays, so some tuples can map to arrays but mixed type ones cannot.

          Something we need to do is generate structure types as we encounter mixed tuples, and maintain a 
          store of previously generated tuple type combinations to cut down on the amount of junk we're generating.

          A question for LLVM folks is "what differences are there when it comes to using arrays vs structs in LLVM
           other than the packing style? (aka, optimization or codegen differences)"


        *)


          if List.foldl CFGTyUtil.equal true memberTys then
            "[" ^ (i2s (List.length memberTys)) ^ " x " ^ (cvtType (List.hd(memberTys))) ^ "]"
          else
            "i8*"

       | CT.T_Block _ => raise Fail "how should this be handled, if at all?"

       | _ => raise Fail "unknown type in llvm print util!"
    )

    "%i" ^ (i2s (Type.szOf t))



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


    fun typeOfC (ct : CF.c_type) : string = (case ct
          of CF.PointerTy => "i8*"
           | CF.BaseTy(RawTypes.T_Float) => "float"
           | CF.BaseTy(RawTypes.T_Double) => "double"
           | CF.BaseTy(rawTy) => "i" ^ (i2s (RawTypes.sizeOf(rawTy) * 8))
           | CF.VoidTy => "void"
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
    fun ty2s x = x
    fun cAttr2s x = x
    fun cTy2s x = x

end
