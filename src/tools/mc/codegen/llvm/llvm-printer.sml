(* llvm-printer.sml
 *
 * COPYRIGHT (c) 2015 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Outputs a CFG program as textual LLVM IR. 
 *    - Depends on the predecessor CFG pass.
 *    - Compatible with LLVM 3.7
 *)

functor LLVMPrinter (structure Spec : TARGET_SPEC) : sig

    val output : (TextIO.outstream * CFG.module) -> unit

  end = struct

    (*

      Plan: since CFG is basically in SSA form, the main things we need to
            keep track of are the pinned register values (allocation ptr,
            vproc, limit ptr, etc) as they change and are changed by various
            actions. For everything else we ought to be able to just reuse the
            vars and not keep track of those things. All of the
            information needed seems to be otherwise already present in the CFG
            representation.

            Another difference is the way we generate heap checks. since we
            need to spill and reload live vars in the case of a GC occuring,
            along with having the std regs change in that case, we need to
            introduce extra GC bbs for the GCs occuring and introduce phis
            for the following block. An additional optimization we talked about
            was to mark such blocks as cold paths so they're not stuck in the middle
            of a hot path.

      *)

  structure C = CFG
  structure CV = CFG.Var
  structure CL = CFG.Label
  structure CT = CFGTy
  structure CF = CFunctions
  structure S = String
  structure Type = AMD64TypesFn (structure Spec = Spec)

fun output (outS, module as C.MODULE { name = module_name,
                                       externs = module_externs,
                                       code = module_code } ) = let
  
  (* print/string utils *)
  fun pr s = TextIO.output(outS, s)
  fun prl s = pr(S.concat s)
  val i2s = Int.toString

  fun mapSep(f, init, sep, lst) = List.foldr 
                      (fn (x, nil) => f(x) :: nil 
                        | (x, y) => let val fx = f(x) in
                          if fx = "" (* skip empty strings *)
                          then y
                          else fx :: sep :: y
                        end)
                      init
                      lst



  (* links together the attribute number and the standard attribute list *)

  datatype llvm_attributes = MantiFun | ExternCFun

  fun stdAttrs (MantiFun) = "naked nounwind"

    (* because I'm not sure of the effect inlining a C func into a naked func right now. *)
    | stdAttrs (ExternCFun) = "noinline" 

  (**)



  (* type stuff *)

  (* we keep everything, including pointers, in the form of a known-width integer
     and perform casts as needed *)
  fun typeName (t : CT.ty) : string = "%i" ^ (Int.toString (Type.szOf t))
  (**)


  (* translation utils *)
  local
    (* TODO: this might be pointless because one can access the exported
       name through the label once its encountered. *)
    val externInfo = ref CL.Map.empty
  in

    fun externInfoAdd (v : C.label, s : string) : unit = 
      externInfo := CL.Map.insert(!externInfo, v, s)

    fun externInfoGet (v : C.label) : string = 
      (case CL.Map.find(!externInfo, v)
        of SOME s => s
         | NONE => 
            raise Fail ("Unable to find extern name associated with var " ^ (CL.toString v))
      (* end case *))

  end

  
  (* Terminators, aka transfers in CFG *)

  fun mkTransfer (t : C.transfer) = (case t

    of (C.Switch _) => raise Fail "implement me"

    (* this will require inspecting the Prim.cond and generating the test as well *)
     | (C.If _) => raise Fail "implement me"

     (* br *)
     | (C.Goto _) => raise Fail "implement me"


     (* see above. also, need to figure out the difference between these two. *)
     | (C.HeapCheck _) => raise Fail "implement me"
     | (C.HeapCheckN _) => raise Fail "implement me"


     (* generate musttail calls *)
     | (C.StdApply _) => raise Fail "implement me"
     | (C.StdThrow _) => raise Fail "implement me"
     | (C.Apply _) => raise Fail "implement me"

     | _ => raise Fail "not sure how to handle AllocCCall right now "

    (* end case *))

  (* end of Terminators *)


  (* Basic Blocks *)

  fun mkBody (exps : C.exp list) : string = ""

  fun mkBasicBlock (b : C.block) : string = ""

  (* end of Basic Blocks *)

  (* Functions *)

  (* resolves the LLVM name assigned to a specific label kind  *)
  local
    (* lifted part of this from Stamp *)
    fun manti2LLVM(name, id) =  
                     name ^
                     "_" ^
                     (StringCvt.padLeft #"0" 4 (Word.toString (Stamp.hash id)))
  in
    fun getLabelName (labelK) = (case labelK
      of C.LK_Func { export = NONE,
                     func = C.FUNC { lab = VarRep.V{ name, id, ... }, ... } } => "@" ^ manti2LLVM(name, id)
                     
       | C.LK_Func { export = SOME s, ... } => "@" ^ s
       | C.LK_Extern s => "@" ^ s
       | C.LK_Block (C.BLK { lab = VarRep.V{ name, id, ... }, ... }) => manti2LLVM(name, id)
       (* end case *))
  end

  local
    fun getLinkage (labelK) = (case labelK
      of C.LK_Func { export = NONE, ... } => "internal"
       | C.LK_Func { export = SOME _, ... } => "external"
       | _ => raise Fail ("getLinkage is only valid for manticore functions.")
       (* end case *))
  in
    fun mkFunc (f as C.FUNC { lab, ... }) : string = let
      val labelK = CL.kindOf lab

      val linkage = getLinkage (labelK)
      val cc = "" (* TODO: determine this *)
      val llName = getLabelName (labelK)



      val decl = S.concat ["define ", linkage, " void ", llName, "() ", stdAttrs(MantiFun), " {}\n"]
    in
      decl
    end
  end

  (* end of Functions *)


  (* Module *)
  
  (* in particular, this just generates essentially a "header" for the LLVM module
     with things such as the datatype layouts, externals, attributes and so on.
     it also initializes the extern info map. *)
  fun mkModuleHeader () : string = let

    (* external C function *)
    fun toLLVMDecl (CF.CFun { var, name, retTy, argTys, varArg, attrs }) = let
      
      fun llTy (ct : CF.c_type) : string = (case ct
        of CF.PointerTy => "i8*"
         | CF.BaseTy(RawTypes.T_Float) => "float"
         | CF.BaseTy(RawTypes.T_Double) => "double"
         | CF.BaseTy(rawTy) => "i" ^ (i2s (RawTypes.sizeOf(rawTy) * 8))
         | CF.VoidTy => "void"
        (* end case *))

      fun llAttr (a : CF.attribute) = (case a
        of CF.A_pure => "readonly"
         | CF.A_noreturn => "noreturn"
         (* alloc/malloc attribute in C doesn't seem to translate over to LLVM IR *)
         | _ => ""
        (* end case *))

        val llvmParams = mapSep(llTy, nil, ", ", argTys)

        val llvmParams = if not varArg
                      then llvmParams
                      else if List.length llvmParams > 0
                        then llvmParams @ [", ..."]
                        else ["..."]

        val llvmAttrs = mapSep(llAttr, [stdAttrs(ExternCFun)], " ", attrs)

        (* record this for translation later *)
        val _ = externInfoAdd(var, name)

      in
        S.concat (["declare ", (llTy retTy), " @", name, "("] 
                  @ llvmParams @ [") "]
                  @ llvmAttrs @ ["\n"])
      end

    val arch = (case Spec.archName
      of "x86_64" => "x86_64-"
       | _ => raise Fail ("Unsupported archicture type: " ^ Spec.archName)
      (* end case *))

    val (targetTriple, dataLayout) = (case Spec.osName
      (* QUESTION: should this be pc-darwin instead, or is the only darwin OS we're referring to OS X? *)
      (* might want to specify OS X version, and ensure this data layout matches our needs *)
      of "darwin" => (arch ^ "apple-macosx", "e-m:o-i64:64-f80:128-n8:16:32:64-S128")
       | "linux" => (arch ^ "pc-linux", "unknown")
       | _ => raise Fail ("Unsupported OS type: " ^ Spec.archName)
      (* end case *))

    val moduleName = Atom.toString module_name

    val externDecls = S.concat (List.map toLLVMDecl module_externs)

    val header = S.concat [
      "; Generated by Manticore\n",
      "; ModuleID = '", moduleName, "'\n",
      "target datalayout = \"", dataLayout, "\"\n",
      "target triple = \"", targetTriple, "\"\n\n",
      externDecls, "\n\n"
       ]

    in
      header
    end

  (* end of Module *)




(* Notes:
    
      ordering of declarations only matters in LLVM for types.
        
        so, string constants need to be saved as we generate the module, and then we can
          shove them at the end of processing the functions.

      *)

in
  ( (* output sequence *)
    pr (mkModuleHeader ()) ; (* handles the externs as well *)
    List.app (pr o mkFunc) module_code ;
    pr "\n\n\n\n; -------------------------------------- \n\n\n\n" ;
    PrintCFG.output {counts=true, types=true, preds=true} (outS, module) ;
    ()
  )
end

     

end
