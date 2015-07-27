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
  structure CT = CFGTy
  structure CF = CFunctions
  structure S = String
  structure Type = AMD64TypesFn (structure Spec = Spec)

fun output (outS, module as C.MODULE { name = module_name,
                                       externs = module_externs,
                                       code = module_code } ) = let
  
  (* print utils *)
  fun pr s = TextIO.output(outS, s)
  fun prl s = pr(S.concat s)
  val i2s = Int.toString


  (* links together the attribute number and attribute declaration *)

    (* TODO: rethink this, not flexible enough. *)
  datatype llvm_attributes = MantiFun | CFun

  fun attrNum (MantiFun) = 0
    | attrNum (CFun) = 1

  fun attrStr x = "#" ^ (i2s (attrNum x))

  val attrDecl = S.concat [
    "attributes ", attrStr(MantiFun), " = { naked nounwind }\n",

    (* it seems dangerous to allow inlining into naked funs *)
    "attributes ", attrStr(CFun), " = { noinline }\n"
    ]



  (**)

  (* type stuff *)

  (* we keep everything, including pointers, in the form of a known-width integer
     and perform casts as needed *)
  fun typeName (t : CT.ty) : string = "%i" ^ (Int.toString (Type.szOf t))
  (**)

  
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


  fun mkFunc (f : C.func) : string = ""

  (* end of Functions *)


  (* Module *)
  
  (* in particular, this just generates essentially a "header" for the LLVM module
     with things such as the datatype layouts, externals, attributes and so on. *)
  fun mkModuleHeader () : string = let

    (* external C function *)
    fun toLLVMDecl (CF.CFun { name, retTy, argTys, varArg, attrs, ... }) = let
      
      fun llTy (ct : CF.c_type) : string = (case ct
        of CF.PointerTy => "i8*"
         | CF.BaseTy(RawTypes.T_Float) => "float"
         | CF.BaseTy(RawTypes.T_Double) => "double"
         | CF.BaseTy(rawTy) => "i" ^ (i2s (RawTypes.sizeOf(rawTy) * 8))
         | CF.VoidTy => "void"
        (* end case *))

        val params = List.foldr 
                      (fn (x, nil) => llTy(x) :: nil 
                        | (x, y) => llTy(x) :: ", " :: y)
                      nil
                      argTys
        val params = if not varArg
                      then params
                      else if List.length params > 0
                        then params @ [", ..."]
                        else ["..."]

          (* TODO: the attrs for each function should be added for readonly <-> pure & noreturn
                   we need a better way to generate and keep track of attribute #'s *)
      in
        S.concat (["declare ", (llTy retTy), " @", name, "("] @ params @ [")\n"])
      end





    val arch = (if Spec.archName = "x86_64" 
                then "x86_64-"
                else raise Fail ("Unsupported archicture type: " ^ Spec.archName))

    val (targetTriple, dataLayout) = 
        if Spec.osName = "darwin" 
          then (arch ^ "apple-macosx", "e-m:o-i64:64-f80:128-n8:16:32:64-S128")

          (* QUESTION: should this be pc-darwin instead, or is the only darwin OS we're referring to OS X? *)
          (* might want to specify OS X version, and ensure this data layout matches our needs *)

        else if Spec.osName = "linux"
          then (arch ^ "pc-linux", "unknown")

        else raise Fail ("Unsupported OS type: " ^ Spec.archName)

    val moduleName = Atom.toString module_name

    val externDecls = S.concat (List.map toLLVMDecl module_externs)


      (* TODO: at this point we need to generate a map from manticore vars to llvm vars for use during
         the rest of translation. printf for example is accessed through a manticore var, and the extern
         records we just processed are what make the connection between that and the C function. *)


    val header = S.concat [
      "; Generated by Manticore\n",
      "; ModuleID = '", moduleName, "'\n",
      "target datalayout = \"", dataLayout, "\"\n",
      "target triple = \"", targetTriple, "\"\n\n",
      attrDecl, "\n\n",
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
    pr (mkModuleHeader ()) ;
    List.app (pr o mkFunc) module_code ;
    pr "\n\n\n\n; -------------------------------------- \n\n\n\n" ;
    PrintCFG.output {counts=true, types=true, preds=true} (outS, module) ;
    ()
  )
end

     

end
