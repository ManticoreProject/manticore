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
  structure CTU = CFGTyUtil
  structure CF = CFunctions
  structure S = String
  structure U = LLVMPrintUtil

  (*  *)
  structure LV = LLVMVar
  structure LB = LLVMBuilder 

  structure LT = LV.LT
  structure Op = LLVMOp
  

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

    (* TODO: because I'm not sure of the effect inlining a C func into a naked func right now. *)
    | stdAttrs (ExternCFun) = "noinline" 

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

  fun mkFunc (f as C.FUNC { lab, entry, start=(start as C.BLK{ args, ... }), body }) : string = let
    val linkage = (U.link2s o U.linkageOf) lab
    val cc = "" (* TODO(kavon): determine this *)
    val llName = (U.lab2FullName o U.cvtLabel) lab

    val cfgTy = LT.typeOfConv(entry, args)
    val llParamTys = LT.typesInConv cfgTy
    (* TODO(kavon): add a check to ensure # of GPR <= arity. Spec currently lists
                    the max number of GPRs for args, not total with pinned regs *)
    val comment = S.concat ["; ", CTU.toString cfgTy, "\n",
                            "; arity = ", i2s(List.length llParamTys), "\n" ] 

    val llparams = mapSep(LT.nameOf, nil, ", ", llParamTys)

                      (* TODO: get the arg list from the starting block.
                               also, the start block should be treated specially
                               when we output it here.
                               we also probably need a rename environment? *)
    val decl = [comment, "define ", linkage, " void ", llName, "("] @ llparams @ [") ", stdAttrs(MantiFun), " {\n"]
    
    (* testing llvm bb generator *)
    val t = LB.new(LV.new("entry", LT.labelTy))
      (* helpers *)
      val intTy = LT.mkInt(LT.cnt 32)
      fun mkInt i = LB.fromC(LB.intC(intTy, i))
      val bop = LB.bop t
      val ret = LB.ret t
      val uop = LB.uop t

    val bb = ret (uop Op.Neg ((bop Op.Add (mkInt 10, mkInt 200))))
    
    val done = LB.toString bb

    val body = [
      done
    ]


     (* List.map mkBasicBlock (start::body) *)

    val total = S.concat (decl @ body @ ["\n}\n\n"])
  in
    total
  end

  (* end of Functions *)


  (* Module *)
  
  (* in particular, this just generates essentially a "header" for the LLVM module
     with things such as the datatype layouts, externals, attributes and so on.
     it also initializes the extern info map. *)
  fun mkFunDecls () : string = let

    (* external C function *)
    fun toLLVMDecl (CF.CFun { var, name, retTy, argTys, varArg, attrs }) = let

        val c2ll = LT.nameOf o LT.typeOfC

        val llvmParams = mapSep(c2ll, nil, ", ", argTys)

        val llvmParams = if not varArg
                      then llvmParams
                      else if List.length llvmParams > 0
                        then llvmParams @ [", ..."]
                        else ["..."]

        val llvmAttrs = mapSep(U.attrOfC, [stdAttrs(ExternCFun)], " ", attrs)

        (* record this for translation later *)
        val _ = externInfoAdd(var, name)

      in
        S.concat (["declare ", (c2ll retTy), " @", name, "("] 
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

    val externDecls = S.concat (List.map toLLVMDecl module_externs)

    val header = S.concat [
      "target datalayout = \"", dataLayout, "\"\n",
      "target triple = \"", targetTriple, "\"\n\n",
      externDecls
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

  (* process the whole module, generating a string for each function and populating the type
     and string literal caches *)
  val funStrings = List.map mkFunc module_code  

in
  ( (* output sequence *)
    
    (* header *)
    pr (S.concat 
        ["; Generated by Manticore\n",
         "; ModuleID = '", Atom.toString module_name, "'"]) ;

    (* types need to go first, because they must be declared before used in functions etc*)
    pr "\n\n; type decls\n\n" ;
    pr (LT.typeDecl()) ;  

    pr "\n\n; externs & target info\n\n" ;
    pr (mkFunDecls ()) ; (* declare extern funs, target triple, and datalayout *)

    pr "\n\n; manticore function defs\n\n" ;
    List.app pr funStrings ;

    pr "\n\n\n\n; ---------------- end of LLVM generation ---------------------- \n\n\n\n" ;
    PrintCFG.output {counts=true, types=true, preds=true} (outS, module) ;
    ()
  )

end

     

end
