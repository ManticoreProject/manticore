(* llvm-var.sml
 *
 * COPYRIGHT (c) 2015 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Utility for managing/generating LLVM vars
 *)

functor LLVMVar (structure Spec : TARGET_SPEC) =
 
 struct

    (* when debugging, it is nice to know which CFG variables the LLVM variables correspond with,
       so if this is true we include CFG stamps with the LLVM variable names that are internal or
       global but not exported. *)
    val debuggingVariableNames = true


    structure LT = LLVMType (structure Spec = Spec)
    structure C = CFG
    structure CL = CFG.Label
    structure CV = CFG.Var

    (* simply indicates whether this var is considered a global or local identifier.
       In LLVM, global kinds start with a @ and are for function names and global variables,
       whereas local kinds start with a % and are for register names and types *)
    datatype var_kind =
    (* is this an externed global? if so identOf will not attach the unique stamp *)
        VK_Global of bool 
      | VK_Local

    fun varKindToString (VK_Global _) = "Global"
      | varKindToString VK_Local = "Local"

    local
      structure V = VarFn (
        struct
          type kind = var_kind
          type ty = LT.ty
          val defaultKind = VK_Local
          val kindToString = varKindToString
          val tyToString = LT.nameOf
        end)
    in


    open V


    local 

      (* regex for LLVM identifiers: [%@][-a-zA-Z$._][-a-zA-Z$._0-9]*  *)
      fun inspector c = let
          val repl = "-"
        in
          (case c
          (* best attempt at covering all of the disallowed chars that
             might appear in CFG identifiers, based on SML'97 defn  *)
          of #"!" => repl
           | #"%" => repl
           | #"&" => repl
           | #"#" => repl
           | #"+" => repl
           | #"/" => repl
           | #":" => repl
           | #"<" => repl
           | #"=" => repl
           | #">" => repl
           | #"?" => repl
           | #"@" => repl
           | #"\\" => repl 
           | #"~" => repl
           | #"'" => repl
           | #"^" => repl
           | #"|" => repl
           | #"*" => repl
           | c => Char.toString c
          (* esac *)) 
        end

      val cvtIllegal = String.translate inspector

      fun llvmIdent (name, id) = name ^ "_" ^ (StringCvt.padLeft #"0" 4 (Word.toString (Stamp.hash id))) 

    in

        (* use this to get a valid, correct LLVM identifier with the @ or % qualifier  *)
      fun toString x = (case kindOf x
        of VK_Global true => "@" ^ identOf x
         | VK_Global false => "@" ^ identOf x
         | VK_Local => "%" ^ identOf x
        (* esac *)) 

      (* get just the identifer of this var, aka, toString without the @ or % *)
      and identOf (x as VarRep.V{name, id, ...}) = (case kindOf x
        of VK_Global true => name
         | _ => llvmIdent(name, id)
        (* esac *)) 


      fun convertWithKind ((cfgVar as VarRep.V{id,...} ): CFG.var, kynd) = let
          val llvmTy = LT.typeOf(CV.typeOf cfgVar)
          val llvmName = maybeDebugging(id, cvtIllegal (CV.nameOf cfgVar))
        in
          newWithKind(llvmName, kynd, llvmTy)
        end

      and convert (cfgVar : CFG.var) = convertWithKind(cfgVar, VK_Local)

      and maybeDebugging (id, name) = 
        if debuggingVariableNames 
          then name ^ "_cfg" ^ (Word.toString (Stamp.hash id))
          else name

      (* we need to check whether the label is exported, an external, or a basic block to determine the right kind *)
      and convertLabel (label : CFG.label) = let
          val k = CL.kindOf label
        in
          (case k
          of C.LK_Func { export = NONE,
                         func = C.FUNC { lab = VarRep.V{ name, id, ... }, ... } } => 

                          newWithKind(maybeDebugging(id, cvtIllegal name), VK_Global false, LT.typeOf(CL.typeOf label))
                         
           | C.LK_Func { export = SOME s, ... } => newWithKind(cvtIllegal s, VK_Global true, LT.typeOf(CL.typeOf label))
           
                            (* NOTE(kavon): C extern identifiers should already be LLVM compatible *)
           | C.LK_Extern s => newWithKind(cvtIllegal s, VK_Global true, LT.typeOf(CL.typeOf label))
           
           | C.LK_Block (C.BLK { lab = VarRep.V{ name, id, ... }, ... }) => newWithKind(maybeDebugging(id, cvtIllegal name), VK_Local, LT.typeOf(CL.typeOf label))
           (* end case *))
        end

    end

    



    end (* local *)
end
