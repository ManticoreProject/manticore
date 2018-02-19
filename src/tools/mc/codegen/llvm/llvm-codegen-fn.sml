(* llvm-codegen-fn.sml
 * 
 * COPYRIGHT (c) 2015 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * 
 *)

functor LLVMCodeGenFn (structure Spec : TARGET_SPEC) :> CODE_GEN = 
struct

	structure LLVMBackend = LLVMTranslator (structure Spec = Spec)

	fun doCodeGen {code : CFG.module, dst : TextIO.outstream} = 
		LLVMBackend.output(dst, code)
    
    val codeGen : {code: CFG.module, dst: TextIO.outstream} -> unit =
	  BasicControl.mkTracePassSimple {
	      passName = "codeGen (llvm)",
	      pass = doCodeGen
	    }
		
end
