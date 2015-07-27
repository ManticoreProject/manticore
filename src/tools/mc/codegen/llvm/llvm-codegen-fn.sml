(* llvm-codegen-fn.sml
 * 
 * COPYRIGHT (c) 2015 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * 
 *)

functor LLVMCodeGenFn (structure Spec : TARGET_SPEC) :> CODE_GEN = 
struct

	structure LLVMBackend = LLVMPrinter (structure Spec = Spec)

	fun codeGen {code : CFG.module, dst : TextIO.outstream} = let
		val _ = Predecessors.analyze code
	in
		LLVMBackend.output(dst, code)
	end
		
end
