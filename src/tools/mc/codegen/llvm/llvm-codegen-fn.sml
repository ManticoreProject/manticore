(* llvm-codegen-fn.sml
 * 
 * COPYRIGHT (c) 2015 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * 
 *)

functor LLVMCodeGenFn (structure Spec : TARGET_SPEC) :> CODE_GEN = 
struct

	fun codeGen {code: CFG.module, dst: TextIO.outstream} =
		raise Fail "LLVM Backend is not implemented yet!"
		
end
