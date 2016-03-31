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
		(* As of 3/30/16, this codegen function assumes that contract has been run on the 
           CFG representation to ensure that there are no basic blocks without predecessors
           in the representation. 
           
           If such BBs exist, this code generator may output phi instructions with 
           variables incoming from the predecessor-less block, but that block will not 
           have defined the variable because it was an argument to to it. *)

        (* a pass that annotated blocks with predecessor info. Decided agianst this route. *)
		(* val _ = Predecessors.analyze code *)

	in
		LLVMBackend.output(dst, code)
	end
		
end
