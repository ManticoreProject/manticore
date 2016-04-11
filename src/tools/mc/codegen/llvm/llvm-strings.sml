(* llvm-strings.sml
 * 
 * COPYRIGHT (c) 2016 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Infrastructure to translate string literals into LLVM array initializers
 * and to collect all such literals in order to turn them into valid LLVM decls.
 * We use a rather side-effecty way of doing this, in that lookup will modify interally
 * managed state.
 *)

structure LLVMStrings =
  struct
  
  
  local
    structure LV = LLVMVar
    structure LT = LLVMType

    
    
    
  in
  
  (* If the literal is not already contained, we will generate a new one. *)
  fun lookup (s : string) : LV.var = raise Fail "not implemented"
  
  (* purge all strings contained. *)
  fun clear () : unit = raise Fail "not implemented"
  
  (* turn all string literals into a list of LLVM string literal declarations  *)
  fun export () : string list = raise Fail "not implemented"
  
    
  end
  
  end (* LLVMStrings *)
