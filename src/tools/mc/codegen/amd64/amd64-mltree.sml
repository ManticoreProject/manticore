(* amd64-mltree.sml
 * 
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Specialize the MLRISC tree to the AMD64.
 *)

functor AMD64MLTreeFn (structure AMD64Constant : CONSTANT) =
  struct

    structure AMD64MLTree = MLTreeF (
       structure Constant = AMD64Constant
       structure Region = ManticoreRegion
       structure Extension = AMD64Extension)
    
    structure AMD64Instr = AMD64Instr (AMD64MLTree)
    
    structure AMD64Shuffle = AMD64Shuffle (AMD64Instr)
    
    structure AMD64MLTreeEval = MLTreeEval (
       structure T = AMD64MLTree
       fun eqSext _ _ = raise Fail "eqSext unimplemented"
       fun eqRext _ _ = raise Fail "eqRext unimplemented"
       fun eqFext _ _ = raise Fail "eqFext unimplemented"
       fun eqCCext _ _ = raise Fail "eqCCext unimplemented")
				
    structure AMD64GasPseudoOps = AMD64PseudoOpsFn (
       structure T = AMD64MLTree
       structure MLTreeEval=AMD64MLTreeEval)
    
    structure AMD64Imports = struct
      type import_kind = unit
      fun add _ = ()
      fun output _ = ""
    end
    
    structure AMD64MLTreeHash =  MLTreeHash (
       structure T = AMD64MLTree
       fun h _ _ = 0w0
       val hashRext = h val hashFext = h
       val hashCCext = h val hashSext = h)
    
    structure AMD64MLTreeEval = MLTreeEval (
       structure T = AMD64MLTree
       fun eq _ _ = false
       val eqRext = eq val eqFext = eq
       val eqCCext = eq val eqSext = eq)
    
    structure AMD64Props = AMD64Props (
			    structure Instr = AMD64Instr
			    structure MLTreeHash = AMD64MLTreeHash
			    structure MLTreeEval = AMD64MLTreeEval)
    
    structure AMD64MLTreeUtils : MLTREE_UTILS =
      struct 
	structure T = AMD64MLTree
	structure IX = AMD64InstrExt		 
	structure U = MLTreeUtils (
	    structure T = T
	    fun hashSext _ _ = 0w0
	    fun hashRext _ _ = 0w0
	    fun hashFext _ _ = 0w0
	    fun hashCCext _ _ = 0w0
	    fun eqSext _ _ = raise Fail "eqSext"
	    fun eqRext _ _ = raise Fail "eqRext"
	    fun eqFext _ _ = raise Fail "eqFext"
	    fun eqCCext _ _ = raise Fail "eqCCext"
	    fun showSext (prt : T.printer) ext = (case ext
		   of (IX.PUSHQ rexp) => concat["PUSHL(", #rexp prt rexp, ")"]
		    | (IX.POP rexp) => concat["POP(", #rexp prt rexp, ")"]
		    | IX.LEAVE => "LEAVE"
		    | (IX.RET rexp) => concat["RET(", #rexp prt rexp, ")"]
		    | (IX.LOCK_CMPXCHGL(re1, re2)) => concat[
			  "LOCK_CMPXCHGL(", #rexp prt re1, ",", #rexp prt re2, ")"
			]
		    | (IX.LOCK_CMPXCHGQ(re1, re2)) => concat[
			  "LOCK_CMPXCHGQ(", #rexp prt re1, ",", #rexp prt re2, ")"
			]
		    | (IX.LOCK_XCHGL(re1, re2)) => concat[
			  "LOCK_XCHGL(", #rexp prt re1, ",", #rexp prt re2, ")"
			]
		    | (IX.LOCK_XCHGQ(re1, re2)) => concat[
			  "LOCK_XCHGQ(", #rexp prt re1, ",", #rexp prt re2, ")"
			]
		    | IX.LOCK_XADDL(addr, x) =>
		      concat[
			  "LOCK_XADDL(", #rexp prt addr, ",", #rexp prt x, ")"
		      ]
		    | IX.LOCK_XADDQ(addr, x) =>
		      concat[
			  "LOCK_XADDQ(", #rexp prt addr, ",", #rexp prt x, ")"
		      ]
		    | _ => (print "Warning: cannot format unsupported instruction"; "")
		  (* end case *))
	    fun showRext _ _ = raise Fail "showRext"
	    fun showFext _ _ = raise Fail "showFext"
	    fun showCCext _ _ = raise Fail "showCCext")    
	open U  
      end (* AMD64MLTreeUtils *)
    
    end (* AMD64MLTree *)
