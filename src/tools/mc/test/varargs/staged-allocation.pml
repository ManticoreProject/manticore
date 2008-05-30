structure StagedAllocation =
  struct

    (* kinds of locations for passing C arguments *)
    datatype location_kinds
      = K_GPR                (* general-purpose registers *)
      | K_FPR                (* floating-point registers *)
      | K_MEM                (* memory locations *)

    type width = int
    type reg = int
    type reg_info = (width * reg)
    type counter = int
    datatype block_direction = UP | DOWN
    type slot = (width * location_kinds * int)

    (* locations consist of machine registers, offsets in to overflow blocks, combinations of
     * locations, and narrowed locations.
     *)
    datatype location 
      = REG of reg_info
      | BLOCK_OFFSET of int
      | COMBINE of (location * location)  
      | NARROW of (location * width * location_kinds) 

    type location_info = (width * location * location_kinds)

    (* language for specifying calling conventions *)
    datatype stage 
      = OVERFLOW of (counter * block_direction * int)  (* overflow block (usually corresponds to a runtime stack) *)
      | WIDEN of (width -> width)      
      | CHOICE of ( (slot -> bool) * stage) list     (* choose the first stage whose corresponding 
						      * predicate is true. *)
      | REGS_BY_ARGS of (counter * reg_info list)    (* the first n arguments go into the first n
						      * registers *)
      | ARGCOUNTER of counter
      | REGS_BY_BITS of (counter * reg_info list)    (* the first n bits arguments go into the first 
						      * n bits of registers *)
      | BITCOUNTER of counter                        
      | SEQ of stage list                            (* sequence of stages *)
      | PAD of counter                               (* specifies an alignment (this rule applies even 
						      * for registers) *)      
      | ALIGN_TO of (width -> width)                 (* specifies an alignment *)

    val cntCh = channel ()
    fun cntThd i = (send(cntCh, i); cntThd(i+1))
    val _ = spawn(cntThd 0)

    fun freshCounter () = recv cntCh

  end
