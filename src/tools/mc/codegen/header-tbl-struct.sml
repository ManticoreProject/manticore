(* 
 * 
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Hash tables that maps the header Tagbits to id.
 *)

    (* new Header Table *)
    
structure HeaderTableStruct =
struct

    (* NOTE number of predefined table entries.
       by pre-defined, we mean "non-mixed header entries". 
       
       This is a very important value, as it shifts the starting point of
       header IDs corresponding to mixed type tuples.
       
       If you change this value, you must also update the integer literal
       called "predefined" in the following places:
       
       1. the C runtime system, in alloc.c
       
       
    *)
    val predefined = 5

    structure HeaderConfig = struct
        type hdr = HeaderHash.hdr
        val hash = HeaderHash.hash
        val same = HeaderHash.same
        val startID = predefined
    end

    structure HeaderTable = HeaderTblFn(HeaderConfig)
 
    val header = HeaderTable.new ()
    
    (* NOTE: these constant bit-tags are here so that the C runtime system
       can allocate certian mixed-header tuples. See AllocNonUniform in alloc.c
       for the correspondence. Do not change the order of these calls!  *)
    val _ = HeaderTable.addHdr (header, "0")
    val _ = HeaderTable.addHdr (header, "01")
	val _ = HeaderTable.addHdr (header, "1")
    val _ = HeaderTable.addHdr (header, "11010")
    val _ = HeaderTable.addHdr (header, "100")
    
    (* new Header Table END *)
end
