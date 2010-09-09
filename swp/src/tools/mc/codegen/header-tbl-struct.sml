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
    structure HeaderTable = HeaderTblFn(HeaderHash)
 
    val header = HeaderTable.new ()
    
    val _ = HeaderTable.addHdr (header, "0")
    val _ = HeaderTable.addHdr (header, "01")
	val _ = HeaderTable.addHdr (header, "1")
    val _ = HeaderTable.addHdr (header, "1010")
    
    (* new Header Table END *)
end
