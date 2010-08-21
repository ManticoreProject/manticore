(* 
 * 
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Hash tables that maps the header Tagbits to id.
 *)

signature HEADER_TABLE = sig
    
    type hdr                 (* representation of the Tagbits *)
    type hdr_tbl             (* hash table mapping Tagbits to id *)

    val new : unit -> hdr_tbl
    val addHdr : (hdr_tbl * hdr) -> int
    val appi : ((hdr * int) -> unit) -> hdr_tbl -> unit    
    val print : (hdr_tbl) -> (hdr * int) list

end (* HEADER_TABLE *)

structure HeaderHash = 
struct
    type hdr = string
    val hash = HashString.hashString 
    val same = (op=) : string * string -> bool
end

functor HeaderTblFn (
	A : 
	sig
	    type hdr
	    val hash : hdr -> word
	    val same : (hdr * hdr) -> bool
	end
) : HEADER_TABLE = struct

  structure Tbl = HashTableFn (struct
			         type hash_key = A.hdr
				 val hashVal = A.hash
				 val sameKey = A.same
			       end)
  type hdr = A.hdr
  type hdr_tbl = int Tbl.hash_table
  
  (* number of predefind objects *)
  val counter = ref 3
  
  (* counter for the ids *)
  fun newid () = let 
    val c = !counter
    in
      counter := c+1;
      c
    end

  fun new () = Tbl.mkTable (32, Fail "LiteralTable")

  fun addHdr (tbl, hdr) = 
      (case Tbl.find tbl hdr
	of NONE => let
	       val id = newid ()
	   in
	       Tbl.insert tbl (hdr, id);
	       id
	   end
	 | (SOME id) => id
      (* end case *))
      
  fun print (tbl) = Tbl.listItemsi tbl

  val appi = Tbl.appi

end (* LiteralTblFn *)