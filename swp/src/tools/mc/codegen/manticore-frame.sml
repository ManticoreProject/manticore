(* manticore-frame.sml
 * 
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Handles stack frames for calls and spills.
 *)

structure ManticoreFrame = struct

  datatype loc = Float of int
	       | Word of int
	       | Stk of {id : int, offset : int}

  fun locToString (Float i) = Format.format "Float(%d)" [Format.INT i]
    | locToString (Word i) = Format.format "Word(%d)" [Format.INT i]
    | locToString (Stk{id, offset=0}) = Format.format "Stk(%d)" [Format.INT id]
    | locToString (Stk{id, offset}) =
      if (offset < 0)
      then Format.format "Stk(%d)-%d" [Format.INT id, Format.INT(~offset)]
      else Format.format "Stk(%d)+%d" [Format.INT id, Format.INT offset]
	   
  (* compute a hash value for a location. *)
  fun hashLoc (Float i) = Word.fromInt i + 0w1
    | hashLoc (Word i) = Word.fromInt i + 0w101
    | hashLoc (Stk{id, ...}) = Word.fromInt id + 0w137

  (* map RA spill locations and stack-alloc IDs to frame positions *)
  type slot_map = int IntHashTable.hash_table

  type frame_sz_info = {
       argSz : int,	
       resSz : int,
       reserved   : int ref,	(* bytes reserved for extra tail-call args *)
       numWords   : int ref,	(* number of non-float spills *)
       numFloats  : int ref,	(* number of float spills *)
       szStkAlloc : int ref,	(* number of bytes of stack allocation *)
       wordsLoc   : slot_map,	(* RA loc to frame position *)
       floatsLoc  : slot_map,	(* RA loc to frame position *)
       stkLoc     : slot_map	(* stack-alloc ID to frame position *)
  }
		       
  fun newFrameSzInfo {argSz, resSz} = {
      argSz	= argSz,
      resSz	= resSz,
      reserved	= ref 0,
      numWords	= ref 0,
      numFloats	= ref 0,
      szStkAlloc	= ref 0,
      wordsLoc	= IntHashTable.mkTable (16, Fail "wordsLoc"),
      floatsLoc	= IntHashTable.mkTable (8, Fail "floatsLoc"),
      stkLoc	= IntHashTable.mkTable (4, Fail "stkLoc")
  }

  fun record (info : frame_sz_info, loc) = let
      val (table, count, raLoc) = 
	  (case loc 
	    of Float i => (#floatsLoc info, #numFloats info, i)
	     | Word  i => (#wordsLoc  info, #numWords  info, i)
	     | Stk _ => raise Fail "stack location"
	  (* end case *))
  in
      case IntHashTable.find table raLoc
       of NONE => (
	  count := !count + 1;
	  IntHashTable.insert table (raLoc, !count))
	| (SOME _) => ()
    (* end case *);
      loc
  end
					   
  fun recordSpill (info : frame_sz_info, i) = record (info, Word i)
  fun recordFSpill (info : frame_sz_info, i) = record (info, Float i)
			     
end (* ManticoreFrame *)
