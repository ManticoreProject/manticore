(* predecessors.sml
 *
 * COPYRIGHT (c) 2015 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure Predecessors : sig

    (* For all basic blocks of the module, predecessor block information
       will be filled in. 
        
        This info can be accessed via CFG.getPreds, etc.
    *)

    val analyze : CFG.module -> unit
    
    val clear : CFG.module -> unit

  end = struct

    structure C = CFG
    structure L = CFG.Label
    structure S = String

    (* keep it consistent *)
    val setPreds = C.setPreds
    val getPreds = C.getPreds
    val maybeGetPreds = C.maybeGetPreds
    fun addPred (lb : C.label, tgt : C.label) : unit = setPreds(lb, tgt::(getPreds lb))

    (* key assumption: all of the predecessor props have been cleared/not initialized. *)
    fun analyze (C.MODULE{ code, ... }) = List.app doFunc code

    and doFunc (C.FUNC{ start=C.BLK{lab, exit, ... }, ... }) = 
      (* start block does not have predecessors *)
      ( setPreds(lab, []) ; chk (lab, exit) )

    and chk (source : C.label, xfer : C.transfer) = let

      fun examineJump (destLab : C.label) : unit = (
          print (S.concat["saw edge: ", L.toString destLab, " <- ", L.toString source, "\n"]) ;
        if haveVisited destLab
        then addPred (destLab, source)
        else (setPreds(destLab, [source]) ; chk (destLab, getExitOf destLab)))
      in
        List.app examineJump (CFGUtil.labelsOfXfer xfer)
    end

    and haveVisited destLab = 
      (case maybeGetPreds destLab
        of SOME _ => true
         | NONE => false
      (* end case *))

    and getExitOf destLab =
      (case L.kindOf destLab
        of C.LK_Block (C.BLK{ exit, ...}) => exit
         | _ => raise Fail "not a block"
      (* end case *))


      fun clear (C.MODULE{ code, ... }) = List.app clrFunc code
      and clrFunc (C.FUNC{start, body, ...}) = List.app clrBlk (start::body)
      and clrBlk (C.BLK{lab, ...}) = C.clrPreds lab

  end
