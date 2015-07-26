(* predecessors.sml
 *
 * COPYRIGHT (c) 2015 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure Predecessors : sig

    (* For all basic blocks of the module, predecessor information
       will be filled in. Needed during translation from CFG to SSA
       when creating phi-nodes, but might be handy for other CFG optimizations. *)

    val analyze : CFG.module -> unit

  end = struct

    structure C = CFG
    structure L = CFG.Label

    (* keep it consistent *)
    val setPreds = C.setPreds
    val getPreds = C.getPreds
    val maybeGetPreds = C.maybeGetPreds
    fun addPred (lb : C.label, jmp : C.jump) : unit = setPreds(lb, jmp::getPreds(lb))

    (* key assumption: all of the predecessor props have been cleared/not initialized. *)
    fun analyze (C.MODULE{ code, ... }) = List.app doFunc code

    and doFunc (C.FUNC{ start=C.BLK{lab, exit, ... }, ... }) = 
      (* start block does not have predecessors *)
      ( setPreds(lab, []) ; fly(lab, exit) )

    and fly (source : C.label, xfer : C.transfer) = let

      fun examineJump ((destLab, args) : C.jump) : unit = 
        (case (maybeBlock destLab, haveVisited destLab)
          of (SOME _, true) => 
                (* we've already been to this block, just add src as predecessor *)
                addPred(destLab, (source, args))
           | (SOME (C.BLK{ exit, ...}), false) => 
                (setPreds(destLab, [(source, args)]) ; fly(destLab, exit))
           | _ => () (* we only fly to blocks within the function *)
          (* end case *))

      in
         (case xfer
          of C.Goto jump => examineJump jump
           | C.If (_, jumpA, jumpB) => (examineJump jumpA ; examineJump jumpB)
           | C.Switch (_, branches, maybeJ) => let
               fun process (_, jump) = examineJump jump
             in

               (List.app process branches ; 
                if Option.isSome maybeJ 
                then examineJump(Option.valOf maybeJ)
                else () )

             end
           | C.HeapCheck { nogc, ... } => examineJump nogc 
           | C.HeapCheckN { nogc, ... } => examineJump nogc  
           | C.AllocCCall { ret, ... } => examineJump ret           
           | _ => ()
        (* end xfer case *))
    end

    and haveVisited destLab = 
      (case maybeGetPreds destLab
        of SOME _ => true
         | NONE => false
      (* end case *))

    and maybeBlock destLab = 
      (case L.kindOf destLab
        of C.LK_Block blk => SOME blk
         | _ => NONE
      (* end case *))

  end
