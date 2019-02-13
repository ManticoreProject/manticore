(* absorb-heap-checks.sml
 *
 * COPYRIGHT (c) 2019 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

 structure AbsorbHeapChecks : sig

    (*
       If linked-frames are in use, then the prologue emitted
       by LLVM will already contain a heap check.

       Thus, this pass will try to drop any redundant heap tests
       that appear near the function entry, and pass along this
       information to the LLVM code generator so that the amount
       of allocation that was needed is absorbed into the prologue.
    *)

     val transform : CFG.module -> CFG.module

     (* Given a CFG function label, returns the number of bytes
        that the prologue should additionally check for in the local heap.

        NONE indicates that no check was absorbed, while SOME w indicates
        that a check was absorbed, so the prologue must have some sort of
        test, even for 0 bytes. *)
     val prologueAlloc : CFG.label -> word option

end = struct

  structure ST = Stats
  structure C = CFG
  structure CL = CFG.Label
  structure CFA = CFACFG
  structure L = List

  val cntElimCheck  = ST.newCounter "absorb-heap-checks:elim"

  local
      val {getFn : CFG.label -> word option, setFn, ...} =
          CFG.Label.newProp (fn l => NONE)
  in
      val prologueAlloc = getFn

      fun addPrologueAlloc (lab, sz) = setFn (lab, SOME (
        case prologueAlloc lab
          of NONE => sz
           | SOME n => n + sz
        ))
  end

  (* the simple yet effective approach is to just check the entry block.
      we could get more fancy, but this covers most of the cases. *)
  fun doFn (f as C.FUNC{lab, entry, start, body}) = let
        (* we're inspecting the start block and its successors *)
        val C.BLK{lab = startLab, exit = startExit,...} = start

        val tgts = startLab :: nil (* see FIXME below: getSuccLabs startExit *)
        val allBlocks = start :: body

        val maybeRepls = map (tryDrops (lab, tgts)) allBlocks
        val newStart :: newBody = ListPair.mapEq getOpt (maybeRepls, allBlocks)
    in
      C.FUNC{lab=lab, entry=entry,
              start = newStart, body = newBody }
    end

  and tryDrops (fnLab, tgts) blk =
      if matchingLab tgts blk
        then tryDrop (fnLab, blk)
        else NONE

  and matchingLab set (C.BLK{lab,...}) =
    L.exists (fn x => CL.same(x, lab)) set

  and tryDrop (fnLab, blk as C.BLK{lab, exit, ...}) =
    (* FIXME: this condition is not enough to be safe to perform correct
        absorption of successors of the start block.
        Requires more work, but I'm leaving my attempt here. - kavon *)
    (*
      if length (C.getPreds lab) >= 2
          orelse CFA.isEscaping lab orelse CFA.hasUnknownReturn lab
      then NONE (* we can't absorb safely (e.g., can be part of cycle) *)
      else
      *)
      (case exit
       of C.HeapCheck {hck = C.HCK_Local, szb, nogc} =>
              SOME (absorb (blk, fnLab, szb, nogc))

        (* HeapCheckN, and Global heap tests, can't be handled. *)
        | _ => NONE
    (* end case *))

  and absorb (C.BLK{lab, args, body, ...}, fnLab, sz, cont) = (
      ST.tick cntElimCheck ;
      addPrologueAlloc (fnLab, sz) ;
      C.BLK {lab=lab, args=args, body=body, exit = C.Goto cont}
    )

  (* get successors for a select few exit types.
     in particular, we don't use labelsOfXfer because
     we want to exclude HeapCheck exits, and Gotos, etc. *)
  and getSuccLabs exit = (case exit
      of C.If(_, (lab1, _), (lab2, _)) => [lab1, lab2]
       | C.Switch(_, cases, dflt) => let
              fun f ((_, (lab, _)), l) = lab :: l
            in
              foldl f (case dflt of SOME(lab, _) => [lab] | _ => []) cases
            end
       | _ => []
       (* end case *))

  fun transform (m as C.MODULE{name, externs, mantiExterns, code}) =
    if not (Controls.get BasicControl.linkstack)
      then m
      else let
              (* TODO: skip analysis until the FIXME above is handled.
              val () = Predecessors.clear m
              val () = Predecessors.analyze m
              val () = CFA.clearInfo m
              val () = CFA.analyze m
              *)
              val _ = ()
            in
            C.MODULE{name=name, externs=externs, mantiExterns=mantiExterns,
                            code = map doFn code }
            end

end
