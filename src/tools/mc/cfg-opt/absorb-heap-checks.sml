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
  fun doFn (f as C.FUNC{lab, entry, start, body}) =
    C.FUNC{lab=lab, entry=entry, body=body,
            start = tryDrop (lab, start) }

  and tryDrop (fnLab, blk as C.BLK{exit, ...}) =
    (case exit
       of C.HeapCheck {hck = C.HCK_Local, szb, nogc} =>
              absorb (blk, fnLab, szb, nogc)

        (* HeapCheckN, and Global heap tests, can't be handled. *)
        | _ => blk
    (* end case *))

  and absorb (C.BLK{lab, args, body, ...}, fnLab, sz, cont) = (
      ST.tick cntElimCheck ;
      addPrologueAlloc (fnLab, sz) ;
      C.BLK {lab=lab, args=args, body=body, exit = C.Goto cont}
    )

  fun transform (m as C.MODULE{name, externs, mantiExterns, code}) =
    if Controls.get BasicControl.linkstack
      then C.MODULE{name=name, externs=externs, mantiExterns=mantiExterns,
                      code = map doFn code }
      else m

end
