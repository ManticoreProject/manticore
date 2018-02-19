(* simplify-graph.sml
 *
 * COPYRIGHT (c) 2018 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 *  Perform peephole-like, control-flow graph simplifications.
 *)

structure SimplifyGraph : sig

    (* Relies on accurate census information in the input module. *)
    val transform : CFG.module -> CFG.module

  end = struct
  
  structure L = List
  structure C = CFG
  structure CT = CFGTy
  structure CL = CFG.Label
  structure CV = CFG.Var
  structure Tbl = CFG.Label.Tbl
  structure ST = Stats
  
  (* statistics that control the number of iterations *)
  val cntUnusedArg		= ST.newCounter "cfg-simplifygraph:args-elim"
  val cntInlinedBlk		= ST.newCounter "cfg-simplifygraph:blocks-inlined"
(* first and last counters *)
  val firstCounter		= cntUnusedArg
  val lastCounter		= cntInlinedBlk
  
  (* count iterations too *)
  val cntIters		= ST.newCounter "cfg-simplifygraph:iterations"
  
  fun ticks () = ST.sum {from = firstCounter, to = lastCounter}
  
  fun unused v = (CV.useCount v = 0)
  fun decUse v = CV.addToCount(v, ~1)
  
  
  fun doFn (f as C.FUNC{lab, entry, start, body}) = let
    (* tbl holds the current version of the block, if it is still around. *)
    val tbl = Tbl.mkTable(L.length body, Fail "block not found.")
    
    fun initBlk (b as C.BLK{lab,...}) = (Tbl.insert tbl (lab, b); lab)
    
    (* setup for iteration *)
    val (startL::bodyLs) = L.map initBlk (start::body)
    val simplifyBlock = simplifyBlock tbl
    
    fun doSimp () = 
        (simplifyBlock true startL ; 
         L.app (simplifyBlock false) bodyLs)
    
    (* iterate until a fixed point *)
    fun fixedpt oldSum = let
            val _ = ST.tick cntIters
            val () = doSimp ()
            val newSum = ticks()
        in
            if oldSum <> newSum
            then fixedpt newSum
            else ()
        end 
    val () = fixedpt (ticks())
    
    (* collect the new blocks together *)    
    val SOME start' = Tbl.find tbl startL
    val body' = L.mapPartial (Tbl.find tbl) bodyLs
  in
    C.mkLocalFunc(lab, entry, start', body')
  end
  
  and simplifyBlock tbl isStart = let
    (* utilities *)
    val exists = Tbl.inDomain tbl
    val lookup = Tbl.lookup tbl
    val find = Tbl.find tbl
    val insert = Tbl.insert tbl
    
      
    fun updateBlock f lb = (case find lb
        of SOME blk => insert (lb, f blk)
         | NONE => raise Fail ("block does not exist " ^ CL.toString lb)
        (* end case *))
    
    
    (* master list of optimizations *)
    fun examine bl = 
        if exists bl
        then (
            removeDeadParams bl ;
            (* TODO try to inline a successor with a single predecessor *)
            ()
            )
        else ()
        
    (**********************)
        
    (* first peephole optimization: delete dead args in non-start blocks *)
    and removeDeadParams bl = 
        if isStart orelse (CL.useCount bl <> L.length (C.getPreds bl))
        then () 
        else let
        val (C.BLK{args, ...}) = lookup bl
        
        fun look (v, (i, ds)) =
            if unused v
            then (i+1, i::ds)
            else (i+1, ds)
        
        (* look for dead arguments in this block *)
        val (_, deadArgNums) = L.foldl look (0, []) args
        val deadArgNums = L.rev deadArgNums
        
        (* in the sorted, zero-based index list, drop all CFG vars in ls
           in those positions, and and adjust their use count. *)
        fun drop idxs ls = let
            fun lp (_, [], xs, remain) = (List.rev remain) @ xs
              | lp (i, d::ds, x::xs, remain) =
                if i = d
                then (* drop x *)
                    (decUse x ; lp (i+1, ds, xs, remain))
                else
                    lp (i+1, d::ds, xs, x::remain)
        in
            lp(0, idxs, ls, [])
        end
        
        fun changePred (C.BLK{lab, args, body, exit}) = let
        (*    val _ = print (String.concat 
                ["changing ", CL.toString lab, " -> ", CL.toString bl, "\n"]) *)
                
            fun chkJ (j as (tgt, vars)) = 
                if CL.same (tgt, bl)
                then (tgt, drop deadArgNums vars)
                else j
            
            val exit' = CFGUtil.mapSuccOfXfer chkJ exit
        in
            C.mkBlock(lab, args, body, exit')
        end
        
        fun changeSelf (C.BLK{lab, args, body, exit}) = let
            val args' = drop deadArgNums args
            val (CT.T_Block _) = CL.typeOf lab (* sanity check *)
            val newTy = CT.T_Block {args = L.map CV.typeOf args'}
        in
            ( CL.setType (lab, newTy) ; C.mkBlock(lab, args', body, exit) )
        end
        
        
        val numElim = L.length deadArgNums
        val predecessors = C.getPreds bl
    in
        if numElim = 0
        then ()
        else ( ST.bump (cntUnusedArg, numElim * (L.length predecessors))
            ; (* update my own signature *)
              updateBlock changeSelf bl
            ; (* update my predecessors *)
               L.app (updateBlock changePred) predecessors
            )  
    end
  in
    examine
  end
  
  
  
  fun transform m = let
    val () = Predecessors.clear m
    val () = Predecessors.analyze m
    val (C.MODULE{name, externs, mantiExterns, code}) = m
    val code' = L.map doFn code
    val m' = C.mkModule(name, externs, mantiExterns, code')
    val () = Predecessors.clear m'
  in
    m'
  end
  
end
