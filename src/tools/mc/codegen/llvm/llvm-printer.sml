(* llvm-printer.sml
 *
 * COPYRIGHT (c) 2015 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Outputs a CFG program as textual LLVM IR. 
 *    - Depends on the CFG contraction pass (to remove blocks with no predecessors).
 *    - Known to be compatible with a modified version of LLVM 3.8 through 4.0
 *      (with the JWA calling convention). 
 *
 * Originally authored by Kavon Farvardin (kavon@farvard.in)
 *)

functor LLVMPrinter (structure Spec : TARGET_SPEC) : sig

    val output : (TextIO.outstream * CFG.module) -> unit

  end = struct
  
  (* if set to true, will send CFG output to stderr for debugging purposes *)
  val DEBUGGING = false

  structure C = CFG
  structure CV = CFG.Var
  structure CL = CFG.Label
  structure CT = CFGTy
  structure CTU = CFGTyUtil
  structure CF = CFunctions
  structure S = String
  structure L = List
  structure V = Vector

  (*  *)
  structure LPU = LLVMPrinterUtil
  structure LV = LLVMVar
  structure LB = LLVMBuilder 
  structure LR = LLVMRuntime
  structure LS = LLVMStrings
  structure A = LLVMAttribute
  structure AS = LLVMAttribute.Set

  structure LT = LV.LT
  structure Ty = LLVMTy
  structure Op = LLVMOp
  structure OU = LLVMOpUtil
  structure P = Prim
  structure PU = PrimUtil
  structure CU = CondUtil
  

fun output (outS, module as C.MODULE { name = module_name,
                                       externs = module_externs,
                                       code = module_code } ) = let
  
  (* print/string utils *)
  fun pr s = TextIO.output(outS, s)
  fun prl s = pr(S.concat s)
  val i2s = Int.toString

  fun mapSep(f, init, sep, lst) = List.foldr 
                      (fn (x, nil) => f(x) :: nil 
                        | (x, y) => let val fx = f(x) in
                          if fx = "" (* skip empty strings *)
                          then y
                          else fx :: sep :: y
                        end)
                      init
                      lst



  (* links together the attribute number and the standard attribute list *)

  datatype llvm_attributes = MantiFun | ExternCFun

  fun stdAttrs (MantiFun) = "nounwind naked"

    (* NOTE: because I'm not sure of the effect inlining a C func into a naked func right now. *)
    | stdAttrs (ExternCFun) = "noinline" 

  (**)

  (* translation environment utilities *)
  
  (* lightweight management tools for 
     the implicit machine values found in the CFG representation *)
  
  datatype machineVal 
    = MV_Alloc
    | MV_Vproc
    
  fun machineInfo mv = (case mv
    of MV_Alloc => (0, "allocPtr", LT.allocPtrTy)
     | MV_Vproc => (1, "vprocPtr", LT.vprocTy)
    (* end case *))
    
  fun machineValIdx mv = #1(machineInfo mv)
  fun machineValStr mv = #2(machineInfo mv)
  fun machineValTy  mv = #3(machineInfo mv)
      
  fun IdxMachineVal n = (case n
      of 0 => SOME MV_Alloc
       | 1 => SOME MV_Vproc
       | _ => NONE
      (* end case *))
      
  val mvCC = [ MV_Alloc, MV_Vproc ] (* parameters added to all basic blocks 
                                       dont change order *)
    
  fun freshMVs () = let
        val fresh = (fn x => LV.new(machineValStr x, machineValTy x))
    in
        (mvCC, L.map fresh mvCC)
    end
  
  val numMachineVals = 2
  
  (* end of machine value tools *)
  

  datatype gamma = ENV of {
    labs : LV.var CL.Map.map,    (* CFG Labels -> LLVMVars *)
    blks : LB.t LV.Map.map,     (* LLVMVars -> basic blocks *)
    vars : LB.instr CV.Map.map,     (* CFG Vars -> LLVM Instructions *)
    mvs : LB.instr vector          (* current LLVM Instructions representing machine vals *)
  }
  
  val emptyEnv = ENV {labs=CL.Map.empty, blks=LV.Map.empty, vars=CV.Map.empty, mvs=(#[])}

  fun lookupV (ENV{vars,...}, v) = 
    (case CV.Map.find(vars, v)
      of SOME lv => lv
       | NONE => raise Fail ("lookupV -- unknown CFG Var: " ^ CV.toString v)
    (* esac *))

  fun lookupL (ENV{labs,...}, l) = 
    (case CL.Map.find(labs, l)
      of SOME ll => ll
       | NONE => raise Fail ("lookupL -- unknown CFG Label: " ^ CL.toString l)
    (* esac *))
    
  fun lookupMV (ENV{mvs,...}, kind) = Vector.sub(mvs, machineValIdx kind)
  
  fun lookupBB (ENV{blks,...}, llv) =
    (case LV.Map.find(blks, llv)
      of SOME bb => bb
       | NONE => raise Fail ("lookupBB -- unknown LLVM Basic Block: " ^ LV.toString llv)
    (* esac *))

  fun insertV (ENV{vars, blks, labs, mvs}, v, lv) = 
        ENV{vars=(CV.Map.insert(vars, v, lv)), blks=blks, labs=labs, mvs=mvs}

  fun insertL (ENV{vars, blks, labs, mvs}, l, ll) = 
        ENV{vars=vars, blks=blks, labs=(CL.Map.insert(labs, l, ll)), mvs=mvs}
        
  fun insertBB (ENV{vars, blks, labs, mvs}, llv, bb) = 
        ENV{vars=vars, blks=(LV.Map.insert(blks, llv, bb)), labs=labs, mvs=mvs}
        
  fun updateMV(ENV{vars, blks, labs, mvs}, kind, lv) =
        ENV{vars=vars, labs=labs, blks=blks,
            mvs= Vector.update(mvs, machineValIdx kind, lv)}

  (* end translation environment utilities *)
  
  
  

  (* Basic Blocks *)
  
  datatype 'a start_conv 
    = Padded of 'a          (* padded & uniform types, 
                                used in CPS generation for musttail calls.
                                it's an 'a because I was lazy to write out the type *)
    
    | Regular of                    (* used for direct-style calls *)
        { llArgs : LV.var list, 
          cfgArgs : C.var list, 
          mvs : LV.var list }

  fun mkBasicBlocks (initEnv : gamma, start : C.block, body : C.block list, llvmCC) : string list = let
    (* no branches should be expected to target the start block, 
      because they should be calls (the start block has the type of the function
    and for all intents and purposes it represents the function) *)

      fun convertLabs (C.BLK{lab,...}) = (lab, LV.convertLabel lab)

      val initialEnv = L.foldr (fn ((old, new), acc) => insertL(acc, old, new))
                  initEnv 
                  (L.map convertLabs body)

      (* NOTE note adding start block to env because nobody can branch to it anyways *)

      fun mkRegBlock (C.BLK{lab, body, exit, args}) = let
          val llArgs  = L.map LV.convert args
          
          (* we need to add implicit values from CFG to the "branching convention" of all other
          blocks (start block is already taken care of with the JWA CC) *)
          
          val (newMVs as (_, mvVars)) = freshMVs()
          
          val env = L.foldr (fn ((old, new), acc) => insertV(acc, old, LB.fromV new))
                      initialEnv
                      (ListPair.zip(args, llArgs))
            
          
          
          val env = L.foldr (fn ((mvKind, mvVar), acc) => updateMV(acc, mvKind, LB.fromV mvVar)) 
                    env
                    (ListPair.zip newMVs)
          
          val b = LB.new (lookupL(env, lab), mvVars @ llArgs)
        in
          (b, env, fn (b, env) => fillBlock b (env, body, exit)) 
          (* fillBlock b (env, body, exit) *)
        end

      fun mkStartBlock (C.BLK{body, exit, ...}, Padded (cc, ccRegs, mvRegs)) = let
              (* start needs to be treated specially because its inputs
                 are the parameters to the function that need a special calling convention, and
                 we need to add bitcasts of the parameters instead of phi nodes for  *)
                 
                 val inputs = L.map (fn (_, var, _) => var) (mvRegs @ ccRegs)
                 
                 val blk = LB.new(LV.new("entry", LT.labelTy), inputs)
                 
                 fun addBitcastCC (((_, cfgVar), (_, llReg, realTy)), acc) = let
                        val castPair = (LV.typeOf llReg, realTy)
                        val argPair = (LB.fromV llReg, realTy)
                        val newVar = LB.cast blk (Op.simpleCast castPair) argPair
                    in
                        insertV(acc, cfgVar, newVar) (*  *)
                    end
                    
                fun addCastsMV ((i, llReg, realTy), acc) = let
                       val castPair = (LV.typeOf llReg, realTy)
                       val argPair = (LB.fromV llReg, realTy)
                       val newVar = LB.cast blk (Op.equivCast castPair) argPair
                       val SOME mv = IdxMachineVal i
                   in
                       updateMV(acc, mv, newVar)
                   end
                 
                 
                 val env = L.foldl 
                    addBitcastCC
                    initialEnv
                    (ListPair.zipEq(cc, ccRegs))
                    
                val env = L.foldl addCastsMV env mvRegs
              
            in
                (blk, env, fn (blk, env) => fillBlock blk (env, body, exit))
                (* fillBlock blk (env, body, exit) *)
            end
            
        | mkStartBlock (C.BLK{body, exit, ...}, Regular{llArgs, cfgArgs, mvs}) = let
                val blk = LB.new(LV.new("entry", LT.labelTy), mvs @ llArgs)
                
                (* add the args to the env. 
                   machine vals are already in the env with the correct types. *)
                val env = L.foldl
                            (fn ((cfgV, llvmV), acc) => insertV(acc, cfgV, LB.fromV llvmV))
                            initialEnv
                            (ListPair.zipEq(cfgArgs, llArgs))
            in
                (blk, env, fn (blk, env) => fillBlock blk (env, body, exit))
            end

      (* make new blocks and setup their individual environments wrt their calling conventions *)
      val allBlocks = mkStartBlock(start, llvmCC) :: (L.map mkRegBlock body)
      
      (* now collect all of these blocks into a map in order to add predecessor edges for transfers
         when we fill the blocks later *)
      val bbMap = 
            L.foldl 
            (fn ((blk, _, _), map) => LV.Map.insert(map, LB.labelOf blk, blk))
            LV.Map.empty 
            allBlocks
            
     (* update all of the block environments with the new map 
        NOTE we wipe out the old blks map because it should be empty before this point anyways. *)
     val allBlocks = L.map (fn (b, ENV{vars, blks, labs, mvs}, f) => 
                                (b, ENV{vars=vars, blks=bbMap, labs=labs, mvs=mvs}, f)) allBlocks
    
    (* results in a list of thunks that will cap off the blocks, and those thunks return a 
       list of blocks that is the result of generating this block. Heap allocations in 
       particular generate extra blocks, but all others should just be a singleton list *)
    val allBlocks = L.map (fn (blk, env, f) => f (blk, env)) allBlocks
      
    (* force the thunks now that all blocks have added their predecessor edges to
       all of the blocks. then flatten the LB.bb list list and map them to strings *)
    val stringyBlocks = L.foldr
                (fn (thunk, ys) => case thunk()
                  of nil => raise Fail "uh oh, how did a block go missing?"
                   | [x] => (LB.toString x)::ys
                   | xs => (L.map LB.toString xs) @ ys)
                []
                allBlocks

    in
        stringyBlocks
    end
      
(* determines calling conventions. we keep it all localized here
   so we don't mess it up *)
and determineCC (* returns a ListPair of slots and CFG vars assigned to those slots,
                   and the list of types for machine vals. the indices are defined by 
                   the machine val's index function *)
    (conv : CFG.convention, args : C.var list) : (LT.ty list * (int * C.var) list) = let
        
        (*val _ = if L.length args <= 0 then raise Fail "no arg?" else ()*)
        
        val getTy = LT.toRegType o LT.typeOf o C.Var.typeOf
        
        (* dummy machine val padding *)
        val genericPadding = LT.toRegType LT.allocPtrTy
        
        (* this is padding at the front of the convention, where we always put the machine values. *)
        val machineValPadding =
            List.tabulate(numMachineVals, fn _ => genericPadding)
        
        fun withPadding convVars = 
            machineValPadding 
            @ (List.map getTy convVars)
        
        fun determineIndices convVars = 
            L.drop((LT.allocateToRegs o withPadding) convVars, numMachineVals)
    in
        (case conv
            of C.StdFunc { clos, ret, exh } => let
                val convVars = [clos, ret, exh] @ args
                in
                    (machineValPadding, ListPair.zipEq(determineIndices convVars, convVars))
                end
                
                

            | (C.StdCont { clos } | C.KnownFunc { clos }) => let
            
            (* NOTE there is no exn handler or retk, so we need to add artifical padding
               in order to get the args into the right registers, we also need CFG vars
               in this list so we just duplicate the clos. Kinda gross but it's fine. *)
                val actualConvVars = clos :: args
                
                val paddedConv = clos :: [clos, clos] @ args
                
                (* now that everything's been assigned to slots, drop the two generic paddings in
                   we added between the first clos and the args. *)
                val (closI :: _ :: _ :: restI) = determineIndices paddedConv
                val actualIndices = closI :: restI
                
                in
                    (machineValPadding, ListPair.zipEq(actualIndices, actualConvVars))
                end
        (* end case *))
  end

  and fillBlock (b : LB.t) (initialEnv : gamma, body : C.exp list, exit : C.transfer) : (unit -> LB.bb list) = let
    
    (* a jump list is a (label * var list) which indicates
       where a jump comes from, and the names of the vars from that BB.
       We'll need to stick a sequence of phis at the beginning of each
       BB once we know all of the control flow in the program. in particular,
       during the generation of transfers we'll be creating new blocks.

       In the meantime, we should save the args and preds into the block, and later
        once we terminate the block we'll generate the following at the beginning
        of the block:

        arg[i] <- phi [ jump[k].arg[i], jump[k].label ], [ jump[k+1].arg[i], jump[k+1].label ], ...
        arg[i+1] <- phi [ jump[k].arg[i+1], jump[k].label ], [ jump[k+1].arg[i+1], jump[k+1].label ], ...
        ...
    *)
      
      (* handy stuff used in several places *)
      
      (* tag enums to distinguish them from pointers: enum(e) => 2*e+1 
         lifted directly from codegen-fn.sml in the MLRISC backend *)
        fun encodeEnum e = Word.<<(e, 0w1) + 0w1
        
      (* we want the two low bits of the state-value representation to be zero *)
        fun encodeStateVal n = Word.<<(n, 0w2) 
      
      (* the noattr instruction maker *)
      val mk = LB.mk b AS.empty
      val cast = LB.cast b
      val mkVolatile = LB.mk b (AS.singleton A.Volatile)

      (* end handy stuff *)
        
(****************************************************************************************


                                start of finish function

                    This function generates code for CFG exits.
                    
                    
*****************************************************************************************)        
      fun finish(env, exit) = let
      (*
        NOTE All transfers making this basic block a predecessor of some other basic block
             within this function _must_ use LB.addIncoming to tell that block about its 
             new predecessor _before_ constructing the thunk that delays the actual 
             addition of the block terminator to the LB.t that turns it into a LB.bb. 
             
             This kind of hacky design is due to the fact that I decided agianst having a 
             seperate pass that annotates basic blocks in the CFG with their predecessors 
             (though some remnants of that pass I wrote are around).  ~kavon
      *)
        datatype space_needed = SN_Const of word | SN_Var of LB.instr
    
    (************* start heapCheckHelper ****************)  
        fun heapCheckHelper env szb nogc = let
             (* FIXME: this value should come from the runtime constants
                CURRENT SOURCE: alloc64-fn.sml. slop space is 4kb, and we leave 512b
                for spilling values. might not be enough tbh? *)
             val heapSlopSzB = Word.- (Word.<< (0w1, 0w12), 0w512)
             val limitPtrOffset = Spec.ABI.limitPtr
             
             (* wraps an i1 value with an annotation that tells the optimizer that its expected
               to be i1 0 *)
             fun expectFalse b condInstr = let
                val falseC = LB.iconst LT.i1 0
                val (expectLab, _) = LR.expect_i1
                val annotated = LB.call b (LB.fromV expectLab, #[condInstr, falseC])
             in
                annotated
             end
             
             fun notEnoughSpace b vproc allocPtr (SN_Const szb) = let 
                     val mk = LB.mk b AS.empty
                     val cast = LB.cast b
                     
                     val limitPtrAddr = LPU.vpOffset b vproc limitPtrOffset (LT.mkPtr LT.i64)
                     val limitPtrVal = LB.mk b (AS.singleton A.Volatile) Op.Load #[limitPtrAddr]
                     
                     (* val allocPtr = lookupMV(env, MV_Alloc) *)
                     val allocPtrVal = cast Op.PtrToInt (allocPtr, LT.i64)
                     
                     val diff = mk Op.Sub #[limitPtrVal, allocPtrVal]
                     
                     val zero = LB.iconst LT.i64 0
                     val szbC = LB.iconst LT.i64 (Word.toInt szb)
                 in
                     if szb <= heapSlopSzB
                     then (* just check if the diff is negative,
                            which should be more efficient *)
                         mk (Op.Icmp(Op.S Op.LE)) #[diff, zero]
                         
                     else (* check for the headroom *)
                         mk (Op.Icmp(Op.S Op.LE)) #[diff, szbC]
                     
                 end
               | notEnoughSpace b vproc allocPtr (SN_Var szb) = let 
                       val mk = LB.mk b AS.empty
                       val cast = LB.cast b
                       
                       val limitPtrAddr = LPU.vpOffset b vproc limitPtrOffset (LT.mkPtr LT.i64)
                       val limitPtrVal = LB.mk b (AS.singleton A.Volatile) Op.Load #[limitPtrAddr]
                       
                       (* val allocPtr = lookupMV(env, MV_Alloc) *)
                       val allocPtrVal = cast Op.PtrToInt (allocPtr, LT.i64)
                       
                       val diff = mk Op.Sub #[limitPtrVal, allocPtrVal]
                   in
                        mk (Op.Icmp(Op.S Op.LE)) #[diff, szb]
                   end
               (* end of notEnoughSpace *)
             
             fun doGC constBytesNeeded (incoming as [incFrame, incAlloc, incVp]) = let
                 val bbLab = LV.new("doGC", LT.labelTy)
                 
                 val params = [
                     LV.new("rootPtr", LB.toTy incFrame),
                     LV.new("allocPtr", LB.toTy incAlloc),
                     LV.new("vprocPtr", LB.toTy incVp)
                 ]
                 
                 val [framePtr, allocPtr, vprocPtr] = L.map (fn v => LB.fromV v) params
                 
                 val myBB = LB.new(bbLab, params)
                 
                 val castedFP = LB.cast myBB Op.BitCast (framePtr, LT.uniformTy)
                 
                 (* exn and arg are saved in the frame if they're live, and the
                    InvokeGC doesn't actually need them to be populated *)
                 
                 (* call the GC and get the resulting struct *)
                 val (gcFun, SOME cc) = LR.invokeGC
                 
                 (* NOTE always ensure that this matches up with the InvokeGC function in
                    llvm-runtime.sml and the actual ASM *)
                 val gcArgs = #[ allocPtr, vprocPtr, castedFP ] 
                 
                 val res = LB.callAs myBB cc (LB.fromV gcFun, gcArgs)
                 
                 fun extract strct i = LB.extractV myBB (strct, #[LB.intC(LT.i32, Int.toLarge i)])
                 
                 (* pull the new vals out of the struct, and cast the frame ptr back.
                    the order of the elements in this list needs to match
                    the parameters of this block *)
                 val (outgoing as [newFrame, newAlloc, newVProc]) = [
                     LB.cast myBB Op.BitCast (extract res 2, LB.toTy framePtr), (* frame *)
                     extract res 0, (* alloc ptr *)
                     extract res 1 (* vproc *)
                 ]
                 
                 val szb = if constBytesNeeded then szb
                            else let
                                (* the szb currently in scope refers to a value computed
                                   _before_ the GC call. we must retrieve that value
                                   from the spill frame returned by the GC instead, in
                                   order to prevent any values from being live across a
                                   GC call. we know this will work because LLVM
                                   can't make any assumptions about the value in that
                                   frame once the pointer is returned by the GC, so it
                                   must use the new one even if its the same value (which
                                   it will be of course). *)
                                   
                                val SOME addr = LPU.calcAddr myBB 0 newFrame
                                val newSzbI = LB.mk myBB AS.empty Op.Load #[addr]
                            in
                                SN_Var newSzbI
                            end
                            
                 val notEnoughSpaceCond = expectFalse myBB (notEnoughSpace myBB newVProc newAlloc szb)
                 
                 
                 val jump = (bbLab, outgoing)
                 (* we will loop back if there isn't enough space *)
                 val _ = LB.addIncoming myBB jump
                 
                 (* we will branch back to doGC if there is not enough space
                    after entering from the runtime system. Because we may have been
                    preempted, we're not guarenteed to have enough heap space
                    when the scheduler decides to resume us *)
                 fun brTo enoughBB = (
                          (* LB.addIncoming enoughBB jump ; *)
                          (fn () => LB.condBr myBB 
                             (notEnoughSpaceCond, bbLab, (LB.labelOf enoughBB))) 
                         )
                 
             in
                 (myBB, brTo, outgoing)
             end (* end doGC *)
        
             (* first we need to determine whether the number of bytes needed at this GC check
                is a constant or not. if it's not, it's a live value and we need to save it in the root set.
                it's assigned to slot 0. *)
             fun prepareGC env (SN_Const _) ((targ, live)) = let
                        val tys = L.map CV.typeOf live
                     in
                        prepGCHelper env (LPU.headerTag tys) (targ, prepCvt env live)
                     end
               
               | prepareGC env (SN_Var szbI) ((targ, live)) = let
                        (* just a 64 bit integer *)
                        val szBCFGTy = CFGTy.T_Raw RawTypes.T_Long
                        
                        val tys = szBCFGTy :: (L.map CV.typeOf live)
                        val liveLL = szbI :: (prepCvt env live)
                     in
                         prepGCHelper env (LPU.headerTag tys) (targ, liveLL)
                     end
               
              
               
             and prepCvt env live = L.map (fn x => lookupV(env, x)) live
        
             and prepGCHelper env tag ((targ, live)) = let
                 (* NOTE basically, we just save roots to the heap to create a spill frame.
                    returns a function to finish off this block once its br targ is
                    created, and the live values it will carry to that block. *)

                 val bbLab = LV.new("packageGC", LT.labelTy)
                 
                 (* no parameters since this block has exactly one predecessor,
                    and it's environment is an extension of its predecessor,
                    so there is no point in rebinding everything to fresh
                    variables as we already do for a typical single pred block. *)
                 val myBB = LB.new(bbLab, [])
                 val gep = LB.gep_ib myBB
                 val cast = LB.cast myBB
                 val mk = LB.mk myBB AS.empty
                 
                 val vprocPtr = lookupMV(env, MV_Vproc)
                                             
                 val {newAllocPtr=allocPtr, tupleAddr=framePtr} = 
                     LPU.doAlloc myBB (lookupMV(env, MV_Alloc)) live tag
                 

                 val outgoingLive = [framePtr, allocPtr, vprocPtr]
                         
                 fun brTo gotoBB = (
                          LB.addIncoming gotoBB (bbLab, outgoingLive) ;
                          (fn () => LB.br myBB (LB.labelOf gotoBB))
                         )

             in
                 (myBB, brTo, outgoingLive, live)
             end
             
             
             fun extractGC constBytesNeeded ([framePtr, allocPtr, vprocPtr]) origLive = let
                 
                 val bbLab = LV.new("extractGC", LT.labelTy)
                 (* there's no point in having parameters to this block, it
                    has exactly one predecessor, and we created it here, and
                    have the arguments too *)
                 val myBB = LB.new(bbLab, []) 
                 val mk = LB.mk myBB AS.empty
                 
                 (* NOTE the goal of extractGC is to reinitialize
                    the state we had before we entered the runtime system.
                    
                    As of now it takes 3 values. but it actually will only take
                    the allocation pointer.
                     *)
                     
                 (* prepareGC assigned the non-constant byte var to slot 0 in the
                    spill frame, so we need to offset the indices in that case *)
                 val tabulateFn = if constBytesNeeded then (fn x => x)
                                  else (fn x => x + 1)
                 
                 (* the non-const bytes val was added to the origLive
                    list, and that's dead as of now so we drop it *)
                 val origLive = if constBytesNeeded then origLive
                                    else L.drop(origLive, 1)
                
                
                fun asPtrTo ty addr = let
                        val addrTy = LB.toTy addr
                        val desiredTy = LT.mkPtr ty
                    in
                        if LT.same(addrTy, desiredTy)
                        then addr
                        else LB.cast myBB Op.BitCast (addr, desiredTy)
                    end
                 
                 val origTys = ListPair.zipEq( 
                             L.tabulate(L.length origLive, tabulateFn),
                             L.map (fn x => LB.toTy x) origLive
                             )
                             
                (* these loads need to occur just like in a SELECT, which casts pointers as needed *)
                 val loadedInstrs = L.map 
                     (fn (i, origTy) => case LPU.calcAddr myBB i framePtr
                       of SOME addr => mk Op.Load #[asPtrTo origTy addr]
                        | NONE => raise Fail "extractGC error"
                     ) origTys
                 
                 
                 (* following mvCC and markPred for the block calling convention *)
                 fun brTo gotoBB = (
                         LB.addIncoming gotoBB 
                             (bbLab, [allocPtr, vprocPtr] @ loadedInstrs)  ;
                         (fn () => LB.br myBB (LB.labelOf gotoBB))
                     )
                 
             in
                 (myBB, brTo)
             end
                          
             (* this enoughSpaceCond is for the initial heap check. *)
             val notEnoughSpaceCond = expectFalse b (notEnoughSpace b (lookupMV(env, MV_Vproc)) (lookupMV(env, MV_Alloc)) szb)
             
             val (nogcTarg, _) = markPred env nogc
             val nogcBB = lookupBB(env, nogcTarg)
             
             val constBytesNeeded = (case szb of SN_Var _ => false | _ => true)
             
             (* build the extra BBs we need to enter GC *)
             val (prepBB, prepGoto, prepOut, origOut) = prepareGC env szb nogc
             
             val (dogcBB, dogcGoto, dogcOut) = doGC constBytesNeeded prepOut
             
             val (extractBB, extractGoto) = extractGC constBytesNeeded dogcOut origOut
             
             
             (* setup the predecessor edges *)
             val prepTerminator = prepGoto dogcBB
             val gcTerminator = dogcGoto extractBB
             val xtractTerminator = extractGoto nogcBB
             
             
             
        in
             (fn () => [LB.condBr b (notEnoughSpaceCond, LB.labelOf prepBB, nogcTarg),
                        prepTerminator(), gcTerminator(), xtractTerminator()])
        end 
    (**** end heapCheckHelper ****)
      
      
        (* 
            in the CFG rep, the types of values might be implicitly cast
            during a transfer. for example:
            
            BLOCK 1 (block $then<100E9>#4)
            let _t<100EC>#1:[any] = alloc (acc<100E7>)
            goto $letJoinK<10029>(ep<100E6>,_t<100EC>)
            
            BLOCK 2
            acc<100D4>#1:[[int,any,int,any,![enum(1)]],[cont([cont(any/enum(0)),...]/enum(0)),...],any]
            ...
            let _t<100D9>#1:[any] = alloc (acc<100D4>)
            goto $letJoinK<10029>(ep<100D3>,_t<100D9>)
            
            block $letJoinK<10029>#8 
              (
                ep<10027>#2: <type sig>,
                item<10028>#2:any
              ) = ...
            
        *)
        and matchCC (llArgs, params) = let
                (* jumps sometimes implicitly cast the values, so we add bitcasts as needed
                   so that the llArgs are matched up with the block's parameters *)
                fun maybeCast (llArg, param) = 
                    let val neededTy = LV.typeOf param in
                    if LT.same(neededTy, LB.toTy llArg) 
                        then llArg
                        else cast (Op.equivCast(LB.toTy llArg, neededTy)) (llArg, neededTy)
                    end
            in
                L.map maybeCast (ListPair.zipEq(llArgs, params))
            end
        
        and markPred env jmp = markPredFrom b env (getCC env jmp)
            
        and markPredFrom b env (llLab, allArgs) = let
                val llBB = lookupBB(env, llLab)
                val bbParams = LB.paramsOf llBB
                
                val from = LB.labelOf b
                val castedArgs = matchCC(allArgs, bbParams)
                
            in
                (LB.addIncoming llBB (from, castedArgs) ; (llLab, castedArgs))
            end
            
        (* similar to markPred but does not perform casts, does not assume the from
           block is the current basic block, and does not add an incoming edge. 
           this just looks stuff up and puts the args together with the machine values. *)
        and getCC env (to, args) = let
                val llLab = lookupL(env, to)
        
                val llArgs = L.map (fn a => lookupV(env, a)) args
                val mvArgs = L.map (fn mv => lookupMV(env, mv)) mvCC
                val allArgs = mvArgs @ llArgs
                
            in
                (llLab, allArgs)
            end
            
            
        and mantiFnCall (func, (_, cc : (int * C.var) list)) = let
                
                val mvs = ListPair.zipEq(
                            L.tabulate(numMachineVals, fn i => i), 
                            L.map (fn mv => lookupMV(env, mv)) mvCC)
                            
                val cc = L.map (fn (i, cv) => (i, lookupV(env, cv))) cc
                
                val allRegs = mvs @ cc
                val slotNums = L.tabulate(V.length LT.jwaCC, fn i => i)
                
                (* mostly copied from elsewhere in here :shrug: *)
                datatype slotTy
                   = Used of (LB.instr * LT.ty)
                   | NotUsed of LT.ty
                     
                (* NOTE the regs must be ordered by slot num *)
                fun assign(nil, nil, res) = L.rev res 
                  | assign(slot::rest, nil, res) = assign(rest, nil, (NotUsed (V.sub(LT.jwaCC, slot)))::res)
                  | assign(slot::rest, (regs as (r::rs)), res) = let
                    val (idx, var) = r
                    val slotTy = V.sub(LT.jwaCC, slot)
                    in
                    if idx = slot 
                        then assign(rest, rs, (Used (var, slotTy))::res)
                        else assign(rest, regs, (NotUsed slotTy)::res)
                    end
                
                val allAssign = assign(slotNums, allRegs, nil)
                
                val allCvtdArgs = L.map 
                    (fn NotUsed t => LB.fromC(LB.undef t)
                      | Used (var, t) => cast (Op.safeCast (LB.toTy var, t)) (var, t)
                    ) allAssign
                
                val llFun = lookupV(env, func)
            in
                (fn () => [LB.tailCall b (llFun, V.fromList allCvtdArgs)])
            end
            
      in
          (case exit
              of C.Goto jmp => let
                    val (targ, _) = markPred env jmp
                  in
                    (fn () => [LB.br b targ])
                  end
              
               | C.If (cond, trueJ, falseJ) => let
                    val (trueLab, _) = markPred env trueJ
                    
                    val (maybeFalseTarg as (falseLab, falseArgs)) = getCC env falseJ
                    
                    val ((falseLab, _), maybeNewBlock) = if not (LV.same(falseLab, trueLab))
                                    
                                    then (markPredFrom b env maybeFalseTarg, NONE) (* no need to fret *)
                                    
                                    else let (* we need to make a dummy basic block
                                                to serve as the precessor block for the target
                                                block in the event of the cond being false.
                                                in LLVM SSA we cannot have phi's from the same
                                                predecessor but different values. *)
                                          
                                          (* almost do what markPredFrom does, just a bit diff *)
                                          val realSuccB = lookupBB(env, falseLab)
                                          val dummyB = LB.copy realSuccB
                                          val dummyLab = LB.labelOf dummyB
                                          val dummyParams = LB.paramsOf dummyB
                                          
                                          (* no real need to update the env *)
                                          (*val env = insertBB(env, dummyLab, dummyB)*)
                                          
                                          val dummyArgs = matchCC(falseArgs, dummyParams)
                                          
                                          
                                          (* edge from current block to our new dummy block.
                                             this represents the new false arm of the current block.
                                             
                                             NOTE how we are not using markPredFrom. We do not want
                                             to actually mark dummyB as having a predecessor,
                                             because we do not want any phi's emitted for the dummy
                                             block. we literally want it to just be a branch instruction. *)
                                          val dummyJump = (dummyLab, dummyArgs)
                                          
                                          (* also need to mark the successor block *)
                                          val _ = LB.addIncoming realSuccB dummyJump
                                          
                                          (* the dummy block just dispatches to the actual
                                             location this blocks is going to *)
                                          val dummyFin = LB.br dummyB falseLab
                                          
                                         in
                                            (dummyJump, SOME dummyFin)
                                         end
                    (*
                    val (trueTarg, _) = markPred env trueJ
                    val (falseTarg, _) = markPred env falseJ*)
                    
                    val llArgs = L.map (fn x => lookupV(env, x)) (CU.varsOf cond)
                    val cvtr = OU.fromCond b cond
                    
                    (* the i1 result of evaluating the condition *)
                    val result = cvtr llArgs
                     
                   in
                     
                     (fn () => L.mapPartial (fn x => x) [
                            SOME (LB.condBr b (result, trueLab, falseLab)),
                            maybeNewBlock
                        ])
                     
                   end
                   
               
               | C.Switch(cond, arms, maybeDefault) => let
                    
                    (* NOTE if there's no default because its exhaustive, we need
                     to take one of the arms and make it the default. we pick
                     the last one in case somebody ordered them by likelihood *)
                    
                    val (arms, default) = 
                        (case maybeDefault 
                            of SOME default => (arms, default)
                             | NONE => let 
                                val len = L.length arms 
                                val (_, newDefault) = L.last arms
                                in
                                    (L.take(arms, len-1), newDefault)
                                end
                            (* esac *))
                            
                    fun xlateArm (word, jump) = let 
                            (* switches on enums require a special encoding *)
                            val word = (case CV.typeOf cond
                                        of CFGTy.T_Enum _ => encodeEnum word
                                        | _ => word
                                       (* esac *))
                            
                            val (llTarg, _) = markPred env jump
                            val llConst = LB.intC(LT.enumTy, Word.toLargeInt word)
                        in 
                            (llConst, llTarg)
                        end
                    
                    val llArms = L.map xlateArm arms
                    val (llDefault, _) = markPred env default
                    val llCond = lookupV(env, cond)
                    
               in
                    (fn () => [LB.switch b llCond (llDefault, llArms)])
               end
               
               (* all types are CFG vars *)
               | C.StdApply {f, clos, args, ret, exh} 
                    => mantiFnCall(f, 
                         determineCC(C.StdFunc{clos=clos, ret=ret, exh=exh}, args))
                    
               | C.StdThrow {k, clos, args}
                    => mantiFnCall(k, determineCC(C.StdCont{clos=clos}, args))
                        
               | C.Apply {f, clos, args}
                    => mantiFnCall(f, determineCC(C.KnownFunc{clos=clos}, args))
               
               | (C.HeapCheck {hck = C.HCK_Global, ...} | C.HeapCheckN {hck = C.HCK_Global, ...})
                    => raise Fail "global heap checks not implemented in MLRISC backend either."
               
               | C.HeapCheck {hck = C.HCK_Local, szb, nogc} => 
                    heapCheckHelper env (SN_Const szb) nogc

               | (C.HeapCheckN {hck = C.HCK_Local, nogc, n, szb}) => let
                    val n = lookupV(env, n)
                    
                    (* see genAllocNCheck in alloc64-fn.sml. Kavon doesn't know why we add 4. *)
                    val nZxt = cast Op.ZExt (n, LT.i64)
                    val sum = mk Op.Add #[nZxt, LB.iconst LT.i64 4]
                    val totalNeeded = mk Op.Mul #[sum, LB.iconst LT.i64 (Word.toInt szb)]
               in
                    heapCheckHelper env (SN_Var totalNeeded) nogc
               end
                
               | C.AllocCCall _ => raise Fail "not implemented because it's used nowhere at all."
               
               (* NOTE: Return vars => 
                    1. retVals = mvCC @ vars
                    2. initialize a struct S that is "in register" with the retVals
                    3. return S
               *)
               | C.Return vars => let
                    val vars = L.map (fn v => lookupV(env, v)) vars
                    val mvs = L.map (fn mv => lookupMV(env, mv)) mvCC
                    val retVals = mvs @ vars
                    
                    fun toC i = LB.intC(LT.i32, IntInf.fromInt i)
                    
                    val offsets = L.tabulate(L.length retVals, toC)
                    val startStruct = LB.fromC(LB.undef (LT.mkUStruct (L.map LB.toTy retVals)))
                    
                    val retStruct = ListPair.foldlEq 
                                    (fn (i, v, strct) => LB.insertV b (strct, v, #[i]))
                                    startStruct
                                    (offsets, retVals)
                                    
               in
                    (fn () => [LB.ret b retStruct])
               end
               
               
               (* NOTE: Call =>
                    1. lookup f, clos, args, and lives;
                          where lives = liveAfter - lhs
                    2. token = call{f, clos, args, lives}
                    3. retStruct = returnOf token
                    4. env += lhs_i -> retStruct_i, and env += mv_i -> retStruct_i
                    5. relos = relosOf token
                    6. env += lives_i -> relos_i
                    7. lookup in env everything in liveAfter and do a branch.
                *)
               | C.Call {f, clos, args, next=SOME(lhs, jmp as (_, liveAfter))} => let
                    (* lookup everything in the env *)
                    val f = lookupV(env, f)
                    val clos = lookupV(env, clos)
                    val args = L.map (fn v => lookupV(env, v)) args
                    val mvs = L.map (fn mv => lookupMV(env, mv)) mvCC
                    val allArgs = mvs @ clos :: args
                    
                    (* remove the lhs vars from the liveAfter list *)
                    val lives = L.filter 
                                (fn v => 
                                    not(L.exists 
                                        (fn x => CV.same(v, x)) 
                                        lhs)) 
                                liveAfter
                                
                    val lives_llvm = L.map (fn v => lookupV(env, v)) lives
                    
                    val {ret, relos} = LLVMStatepoint.call { 
                                          blk = b,
                                          conv = LB.fastCC,
                                          func = f,
                                          args = allArgs,
                                          lives = lives_llvm
                                        }
                                        
                    (* grab the return values *)
                    fun toC i = LB.intC(LT.i32, IntInf.fromInt i)
                    
                    val idxs = L.tabulate(numMachineVals + L.length lhs, toC)
                    val retVals = L.map (fn i => LB.extractV b (ret, #[i])) idxs
                    
                    (* update the machine vals in the env *)
                    val newMVs = L.take(retVals, numMachineVals)
                    val env = ListPair.foldlEq
                                (fn (mv, valu, acc) => updateMV(acc, mv, valu))
                                env
                                (mvCC, newMVs)
                    
                    (* bind the lhs values *)
                    val rhs = L.drop(retVals, numMachineVals)
                    val env = ListPair.foldlEq
                                (fn (lhs, rhs, acc) => insertV(acc, lhs, rhs))
                                env
                                (lhs, rhs)
                    
                    (* update the env with the relocated values *)
                    val env = ListPair.foldlEq
                                (fn (live, relo, acc) => insertV(acc, live, relo))
                                env
                                (lives, relos)
                                
                    (* do the jump *)
                    val (targ, _) = markPred env jmp
               in
                    (fn () => [LB.br b targ])
               end
               
               | C.Call {next=NONE,...} => (fn () => [LB.retVoid b]) (* FIXME *)
               
              (* esac *))  
      end
      
(****************************************************************************************
                                
                                
                                end of finish function
                                
                                
*****************************************************************************************)
      
      (* handle the list of exp's in a CFG block *)
      and process(env, []) = env
        | process(env, x::xs) = let
          val env =
            (case x
              of C.E_Var rhs => genAssignments(env, rhs)
               | C.E_Const rhs => genConst(env, rhs)
               | C.E_Cast rhs => genCast(env, rhs)
               | C.E_Label rhs => genLabel(env, rhs)
               | C.E_Select rhs => genSelect(env, rhs)
               | C.E_Update rhs => genUpdate(env, rhs)
               | C.E_AddrOf rhs => genAddrOf(env, rhs)
               | C.E_Alloc rhs => genAlloc(env, rhs)
               | C.E_GAlloc rhs => genGAlloc(env, rhs)
               | C.E_Promote rhs => genPromote(env, rhs)
               | C.E_Prim0 rhs => genPrim0(env, rhs)
               | C.E_Prim rhs => genPrim(env, rhs)
               | C.E_CCall rhs => genCCall(env, rhs)
               | C.E_HostVProc rhs => genHostVProc(env, rhs)
               | C.E_VPLoad rhs => genVPLoad(env, rhs)
               | C.E_VPStore rhs => genVPStore(env, rhs)
               | C.E_VPAddr rhs => genVPAddr(env, rhs)
              (* esac *))
          in
            process(env, xs)
          end
          
      and genAssignments(env, (lefts, rights)) = 
      (* NOTE LLVM doesn't directly support renaming operations, the
         closest you could get is to bitcast the value to the same type,
         since bitcasts are considered noops in llvm. Doing this might be handy
         for debugging, but for now we'll just update env mappings.
       *)
           L.foldr
           (fn ((lhs, rhs), acc) => insertV(acc, lhs, lookupV(acc, rhs)))
           env
           (ListPair.zipEq (lefts, rights))
    
      
      and genConst(env, (lhsVar, lit, ty)) = let
        val constInstr = (case lit
            of Literal.Int il => 
                  LB.fromC(LB.intC(LT.typeOf ty, il))
             
             (* this isn't implemented in old backend either *)
             | Literal.Bool b => raise Fail "unexpected Literal.Bool" 
                  (* insertV(env, lhsVar, LB.fromC(LB.intC(LT.boolTy, if b then 1 else 0))) *)
                  
             | Literal.Float f =>
                  LB.fromC(LB.floatC(LT.typeOf ty, f))
                  
             | Literal.Enum c =>
                  LB.fromC(LB.intC(LT.enumTy, (Word.toLargeInt o encodeEnum) c))
                  
             | Literal.StateVal n =>          (* NOTE that we're using enumTy here too. not sure if that's right *)
                  LB.fromC(LB.intC(  LT.enumTy  , (Word.toLargeInt o encodeStateVal) n ))
                  
             | Literal.Char _ => raise Fail "not implemented" (* not implemented in MLRISC backend either *)
             
             | Literal.String s => let
                  val llv = LS.lookup s
                  
                  (* calculate the address of the first byte in this ptr to an 
                     array of bytes to turn it into an i8*. NOTE it's not
                     safe to use any other offset except 0 here because of the data
                     layout. *)
                  val SOME gep = LPU.calcAddr b 0 (LB.fromV llv)
             in
                  gep
             end
             
             | Literal.Tag t => let
                  val llv = LS.lookup t
                  val lhsTy = (LT.typeOf o CV.typeOf) lhsVar
                  
                  (* calculate the address of the first byte in this ptr to an 
                     array of bytes to turn it into an i8*. see NOTE above
                     regarding data layout *)
                  val SOME gep = LPU.calcAddr b 0 (LB.fromV llv)
                  val casted = cast (Op.safeCast(LB.toTy gep, lhsTy)) (gep, lhsTy)
             in
                 casted
             end
            (* esac *)) (* end constInstr *)
            
            (* NOTE we introduce this dummy binding here so we do not accidentially perform
              an unsafe constant propigation if we were to add the raw constant to the env otherwise. *)
              
            val constTy = LB.toTy constInstr
            val boundConst = LB.cast b Op.BitCast (constInstr, constTy)
      in
            insertV(env, lhsVar, boundConst)
      end
        
        
      and genCast(env, (lhsVar, cfgTy, oldVar)) = let
        val llv = lookupV(env, oldVar)
        val targetTy = LT.typeOf cfgTy
        
        val castPair = (LB.toTy llv, targetTy)
        val argPair = (llv, targetTy)
        val newLLVar = LB.cast b (Op.autoCast castPair) argPair
      in
        insertV(env, lhsVar, newLLVar)
      end
      
      and genLabel(env, (lhsVar, rhsLabel)) = let
        val llv = lookupL(env, rhsLabel)
      in
        insertV(env, lhsVar, LB.fromV(llv))
      end
      
      
      
      and genSelect(env, (lhsVar, i, rhsVar)) = let
        val llv = lookupV(env, rhsVar)
        val addr = (case LPU.calcAddr b i llv
                    of SOME addr => addr
                     | NONE => ( debug ("SELECT " ^ Int.toString i) rhsVar llv ; raise Fail "unrecoverable error")
                    (* esac *))
                    
        (* now that we have the right pointer, we need to cast the pointer to the right type. *)
        val cfgSlotTy = CFGTyUtil.select(CFG.Var.typeOf rhsVar, i)
        val llSlotPtrTy = LT.mkPtr(LT.typeOf cfgSlotTy)
        
        val addr = cast Op.BitCast (addr, llSlotPtrTy)
        val loadedVal = mk Op.Load #[addr]
      
      (* In CFG, there appears to be an implicit type casting going on with SELECT, so lets see
         if a cast is needed on the loaded value. *)        
        
        val loadedVal = let
                val lhsTy = (LT.typeOf o CFG.Var.typeOf) lhsVar
                val loadedTy = LB.toTy loadedVal
            in
                if LT.same(lhsTy, loadedTy)
                then loadedVal (* do nothing *)
                else cast (Op.equivCast(loadedTy, lhsTy)) (loadedVal, lhsTy)
            end  
            
        in
            insertV(env, lhsVar, loadedVal)
        end

      
      and genUpdate(env, (i, ptr, var)) = let      
        val llVal = lookupV(env, var)
        val llPtr = lookupV(env, ptr)
        val SOME addr = LPU.calcAddr b i llPtr
        
        (* we need to cast the pointer to the correct type before the store *)
          val caster = let
                  val slotTy = CFGTyUtil.select(CFG.Var.typeOf ptr, i)
                  val slotLLTy = LT.mkPtr(LT.typeOf slotTy)
                  val addrTy = LB.toTy addr
              in
                  if LT.same(slotLLTy, addrTy)
                  then (fn x => x) (* do nothing *)
                  else (fn instr => cast Op.BitCast (addr, slotLLTy))         
              end  
        
        val newLLVar = mk Op.Store #[caster addr, llVal]
      in
        env  (* store has an empty result *)
      end
      
      and genAddrOf(env, (lhsVar, i, var)) = let
        val llv = lookupV(env, var)
      in
        (case LPU.calcAddr b i llv
            of SOME newLLVar => insertV(env, lhsVar, newLLVar)
             | NONE => ( debug ("AddrOf " ^ Int.toString i) var llv ; raise Fail "unrecoverable error" )
        (* esac *))
      end
      
      and debug thing rhsVar llv = (print (
          "problem translating " ^ thing ^ " whose argument is CFG var "
          ^ (CV.toString rhsVar) ^ " : " ^ (CFGTyUtil.toString(CFG.Var.typeOf rhsVar)) ^ "\nwhich became LLVM var of type "
          ^ (LT.fullNameOf(LB.toTy llv)) ^ "\n"
          ))
          
      
      and genAlloc(env, (lhsVar, ty, vars)) = let
        val llVars = L.map (fn x => lookupV(env, x)) vars
        
        val _ = if L.length llVars = 0 then raise Fail "empty alloc!" else ()
        
        val headerTag = LPU.headerTag (L.map CV.typeOf vars)
        val {newAllocPtr, tupleAddr=allocatedTuple} = 
            LPU.doAlloc b (lookupMV(env, MV_Alloc)) llVars headerTag
        
        val lhsTy = LT.typeOf ty
        val maybeCasted = if LT.same(lhsTy, LB.toTy allocatedTuple) then allocatedTuple
                          else (* there was an implicit cast involving the any ty upon binding *)
                            cast Op.BitCast (allocatedTuple, lhsTy)
        
        val env = insertV(env, lhsVar, maybeCasted)
        val env = updateMV(env, MV_Alloc, newAllocPtr)
      in
        env
      end
      
      and genGAlloc(env, (lhsVar, ty, vars)) = 
      (* NOTE not implemented yet. should go very similarly to local alloc *)
        raise Fail "not implemented yet"
      
      and genPromote(env, (lhsVar, var)) = let
        val (promLab, NONE) = LR.promote
        val llFunc = LB.fromV promLab
        val paramTys = (LT.argsOf o LT.deref o LB.toTy) llFunc
        
        val args = [lookupMV(env, MV_Vproc), lookupV(env, var)]
        
        val llArgs = L.map (fn (ll, realTy) => let
                            val llty = LB.toTy ll
                        in
                            cast (Op.equivCast (llty, realTy)) (ll, realTy)
                        end)
                            (ListPair.zipEq(args, paramTys))

        (* we must save and restore the allocation pointer using the vproc
           before & after the call to promote just like a c call that allocates. *)
        val loc = { vproc = lookupMV(env, MV_Vproc),
                    off   = Spec.ABI.allocPtr }
        val alloc = lookupMV(env, MV_Alloc)
        
        val _ = LPU.saveAllocPtr b loc alloc
        
        (* do call *)
        val llCall = LB.call b (llFunc, V.fromList llArgs)
            
        val env = updateMV(env, MV_Alloc, LPU.restoreAllocPtr b loc)
        
        
        (* cast result back *)
        val lhsTy = (LT.typeOf o CV.typeOf) lhsVar
        val llRes = cast (Op.equivCast (LB.toTy llCall, lhsTy)) (llCall, lhsTy)
        
        in
            insertV(env, lhsVar, llRes)
        end
        
      
      and genPrim0(env, prim) = let
        val llArgs = L.map (fn x => lookupV(env, x)) (PU.varsOf prim)
        val cvtr = OU.fromPrim b prim
      in
        (* lhs for Prim0 so dont update the env. NOTE we're assuming
            no regular prims with a lhs ended up in a Prim0 *)
        (cvtr llArgs ; env) 
      end
      
      and genPrim(env, (lhsVar, prim)) = let
        val llArgs = L.map (fn x => lookupV(env, x)) (PU.varsOf prim)
        val cvtr = OU.fromPrim b prim
        val (result, env) = (cvtr llArgs, env) 
                            handle OU.ParrayPrim f => let
                                val arg = { resTy = (LT.typeOf o CV.typeOf) lhsVar,
                                            vproc = lookupMV(env, MV_Vproc),
                                            alloc = lookupMV(env, MV_Alloc),
                                            allocOffset = Spec.ABI.allocPtr }
                                val { result, alloc } = f arg
                            in
                                (result, updateMV(env, MV_Alloc, alloc))
                            end
      in
        insertV(env, lhsVar, result)
      end
         
      
      (* A standard C call that returns. *)
      and genCCall(env, (results, func, args)) = let
            val llFunc = lookupV(env, func)            
            val argTys = (LT.argsOf o LT.deref o LB.toTy) llFunc
            
            val llArgs = L.map (fn (a, realTy) => let
                                val ll = lookupV(env, a)
                                val llty = LB.toTy ll
                            in
                                cast (Op.equivCast (llty, realTy)) (ll, realTy)
                            end)
                                (ListPair.zipEq(args, argTys))
                                
            (* thunk the call for a hot minute *)
            val doCall = (fn () => LB.call b (llFunc, V.fromList llArgs))
            
            (* check if the C function might allocate before performing the call *)
            val allocates = LPU.cfunDoesAlloc func
            
            val (llCall, env) = if not allocates then (doCall(), env)
                   else let (* we need to pass the allocation pointer to the runtime system,
                                    and we do so by writing it to the vproc. *)
                            val loc = { vproc = lookupMV(env, MV_Vproc),
                                        off   = Spec.ABI.allocPtr }
                            val alloc = lookupMV(env, MV_Alloc)
                            
                            val _ = LPU.saveAllocPtr b loc alloc
                            
                            val llCall = doCall()
                            
                            val newAlloc = LPU.restoreAllocPtr b loc
                        in
                            (llCall, updateMV(env, MV_Alloc, newAlloc))
                        end
            
          in
            (case results
              of nil => env
               | [res] => let
                        val lhsTy = (LT.typeOf o CV.typeOf) res
                        val llRes = cast (Op.equivCast (LB.toTy llCall, lhsTy)) (llCall, lhsTy)
                    in
                        insertV(env, res, llRes)
                    end
               | _ => raise Fail "dont know how to handle this"
            (* esac *))
          end
          
      
      and genHostVProc(env, lhsVar) = insertV(env, lhsVar, lookupMV(env, MV_Vproc))
      
      and genVPLoad(env, (lhsVar, offset, vpVar)) = let
        val lhsTy = (LT.typeOf o CV.typeOf) lhsVar
        val vpLL = lookupV(env, vpVar)
        
        (* now we do the offset & loading sequence *)
        val addr = LPU.vpOffset b vpLL offset (LT.mkPtr(lhsTy))
        val final = mkVolatile Op.Load #[addr] 
      in
        insertV(env, lhsVar, final)
      end
      
      and genVPStore(env, (offset, vpVar, arg)) = let
        val argLL = lookupV(env, arg)
        val argTy = LB.toTy argLL
        val vpLL = lookupV(env, vpVar)    
        
        (* offset and store seq *)
        val addr = LPU.vpOffset b vpLL offset (LT.mkPtr(argTy))
        val _ = mkVolatile Op.Store #[addr, argLL] (* no resulting instr after store *)
      in
        env
      end
      
      and genVPAddr(env, (lhsVar, offset, vpVar)) = let
        val lhsTy = (LT.typeOf o CV.typeOf) lhsVar
        val vpLL = lookupV(env, vpVar)
      in
        insertV(env, lhsVar, LPU.vpOffset b vpLL offset lhsTy)
      end

    in
        (* we return a thunk that will actually terminate the block once all of its
           predecessors have been processed *)
        finish(process(initialEnv, body), exit)
    end (* end of fillBlock *)

    

  (* end of Basic Blocks *)

(****** Functions ******)
    (* NOTE: this probably should be moved into a new module or something *)
    
  fun mkFunc (func as C.FUNC{entry,...}, initEnv) = (case entry
      of C.KnownDirectFunc {ret,...} => mkDSFunc(func, ret, initEnv)
       | C.StdDirectFunc {ret,...} => mkDSFunc(func, ret, initEnv)
       
       | C.StdFunc _ => mkCPSFunc(func, initEnv)
       | C.StdCont _ => mkCPSFunc(func, initEnv)
       | C.KnownFunc _ => mkCPSFunc(func, initEnv)
      (* end case *))
  
  and mkCPSFunc (f as C.FUNC { lab, entry, start=(start as C.BLK{ args=cfgArgs, ... }), body }, 
              initEnv as ENV{labs=inherited_labs, vars=inherited_vars, blks=inherited_blks, ...}) : string = let
    
    val (mvTys : LT.ty list, cc : (int * C.var) list) = determineCC(entry, cfgArgs)
    
    val pairedMvTys = ListPair.zipEq(L.tabulate(numMachineVals, fn i => i), mvTys)
    
    (* reg vars and the real types *)
    val mvRegs = L.map (fn (i, ty) => let
            val (SOME mv) = IdxMachineVal i
            val (_, name, realTy) = machineInfo mv
        in
            (i, LV.new(name, ty), realTy)
        end) pairedMvTys
        
    val ccRegs = L.map (fn (i, cvar) => let
            val name = CV.nameOf cvar
            val realTy = (LT.typeOf o CV.typeOf) cvar
            val ty = LT.toRegType realTy
        in
            (i, LV.new(name, ty), realTy)
        end) cc
    
    
    (* NEXT now we assign mvRegs :: ccRegs to the jwaCC slots,
       filling in junk slots with "unused" LV's.
       then we pass these two lists to mkBasicBlocks so that 
       a block of bitcasts is produced in the header to fixup 
       the environment. *)
       
    datatype slotTy
     = Used of LV.var
     | NotUsed of LT.ty
       
    (* NOTE the regs must be ordered by slot num *)
    fun assign(nil, nil, res) = L.rev res 
      | assign(slot::rest, nil, res) = assign(rest, nil, (NotUsed (V.sub(LT.jwaCC, slot)))::res)
      | assign(slot::rest, (regs as ((r as (idx, var, _))::rs)), res) =
        if idx = slot 
            then assign(rest, rs, (Used var)::res)
            else assign(rest, regs, (NotUsed (V.sub(LT.jwaCC, slot)))::res)
       
    val slotNums = L.tabulate(V.length LT.jwaCC, fn i => i)
    
    val allRegs = mvRegs @ ccRegs
    
    val _ = if (L.length allRegs) > (L.length slotNums)
            then print ("(llvm-backend) warning: number of live vars across a function call\n"
                        ^ "exceeds the number of registers in jwaCC, thus some values may\n"
                        ^ "be passed via the stack!") else ()
                        
                        (* NOTE this warning is mostly of concern for loops, as
                           each iteration will cause a register spill/reload.
                           If a GC triggers, we'll also have to load these values
                           from the stack just to move them to the heap, and back again
                           upon resuming.
                        *)
    
    val allAssign = assign(slotNums, allRegs, nil)  
    
    val mvs = V.fromList(L.map (fn (_, var, _) => LB.fromV var) mvRegs)
    
    fun mkDecl (Used var) = ((LT.nameOf o LV.typeOf) var) ^ " " ^ (LV.toString var)
      | mkDecl (NotUsed ty) = LT.nameOf ty
    
    fun stringify vars = S.concatWith ", " (L.map mkDecl vars)
    
    val comment = (* "; comment use to be here \n" *) ""
    
    (*val comment = S.concat ["; CFG type: ", CTU.toString cfgTy, "\n",
                            "; LLVM type: ", (stringify  llParamTys), "\n",
                            "; LLVM arity = ", i2s(List.length llParamTys), "\n" ]*)
   
    (* string building code *)
    val linkage = linkageOf lab
    val ccStr = " " ^ (LB.cctoStr LB.jwaCC) ^ " " (* Only available in Kavon's modified version of LLVM. *)
    val llName = LV.toString(lookupL(initEnv, lab))
    val decl = [comment, "define ", linkage, ccStr,
                "void ", llName, "(", (stringify  allAssign), ") ",
                stdAttrs(MantiFun), " {\n"]
                
                (* FIXME put a noalias on the allocation pointer and see if it improves LLVM's codegen *)
    
    (* now we setup the environment, we need to make fresh vars for the reg types,
       and map the original parameters to the reg types when we call mk bbelow *)
    
    val body = mkBasicBlocks (ENV{labs=inherited_labs,
                                  vars=inherited_vars,
                                  blks=inherited_blks,
                                  mvs=mvs},
                                start, body, Padded (cc, ccRegs, mvRegs))  

    val total = S.concat (decl @ body @ ["\n}\n\n"])
  in
    total
  end
  
  
  and mkDSFunc (f as C.FUNC { lab, entry, start=(start as C.BLK{ args=cfgArgs, ... }), body }, 
                ret,
              initEnv as ENV{labs=inherited_labs, vars=inherited_vars, blks=inherited_blks, ...}) : string = let
    
    fun dclToStr var = ((LT.nameOf o LV.typeOf) var) ^ " " ^ (LV.toString var)
    fun stringify vars = S.concatWith ", " (L.map dclToStr vars)

    val (_,mvs) = freshMVs()
    
    (* the calling convention used by DS funs *)
    val cfgArgs = C.paramsOfConv(entry, cfgArgs)
    val llArgs = List.map LV.convert cfgArgs
    val allAssign = mvs @ llArgs
    
    (* the return convention/type used by DS funs *)
    val mvTys = L.map machineValTy mvCC
    val returnedValTys = L.map LT.typeOf ret
    val retTyStr = LT.nameOf(LT.mkUStruct (mvTys @ returnedValTys))
    
    val attrs = ""
    
    (* string building code *)
    val linkage = linkageOf lab
    val ccStr = " " ^ (LB.cctoStr LB.fastCC) ^ " "
    val llName = LV.toString(lookupL(initEnv, lab))
    val decl = ["define ", linkage, ccStr,
                retTyStr, " ", llName, "(", (stringify allAssign), ") ",
                attrs, " gc \"statepoint-example\" {\n"]
                
                (* FIXME put a noalias on the allocation pointer and see if it improves LLVM's codegen *)
    
    (* now we setup the environment, we need to make fresh vars for the reg types,
       and map the original parameters to the reg types when we call mk bbelow *)
    
    val body = mkBasicBlocks (ENV{labs=inherited_labs,
                                  vars=inherited_vars,
                                  blks=inherited_blks,
                                  mvs= V.fromList(L.map LB.fromV mvs) },
                                start, body, Regular {llArgs=llArgs,cfgArgs=cfgArgs,mvs=mvs})  
                                
    val total = S.concat (decl @ body @ ["\n}\n\n"])
  in
    total
  end

  and linkageOf (label) = (case CL.kindOf label
    of C.LK_Func { export = NONE, ... } => "internal"
     | C.LK_Func { export = SOME _, ... } => "external"
     | _ => raise Fail ("linkageOf is only valid for manticore functions.")
     (* end case *))

(****** end of Functions ******)


  (* Module *)
  
  (* in particular, this just generates essentially a "header" for the LLVM module
     with things such as the datatype layouts, externals, attributes and so on.
     it also initializes the extern info map. *)
  fun mkFunDecls () = let


    fun attrOfC (a : CF.attribute) = (case a
          of CF.A_pure => "" (* FIXME "readonly" *)
           | CF.A_noreturn => "noreturn"
           (* alloc/malloc attribute in C doesn't seem to translate over to LLVM IR *)
           | _ => ""
          (* end case *)) 
          

    (* external C function. note that var has type CFG.label *)
    fun toLLVMDecl (CF.CFun { var, attrs, ... }) = let
        val asLLV = LV.convertLabel var
        val llvmAttrs = S.concat(mapSep(attrOfC, [stdAttrs(ExternCFun)], " ", attrs))
      in
        ( (LT.declOf (LV.typeOf asLLV) "" (LV.toString asLLV)) ^ " " ^ llvmAttrs ^ "\n",
         var,
         asLLV)
      end

    val (targetTriple, dataLayout) = let
        (* NOTE it turns out that the code generator does not respect datalayout strings! NOTE
        
        In non-packed structs, padding between field types is inserted as defined by the 
        DataLayout string in the module, which is required to match what the underlying 
        code generator expects.
        
        The alignment of fields in the unpacked structs is important for our current GC 
        implementation, because our GC assumes that all fields are 8 bytes wide (doesn't 
        have to, but that's what it is currently doing) when interpreting the tag bits. 
        Address arithmetic in our current BOM code probably relies on this too but I
        haven't confirmed this. Thus, when we set the alignments of things below, it's
        relative to the struct it is in.
        
        I will walk through the different components of the data layout we're using below,
        as every part of it is very important!
        *)
        fun dataLayout endianness mangling = S.concatWith "-" 
            [ endianness        (* E is big endian, e is small endian *)
            , mangling          (* m:o = Mach-O name mangling, m:e = ELF *)
            , "S64"             (* stack is naturally 8 byte aligned (see the tech report
                                   for details about why we need to specify this). NOTE
                                   that this specification is currently ignored by llc,
                                   it's more or less just to tell opt what the alignment
                                   will be. we will need to tell llc about this alignment
                                   manually. *)
            , "p:64:64:64"      (* pointers are 64 bits wide, and both the abi and preferred
                                   alignment is 64 bits *)
            , "i64:64:64"          (* 64 bit integers are 64 bit aligned *)
            
            (* the following requests will not actually be respected by the code generator,
               instead, this datalayout string must match what the code generator decides to do. 
            
            , "i32:64:64"          (* 32 bit integers are 64 bit aligned *)
            , "i16:64:64"          (* 16 bit integers are 64 bit aligned *)
            , "i8:64:64"           (* 8 bit integers are 64 bit aligned *)
            , "i1:64:64"           (* 1 bit integers are 64 bit aligned *)
            , "f16:64:64"          (* 16 bit floats are 64 bit aligned *)
            , "f32:64:64"          (* 32 bit floats are 64 bit aligned *)
            , "f64:64:64"          (* 64 bit floats are 64 bit aligned *)
            , "a:0:64"           (* 64 bit alignment of aggregate type *)
            *)
            ]

          in (case (Spec.archName, Spec.osName)
          of ("x86_64", "darwin") => ("x86_64-apple-macosx", dataLayout "e" "m:o")
           | ("x86_64", "linux") => ("x86_64-pc-linux", dataLayout "e" "m:e")
           | _ => raise Fail ("Unsupported OS/arch type: " ^ Spec.archName ^ " " ^ Spec.archName)
          (* end case *))
          end


    val convertedExterns = List.map toLLVMDecl module_externs
    val externDecls = S.concat (List.map (fn (s, _, _) => s) convertedExterns)
    
    (* now we add the magic llvm runtime stuff that are not actually part of the CFG module. *)
    val runtimeDecls = magicDecls LR.runtime (stdAttrs ExternCFun)
    
                                                    
    val intrinsicDecls = magicDecls LR.intrinsics ""
    

    val header = S.concat [
      "target datalayout = \"", dataLayout, "\"\n",
      "target triple = \"", targetTriple, "\"\n\n",
      "\n; externed decls in the module\n",
      externDecls,
      "\n; decls to access runtime system\n",
      runtimeDecls,
      "\n; llvm intrinsics\n",
      intrinsicDecls
       ]

    in
      (header, convertedExterns)
    end
    
  and magicDecls lst attrs = S.concat (L.map 
        (fn (llv, cc) => let
          val cc = case cc of SOME lbCC => LB.cctoStr lbCC | NONE => ""
        in
          (LT.declOf (LV.typeOf llv) cc (LV.toString llv)) ^ " " ^ attrs ^ "\n"
        end) lst)

  (* end of Module *)

  (* initialize the initial environment, which has all of the functions in it. *)
  
  val (header, convertedExterns) = mkFunDecls()
  
  (* manticore functions *)
  val initEnv = L.foldl 
    (fn (C.FUNC { lab, ...}, acc) =>
        insertL(acc, lab, LV.convertLabel lab)) emptyEnv module_code
        
  (* C external functions *)
  val initEnv = L.foldl 
    (fn ((_, old, new), acc) =>
        insertL(acc, old, new)) initEnv convertedExterns


  (* we need the name of the main function in the module in order to generate an alias
     for it called maintEntry so the C runtime knows what the main fn is. *)
  val (C.FUNC { lab = mainLab, ...})::_ = module_code
  val mainFn = lookupL(initEnv, mainLab)

  (* process the whole module, generating a string for each function and populating the type
     and string literal caches *)
  val funStrings = List.map (fn func => mkFunc(func, initEnv)) module_code  
  
  fun externalConstants () = let
    fun globalFormatter lv init = S.concat[LV.toString lv, " = global ",
                                        (LT.nameOf o LV.typeOf) lv, " ", init, ", align 8\n"]
                                        
    fun aliasFormatter lv init = S.concat[LV.toString lv, " = alias ",
                                        (LT.nameOf o LT.deref o LV.typeOf) lv, ", ",
                                        (LT.nameOf o LV.typeOf) lv, " ", init, "\n"]
                                        
    val sequentialFlag = if Controls.get BasicControl.sequential then 1 else 0
  in
    [ globalFormatter LR.magic (IntInf.toString Spec.ABI.magic),
      globalFormatter LR.sequential (IntInf.toString sequentialFlag),
      aliasFormatter (LR.main (LV.typeOf mainFn)) (LV.toString mainFn)
    ]
  end

in
  ( (* output sequence *)
    
    (* header *)
    pr (S.concat 
        ["; Generated by Manticore\n",
         "; ModuleID = '", Atom.toString module_name, "'"]) ;

    (* types need to go first, because they must be declared before used in functions etc*)
    pr "\n\n; type decls (if any)\n\n" ;
    pr (LT.typeDecl()) ;  

    pr "\n\n; externs & target info\n\n" ;
    pr (header) ; (* declare extern funs, target triple, and datalayout *)

    pr "\n\n; manticore function defs\n\n" ;
    List.app pr funStrings ;

    pr "\n\n; string literals\n\n" ;
    prl (LS.export()) ;  
    
    pr "\n\n; external constants\n" ;
    prl (externalConstants()) ;
    
    pr "\n\n; statepoint intrinsics, if any\n\n" ;
    pr (magicDecls (LLVMStatepoint.exportDecls ()) "") ;

    pr "\n\n\n\n; ---------------- end of LLVM generation ---------------------- \n\n\n\n" ;
    (if DEBUGGING then
        PrintCFG.output {counts=true, types=PrintCFG.Full, preds=false} (TextIO.stdErr, module)
    else ()) ;
    ()
  )

end

     

end
