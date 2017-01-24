(* llvm-print-util.sml
 *
 * COPYRIGHT (c) 2016 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Utility functions used during the translation from CFG to LLVM
 *)
 
structure LLVMPrinterUtil = struct
local
    
    
    structure LV = LLVMVar
    structure LT = LV.LT
    structure LB = LLVMBuilder 
    structure Ty = LLVMTy
    structure Op = LLVMOp
    structure S = String
    structure L = List
    structure V = Vector
    structure A = LLVMAttribute
    structure AS = LLVMAttribute.Set
    structure W = Word64
    structure C = CFG
    structure CV = CFG.Var
    structure CL = CFG.Label
    structure CT = CFGTy
    structure CTU = CFGTyUtil
    structure CF = CFunctions

in

      
  fun calcAddr b idx llInstr = let
    val llvTy = LB.toTy llInstr
    val zero = LB.intC(LT.i32, 0)
    val idxNum = Int.toLarge idx
  in
      (case LT.node llvTy
        of Ty.T_Ptr (_, t) => (case LT.node t
            of (Ty.T_Vector _
               | Ty.T_Array _
               | Ty.T_Struct _
               | Ty.T_UStruct _) => SOME (LB.gep_ib b (llInstr, #[zero, LB.intC(LT.i32, idxNum)]))
             
             | _ => SOME (LB.gep_ib b (llInstr, #[LB.intC(LT.i32, idxNum)]))
             
            (* esac *))
         | _ => NONE
      (* esac *))
  end
  
  (* just to keep the vp instructions consistent *)
  fun vpOffset b vpLL offset resTy = let
    val offsetLL = LB.fromC(LB.intC(LT.i64, offset))
    
    (* We take the VProc ptr, offset it, and bitcast it to the kind of pointer we want *)
    val r1 = LB.cast b Op.PtrToInt (vpLL, LT.i64)
    val r2 = LB.mk b AS.empty Op.Add #[r1, offsetLL]
    val final = LB.cast b Op.IntToPtr (r2, resTy)
  in
    final
  end
  
  (* Given a list of CFG tys, returns a header tag corresponding to an allocation
     of corresponding values in the heap. The order must match the layout in the actual heap,
     from decreasing to increasing. Here's the picture just before an allocation is going to
     occur, where the alloc pointer is currently pointing at first free word in the heap.
     
         [ HEADER ][ cfgVar1, cfgVar2, ...., cfgVarN ]
             ^
             |
         alloc ptr
    
     <- Low address                                High address ->
     
     We cannot operate solely on LLVM types because enum types are represented with
     integers, and we need to determine the kind of enum from the CFG representation
     so we know whether it has a uniform rep or a mixed rep. Also some other things which
     are pointers in LLVM are not pointers into the heap (function pointers, vproc, deque etc.)
     This implementation is based on alloc64-fn.sml
     
      *)
      
  (* TODO I wonder why CFG.T_Addr is not considered a heap pointer in the old backend?
     my guess is that Addrs are for pointers derived from pointer arithmetic. *)
     
  fun isHeapPointer CFG.T_Any = true
    | isHeapPointer (CFG.T_Tuple _) = true
    | isHeapPointer (CFG.T_OpenTuple _) = true
    | isHeapPointer _ = false
      
  fun headerTag (ctys : CFGTy.ty list) : LB.instr = let
    
    
      
      
    (* initializes a non-forwarding pointer header, following header-bits.h
        
        ---------------------------------------------- 
        | -- 48 bits -- | -- 15 bits -- | -- 1 bit -- |
        |	  length    |      ID       |      1      |
        ----------------------------------------------
     *)
    fun packHeader length id = W.toLargeInt (
        W.orb (W.orb (W.<< (W.fromInt length, 0w16), 
                        W.<< (W.fromInt id, 0w1)
                       ),
              0w1))
      
    (* all non-pointer (raw) values. assuming proper word alignment *)
    and rawHeader ctys = let
        val id = 0
        val nWords = L.length ctys (* NOTE how this isn't the number of bytes! *)
        val hdrWord = packHeader nWords id
    in
        hdrWord
    end
    
    (* all pointer values. *)
    and vectorHeader ctys = let
        val id = 1
        val nWords = L.length ctys (* NOTE how this isn't the number of bytes! *)
  	    val hdrWord = packHeader nWords id
    in
        hdrWord
    end
    
    (* a mix of pointers and raw values *)
    and mixedHeader ctys = let
        
        fun setPtrBits (x, acc) = 
            (if isHeapPointer x then "1" else "0") ^ acc
    
        val ptrMask = L.foldl setPtrBits "" ctys
        
        val id = HeaderTableStruct.HeaderTable.addHdr (HeaderTableStruct.header, ptrMask)
        val nWords = L.length ctys (* NOTE how this isn't the number of bytes! *)
        val hdrWord = packHeader nWords id
    in
        hdrWord
    end
  
    and classify (hasPtr, hasRaw, c::cs) = 
            if isHeapPointer c 
                then classify (true, hasRaw, cs) 
            else if CFGTyUtil.hasUniformRep c 
                then classify (hasPtr, hasRaw, cs)
            else classify (hasPtr, true, cs)
            
      | classify (true, false, []) = vectorHeader ctys
      | classify (true, true, []) = mixedHeader ctys
      | classify (false, _, []) = rawHeader ctys
      
      
  in
    LB.fromC(LB.intC(LT.gcHeaderTy, classify (false, false, ctys)))
  end
  
  
  (* allocates space on the heap and returns all of the interesting
     addresses for the new allocation. In particular, it will return a function
     that computes the addresses of the slots into which
     elements can be stored to initialize them. It takes
     integers to index these slots and generates the instructions.
     
     NOTE this function will NOT initialize the new space, it's up to the caller to do it.
     
     
     The convention we follow in order to match up with the runtime system is the following:
     
     
     [ end of last allocation ][8 bytes][8 bytes][8 bytes ....]
                                                
                                           ^
                                           |
                                       alloc ptr        
                                       
    Runtime system functions which do allocation expect the allocation pointer to be in this
    state, because they offset by [-1] to write the header.
   *)
  fun bumpAllocPtr b allocPtr llTys = let
      val gep = LB.gep_ib b
      val cast = LB.cast b
      val mk = LB.mk b AS.empty
      
      (* uniformTy corresponds to the type of a tuple. *)
      val tupleAddr = cast Op.BitCast (allocPtr, LT.uniformTy) 
      
      
      
      fun c idxNum = LB.intC(LT.i32, Int.toLarge idxNum)
      
      (* header addr offsets behind the allocation pointer *)
      val headerAddr = gep (allocPtr, #[c ~1])
      
      val bumpOffset = (L.length llTys) + 1
      val newAllocPtr = gep (allocPtr, #[c bumpOffset])
      
      
      fun tupleCalc idx = gep (tupleAddr, #[c idx])
  
  in
    {tupleCalc=tupleCalc, tupleAddr=tupleAddr, newAllocPtr=newAllocPtr, headerAddr=headerAddr}
  end
  
  
  (* returns ptr to new allocation and the properly offset alloc ptr.
      callers should expect to cast the returned pointer to the correct type! *)
  fun doAlloc b allocPtr llVars headerTag = let
    val gep = LB.gep_ib b
    val cast = LB.cast b
    val mk = LB.mk b AS.empty
    
    fun asPtrTo ty addr = let
            val addrTy = LB.toTy addr
            val desiredTy = LT.mkPtr ty
        in
            if LT.same(addrTy, desiredTy)
            then addr
            else cast Op.BitCast (addr, desiredTy)
        end
    
    val llTys = L.map (fn x => LB.toTy x) llVars
    val {tupleCalc, tupleAddr, newAllocPtr, headerAddr} = bumpAllocPtr b allocPtr llTys
    
    val _ = mk Op.Store #[asPtrTo LT.gcHeaderTy headerAddr, headerTag]
    
    val _ = L.foldl (fn (var, idx) => let
                            val varTy = LB.toTy var
                            val slotAddr = tupleCalc idx
                            val _ = mk Op.Store #[asPtrTo varTy slotAddr, var]
                        in idx + 1 end)
                    0 llVars
    in
        {newAllocPtr=newAllocPtr, tupleAddr=tupleAddr}
    end
        
  
  fun saveAllocPtr bb {vproc, off} allocPtr = let
        val volatile = AS.singleton A.Volatile
        val slot = vpOffset bb vproc off (LT.mkPtr LT.allocPtrTy)
        val _ = LB.mk bb volatile Op.Store #[slot, allocPtr]
      in
        ()
      end
      
  and restoreAllocPtr bb {vproc, off} = let
        val volatile = AS.singleton A.Volatile
        val slot = vpOffset bb vproc off (LT.mkPtr LT.allocPtrTy)
        val newAlloc = LB.mk bb volatile Op.Load #[slot]
      in
        newAlloc
      end
     
local 
  fun getCPrototype f = (case CV.typeOf f
      of CT.T_CFun proto => proto
	   | _ => raise Fail ((CV.toString f) ^ " is not a C function!")
      (* end case *))
in
    (* returns true iff this cfun perform allocation *)
    fun cfunDoesAlloc f = let
        (* get the C function's prototype *)
         val cProtoTy = getCPrototype f
        (* check if the C function might allocate *)
         val allocates = CFunctions.protoHasAttr CFunctions.A_alloc cProtoTy
    in
        allocates
    end
end      
      
end (* end local scope *)
end (* end LLVMPrinterUtil *)
