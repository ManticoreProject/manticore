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

in

      
  fun calcAddr b idx llInstr = let
    val llvTy = LB.toTy llInstr
    val zero = LB.intC(LT.i32, 0)
    val idxNum = Int.toLarge idx
  in
      (case LT.node llvTy
        of Ty.T_Ptr t => (case LT.node t
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
  fun headerTag (ctys : CFGTy.ty list) : LB.instr = let
    
    (* TODO I wonder why CFG.T_Addr is not considered a heap pointer in the old backend?
       my guess is that Addrs are for pointers derived from pointer arithmetic. *)
       
    fun isHeapPointer CFG.T_Any = true
      | isHeapPointer (CFG.T_Tuple _) = true
      | isHeapPointer (CFG.T_OpenTuple _) = true
      | isHeapPointer _ = false
      
      
    (* initializes a non-forwarding pointer header, following header-bits.h
        
        ---------------------------------------------- 
        | -- 48 bits -- | -- 15 bits -- | -- 1 bit -- |
        |	  length    |      ID       |      1      |
        ----------------------------------------------
     *)
    and packHeader length id = W.toLargeInt (
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
   *)
  fun bumpAllocPtr b allocPtr llTys = let
      val gep = LB.gep_ib b
      val cast = LB.cast b
      val mk = LB.mk b AS.empty
      
      val tagTy = LT.gcHeaderTy
      val oldAllocPtrTy = LB.toTy allocPtr
      
      (* build the types we'll need *)
      (* it's important that the tupleTy is an unpacked struct, because
         the datalayout correct pads the values so the GC is happy with it *)
      val tupleTy = LT.mkUStruct(llTys) 
      val heapFrameTy = LT.mkPtr(LT.mkUStruct( tagTy :: tupleTy :: nil ))
      
      (*  now lets calculate addresses. the invariant about the alloc pointer is that it
          points to unallocated memory (the next allocation's header ty), so that's
          what we need to return *)
      val allocPtr = cast Op.BitCast (allocPtr, heapFrameTy)
      
      
      
      fun c idxNum = LB.intC(LT.i32, Int.toLarge idxNum)
      
      val headerAddr = gep (allocPtr, #[c 0, c 0])
      val tupleAddr = gep (allocPtr, #[c 0, c 1])
      
      val newAllocPtr = cast Op.BitCast (gep (allocPtr, #[c 1]), oldAllocPtrTy)
      
      
      fun tupleCalc idx = gep (allocPtr, #[c 0, c 1, c idx])
  
  in
    {tupleCalc=tupleCalc, tupleAddr=tupleAddr, newAllocPtr=newAllocPtr, headerAddr=headerAddr}
  end
  
  
  (* returns ptr to new allocation and the properly offset alloc ptr *)
  fun doAlloc b allocPtr llVars headerTag = let
    val gep = LB.gep_ib b
    val cast = LB.cast b
    val mk = LB.mk b AS.empty
    
    val llTys = L.map (fn x => LB.toTy x) llVars
    val {tupleCalc, tupleAddr, newAllocPtr, headerAddr} = bumpAllocPtr b allocPtr llTys
    
    val _ = mk Op.Store #[headerAddr, headerTag]
    
    val _ = L.foldl (fn (var, idx) =>
                ((mk Op.Store #[tupleCalc idx, var]) ; (idx + 1))) 0 llVars
    
    in
        {newAllocPtr=newAllocPtr, tupleAddr=tupleAddr}
    end
        
      
end (* end local scope *)
end (* end LLVMPrinterUtil *)
