(* llvm-op-util.sml
 *
 * COPYRIGHT (c) 2016 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Utility functions used during the translation from CFG to LLVM, particularly to handle
 * primops.
 *)

structure LLVMOpUtil = struct

(* because cyclic Op.dependency between LLVMOp & LLVMBuilder *)

  structure LB = LLVMBuilder
  structure P = Prim
  structure AS = LLVMAttribute.Set
  structure LT = LLVMType
  structure LR = LLVMRuntime
  structure A = LLVMAttribute
  structure Op = LLVMOp
  structure LPU = LLVMPrinterUtil
  
  structure V = Vector
  structure L = List
  structure S = String
  
  (* Turns out that a handful of primops realated to parrays are
     implemented in such a way that the current vproc and allocation pointer
     are needed, and the allocation pointer is modified by C code in the runtime system.
     While it would be nice to translate this C code to LLVM to avoid the cost of the call,
     ideally in the future we would upgrade the implementation to take advantage of SSE hardware.
     
     The "allocOffset" is the constant offset in the vproc data structure that hold the current
     alloc pointer.
   *)
    
  exception ParrayPrim of 
    { resTy : LT.ty,
      vproc : LB.instr, 
      alloc : LB.instr,
      allocOffset : IntegerLit.integer } -> { alloc : LB.instr, result : LB.instr }


local

    val e = AS.empty
    
    (* generate a integer constant with the same type
    as the given integer instruction type *)
    fun const f instr c = LB.fromC(f (LB.toTy instr, c))
    val Iconst = const LB.intC
    val Fconst = const LB.floatC
    
    fun asTy bb ty instr = LB.cast bb (Op.equivCast(LB.toTy instr, ty)) (instr, ty)
    
    (* the implementation of this follows directly from the output of the old MLRISC backend. *)
    fun allocXArray bb (label, NONE) = 
        (fn [ n ] => raise ParrayPrim (fn {resTy, vproc, alloc, allocOffset} => let
                val loc = {vproc=vproc, off=allocOffset}
                
                val _ = LPU.saveAllocPtr bb loc alloc

                (* call C routine to do the allocation, with bitcasts as needed *)
                val res = asTy bb resTy (LB.call bb 
                    (LB.fromV label, #[LB.cast bb Op.BitCast (vproc, LT.voidStar), n ]))
                    
                (* retrieve the modified allocation pointer *)
                val newAlloc = LPU.restoreAllocPtr bb loc
                
            in
                { alloc=newAlloc, result=res }
            end))
            
    
    fun intTy sz = LT.mkInt(LT.cnt sz)
    val i64 = LT.i64
    val i32 = LT.i32
    val i16 = LT.i16
    val i8  = LT.i8
    val float = LT.floatTy
    val double = LT.doubleTy
    val i64Star = LT.mkPtr i64
    val i32Star = LT.mkPtr i32
    
    fun id x = x
    
    fun addrArith bb sext opc = let
        val cast = LB.cast bb
        val mk = LB.mk bb e
        
        (* TODO NOTE this was written before LB.calcAddr was added,
           and we can eliminate inttoptr/ptrtoint and sext business and just do this:
           
           %r1 = bitcast adr to i8*
           %r2 = GEP %r1, off (whatever int type it is, whether const or not, will work, make sure to negate if the opc is a subtract since GEP is additive)
           %r3 = bitcast %r2 to adrTy
           
           its unclear whether this will have a benefit as of now (3/13), and since this
           currently works lets leave it alone
            *)
        
        fun sextI64 i = cast Op.SExt (i, i64)
        fun toI64 a = cast Op.PtrToInt (a, i64)
        fun toPtr i ty = cast Op.IntToPtr (i, ty)    
        
        (* LLVM rejects sext i64 to i64, rhs must be a smaller width *)
        val doSext = if sext then sextI64 else id    
    in
        (fn [adr, off] =>
            toPtr (mk opc #[toI64 adr, doSext off]) (LB.toTy adr))
    end
    
    (* arrays in CFG are represented with "ptr to any", thus we need to do a cast.  *)
    fun getArrOffset bb elmTy arrInstr idxInstr = let
            val castedArr = LB.cast bb Op.PtrToInt (arrInstr, LT.i64)
            
            (* determine the width of the array element's type in bytes *)
            val widthBitz = LT.widthOf elmTy (* gives bits *)
            val width = if (widthBitz mod 8) = 0 then Int.quot(widthBitz, 8) 
                         else raise Fail 
                            ("an array containing " ^ (LT.fullNameOf elmTy) 
                            ^ " is not byte addressable without alignment information!")
            
            (* might need to sign extend the index. we're assuming it's an integer of width
               at most 64 bits *)
            val sexted = if ((LT.widthOf o LB.toTy) idxInstr) = 64 
                         then idxInstr (* cannot sext an i64 to i64, so we leave it alone *)
                         else LB.cast bb Op.SExt (idxInstr, LT.i64)
                         
            (* offset is  index * width *)
            val neededOffset = LB.mk bb e Op.Mul #[sexted, LB.iconst LT.i64 width]
                            
            val offsetAddr = LB.mk bb e Op.Add #[castedArr, neededOffset]
            val offsetPtr = LB.cast bb Op.IntToPtr (offsetAddr, LT.mkPtr elmTy)
        in
            offsetPtr
        end
    
    and arrayLoad bb elmTy = (fn [arr, idx] => LB.mk bb e Op.Load #[getArrOffset bb elmTy arr idx])
        
    and arrayStore bb elmTy = (fn [arr, idx, elm] => LB.mk bb e Op.Store #[getArrOffset bb elmTy arr idx, elm])
    
in

(* b is the basic Op.block, p is the 'var prim, returns
  (LB.instr list -> LV.instr) that, when applied
  to a list of arguments for this llvmPrim, adds the right
  instructions to the given block and returns the final result
  of the operation *)
fun fromPrim bb p = let
  val f = LB.mk bb
  val c = LB.cast bb
  val fv = LB.fromV
in (case p
  of (P.I32Add _ | P.I64Add _)
      => (fn [a, b] => f e Op.Add #[a, b])
      
  | (P.I32Sub _ | P.I64Sub _)
      => (fn [a, b] => f e Op.Sub #[a, b])
          
  (* NOTE Mul in LLVM works on both signed or
    unsigned integers, but in CFG types its
    not clear whether we manage signedness properly.
    same goes for constants. we might need to
    do a conversion or something. 
    
    One thing to keep in mind is that all of
    our integers in CFG _do_ wrapping, so NSW/NUW are _not_ to be added *)
    
  | (P.I32Mul _ | P.I64Mul _ | P.U64Mul _) 
      => (fn [a, b] => f e Op.Mul #[a, b])
      
  | (P.I32Div _ | P.I64Div _)
      => (fn [a, b] => f e Op.SDiv #[a, b])
      
  | (P.I32Mod _ | P.I64Mod _)
      => (fn [a, b] => f e Op.SRem #[a, b])
  
  | (P.I32LSh _ | P.I64LSh _)
      => (fn [a, b] => f e Op.Shl #[a, b])
  
  | (P.I32Neg _ | P.I64Neg _)
      => (fn [a] => f e Op.Sub #[Iconst a 0, a])
      
      
  | P.U64Div _ => (fn [a, b] => f e Op.UDiv #[a, b])
  
  | P.U64Rem _ => (fn [a, b] => f e Op.URem #[a, b])
  
  
  | (P.F32Add _ | P.F64Add _)
      => (fn [a, b] => f e Op.FAdd #[a, b])
      
  | (P.F32Sub _ | P.F64Sub _)
      => (fn [a, b] => f e Op.FSub #[a, b])
      
  | (P.F32Mul _ | P.F64Mul _)
      => (fn [a, b] => f e Op.FMul #[a, b])
      
  | (P.F32Div _ | P.F64Div _)
      => (fn [a, b] => f e Op.FDiv #[a, b])
      
  | (P.F32Neg _ | P.F64Neg _)
      => (fn [a] => f e Op.FSub #[Fconst a (FloatLit.zero false), a])
  
      (* TODO add support for LLVM instrinsics to
       perform the primops we need. 
       http://llvm.org/docs/LangRef.html#llvm-sqrt-intrinsic
       http://llvm.org/docs/LangRef.html#llvm-fabs-intrinsic
       
       a key thing we need to do is declare the intrinsics we're using at the top of the file like so:
       
       declare float     @llvm.sqrt.f32(float)
       
       define ... {
          %r = call float @llvm.sqrt.f32(float 2.0)
       }
       
       
        *)
  | P.F32Sqrt _ => (fn [a] => LB.call bb (fv (#1(LR.sqrt_f32)), #[a]))
  | P.F64Sqrt _ => (fn [a] => LB.call bb (fv (#1(LR.sqrt_f64)), #[a]))
  
  | P.F32Abs _ => (fn [a] => LB.call bb (fv (#1(LR.abs_f32)), #[a]))
  | P.F64Abs _ => (fn [a] => LB.call bb (fv (#1(LR.abs_f64)), #[a]))


  | (P.I8RSh _ | P.I16RSh _ | P.I32RSh _ | P.I64RSh _ )
      => (fn [a, b] => f e Op.LShr #[a, b])
      
  (* conversions *)
  
  | P.I32ToI64X _ => (fn [a] => c Op.SExt (a, i64))
  | P.I32ToI64 _ => (fn [a] => c Op.ZExt (a, i64))
  | P.I64ToI32 _ => (fn [a] => c Op.Trunc (a, i32))
  
  | (P.I32ToF32 _ | P.I64ToF32 _) 
      => (fn [a] => c Op.SIToFP (a, float))
  
  | (P.I32ToF64 _ | P.I64ToF64 _) 
      => (fn [a] => c Op.SIToFP (a, double))
      
  | P.F64ToI32 _ => (fn [a] => c Op.FPToSI (a, i32))
  
  | P.I32ToI16 _ => (fn [a] => c Op.Trunc (a, i16))
  
  | P.I16ToI8 _ => (fn [a] => c Op.Trunc (a, i8))
  
  (* NOTE we can't use GEP for these address prims mostly because GEP
     requires the offsets to be constants, whereas
     AdrAdd does not nessecarily do that. we lose out on some
     alias analysis friendliness, but we can worry about that later.
     
     TODO evaluate whether this is true or not. we ought to be able to
     case to i8* , GEP , then cast back? GEP accepts non-const
     offsets if its an array. *)
     
  | P.AdrAddI32 _ => addrArith bb true Op.Add
  | P.AdrSubI32 _ => addrArith bb true Op.Sub
  
  | P.AdrAddI64 _ => addrArith bb false Op.Add
  | P.AdrSubI64 _ => addrArith bb false Op.Sub
  
  | ( P.AdrLoadI8 _
    | P.AdrLoadU8 _
    | P.AdrLoadI16 _
    | P.AdrLoadU16 _
    | P.AdrLoadI32 _
    | P.AdrLoadI64 _
    | P.AdrLoadF32 _
    | P.AdrLoadF64 _
    | P.AdrLoad _  ) => (fn [a] => f e Op.Load #[a])

  | (P.AdrLoadAdr _) => 
        (* we have to bitcast after this kind of load because of the type signature of AdrLoadAdr in prim-ty,
          the expected result is to be a ptr to any, and sometimes you do this load on an any type I believe,
          which would be one star short *)
        (fn [a] => c Op.BitCast (f e Op.Load #[a], LT.mkPtr(LT.uniformTy)))
  
  | ( P.AdrStoreI8 _
    | P.AdrStoreI16 _
    | P.AdrStoreI32 _
    | P.AdrStoreI64 _
    | P.AdrStoreF32 _
    | P.AdrStoreF64 _ ) => (fn [targ, value] => f e Op.Store #[targ, value])

  | (P.AdrStoreAdr _ 
     | P.AdrStore _ ) => (fn [targ, value] => 
      f e Op.Store #[c Op.BitCast (targ, LT.mkPtr(LB.toTy value)), value])
      
      (*  NOTE
          Original CFG
          (1) let _t<113C3>#1:addr(any) = &0 deq<113C4>
          ...
          (2) let _t<113CA>#1:addr(any) = AdrAddI64(_t<113C3>,_t<113C9>)
          (3) do AdrStore(_t<113CA>,_t<113BA>)
          
          
          The way we translate the CFG above is (note, deq's type is i8* in this example)
          (1) %r_127A6 = getelementptr inbounds i8, i8* %DEQ, i32 0
          ...
          (2) %r_127AA = ptrtoint i8* %r_127A6 to i64
          (2) %r_127AB = add i64 %r_127AA, %r_127A9
          (2) %r_127AC = inttoptr i64 %r_127AB to i8*
          
          (3) %r_127AD = bitcast i8* %r_127AC to %_tupTy.57**
          (3) store %_tupTy.57* %_t_cfg113BA_1279D, %_tupTy.57** %r_127AD
          
          Thus, we need to cast the target, which is _some_ address, to be
          the right pointer type in LLVM to perform the store. There may be some
          confusion in the future about the fact that anyTy and addr are the same thing,
          so be careful basically.
      *)
      
        
  

    
  (* array load operations *)
    | P.ArrLoadI32 _ => arrayLoad bb i32
    | P.ArrLoadI64 _ => arrayLoad bb i64
    | P.ArrLoadF32 _ => arrayLoad bb float
    | P.ArrLoadF64 _ => arrayLoad bb double
    
    (* load a uniform value *)
    | P.ArrLoad _ =>  arrayLoad bb LT.uniformTy	


  (* array store operations *)
    | P.ArrStoreI32 _ => arrayStore bb i32
    | P.ArrStoreI64 _ => arrayStore bb i64
    | P.ArrStoreF32 _ => arrayStore bb float
    | P.ArrStoreF64 _ => arrayStore bb double
    
    (* store a uniform value *)
    | P.ArrStore _ => arrayStore bb LT.uniformTy
    
  (* atomic Op.operations *)
    | P.I32FetchAndAdd _ => let
            val desiredTy = i32Star
            fun caster targ = if LT.same(LB.toTy targ, desiredTy)
                              then targ
                              else c Op.BitCast (targ, desiredTy)
        in
            (fn [targ, value] => f e (Op.Armw Op.P_Add) #[caster targ, value])
        end
        
    | P.I64FetchAndAdd _ => let
            val desiredTy = i64Star
            fun caster targ = if LT.same(LB.toTy targ, desiredTy)
                              then targ
                              else c Op.BitCast (targ, desiredTy)
        in
            (fn [targ, value] => f e (Op.Armw Op.P_Add) #[caster targ, value])
        end
        
        
    | (P.CAS _) =>
        (fn [targ, cmp, new] => let
                (* cmpxchg operand must be on integers, thus we need casts *)
                val resTy = LB.toTy cmp
                val llTarg = c Op.BitCast (targ, i64Star)
                val llCmp = c (Op.equivCast(LB.toTy cmp, i64)) (cmp, i64)
                val llNew = c (Op.equivCast(LB.toTy new, i64)) (new, i64)
                
                (* do operation and get the value *)
                val xchg = f e Op.CmpXchg #[llTarg, llCmp, llNew]
                val extr = LB.extractV bb (xchg, #[LB.intC(i32, 0)])
                
                (* restore the type *)
                val res = c (Op.equivCast(LB.toTy extr, resTy)) (extr, resTy)
                
            in
                res
            end
            
        )
        
        
        (* Refer to prim-gen-fn.sml and heap-transfer-fn.sml, and alloc.c in the MLRISC
           backend and runtime system. The AllocPolyVec is nearly the same as the others,
           it's just recopied and modified down here because the types of the runtime function
           and primop don't line up with the others. *)
    | P.AllocPolyVec _ => let
            val (label, NONE) = LR.allocVector
            
            (* in prim-gen-fn.sml the args are (n, xs), but n is unused. *)  
            fun cvtr ([ _, xs ]) = raise ParrayPrim (haveStuff xs)
            
            and haveStuff xs {resTy, vproc, alloc, allocOffset} = let
                    val loc = {vproc=vproc, off=allocOffset}
                    
                    val _ = LPU.saveAllocPtr bb loc alloc
                    
                    (* call C routine to do the allocation, with bitcasts as needed *)
                    val res = asTy bb resTy (
                        LB.call bb (fv label, #[c Op.BitCast (vproc, LT.voidStar),
                                             c (Op.equivCast(LB.toTy xs, LT.voidStar)) (xs, LT.voidStar)]))
                        
                    (* retrieve the modified allocation pointer *)
                    val newAlloc = LPU.restoreAllocPtr bb loc
                    
                in
                    { alloc=newAlloc, result=res }
                end
            in
                cvtr
            end

    | P.AllocIntArray _ => allocXArray bb LR.allocIntArray
    | P.AllocLongArray _ => allocXArray bb LR.allocLongArray
    | P.AllocFloatArray _ => allocXArray bb LR.allocFloatArray
    | P.AllocDoubleArray _ => allocXArray bb LR.allocDoubleArray
        
    | P.TimeStampCounter => (fn _ => LB.call bb (fv (#1(LR.readtsc)), #[]))
    
    | P.Pause => (fn _ => f e Op.Pause #[])
    
    | (P.FenceRead | P.FenceWrite | P.FenceRW ) =>
        (fn _ => f e Op.Fence #[])
    
    (*     
  
    NOTE It looks like we need to mark all loads/stores as seq_cst to play it
    safe for now, in order to have a working fence in LLVM.
    
    http://llvm.org/releases/3.8.0/docs/Atomics.html#atomics-and-ir-optimization
  
    *)
    
    (* | _ => raise TODO ("primop " ^ (PrimUtil.nameOf p) ^ " not implemented") *)
    
    (* esac *))
  end (* end let of fromPrim*)
  
  
  
  
  
  
  
  
  
  
  (* returns a function that when applied to LLVM equivalents of the vars, it will
    return the result of performing the test as an LLVM instruction result, thus the
    type of the LLVM instr is an i1 for use in branch instructions *)
  fun fromCond bb cond = let
    val f = LB.mk bb
    val c = LB.cast bb
    
    (* two argument cmp with a direct equivalent *)
    fun simpleCmp kind opc = (fn [a, b] => f e (kind opc) #[a, b])
    
    fun addrCmpCast a = c (Op.simpleCast (LB.toTy a, i64)) (a, i64)
    
    (* compare two pointers. NOTE it seems integers are being compared with pointers
       sometimes, presumably because Equal(int, any) is allowed, so we use simpleCast. 
       Should not expect any floats though. *)
    fun addrCmp cmp = 
        (fn [a, b] =>
            f e (Op.Icmp cmp) #[addrCmpCast a, addrCmpCast b])
            
    fun boxCmp cmp =
        (* EQ means isBoxed, NE means isUnboxed *)
        (fn [a] => let
                val a = addrCmpCast a
                val zero = LB.fromC(LB.intC(LT.i64, 0))
                val one = LB.fromC(LB.intC(LT.i64, 1))
                
                val masked = f e Op.And #[a, one]
                val res = f e (Op.Icmp(Op.US cmp)) #[masked, zero]
            in
                res
            end)
    
  in (case cond
     
      of (P.I32Eq _ | P.I64Eq _)    => simpleCmp Op.Icmp (Op.S Op.EQ)
       | (P.I32NEq _ | P.I64NEq _)  => simpleCmp Op.Icmp (Op.S Op.NE)
       | (P.I32Lt _ | P.I64Lt _)    => simpleCmp Op.Icmp (Op.S Op.LT)
       | (P.I32Lte _ | P.I64Lte _)  => simpleCmp Op.Icmp (Op.S Op.LE)
       | (P.I32Gt _ | P.I64Gt _)    => simpleCmp Op.Icmp (Op.S Op.GT)
       | (P.I32Gte _ | P.I64Gte _)  => simpleCmp Op.Icmp (Op.S Op.GE)
       
       | (P.U32Lt _ | P.U64Lt _)    => simpleCmp Op.Icmp (Op.US Op.LT) 
       
       
       (* TODO could put fast-math flags on these fcmps, also, are the arguments
          guarenteed to be ordered? there don't seem to be conds which check
          whether its a NaN or not so maybe it could be? for now we play it safe *)
          
       | (P.F32Eq _ | P.F64Eq _)    => simpleCmp Op.Fcmp (Op.UO Op.EQ)
       | (P.F32NEq _ | P.F64NEq _)  => simpleCmp Op.Fcmp (Op.UO Op.NE)
       | (P.F32Lt _ | P.F64Lt _)    => simpleCmp Op.Fcmp (Op.UO Op.LT)
       | (P.F32Lte _ | P.F64Lte _)  => simpleCmp Op.Fcmp (Op.UO Op.LE)
       | (P.F32Gt _ | P.F64Gt _)    => simpleCmp Op.Fcmp (Op.UO Op.GT)
       | (P.F32Gte _ | P.F64Gte _)  => simpleCmp Op.Fcmp (Op.UO Op.GE)
       
       (* shouldn't matter for equality questions, but we chose unsigned since im
          pretty sure these are never considered negative values anyways. *)
       | (P.Equal _ | P.AdrEq _)        => addrCmp (Op.US Op.EQ)
       | (P.NotEqual _ | P.AdrNEq _)    => addrCmp (Op.US Op.NE)
       | (P.EnumEq _)     => simpleCmp Op.Icmp (Op.US Op.EQ)
       | (P.EnumNEq _)    => simpleCmp Op.Icmp (Op.US Op.NE)
       
       | (P.BCAS _) =>
           (fn [targ, cmp, new] => let
                   (* cmpxchg operand must be on integers, thus we need casts *)
                   val llTarg = c Op.BitCast (targ, i64Star)
                   val llCmp = c (Op.equivCast(LB.toTy cmp, i64)) (cmp, i64)
                   val llNew = c (Op.equivCast(LB.toTy new, i64)) (new, i64)
                   
                   (* do operation and get the value *)
                   val xchg = f e Op.CmpXchg #[llTarg, llCmp, llNew]
                   val extr = LB.extractV bb (xchg, #[LB.intC(i32, 1)])
                   
               in
                   extr
               end
               
           )
           
           
       | P.I32TAS _ =>
            (fn [targ] => let
                    val targ = c Op.BitCast (targ, i32Star)
                    val lit = LB.fromC (LB.intC(LT.i32, 1))
                    val xchg = f e (Op.Armw Op.P_Xchg) #[targ, lit]
                    
                    (* NOTE if we have a guarentee that the i32 values are only
                       ever 0 or 1, we could optimize this to become just
                       a bitcast instead. This will probably be fast anyways
                       since we're comparing with 1. *)
                       
                    val cmp = f e (Op.Icmp (Op.S Op.EQ)) #[xchg, lit]
                in
                    cmp
                end)
                
                
       | P.I32isSet _ =>
            (fn [targ] => let
                    val targ = c Op.BitCast (targ, i32Star)
                    val lit = LB.fromC (LB.intC(LT.i32, 1))
                    
                    (* NOTE this load is marked volatile because its a TAS
                           candidate location that's shared among threads *)
                    val volatile = AS.singleton A.Volatile
                    val load = f volatile Op.Load #[targ]
                    val cmp = f e (Op.Icmp (Op.S Op.EQ)) #[load, lit]
                in
                    cmp
                end)
                
       | P.isBoxed _ => boxCmp Op.EQ
       | P.isUnboxed _ => boxCmp Op.NE
      
      (* esac *))
      
  end (* end let of fromCond *)
    
end (* end local *)


end (* end struct *)
