structure LLVMOpUtil = struct

(* because cyclic Op.dependency between LLVMOp & LLVMBuilder *)

  exception TODO of string

  structure LB = LLVMBuilder
  structure P = Prim
  structure AS = LLVMAttribute.Set
  structure LT = LLVMType
  structure A = LLVMAttribute
  structure Op = LLVMOp
  
  structure V = Vector
  structure L = List
  structure S = String


local

    val e = AS.empty
    
    (* generate a integer constant with the same type
    as the given integer instruction type *)
    fun const f instr c = LB.fromC(f (LB.toTy instr, c))
    val Iconst = const LB.intC
    val Fconst = const LB.floatC
    
    fun intTy sz = LT.mkInt(LT.cnt sz)
    val i64 = intTy 64
    val i32 = intTy 32
    val i16 = intTy 16
    val i8  = intTy 8
    val float = LT.floatTy
    val double = LT.doubleTy
    
    
    
    fun id x = x
    
    fun addrArith bb sext opc = let
        val cast = LB.cast bb
        val mk = LB.mk bb e
        
        fun sextI64 i = cast Op.SExt (i, i64)
        fun toI64 a = cast Op.PtrToInt (a, i64)
        fun toPtr i ty = cast Op.IntToPtr (i, ty)    
        
        (* LLVM rejects sext i64 to i64, rhs must be a smaller width *)
        val doSext = if sext then sextI64 else id    
    in
        (fn [adr, off] =>
            toPtr (mk opc #[toI64 adr, doSext off]) (LB.toTy adr))
    end
    
    (* TODO llvm frontend guide suggests using fastmath and NSW etc *)

in

(* b is the basic Op.block, p is the 'var prim, returns
  (LB.instr list -> LV.instr) that, when applied
  to a list of arguments for this llvmPrim, adds the right
  instructions to the given block and returns the final result
  of the operation *)
fun fromPrim bb p = let
  val f = LB.mk bb
  val c = LB.cast bb
in (case p
  of (P.I32Add _ | P.I64Add _)
      => (fn [a, b] => f e Op.Add #[a, b])
      
  | (P.I32Sub _ | P.I64Sub _)
      => (fn [a, b] => f e Op.Sub #[a, b])
          
  (* NOTE Mul in LLVM works on both signed or
    unsigned integers, but in CFG types its
    not clear whether we manage signedness properly.
    same goes for constants. we might need to
    do a conversion or something *)
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
      => (fn [a] => f e Op.FSub #[Fconst a 0.0, a])
  
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
  | (P.F32Sqrt _ | P.F64Sqrt _ | P.F32Abs _ | P.F64Abs _ )
      => raise Fail "See the todo here."


  | (P.I8RSh _ | P.I16RSh _ | P.I32RSh _ | P.I64RSh _ )
      => (fn [a, b] => f e Op.AShr #[a, b])
      
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
     alias analysis friendliness, but we can worry about that later *)
     
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
    | P.AdrLoadAdr _
    | P.AdrLoad _) => (fn [a] => f e Op.Load #[a])
  
  | ( P.AdrStoreI8 _
    | P.AdrStoreI16 _
    | P.AdrStoreI32 _
    | P.AdrStoreI64 _
    | P.AdrStoreF32 _
    | P.AdrStoreF64 _
    | P.AdrStoreAdr _  
    | P.AdrStore _) => (fn [targ, value] => let
            (* we bitcast because CFG types dont differentiate between
               different address types, but in LLVM you cannot
               store to a different pointer type. sometimes we're bitcasting
               a type to itself, but bitcasts are noops anyways. *)
            val derefTy = (LT.deref o LB.toTy) targ
        in
            f e Op.Store #[targ, c Op.BitCast (value, derefTy)]
        end)
        
  
    (*
    
  (* loads from addresses *)
  
     =>                   (* load a uniform value from the given address *)
  (* stores to addresses *)
     =>           (* store a uniform value at the given address *)
    
  (* array load operations *)
    | P.ArrLoadI32 _ =>
    | P.ArrLoadI64 _ =>
    | P.ArrLoadF32 _ =>
    | P.ArrLoadF64 _ =>
    | P.ArrLoad _ =>	(* load a uniform value *)
  (* array store operations *)
    | P.ArrStoreI32 _ => * 'var
    | P.ArrStoreI64 _ => * 'var
    | P.ArrStoreF32 _ => * 'var
    | P.ArrStoreF64 _ => * 'var
    | P.ArrStore _ => * 'var (* store a uniform value *)*)
    
  (* atomic Op.operations *)
    (*| P.I32FetchAndAdd _ =>
    | P.I64FetchAndAdd _ =>
    | P.CAS _ => * 'var	(* compare and swap; returns old value *)
  (* memory-system operations *)
    | Pause				(* yield processor to allow memory operations to be seen *)
    | FenceRead			(* memory fence for reads *)
    | FenceWrite			(* memory fence for writes *)
    | FenceRW				(* memory fence for both reads and writes *)
  (* allocation primitives *)
    | P.AllocPolyVec _ =>     (* AllocPolyVec (n, xs): allocate in the local heap a vector 
                   * v of length n s.t. v[i] := l[i] for 0 <= i < n *)
    | P.AllocIntArray _ =>           (* allocates an array of ints in the local heap *)
    | P.AllocLongArray _ =>          (* allocates an array of longs in the local heap *)
    | P.AllocFloatArray _ =>         (* allocates an array of floats in the local heap *)
    | P.AllocDoubleArray _ =>        (* allocates an array of doubles in the local heap *)
  (* time-stamp counter *)
    | TimeStampCounter =>               (* returns the number of processor ticks counted by the TSc Op.register *)
    *)
    
    | _ => raise TODO ("primop " ^ (PrimUtil.nameOf p) ^ " not implemented")
    
    (* esac *))
  end (* end let *)
    
end (* end local *)


end (* end struct *)
