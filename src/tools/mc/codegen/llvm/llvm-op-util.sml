structure LLVMOpUtil = struct

(* because cyclic dependency between LLVMOp & LLVMBuilder *)

  structure LB = LLVMBuilder
  structure P = Prim
  structure AS = LLVMAttribute.Set
  structure LT = LLVMType
  structure A = LLVMAttribute
  
  structure V = Vector
  structure L = List
  structure S = String


local

    val e = AS.empty
    
    (* generate a integer constant with the same type
    as the given integer instruction type *)
    fun const f instr c = f(LB.toTy instr, c)
    val Iconst = const LB.intC
    val Fconst = const LB.floatC
    
    fun intTy sz = LT.mkInt(LT.cnt sz)
    val i64 = intTy 64
    val i32 = intTy 32
    val i16 = intTy 16
    val i8  = intTy 8
    val float = LT.floatTy
    val double = LT.doubleTy
    
    
    (* TODO llvm frontend guide suggests using fastmath and NSW etc *)

in

(* b is the basic block, p is the 'var prim, returns
  (LB.instr list -> LV.instr) that, when applied
  to a list of arguments for this llvmPrim, adds the right
  instructions to the given block and returns the final result
  of the operation *)
fun fromPrim b p = let
  val f = LB.mk b
  val c = LB.cast b
in (case p
  of (I32Add _ | I64Add _)
      => (fn [a, b] => f e Add #[a, b])
      
  | (I32Sub _ | I64Sub _)
      => (fn [a, b] => f e Sub #[a, b])
          
  (* NOTE Mul in LLVM works on both signed or
    unsigned integers, but in CFG types its
    not clear whether we manage signedness properly.
    same goes for constants. we might need to
    do a conversion or something *)
  | (I32Mul _ | I64Mul _ | U64Mul _) 
      => (fn [a, b] => f e Mul #[a, b])
      
  | (I32Div _ | I64Div _)
      => (fn [a, b] => f e SDiv #[a, b])
      
  | (I32Mod _ | I64Mod _)
      => (fn [a, b] => f e SRem #[a, b])
  
  | (I32LSh _ | I64LSh _)
      => (fn [a, b] => f e Shl #[a, b])
  
  | (I32Neg _ | I64Neg _)
      => (fn [a] => f e Sub #[Iconst a 0, a])
      
      
  | U64Div _ => (fn [a, b] => f e UDiv #[a, b])
  
  | U64Rem _ => (fn [a, b] => f e URem #[a, b])
  
  
  | (F32Add _ | F64Add _)
      => (fn [a, b] => f e FAdd #[a, b])
      
  | (F32Sub _ | F64Sub _)
      => (fn [a, b] => f e FSub #[a, b])
      
  | (F32Mul _ | F64Mul _)
      => (fn [a, b] => f e FMul #[a, b])
      
  | (F32Div _ | F64Div _)
      => (fn [a, b] => f e FDiv #[a, b])
      
  | (F32Neg _ | F64Neg _)
      => (fn [a] => f e FSub #[Fconst a 0.0, a])
  
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
  | (F32Sqrt _ | F64Sqrt _ | F32Abs _ | F64Abs _ )
      => raise Fail "See the todo here."


  | (I8RSh _ | I16RSh _ | I32RSh _ | I64RSh _ )
      => (fn [a, b] => f e AShr #[a, b])
      
  (* conversions *)
  
  | I32ToI64X _ => (fn [a] => c SExt (a, i64))
  | I32ToI64 _ => (fn [a] => c ZExt (a, i64))
  | I64ToI32 _ => (fn [a] => c Trunc (a, i32))
  
  | (I32ToF32 _ | I64ToF32 _) 
      => (fn [a] => c SIToFP (a, float))
  
  | (I32ToF64 _ | I64ToF64 _) 
      => (fn [a] => c SIToFP (a, double))
      
  | F64ToI32 => (fn [a] => c FPToSI (a, i32))
  
  | I32ToI16 => (fn [a] => c Trunc (a, i16))
  
  | I16ToI8 => (fn [a] => c Trunc (a, i8))
  
    (*
  (* address arithmetic *)
  
    | AdrAddI32 _ =>
    | AdrAddI64 _ =>
    | AdrSubI32 _ =>
    | AdrSubI64 _ =>
  (* loads from addresses *)
  
    | AdrLoadI8 _ =>
    | AdrLoadU8 _ =>
    | AdrLoadI16 _ =>
    | AdrLoadU16 _ =>
    | AdrLoadI32 _ =>
    | AdrLoadI64 _ =>
    | AdrLoadF32 _ =>
    | AdrLoadF64 _ =>
    | AdrLoadAdr _ =>
    | AdrLoad _ =>                   (* load a uniform value from the given address *)
  (* stores to addresses *)
    | AdrStoreI8 _ =>
    | AdrStoreI16 _ =>
    | AdrStoreI32 _ =>
    | AdrStoreI64 _ =>
    | AdrStoreF32 _ =>
    | AdrStoreF64 _ =>
    | AdrStoreAdr _ =>
    | AdrStore _ =>           (* store a uniform value at the given address *)
  (* array load operations *)
    | ArrLoadI32 _ =>
    | ArrLoadI64 _ =>
    | ArrLoadF32 _ =>
    | ArrLoadF64 _ =>
    | ArrLoad _ =>	(* load a uniform value *)
  (* array store operations *)
    | ArrStoreI32 _ => * 'var
    | ArrStoreI64 _ => * 'var
    | ArrStoreF32 _ => * 'var
    | ArrStoreF64 _ => * 'var
    | ArrStore _ => * 'var (* store a uniform value *)*)
    
  (* atomic operations *)
    (*| I32FetchAndAdd _ =>
    | I64FetchAndAdd _ =>
    | CAS _ => * 'var	(* compare and swap; returns old value *)
  (* memory-system operations *)
    | Pause				(* yield processor to allow memory operations to be seen *)
    | FenceRead			(* memory fence for reads *)
    | FenceWrite			(* memory fence for writes *)
    | FenceRW				(* memory fence for both reads and writes *)
  (* allocation primitives *)
    | AllocPolyVec _ =>     (* AllocPolyVec (n, xs): allocate in the local heap a vector 
                   * v of length n s.t. v[i] := l[i] for 0 <= i < n *)
    | AllocIntArray _ =>           (* allocates an array of ints in the local heap *)
    | AllocLongArray _ =>          (* allocates an array of longs in the local heap *)
    | AllocFloatArray _ =>         (* allocates an array of floats in the local heap *)
    | AllocDoubleArray _ =>        (* allocates an array of doubles in the local heap *)
  (* time-stamp counter *)
    | TimeStampCounter =>               (* returns the number of processor ticks counted by the TSC register *)
    *)
    
    | _ => raise Fail ("primop" ^ (PrimUtil.nameOf p) ^ " not implemented")
    
    (* esac *))
  end (* end let *)
    
end (* end local *)


end (* end struct *)
