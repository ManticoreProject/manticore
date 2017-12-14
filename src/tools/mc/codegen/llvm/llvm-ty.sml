(* llvm-ty.sml
 *
 * COPYRIGHT (c) 2015 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Provides a single definition of an LLVM type used in various places.
 * Use LLVMTy.t when talking about an LLVM type.
 *)

structure LLVMTy = struct

  structure HC = HashCons
  structure HCInt = HashConsGroundFn (
  struct
    type hash_key = int
    val sameKey = (op = : int * int -> bool)

    (* QUESTION(kavon): is this hashVal okay or do we need to hash the integer with
                 some sort of prime number multiplication? *)
    val hashVal = Word.fromInt 

  end)

  (* must use the ctor functions defined below *)
  type count = HCInt.obj
  datatype t_node = 
      T_Void
    | T_Label
    | T_Token
    | T_Func of t list  (* first element is the return type *)
    | T_VFunc of t list (* same as T_Func, but this function has varargs. 
                           mostly needed for c functinos *)
    | T_Int of count (* number of bits *)
    | T_Float
    | T_Double
    | T_Ptr of count * t        (* count is the address space number *)
    | T_Vector of count * t
    | T_Array of count * t
    | T_Struct of t list  (* packed struct *)
    | T_UStruct of t list  (* unpacked struct *)
    withtype t = t_node HC.obj  
    
    
  local
    (* ctors for ty *)
    fun eq query = (case query
      of (T_Void, T_Void) => true
       | (T_Label, T_Label) => true
       | (T_Token, T_Token) => true
       | (T_Func xs, T_Func ys) => ListPair.allEq HC.same (xs, ys)
       | (T_VFunc xs, T_VFunc ys) => ListPair.allEq HC.same (xs, ys)
       | (T_Int x, T_Int y) => HC.same(x, y)
       | (T_Float, T_Float) => true
       | (T_Double, T_Double) => true
       | (T_Ptr (xspace, x), T_Ptr (yspace, y)) => HC.same(xspace, yspace) andalso HC.same(x, y)
       | (T_Vector (xcount, x), T_Vector (ycount, y)) => HC.same(xcount, ycount) andalso HC.same(x, y)
       | (T_Array (xcount, x), T_Array (ycount, y)) => HC.same(xcount, ycount) andalso HC.same(x, y)
       | (T_Struct xs, T_Struct ys) => ListPair.allEq HC.same (xs, ys)
       | (T_UStruct xs, T_UStruct ys) => ListPair.allEq HC.same (xs, ys)
       | _ => false
      (* esac *))
    
    val tbl = HC.new {eq = eq}

  in
    (* should be prime numbers.
       I skip 2 because I think the hash function uses it to combine these? *)
    val voidTy = HC.cons0 tbl (0w3, T_Void)
    val labelTy = HC.cons0 tbl (0w7, T_Label)
    val mkFunc = HC.consList tbl (0w11, T_Func)
    val mkInt = HC.cons1 tbl (0w13, T_Int)
    val floatTy = HC.cons0 tbl (0w17, T_Float)
    val doubleTy = HC.cons0 tbl (0w19, T_Double)
    fun mkPtr ty = (HC.cons2 tbl (0w23, T_Ptr)) (HCInt.mk 0, ty)
    fun mkGCPtr ty = (HC.cons2 tbl (0w23, T_Ptr)) (HCInt.mk 1, ty)
    val mkVector = HC.cons2 tbl (0w29, T_Vector)
    val mkArray = HC.cons2 tbl (0w31, T_Array)
    val mkStruct = HC.consList tbl (0w37, T_Struct)
    val mkUStruct = HC.consList tbl (0w43, T_UStruct)
    val mkVFunc = HC.consList tbl (0w47, T_VFunc)
    val tokenTy = HC.cons0 tbl (0w53, T_Token)
    
    (* more primes  59     61     67     71 *)

    val cnt = HCInt.mk
    val tnc = HC.node
  end
  
  
  (* useful pre-defined types *)
  
  val uniformTy = mkPtr(mkInt(cnt 64)) 
      (* the "any" type. we want an i64* instead of an i8 because SELECT(2, any) will
         generate a GEP, and we want it to calculate offsets of 8 bytes at a time. *)
         
  val allocPtrTy = mkPtr(uniformTy)
  val vprocTy = mkPtr(uniformTy)
  val boolTy = mkInt(cnt 1)
  val i64 = mkInt(cnt 64)
  val i32 = mkInt(cnt 32)
  val i16 = mkInt(cnt 16)
  val i8  = mkInt(cnt 8)
  val i1  = mkInt(cnt 1)
  val enumTy = i64
  val gcHeaderTy = i64
  val voidStar = mkPtr(i8)
  val dequeTy = mkPtr(i64)
  
end
