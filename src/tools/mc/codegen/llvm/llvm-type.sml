(* llvm-type.sml
 *
 * COPYRIGHT (c) 2015 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Utility for translating to and managing types in LLVM
 *)

functor LLVMType (structure Spec : TARGET_SPEC) : sig

    type ty

    (*  
      converts a CFG type to an LLVM type. Uses a simple
      caching system so that structurally equivalent aggregate
      types share the same LLVM type declaration. 

      TODO(kavon): The only point of doing the caching is to save characters, is this important right now?
    *)
    val typeOf : CFGTy.ty -> ty

    val typeOfC : CFunctions.c_type -> ty

    (* only valid for function/continuation types, excluding basic blocks as well.
       breaks apart the types involved in this convention into its list of parameters
       suitable for printing.  *)
    val typesInConv : CFGTy.ty -> ty list

    val typeOfConv : (CFG.convention * CFG.var list) -> CFGTy.ty


    (*
      get the name of an LLVM type
    *)
    val toString : ty -> string

    (* 
      generate the type declaration block for LLVM IR output for
      all of the LLVM types generated. 
    *)
    (* val typeDecl : unit -> string *)
    
  end = struct

  structure S = String
  structure C = CFG
  structure CT = CFGTy
  structure CTU = CFGTyUtil
  structure CF = CFunctions

  structure HC = HashCons

  structure HCInt = HashConsGroundFn (
  struct
    type hash_key = int
    val sameKey = (op = : int * int -> bool)
    val hashVal = Word.fromInt
  end)

  type count = HCInt.obj
  datatype llvm_ty_node = 
      T_Void
    | T_VProc
    | T_Deque
    | T_Label
    | T_Func of llvm_ty list  (* first element is the return type *)
    | T_Int of count (* number of bits *)
    | T_Float
    | T_Double
    | T_Ptr of llvm_ty
    | T_Vector of count * llvm_ty
    | T_Array of count * llvm_ty
    | T_Struct of llvm_ty list

    withtype llvm_ty = llvm_ty_node HC.obj



  local
    fun eq x = raise Fail "implement me"
    val tbl = HC.new {eq = eq}
  in
    val mkVoid = HC.cons0 tbl (0wx2, T_Void)
    val mkVProc = HC.cons0 tbl (0wx3, T_VProc)
    val mkLabel = HC.cons0 tbl (0wx5, T_Label)
    val mkFunc = HC.consList tbl (0wx7, T_Func)
    val mkInt = HC.cons1 tbl (0wx11, T_Int)
    val mkFloat = HC.cons0 tbl (0wx13, T_Float)
    val mkDouble = HC.cons0 tbl (0wx17, T_Double)
    val mkPtr = HC.cons1 tbl (0wx19, T_Ptr)
    val mkVector = HC.cons2 tbl (0wx23, T_Vector)
    val mkArray = HC.cons2 tbl (0wx29, T_Array)
    val mkStruct = HC.consList tbl (0wx31, T_Struct)
    val mkDeque = HC.cons0 tbl (0wx41, T_Deque)

    val mkCount = HCInt.mk
  end

  type ty = llvm_ty
  

  val i2s = Int.toString

  val wordSzB = IntInf.toInt Spec.ABI.wordSzB

  (* FIXME(kavon): this isn't target independent! we need an archSizes functor instead
                   of this rather useless Spec thing. *)
  fun sizeOfRawTy rt = (case rt
    of CT.T_Byte => 8
     | CT.T_Short => 16
     | CT.T_Int => 32
     | CT.T_Long => 64
     | CT.T_Float => 32
     | CT.T_Double => 64
     | CT.T_Vec128 => 128
  (* escac *))

  fun mapSep(f, init, sep, lst) = List.foldr 
                      (fn (x, nil) => f(x) :: nil 
                        | (x, y) => let val fx = f(x) in
                          if fx = "" (* skip empty strings *)
                          then y
                          else fx :: sep :: y
                        end)
                      init
                      lst


  fun toString (t : llvm_ty) = let
    fun nodeToStr (nt : llvm_ty_node) : string =
      let
        val i2s = i2s o HC.node
      in
       (case nt 
           of T_Void => "void"
            | T_Int width => "i" ^ (i2s width)
            | T_Float => "float"
            | T_Double => "double"
            | T_Label => "label"
            | T_Deque => "%_deque.ty"
            | T_VProc => "%_vproc.ty"
            | T_Ptr t => (toString t) ^ "*"
            
            | T_Func (ret::params) => let
                val llvmParams = mapSep(toString, nil, ", ", params)
              in
                S.concat ([toString ret, " ("] @ llvmParams @ [")"])
              end      
            
            | T_Vector (nelms, t) => S.concat ["<", i2s nelms, " x ", toString t, ">"]

            | T_Array (nelms, t) => S.concat ["[", i2s nelms, " x ", toString t, "]"]

            | T_Struct ts => S.concat (["{ "] @ mapSep(toString, nil, ", ", ts) @ [" }"])

          (* end case *))
       end
    in
      (nodeToStr o HC.node) t
    end


  fun typeOf (cty : CT.ty) : llvm_ty = (case cty

    of CT.T_Any => mkPtr(mkInt(mkCount 8))

     (* in a mixed type representation, the GC expects wordsize width elements.

        IDEA(kavon): do some analysis and determine when we should zero extend
          the enum and or when to truncate it (and how much we can chop off).

      *)
     | CT.T_Enum _ => mkInt(mkCount (8 * wordSzB)) (* NOTE: these are tagged integers, be careful when you create these *)

     | CT.T_Block _ => mkLabel

     | CT.T_Raw (CT.T_Double) => mkDouble

     | CT.T_Raw (CT.T_Float) => mkFloat

     (* FIXME(kavon): it's expected that this gets casted as needed since we don't actually know what
        the underlying values in the vector are, so this might be broken/hacky *)
     | CT.T_Raw (CT.T_Vec128) => mkVector(mkCount 16, mkInt(mkCount 8))

     | CT.T_Raw rt => mkInt(mkCount (sizeOfRawTy rt))

      (* TODO(kavon): 
                      - when it comes to GC header tags, be careful of the dead store elim pass
                        - additionally, we need to mark structs for their header tag for initialization later.
                          thus, we need to determine whether it needs a RAW, VECTOR, or MIXED header and what
                          bits need to be set based upon the positions of the elements.
                      - should we check to see if all types are the same, and if so turn
                        this into an array? 
                      - NOTE: tuples are pointers to structs allocated in the heap, thus
                              for an alloc expression, we should strip the pointer
                              wrapping the type away and then get the size of that. note that we
                              can't flatten a tuple of a single element because it must be in the heap
                              and everything in the heap needs a tag, which at the time of allocation must be known *)
      
      (* always a pointer type. *)
      | CT.T_Tuple (_, ts) => (case ts
        of nil => raise Fail "empty tuple. should be an enum for unit?"

         | t::nil => mkPtr(typeOf t)

         | ts => mkPtr(mkStruct(List.map typeOf ts))

        (* esac *))


      (* TODO(kavon): we represent the vararg part as the any type. might want to change it to an arbitrary C pointer? *)
      | CT.T_OpenTuple ts => mkPtr(mkStruct((List.map typeOf ts) @ [ typeOf(CT.T_Any) ]))

      | CT.T_Addr t => mkPtr(typeOf t)

      (* TODO(kavon): store these in the alias cache *)
      | CT.T_VProc => mkVProc

      | CT.T_Deque => mkDeque

      | CT.T_CFun(CF.CProto(retTy, argTys, _)) => mkFunc([typeOfC retTy] @ (List.map typeOfC argTys))

      (* TODO(kavon): we don't know what our calling convention is right now, so leaving this blank. *)
      | CT.T_StdFun _ => mkPtr(mkFunc( [mkVoid] @ typesInConv(cty) ))

      | CT.T_StdCont _ => mkPtr(mkFunc( [mkVoid] @ typesInConv(cty) ))

      | CT.T_KnownFunc _ => mkPtr(mkFunc( [mkVoid] @ typesInConv(cty) ))

    (* end case *))

    and typeOfC (ct : CF.c_type) : llvm_ty = (case ct
          of CF.PointerTy => mkPtr(mkInt(mkCount 8))  (* LLVM's void* *)
           | CF.BaseTy(rawTy) => typeOf(CT.T_Raw rawTy)
           | CF.VoidTy => mkVoid
          (* end case *))


    (* TODO(kavon): does not yet include types for pinned registers *)
    and typesInConv (cty : CT.ty) : llvm_ty list = (case cty
      
      of CT.T_StdFun { clos, args, ret, exh } =>
            (typeOf clos) :: (List.map typeOf args) @ [typeOf ret, typeOf exh]
       
       | CT.T_StdCont {clos, args} =>
            (typeOf clos) :: (List.map typeOf args)
       
       | CT.T_KnownFunc {clos, args} => 
            (typeOf clos) :: (List.map typeOf args)

       | _ => raise Fail "only functions/continuations have calling convention types")


    local 
      val getTy = C.Var.typeOf
    in
      fun typeOfConv (conv : C.convention, args : C.var list) : CT.ty = (case conv
        of C.StdFunc { clos, ret, exh } => 
            CT.T_StdFun {clos=(getTy clos), args=(List.map getTy args),
                         ret=(getTy ret), exh=(getTy exh)} 

         | C.StdCont { clos } =>
            CT.T_StdCont {clos=(getTy clos), args=(List.map getTy args)} 

         | C.KnownFunc { clos } => 
            CT.T_KnownFunc {clos=(getTy clos), args=(List.map getTy args)} 
        (* end case *))
    end


(*********)

  (* TODO(kavon): turns out that without caching these types, the output becomes
                  quite unreadable. Especially since we need to add to
                  every tuple a GC tag field. So, we should cache all boxed
                  types (tuples, open tuples) and manticore function types  *)
(*
  local
    val cache = Map.empty ref
    fun mkTy (name, llt) = (name, llt) (* in case i want to extend it later *)
  in
  

    fun typeOf (cty : CT.ty) : ty = let
      (* TODO(kavon): some of these strings are rather long, so
                      maybe this is very inefficient? real hash consing
                      might be better.  *)
      val key = CTU.toString cty
    in
      (case Map.find(!cache, key)
        of SOME cachedTy => cachedTy
         | NONE => let
           val genned = typeOf cty
           val shortName = mkShortName genned
           val newTy = mkTy(genned, shortName)
           val _ = cache := Map.insert(!cache, key, newTy)
         in
           newTy
         end
      (* esac *))
    end

  and mkShortName (t : llvm_ty) =
      
  

  end
*)
     

end
