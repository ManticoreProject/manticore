(* llvm-type.sml
 *
 * COPYRIGHT (c) 2015 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Utility for translating to and managing types in LLVM
 *)

functor LLVMType (structure Spec : TARGET_SPEC) : sig

    type ty
    type ty_node
    type count

    (*  
      converts a CFG type to an LLVM type.
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
    val nameOf : ty -> string

    (*
      O(1) operation to compare two types for equality.
      
      VProc and Deque are considered to be distinct types, even if
      they alias the same type.
    *)
    val same : ty * ty -> bool

    (* select i'th type component from a structure *)
    val select : ty * int -> ty

    (* turn a Ptr(ty) -> ty *)
    val deref : ty -> ty

    (* returns the resulting type of a getelementptr operation
       performed on the provided type. *)
    val gepType : (ty * int vector) -> ty


    (* QUESTION(kavon): need to think more about sizes with respect to the GC. for example,
   an integer type with < wordSizeB bytes as part of a vector of non-pointers does not
   nessecarily need to be sign extended to wordSizeB size. so the sizes of things
   depends on what container the value appears in on the heap, since mixed type
   should have everything wordSizeB to calculate offsets.   *)
  (* QUESTION/TODO(kavon): 
      - when it comes to GC header tags, be careful of the dead store elim pass
        - additionally, we need to mark structs for their header tag for initialization later.
          thus, we need to determine whether it needs a RAW, VECTOR, or MIXED header and what
          bits need to be set based upon the positions of the elements.
      - should we check to see if all types are the same, and if so turn
        a struct into an array?
      - NOTE: tuples are pointers to structs allocated in the heap, thus
              for an alloc expression, we should strip the pointer
              wrapping the type away and then get the size of that. note that we
              can't flatten a tuple of a single element because it must be in the heap
              and everything in the heap needs a tag, which at the time of allocation must be known *)

    (* val sizeOf : ty * ty -> int *)

    (* project the node out of a ty. 
       useful for clean pattern matching *)
    val node : ty -> ty_node

    (* the types *)

    val voidTy : ty
    val vprocTy : ty
    val dequeTy : ty
    val labelTy : ty
    val floatTy : ty
    val doubleTy : ty

    (* count is number of bits wide *)
    val mkInt : count -> ty

    (* first element is return type *)
    val mkFunc : ty list -> ty

    val mkPtr : ty -> ty

    (* count is number of elements of type ty *)
    val mkVector : count * ty -> ty
    val mkArray : count * ty -> ty

    val mkStruct : ty list -> ty
    
    val cnt : int -> count

    (* 
      generate the type declaration block for LLVM IR output for
      all of the LLVM types cached. The types become cached
      as nameOf is used to determine the name of the type.
    *)
    val typeDecl : unit -> string
    
  end = struct

  structure S = String
  structure C = CFG
  structure CT = CFGTy
  structure CTU = CFGTyUtil
  structure CF = CFunctions
  structure V = Vector

  structure HC = HashCons
  structure HCM = HashConsMap
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

  type ty = llvm_ty
  type ty_node = llvm_ty_node

  local
    (* ctors for llvm_ty *)
    fun eq query = (case query
      of (T_Void, T_Void) => true
       | (T_VProc, T_VProc) => true
       | (T_Deque, T_Deque) => true
       | (T_Label, T_Label) => true
       | (T_Func xs, T_Func ys) => ListPair.allEq HC.same (xs, ys)
       | (T_Int x, T_Int y) => HC.same(x, y)
       | (T_Float, T_Float) => true
       | (T_Double, T_Double) => true
       | (T_Ptr x, T_Ptr y) => HC.same(x, y)
       | (T_Vector (xcount, x), T_Vector (ycount, y)) => HC.same(xcount, ycount) andalso HC.same(x, y)
       | (T_Array (xcount, x), T_Array (ycount, y)) => HC.same(xcount, ycount) andalso HC.same(x, y)
       | (T_Struct xs, T_Struct ys) => ListPair.allEq HC.same (xs, ys)
       | _ => false
      (* esac *))
    
    val tbl = HC.new {eq = eq}

  in
    (* should be prime numbers.
       I skip 2 because I think the hash function uses it to combine these? *)
    val voidTy = HC.cons0 tbl (0w3, T_Void)
    val vprocTy = HC.cons0 tbl (0w5, T_VProc)
    val labelTy = HC.cons0 tbl (0w7, T_Label)
    val mkFunc = HC.consList tbl (0w11, T_Func)
    val mkInt = HC.cons1 tbl (0w13, T_Int)
    val floatTy = HC.cons0 tbl (0w17, T_Float)
    val doubleTy = HC.cons0 tbl (0wx19, T_Double)
    val mkPtr = HC.cons1 tbl (0w23, T_Ptr)
    val mkVector = HC.cons2 tbl (0w29, T_Vector)
    val mkArray = HC.cons2 tbl (0w31, T_Array)
    val mkStruct = HC.consList tbl (0w37, T_Struct)
    val dequeTy = HC.cons0 tbl (0w41, T_Deque)

    val cnt = HCInt.mk
  end
  
  

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


  local
    val cache = ref HCM.empty
    val stamp = ref 0

    val vprocTyName = "%_vproc.ty"
    val vprocTyDef = "i8*"

    val dequeTyName = "%_deque.ty"
    val dequeTyDef = "i8*"

  in  

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
              | T_Ptr t => (nameOf t) ^ "*"
              | T_VProc => vprocTyName
              | T_Deque => dequeTyName
              | T_Func (ret::params) => let
                  val llvmParams = mapSep(nameOf, nil, ", ", params)
                in
                  S.concat ([nameOf ret, " ("] @ llvmParams @ [")"])
                end      
              
              | T_Vector (nelms, t) => S.concat ["<", i2s nelms, " x ", nameOf t, ">"]

              | T_Array (nelms, t) => S.concat ["[", i2s nelms, " x ", nameOf t, "]"]

              | T_Struct ts => S.concat (["{ "] @ mapSep(nameOf, nil, ", ", ts) @ [" }"])

              | _ => raise Fail "base type name unknown"

            (* end case *))
         end
      in
        (nodeToStr o HC.node) t
      end
                              (* `name` = type `rhs` *)
    and mkAlias (t : llvm_ty) : (string * string option) = let
        fun freshStamp () = (stamp := !stamp + 1 ; !stamp)
      in
        (case HC.node t
          of T_Struct _ => ("%_tupTy." ^ i2s(freshStamp()) , SOME(toString t))
           
           (* NOTE(kavon): turns out you cannot forward reference non-struct types in LLVM.
                           if we figure out a way to do it at some point, you can uncomment
                           and change the following case. *)

           (* | T_Func _ => ("%_funTy." ^ i2s(freshStamp()) , SOME(toString t)) *)

           | _ => (toString t, NONE)
        (* esac *))
      end

    (* looks up this type in the cache. if it is
       not already present, it will generate a new entry and return its name. *)
    and nameOf x = (case HCM.find(!cache, x)
      of SOME (name, _) => name
       | NONE => (case mkAlias x
          of (name, NONE) => name
           | (name, SOME rhs) => 
              ( cache := HCM.insert(!cache, x, (name, rhs)) ; name )
          (* esac *))
       (* esac *))
    

    fun typeDecl () = let
      fun assignToString (name, def) = name ^ " = type " ^ def ^ "\n"
      
      val decls = [(dequeTyName, dequeTyDef), (vprocTyName, vprocTyDef)] @ (HCM.listItems (!cache))
    in
      S.concat (List.map assignToString decls)
    end

  end


  fun typeOf (cty : CT.ty) : llvm_ty = (case cty

    of CT.T_Any => mkPtr(mkInt(cnt 8))

     (* in a mixed type representation, the GC expects wordsize width elements.

        QUESTION/IDEA(kavon): do some analysis and determine when we should zero extend
          the enum and or when to truncate it (and how much we can chop off).

      *)
     | CT.T_Enum _ => mkInt(cnt (8 * wordSzB)) (* NOTE(kavon): these are tagged integers, be careful when you create these *)

     | CT.T_Block _ => labelTy

     | CT.T_Raw (CT.T_Double) => doubleTy

     | CT.T_Raw (CT.T_Float) => floatTy

     (* QUESTION/FIXME(kavon): it's expected that this gets casted as needed since we don't actually know what
        the underlying values in the vector are, so this might be broken/hacky *)
     | CT.T_Raw (CT.T_Vec128) => mkVector(cnt 16, mkInt(cnt 8))

     | CT.T_Raw rt => mkInt(cnt (sizeOfRawTy rt))
      
      (* always a pointer type. *)
      | CT.T_Tuple (_, ts) => (case ts
        of nil => raise Fail "empty tuple. should be an enum for unit."

         | t::nil => mkPtr(typeOf t)

         | ts => mkPtr(mkStruct(List.map typeOf ts))

        (* esac *))


      (* QUESTION(kavon): we represent the vararg part as an arbitrary pointer. is this right? *)
      | CT.T_OpenTuple ts => mkPtr(mkStruct((List.map typeOf ts) @ [ typeOfC(CF.PointerTy) ]))

      | CT.T_Addr t => mkPtr(typeOf t)

      | CT.T_VProc => vprocTy

      | CT.T_Deque => dequeTy

      | CT.T_CFun(CF.CProto(retTy, argTys, _)) => mkFunc([typeOfC retTy] @ (List.map typeOfC argTys))


      (* TODO(kavon): we don't know what our calling convention is right now. so we need to change typesInConv.
                      also in the future we need to append/prepend the pinned register types *)

      | CT.T_StdFun _ => mkPtr(mkFunc( [voidTy] @ typesInConv(cty) ))

      | CT.T_StdCont _ => mkPtr(mkFunc( [voidTy] @ typesInConv(cty) ))

      | CT.T_KnownFunc _ => mkPtr(mkFunc( [voidTy] @ typesInConv(cty) ))

    (* end case *))

    and typeOfC (ct : CF.c_type) : llvm_ty = (case ct
          of CF.PointerTy => mkPtr(mkInt(cnt 8))  (* LLVM's void* *)
           | CF.BaseTy(rawTy) => typeOf(CT.T_Raw rawTy)
           | CF.VoidTy => voidTy
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


  val same = HC.same
  val node = HC.node

  fun select (t : ty, i : int) = let
      fun err () = raise Fail(S.concat["llvm: cannot select ", Int.toString i, "'th type from non-struct ", nameOf t])
      fun sel (_, []) = err()
        | sel (0, t::r) = t
        | sel (i, _::r) = sel(i-1, r)
      in
        case HC.node t
         of T_Struct ts => sel(i, ts)
          | _ => err()
        (* end case *)
      end  

  fun deref (t : ty) = let
    fun err () = raise Fail(S.concat["llvm: cannot dereference non-pointer type ", nameOf t])
  in
    case HC.node t
      of T_Ptr innerT => innerT
       | _ => err()
  end

  (* gepType : (ty, int vector) -> ty *)
  fun gepType (t, vec) = let

    fun err1 (wrongTy, idx) = 
      raise Fail ("gepType: index position " 
                  ^ i2s idx ^ " cannot select from type "
                  ^ toString wrongTy)

    fun err2 (wrongTy, idx, offset) =
      raise Fail ("gepType: problem with index " 
                  ^ i2s idx ^ " of GEP. element "
                  ^ i2s offset ^ " cannot be selected from type "
                  ^ toString wrongTy ^ ", which is part of overall type"
                  ^ toString t)

    fun lp(0, _, t') = t' 
      
      | lp(elms, 1, t) = (case HC.node t
        (* t must be a pointer type. *)
        of T_Ptr t' => lp(elms-1, 2, t')
         | _ => raise Fail "gepType: GEP must be performed on a pointer type"
        (* esac *))

      | lp(elms, idx, t) = let
        (* t cannot be a pointer type. *)
          val t' = 
            (case HC.node t
              of T_Struct tys => let
                  val offset = V.sub(vec, idx)
                in
                  if offset < List.length tys
                  then List.nth(tys, offset)
                  else err2(t, idx, offset)
                end

               | T_Array(_, t') => t'
               | T_Vector(_, t') => t'
               | _ => err1(t, idx)
            (* esac *))
        in
          lp(elms-1, idx+1, t')
        end

    val len = V.length vec

  in
    if len > 0 
    then lp(len, 0, t)
    else raise Fail "gepType: empty index list"
  end


end
