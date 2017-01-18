(* llvm-type.sml
 *
 * COPYRIGHT (c) 2015 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Utility for translating to and managing types in LLVM
 *)

structure LLVMType : sig

    type ty
    type ty_node
    type count

    (*  
      converts a CFG type to an LLVM type.
    *)
    val typeOf : CFGTy.ty -> ty

    val typeOfC : CFunctions.c_type -> ty
    
    (* projects the types of the arguments or return type out of a function type. currently
       does not work on a ptr to a function. *)
    val argsOf : ty -> ty list
    val retOf : ty -> ty
    
    (* produces a thunk that produces a valid LLVM function declaration. 
       thunk's arguments are as follows:
       1. function's calling convention (empty string represents no cc)
       2. function's name (with the @)
     *)
    val declOf : ty -> string -> string -> string

    (* takes a list of "register types" and produces
       a list of indices for these types to assign them according
       to the JWA calling convention in LLVM. the only types
       allowed are those kinds output by toRegType, and
       the indices correspond to the types in jwaCC *)
    val allocateToRegs : ty list -> int list

    (* convert an LLVM type to its standard type that fits within a register.
       this is used in the process of generating a call using the standard
       calling convention, thus it is only implemented for types that can be
       passed in a function call.

       Right now, vector types are not supported (2/11/15), but floats/doubles
       are (they're passed in XMM registers anyways though).
       *)
    val toRegType : ty -> ty
    
    (* computes the width of a given type in BITS.
       note that this function will not reason about unpacked structs
       because their padding is defined by the data layout string, 
       and there is currently no infrastructure to reason about it
       in a general way (we could always hardcode our Manticore assumptions,
       but we don't even need that right now) *)
    val widthOf : ty -> int

    (* get the "Jump With Arguments" calling convention types.
       this is just a list of the types all functions should use
       in order to use the JWA calling convention with proper tail
       call support. jwaCC returns a list of types such that only types
       returned by toRegType are contained in it and nothing else,
       and it always returns the same list. *)
    val jwaCC : ty vector


    (*
      get the name of an LLVM type
    *)
    val nameOf : ty -> string
    
    (* name of type without using the type cache *)
    val fullNameOf : ty -> string

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

    (* Returns the result type of a GEP instruction performed on a value
       of the given type, using the offsets provided. this is the most confusing
       aspect of LLVM, and it's good to read about it here:
       http://llvm.org/docs/GetElementPtr.html
     *)
    val gepType : (ty * int vector) -> ty
    
    (* returns the resulting type of a value extraction/insertion operation
       performed on the provided type. for example, extractvalue in LLVM *)
    val gevType : (ty * int vector) -> ty

    (* if ty is a function type or a pointer to one, get the function's return type.
       if it is not either of those, it returns NONE *)
    val returnTy : ty -> ty option

    (* TODO might be a useful function *)
    (* val sizeOf : ty * ty -> int *)

    (* project the node out of a ty. 
       useful for clean pattern matching *)
    val node : ty -> ty_node

    (* the types *)

    val voidTy : ty
    val vprocTy : ty
    val allocPtrTy : ty
    val dequeTy : ty
    val labelTy : ty
    val floatTy : ty
    val doubleTy : ty
    val boolTy : ty
    val uniformTy : ty
    val enumTy : ty
    val voidStar : ty
    val gcHeaderTy : ty
    
    (* common integer types *)
    val i64 : ty
    val i32 : ty
    val i16 : ty
    val i8 : ty
    val i1 : ty

    (* count is number of bits wide for a custom int *)
    val mkInt : count -> ty

    (* first element is return type *)
    val mkFunc : ty list -> ty
    
    (* first element is return type. VFunc is a vararg function *)
    val mkVFunc : ty list -> ty

    val mkPtr : ty -> ty

    (* count is number of elements of type ty *)
    val mkVector : count * ty -> ty
    val mkArray : count * ty -> ty

    val mkStruct : ty list -> ty
    val mkUStruct : ty list -> ty
    
    val cnt : int -> count
    val tnc : count -> int

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

  structure Ty = LLVMTy
  structure HCInt = Ty.HCInt
  type ty = Ty.t
  type ty_node = Ty.t_node
  type count = Ty.count

  local
    (* ctors for ty *)
    fun eq query = (case query
      of (Ty.T_Void, Ty.T_Void) => true
       | (Ty.T_Label, Ty.T_Label) => true
       | (Ty.T_Token, Ty.T_Token) => true
       | (Ty.T_Func xs, Ty.T_Func ys) => ListPair.allEq HC.same (xs, ys)
       | (Ty.T_VFunc xs, Ty.T_VFunc ys) => ListPair.allEq HC.same (xs, ys)
       | (Ty.T_Int x, Ty.T_Int y) => HC.same(x, y)
       | (Ty.T_Float, Ty.T_Float) => true
       | (Ty.T_Double, Ty.T_Double) => true
       | (Ty.T_Ptr x, Ty.T_Ptr y) => HC.same(x, y)
       | (Ty.T_Vector (xcount, x), Ty.T_Vector (ycount, y)) => HC.same(xcount, ycount) andalso HC.same(x, y)
       | (Ty.T_Array (xcount, x), Ty.T_Array (ycount, y)) => HC.same(xcount, ycount) andalso HC.same(x, y)
       | (Ty.T_Struct xs, Ty.T_Struct ys) => ListPair.allEq HC.same (xs, ys)
       | (Ty.T_UStruct xs, Ty.T_UStruct ys) => ListPair.allEq HC.same (xs, ys)
       | _ => false
      (* esac *))
    
    val tbl = HC.new {eq = eq}

  in
    (* should be prime numbers.
       I skip 2 because I think the hash function uses it to combine these? *)
    val voidTy = HC.cons0 tbl (0w3, Ty.T_Void)
    val labelTy = HC.cons0 tbl (0w7, Ty.T_Label)
    val mkFunc = HC.consList tbl (0w11, Ty.T_Func)
    val mkInt = HC.cons1 tbl (0w13, Ty.T_Int)
    val floatTy = HC.cons0 tbl (0w17, Ty.T_Float)
    val doubleTy = HC.cons0 tbl (0w19, Ty.T_Double)
    val mkPtr = HC.cons1 tbl (0w23, Ty.T_Ptr)
    val mkVector = HC.cons2 tbl (0w29, Ty.T_Vector)
    val mkArray = HC.cons2 tbl (0w31, Ty.T_Array)
    val mkStruct = HC.consList tbl (0w37, Ty.T_Struct)
    val mkUStruct = HC.consList tbl (0w43, Ty.T_UStruct)
    val mkVFunc = HC.consList tbl (0w47, Ty.T_VFunc)
    val tokenTy = HC.cons0 tbl (0w53, Ty.T_Token)
    
    (* more primes  59     61     67     71 *)

    val cnt = HCInt.mk
    val tnc = HC.node
  end
  
  

  val i2s = Int.toString

  val wordSzB = 8

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
    
  in  
  
    fun toString t = mkString nameOf t

    and mkString (recur : ty -> string) (t : ty) = let
      fun nodeToStr (nt : ty_node) : string =
        let
          val i2s = i2s o HC.node
          
          fun funTyStr varArg = fn (ret::params) =>
            let
                val llvmParams = mapSep(recur, nil, ", ", params)
              in
                S.concat ([recur ret, " ("] @ llvmParams @ [ if varArg then "..." else "", ")"])
              end
            
        in
         (case nt 
             of Ty.T_Void => "void"
              | Ty.T_Int width => "i" ^ (i2s width)
              | Ty.T_Float => "float"
              | Ty.T_Double => "double"
              | Ty.T_Label => "label"
              | Ty.T_Token => "token"
              | Ty.T_Ptr t => (recur t) ^ "*"
              | Ty.T_Func ts => funTyStr false ts
              | Ty.T_VFunc ts => funTyStr true ts
              
              | Ty.T_Vector (nelms, t) => S.concat ["<", i2s nelms, " x ", recur t, ">"]

              | Ty.T_Array (nelms, t) => S.concat ["[", i2s nelms, " x ", recur t, "]"]

                (* these are packed structs *)
              | Ty.T_Struct ts => S.concat (["<{ "] @ mapSep(recur, nil, ", ", ts) @ [" }>"])
              
              | Ty.T_UStruct ts => S.concat (["{ "] @ mapSep(recur, nil, ", ", ts) @ [" }"])

              (* | _ => raise Fail "base type name unknown" *)

            (* end case *))
         end
      in
        (nodeToStr o HC.node) t
      end
                              (* `name` = type `rhs` *)
    and mkAlias recur (t : ty) : (string * string option) = let
        fun freshStamp () = (stamp := !stamp + 1 ; !stamp)
      in
        (case HC.node t
          of Ty.T_Struct _ => ("%_tupTy." ^ i2s(freshStamp()) , SOME(mkString recur t))
           | Ty.T_UStruct _ => ("%_utupTy." ^ i2s(freshStamp()) , SOME(mkString recur t))
           
           (* NOTE(kavon): turns out you cannot forward reference non-struct types in LLVM.
                           if we figure out a way to do it at some point, you can uncomment
                           and change the following case. *)

           (* | Ty.T_Func _ => ("%_funTy." ^ i2s(freshStamp()) , SOME(toString t)) *)

           | _ => (mkString recur t, NONE)
        (* esac *))
      end

    (* looks up this type in the cache. if it is
       not already present, it will generate a new entry and return its name. *)
    and nameOf x = (case HCM.find(!cache, x)
      of SOME (name, _) => name
       | NONE => (case mkAlias nameOf x
          of (name, NONE) => name
           | (name, SOME rhs) => 
              ( cache := HCM.insert(!cache, x, (name, rhs)) ; name )
          (* esac *))
       (* esac *))
       
    fun fullNameOf x = mkString fullNameOf x
       
       
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
       
       
    local
     (* FIXME for now, we assume that there are no vector types, and
     labels already can't be passed as an arg anyways *)
     val gprTy = mkInt(cnt 64)
     val f32Ty = floatTy
     val f64Ty = doubleTy

     (* [x, y) kinda gross but whatever.
        NOTE that GPRS _must_ start at 0, and be allocated in order,
        because in llvm-printer, the machine indices for the vector
       also starts at 0 and we just reuse that numbering.  *)
     val gprRange = (0, 14)
     val f32Range = (14, 22)
     val f64Range = (22, 30)
    in
     fun toRegType t = (case HC.node t
       of Ty.T_Ptr _ => gprTy
        | Ty.T_Int _ => gprTy
        | Ty.T_Float => f32Ty
        | Ty.T_Double => f64Ty
        | _ => raise Fail ("Type \n"
                 ^ (nameOf t)
                 ^ "\n does not fit in a machine register, or is not allowed in the calling convention!")
       (* esac *))

       (* RAX, RBX, RCX, RDX, RSI, RDI, R8, R9, R10, R11, R12, R13, R14, R15
          XMM0 - XMM15 for floats, whether 32 or 64 bit *)
      val jwaCC =
        Vector.tabulate(#2(f64Range), fn x =>
             if x < #2(gprRange) then gprTy
             else if x < #2(f32Range) then f32Ty
             else f64Ty)

     fun allocateToRegs ts = let
     (* might be better to use a vector instead of 3 integers for code cleanliness *)
         fun alloc (nil, is, _, _, _) = List.rev is
           | alloc (t::ts, is, gpr, f32, f64) =
             if (gpr < #2(gprRange)
                 andalso f32 < #2(f32Range)
                 andalso f64 < #2(f64Range))
             then (case HC.node t (* NOTE: assuming Int x where x <= 64 *)
                   of Ty.T_Int _ => alloc(ts, gpr::is, gpr+1, f32, f64)
                    | Ty.T_Float   => alloc(ts, f32::is, gpr, f32+1, f64)
                    | Ty.T_Double  => alloc(ts, f64::is, gpr, f32, f64+1)
                    | _ => raise Fail "bad register type"
                   (* esac *))
             else raise Fail "allocateToRegs: overflow of JWA calling convention detected!"
     in
         alloc(ts, [], #1(gprRange), #1(f32Range), #1(f64Range))
     end
       
       
       
       
    

    fun typeDecl () = let
      fun assignToString (name, def) = name ^ " = type " ^ def ^ "\n"
      
      val decls = (HCM.listItems (!cache))
    in
      S.concat (List.map assignToString decls)
    end

  end

  
  fun typeOf (cty : CT.ty) : ty = (case cty

    of CT.T_Any => uniformTy

     | CT.T_Enum _ => enumTy (* mkInt(cnt (8 * wordSzB)) *)
            (* NOTE(kavon): these are tagged integers, be careful when you create these *)

     | CT.T_Block _ => labelTy

     | CT.T_Raw (CT.T_Double) => doubleTy

     | CT.T_Raw (CT.T_Float) => floatTy

     (* NOTE(kavon): We need to redesign Vec128 to be something like
        a Vec(int, rawTy) or Vec128(rawTy) to indicate the type of its components.
        Right now, nobody is using Vec128 so it's not implemented yet. *)
     | CT.T_Raw (CT.T_Vec128) => raise Fail "Vec128 is not yet implemented in the LLVM backend."

     | CT.T_Raw rt => mkInt(cnt (sizeOfRawTy rt))
      
      (* always a pointer type. *)
      | CT.T_Tuple (_, ts) => uniformTy (* determineTuple ts *)

      | CT.T_OpenTuple ts => uniformTy (* determineTuple ts *)

      | CT.T_Addr t => mkPtr(typeOf t)

      | CT.T_VProc => vprocTy

      | CT.T_Deque => dequeTy

      | CT.T_CFun(CF.CProto(retTy, argTys, _, varArg)) => let
            val funCtor = if varArg then mkVFunc else mkFunc
        in
            funCtor([typeOfC retTy] @ (List.map typeOfC argTys))
        end
      
      | CT.T_StdFun _ => mkPtr(mkFunc( [voidTy] @ typesInConv(cty) ))
      | CT.T_StdCont _ => mkPtr(mkFunc( [voidTy] @ typesInConv(cty) ))
      | CT.T_KnownFunc _ => mkPtr(mkFunc( [voidTy] @ typesInConv(cty) ))
      
      (* FIXME? right now we're assuming a single return value. multiple
               return values would warrant returning a struct, but
               I don't actually know how we'll allocate, initialize, and return
               such a struct. *)
               
      (* NOTE no need to do hacky stuff with types given the JWA convention in the
              direct-style case. *)
      
      (* NOTE see paramsOfConv to ensure the ordering is right here. *)
      | CT.T_KnownDirFunc {ret=[retTy], clos, args} => 
            mkPtr(mkFunc(List.map typeOf (retTy :: clos :: args) ))
                            
      | CT.T_StdDirFun {ret=[retTy], clos, args, exh} => 
            mkPtr(mkFunc(List.map typeOf (retTy :: clos :: args @ [exh]) ))

    (* end case *))

    (* single element tuples are just a pointer to the object itself.
    
        NOTE 6/14/16
        
        We can't rely on unpacked structs + data layout strings to generate code
        that respects the alignment constraints of our runtime system.
        
     *)
    and determineTuple ts = (case ts
        of nil => raise Fail "empty tuple. should be an enum for unit."
        
         | t::nil => mkPtr(typeOf t)

         | ts => mkPtr(mkUStruct(List.map typeOf ts))
        (* esac *))

    and typeOfC (ct : CF.c_type) : ty = (case ct
          of CF.PointerTy => voidStar  (* LLVM's void* *)
           | CF.BaseTy(rawTy) => typeOf(CT.T_Raw rawTy)
           | CF.VoidTy => voidTy
          (* end case *))
          
    and argsOf (fnTy : ty) : ty list = (case HC.node fnTy
        of (Ty.T_Func (_::args) | Ty.T_VFunc (_::args)) => args
         | _ => raise Fail "not a function type"
        (* esac *))
        
    and retOf (fnTy : ty) : ty = (case HC.node fnTy
        of (Ty.T_Func (ret::_) | Ty.T_VFunc (ret::_)) => ret
         | _ => raise Fail "not a function type"
        (* esac *))
        
    and declOf (fnTy : ty) = let
        fun mkDecl ret params varArg = let
            val llvmParams = S.concatWith ", " (List.map toString params)
            val llvmRet = toString ret

            val llvmParams = if not varArg
                          then llvmParams
                          else if S.size llvmParams > 0
                            then S.concat [llvmParams, ", ..."]
                            else "..."
        in
            (fn cc => fn name => S.concat ["declare ", (if cc = "" then "" else cc ^ " "),
                                            llvmRet, " ", name, "(", llvmParams, ")"])
        end
        
    in
    (case HC.node fnTy
        of Ty.T_Func (ret::params) => mkDecl ret params false
         | Ty.T_VFunc (ret::params) => mkDecl ret params true
         | _ => raise Fail "not a function type"
        (* esac *))
    end
    
    
    

    (* everybody has same types according to LLVM *)
    and typesInConv (cty : CT.ty) : ty list = (case cty
        of (CT.T_StdFun _ 
            | CT.T_StdCont _ 
            | CT.T_KnownFunc _ ) =>
                List.tabulate(Vector.length jwaCC, 
                    fn i => Vector.sub(jwaCC, i))
         | _ => raise Fail ("only functions/continuations have calling convention types")
        (* esac *))

  val same = HC.same
  val node = HC.node

  fun select (t : ty, i : int) = let
      fun err () = raise Fail(S.concat["llvm: cannot select ", Int.toString i, "'th type from non-struct ", nameOf t])
      fun sel (_, []) = err()
        | sel (0, t::r) = t
        | sel (i, _::r) = sel(i-1, r)
      in
        case HC.node t
         of (Ty.T_UStruct ts | Ty.T_Struct ts) => sel(i, ts)
          | _ => err()
        (* end case *)
      end  

  fun deref (t : ty) = let
    fun err () = raise Fail(S.concat["llvm: cannot dereference non-pointer type ", nameOf t])
  in
    case HC.node t
      of Ty.T_Ptr innerT => innerT
       | _ => err()
  end

  
(* gepType : (ty, int vector) -> ty *)
  fun gepType (t, vec) = let

    (*fun err1 (wrongTy, idx) = 
      raise Fail ("gepType: index position " 
                  ^ i2s idx ^ " cannot select from type "
                  ^ toString wrongTy)

    fun err2 (wrongTy, idx, offset) =
      raise Fail ("gepType: problem with index " 
                  ^ i2s idx ^ " of GEP.\n element "
                  ^ i2s offset ^ " \ncannot be selected from type "
                  ^ toString wrongTy ^ ",\n which is part of overall type"
                  ^ toString t)*)
                  
    fun ohno (wrongTy, idx) = raise Fail ("gepType: problem with index: " ^ i2s idx ^ " of GEP.\n"
                ^ "element offset: " ^ i2s (V.sub(vec, idx))
                ^ " \ncannot be selected from type: " ^ (toString  wrongTy)
                ^ ",\n which is part of overall type: " ^ (toString t)
                ^ "\n being operated on using GEP args "
                ^ (V.foldl (fn (x, acc) => acc ^ ", " ^ (Int.toString x)) "" vec))

    fun lp(0, _, t') = t' 
      
      | lp(elms, (idx as 0), ogT) = (case HC.node ogT
        (* t must be a pointer type, we step through the pointer *)
        of Ty.T_Ptr t' => lp(elms-1, idx+1, t')
         | _ => ohno(ogT, idx)
        (* esac *))

      | lp(elms, idx, t) = let
        val t' = (case HC.node t
                 of (Ty.T_Void | Ty.T_Label | Ty.T_Token) => ohno(t, idx)
                  | (Ty.T_Struct tys | Ty.T_UStruct tys) => let
                      val offset = V.sub(vec, idx)
                    in
                      if offset < List.length tys
                      then List.nth(tys, offset)
                      else ohno(t, idx)
                    end

                   | Ty.T_Array(_, t') => t'
                   | Ty.T_Vector(_, t') => t'
                   | _  => t (* we do not step through any further pointers *)
                (* esac *))            
        in
          lp(elms-1, idx+1, t')
        end

    val len = V.length vec

  in
    if len > 0 
    then mkPtr(lp(len, 0, t))
    else raise Fail "gepType: empty index list"
  end
  
  (* get element Value type, aka, for extractvalue/insertvalue operations,
    which works the same as GEP except there's an implicit i32 0 as the first index,
    and it only works on structs or arrays (tho theres no arrays right now) NOTE *)
  (* gepType : (ty, int vector) -> ty *)
  fun gevType (t, vec) = let

    fun err1 (wrongTy, idx) = 
      raise Fail ("gevType: index position " 
                  ^ i2s idx ^ " cannot select from type "
                  ^ toString wrongTy)

    fun err2 (wrongTy, idx, offset) =
      raise Fail ("gevType: problem with index " 
                  ^ i2s idx ^ " of GEV. element "
                  ^ i2s offset ^ " cannot be selected from type "
                  ^ toString wrongTy ^ ", which is part of overall type"
                  ^ toString t)

    fun lp(0, _, t') = t' 

      | lp(elms, idx, t) = let
        (* t must be a struct or array *)
          val t' = 
            (case HC.node t
              of (Ty.T_Struct tys | Ty.T_UStruct tys) => let
                  val offset = V.sub(vec, idx)
                in
                  if offset < List.length tys
                  then List.nth(tys, offset)
                  else err2(t, idx, offset)
                end

               | Ty.T_Array(_, t') => t'
               | Ty.T_Vector(_, t') => t'
               | _ => err1(t, idx)
            (* esac *))
        in
          lp(elms-1, idx+1, t')
        end

    val len = V.length vec

  in
    if len > 0 
    then lp(len, 0, t)
    else raise Fail "gevType: empty index list"
  end

  (* returnTy : ty -> ty option *)
  fun returnTy t = (case HC.node t
    of Ty.T_Ptr maybeFunc => (case HC.node maybeFunc
      of (Ty.T_Func (ret::_) | Ty.T_VFunc (ret::_)) => SOME ret
       | _ => NONE
      (* esac *))
     | (Ty.T_Func (ret::_) | Ty.T_VFunc (ret::_)) => SOME ret
     | _ => NONE 
    (* esac *))
    
   (* in bits *)
   fun widthOf someTy = (case HC.node someTy
     of Ty.T_Int width => tnc width
      | Ty.T_Float => 32
      | Ty.T_Double => 64
      | Ty.T_Ptr t => 64 (* this is a fundamental assumption made throughout our backend *)
      | (Ty.T_Array (nelms, t) | Ty.T_Vector (nelms, t)) => (tnc nelms) * (widthOf t)
      | Ty.T_Struct ts => List.foldl (fn (x, acc) => (widthOf x) + acc) 0 ts
      | _ => raise Fail ("type " ^ (fullNameOf someTy) ^ " has unknown width.")
      (* esac *))

end (* end local *)


end (* end struct *)
