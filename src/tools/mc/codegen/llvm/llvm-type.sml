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
    
    (*
      get the name of an LLVM type
    *)
    val nameOf : ty -> string
    
    (* name of type without using the type cache *)
    val fullNameOf : ty -> string
    
    (*
        get the mangled name of the type for LLVM.
        implements lib/IR/Function.cpp, function "getMangledTypeStr"
    *)
    val mangledNameOf : ty -> string

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
    
    (* determines the arity of a non-vararg function type,
       or a pointer to such a function *)
    val arityOf : ty -> int option

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
    val tokenTy : ty
    
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
    
    (* addrspace(1) pointer; for statepoint calls. *)
    val mkGCPtr : ty -> ty

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
  structure L = List
  structure MV = LLVMMachineVal

  structure HC = HashCons
  structure HCM = HashConsMap

  structure Ty = LLVMTy
  structure HCInt = Ty.HCInt
  type ty = Ty.t
  type ty_node = Ty.t_node
  type count = Ty.count
  
  (* NOTE a refactoring occured to break cyclic dependencies, but I didn't replace
     everything in the codebase, instead I just bring them back into here *)
  open LLVMTy
  
  

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
    
    and mangledNameOf ty = let
        val c2s = i2s o HC.node
    in
        (case HC.node ty
            of Ty.T_Ptr (space, ty) => "p" ^ (c2s space) ^ mangledNameOf ty
             | Ty.T_Array (nelms, ty) => "a" ^ (c2s nelms) ^ mangledNameOf ty
             | Ty.T_Vector (nelms, ty) => "v" ^ (c2s nelms) ^ mangledNameOf ty
             | Ty.T_Func (retTy :: argTys) => S.concat [
                        "f_", mangledNameOf retTy, S.concatWithMap "" mangledNameOf argTys, "f"
                    ]
             | Ty.T_VFunc (retTy :: argTys) => S.concat [
                        "f_", mangledNameOf retTy, S.concatWithMap "" mangledNameOf argTys, "varargf"
                    ]
             | (Ty.T_Struct _ | Ty.T_UStruct _) => let
                    val typedefdName = nameOf ty
                 in
                    if S.sub(typedefdName, 0) = #"%"
                        then S.extract(typedefdName, 1, NONE)
                        else raise Fail "literal struct types cannot be mangled"
                 end
             | _ => fullNameOf ty
            (* end case *))
    end
    

    and mkString (recur : ty -> string) (t : ty) = let
      fun nodeToStr (nt : ty_node) : string =
        let
          val i2s = i2s o HC.node
          
          fun funTyStr varArg = fn (ret::params) =>
            let
                val llvmParams = mapSep(recur, nil, ", ", params)
              in
                S.concat ([recur ret, " ("] @ llvmParams @ [ if varArg then ",..." else "", ")"])
              end
            
        in
         (case nt 
             of Ty.T_Void => "void"
              | Ty.T_Int width => "i" ^ (i2s width)
              | Ty.T_Float => "float"
              | Ty.T_Double => "double"
              | Ty.T_Label => "label"
              | Ty.T_Token => "token"
              | Ty.T_Ptr (space, t) => (case HC.node space
                                         of 0 => (recur t) ^ "*"
                                          | n => (recur t) ^ " addrspace(" ^ (Int.toString n) ^ ")*"
                                        (* end case *))
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
       
    and fullNameOf x = mkString fullNameOf x

    fun toRegType t = (case HC.node t
        of Ty.T_Ptr _ => i64
        | Ty.T_Int _ => i64
        | Ty.T_Float => floatTy
        | Ty.T_Double => doubleTy
        | _ => raise Fail ("Type \n"
                 ^ (nameOf t)
                 ^ "\n does not fit in a machine register, or is not allowed in the calling convention!")
        (* esac *))

    fun typeDecl () = let
      fun assignToString (name, def) = name ^ " = type " ^ def ^ "\n"
      
      val decls = (HCM.listItems (!cache))
    in
      S.concat (List.map assignToString decls)
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
      | CT.T_Tuple (_, ts) => uniformTy

      | CT.T_OpenTuple ts => uniformTy

      | CT.T_Addr t => mkPtr(typeOf t)

      | CT.T_VProc => vprocTy

      | CT.T_Deque => dequeTy

      | CT.T_CFun(CF.CProto(retTy, argTys, _, varArg)) => let
            val funCtor = if varArg then mkVFunc else mkFunc
        in
            mkPtr(funCtor([typeOfC retTy] @ (List.map typeOfC argTys)))
        end
      
      | CT.T_StdFun _ => 
            if Controls.get BasicControl.direct
            then raise Fail "did not expect any CPS StdFuns"
            else mkPtr(mkFunc( voidTy :: getArgsFor cty ))
      | CT.T_StdCont _ => (* we use T_StdCont in both direct and CPS, and they have the same type *)
            mkPtr(mkFunc( voidTy :: getArgsFor cty ))
      | CT.T_KnownFunc _ =>
            if Controls.get BasicControl.direct
            then raise Fail "did not expect any CPS KnownFuncs"
            else mkPtr(mkFunc( voidTy :: getArgsFor cty ))
      | CT.T_KnownDirFunc _ => 
            mkPtr(mkFunc( dsReturnConv cty :: getArgsFor cty ))
                            
      | CT.T_StdDirFun _ => 
            mkPtr(mkFunc( dsReturnConv cty :: getArgsFor cty ))

    (* end case *))
    
    (* the return convention/type used by the given convention. *)
    and dsReturnConv cty = mkUStruct(getRetsFor cty)
    
    (* NOTE this needs to match up with LLVMCall.determineCC *)
    and getArgsFor t = let 
            val padTy = i64
            val getTy = toRegType o typeOf
            val mvTys = L.map MV.machineValTy MV.mvCC
        in
            (case t
             of CT.T_StdFun {clos, args, ret, exh} => 
                    mvTys @ L.map getTy ([clos, ret, exh] @ args)
                     
              | CT.T_StdDirFun {clos, args, exh,...} =>
                    mvTys @ [getTy clos, padTy, getTy exh] @ (L.map getTy args)
                     
              | (  CT.T_StdCont {clos, args} 
                 | CT.T_KnownFunc {clos, args} 
                 | CT.T_KnownDirFunc {clos, args,...}
                ) =>
                    mvTys @ [getTy clos, padTy, padTy] @ (L.map getTy args)
                
              | _ => raise Fail "error: not a function or continuation type!"
             (* end case *))
        end 
        
    (* NOTE this needs to match up with LLVMCall.determineRet *)
    and getRetsFor t = let
        val padTy = i64
        val getTy = toRegType o typeOf
        val mvTys = L.map MV.machineValTy MV.mvCC
        fun withPadding args =
            mvTys @ [padTy, padTy, padTy] @ L.map getTy args
    in
        (case t 
           of (CT.T_StdDirFun {ret,...} | CT.T_KnownDirFunc {ret,...}) =>
                withPadding ret
            | (CT.T_StdCont _) => withPadding [CFGTy.T_Any]  (* NB: doesn't actually return *)
            | (CT.T_KnownFunc _) => withPadding [CFGTy.T_Any] (* NB: doesn't actually return *)
            | _ => raise Fail "error: impossible type when using direct-style!"
            (* esac *))
    end

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
            val llvmParams = S.concatWith ", " (List.map nameOf params)
            val llvmRet = nameOf ret

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
         | Ty.T_Ptr(_,ty) => declOf ty (* a bit aggressive in cutting through pointers here :) *)
         | _ => raise Fail "not a function type"
        (* esac *))
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
         of (Ty.T_UStruct ts | Ty.T_Struct ts) => sel(i, ts)
          | _ => err()
        (* end case *)
      end  

  fun deref (t : ty) = let
    fun err () = raise Fail(S.concat["llvm: cannot dereference non-pointer type ", nameOf t])
  in
    case HC.node t
      of Ty.T_Ptr (_, innerT) => innerT
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
        of Ty.T_Ptr (_, t') => lp(elms-1, idx+1, t')
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
    of Ty.T_Ptr (_, maybeFunc) => (case HC.node maybeFunc
      of (Ty.T_Func (ret::_) | Ty.T_VFunc (ret::_)) => SOME ret
       | _ => NONE
      (* esac *))
     | (Ty.T_Func (ret::_) | Ty.T_VFunc (ret::_)) => SOME ret
     | _ => NONE 
    (* esac *))
    
   (* arityOf : ty -> int option *)
   fun arityOf t = (case HC.node t
     of Ty.T_Ptr (_, maybeFunc) => (case HC.node maybeFunc
       (* arity of a vararg func is not fixed *)
       of Ty.T_Func (_::args) => SOME (List.length args)
        | _ => NONE
       (* esac *))
      | Ty.T_Func (_::args) => SOME (List.length args)
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
