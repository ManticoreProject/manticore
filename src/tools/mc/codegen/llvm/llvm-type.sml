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
      get the LLVM name of this type
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
  structure CF = CFunctions

  datatype ty = 
      T_Void
    | T_VProc
    | T_Deque
    | T_Label
    | T_Func of ty * (ty list) * bool
    | T_Int of int (* width *)
    | T_Float
    | T_Double
    | T_Ptr of ty
    | T_Vector of int * ty
    | T_Array of int * ty
    | T_Struct of ty list

  

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

  fun toString (t : ty) = (case t
     
     of T_Void => "void"
      | T_Int width => "i" ^ (i2s width)
      | T_Float => "float"
      | T_Double => "double"
      | T_Label => "label"
      | T_VProc => "%_vproc.ty" (* TODO(kavon): need to add typedefs for deque/vproc <-> i8* *)
      | T_Deque => "%_deque.ty"
      | T_Ptr t => (toString t) ^ "*"
      
      | T_Func (ret, params, varArg) => let
          val llvmParams = mapSep(toString, nil, ", ", params)

          val llvmParams = if not varArg
                      then llvmParams
                      else if List.length llvmParams > 0
                        then llvmParams @ [", ..."]
                        else ["..."]
        in
          S.concat ([toString ret, " ("] @ llvmParams @ [")"])
        end      
      
      | T_Vector (nelms, t) => S.concat ["<", i2s nelms, " x ", toString t, ">"]

      | T_Array (nelms, t) => S.concat ["[", i2s nelms, " x ", toString t, "]"]

      | T_Struct ts => S.concat (["{ "] @ mapSep(toString, nil, ", ", ts) @ [" }"])

    (* end case *))


  (* TODO(kavon): turns out that without caching these types, the output becomes
                  quite unreadable. Especially since we need to add to
                  every tuple a GC tag field. So, we should cache all boxed
                  types (tuples, open tuples) and manticore function types  *)

  fun typeOf (cty : CT.ty) : ty = (case cty

    of CT.T_Any => T_Ptr (T_Int 8) 

     (* in a mixed type representation, the GC expects wordsize width elements.

        IDEA(kavon): do some analysis and determine when we should zero extend
          the enum and or when to truncate it (and how much we can chop off).

      *)
     | CT.T_Enum _ => T_Int (8 * wordSzB) (* NOTE: these are tagged integers, be careful when you create these *)

     | CT.T_Block _ => T_Label

     | CT.T_Raw (CT.T_Double) => T_Double

     | CT.T_Raw (CT.T_Float) => T_Float

     (* FIXME(kavon): it's expected that this gets casted as needed since we don't actually know what
        the underlying values in the vector are, so this might be broken/hacky *)
     | CT.T_Raw (CT.T_Vec128) => T_Vector(16, T_Int 8)

     | CT.T_Raw rt => T_Int (sizeOfRawTy rt)

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
      | CT.T_Tuple (_, ts) =>  T_Ptr(T_Struct (List.map typeOf ts))

      (* TODO(kavon): we represent the vararg part as the any type. might want to change it to an arbitrary C pointer? *)
      | CT.T_OpenTuple ts => T_Ptr(T_Struct ((List.map typeOf ts) @ [ typeOf(CT.T_Any) ]))

      | CT.T_Addr t => T_Ptr (typeOf t)

      (* TODO(kavon): vproc and deque should probably be externally defined types?
                      can we even do that? if not, i8* works too since we use hardcoded offsets
                       right now anyways. *)
      | CT.T_VProc => T_VProc

      | CT.T_Deque => T_Deque

      | CT.T_CFun(CF.CProto(retTy, argTys, _)) => T_Func(typeOfC retTy, List.map typeOfC argTys, false)

      (* TODO(kavon): we don't know what our calling convention is right now, so leaving this blank. *)
      | CT.T_StdFun _ => T_Ptr(T_Func(T_Void, typesInConv(cty), false)) 

      | CT.T_StdCont _ => T_Ptr(T_Func(T_Void, typesInConv(cty), false))

      | CT.T_KnownFunc _ => T_Ptr(T_Func(T_Void, typesInConv(cty), false))

    (* end case *))

    and typeOfC (ct : CF.c_type) : ty = (case ct
          of CF.PointerTy => T_Ptr(T_Int 8) (* LLVM's void* *)
           | CF.BaseTy(rawTy) => typeOf(CT.T_Raw rawTy)
           | CF.VoidTy => T_Void
          (* end case *))


    (* TODO(kavon): does not yet include types for pinned registers *)
    and typesInConv (cty : CT.ty) : ty list = (case cty
      
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

     

end
