(* c-functions.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure CFunctions =
  struct

    datatype c_type
      = PointerTy
      | BaseTy of RawTypes.raw_ty
      | VoidTy				(* function return types only *)

    datatype attribute
      = A_pure				(* the C function is side-effect free *)
      | A_alloc				(* the C function may allocate head data *)

    datatype 'var c_fun = CFun of {
	var : 'var,			(* name of the Manticore variable bound to *)
					(* this function *)
	name : string,			(* external name *)
	retTy : c_type,			(* return type *)
	argTys : c_type list,		(* argument type *)
	attrs : attribute list
      }

    datatype c_proto = CProto of c_type * c_type list

    fun varOf (CFun{var, ...}) = var
    fun typeOf (CFun{retTy, argTys, ...}) = CProto(retTy, argTys)

    fun tyToString PointerTy = "void *"
      | tyToString (BaseTy rTy) = RawTypes.toString rTy
      | tyToString VoidTy = "void"

    fun protoToString (CProto(retTy, argTys)) = let
	  fun tys2l [] = [")"]
	    | tys2l [ty] = [tyToString ty, ")"]
	    | tys2l (ty::tys) = tyToString ty :: "," :: tys2l tys
	  in
	    String.concat(tyToString retTy :: "(" :: tys2l argTys)
	  end

    fun attrToString A_pure = "pure"
      | attrToString A_alloc = "alloc"

    fun cfunToString (CFun{name, retTy, argTys, attrs, ...}) = let
	  fun list2slist (l, tos, r) lst = let
		fun l2sl [] = r
		  | l2sl [ty] = tos ty :: r
		  | l2sl (x::xs) = tos x :: "," :: l2sl xs
		in
		  l :: l2sl lst
		end
	  val attrs = if null attrs
		then [")"]
		else list2slist (") __attribute__ ((", attrToString, ["))"]) attrs
	  in
	    concat("extern " :: tyToString retTy :: " " :: name ::
		list2slist (" (", tyToString, attrs) argTys)
	  end

  end
