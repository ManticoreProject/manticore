(* translate-types.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure TranslateTypes : sig

    val tr : TranslateEnv.env * AST.ty -> BOM.ty
    val trScheme : TranslateEnv.env * AST.ty_scheme -> BOM.ty

    val trDataCon : TranslateEnv.env * AST.dcon -> TranslateEnv.con_bind

  end = struct

    structure Ty = Types;
    structure BTy = BOMTy
    structure BTyc = BOMTyCon
    structure E = TranslateEnv

    fun appi f = let
	  fun appf (_, []) = ()
	    | appf (i, x::xs) = (f(i, x); appf(i+1, xs))
	  in
	    fn l => appf (0, l)
	  end

  (* flatten tuple types and wrapped raw values into a list of types *)
    fun flatten (BTy.T_Tuple(false, [ty as BTy.T_Raw _])) = [ty]
      | flatten (BTy.T_Tuple(false, tys)) = List.foldr (fn (ty, tys) => flatten ty @ tys) [] tys
      | flatten ty = [ty]

    fun insertConst (env, dc, w, ty) = E.insertCon (env, dc, E.Const(w, ty))
    fun insertDCon (env, dc, dc') = E.insertCon (env, dc, E.DCon dc')

    fun tr (env, ty) = let
	  fun tr' ty = (case TypeUtil.prune ty
		 of Ty.ErrorTy => raise Fail "unexpected ErrorTy"
		  | Ty.MetaTy(Ty.MVar{info=ref(Ty.UNIV _), ...}) => BTy.T_Any
		  | Ty.MetaTy _ => raise Fail "unexpected kinded MetaTy"
		  | Ty.VarTy _ => BTy.T_Any
		  | Ty.ConTy(tyArgs, tyc) => (
		      case TranslateEnv.findTyc (env, tyc)
		       of SOME ty => ty
			| NONE => trTyc (env, tyc)
		      (* end case *))
		  | Ty.FunTy(ty1, ty2) => BTy.T_Fun([tr' ty1], [BTy.exhTy], [tr' ty2])
		  | Ty.TupleTy [] => BTy.unitTy
		  | Ty.TupleTy tys => BTy.T_Tuple(false, List.map tr' tys)
		(* end case *))
	  in
	    tr' ty
	  end

    and trTyc (env, ab as Ty.AbsTyc{name, ...}) =
	  raise Fail("Unknown abstract type " ^ Atom.toString name)
      | trTyc (env, tyc as Ty.DataTyc{name, cons, ...}) = let
	(* insert a placeholder representation for tyc to avoid infinite loops *)
	  val _ = E.insertTyc (env, tyc, BTy.T_Any)
	(* partition constructors into constants and constructor function lists *)
	  val (consts, conFuns) =
		List.partition
		  (fn (Ty.DCon{argTy=NONE, ...}) => true | _ => false)
		    (! cons)
	(* create the datatype constructor *)
	  val nConsts = List.length consts
	  val dataTyc as BTy.DataTyc{rep, kind, ...} =
		BOMTyCon.newDataTyc (Atom.toString name, nConsts)
	  fun setRep (ty, k) = (rep := ty; kind := k)
	(* assign representations for the constants *)
	  fun assignConstRep (i, dc) =
		insertConst (env, dc, Word.fromInt i, BTy.T_Enum(Word.fromInt nConsts - 0w1))
	  val _ = appi assignConstRep consts
	(* assign representations for the constructor functions *)
	  val newDataCon = BTyc.newDataCon dataTyc
	  fun mkDC (dc, rep, tys) = let
		val dc' = newDataCon (DataCon.nameOf dc, rep, tys)
		in
		  insertDCon (env, dc, dc')
		end
	(* translate the argument type of a data constructor; for tuples of two, or more,
	 * components, we flatten the representation.
	 *)
	  fun trArgTy dc = (case tr (env, valOf (DataCon.argTypeOf dc))
		 of BTy.T_Tuple(false, tys as _::_::_) => tys
		  | ty => [ty]
		(* end case *))
	  in
	    case (consts, conFuns)
	     of (_::_, []) => setRep (BTy.T_Enum(Word.fromInt nConsts - 0w1), BTy.K_UNBOXED)
	      | ([], [dc]) => let
		  val ty = tr (env, valOf (DataCon.argTypeOf dc))
		  in
		    setRep (ty, BOMTyUtil.kindOf ty);
		    mkDC (dc, BTy.Transparent, [ty])
		  end
	      | (_, [dc]) => (
		  case bomKindOfType (env, valOf(DataCon.argTypeOf dc))
		   of BTy.K_BOXED => mkDC (dc, BTy.Transparent, trArgTy dc)
		    | _ => (* need to use singleton tuple to represent data constructor *)
			mkDC (dc, BTy.Tuple, [BTy.T_Tuple(false, trArgTy dc)])
		  (* end case *))
	      | ([], [dc1, dc2]) => raise Fail "FIXME"
	      | (_, _) => let
		  fun mkDC' (i, dc) = mkDC (dc, BTy.TaggedTuple(Word.fromInt i), trArgTy dc)
		  in
		    appi mkDC' conFuns
		  end
	    (* end case *);
	    E.insertTyc (env, tyc, BTy.T_TyCon dataTyc);
	    BTy.T_TyCon dataTyc
	  end

  (* return the BOM kind of an AST type; this code looks at the top-level structure
   * of the type to determine the kind.
   *)
    and bomKindOfType (env, ty) = (case ty
	   of (Ty.FunTy _) => BTy.K_BOXED
	    | (Ty.TupleTy []) => BTy.K_UNBOXED
	    | (Ty.TupleTy _) => BTy.K_BOXED
	    | (Ty.ConTy(_, tyc)) => (
		case TranslateEnv.findTyc (env, tyc)
		 of SOME ty => BOMTyUtil.kindOf ty
		  | NONE => BOMTyUtil.kindOf (trTyc (env, tyc))
		(* end case *))
	    | _ => BTy.K_UNIFORM
	  (* end case *))

    fun trScheme (env, Ty.TyScheme(_, ty)) = tr (env, ty)

    fun trDataCon (env, dc as Ty.DCon{owner, ...}) = (case E.findDCon(env, dc)
	   of SOME dc' => dc'
	    | NONE => (ignore (trTyc(env, owner)); trDataCon (env, dc))
	  (* end case *))

  end
