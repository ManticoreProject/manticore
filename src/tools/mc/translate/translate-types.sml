(* translate-types.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure TranslateTypes : sig

    val tr : TranslateEnv.env * AST.ty -> BOM.ty
    val trScheme : TranslateEnv.env * AST.ty_scheme -> BOM.ty

    val trDataCon : TranslateEnv.env * AST.dcon -> TranslateEnv.con_bind

  (* record the BOM kind of the representation of an AST type constructor *)
    val setTycKind : Types.tycon * BOMTy.kind -> unit

  end = struct

    structure Ty = Types
    structure BTy = BOMTy
    structure BTyc = BOMTyCon
    structure E = TranslateEnv

    fun appi f = let
	  fun appf (_, []) = ()
	    | appf (i, x::xs) = (f(i, x); appf(i+1, xs))
	  in
	    fn l => appf (0, l)
	  end

  (* a property to track the mapping from AST type constructors to BOM kinds *)
    local
      val {getFn, setFn, ...} = TyCon.newProp (fn _ => BTy.K_UNIFORM)
    in
    val getTycKind = getFn
    val setTycKind = setFn
    end

  (* flatten tuple types and wrapped raw values into a list of types *)
    fun flatten ty = #2(FlattenRep.flatten ty)

    fun insertConst (env, dc, w, ty) = E.insertCon (env, dc, E.Const(w, ty))
    fun insertDCon (env, dc, dc') = E.insertCon (env, dc, E.DCon dc')

  (* return the BOM kind of the argument of an AST data constructor; this code
   * looks at the top-level structure of the type to determine the kind.
   *)
    fun bomKindOfArgTy dc = (case DataCon.argTypeOf dc
	   of SOME(Ty.FunTy _) => BTy.K_BOXED
	    | SOME(Ty.TupleTy[]) => BTy.K_UNBOXED
	    | SOME(Ty.TupleTy _) => BTy.K_BOXED
	    | SOME(Ty.ConTy(_, tyc)) => getTycKind tyc
	    | _ => BTy.K_UNIFORM
	  (* end case *))

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

    and trTyc (env, tyc as Ty.Tyc{name, def, ...}) = (case def
	   of Ty.AbsTyc => raise Fail("Unknown abstract type " ^ Atom.toString name)
	    | Ty.DataTyc{cons, ...} => let
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
		fun setRep (ty, k) = (rep := ty; kind := k; setTycKind(tyc, k))
	      (* assign representations for the constants *)
		fun assignConstRep (i, dc) =
		      insertConst (env, dc, Word.fromInt i, BTy.T_Enum(Word.fromInt nConsts - 0w1))
		val _ = appi assignConstRep consts
	      (* translate the argument type of a data constructor; for tuples of two, or more,
	       * components, we flatten the representation.
	       *)
		fun trArgTy dc = flatten (tr (env, valOf (DataCon.argTypeOf dc)))
	      (* assign representations for the constructor functions *)
		val newDataCon = BTyc.newDataCon dataTyc
		fun mkDC (dc, rep, tys) = let
		      val dc' = newDataCon (DataCon.nameOf dc, rep, tys)
		      in
			insertDCon (env, dc, dc')
		      end
		fun mkTaggedDC (i, dc) = mkDC (dc, BTy.TaggedTuple(Word.fromInt i), trArgTy dc)
		in
		  case (consts, conFuns)
		   of (_::_, []) => setRep (BTy.T_Enum(Word.fromInt nConsts - 0w1), BTy.K_UNBOXED)
		    | ([], [dc]) => (case trArgTy dc
			 of [ty] => (
			      setRep (ty, BOMTyUtil.kindOf ty);
			      mkDC (dc, BTy.Transparent, [ty]))
			  | tys => (
			      setRep (BTy.T_Tuple(false, tys), BTy.K_BOXED);
			      mkDC (dc, BTy.Tuple, tys))
			(* end case *))
		    | (_, [dc]) => (
			case bomKindOfArgTy dc
			 of BTy.K_BOXED => (case trArgTy dc
			       of [ty] => mkDC (dc, BTy.Transparent, [ty])
				| tys => mkDC (dc, BTy.Tuple, tys)
			      (* end case *))
			  | _ => (* need to use singleton tuple to represent data constructor *)
			      mkDC (dc, BTy.Tuple, [BTy.T_Tuple(false, trArgTy dc)])
			(* end case *);
			setRep (BTy.T_Any, BTy.K_UNIFORM))
		    | ([], [dc1, dc2]) => (case (bomKindOfArgTy dc1, bomKindOfArgTy dc2)
			 of (BTy.K_BOXED, BTy.K_UNBOXED) => (
			      mkDC (dc1, BTy.Tuple, trArgTy dc1);
			      mkDC (dc2, BTy.Transparent, trArgTy dc2);
			      setRep (BTy.T_Any, BTy.K_UNIFORM))
			  | (BTy.K_UNBOXED, BTy.K_BOXED) => (
			      mkDC (dc1, BTy.Transparent, trArgTy dc1);
			      mkDC (dc2, BTy.Tuple, trArgTy dc2);
			      setRep (BTy.T_Any, BTy.K_UNIFORM))
			  | _ => (
			      mkTaggedDC (0, dc1);
			      mkTaggedDC (1, dc2);
			      setRep (BTy.T_Any, BTy.K_BOXED))
			(* end case *))
		    | ([], _) => (
			appi mkTaggedDC conFuns;
			setRep (BTy.T_Any, BTy.K_BOXED))
		    | (_, _) =>  (
			appi mkTaggedDC conFuns;
			setRep (BTy.T_Any, BTy.K_UNIFORM))
		  (* end case *);
		  E.insertTyc (env, tyc, BTy.T_TyCon dataTyc);
		  BTy.T_TyCon dataTyc
		end
	  (* end case *))

    fun trScheme (env, Ty.TyScheme(_, ty)) = tr (env, ty)

    fun trDataCon (env, dc as Ty.DCon{owner, ...}) = (case E.findDCon(env, dc)
	   of SOME dc' => dc'
	    | NONE => (ignore (trTyc(env, owner)); trDataCon (env, dc))
	  (* end case *))

  end
