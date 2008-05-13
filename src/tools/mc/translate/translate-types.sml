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

    fun insertConst (env, dc, w, ty) = E.insertCon (env, dc, E.Const(w, ty))
    fun insertDCon (env, dc, repTr, dc') = E.insertCon (env, dc, E.DCon(dc', repTr))

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
			| NONE => 
			  (case tyc
			    of Ty.Tyc{def=Ty.AbsTyc, ...} => 
			       (* look for the concrete type of the constructor *)
			       (case MatchSig.realizationOfTyc tyc
				 of SOME (ModuleEnv.TyCon tyc) => trTyc(env, tyc)
				  | SOME (ModuleEnv.TyDef tys) => trScheme(env, tys)
				  | NONE => trTyc (env, tyc)				
			       (* end case *))
			     | _ => trTyc (env, tyc)
			  (* end case *))
		      (* end case *))
		  | Ty.FunTy(ty1, ty2) => BTy.T_Fun([tr' ty1], [BTy.exhTy], [tr' ty2])
		  | Ty.TupleTy [] => BTy.unitTy
		  | Ty.TupleTy tys => BTy.T_Tuple(false, List.map tr' tys)
		(* end case *))
	  in
	    tr' ty
	  end

    and trScheme (env, Ty.TyScheme(_, ty)) = tr (env, ty)

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
	      (* assign representations for the constructor functions *)
		val newDataCon = BTyc.newDataCon dataTyc
		fun mkDC (dc, rep, repTr, tys) = let
		      val dc' = newDataCon (DataCon.nameOf dc, rep, tys)
		      in
			insertDCon (env, dc, repTr, dc')
		      end
		fun mkDC' (dc, rep, (repTr, tys)) = mkDC (dc, rep, repTr, tys)
		fun mkTaggedDC (i, dc) = mkDC' (dc, BTy.TaggedTuple(Word.fromInt i), trArgTy(env, dc))
		in
		  case (consts, conFuns)
		   of (_::_, []) => setRep (BTy.T_Enum(Word.fromInt nConsts - 0w1), BTy.K_UNBOXED)
		    | ([], [dc]) => (case trArgTy(env, dc)
			 of (repTr, [ty]) => (
			      setRep (ty, BOMTyUtil.kindOf ty);
			      mkDC (dc, BTy.Transparent, repTr, [ty]))
			  | (repTr, tys) => (
			      setRep (BTy.T_Tuple(false, tys), BTy.K_BOXED);
			      mkDC (dc, BTy.Tuple, repTr, tys))
			(* end case *))
		    | (_, [dc]) => (
			case bomKindOfArgTy dc
			 of BTy.K_BOXED => (case trArgTy(env, dc)
			       of (repTr, [ty]) => mkDC (dc, BTy.Transparent, repTr, [ty])
				| (repTr, tys) => mkDC (dc, BTy.Tuple, repTr, tys)
			      (* end case *))
			  | _ => let (* need to use singleton tuple to represent data constructor *)
			      val argTy = tr (env, valOf(DataCon.argTypeOf dc))
			      in
				mkDC (dc, BTy.Tuple, FlattenRep.ATOM argTy, [argTy])
			      end
			(* end case *);
			setRep (BTy.T_Any, BTy.K_UNIFORM))
		    | ([], [dc1, dc2]) => (case (bomKindOfArgTy dc1, bomKindOfArgTy dc2)
			 of (BTy.K_BOXED, BTy.K_UNBOXED) => (
			      mkDC' (dc1, BTy.Tuple, trArgTy(env, dc1));
			      mkDC' (dc2, BTy.Transparent, trArgTy(env, dc2));
			      setRep (BTy.T_Any, BTy.K_UNIFORM))
			  | (BTy.K_UNBOXED, BTy.K_BOXED) => (
			      mkDC' (dc1, BTy.Transparent, trArgTy(env, dc1));
			      mkDC' (dc2, BTy.Tuple, trArgTy(env, dc2));
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

  (* translate the argument type of a data constructor; for tuples of two, or more,
   * components, we flatten the representation.
   *)
    and trArgTy (env, dc) = let
	  val rep = FlattenRep.flattenRep (tr (env, valOf (DataCon.argTypeOf dc)))
	  in
	    (rep, FlattenRep.dstTys rep)
	  end

    fun trDataCon (env, dc) = (case E.findDCon(env, dc)
	     of SOME dc' => dc'
	      | NONE => if Exn.isExn dc
		  then (case DataCon.argTypeOf dc
		     of NONE => let (* nullary exception constructor *)
			  val dc' = BTyc.newExnCon (DataCon.nameOf dc, [])
			  val result = E.ExnConst dc'
			  in
			    E.insertCon (env, dc, result);
			    result
			  end
		      | SOME ty => let
(* NOTE: we may want to use a flat representation for exn values! *)
			  val ty' = tr (env, ty)
			  val dc' = BTyc.newExnCon (DataCon.nameOf dc, [ty'])
			  val repTr = FlattenRep.TUPLE([ty'], [FlattenRep.ATOM ty'])
			  val result = E.DCon(dc', repTr)
			  in
			    E.insertCon (env, dc, result);
			    result
			  end
		    (* end case *))
		  else (
		    ignore (trTyc(env, DataCon.ownerOf dc));
		    valOf (E.findDCon(env, dc)))
	    (* end case *))

  end
