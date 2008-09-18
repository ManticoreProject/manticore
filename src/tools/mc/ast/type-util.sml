(* type-util.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Based on CMSC 22610 Sample code (Winter 2007)
 *
 * Various utility functions for manipulating types.
 *)

structure TypeUtil : sig

  (* return the "head-normal form" by pruning an instantiated meta
   * variables.
   *)
    val prune : Types.ty -> Types.ty

  (* apply a type variable to type substitution to a type.  The substitution
   * is represented as a list of type variable/type pairs.
   *)
    val substitute : (Types.ty * (Types.tyvar * Types.ty) list) -> Types.ty

  (* instantiate a type scheme at the given lambda-nesting depth.  This function
   * returns the list of type arguments (i.e., fresh metavariables allocated
   * for the bound variables of the scheme) and the resulting type.
   *)
    val instantiate : (int * Types.ty_scheme) -> (Types.ty list * Types.ty)

  (* instantiate a type scheme to a specific list of types.  We assume that
   * the types have the correct kinds for the corresponding type variables.
   *)
    val apply : (Types.ty_scheme * Types.ty list) -> Types.ty

  (* close a type w.r.t. to a set of non-generic variables (i.e., those
   * variables whose depth is less than or equal to the given depth).
   *)
    val closeTy : (int * Types.ty) -> Types.ty_scheme

  (* replace type variables with fresh meta variables *)
    val openTy : (int * Types.ty) -> Types.ty

    val toMonoTy : Types.ty_scheme -> Types.ty

    val openTyScheme : (int * Types.ty_scheme) -> Types.ty

  (* return true if two types are equal.  Note that this function does not
   * do unification or alpha renaming of meta variables, but it does chase
   * instantiated meta-variable types.
   *)
    val same : (Types.ty * Types.ty) -> bool

  (* return true if the type supports equality *)
    val eqType : Types.ty -> bool

  (* convert various things to strings *)
    val tyvarToString : Types.tyvar -> string
    val fmt : {long : bool} -> Types.ty -> string
    val fmtMeta : {long : bool} -> Types.meta -> string
    val toString : Types.ty -> string
    val fmtScheme : {long : bool} -> Types.ty_scheme -> string
    val schemeToString : Types.ty_scheme -> string

  (* smart constructors *)
    val tupleTy : Types.ty list -> Types.ty
    val funTy : Types.ty list * Types.ty list -> Types.ty

    val rangeType : Types.ty -> Types.ty


  end = struct

    structure MV = MetaVar
    structure TVMap = TyVar.Map
    structure MVMap = MetaVar.Map
    structure Ty = Types

    fun tyvarToString (Ty.TVar{name, stamp, ...}) = Atom.toString name
						    (*^"<"^Stamp.toString stamp^">"*)

  (* return a string representation of a type (for debugging) *)
    fun fmt {long} = let
	  fun toS(Ty.ErrorTy) = "<error>"
	    | toS (Ty.MetaTy mv) = fmtMeta {long=long} mv
	    | toS (Ty.VarTy tv) = tyvarToString tv
	    | toS (Ty.ConTy([], tyc)) = TyCon.toString tyc
	    | toS (Ty.ConTy([ty], tyc)) = concat[
		  toS ty, " ", TyCon.toString tyc
		]
	    | toS (Ty.ConTy(tys, tyc)) = concat[
		  "(", String.concatWith "," (List.map toS tys), ")",
		  TyCon.toString tyc
		]
	    | toS (Ty.FunTy(ty1 as Ty.FunTy _, ty2)) =
		concat["(", toS ty1, ") -> ", toS ty2]
	    | toS (Ty.FunTy(ty1, ty2)) = concat[toS ty1, " -> ", toS ty2]
	    | toS (Ty.TupleTy []) = "unit"
	    | toS (Ty.TupleTy tys) = let
		fun toS' (ty as Ty.FunTy _) = concat["(", toS ty, ")"]
		  | toS' ty = toS ty
		in
		  concat["(", String.concatWith " * " (List.map toS' tys), ")"]
		end
	  in
	    toS
	  end

    and fmtMeta {long} (Ty.MVar{stamp, info}) = (case !info
	   of Ty.UNIV d => if long
		then concat["$", Stamp.toString stamp, "@", Int.toString d]
		else "$" ^ Stamp.toString stamp
	    | Ty.CLASS cls => concat["$", Stamp.toString stamp, "::", TypeClass.toString cls]
	    | Ty.INSTANCE ty => if long
		then (
		  info := Ty.UNIV(~1);
		  concat["($", Stamp.toString stamp, " == ", fmt {long=true} ty, ")"]
		    before info := Ty.INSTANCE ty)
		else (
		  info := Ty.UNIV(~1);
		  fmt {long=false} ty before info := Ty.INSTANCE ty)
	  (* end case *))

    val toString = fmt {long=false}

    fun fmtScheme {long} (Ty.TyScheme([], ty)) = fmt {long=long} ty
      | fmtScheme {long} (Ty.TyScheme(tvs, ty)) = concat[
	    "[", String.concatWith "," (List.map tyvarToString tvs), "]", fmt {long=long} ty
	  ]

  (* return the string representation of a type scheme *)
    val schemeToString = fmtScheme {long=false}

  (* return the "head-normal form" by pruning an instantiated meta
   * variables.
   *)
    fun prune (Ty.MetaTy(Ty.MVar{info as ref(Ty.INSTANCE ty), ...})) = let
	  val ty = prune ty
	  in
	    info := Ty.INSTANCE ty;	(* path compression *)
	    ty
	  end
      | prune ty = ty

  (* apply a type variable to type substitution to a type *)
    fun applySubst (subst, ty0) = let
	  fun inst ty = (case prune ty
		 of Ty.ErrorTy => ty
		  | ty as Ty.MetaTy _ => ty
		  | ty as Ty.VarTy tv => (case TVMap.find(subst, tv)
		       of NONE => ty
			| SOME ty => ty
		      (* end case *))
		  | Ty.ConTy(args, tyc) => Ty.ConTy(List.map inst args, tyc)
		  | Ty.FunTy(ty1, ty2) => Ty.FunTy(inst ty1, inst ty2)
		  | Ty.TupleTy tys => Ty.TupleTy(List.map inst tys)
		(* end case *))
	  in
	    inst ty0
	  end

  (* apply a type-variable-to-type substitution to a type.  The substitution
   * is represented as a list of type variable/type pairs.
   *)
    fun substitute (ty, []) = ty
      | substitute (ty, s) = applySubst (List.foldl TVMap.insert' TVMap.empty s, ty)

  (* instantiate a type scheme at the given lambda-nesting depth *)
    fun instantiate (_, Ty.TyScheme([], ty)) = ([], ty)
      | instantiate (depth, Ty.TyScheme(tvs, ty)) = let
	(* create a substitution from type variables to fresh meta variables *)
	  fun f (tv as AST.TVar{class, ...}, (s, mvs)) = (case class
		   of NONE => let
			val mv = Ty.MetaTy(MV.new depth)
			in
			  (TVMap.insert(s, tv, mv), mv :: mvs)
			end
		    | SOME cls => let
			val cmv = TypeClass.new cls
			in
			  (TVMap.insert(s, tv, cmv), cmv :: mvs)
			end
		  (* end case *))
	  val (subst, mvs) = List.foldr f (TVMap.empty, []) tvs
	  in
	    (mvs, applySubst (subst, ty))
	  end

  (* instantiate a type scheme to a specific list of types.  We assume that
   * the types have the correct kinds for the corresponding type variables.
   *)
    fun apply (Ty.TyScheme([], ty), []) = ty
      | apply (sigma as Ty.TyScheme(tvs, ty), tys) = let
	  fun ins (tv, ty, s) = TVMap.insert(s, tv, ty)
	  fun println s = (print s; print "\n")
          (* val _ = (println "=====> In TypeUtil.apply";
		   print "scheme: ";
		   println (schemeToString sigma);
		   print "types:  ";
		   println (String.concatWith "," (map toString tys));
		   print "\n") *)
	  in
	    applySubst (ListPair.foldlEq ins TVMap.empty (tvs, tys), ty)
	  end
handle ex => (print(concat["apply(", schemeToString(Ty.TyScheme(tvs, ty)), ", [",
String.concatWith "," (List.map toString tys), "])\n"]); raise ex)

  (* close a type w.r.t. to a set of non-generic variables (i.e., those
   * variables whose depth is less than or equal to the given depth).
   *)
    fun closeTy (depth, ty) = let
	  val count = ref 0
	(* generate a fresh type variable *)
	  fun newVar () = let
		val id = !count
		in
		  count := id+1;
		  TyVar.new(Atom.atom("'M" ^ Int.toString id))
		end
	  fun genVars (ty, env) = (case prune ty
		 of Ty.ErrorTy => (env, Ty.ErrorTy)
		  | ty as Ty.MetaTy(mv as Ty.MVar{info, ...}) => (case !info
		       of Ty.UNIV d => if (d > depth)
			    then (case MVMap.find(env, mv) (* generic variable *)
			       of SOME tv => (env, Ty.VarTy tv)
				| NONE => let
				    val tv = newVar()
				    in
				      (MVMap.insert(env, mv, tv), Ty.VarTy tv)
				    end
			      (* end case *))
			    else (env, ty) (* non-generic variable *)
			| Ty.CLASS _ => (env, ty)
			|_ => raise Fail "impossible"
		      (* end case *))
		  | Ty.VarTy _ =>
		    (* FIXME: is this rule correct? *)
		    (env, ty)
		  | Ty.ConTy(args, tyc) => let
		      val (env, tys) = genVarsForTys (args, env)
		      in
			(env, Ty.ConTy(tys, tyc))
		      end
		  | Ty.FunTy(ty1, ty2) => let
		      val (env, ty1) = genVars (ty1, env)
		      val (env, ty2) = genVars (ty2, env)
		      in
			(env, Ty.FunTy(ty1, ty2))
		      end
		  | Ty.TupleTy tys => let
		      val (env, tys) = genVarsForTys (tys, env)
		      in
			(env, Ty.TupleTy tys)
		      end
		(* end case *))
	  and genVarsForTys (tys, env) = let
		fun f (ty, (env, tys)) = let
			val (env', ty') = genVars(ty, env)
			in
			  (env', ty'::tys)
			end
		in
		  List.foldr f (env, []) tys
		end
	  val (tvs, ty) = genVars (ty, MetaVar.Map.empty)
	  in
	    Ty.TyScheme(MVMap.listItems tvs, ty)
	  end

  (* return true if two types are equal.  Note that this function does not
   * do unification or alpha renaming of meta variables, but it does chase
   * instantiated meta-variable types and allows ErrorTy to equal any type.
   *)
    fun same (ty1, ty2) = (case (prune ty1, prune ty2)
	   of (Ty.ErrorTy, _) => true
	    | (_, Ty.ErrorTy) => true
(* QUESTION: should two meta variables that are both of the same class be equal? *)
	    | (Ty.MetaTy mv1, Ty.MetaTy mv2) => MV.same(mv1, mv2)
	    | (Ty.VarTy tv1, Ty.VarTy tv2) => TyVar.same(tv1, tv2)
	    | (Ty.ConTy(args1, tyc1), Ty.ConTy(args2, tyc2)) =>
		TyCon.same(tyc1, tyc2) andalso ListPair.allEq same (args1, args2)
	    | (Ty.FunTy(ty11, ty12), Ty.FunTy(ty21, ty22)) =>
		same(ty11, ty21) andalso same(ty21, ty22)
	    | (Ty.TupleTy tys1, Ty.TupleTy tys2) =>
		ListPair.allEq same (tys1, tys2)
	    | _ => false
	  (* end case *))

(* QUESTION: do we really need both this function and TypeClass.isEqualityType? *)
  (* return true if the type supports equality *)
    fun eqType ty = (case prune ty
	   of Ty.ErrorTy => true
	    | Ty.MetaTy _ => false (* should have been resolved by now *)
	    | Ty.VarTy _ => false
	    | Ty.ConTy(_, tyc) => TyCon.isEqTyc tyc
	    | Ty.FunTy _ => false
	    | Ty.TupleTy tys => List.all eqType tys
	  (* end case *))

  (* convert various things to strings *)
  (* smart constructors *)
    fun tupleTy [ty] = ty
      | tupleTy tys = Types.TupleTy tys

    fun funTy (dom, rng) = Types.FunTy(tupleTy dom, tupleTy rng)

    fun rangeType (Types.FunTy (_, rng)) = rng
      | rangeType _ = raise Fail "rangeType"

  (* replace type variables with fresh meta variables *)
    fun openTy (depth, ty) = let
	val env = ref (TVMap.empty : Ty.meta TVMap.map)
	fun ins (v, mv) = env := TVMap.insert(!env, v, mv)
	fun find v = TVMap.find(!env, v)
	fun oTy ty = (
	    case ty
	     of Ty.MetaTy meta => Ty.MetaTy(oMeta meta)
	      | Ty.VarTy tv => (
		case find tv
		 of NONE => let
	              val meta = MetaVar.new depth
		      in
			ins(tv, meta);
			Ty.MetaTy meta
		      end
		  | SOME meta => Ty.MetaTy meta
		(* end case *))
	      | Ty.ConTy(tys, tycon) =>
		Ty.ConTy(List.map oTy tys, oTycon tycon)
	      | Ty.FunTy(ty1, ty2) => Ty.FunTy(oTy ty1, oTy ty2)
	      | Ty.TupleTy tys => Ty.TupleTy (List.map oTy tys)
  	    (* end case *))
	and oMeta (Ty.MVar{stamp, info}) = (
	    case !info
	     of Ty.INSTANCE ty => info := Ty.INSTANCE (oTy ty)
	      | _ => ();
	    Ty.MVar{stamp=stamp, info=info})
	and oTycon (Ty.Tyc{stamp, name, arity, params, props, def}) = (
	    Ty.Tyc{stamp=stamp, name=name, arity=arity, params=params, props=props, def=
	    case def
	     of Ty.AbsTyc => Ty.AbsTyc
	      | Ty.DataTyc {nCons, cons} => 
		Ty.DataTyc {nCons=nCons, cons=cons}
 	    (* end case *)})
	and oDCon (Ty.DCon{id, name, owner, argTy}) =
	    Ty.DCon{id=id, name=name, owner=owner, argTy=Option.map oTy argTy}
	in
	  oTy ty
	end

    fun openTyScheme (depth, Ty.TyScheme(_, ty)) = openTy(depth, ty)

    fun toMonoTy (Ty.TyScheme(_, ty)) = let
	  val Ty.TyScheme([], ty) = closeTy(0, ty)
          in
	    openTy(0, ty)
          end
	  
    
  end

