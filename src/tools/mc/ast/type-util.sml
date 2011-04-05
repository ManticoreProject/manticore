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

    val deepPrune : Types.ty -> Types.ty

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

  (* compare : useful for building ORD_x collections of types *)
    val compare : Types.ty * Types.ty -> order

  (* return true if the type supports equality *)
    val eqType : Types.ty -> bool

  (* properties of tycons *)
    val isAbsTyc  : Types.tycon -> bool
    val isDataTyc : Types.tycon -> bool
    val isNullary : Types.tycon -> bool

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

    val domainType : Types.ty -> Types.ty
    val rangeType  : Types.ty -> Types.ty

  (* nesting tree utils *)
    val deeperNTree : Types.nt_ty * Types.nt_ty -> bool
    val maxNTree : Types.nt_ty list -> Types.nt_ty

  end = struct

    structure MV = MetaVar
    structure TVMap = TyVar.Map
    structure MVMap = MetaVar.Map
    structure Ty = Types

    fun tyvarToString (Ty.TVar{name, stamp, ...}) = Atom.toString name

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
	    | toS (Ty.FArrayTy (t, n)) = let
                fun ntree Ty.LfTy = "lf"
		  | ntree (Ty.NdTy n) = "nd(" ^ ntree n ^ ")"
	        in
		  concat["{", toS t, ";", ntree n, "}"]
		end
	  in
	    toS
	  end

    and fmtMeta {long} (Ty.MVar{stamp, info}) = (case !info
	   of Ty.UNIV d => if long
		then concat["UNIV$", Stamp.toString stamp, "@", Int.toString d]
		else "UNIV$" ^ Stamp.toString stamp
	    | Ty.CLASS cls => concat["CLASS$", Stamp.toString stamp, "::", TypeClass.toString cls]
	    | Ty.INSTANCE ty => if long
		then (
		  info := Ty.UNIV(~1);
		  concat["(", Stamp.toString stamp, " == ", fmt {long=true} ty, ")"]
		    before info := Ty.INSTANCE ty)
		else (
		  info := Ty.UNIV(~1);
		  "I." ^ (fmt {long=false} ty)
                    before info := Ty.INSTANCE ty)
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

  (* prune all the way through *)
    fun deepPrune (t : Ty.ty) : Ty.ty = let
      fun p (Ty.ErrorTy) = Ty.ErrorTy
	| p (Ty.MetaTy (Ty.MVar {info as ref (Ty.INSTANCE ty), ...})) = let
            val ty' = p ty
            in
              info := Ty.INSTANCE ty';
	      ty'
	    end
	| p (tv as Ty.VarTy _) = tv
	| p (Ty.ConTy (ts, c)) = Ty.ConTy (List.map p ts, c)
	| p (Ty.FunTy (t1, t2)) = Ty.FunTy (p t1, p t2)
	| p (Ty.TupleTy ts) = Ty.TupleTy (List.map p ts)
	| p (Ty.FArrayTy (t, n)) = Ty.FArrayTy (p t, n)
      in
	p t
      end
      
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
		  | Ty.FArrayTy(ty, n) => Ty.FArrayTy(inst ty, n)
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
handle ex => let
  fun prcat ss = (print (concat ss); print "\n")
  val sch' = schemeToString (Ty.TyScheme (tvs, ty))
  val tys' = "[" ^ String.concatWith "," (List.map toString tys) ^ "]"
  in
    prcat ["exception in TypeUtil.apply"];
    prcat ["length tvs: ", Int.toString (List.length tvs)];
    prcat ["length tys: ", Int.toString (List.length tys)];
    prcat ["apply(", sch', ", " , tys', ")"];
    raise ex
  end

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
		  | Ty.FArrayTy (ty, n) => let
                      val (env, ty) = genVars (ty, env)
                      in
			(env, Ty.FArrayTy (ty, n))
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
	    | (Ty.FArrayTy (t1, n1), Ty.FArrayTy (t2, n2)) =>
                same(t1,t2) andalso sameNTree(n1,n2)
	    | _ => false
	  (* end case *))
    and sameNTree (Ty.LfTy, Ty.LfTy) = true
      | sameNTree (Ty.NdTy n1, Ty.NdTy n2) = sameNTree (n1, n2)
      | sameNTree _ = false

(* QUESTION: do we really need both this function and TypeClass.isEqualityType? *)
  (* return true if the type supports equality *)
    fun eqType ty = (case prune ty
	   of Ty.ErrorTy => true
	    | Ty.MetaTy _ => false (* should have been resolved by now *)
	    | Ty.VarTy _ => false
	    | Ty.ConTy(_, tyc) => TyCon.isEqTyc tyc
	    | Ty.FunTy _ => false
	    | Ty.TupleTy tys => List.all eqType tys
	    | Ty.FArrayTy(ty, n) => eqType ty
	  (* end case *))

  (* test if a tycon is abstract *)
    fun isAbsTyc (Ty.Tyc {def=Ty.AbsTyc, ...}) = true
      | isAbsTyc _ = false

  (* test if a tycon is a data tycon (not abstract) *)
    val isDataTyc = not o isAbsTyc 

  (* test if a tycon is nullary *)
    fun isNullary (Ty.Tyc {arity, ...}) = (arity = 0)

  (* convert various things to strings *)
  (* smart constructors *)
    fun tupleTy [ty] = ty
      | tupleTy tys = Types.TupleTy tys

    fun funTy (dom, rng) = Types.FunTy(tupleTy dom, tupleTy rng)

    fun domainType (Types.FunTy (dom, _)) = dom
      | domainType _ = raise Fail "domainType"

    fun rangeType (Types.FunTy (_, rng)) = rng
      | rangeType _ = raise Fail "rangeType"

  (* replace type variables with fresh meta variables *)
    fun openTy (depth, ty) = let
	val env = ref (TVMap.empty : Ty.meta TVMap.map)
	fun ins (v, mv) = env := TVMap.insert(!env, v, mv)
	fun find v = TVMap.find(!env, v)
	fun oTy ty = (case ty
	     of Ty.MetaTy meta => Ty.MetaTy(oMeta meta)
	      | Ty.VarTy tv => (case find tv
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
	      | Ty.ErrorTy => Ty.ErrorTy
	      | Ty.FArrayTy (ty, n) => Ty.FArrayTy (oTy ty, n)
  	    (* end case *))
	and oMeta (Ty.MVar{stamp, info}) = (case !info
	     of Ty.INSTANCE ty => info := Ty.INSTANCE (oTy ty)
	      | _ => ();
	    Ty.MVar{stamp=stamp, info=info})
	and oTycon (Ty.Tyc{stamp, name, arity, params, props, def}) = (
	    Ty.Tyc{
		stamp=stamp, name=name, arity=arity, params=params, props=props,
		def = (case def
		   of Ty.AbsTyc => Ty.AbsTyc
		    | Ty.DataTyc {nCons, cons} => 
		      Ty.DataTyc {nCons=nCons, cons=cons}
		  (* end case *))
	      })
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

  (* deeperNTree : ntree * ntree -> bool *)
  (* compare two nesting trees by depth *)
    fun deeperNTree (n1, n2) : bool = let
      fun dep (Ty.LfTy) = 0
	| dep (Ty.NdTy n) = 1 + dep n
      in
        dep n1 > dep n2
      end

  (* maxNTree : ntree list -> ntree *)
  (* return deepest ntree in the bunch *)
  (* raise Fail on empty list arg *)
    fun maxNTree [] = raise Fail "empty"
      | maxNTree (n::ns) = let
          fun deeper (n1, n2) = if deeperNTree (n1, n2) then n1 else n2
          in
            List.foldl deeper n ns
          end

  (* compare : ty * ty -> order *)
  (* The ground terms in types are meta types, tyvars, tycons and nt_tys. *)
  (* The first three of these have unique stamps. *)
  (* The latter has a natural mapping onto the natural numbers. *)
  (* All these can be used as a basis for comparison. *)
  (* Last but not least, an arbitrary order is chosen for the variants of ty. *)
  (* The rest follows. *)
    fun compare (t1 : Ty.ty, t2 : Ty.ty) : order = let
      fun consIndex t = (case t
        of Ty.ErrorTy    => 0
	 | Ty.MetaTy _   => 1
	 | Ty.VarTy _    => 2
	 | Ty.ConTy _    => 3
	 | Ty.FunTy _    => 4
	 | Ty.TupleTy _  => 5
	 | Ty.FArrayTy _ => 6
        (* end case *))
      fun ntreeInt (Ty.LfTy) = 0
	| ntreeInt (Ty.NdTy n) = 1 + ntreeInt n
      fun meta (Ty.MVar {stamp=s1, ...}, Ty.MVar {stamp=s2, ...}) =
        Stamp.compare (s1, s2)
      fun cmp (t1, t2) = let
        val i1 = consIndex t1
	val i2 = consIndex t2
        in
          if i1 <> i2 then 
	    Int.compare (i1, i2)  
          else (* the two constructors are the same *) case (t1, t2)
            of (Ty.ErrorTy, _) => EQUAL
	     | (Ty.MetaTy m1, Ty.MetaTy m2) => meta (m1, m2)
	     | (Ty.VarTy a, Ty.VarTy b) => TyVar.compare (a, b)
	     | (Ty.ConTy (ts1, c1), Ty.ConTy (ts2, c2)) => 
	        (case TyCon.compare (c1, c2)
		  of EQUAL => tys (ts1, ts2)
		   | neq => neq)
	     | (Ty.FunTy (d1, r1), Ty.FunTy (d2, r2)) => 
                (case cmp (d1, d2)
		  of EQUAL => cmp (r1, r2)
		   | neq => neq)
	     | (Ty.TupleTy ts1, Ty.TupleTy ts2) => tys (ts1, ts2)
	     | (Ty.FArrayTy (t1, n1), Ty.FArrayTy (t2, n2)) => 
                (case cmp (t1, t2)
		  of EQUAL => Int.compare (ntreeInt n1, ntreeInt n2)
		   | neq => neq)
	     | _ => raise Fail "BUG!" (* shouldn't happen *)
        end
      and tys (ts1, ts2) = let
        fun lp ([], []) = EQUAL
	  | lp ([], t::_) = LESS
	  | lp (t::_, []) = GREATER
          | lp (t1::ts1, t2::ts2) = (case cmp (t1, t2)
              of EQUAL => lp (ts1, ts2)
	       | neq => neq
	      (* end case *))
        in
	  lp (ts1, ts2)
        end
      in
	cmp (t1, t2)
      end
            
  end

