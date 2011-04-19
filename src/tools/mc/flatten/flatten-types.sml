(* flatten-types.sml
 *
 * COPYRIGHT (c) 2010 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Flatten AST types.
 *
 *)

structure FlattenTypes (* : sig

  val flattenTy       : FlattenEnv.env -> Types.ty -> Types.ty
  val flattenTyScheme : FlattenEnv.env -> Types.ty_scheme -> Types.ty_scheme

end *) = struct

  structure T = Types 
  structure B = Basis

  structure TU = TypeUtil

  structure FEnv = FlattenEnv

  fun assert msg fact = 
    if not fact then raise Fail ("assertion failure: " ^ msg) else ()

  fun basisTyc c = List.exists (fn d => TyCon.same (c, d)) B.primTycs

(* nullary basis tycons are considered ground types *)
  fun isGround (T.ConTy ([], c)) = basisTyc c
    | isGround _ = false

  fun isPArrayTyc c = TyCon.same (B.parrayTyc, c)

(* mustFlattenTyc : env * tyc -> bool *)
(* side effect: all decisions about flattening are recorded in the env *)
  fun mustFlattenTyc (env, tyc) : bool = let
    fun mark bool = FEnv.markFlattenTyc (env, tyc, bool)
    fun decide bool = (mark bool; bool)
    in (case FEnv.findMustFlatten (env, tyc)
      of SOME decision => decision
       | NONE => let
           val _ = () (* print ("~~~~~ inspecting " ^ TyCon.toString tyc ^ "\n") *)
           val (T.Tyc {def, ...}) = tyc
           in (case def 
             of T.AbsTyc => decide false
	      | T.DataTyc {cons, ...} => let
                  val cs = !cons
		  fun dcon (T.DCon {argTy, ...}) = (case argTy
	            of NONE => false
		     | SOME t => (mark false; mustFlattenTy (env, t))
                    (* end case *))
                  in
		    decide (List.exists dcon cs)
	          end	
	      (* end case *))
	     end
       (* end case *))
    end      

(* ty : env * ty -> bool *)
(* side effect: decisions about flattening are marked in env *)
  and mustFlattenTy (env, t) = let
    fun ty t = (case t
      of T.ErrorTy => false
       | T.MetaTy (T.MVar {info, ...}) => (case !info
           of T.INSTANCE t => ty t
	    | _ => false
           (* end case *))
       | T.VarTy _ => false
       | T.ConTy (ts, c) => 
           isPArrayTyc c orelse mustFlattenTyc (env, c) orelse List.exists ty ts
       | T.FunTy (t1, t2) => ty t1 orelse ty t2
       | T.TupleTy ts => List.exists ty ts
       | T.FArrayTy _ => raise Fail "mustFlatten: shouldn't happen"
      (* end case *))
    in
      ty t
    end

  fun flattenTy (env : FEnv.env) (t : T.ty) : T.ty = let
    fun ty T.ErrorTy = T.ErrorTy
      | ty (meta as T.MetaTy m) = (case m
          of T.MVar {info=info as ref(T.INSTANCE t), ...} => let
               val t' = flattenTy env t
               in
                 info := T.INSTANCE t';
		 meta
	       end
	   | _ => meta
	  (* end case *))
      | ty (T.VarTy a) = T.VarTy a
      | ty (T.FunTy (t1, t2)) = T.FunTy (ty t1, ty t2)
      | ty (T.TupleTy ts) = T.TupleTy (List.map ty ts)
      | ty (f as T.FArrayTy (t, n)) = 
          (print ("***** asked to flatten " ^ TypeUtil.toString f ^ "\n");
	   raise Fail "TODO" (* not sure this circ should arise *))
      | ty (T.ConTy (ts, c)) = conTy (ts, c)
    and conTy (ts, c) = 
	 (if isPArrayTyc c then (case ts 
            of [] => raise Fail "conTy: parray tyc has no type args"
	     | [t] => if isGround t then 
		        T.FArrayTy (t, T.LfTy)
		      else (case TypeUtil.prune t
		        of T.FunTy (dom, rng) => let
			     val t' = T.FunTy (ty dom, ty rng)
			     in
			       T.FArrayTy (t', T.LfTy)
			     end
			 | T.TupleTy [] (* unit *) =>
			     T.FArrayTy (T.TupleTy [], T.LfTy)
			 | T.TupleTy ts => let
                             val ts' = List.map (ty o B.parrayTy) ts
			     in
			       T.TupleTy ts'
		             end
			 | T.ConTy (ts', c') =>
			    (if isPArrayTyc c' 
			     then operN (ty t) 
			     else let
		               val t' = flattenTy env t
                               in
                                 T.FArrayTy (t', T.LfTy)
                               end)
			 | T.VarTy a => (* FIXME not sure this works*) 
			                (* raise Fail ("tyvar: parray of " ^ TU.toString t) *)
                             T.FArrayTy (T.VarTy a, T.LfTy)
			 | mty as T.MetaTy m => let
			     val msg = "unresolved overloading: " ^ TU.toString mty
			     in
			       raise Fail msg
			     end
 (* (case m  *)
 (* 			     of T.MVar {info=ref(T.UNIV n), ...} => mty *)
 (* 			      | _ => let *)
 (* 			          val msg = "unresolved overloading: " ^ TU.toString mty *)
 (* 				  in *)
 (* 				    raise Fail msg *)
 (* 				  end *)
 (* 			     (\* end case *\)) *)
			 | _ => raise Fail ("?: parray of " ^ TU.toString t)
		       (* end case *))
	     | ts => raise Fail "conTy: parray tyc has too many type args"    
	   (* end case *))
	  else (* not a parray *) (case FEnv.findTyc (env, c)
            of NONE => 
                 if mustFlattenTyc (env, c) then let
                   val c' = newFlatTyc env c
                   in
		     T.ConTy (List.map ty ts, c')
	           end
		 else
		   T.ConTy (List.map ty ts, c)
	     | SOME c' => T.ConTy (List.map ty ts, c')
           (* end case *))
	 (* end if *))
    and operN t =
         (case TypeUtil.prune t
	   of T.FArrayTy (t, n) => T.FArrayTy (t, T.NdTy n)
	    | T.TupleTy ts => T.TupleTy (List.map operN ts)
	    | _ => raise Fail ("operN: " ^ TU.toString t)
	  (* end case *)) 
    in
      ty t
    end

(* newFlatTyc : env -> tyc -> tyc *)
(* side effect: new tyc is inserted into the env *)
(* side effect: new dcons of that tyc are inserted into the env *)
  and newFlatTyc (env : FEnv.env) (tyc : T.tycon) : T.tycon = let
	val _ = assert "data tyc" (TU.isDataTyc tyc)
        fun underscore a = Atom.atom (Atom.toString a ^ "_")
        val (T.Tyc {name, params, def, ...}) = tyc
        val cons = (case def
	  of T.DataTyc {cons, ...} => !cons
	   | _ => raise Fail "newFlatTyc"
          (* end case *))
	val name' = underscore name     
        val tyc' = TyCon.newDataTyc (name', params) (* FIXME OK to reuse params? *)
        fun dcon d = let
          val name = DataCon.nameOf d
	  val name' = Atom.atom (name ^ "_")
	  val argTy = DataCon.argTypeOf d
	  val argTy' = Option.map (flattenTy env) argTy
	  val d' = DataCon.new tyc' (name', argTy') 
            (* note: this registers d' with tyc' as a side effect *)
          in
            FEnv.insertDCon (env, d, d')
	  end
        in
          print ("%%%%% flattening " ^ Atom.toString name ^ " to " ^ Atom.toString name' ^ "\n");
          FEnv.insertTyc (env, tyc, tyc'); (* insert new tyc into env *)
          List.app dcon cons; (* insert new dcons into env *)
	  tyc'
        end

(* mustFlattenTyScheme : env * ty_scheme -> bool *)
(* side effect: all decisions about flattening are recorded in the env *)
  fun mustFlattenTyScheme (env, T.TyScheme (_, t)) = mustFlattenTy (env, t)

  fun flattenTyScheme (env : FEnv.env) (T.TyScheme (vs, t)) = 
    T.TyScheme (vs, flattenTy env t)

end
