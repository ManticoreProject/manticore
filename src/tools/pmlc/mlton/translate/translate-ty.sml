(* translate-ty.sml
 *
 * COPYRIGHT (c) 2013 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

(* WARNING: we need to make sure that all of the ML datatype tycons have been mapped
 * to BOM types before we try to translate any ML types.
 *)

structure TranslateTy : sig

    val translate : TranslateEnv.env -> Sxml.Type.t -> BOM.ty

  end = struct

    structure S = PMLFrontEnd.Sxml
    structure Tyc = PMLFrontEnd.Tycon
    structure Env = TranslateEnv
    structure PL = PMLFrontEnd.PropertyList

  (* properties for caching the result of type translations *)
    val {get, set, ...} =
	  Property.destGetSet (S.Type.plist, Property.initConst NONE)

    fun exhTy () = BOMTy.T_Cont[valOf(get S.Type.exn)]

    fun translate env = let
	  val exhTy = 
	  fun tr ty = (case get ty
		 of SOME ty' => ty'
		  | NONE => (case S.Type.dest ty
		       of S.Type.Var _ => raise Fail "impossible type variable"
			| S.Type.Con(tyc, args) => let
			    val args' = Vector.foldr (fn (ty, tys) => tr ty :: tys) [] args
			    val ty' = if Tyc.equals(tyc, Tyc.arrow)
				    then let (* function type *)
				      val [domTy, rngTy] = args'
				      in
					BOMTy.T_Fun([domTy], [exhTy ()], [rngTy])
				      end
				  else if Tyc.equals(tyc, Tyc.tuple)
				    then BOMTy.T_Tuple args'
(* possible other special tycs: ref, array, vector, exn, bool, list *)
				    else (* lookup the tyc in the environment *)
			    in
			      set (ty, SOME ty');
			      ty'
			    end
		      (* end case *))
		(* end case *))
	  in
	    tr
	  end

  end
