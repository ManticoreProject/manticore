(* prim-flatten-env.sml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

(* TODO primitive opers. right now, this is just tycs, tys, dcons *)

structure PrimFlattenEnv : sig

  val primFlattenEnv : FlattenEnv.env

end = struct

  structure A = AST
  structure B = Basis
  structure F = FLAST
  structure FB = FBasis

  structure ATy = Types
  structure FTy = FTTypes

  structure FE = FlattenEnv

  val env0 = FE.mkEnv ()
  
  val tycs = [FB.boolTyc,
	      FB.exnTyc,
	      FB.intTyc,
	      FB.longTyc,
	      FB.integerTyc,
	      FB.floatTyc,
	      FB.doubleTyc,
	      FB.charTyc,
	      FB.runeTyc,
	      FB.stringTyc,
	      FB.listTyc,
	      FB.parrayTyc,
	      FB.chanTyc,
	      FB.ivarTyc,
	      FB.mvarTyc,
	      FB.eventTyc,
	      FB.threadIdTyc]

  val cons = [FB.boolTrue,
	      FB.boolFalse,
	      FB.listNil,
	      FB.listCons,
	      FB.exnBind,
	      FB.exnFail,
	      FB.exnDiv,
	      FB.exnMatch]
	      
(* populate environment with basis tycons *)	      
  local 
    fun pop (fTyc : FTy.tycon) : unit = let
      val astTyc = FTTyCon.interfaceOf fTyc
      in
        FE.insertTyc (env0, astTyc, fTyc)
      end
  in
    val _ = List.app pop tycs
  end (* local *)

(* populate environment with basis data cons *)
  local
    fun pop (fCon : FTy.dcon) : unit = let
      val astCon = FTDataCon.interfaceOf fCon
      in
        FE.insertCon (env0, astCon, fCon)
      end
  in
    val _ = List.app pop cons
  end (* local *)

  local (* spot checks *)
    val _ = isSome (FE.findTyc (env0, B.threadIdTyc))
    val _ = isSome (FE.findDCon (env0, B.exnBind))
  in
    val primFlattenEnv = env0
  end (* local *)

end	     
	 
