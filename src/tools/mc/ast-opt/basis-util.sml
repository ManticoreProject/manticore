(* basis-util.sml
 *
 * COPYRIGHT (c) 2010 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Various utility functions for working with basis values in generated code.
 *)

structure BasisUtil : sig
  
(* options *)
  val optTyc  : unit -> Types.tycon
  val optSOME : unit -> AST.dcon
  val optNONE : unit -> AST.dcon
  val optTy   : AST.ty -> AST.ty

(* ref cells *)
  val refTyc : unit -> Types.tycon
  val refNew : unit -> AST.var
  val refSet : unit -> AST.var
  val refGet : unit -> AST.var
  val refTy  : AST.ty -> AST.ty

(* results *)
  val resTyc : unit -> Types.tycon
  val resRES : unit -> AST.dcon
  val resEXN : unit -> AST.dcon
  val resTy  : AST.ty -> AST.ty

(* mvars *)
  val mvarTyc  : unit -> Types.tycon
  val mvarNew  : unit -> AST.var
  val mvarPut  : unit -> AST.var
  val mvarTake : unit -> AST.var
  val mvarTy   : AST.ty -> AST.ty    

(* arrays *)
  val arrayTyc    : unit -> Types.tycon
  val arrayArray  : unit -> AST.var
  val arrayLength : unit -> AST.var
  val arraySub    : unit -> AST.var
  val arrayUpdate : unit -> AST.var
  val arrayTy     : AST.ty -> AST.ty

(* cancellables *)
  val cancelNew    : unit -> AST.var
  val cancelCancel : unit -> AST.var
  val cancelSpawn  : unit -> AST.var
  val cancelTy     : unit -> AST.ty

(* bitvecs *)
  val bitvecNew   : unit -> AST.var
  val bitvecSet0  : unit -> AST.var
  val bitvecSet1  : unit -> AST.var
  val bitvecSet0F : unit -> AST.var
  val bitvecSet1F : unit -> AST.var
  val bitvecEq    : unit -> AST.var
  val bitvecTy    : unit -> AST.ty

end = struct

  structure A = AST
  structure B = Basis
  structure M = Memo

(* (local) utilities for making memo cells *)
  fun dcon path = M.new (fn _ => BasisEnv.getDConFromBasis path)
  fun tyc path  = M.new (fn _ => BasisEnv.getTyConFromBasis path)
  fun var path  = M.new (fn _ => BasisEnv.getVarFromBasis path)

(* applyTyc : (unit -> Types.tycon) -> (AST.ty -> AST.ty) *)
  fun applyTyc tycThunk = (fn ty => A.ConTy ([ty], tycThunk ()))

(* options *)
  val mOptTyc = tyc ["Option", "option"]
  val mSOME   = dcon ["Option", "NONE"]
  val mNONE   = dcon ["Option", "NONE"]

  fun optTyc ()  = M.get mOptTyc
  fun optSOME () = M.get mSOME
  fun optNONE () = M.get mNONE
  val optTy      = applyTyc optTyc

(* ref cells *)
  val mRefTyc = tyc ["Ref", "ref"]
  val mRefNew = var ["Ref", "new"]
  val mRefGet = var ["Ref", "get"]
  val mRefSet = var ["Ref", "set"]

  fun refTyc () = M.get mRefTyc
  fun refNew () = M.get mRefNew
  fun refGet () = M.get mRefGet
  fun refSet () = M.get mRefSet
  val refTy     = applyTyc refTyc

(* results *)
  val mResTyc = tyc ["Result", "result"]
  val mRES    = dcon ["Result", "RES"]
  val mEXN    = dcon ["Result", "EXN"]

  fun resTyc () = M.get mResTyc
  fun resRES () = M.get mRES
  fun resEXN () = M.get mEXN
  val resTy     = applyTyc resTyc
					   
(* mvars *)
  val mMVarTyc  = tyc ["MVar", "mvar"]
  val mMVarNew  = var ["MVar", "new"]
  val mMVarPut  = var ["MVar", "put"]
  val mMVarTake = var ["MVar", "take"]

  fun mvarTyc ()  = M.get mMVarTyc 
  fun mvarNew ()  = M.get mMVarNew
  fun mvarPut ()  = M.get mMVarPut
  fun mvarTake () = M.get mMVarTake
  val mvarTy      = applyTyc mvarTyc

(* arrays *)
  val mArrayTyc    = tyc ["Array64", "array"]
  val mArrayArray  = var ["Array64", "array"]
  val mArrayLength = var ["Array64", "length"]
  val mArraySub    = var ["Array64", "sub"]
  val mArrayUpdate = var ["Array64", "update"]

  fun arrayTyc ()    = M.get mArrayTyc
  fun arrayArray ()  = M.get mArrayArray
  fun arrayLength () = M.get mArrayLength
  fun arraySub ()    = M.get mArraySub
  fun arrayUpdate () = M.get mArrayUpdate
  val arrayTy        = applyTyc arrayTyc

(* cancellables *)
  val mCancelTyc    = tyc ["Cancelation", "cancelable"]
  val mCancelNew    = var ["Cancelation", "new"]
  val mCancelCancel = var ["Cancelation", "cancel"]
  val mCancelSpawn  = var ["Cancelation", "spawn"]

  fun cancelTyc ()    = M.get mCancelTyc
  fun cancelNew ()    = M.get mCancelNew
  fun cancelCancel () = M.get mCancelCancel
  fun cancelSpawn ()  = M.get mCancelSpawn
  fun cancelTy ()     = A.ConTy ([], cancelTyc ())

(* bitvecs *)
  val mBitvecTyc   = tyc ["BitVec", "bitvec"]
  val mBitvecNew   = var ["BitVec", "new"]
  val mBitvecSet0  = var ["BitVec", "set0"]
  val mBitvecSet1  = var ["BitVec", "set1"]
  val mBitvecSet0F = var ["BitVec", "set0F"]
  val mBitvecSet1F = var ["BitVec", "set1F"]
  val mBitvecEq    = var ["BitVec", "eq"]

  fun bitvecNew ()   = M.get mBitvecNew
  fun bitvecSet0 ()  = M.get mBitvecSet0
  fun bitvecSet1 ()  = M.get mBitvecSet1
  fun bitvecSet0F () = M.get mBitvecSet0F
  fun bitvecSet1F () = M.get mBitvecSet1F
  fun bitvecEq ()    = M.get mBitvecEq 
  fun bitvecTy ()    = A.ConTy ([], M.get mBitvecTyc)

end
