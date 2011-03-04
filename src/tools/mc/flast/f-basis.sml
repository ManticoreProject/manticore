(* f-basis.sml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Basis library for FLAST representation.
 *)

(* FIXME all elements here need to be included in the flatten env
 * before translation
 *)

structure FBasis : sig

(* type constructors *)
  val boolTyc : FTTypes.tycon
  val exnTyc : FTTypes.tycon
  val intTyc : FTTypes.tycon
  val longTyc : FTTypes.tycon
  val integerTyc : FTTypes.tycon
  val floatTyc : FTTypes.tycon
  val doubleTyc : FTTypes.tycon
  val charTyc : FTTypes.tycon
  val runeTyc : FTTypes.tycon
  val stringTyc : FTTypes.tycon
  val listTyc : FTTypes.tycon
  val parrayTyc : FTTypes.tycon
  val chanTyc : FTTypes.tycon
  val ivarTyc : FTTypes.tycon
  val mvarTyc : FTTypes.tycon
  val eventTyc : FTTypes.tycon
  val threadIdTyc : FTTypes.tycon

(* types *)
  val unitTy : FTTypes.ty
  val boolTy : FTTypes.ty
  val exnTy : FTTypes.ty
  val intTy : FTTypes.ty
  val floatTy : FTTypes.ty
  val stringTy : FTTypes.ty
  val listTy : FTTypes.ty -> FTTypes.ty
  val parrayTy : FTTypes.ty -> FTTypes.ty
  val threadIdTy : FTTypes.ty

(* constructors *)
  val boolTrue  : FTTypes.dcon
  val boolFalse : FTTypes.dcon
  val listNil   : FTTypes.dcon
  val listCons  : FTTypes.dcon
  val exnBind   : FTTypes.dcon
  val exnDiv    : FTTypes.dcon
  val exnFail   : FTTypes.dcon
  val exnMatch  : FTTypes.dcon

end = struct

  structure F = FLAST
  structure B = Basis
  structure N = BasisNames

  structure ATy = Types
  structure FTy = FTTypes

(* params: int -> ATy.tyvar list *)
(* utility for making list of fresh tyvars *)
(* ex: params 3 --> [ 'a, 'b, 'c ] *)
(* raises exn if you ask for more than 26 letters *)
  fun params n = let
    fun letter i = if i>25 then raise Fail "letter"
		   else Char.toString (chr (i + ord #"a"))
    fun mk i = TyVar.new (Atom.atom ("'" ^ letter i))
    in
      List.tabulate (n, mk)
    end

(* abs : ATy.tycon -> FTy.tycon *)
(* generate an FTy abs tycon from an ATy abs tycon *)
  fun abs (original: ATy.tycon) : FTy.tycon = let
    val name  = TyCon.nameOf original
    val arity = TyCon.arityOf original
    val eq    = TyCon.isEqTyc original
    in
      FTTyCon.newAbsTyc (name, arity, eq, original)
    end

(* data : ATy.tycon -> FTy.tycon *)
(* generate an FTy data tycon from an ATy data tycon *)
  fun data (original : ATy.tycon) : FTy.tycon = let
    val name  = TyCon.nameOf original
    val arity = TyCon.arityOf original
    val eq    = TyCon.isEqTyc original
    in
      FTTyCon.newDataTyc (name, params arity, original)
    end

(* tyc : ATy.tycon -> FTy.tycon *)
(* dispatching function *)
(* generates an FTy tycon from an ATy tycon *)
  fun tyc (original : ATy.tycon) : FTy.tycon = let
    val ATy.Tyc {def, ...} = original
    in
      case def
        of ATy.AbsTyc => abs original
	 | ATy.DataTyc _ => data original
    end 

(* paramsOf : ATy.tycon -> ATy.tyvar list *)
  fun paramsOf (ATy.Tyc {params, ...}) : ATy.tyvar list = params

(* type constructors *)
  val boolTyc = tyc B.boolTyc
  val exnTyc = tyc B.exnTyc
  val intTyc = tyc B.intTyc
  val longTyc = tyc B.longTyc
  val integerTyc = tyc B.integerTyc
  val floatTyc = tyc B.integerTyc
  val doubleTyc = tyc B.doubleTyc
  val charTyc = tyc B.charTyc
  val runeTyc = tyc B.runeTyc
  val stringTyc = tyc B.stringTyc
  val listTyc = tyc B.listTyc
  val parrayTyc = tyc B.parrayTyc
  val chanTyc = tyc B.chanTyc
  val ivarTyc = tyc B.ivarTyc
  val mvarTyc = tyc B.mvarTyc
  val eventTyc = tyc B.eventTyc
  val threadIdTyc = tyc B.threadIdTyc

(* types *)
  val unitTy = FTy.TupleTy (B.unitTy, [])
  val boolTy = FTy.ConTy ([], boolTyc)
  val exnTy  = FTy.ConTy ([], exnTyc)
  val intTy      = FLAST.ConTy ([], intTyc)
  val longTy = FTy.ConTy ([], longTyc)
  val integerTy = FTy.ConTy ([], integerTyc)
  val floatTy = FTy.ConTy ([], floatTyc)
  val doubleTy = FTy.ConTy ([], doubleTyc)
  val charTy = FTy.ConTy ([], charTyc)
  val runeTy = FTy.ConTy ([], runeTyc)
  val stringTy = FTy.ConTy ([], stringTyc)
  fun listTy t = FTy.ConTy ([t], listTyc)
  fun parrayTy t = FTy.ConTy ([t], parrayTyc)
  val threadIdTy = FTy.ConTy ([], threadIdTyc)

(* constructors *)
  val boolFalse = FTDataCon.new boolTyc (N.boolFalse, NONE, B.boolFalse)
  val boolTrue = FTDataCon.new boolTyc (N.boolTrue, NONE, B.boolTrue)

  local (* list constructors *)
    val listParams = paramsOf B.listTyc
    val alphaTy = (case listParams 
		     of [alpha] => FTy.VarTy alpha
		      | _ => raise Fail "alphaTy")
  in
    val listNil = FTDataCon.new listTyc (N.listNil, NONE, Basis.listNil)
    val listCons = let
      val interfaceTy = valOf (DataCon.argTypeOf B.listCons)
      val tupTy = FTy.TupleTy (interfaceTy, [alphaTy, FTy.ConTy ([alphaTy], listTyc)])
      in
        FTDataCon.new listTyc (N.listCons, SOME tupTy, B.listCons)
      end
  end (* local *)

(* exceptions *)
  local
    val dc = FTDataCon.new exnTyc
  in
    val exnBind  = dc (N.exnBind, NONE, B.exnBind)
    val exnDiv   = dc (N.exnDiv, NONE, B.exnDiv)
    val exnFail  = dc (N.exnFail, SOME stringTy, B.exnFail)
    val exnMatch = dc (N.exnMatch, NONE, B.exnMatch)
  end (* local *)
end
