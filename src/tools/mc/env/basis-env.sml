(* basis-env.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)


structure BasisEnv : sig

    val lookupOp : Atom.atom -> Env.val_bind

    val te0 : Env.ty_env
    val ve0 : Env.var_env

(* FIXME: this operation shouldn't be exported, but the typechecker deals with
 * it in an ad hoc manner (unlike the other overloaded operators).
 *)
  (* overloaded unary operators *)
    val neg : (Types.ty_scheme * AST.var list)

  end = struct

    structure U = BasisUtils
    structure N = BasisNames

    nonfix div mod

    open Basis

(* TODO do @, ! *)

    local
      val --> = AST.FunTy
      fun ** (t1, t2) = AST.TupleTy[t1, t2]
      infix 9 **
      infixr 8 -->

      val forall = U.forall
      val forallMulti = U.forallMulti
      val monoVar = U.monoVar
      val polyVar = U.polyVar
      val polyVarMulti = U.polyVarMulti

      fun monoVar' (at, ty) = monoVar (Atom.toString at, ty)
      fun polyVar' (at, mkTy) = polyVar (Atom.toString at, mkTy)
      fun polyVarMulti' (at, n, mkTy) = polyVarMulti (Atom.toString at, n, mkTy)

    in

  (* create a type scheme that binds a kinded type variable *)
    fun tyScheme (cls, mk) = let
	  val tv = TyVar.newClass (Atom.atom "'a", cls)
	  in
	    Types.TyScheme([tv], mk(Types.VarTy tv))
	  end

    fun eqTyScheme () = tyScheme(Types.Eq, fn tv => (tv ** tv --> boolTy))
    val eq = Var.newPoly(Atom.toString N.eq, eqTyScheme())
    val neq = Var.newPoly(Atom.toString N.neq, eqTyScheme())

    val lte = (tyScheme(Types.Order, fn tv => (tv ** tv --> boolTy)),
	       [int_lte, long_lte, integer_lte, float_lte, double_lte, char_lte, rune_lte, string_lte])
    val lt = (tyScheme(Types.Order, fn tv => (tv ** tv --> boolTy)),
	       [int_lt, long_lt, integer_lt, float_lt, double_lt, char_lt, rune_lt, string_lt])
    val gte = (tyScheme(Types.Order, fn tv => (tv ** tv --> boolTy)),
	       [int_gte, long_gte, integer_gte, float_gte, double_gte, char_gte, rune_gte, string_gte])
    val gt = (tyScheme(Types.Order, fn tv => (tv ** tv --> boolTy)),
	       [int_gt, long_gt, integer_gt, float_gt, double_gt, char_gt, rune_gt, string_gt])

    val plus = (tyScheme(Types.Num, fn tv => (tv ** tv --> tv)),
	       [int_plus, long_plus, integer_plus, float_plus, double_plus])
    val minus = (tyScheme(Types.Num, fn tv => (tv ** tv --> tv)),
	       [int_minus, long_minus, integer_minus, float_minus, double_minus])
    val times = (tyScheme(Types.Num, fn tv => (tv ** tv --> tv)),
	       [int_times, long_times, integer_times, float_times, double_times])

    val fdiv = (tyScheme(Types.Float, fn tv => (tv ** tv --> tv)),
		[float_fdiv, double_fdiv])

    val div = (tyScheme(Types.Int, fn tv => (tv ** tv --> tv)),
	       [int_div, long_div, integer_div])
    val mod = (tyScheme(Types.Int, fn tv => (tv ** tv --> tv)),
	       [int_mod, long_mod, integer_mod])

    val neg = (tyScheme(Types.Num, fn tv => (tv --> tv)),
		[int_neg, long_neg, integer_neg, float_neg, double_neg])

    val lookupOp = let
	  val tbl = AtomTable.mkTable (16, Fail "lookupOp")
	  fun ins (id, info) = AtomTable.insert tbl (id, Env.Overload info)
	  in
	  (* insert constructors *)
	    List.app (AtomTable.insert tbl) [
		(N.listCons,	Env.Con listCons)
	      ];
	  (* insert non-overloaded operators *)
	    List.app (AtomTable.insert tbl) [
		(N.append,	Env.Var listAppend),
		(N.concat,	Env.Var stringConcat),
                (N.psub,        Env.Var psub)
	      ];
	  (* insert equality operators *)
	    List.app (AtomTable.insert tbl) [
		(N.eq,		Env.EqOp eq),
		(N.neq,		Env.EqOp neq)
	      ];
	  (* insert overloaded operators *)
	    List.app ins [
		(N.lte,		lte),
		(N.lt,		lt),
		(N.gte,		gte),
		(N.gt,		gt),
		(N.plus,	plus),
		(N.minus,	minus),
		(N.times,	times),
		(N.fdiv,	fdiv),
		(N.div,		div),
		(N.mod,		mod)
	      ];
	    AtomTable.lookup tbl
	  end


  (* the predefined type environment *)
    val te0 = Env.fromList [
	    (N.unit,		Env.TyDef(Types.TyScheme([], unitTy))),
	    (N.bool,		Env.TyCon boolTyc),
	    (N.int,		Env.TyCon intTyc),
	    (N.long,		Env.TyCon longTyc),
	    (N.integer,		Env.TyCon integerTyc),
	    (N.float,		Env.TyCon floatTyc),
	    (N.double,		Env.TyCon doubleTyc),
	    (N.char,		Env.TyCon charTyc),
	    (N.rune,		Env.TyCon runeTyc),
	    (N.string,		Env.TyCon stringTyc),
	    (N.list,		Env.TyCon listTyc),
	    (N.option,		Env.TyCon optionTyc),
	    (N.thread_id,	Env.TyCon threadIdTyc),
	    (N.parray,		Env.TyCon parrayTyc),
	    (N.chan,		Env.TyCon chanTyc),
	    (N.ivar,		Env.TyCon ivarTyc),
	    (N.mvar,		Env.TyCon mvarTyc),
	    (N.event,		Env.TyCon eventTyc),
	  (* extras *)
	    (N.image,		Env.TyCon imageTyc),
	  (* arrays *)
	    (N.arrayTyc,        Env.TyCon arrayTyc)
	  ]

    val ve0 = Env.fromList [
	    (N.boolTrue,	Env.Con boolTrue),
	    (N.boolFalse,	Env.Con boolFalse),
	    (N.listNil,		Env.Con listNil),
	    (N.listCons,	Env.Con listCons),
	    (N.optionNONE,	Env.Con optionNONE),
	    (N.optionSOME,	Env.Con optionSOME),
	    (N.exnBind,		Env.Con exnBind),
	    (N.exnDiv,		Env.Con exnDiv),
	    (N.exnFail,		Env.Con exnFail),
	    (N.exnMatch,	Env.Con exnMatch),
	    (* Unary minus is overloaded, so it's being handled
             * specially by the typechecker *)
	    (N.not,		Env.Var not),
	    (N.sqrtf,		Env.Var sqrtf),
	    (N.absf,		Env.Var absf),
	    (N.lnf,		Env.Var lnf),
	    (N.log2f,		Env.Var log2f),
	    (N.log10f,		Env.Var log10f),
	    (N.powf,		Env.Var powf),
	    (N.expf,		Env.Var expf),
	    (N.sinf,		Env.Var sinf),
	    (N.cosf,		Env.Var cosf),
	    (N.tanf,		Env.Var tanf),
	    (N.itof,		Env.Var itof),
	    (N.sqrtd,		Env.Var sqrtd),
	    (N.absd,		Env.Var absd),
	    (N.lnd,		Env.Var lnd),
	    (N.log2d,		Env.Var log2d),
	    (N.log10d,		Env.Var log10d),
	    (N.powd,		Env.Var powd),
	    (N.expd,		Env.Var expd),
	    (N.sind,		Env.Var sind),
	    (N.cosd,		Env.Var cosd),
	    (N.tand,		Env.Var tand),
	    (N.itod,		Env.Var itod),
	    (N.channel,		Env.Var channel),
	    (N.send,		Env.Var send),
	    (N.sendEvt,		Env.Var sendEvt),
	    (N.recv,		Env.Var recv),
	    (N.recvEvt,		Env.Var recvEvt),
	    (N.wrap,		Env.Var wrap),
	    (N.choose,		Env.Var choose),
	    (N.never,		Env.Var never),
	    (N.sync,		Env.Var sync),
	    (N.iVar,		Env.Var iVar),
	    (N.iGet,		Env.Var iGet),
	    (N.iPut,		Env.Var iPut),
	    (N.mVar,		Env.Var mVar),
	    (N.mGet,		Env.Var mGet),
	    (N.mTake,		Env.Var mTake),
	    (N.mPut,		Env.Var mPut),
	    (N.itos,		Env.Var itos),
	    (N.ltos,		Env.Var ltos),
	    (N.ftos,		Env.Var ftos),
	    (N.dtos,		Env.Var dtos),
	    (N.print,		Env.Var print),
	    (N.args,		Env.Var args),
	    (N.fail,		Env.Var fail),
            (N.todo,            Env.Var todo),
	    (N.plen,            Env.Var plen),
            (N.prev,            Env.Var prev),
	    (N.pdivide,         Env.Var pdivide),
	    (N.psubseq,         Env.Var psubseq),
            (N.pappend,         Env.Var pappend),
	    (N.sumP,            Env.Var sumP),
	    (N.dist,            Env.Var dist),
	    (N.rev,             Env.Var rev),
            (N.length,          Env.Var length),
	    (N.nth,             Env.Var nth),
	    (N.gettimeofday,	Env.Var gettimeofday),
	    (N.readint,	        Env.Var readint),
	    (N.readfloat,	Env.Var readfloat),
	    (N.readdouble,	Env.Var readdouble),
	    (N.drand,	        Env.Var drand),
	    (N.compose,         Env.Var compose),
	    (N.map,             Env.Var map),
	    (N.filter,          Env.Var filter),
            (N.app,             Env.Var app),
	    (N.papp,            Env.Var papp),
	    (N.foldl,           Env.Var foldl),
	    (N.foldr,           Env.Var foldr),
            (N.tab,             Env.Var tab),
	    (N.reduceP,         Env.Var reduceP),
	    (N.concatWith,      Env.Var stringConcatWith),
(*
	    (N.size,		Env.Var size),
	    (N.sub,		Env.Var sub),
	    (N.substring,	Env.Var substring),
	    (N.concat,		Env.Var concat),
*)
	  (* extras *)
	    (N.newImage,	Env.Var newImage),
	    (N.updateImage3f,	Env.Var updateImage3f),
	    (N.updateImage3d,	Env.Var updateImage3d),
	    (N.outputImage,	Env.Var outputImage),
	    (N.freeImage,	Env.Var freeImage),
	    (N.getNumProcs,	Env.Var getNumProcs),
	    (N.getNumVProcs,	Env.Var getNumVProcs),
	    (N.ltcWaitForAll,	Env.Var ltcWaitForAll),
	    (N.por,	        Env.Var por),
	  (* arrays *)
	    (N.array,           Env.Var array),
	    (N.aupdate,         Env.Var aupdate),
	    (N.asub,            Env.Var asub),
	    (N.alength,         Env.Var alength)
	  ]

    end (* local *)
  end
