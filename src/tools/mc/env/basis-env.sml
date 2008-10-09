(* basis-env.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)


structure BasisEnv : sig

(*    val te0 : ModuleEnv.ty_env
    val ve0 : ModuleEnv.var_env
*)
    val bEnv0 : BindingEnv.env
    val mEnv0 : ModuleEnv.env
    val lookupOpPT : Atom.atom -> BindingEnv.val_bind
    val lookupOpAST : ProgramParseTree.var -> ModuleEnv.val_bind

(* FIXME: this operation shouldn't be exported, but the typechecker deals with
 * it in an ad hoc manner (unlike the other overloaded operators).
 *)
  (* overloaded unary operators *)
    val neg	: (Types.ty_scheme * AST.var list)

    val getValFromBasis : string list -> ModuleEnv.val_bind
    val getTyFromBasis : string list -> ModuleEnv.ty_def
    val getBOMTyFromBasis : string list -> ProgramParseTree.PML2.BOMParseTree.ty

  end = struct

    structure U = BasisUtils
    structure N = BasisNames
    structure PPT = ProgramParseTree
    structure BEnv = BindingEnv
    structure MEnv = ModuleEnv

    nonfix div mod

    open Basis

    val pathToString = String.concatWith "."
    val pathToAtom = Atom.atom o pathToString

  (* use a path (or qualified name) to look up a variable, i.e., 
   *      getValFromBasis(Atom.atom "Future1.future") 
   *)
    fun getValFromBasis path = (
	  case BindingEnv.findValByPath (pathToAtom path)
	   of SOME(BindingEnv.Var v) => (
		case ModuleEnv.getValBind v
		 of SOME vb => vb
		  | NONE => raise Fail ("Unable to locate "^pathToString path)
		(* end case *))
	    | _ => raise Fail ("Unable to locate "^pathToString path)
	  (* end case *))

  (* use a path (or qualified name) to look up a type *)
    fun getTyFromBasis path = (
	  case BindingEnv.findTyByPath (pathToAtom path)
	   of SOME v => (
		case ModuleEnv.getTyDef v
		 of SOME tyd => tyd
		  | NONE => raise Fail ("Unable to locate "^pathToString path)
		(* end case *))
	    | _ => raise Fail ("Unable to locate "^pathToString path)
	  (* end case *))

  (* use a path (or qualified name) to look up a BOM type *)
    fun getBOMTyFromBasis path = (
	  case BindingEnv.findBOMTyByPath(pathToAtom path)
	   of SOME v => (
		case ModuleEnv.getPrimTyDef v
		 of SOME ty => ty
		  | NONE =>  raise Fail ("Unable to locate "^pathToString path^" at "^ProgramParseTree.Var.toString v)
		(* end case *))
	    | NONE =>  raise Fail ("Unable to locate "^pathToString path)
	  (* end case *))


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

      val predefinedTypes = [
	    (N.unit,		MEnv.TyDef(Types.TyScheme([], unitTy))),
	    (N.bool,		MEnv.TyCon boolTyc),
	    (N.int,		MEnv.TyCon intTyc),
	    (N.long,		MEnv.TyCon longTyc),
	    (N.integer,		MEnv.TyCon integerTyc),
	    (N.float,		MEnv.TyCon floatTyc),
	    (N.double,		MEnv.TyCon doubleTyc),
	    (N.char,		MEnv.TyCon charTyc),
	    (N.rune,		MEnv.TyCon runeTyc),
	    (N.string,		MEnv.TyCon stringTyc),
	    (N.list,		MEnv.TyCon listTyc),
	    (N.option,		MEnv.TyCon optionTyc),
	    (N.thread_id,	MEnv.TyCon threadIdTyc),
	    (N.parray,		MEnv.TyCon parrayTyc),
	    (N.chan,		MEnv.TyCon chanTyc),
	    (N.ivar,		MEnv.TyCon ivarTyc),
	    (N.mvar,		MEnv.TyCon mvarTyc),
	    (N.event,		MEnv.TyCon eventTyc),
	  (* extras *)
	    (N.image,		MEnv.TyCon imageTyc),
	  (* arrays *)
	    (N.arrayTyc,        MEnv.TyCon arrayTyc)
	  ]

      val predefinedVars =  [
	    (N.boolTrue,	MEnv.Con boolTrue),
	    (N.boolFalse,	MEnv.Con boolFalse),
	    (N.listNil,		MEnv.Con listNil),
	    (N.listNil',	MEnv.Con listNil),
	    (N.listCons,	MEnv.Con listCons),
	    (N.optionNONE,	MEnv.Con optionNONE),
	    (N.optionSOME,	MEnv.Con optionSOME),
	    (N.exnBind,		MEnv.Con exnBind),
	    (N.exnDiv,		MEnv.Con exnDiv),
	    (N.exnFail,		MEnv.Con exnFail),
	    (N.exnMatch,	MEnv.Con exnMatch),
	    (* Unary minus is overloaded, so it's being handled
             * specially by the typechecker *)
	    (N.not,		MEnv.Var not),
	    (N.sqrtf,		MEnv.Var sqrtf),
	    (N.absf,		MEnv.Var absf),
	    (N.lnf,		MEnv.Var lnf),
	    (N.log2f,		MEnv.Var log2f),
	    (N.log10f,		MEnv.Var log10f),
	    (N.powf,		MEnv.Var powf),
	    (N.expf,		MEnv.Var expf),
	    (N.sinf,		MEnv.Var sinf),
	    (N.cosf,		MEnv.Var cosf),
	    (N.tanf,		MEnv.Var tanf),
	    (N.itof,		MEnv.Var itof),
	    (N.sqrtd,		MEnv.Var sqrtd),
	    (N.absd,		MEnv.Var absd),
	    (N.lnd,		MEnv.Var lnd),
	    (N.log2d,		MEnv.Var log2d),
	    (N.log10d,		MEnv.Var log10d),
	    (N.powd,		MEnv.Var powd),
	    (N.expd,		MEnv.Var expd),
	    (N.sind,		MEnv.Var sind),
	    (N.cosd,		MEnv.Var cosd),
	    (N.tand,		MEnv.Var tand),
	    (N.itod,		MEnv.Var itod),
	    (N.channel,		MEnv.Var channel),
	    (N.send,		MEnv.Var send),
	    (N.sendEvt,		MEnv.Var sendEvt),
	    (N.recv,		MEnv.Var recv),
	    (N.recvEvt,		MEnv.Var recvEvt),
	    (N.wrap,		MEnv.Var wrap),
	    (N.choose,		MEnv.Var choose),
	    (N.never,		MEnv.Var never),
	    (N.sync,		MEnv.Var sync),
	    (N.iVar,		MEnv.Var iVar),
	    (N.iGet,		MEnv.Var iGet),
	    (N.iPut,		MEnv.Var iPut),
	    (N.mVar,		MEnv.Var mVar),
	    (N.mGet,		MEnv.Var mGet),
	    (N.mTake,		MEnv.Var mTake),
	    (N.mPut,		MEnv.Var mPut),
	    (N.itos,		MEnv.Var itos),
	    (N.ltos,		MEnv.Var ltos),
	    (N.ftos,		MEnv.Var ftos),
	    (N.dtos,		MEnv.Var dtos),
	    (N.print,		MEnv.Var print),
	    (N.args,		MEnv.Var args),
	    (N.fail,		MEnv.Var fail),
            (N.todo,            MEnv.Var todo),
	    (N.plen,            MEnv.Var plen),
            (N.prev,            MEnv.Var prev),
	    (N.pdivide,         MEnv.Var pdivide),
	    (N.psubseq,         MEnv.Var psubseq),
            (N.pappend,         MEnv.Var pappend),
	    (N.sumP,            MEnv.Var sumP),
	    (N.dist,            MEnv.Var dist),
	    (N.rev,             MEnv.Var rev),
            (N.length,          MEnv.Var length),
	    (N.nth,             MEnv.Var nth),
	    (N.gettimeofday,	MEnv.Var gettimeofday),
	    (N.readint,	        MEnv.Var readint),
	    (N.readfloat,	MEnv.Var readfloat),
	    (N.readdouble,	MEnv.Var readdouble),
	    (N.drand,	        MEnv.Var drand),
	    (N.compose,         MEnv.Var compose),
	    (N.map,             MEnv.Var map),
	    (N.filter,          MEnv.Var filter),
            (N.app,             MEnv.Var app),
	    (N.papp,            MEnv.Var papp),
	    (N.foldl,           MEnv.Var foldl),
	    (N.foldr,           MEnv.Var foldr),
            (N.tab,             MEnv.Var tab),
	    (N.reduceP,         MEnv.Var reduceP),
	    (N.concatWith,      MEnv.Var stringConcatWith),
(*
	    (N.size,		MEnv.Var size),
	    (N.sub,		MEnv.Var sub),
	    (N.substring,	MEnv.Var substring),
	    (N.concat,		MEnv.Var concat),
*)
	  (* extras *)
	    (N.newImage,	MEnv.Var newImage),
	    (N.updateImage3f,	MEnv.Var updateImage3f),
	    (N.updateImage3d,	MEnv.Var updateImage3d),
	    (N.outputImage,	MEnv.Var outputImage),
	    (N.freeImage,	MEnv.Var freeImage),
	    (N.getNumProcs,	MEnv.Var getNumProcs),
	    (N.getNumVProcs,	MEnv.Var getNumVProcs),
	    (N.ltcWaitForAll,	MEnv.Var ltcWaitForAll),
	    (N.por,	        MEnv.Var por),
	  (* arrays *)
	    (N.array,           MEnv.Var array),
	    (N.aupdate,         MEnv.Var aupdate),
	    (N.asub,            MEnv.Var asub),
	    (N.alength,         MEnv.Var alength)
	  ]

      val predefinedTypes' = [
	    (N.unit,		MEnv.TyDef(Types.TyScheme([], unitTy))),
	    (N.bool,		MEnv.TyCon boolTyc),
	    (N.int,		MEnv.TyCon intTyc),
	    (N.long,		MEnv.TyCon longTyc),
	    (N.integer,		MEnv.TyCon integerTyc),
	    (N.float,		MEnv.TyCon floatTyc),
	    (N.double,		MEnv.TyCon doubleTyc),
	    (N.char,		MEnv.TyCon charTyc),
	    (N.rune,		MEnv.TyCon runeTyc),
	    (N.string,		MEnv.TyCon stringTyc),
	    (N.list,		MEnv.TyCon listTyc),
	    (N.option,		MEnv.TyCon optionTyc),
	    (N.thread_id,	MEnv.TyCon threadIdTyc),
	    (N.parray,		MEnv.TyCon parrayTyc),
	    (N.chan,		MEnv.TyCon chanTyc),
	    (N.ivar,		MEnv.TyCon ivarTyc),
	    (N.mvar,		MEnv.TyCon mvarTyc),
	    (N.event,		MEnv.TyCon eventTyc),
	  (* extras *)
	    (N.image,		MEnv.TyCon imageTyc),
	  (* arrays *)
	    (N.arrayTyc,        MEnv.TyCon arrayTyc)
	  ]

      val predefinedVars' =  [
	    (N.boolTrue,	MEnv.Con boolTrue),
	    (N.boolFalse,	MEnv.Con boolFalse),
	    (N.listNil,		MEnv.Con listNil),
	    (N.listNil',	MEnv.Con listNil),
	    (N.listCons,	MEnv.Con listCons),
	    (N.listCons',	MEnv.Con listCons),
	    (N.optionNONE,	MEnv.Con optionNONE),
	    (N.optionSOME,	MEnv.Con optionSOME),
	    (N.exnBind,		MEnv.Con exnBind),
	    (N.exnDiv,		MEnv.Con exnDiv),
	    (N.exnFail,		MEnv.Con exnFail),
	    (N.exnMatch,	MEnv.Con exnMatch),
	    (* Unary minus is overloaded, so it's being handled
             * specially by the typechecker *)
	    (N.not,		MEnv.Var not),
	    (N.sqrtf,		MEnv.Var sqrtf),
	    (N.absf,		MEnv.Var absf),
	    (N.lnf,		MEnv.Var lnf),
	    (N.log2f,		MEnv.Var log2f),
	    (N.log10f,		MEnv.Var log10f),
	    (N.powf,		MEnv.Var powf),
	    (N.expf,		MEnv.Var expf),
	    (N.sinf,		MEnv.Var sinf),
	    (N.cosf,		MEnv.Var cosf),
	    (N.tanf,		MEnv.Var tanf),
	    (N.itof,		MEnv.Var itof),
	    (N.sqrtd,		MEnv.Var sqrtd),
	    (N.absd,		MEnv.Var absd),
	    (N.lnd,		MEnv.Var lnd),
	    (N.log2d,		MEnv.Var log2d),
	    (N.log10d,		MEnv.Var log10d),
	    (N.powd,		MEnv.Var powd),
	    (N.expd,		MEnv.Var expd),
	    (N.sind,		MEnv.Var sind),
	    (N.cosd,		MEnv.Var cosd),
	    (N.tand,		MEnv.Var tand),
	    (N.itod,		MEnv.Var itod),
	    (N.channel,		MEnv.Var channel),
	    (N.send,		MEnv.Var send),
	    (N.sendEvt,		MEnv.Var sendEvt),
	    (N.recv,		MEnv.Var recv),
	    (N.recvEvt,		MEnv.Var recvEvt),
	    (N.wrap,		MEnv.Var wrap),
	    (N.choose,		MEnv.Var choose),
	    (N.never,		MEnv.Var never),
	    (N.sync,		MEnv.Var sync),
	    (N.iVar,		MEnv.Var iVar),
	    (N.iGet,		MEnv.Var iGet),
	    (N.iPut,		MEnv.Var iPut),
	    (N.mVar,		MEnv.Var mVar),
	    (N.mGet,		MEnv.Var mGet),
	    (N.mTake,		MEnv.Var mTake),
	    (N.mPut,		MEnv.Var mPut),
	    (N.itos,		MEnv.Var itos),
	    (N.ltos,		MEnv.Var ltos),
	    (N.ftos,		MEnv.Var ftos),
	    (N.dtos,		MEnv.Var dtos),
	    (N.print,		MEnv.Var print),
	    (N.args,		MEnv.Var args),
	    (N.fail,		MEnv.Var fail),
            (N.todo,            MEnv.Var todo),
	    (N.plen,            MEnv.Var plen),
            (N.prev,            MEnv.Var prev),
	    (N.pdivide,         MEnv.Var pdivide),
	    (N.psubseq,         MEnv.Var psubseq),
            (N.pappend,         MEnv.Var pappend),
	    (N.sumP,            MEnv.Var sumP),
	    (N.dist,            MEnv.Var dist),
	    (N.rev,             MEnv.Var rev),
            (N.length,          MEnv.Var length),
	    (N.nth,             MEnv.Var nth),
	    (N.gettimeofday,	MEnv.Var gettimeofday),
	    (N.readint,	        MEnv.Var readint),
	    (N.readfloat,	MEnv.Var readfloat),
	    (N.readdouble,	MEnv.Var readdouble),
	    (N.drand,	        MEnv.Var drand),
	    (N.compose,         MEnv.Var compose),
	    (N.map,             MEnv.Var map),
	    (N.filter,          MEnv.Var filter),
            (N.app,             MEnv.Var app),
	    (N.papp,            MEnv.Var papp),
	    (N.foldl,           MEnv.Var foldl),
	    (N.foldr,           MEnv.Var foldr),
            (N.tab,             MEnv.Var tab),
	    (N.reduceP,         MEnv.Var reduceP),
	    (N.concatWith,      MEnv.Var stringConcatWith),
(*
	    (N.size,		MEnv.Var size),
	    (N.sub,		MEnv.Var sub),
	    (N.substring,	MEnv.Var substring),
	    (N.concat,		MEnv.Var concat),
*)
	  (* extras *)
	    (N.newImage,	MEnv.Var newImage),
	    (N.updateImage3f,	MEnv.Var updateImage3f),
	    (N.updateImage3d,	MEnv.Var updateImage3d),
	    (N.outputImage,	MEnv.Var outputImage),
	    (N.freeImage,	MEnv.Var freeImage),
	    (N.getNumProcs,	MEnv.Var getNumProcs),
	    (N.getNumVProcs,	MEnv.Var getNumVProcs),
	    (N.ltcWaitForAll,	MEnv.Var ltcWaitForAll),
	    (N.por,	        MEnv.Var por),
	  (* arrays *)
	    (N.array,           MEnv.Var array),
	    (N.aupdate,         MEnv.Var aupdate),
	    (N.asub,            MEnv.Var asub),
	    (N.alength,         MEnv.Var alength)
	  ]
    in

  (* create a type scheme that binds a kinded type variable *)
    fun tyScheme (cls, mk) = let
	  val tv = TyVar.newClass (Atom.atom "'a", cls)
	  in
	    Types.TyScheme([tv], mk(Types.VarTy tv))
	  end

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

      fun bindTy (n, x) = let
	      val v = PPT.Var.new(Atom.toString n, ())
              in
	        MEnv.setTyDef(v, SOME x);
                ((n, v), (v, x))
              end

      val bindTys = ListPair.unzip o List.map bindTy

      fun bindVal (n, x) = let
	      val v = PPT.Var.new(Atom.toString n, ())
	      val v' = (case x
			 of MEnv.Con _ => BEnv.Con v
			  | _ => BEnv.Var v
		       (* end case *))
              in
	        MEnv.setValBind(v, SOME x);
                ((n, v'), (v, x))
              end

      val bindVals = ListPair.unzip o List.map bindVal

      val (bEnv0, mEnv0) = let	     

	     val (predefinedTyBinds, predefinedTys) = bindTys predefinedTypes'
	     val (predefinedVarBinds, predefinedVars) = bindVals predefinedVars'

           (* create the top-level binding environment *)
	     val BEnv.Env {name, modEnv, sigEnv, bomEnv, outerEnv, ...} = BEnv.empty (Atom.atom "", NONE)
	     val bEnv0 = BEnv.Env{
			    name=name,
                            modEnv=modEnv, 
			    sigEnv=sigEnv, 
			    bomEnv=BOMBasisEnv.bindingEnv,
			    outerEnv=outerEnv, 
			    varEnv=BEnv.fromList predefinedVarBinds, 
			    tyEnv=BEnv.fromList predefinedTyBinds
			   }

           (* create the top-level module environment *)
	     val modRef = AST.MOD{id=Stamp.new(), name=Atom.atom "Basis", formals=NONE, expansionOpts=ref []}
	     val MEnv.ModEnv {modRef, modEnv, sigEnv, outerEnv, ...} = MEnv.fresh (modRef, NONE)
	     val basisEnv = MEnv.ModEnv{
                                 modRef=modRef, 
				 modEnv=modEnv, 
				 sigEnv=sigEnv, 
				 outerEnv=outerEnv,
				 tyEnv=MEnv.fromList predefinedTys, 
				 varEnv=MEnv.fromList predefinedVars
			       }
(*	     val modRef = AST.MOD{id=Stamp.new(), name=Atom.atom "TopLevel", formals=NONE, expansionOpts=ref []}
	     val mEnv0 = MEnv.fresh(modRef, SOME basisEnv)
*)
             in
	        (bEnv0, basisEnv)
	     end

    val (lookupOpPT, lookupOpAST) = let

        (* constructors *)
	  val cons = [
		(N.listCons,	MEnv.Con listCons)
	      ]
        (* non-overloaded operators *)
	  val nonOverloadedOps = [
		(N.append,	MEnv.Var listAppend),
		(N.concat,	MEnv.Var stringConcat),
                (N.psub,        MEnv.Var psub)
	      ]
        (* equality operators *)
	  val eqOps = [
		(N.eq,		MEnv.EqOp eq),
		(N.neq,		MEnv.EqOp neq)
	      ]
        (* overloaded operators *)
	  val ovOps = [
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
	      ]

	  val (consBinds, cons) = bindVals cons
	  val (nonOverloadedOpsBinds, nonOverloadedOps) = bindVals nonOverloadedOps
	  val (eqOpsBinds, eqOps) = bindVals eqOps
	  val ovOps = List.map (fn (id, info) => (id, MEnv.Overload info)) ovOps
	  val (ovOpsBinds, ovOps) = bindVals ovOps

	  val bEnv = BEnv.fromList (List.concat [
		       consBinds, nonOverloadedOpsBinds, eqOpsBinds, ovOpsBinds
		     ])
	  fun lookupOpPT id = Option.valOf(BEnv.Map.find(bEnv, id))

	  val mEnv = MEnv.fromList (List.concat [
		       cons, nonOverloadedOps, eqOps, ovOps
		     ])
	  fun lookupOpAST id = Option.valOf(MEnv.VarMap.find(mEnv, id))
	  in
	    (lookupOpPT, lookupOpAST)
	  end

    end (* local *)
  end
