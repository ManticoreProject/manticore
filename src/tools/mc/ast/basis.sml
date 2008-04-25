(* basis.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Based on CMSC 22610 Sample code (Winter 2007)
 *)

structure Basis : sig

  (* basis type constructors *)
    val boolTyc		: Types.tycon
    val exnTyc		: Types.tycon
    val intTyc		: Types.tycon
    val longTyc		: Types.tycon
    val integerTyc	: Types.tycon
    val floatTyc	: Types.tycon
    val doubleTyc	: Types.tycon
    val charTyc		: Types.tycon
    val runeTyc		: Types.tycon
    val stringTyc	: Types.tycon
    val listTyc		: Types.tycon
    val optionTyc	: Types.tycon
    val parrayTyc	: Types.tycon
    val chanTyc		: Types.tycon
    val ivarTyc		: Types.tycon
    val mvarTyc		: Types.tycon
    val eventTyc	: Types.tycon
    val threadIdTyc	: Types.tycon

  (* basis types *)
    val unitTy		: Types.ty
    val boolTy		: Types.ty
    val exnTy		: Types.ty
    val intTy		: Types.ty
    val floatTy		: Types.ty
    val stringTy	: Types.ty
    val listTy		: Types.ty -> Types.ty
    val optionTy	: Types.ty -> Types.ty
    val threadIdTy	: Types.ty
    val parrayTy	: Types.ty -> Types.ty
    val eventTy		: Types.ty -> Types.ty

  (* type classes as lists of types *)
    val IntClass	: Types.ty list
    val FloatClass	: Types.ty list
    val NumClass	: Types.ty list
    val OrderClass	: Types.ty list

  (* constructors *)
    val boolTrue	: AST.dcon
    val boolFalse	: AST.dcon
    val listNil		: AST.dcon
    val listCons	: AST.dcon
    val optionNONE	: AST.dcon
    val optionSOME	: AST.dcon

  (* exceptions *)
    val exnBind		: AST.dcon
    val exnFail		: AST.dcon
    val exnDiv		: AST.dcon
    val exnMatch	: AST.dcon

  (* equality operations *)
    val eq		: AST.var
    val neq		: AST.var

  (* primitive operators *)
    val listAppend	: AST.var
    val map             : AST.var
    val filter          : AST.var
    val foldl           : AST.var
    val foldr           : AST.var
    val stringConcat	: AST.var
    val stringConcatWith : AST.var
    val psub            : AST.var
    val int_div		: AST.var
    val int_gt		: AST.var
    val int_gte		: AST.var
    val int_lt		: AST.var
    val int_lte		: AST.var
    val int_minus	: AST.var
    val int_mod		: AST.var
    val int_neg		: AST.var
    val int_plus	: AST.var
    val int_times	: AST.var
    val long_div	: AST.var
    val long_gt		: AST.var
    val long_gte	: AST.var
    val long_lt		: AST.var
    val long_lte	: AST.var
    val long_minus	: AST.var
    val long_mod	: AST.var
    val long_neg	: AST.var
    val long_plus	: AST.var
    val long_times	: AST.var
    val integer_div	: AST.var
    val integer_gt	: AST.var
    val integer_gte	: AST.var
    val integer_lt	: AST.var
    val integer_lte	: AST.var
    val integer_minus	: AST.var
    val integer_mod	: AST.var
    val integer_neg	: AST.var
    val integer_plus	: AST.var
    val integer_times	: AST.var
    val float_fdiv	: AST.var
    val float_gt	: AST.var
    val float_gte	: AST.var
    val float_lt	: AST.var
    val float_lte	: AST.var
    val float_minus	: AST.var
    val float_neg	: AST.var
    val float_plus	: AST.var
    val float_times	: AST.var
    val double_fdiv	: AST.var
    val double_gt	: AST.var
    val double_gte	: AST.var
    val double_lt	: AST.var
    val double_lte	: AST.var
    val double_minus	: AST.var
    val double_neg	: AST.var
    val double_plus	: AST.var
    val double_times	: AST.var
    val char_gt		: AST.var
    val char_gte	: AST.var
    val char_lt		: AST.var
    val char_lte	: AST.var
    val rune_gt		: AST.var
    val rune_gte	: AST.var
    val rune_lt		: AST.var
    val rune_lte	: AST.var
    val string_gt	: AST.var
    val string_gte	: AST.var
    val string_lt	: AST.var
    val string_lte	: AST.var

  (* predefined functions *)
    val not		: AST.var
    val sqrtf		: AST.var
    val absf		: AST.var
    val lnf		: AST.var
    val log2f		: AST.var
    val log10f		: AST.var
    val powf		: AST.var
    val expf		: AST.var
    val sinf		: AST.var
    val cosf		: AST.var
    val tanf		: AST.var
    val itof		: AST.var
    val sqrtd		: AST.var
    val absd		: AST.var
    val lnd		: AST.var
    val log2d		: AST.var
    val log10d		: AST.var
    val powd		: AST.var
    val expd		: AST.var
    val cosd		: AST.var
    val sind		: AST.var
    val tand		: AST.var
    val itod		: AST.var
    val channel		: AST.var
    val send		: AST.var
    val recv		: AST.var
    val sendEvt		: AST.var
    val recvEvt		: AST.var
    val wrap		: AST.var
    val choose		: AST.var
    val never		: AST.var
    val always		: AST.var
    val sync		: AST.var
    val iVar		: AST.var
    val iGet		: AST.var
    val iPut		: AST.var
    val mVar		: AST.var
    val mGet		: AST.var
    val mTake		: AST.var
    val mPut		: AST.var
    val itos		: AST.var
    val ltos		: AST.var
    val ftos		: AST.var
    val dtos		: AST.var
    val print		: AST.var
    val args		: AST.var
    val fail		: AST.var
    val todo            : AST.var
    val rev             : AST.var
    val length          : AST.var
    val nth             : AST.var
    val gettimeofday	: AST.var
    val readint 	: AST.var
    val readfloat 	: AST.var
    val readdouble 	: AST.var
    val drand           : AST.var
    val compose         : AST.var
    val app             : AST.var
    val tab             : AST.var

 (* parray operations *)
    val plen            : AST.var
    val prev            : AST.var
    val pdivide         : AST.var        
    val psubseq         : AST.var        
    val pappend         : AST.var
    val sumP            : AST.var
    val reduceP         : AST.var
    val papp            : AST.var
    val dist            : AST.var

  (* extras *)
    val imageTyc 	: Types.tycon
    val newImage	: AST.var
    val updateImage3f	: AST.var
    val updateImage3d	: AST.var
    val outputImage	: AST.var
    val freeImage	: AST.var
    val getNumProcs	: AST.var
    val getNumVProcs	: AST.var
    val ltcWaitForAll	: AST.var
    val por             : AST.var

  (* arrays *)
    val arrayTyc        : Types.tycon
    val array           : AST.var
    val aupdate         : AST.var
    val asub            : AST.var
    val alength         : AST.var

  end = struct

    structure U = BasisUtils
    structure N = BasisNames

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

    val boolTyc = TyCon.newDataTyc (N.bool, [])
    val boolTrue = DataCon.new boolTyc (N.boolTrue, NONE)
    val boolFalse = DataCon.new boolTyc (N.boolFalse, NONE)

    local
	val tv = TyVar.new(Atom.atom "'a")
	val tv' = AST.VarTy tv
    in
    val listTyc = TyCon.newDataTyc (N.list, [tv])
    val listNil = DataCon.new listTyc (N.listNil, NONE)
    val listCons = DataCon.new listTyc
	  (N.listCons, SOME(AST.TupleTy[tv', AST.ConTy([tv'], listTyc)]))
    end (* local *)

    local
	val tv = TyVar.new(Atom.atom "'a")
	val tv' = AST.VarTy tv
    in
    val optionTyc = TyCon.newDataTyc (N.option, [tv])
    val optionNONE = DataCon.new optionTyc (N.optionNONE, NONE)
    val optionSOME = DataCon.new optionTyc (N.optionSOME, SOME(tv'))
    end

    val exnTyc = Exn.exnTyc

    val intTyc = TyCon.newAbsTyc (N.int, 0, true)
    val longTyc = TyCon.newAbsTyc (N.long, 0, true)
    val integerTyc = TyCon.newAbsTyc (N.integer, 0, true)
    val floatTyc = TyCon.newAbsTyc (N.float, 0, true)
    val doubleTyc = TyCon.newAbsTyc (N.double, 0, true)
    val charTyc = TyCon.newAbsTyc (N.char, 0, true)
    val runeTyc = TyCon.newAbsTyc (N.rune, 0, true)
    val stringTyc = TyCon.newAbsTyc (N.string, 0, true)
    val parrayTyc = TyCon.newAbsTyc (N.parray, 1, false)
    val chanTyc = TyCon.newAbsTyc (N.chan, 1, true)
    val ivarTyc = TyCon.newAbsTyc (N.ivar, 1, true)
    val mvarTyc = TyCon.newAbsTyc (N.mvar, 1, true)
    val eventTyc = TyCon.newAbsTyc (N.event, 1, false)
    val threadIdTyc = TyCon.newAbsTyc (N.thread_id, 0, true)

  (* sequential-language predefined types *)
    val unitTy = AST.TupleTy[]
    val boolTy = AST.ConTy([], boolTyc)
    val exnTy = AST.ConTy([], exnTyc)
    val intTy = AST.ConTy([], intTyc)
    val longTy = AST.ConTy([], longTyc)
    val integerTy = AST.ConTy([], integerTyc)
    val floatTy = AST.ConTy([], floatTyc)
    val doubleTy = AST.ConTy([], doubleTyc)
    val charTy = AST.ConTy([], charTyc)
    val runeTy = AST.ConTy([], runeTyc)
    val stringTy = AST.ConTy([], stringTyc)
    fun listTy ty = AST.ConTy([ty], listTyc)
    fun optionTy ty = AST.ConTy([ty], optionTyc)

  (* exceptions *)
    val exnBind = Exn.new (N.exnBind, NONE)
    val exnDiv = Exn.new (N.exnDiv, NONE)
    val exnFail = Exn.new (N.exnFail, SOME stringTy)
    val exnMatch = Exn.new (N.exnMatch, NONE)

  (* concurrent and parallel-language predefined types *)
    val threadIdTy = AST.ConTy([], threadIdTyc)
    fun parrayTy ty = AST.ConTy([ty], parrayTyc)
    fun chanTy ty = AST.ConTy([ty], chanTyc)
    fun ivarTy ty = AST.ConTy([ty], ivarTyc)
    fun mvarTy ty = AST.ConTy([ty], mvarTyc)
    fun eventTy ty = AST.ConTy([ty], eventTyc)

  (* type classes as lists of types *)
    val IntClass = [intTy, longTy, integerTy]
    val FloatClass = [floatTy, doubleTy]
    val NumClass = IntClass @ FloatClass
    val OrderClass = NumClass @ [charTy, runeTy, stringTy]

    local
    fun eqTyScheme () = let
	  val tv = TyVar.newClass (Atom.atom "'a", Types.Eq)
	  val tv' = Types.VarTy tv
	  in
	    Types.TyScheme([tv], tv' ** tv' --> boolTy)
	  end
    in
    val eq = Var.newPoly(Atom.toString N.eq, eqTyScheme())
    val neq = Var.newPoly(Atom.toString N.neq, eqTyScheme())
    end

  (* operator symbols *) 
    val listAppend = Var.newPoly(Atom.toString N.append,
	  forall(fn tv => let
	    val ty = listTy tv
	    in
	      ty ** ty --> ty
	    end))
    val stringConcat = monoVar(Atom.toString N.concat, stringTy ** stringTy --> stringTy)
    val stringConcatWith = monoVar(Atom.toString N.concatWith, stringTy ** (listTy stringTy) --> stringTy) 
    val psub = polyVar(Atom.toString N.psub, fn tv => (parrayTy tv) ** intTy --> tv)

    local
      fun name a = "Int." ^ Atom.toString a
    in
    val int_div =       monoVar(name N.div, intTy ** intTy --> intTy)
    val int_gt =        monoVar(name N.gt, intTy ** intTy --> boolTy)
    val int_gte =       monoVar(name N.gte, intTy ** intTy --> boolTy)
    val int_lt =        monoVar(name N.lt, intTy ** intTy --> boolTy)
    val int_lte =       monoVar(name N.lte, intTy ** intTy --> boolTy)
    val int_minus =     monoVar(name N.minus, intTy ** intTy --> intTy)
    val int_mod =       monoVar(name N.mod, intTy ** intTy --> intTy)
    val int_neg =       monoVar(name N.uMinus, intTy --> intTy)
    val int_plus =      monoVar(name N.plus, intTy ** intTy --> intTy)
    val int_times =     monoVar(name N.times, intTy ** intTy --> intTy)
    end

    local
      fun name a = "Long." ^ Atom.toString a
    in
    val long_div =      monoVar(name N.div, longTy ** longTy --> longTy)
    val long_gt =       monoVar(name N.gt, longTy ** longTy --> boolTy)
    val long_gte =      monoVar(name N.gte, longTy ** longTy --> boolTy)
    val long_lt =       monoVar(name N.lt, longTy ** longTy --> boolTy)
    val long_lte =      monoVar(name N.lte, longTy ** longTy --> boolTy)
    val long_minus =    monoVar(name N.minus, longTy ** longTy --> longTy)
    val long_mod =      monoVar(name N.mod, longTy ** longTy --> longTy)
    val long_neg =      monoVar(name N.uMinus, longTy --> longTy)
    val long_plus =     monoVar(name N.plus, longTy ** longTy --> longTy)
    val long_times =    monoVar(name N.times, longTy ** longTy --> longTy)
    end

    local
      fun name a = "Integer." ^ Atom.toString a
    in
    val integer_div =   monoVar(name N.div, integerTy ** integerTy --> integerTy)
    val integer_gt =    monoVar(name N.gt, integerTy ** integerTy --> boolTy)
    val integer_gte =   monoVar(name N.gte, integerTy ** integerTy --> boolTy)
    val integer_lt =    monoVar(name N.lt, integerTy ** integerTy --> boolTy)
    val integer_lte =   monoVar(name N.lte, integerTy ** integerTy --> boolTy)
    val integer_minus = monoVar(name N.minus, integerTy ** integerTy --> integerTy)
    val integer_mod =   monoVar(name N.mod, integerTy ** integerTy --> integerTy)
    val integer_neg =   monoVar(name N.uMinus, integerTy --> integerTy)
    val integer_plus =  monoVar(name N.plus, integerTy ** integerTy --> integerTy)
    val integer_times = monoVar(name N.times, integerTy ** integerTy --> integerTy)
    end

    local
      fun name a = "Float." ^ Atom.toString a
    in
    val float_fdiv =    monoVar(name N.fdiv, floatTy ** floatTy --> floatTy)
    val float_gt =      monoVar(name N.gt, floatTy ** floatTy --> boolTy)
    val float_gte =     monoVar(name N.gte, floatTy ** floatTy --> boolTy)
    val float_lt =      monoVar(name N.lt, floatTy ** floatTy --> boolTy)
    val float_lte =     monoVar(name N.lte, floatTy ** floatTy --> boolTy)
    val float_minus =   monoVar(name N.minus, floatTy ** floatTy --> floatTy)
    val float_neg =	monoVar(name N.uMinus, floatTy --> floatTy)
    val float_plus =    monoVar(name N.plus, floatTy ** floatTy --> floatTy)
    val float_times =   monoVar(name N.times, floatTy ** floatTy --> floatTy)
    end

    local
      fun name a = "Double." ^ Atom.toString a
    in
    val double_fdiv =   monoVar(name N.fdiv, doubleTy ** doubleTy --> doubleTy)
    val double_gt =     monoVar(name N.gt, doubleTy ** doubleTy --> boolTy)
    val double_gte =    monoVar(name N.gte, doubleTy ** doubleTy --> boolTy)
    val double_lt =     monoVar(name N.lt, doubleTy ** doubleTy --> boolTy)
    val double_lte =    monoVar(name N.lte, doubleTy ** doubleTy --> boolTy)
    val double_minus =  monoVar(name N.minus, doubleTy ** doubleTy --> doubleTy)
    val double_neg =    monoVar(name N.uMinus, doubleTy --> doubleTy)
    val double_plus =   monoVar(name N.plus, doubleTy ** doubleTy --> doubleTy)
    val double_times =  monoVar(name N.times, doubleTy ** doubleTy --> doubleTy)
    end

    local
      fun name a = "Int." ^ Atom.toString a
    in
    val char_gt =       monoVar(name N.gt, charTy ** charTy --> boolTy)
    val char_gte =      monoVar(name N.gte, charTy ** charTy --> boolTy)
    val char_lt =       monoVar(name N.lt, charTy ** charTy --> boolTy)
    val char_lte =      monoVar(name N.lte, charTy ** charTy --> boolTy)
    end

    local
      fun name a = "Int." ^ Atom.toString a
    in
    val rune_gt =       monoVar(name N.gt, runeTy ** runeTy --> boolTy)
    val rune_gte =      monoVar(name N.gte, runeTy ** runeTy --> boolTy)
    val rune_lt =       monoVar(name N.lt, runeTy ** runeTy --> boolTy)
    val rune_lte =      monoVar(name N.lte, runeTy ** runeTy --> boolTy)
    end

    local
      fun name a = "Int." ^ Atom.toString a
    in
    val string_gt =     monoVar(name N.gt, stringTy ** stringTy --> boolTy)
    val string_gte =    monoVar(name N.gte, stringTy ** stringTy --> boolTy)
    val string_lt =     monoVar(name N.lt, stringTy ** stringTy --> boolTy)
    val string_lte =    monoVar(name N.lte, stringTy ** stringTy --> boolTy)
    end

    local
      fun eqTyScheme () = let
	    val tv = TyVar.newClass (Atom.atom "'a", Types.Eq)
	    val tv' = Types.VarTy tv
	    in
	      Types.TyScheme([tv], tv' ** tv' --> boolTy)
	    end
    in
    val eq = Var.newPoly(Atom.toString N.eq, eqTyScheme())
    val neq = Var.newPoly(Atom.toString N.neq, eqTyScheme())
    end

  (* predefined functions *)
    val not =		monoVar'(N.not, boolTy --> boolTy)
    val sqrtf =		monoVar'(N.sqrtf, floatTy --> floatTy)
    val absf =		monoVar'(N.absf, floatTy --> floatTy)
    val lnf =		monoVar'(N.lnf, floatTy --> floatTy)
    val log2f =		monoVar'(N.log2f, floatTy --> floatTy)
    val log10f =	monoVar'(N.log10f, floatTy --> floatTy)
    val powf =		monoVar'(N.powf, floatTy ** floatTy --> floatTy)
    val expf =		monoVar'(N.expf, floatTy --> floatTy)
    val sinf =		monoVar'(N.sinf, floatTy --> floatTy)
    val cosf =		monoVar'(N.cosf, floatTy --> floatTy)
    val tanf =		monoVar'(N.tanf, floatTy --> floatTy)
    val itof =		monoVar'(N.itof, intTy --> floatTy)
    val sqrtd =		monoVar'(N.sqrtd, doubleTy --> doubleTy)
    val absd =		monoVar'(N.absd, doubleTy --> doubleTy)
    val lnd =		monoVar'(N.lnd, doubleTy --> doubleTy)
    val log2d =		monoVar'(N.log2d, doubleTy --> doubleTy)
    val log10d =	monoVar'(N.log10d, doubleTy --> doubleTy)
    val powd =		monoVar'(N.powd, doubleTy ** doubleTy --> doubleTy)
    val expd =		monoVar'(N.expd, doubleTy --> doubleTy)
    val cosd =		monoVar'(N.cosd, doubleTy --> doubleTy)
    val sind =		monoVar'(N.sind, doubleTy --> doubleTy)
    val tand =		monoVar'(N.tand, doubleTy --> doubleTy)
    val itod =		monoVar'(N.itod, intTy --> doubleTy)
    val channel =	polyVar'(N.channel, fn tv => unitTy --> chanTy tv)
    val send =		polyVar'(N.send, fn tv => chanTy tv ** tv --> unitTy)
    val sendEvt =	polyVar'(N.sendEvt, fn tv => chanTy tv ** tv --> eventTy unitTy)
    val recv =		polyVar'(N.recv, fn tv => chanTy tv --> tv)
    val recvEvt =	polyVar'(N.recvEvt, fn tv => chanTy tv --> eventTy tv)
    val wrap =		let
			val a' = TyVar.new(Atom.atom "'a")
			val b' = TyVar.new(Atom.atom "'b")
			in
			  Var.newPoly(Atom.toString N.wrap, 
			    AST.TyScheme([a',b'],
			      (eventTy(AST.VarTy a') ** (AST.VarTy a' --> AST.VarTy b'))
				--> eventTy(AST.VarTy b')))
			end
(*
    val choose =	polyVar'(N.choose, fn tv => listTy(eventTy tv) --> eventTy tv)
*)
    val choose =	polyVar'(N.choose, fn tv => eventTy tv ** eventTy tv --> eventTy tv)
    val always =	polyVar'(N.always, fn tv => tv --> eventTy tv)
    val never =		polyVar'(N.never, fn tv => eventTy tv)
    val sync =		polyVar'(N.sync, fn tv => eventTy tv --> tv)
    val iVar =		polyVar'(N.iVar, fn tv => unitTy --> ivarTy tv)
    val iGet =		polyVar'(N.iGet, fn tv => ivarTy tv --> tv)
    val iPut =		polyVar'(N.iPut, fn tv => (ivarTy tv ** tv) --> unitTy)
    val mVar =		polyVar'(N.mVar, fn tv => unitTy --> mvarTy tv)
    val mGet =		polyVar'(N.mGet, fn tv => mvarTy tv --> tv)
    val mTake =		polyVar'(N.mTake, fn tv => mvarTy tv --> tv)
    val mPut =		polyVar'(N.mPut, fn tv => (mvarTy tv ** tv) --> unitTy)
    val itos =		monoVar'(N.itos, intTy --> stringTy)
    val ltos =		monoVar'(N.ltos, longTy --> stringTy)
    val ftos =		monoVar'(N.ftos, floatTy --> stringTy)
    val dtos =		monoVar'(N.dtos, doubleTy --> stringTy)
    val print =		monoVar'(N.print, stringTy --> unitTy)
    val args =		monoVar'(N.args, unitTy --> listTy stringTy)
    val fail =		polyVar'(N.fail, fn tv => stringTy --> tv)
    val todo =          polyVar'(N.todo, fn tv => stringTy --> tv)
    val plen =          polyVar'(N.plen, fn tv => (parrayTy tv) --> intTy)
    val prev =          polyVar'(N.prev, fn tv => (parrayTy tv) --> (parrayTy tv))
    val pdivide =       polyVar'(N.pdivide, fn tv => (parrayTy tv) --> ((parrayTy tv) ** (parrayTy tv)))
    val psubseq =       polyVar'(N.psubseq, fn tv => (AST.TupleTy [(parrayTy tv), intTy, intTy]) --> (parrayTy tv))
    val pappend =       polyVar'(N.pappend, fn tv => ((parrayTy tv) ** (parrayTy tv)) --> (parrayTy tv))
    val sumP =          monoVar'(N.sumP, (parrayTy intTy) --> intTy)
    val dist =          polyVar'(N.dist, fn tv => ((intTy ** tv) --> (parrayTy tv)))
    val rev =           polyVar'(N.rev, fn tv => listTy tv --> listTy tv)
    val length =        polyVar'(N.length, fn tv => listTy tv --> intTy)
    val nth =           polyVar'(N.nth, fn tv => (listTy tv ** intTy) --> tv)
    val gettimeofday =	monoVar'(N.gettimeofday, unitTy --> doubleTy)
    val readint =	monoVar'(N.readint, unitTy --> intTy)
    val readfloat =	monoVar'(N.readfloat, unitTy --> floatTy)
    val readdouble =	monoVar'(N.readdouble, unitTy --> doubleTy)
    val drand =	        monoVar'(N.drand, (doubleTy ** doubleTy) --> doubleTy)
    val app =           polyVar'(N.app, fn tv => (tv --> unitTy) ** (listTy tv) --> unitTy)
    val papp =          polyVar'(N.papp, fn tv => (tv --> unitTy) ** (parrayTy tv) --> unitTy)
    val tab =           polyVar'(N.tab,
				 fn tv => (AST.TupleTy [intTy --> tv, intTy, intTy, intTy])
                                   --> (listTy tv))

  (* predefined functions with more than one type variable in their types *)
    val compose = let
	  fun mkTy [a,b,c] = ((a --> b) ** (c --> a)) --> (c --> b)
	    | mkTy _ = raise Fail "BUG: bad type instantiation for compose"
	  in
	    polyVarMulti' (N.compose, 3, mkTy)
	  end

    val map = let
	  fun mkTy [a,b] = ((a --> b) ** (listTy a)) --> (listTy b)
	    | mkTy _ = raise Fail "BUG: bad type instantiation for map"
	  in
	    polyVarMulti' (N.map, 2, mkTy)
	  end

    val filter = let
	  fun mkTy a = ((a --> boolTy) ** (listTy a)) --> (listTy a)
	  in
	    polyVar' (N.filter, mkTy)
	  end

    local 
      fun mkMkTy fname = let
	    fun mkTy [a,b] = (AST.TupleTy[(a ** b) --> b, b, listTy a]) --> b
	      | mkTy _ = raise Fail ("BUG: bad type instatiation for " ^ fname)
	    in
	      mkTy
	    end
    in
    val foldl = polyVarMulti' (N.foldl, 2, mkMkTy "foldl")
    val foldr = polyVarMulti' (N.foldr, 2, mkMkTy "foldr")
    end (* local *)

    val reduceP = let
	  fun mkTy [a,b] = (AST.TupleTy[(a**a)-->b, b, parrayTy a]) --> b
	    | mkTy _ = raise Fail "BUG: bad type instantiation for reduceP"
	  in
	    polyVarMulti' (N.reduceP, 2, mkTy)
	  end

(*
    val size =		monoVar(N.size, stringTy --> intTy)
    val sub =		monoVar(N.sub, stringTy ** intTy --> intTy)
    val substring =	monoVar(N.substring, AST.TupleTy[stringTy, intTy, intTy] --> stringTy)
    val concat =	monoVar(N.concat, listTy stringTy --> stringTy)
*)

  (* extras *)
    val imageTyc = TyCon.newAbsTyc (N.image, 0, false)
    val imageTy = AST.ConTy([], imageTyc)

    val newImage = monoVar' (N.newImage, intTy ** intTy --> imageTy)
    val updateImage3f = monoVar' (N.updateImage3f, AST.TupleTy[imageTy, intTy, intTy, floatTy, floatTy, floatTy] --> unitTy)
    val updateImage3d = monoVar' (N.updateImage3d, AST.TupleTy[imageTy, intTy, intTy, doubleTy, doubleTy, doubleTy] --> unitTy)
    val outputImage = monoVar' (N.outputImage, imageTy ** stringTy --> unitTy)
    val freeImage = monoVar' (N.freeImage, imageTy --> unitTy)
    val por = polyVar'(N.por, fn tv => ((unitTy --> optionTy(tv)) ** (unitTy --> optionTy(tv))) --> optionTy(tv))
    val ltcWaitForAll =	monoVar' (N.ltcWaitForAll, unitTy --> unitTy)

    val getNumProcs =	monoVar' (N.getNumProcs, unitTy --> intTy)
    val getNumVProcs =	monoVar' (N.getNumVProcs, unitTy --> intTy)

  (* arrays *)
    val arrayTyc = TyCon.newAbsTyc (N.arrayTyc, 1, false)
    fun arrayTy ty = AST.ConTy([ty], arrayTyc)
    val array =	polyVar'(N.array, fn tv => (intTy ** tv) --> arrayTy tv)
    val aupdate = polyVar'(N.aupdate, fn tv => AST.TupleTy [arrayTy tv, intTy, tv] --> unitTy)
    val asub = polyVar'(N.asub, fn tv => AST.TupleTy [arrayTy tv, intTy] --> tv)
    val alength = polyVar'(N.alength, fn tv => arrayTy tv --> intTy)
    end (* local *)

  end
