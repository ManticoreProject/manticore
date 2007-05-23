(* basis.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Based on CMSC 22610 Sample code (Winter 2007)
 *)

structure Basis =
  struct

    nonfix div mod

    local
      structure N = BasisNames
      val --> = AST.FunTy
      fun ** (t1, t2) = AST.TupleTy[t1, t2]
      infix 9 **
      infixr 8 -->
      fun forall mkTy = let
	    val tv = TyVar.new(Atom.atom "'a")
	    in
	      AST.TyScheme([tv], mkTy tv)
	    end
      fun monoVar (name, ty) = Var.new(Atom.toString name, ty)
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
    val listCons =
	DataCon.new listTyc
		    (N.listCons, SOME(AST.TupleTy[tv', AST.ConTy([tv'], listTyc)]))
    end (* local *)

    val intTyc = TyCon.newAbsTyc (N.int, 0)
    val longTyc = TyCon.newAbsTyc (N.long, 0)
    val integerTyc = TyCon.newAbsTyc (N.integer, 0)
    val floatTyc = TyCon.newAbsTyc (N.float, 0)
    val doubleTyc = TyCon.newAbsTyc (N.double, 0)
    val charTyc = TyCon.newAbsTyc (N.char, 0)
    val runeTyc = TyCon.newAbsTyc (N.rune, 0)
    val stringTyc = TyCon.newAbsTyc (N.string, 0)
    val parrayTyc = TyCon.newAbsTyc (N.parray, 1)
    val threadIdTyc = TyCon.newAbsTyc (N.thread_id, 0)

  (* predefined types *)
    val boolTy = AST.ConTy([], boolTyc)
    val intTy = AST.ConTy([], intTyc)
    val longTy = AST.ConTy([], longTyc)
    val integerTy = AST.ConTy([], integerTyc)
    val floatTy = AST.ConTy([], floatTyc)
    val doubleTy = AST.ConTy([], doubleTyc)
    val charTy = AST.ConTy([], charTyc)
    val runeTy = AST.ConTy([], runeTyc)
    val stringTy = AST.ConTy([], stringTyc)
    val unitTy = AST.TupleTy[]
    fun listTy ty = AST.ConTy([ty], listTyc)
    fun parrayTy ty = AST.ConTy([ty], parrayTyc)
    val threadIdTy = AST.ConTy([], threadIdTyc)

  (* operator symbols *) 
    val append =	Var.newPoly(Atom.toString N.append,
			  forall(fn tv => let
			    val ty = listTy(AST.VarTy tv)
			    in
			      ty ** ty --> ty
			    end))

    val int_lte =	monoVar(N.lte, intTy ** intTy --> boolTy)
    val float_lte =	monoVar(N.lte, floatTy ** floatTy --> boolTy)
    val double_lte =	monoVar(N.lte, doubleTy ** doubleTy --> boolTy)
    val long_lte =	monoVar(N.lte, longTy ** longTy --> boolTy)
    val integer_lte =	monoVar(N.lte, integerTy ** integerTy --> boolTy)
    val char_lte =	monoVar(N.lte, charTy ** charTy --> boolTy)
    val rune_lte =	monoVar(N.lte, runeTy ** runeTy --> boolTy)
    val string_lte =	monoVar(N.lte, stringTy ** stringTy --> boolTy)

    val int_lt =	monoVar(N.lt, intTy ** intTy --> boolTy)
    val float_lt =	monoVar(N.lt, floatTy ** floatTy --> boolTy)
    val double_lt =	monoVar(N.lt, doubleTy ** doubleTy --> boolTy)
    val long_lt =	monoVar(N.lt, longTy ** longTy --> boolTy)
    val integer_lt =	monoVar(N.lt, integerTy ** integerTy --> boolTy)
    val char_lt =	monoVar(N.lt, charTy ** charTy --> boolTy)
    val rune_lt =	monoVar(N.lt, runeTy ** runey --> boolTy)
    val string_lt =	monoVar(N.lt, stringTy ** stringTy --> boolTy)

    val int_gte =	monoVar(N.gte, intTy ** intTy --> boolTy)
    val float_gte =	monoVar(N.gte, floatTy ** floatTy --> boolTy)
    val double_gte =	monoVar(N.gte, doubleTy ** doubleTy --> boolTy)
    val long_gte =	monoVar(N.gte, longTy ** longTy --> boolTy)
    val integer_gte =	monoVar(N.gte, integerTy ** integerTy --> boolTy)
    val char_gte =	monoVar(N.gte, charTy ** charTy --> boolTy)
    val rune_gte =	monoVar(N.gte, runeTy ** runeTy --> boolTy)
    val string_gte =	monoVar(N.gte, stringTy ** stringTy --> boolTy)

    val int_gt =	monoVar(N.gt, intTy ** intTy --> boolTy)
    val float_gt =	monoVar(N.gt, floatTy ** floatTy --> boolTy)
    val double_gt =	monoVar(N.gt, doubleTy ** doubleTy --> boolTy)
    val long_gt =	monoVar(N.gt, longTy ** longTy --> boolTy)
    val integer_gt =	monoVar(N.gt, integerTy ** integerTy --> boolTy)
    val char_gt =	monoVar(N.gt, charTy ** charTy --> boolTy)
    val rune_gt =	monoVar(N.gt, runeTy ** runeTy --> boolTy)
    val string_gt =	monoVar(N.gt, stringTy ** stringTy --> boolTy)

    val int_plus =	monoVar(N.plus, intTy ** intTy --> intTy)
    val float_plus =	monoVar(N.plus, floatTy ** floatTy --> floatTy)
    val double_plus =	monoVar(N.plus, doubleTy ** doubleTy --> doubleTy)
    val long_plus =	monoVar(N.plus, longTy ** longTy --> longTy)
    val integer_plus =	monoVar(N.plus, integerTy ** integerTy --> integerTy)

    val int_minus =	monoVar(N.minus, intTy ** intTy --> intTy)
    val float_minus =	monoVar(N.minus, floatTy ** floatTy --> floatTy)
    val double_minus =	monoVar(N.minus, doubleTy ** doubleTy --> doubleTy)
    val long_minus =	monoVar(N.minus, longTy ** longTy --> longTy)
    val integer_minus =	monoVar(N.minus, integerTy ** integerTy --> integerTy)

    val int_times =	monoVar(N.times, intTy ** intTy --> intTy)
    val float_times =	monoVar(N.times, floatTy ** floatTy --> floatTy)
    val double_times =	monoVar(N.times, doubleTy ** doubleTy --> doubleTy)
    val long_times =	monoVar(N.times, longTy ** longTy --> longTy)
    val integer_times =	monoVar(N.times, integerTy ** integerTy --> integerTy)

    val int_div =	monoVar(N.div, intTy ** intTy --> intTy)
    val long_div =	monoVar(N.div, longTy ** longTy --> longTy)
    val integer_div =	monoVar(N.div, integerTy ** integerTy --> integerTy)

    val int_mod =	monoVar(N.mod, intTy ** intTy --> intTy)
    val long_mod =	monoVar(N.mod, longTy ** longTy --> longTy)
    val integer_mod =	monoVar(N.mod, integerTy ** integerTy --> integerTy)

(* TODO do /, @, ! *)

(* TODO what's up with the equality operators?
    val boolEq =	monoVar(N.eq, boolTy ** boolTy --> boolTy)
    val intEq =		monoVar(N.eq, intTy ** intTy --> boolTy)
    val stringEq =	monoVar(N.eq, stringTy ** stringTy --> boolTy)
*)

(* TODO what about uMinus?
    val uMinus =	monoVar(N.uMinus, intTy --> intTy)
*)

    val (ov1, ov2, ov3, ov4) =
	(TyVar.newClass (Atom.atom "'a", AST.Order),
	 TyVar.newClass (Atom.atom "'a", AST.Order),
	 TyVar.newClass (Atom.atom "'a", AST.Order),
	 TyVar.newClass (Atom.atom "'a", AST.Order))

    val (nv1, nv2, nv3) =
	(TyVar.newClass (Atom.atom "'a", AST.Num),
	 TyVar.newClass (Atom.atom "'a", AST.Num),
	 TyVar.newClass (Atom.atom "'a", AST.Num))

    val (iv1, iv2) =
	(TyVar.newClass (Atom.atom "'a", AST.Int),
	 TyVar.newClass (Atom.atom "'a", AST.Int))

    val lte = (Types.TyScheme ([ov1], (Types.VarTy ov1) ** (Types.VarTy ov1) --> boolTy),
	       [int_lte, long_lte, integer_lte, float_lte, double_lte, char_lte, rune_lte, string_lte])
    val lt = (Types.TyScheme ([ov2], (Types.VarTy ov2) ** (Types.VarTy ov2) --> boolTy),
	       [int_lt, long_lt, integer_lt, float_lt, double_lt, char_lt, rune_lt, string_lt])
    val gte = (Types.TyScheme ([ov3], (Types.VarTy ov3) ** (Types.VarTy ov3) --> boolTy),
	       [int_gte, long_gte, integer_gte, float_gte, double_gte, char_gte, rune_gte, string_gte])
    val gt = (Types.TyScheme ([ov4], (Types.VarTy ov4) ** (Types.VarTy ov4) --> boolTy),
	       [int_gt, long_gt, integer_gt, float_gt, double_gt, char_gt, rune_gt, string_gt])

    val plus = (Types.TyScheme ([nv1], (Types.VarTy nv1) ** (Types.VarTy nv1) --> (Types.VarTy nv1)),
	       [int_plus, long_plus, integer_plus, float_plus, double_plus])
    val minus = (Types.TyScheme ([nv2], (Types.VarTy nv2) ** (Types.VarTy nv2) --> (Types.VarTy nv2)),
	       [int_minus, long_minus, integer_minus, float_minus, double_minus])
    val times = (Types.TyScheme ([nv2], (Types.VarTy nv3) ** (Types.VarTy nv3) --> (Types.VarTy nv3)),
	       [int_times, long_times, integer_times, float_times, double_times])

    val div = (Types.TyScheme ([iv1], (Types.VarTy iv1) ** (Types.VarTy iv1) --> (Types.VarTy iv1)),
	       [int_div, long_div, integer_div])
    val mod = (Types.TyScheme ([iv2], (Types.VarTy iv2) ** (Types.VarTy iv2) --> (Types.VarTy iv2)),
	       [int_mod, long_mod, integer_mod])


    val lookupOp = let
	  val tbl = AtomTable.mkTable (16, Fail "lookupOp")
	  val ins = AtomTable.insert tbl
	  in
	    List.app ins [
	    (N.lte, lte),
	    (N.lt, lt),
	    (N.gte, gte),
	    (N.gt, lt),
	    (N.plus, plus),
	    (N.minus, minus),
	    (N.times, times),
	    (N.div, div),
	    (N.mod, mod),
	      ];
	    AtomTable.lookup tbl
	  end


  (* predefined functions *)
    val args =		monoVar(N.args, unitTy --> listTy stringTy)
    val print =		monoVar(N.print, stringTy --> unitTy)
    val fail =		Var.newPoly(Atom.toString N.fail, forall (fn tv => stringTy --> AST.VarTy tv))
    val itos =		monoVar(N.itos, intTy --> stringTy)
    val size =		monoVar(N.size, stringTy --> intTy)
    val sub =		monoVar(N.sub, stringTy ** intTy --> intTy)
    val substring =	monoVar(N.substring, AST.TupleTy[stringTy, intTy, intTy] --> stringTy)
    val concat =	monoVar(N.concat, listTy stringTy --> stringTy)

  (* the predefined type environment *)
    val te0 = Env.fromList [
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
	    (N.parray,		Env.TyCon parrayTyc),
	    (N.unit,		Env.TyDef(Types.TyScheme([], unitTy)))
	  ]

    val ve0 = Env.fromList [
	    (N.boolTrue,	Env.Con boolTrue),
	    (N.boolFalse,	Env.Con boolFalse),
	    (N.listNil,		Env.Con listNil),
	    (N.listCons,	Env.Con listCons),
	    (*(N.uMinus,		Env.Var uMinus),*)
	    (N.args,		Env.Var args),
	    (N.print,		Env.Var print),
	    (N.fail,		Env.Var fail),
	    (N.itos,		Env.Var itos),
	    (N.size,		Env.Var size),
	    (N.sub,		Env.Var sub),
	    (N.substring,	Env.Var substring),
	    (N.concat,		Env.Var concat)
	  ]

    end (* local *)

  end
