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
    val stringTyc = TyCon.newAbsTyc (N.string, 0)

  (* predefined types *)
    val boolTy = AST.ConTy([], boolTyc)
    val intTy = AST.ConTy([], intTyc)
    val stringTy = AST.ConTy([], stringTyc)
    val unitTy = AST.TupleTy[]
    fun listTy ty = AST.ConTy([ty], listTyc)

  (* operator symbols *) 
    val lte =		monoVar(N.lte, intTy ** intTy --> boolTy)
    val lt =		monoVar(N.lt, intTy ** intTy --> boolTy)
    val append =	Var.newPoly(Atom.toString N.append,
			  forall(fn tv => let
			    val ty = listTy(AST.VarTy tv)
			    in
			      ty ** ty --> ty
			    end))
    val plus =		monoVar(N.plus, intTy ** intTy --> intTy)
    val minus =		monoVar(N.minus, intTy ** intTy --> intTy)
    val times =		monoVar(N.times, intTy ** intTy --> intTy)
    val div =		monoVar(N.div, intTy ** intTy --> intTy)
    val mod =		monoVar(N.mod, intTy ** intTy --> intTy)
    val uMinus =	monoVar(N.uMinus, intTy --> intTy)

    val lookupOp = let
	  val tbl = AtomTable.mkTable (16, Fail "lookupOp")
	  val ins = AtomTable.insert tbl
	  in
	    List.app ins [
		(N.lte, lte),
		(N.lt, lt),
		(N.append, append),
		(N.plus, plus),
		(N.minus, minus),
		(N.times, times),
		(N.div, div),
		(N.mod, mod)
	      ];
	    AtomTable.lookup tbl
	  end

  (* equality operators *)
    val boolEq =	monoVar(N.eq, boolTy ** boolTy --> boolTy)
    val intEq =		monoVar(N.eq, intTy ** intTy --> boolTy)
    val stringEq =	monoVar(N.eq, stringTy ** stringTy --> boolTy)

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
	    (N.list,		Env.TyCon listTyc),
	    (N.string,		Env.TyCon stringTyc),
	    (N.unit,		Env.TyDef(Types.TyScheme([], unitTy)))
	  ]

    val ve0 = Env.fromList [
	    (N.boolTrue,	Env.Con boolTrue),
	    (N.boolFalse,	Env.Con boolFalse),
	    (N.listNil,		Env.Con listNil),
	    (N.listCons,	Env.Con listCons),
	    (N.uMinus,		Env.Var uMinus),
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
