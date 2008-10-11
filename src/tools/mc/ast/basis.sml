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
    val parrayTy	: Types.ty -> Types.ty
    val threadIdTy	: Types.ty

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

  (* exceptions *)
    val exnBind		: AST.dcon
    val exnFail		: AST.dcon
    val exnDiv		: AST.dcon
    val exnMatch	: AST.dcon

  (* equality operations *)
    val eq		: AST.var
    val neq		: AST.var

  (* primitive operators *)
    val list_append	: AST.var
    val string_concat	: AST.var
    val parray_sub      : AST.var
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

  end = struct

    structure N = BasisNames

  (* primitive PML types *)
    val boolTyc = TyCon.newDataTyc (N.bool, [])
    val boolFalse = DataCon.new boolTyc (N.boolFalse, NONE) (* must define false first!!! *)
    val boolTrue = DataCon.new boolTyc (N.boolTrue, NONE) (* must define true second!!! *)

    local
	val tv = TyVar.new(Atom.atom "'a")
	val tv' = AST.VarTy tv
    in
    val listTyc = TyCon.newDataTyc (N.list, [tv])
    val listNil = DataCon.new listTyc (N.listNil, NONE)
    val listCons = DataCon.new listTyc
	  (N.listCons, SOME(AST.TupleTy[tv', AST.ConTy([tv'], listTyc)]))
    end (* local *)

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

  (* predefined types *)
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
    fun parrayTy ty = AST.ConTy([ty], parrayTyc)
    val threadIdTy = AST.ConTy([], threadIdTyc)

  (* exceptions *)
    val exnBind = Exn.new (N.exnBind, NONE)
    val exnDiv = Exn.new (N.exnDiv, NONE)
    val exnFail = Exn.new (N.exnFail, SOME stringTy)
    val exnMatch = Exn.new (N.exnMatch, NONE)

  (* type classes as lists of types *)
    val IntClass = [intTy, longTy, integerTy]
    val FloatClass = [floatTy, doubleTy]
    val NumClass = IntClass @ FloatClass
    val OrderClass = NumClass @ [charTy, runeTy, stringTy]

    val --> = AST.FunTy
    fun ** (t1, t2) = AST.TupleTy[t1, t2]
    infix 9 **
    infixr 8 -->

  (* forall : (AST.ty -> AST.ty) -> AST.ty_scheme *)
    fun forall mkTy = let
        val tv = TyVar.new(Atom.atom "'a")
        in
          AST.TyScheme([tv], mkTy(AST.VarTy tv))
        end

  (* monoVar : string * A.ty -> A.var *)
    fun monoVar (name, ty) = Var.new(name, ty)

  (* polyVar : string * (A.ty -> A.ty) -> A.var *) 
    fun polyVar (name, mkTy) = Var.newPoly(name, forall mkTy)

  (**** operator symbols ****
   *
   * We use the following naming convention for operators that allows automatic
   * mapping to the HLOp names in "initial-basis.pml".  For example, the addition
   * operator for integers is named "Int.add", which is mapped to "@int-add".
   *)

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

    val list_append = Var.newPoly("list-append",
	  forall(fn tv => let
	    val ty = listTy tv
	    in
	      ty ** ty --> ty
	    end))
    val string_concat = monoVar("string-concat2", stringTy ** stringTy --> stringTy)
    val parray_sub = polyVar("parray-sub", fn tv => (parrayTy tv) ** intTy --> tv)

    local
      fun name a = "int-" ^ a
    in
    val int_div =       monoVar(name "div", intTy ** intTy --> intTy)
    val int_gt =        monoVar(name "gt", intTy ** intTy --> boolTy)
    val int_gte =       monoVar(name "gte", intTy ** intTy --> boolTy)
    val int_lt =        monoVar(name "lt", intTy ** intTy --> boolTy)
    val int_lte =       monoVar(name "lte", intTy ** intTy --> boolTy)
    val int_minus =     monoVar(name "sub", intTy ** intTy --> intTy)
    val int_mod =       monoVar(name "mod", intTy ** intTy --> intTy)
    val int_neg =       monoVar(name "neg", intTy --> intTy)
    val int_plus =      monoVar(name "add", intTy ** intTy --> intTy)
    val int_times =     monoVar(name "mul", intTy ** intTy --> intTy)
    end

    local
      fun name a = "long-" ^ a
    in
    val long_div =      monoVar(name "div", longTy ** longTy --> longTy)
    val long_gt =       monoVar(name "gt", longTy ** longTy --> boolTy)
    val long_gte =      monoVar(name "gte", longTy ** longTy --> boolTy)
    val long_lt =       monoVar(name "lt", longTy ** longTy --> boolTy)
    val long_lte =      monoVar(name "lte", longTy ** longTy --> boolTy)
    val long_minus =    monoVar(name "sub", longTy ** longTy --> longTy)
    val long_mod =      monoVar(name "mod", longTy ** longTy --> longTy)
    val long_neg =      monoVar(name "neg", longTy --> longTy)
    val long_plus =     monoVar(name "add", longTy ** longTy --> longTy)
    val long_times =    monoVar(name "mul", longTy ** longTy --> longTy)
    end

    local
      fun name a = "integer-" ^ a
    in
    val integer_div =   monoVar(name "div", integerTy ** integerTy --> integerTy)
    val integer_gt =    monoVar(name "gt", integerTy ** integerTy --> boolTy)
    val integer_gte =   monoVar(name "gte", integerTy ** integerTy --> boolTy)
    val integer_lt =    monoVar(name "lt", integerTy ** integerTy --> boolTy)
    val integer_lte =   monoVar(name "lte", integerTy ** integerTy --> boolTy)
    val integer_minus = monoVar(name "sub", integerTy ** integerTy --> integerTy)
    val integer_mod =   monoVar(name "mod", integerTy ** integerTy --> integerTy)
    val integer_neg =   monoVar(name "neg", integerTy --> integerTy)
    val integer_plus =  monoVar(name "add", integerTy ** integerTy --> integerTy)
    val integer_times = monoVar(name "mul", integerTy ** integerTy --> integerTy)
    end

    local
      fun name a = "float-" ^ a
    in
    val float_fdiv =    monoVar(name "div", floatTy ** floatTy --> floatTy)
    val float_gt =      monoVar(name "gt", floatTy ** floatTy --> boolTy)
    val float_gte =     monoVar(name "gte", floatTy ** floatTy --> boolTy)
    val float_lt =      monoVar(name "lt", floatTy ** floatTy --> boolTy)
    val float_lte =     monoVar(name "lte", floatTy ** floatTy --> boolTy)
    val float_minus =   monoVar(name "sub", floatTy ** floatTy --> floatTy)
    val float_neg =	monoVar(name "neg", floatTy --> floatTy)
    val float_plus =    monoVar(name "add", floatTy ** floatTy --> floatTy)
    val float_times =   monoVar(name "mul", floatTy ** floatTy --> floatTy)
    end

    local
      fun name a = "double-" ^ a
    in
    val double_fdiv =   monoVar(name "div", doubleTy ** doubleTy --> doubleTy)
    val double_gt =     monoVar(name "gt", doubleTy ** doubleTy --> boolTy)
    val double_gte =    monoVar(name "gte", doubleTy ** doubleTy --> boolTy)
    val double_lt =     monoVar(name "lt", doubleTy ** doubleTy --> boolTy)
    val double_lte =    monoVar(name "lte", doubleTy ** doubleTy --> boolTy)
    val double_minus =  monoVar(name "sub", doubleTy ** doubleTy --> doubleTy)
    val double_neg =    monoVar(name "neg", doubleTy --> doubleTy)
    val double_plus =   monoVar(name "add", doubleTy ** doubleTy --> doubleTy)
    val double_times =  monoVar(name "mul", doubleTy ** doubleTy --> doubleTy)
    end

    local
      fun name a = "char-" ^ a
    in
    val char_gt =       monoVar(name "gt", charTy ** charTy --> boolTy)
    val char_gte =      monoVar(name "gte", charTy ** charTy --> boolTy)
    val char_lt =       monoVar(name "lt", charTy ** charTy --> boolTy)
    val char_lte =      monoVar(name "lte", charTy ** charTy --> boolTy)
    end

    local
      fun name a = "rune-" ^ a
    in
    val rune_gt =       monoVar(name "gt", runeTy ** runeTy --> boolTy)
    val rune_gte =      monoVar(name "gte", runeTy ** runeTy --> boolTy)
    val rune_lt =       monoVar(name "lt", runeTy ** runeTy --> boolTy)
    val rune_lte =      monoVar(name "lte", runeTy ** runeTy --> boolTy)
    end

    local
      fun name a = "string-" ^ a
    in
    val string_gt =     monoVar(name "gt", stringTy ** stringTy --> boolTy)
    val string_gte =    monoVar(name "gte", stringTy ** stringTy --> boolTy)
    val string_lt =     monoVar(name "lt", stringTy ** stringTy --> boolTy)
    val string_lte =    monoVar(name "lte", stringTy ** stringTy --> boolTy)
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

  end
