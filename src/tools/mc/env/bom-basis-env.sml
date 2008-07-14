(* std-env.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Mapping from AST types and variables to their BOL representations.
 *
 *)

structure BOMBasisEnv : sig

    val bindingEnv : BindingEnv.bom_env

(*    val translateEnv : TranslateEnv.env*)

  end = struct

    structure B = Basis
    structure BTy = BOMTy
    structure BEnv = BindingEnv

    fun wrapTy rty = BOMTyUtil.wrap(BTy.T_Raw rty)

    val types = [
	    (B.boolTyc,		BTy.K_UNBOXED,	BTy.boolTy),
	    (B.intTyc,		BTy.K_BOXED,	wrapTy BTy.T_Int),
	    (B.longTyc,		BTy.K_BOXED,	wrapTy BTy.T_Long),
	    (B.floatTyc,	BTy.K_BOXED,	wrapTy BTy.T_Float),
	    (B.doubleTyc,	BTy.K_BOXED,	wrapTy BTy.T_Double),
	    (B.stringTyc,	BTy.K_BOXED,	BOMBasis.stringTy),
	    (B.listTyc,		BTy.K_UNIFORM,	BOMBasis.listTy),
	    (B.optionTyc,	BTy.K_UNIFORM,	BOMBasis.optionTy),
	    (B.exnTyc,		BTy.K_BOXED,	BTy.exnTy),
	    (B.threadIdTyc,	BOMTyUtil.kindOf(BTy.tidTy), BTy.tidTy),
	    (B.parrayTyc,       BTy.K_BOXED,	BOMBasis.ropeTy),
            (B.ivarTyc,         BTy.K_BOXED,   BOMBasis.ivarTy),
(*
	    (B.mvarTyc, ),
*)
	    (B.eventTyc,	BTy.K_BOXED,	BOMBasis.evtTy),
	    (B.chanTyc,		BTy.K_BOXED,	BOMBasis.chanTy),
	  (* internal types *)
(*	    (F.futureTyc,       BTy.K_BOXED,	BTy.futureTy),*)
(*	    (R.ropeTyc,         BTy.K_BOXED,	BOMBasis.ropeTy),*)
          (* arrays *)
	    (B.arrayTyc,        BTy.K_BOXED,    BTy.T_Any)
	  ]

    val bindingEnv = BEnv.emptyBOMEnv

  end (* BOMBasisEnv *)
