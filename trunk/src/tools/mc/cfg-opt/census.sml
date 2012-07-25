(* census.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure Census : sig

    val census : CFG.module -> unit

  (* modify the use counts of variables *)
    val inc : CFG.Var.var -> unit
    val inc' : CFG.Var.var list -> unit
    val dec : CFG.Var.var -> unit
    val dec' : CFG.Var.var list -> unit

  (* modify the use counts of labels *)
    val incLab : CFG.Label.var -> unit
    val decLab : CFG.Label.var -> unit

  end = struct

    structure C = CFG

    val clr = C.Var.clrCount
    val clr' = List.app clr
    fun inc x = C.Var.addToCount(x, 1)
    fun dec x = C.Var.addToCount(x, ~1)
    val inc' = List.app inc
    val dec' = List.app dec

    val clrLab = C.Label.clrCount
    fun incLab lab = C.Label.addToCount(lab, 1)
    fun decLab lab = C.Label.addToCount(lab, ~1)

  (* update the census counts for the variables bound in an entry convention *)
    fun doEntry (C.StdFunc{clos, ret, exh}) = (clr clos; clr ret; clr exh)
      | doEntry (C.StdCont{clos}) = (clr clos)
      | doEntry (C.KnownFunc{clos}) = (clr clos)

  (* update the census counts for the variables in an expression *)
    fun doExp (C.E_Var(xs, ys)) = (clr' xs; inc' ys)
      | doExp (C.E_Const(x, _, _)) = clr x
      | doExp (C.E_Cast(x, _, y)) = (clr x; inc y)
      | doExp (C.E_Label(x, lab)) = (clr x; incLab lab)
      | doExp (C.E_Select(x, _, y)) = (clr x; inc y)
      | doExp (C.E_Update(_, x, y)) = (inc x; inc y)
      | doExp (C.E_AddrOf(x, _, y)) = (clr x; inc y)
      | doExp (C.E_Alloc(x, _, ys)) = (clr x; inc' ys)
      | doExp (C.E_GAlloc(x, _, ys)) = (clr x; inc' ys)
      | doExp (C.E_Promote(x, y)) = (clr x; inc y)
      | doExp (C.E_Prim0 p) = PrimUtil.app inc p
      | doExp (C.E_Prim(x, p)) = (clr x; PrimUtil.app inc p)
      | doExp (C.E_CCall(xs, cf, ys)) = (clr' xs; inc cf; inc' ys)
      | doExp (C.E_HostVProc x) = clr x
      | doExp (C.E_VPLoad(x, _, y)) = (clr x; inc y)
      | doExp (C.E_VPStore(_, x, y)) = (inc x; inc y)
      | doExp (C.E_VPAddr(x, _, y)) = (clr x; inc y)

  (* update the census counts for the variables used in a jump *)
    fun doJump (lab, args) = (incLab lab; inc' args)

  (* update the census counts for the variables in a exit transfer *)
    fun doExit (C.StdApply{f, clos, args, ret, exh}) = (inc f; inc clos; inc' args; inc ret; inc exh)
      | doExit (C.StdThrow{k, clos, args}) = (inc k; inc clos; inc' args)
      | doExit (C.Apply{f, clos, args}) = (inc f; inc clos; inc' args)
      | doExit (C.Goto jmp) = doJump jmp
      | doExit (C.If(cond, jmp1, jmp2)) = (CondUtil.app inc cond; doJump jmp1; doJump jmp2)
      | doExit (C.Switch(x, cases, dflt)) = (
	  inc x;
	  List.app (fn (_, jmp) => doJump jmp) cases;
	  Option.app doJump dflt)
      | doExit (C.HeapCheck{nogc, ...}) = doJump nogc
      | doExit (C.HeapCheckN{nogc, n, ...}) = (inc n; doJump nogc)
      | doExit (C.AllocCCall{lhs, f, args, ret}) = (List.app inc lhs; inc f; inc' args; doJump ret)

  (* initialize the census count of a function's label *)
    fun initFun (C.FUNC{lab, start, body, ...}) = let
        fun initBlk (block as CFG.BLK{args,...}) = clr' args
    in
        initBlk start;
        List.app initBlk body;
	  clrLab lab;
	  case C.Label.kindOf lab
	   of C.LK_Func{export = SOME _, ...} => incLab lab
	    | _ => ()
	  (* end case *)
    end

  (* update the census counts for the variables in a function (basic block) *)
    fun doFun (C.FUNC{entry, start, body , ...}) = let
        fun doBlk (block as CFG.BLK{body,args,exit,...}) = (
            List.app doExp body;
	    doExit exit)
    in
	  doEntry entry;
          doBlk start;
          List.app doBlk body
    end

    fun census (C.MODULE{externs, code, ...}) =  let
	  fun clrCFun cf = clrLab(CFunctions.varOf cf)
	  in
	    List.app clrCFun externs;
	    List.app initFun code;
	    List.app doFun code
	  end

  end
