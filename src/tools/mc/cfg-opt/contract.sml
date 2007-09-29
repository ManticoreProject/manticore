(* contract.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure Contract : sig

    val transform : CFG.module -> CFG.module

  end = struct

    structure C = CFG
    structure CV = C.Var
    structure CL = C.Label
    structure ST = Stats
    structure VMap = CV.Map
    structure LTbl = C.Label.Tbl

  (********** Counters for statistics **********)
    val cntJumpChain		= ST.newCounter "cfg-contract:jump-chain"
    val cntVarRename		= ST.newCounter "cfg-contract:var-rename"
    val cntUnusedVar		= ST.newCounter "cfg-contract:unused-var"
    val cntUnusedBlock		= ST.newCounter "cfg-contract:unused-block"
  (* first and last counters *)
    val firstCounter		= cntJumpChain
    val lastCounter		= cntUnusedBlock
  (* these counters track the number of contraction phases/iterations *)
    val cntPhases		= ST.newCounter "cfg-contract:phases"
    val cntIters		= ST.newCounter "cfg-contract:iterations"

    fun unused x = (CV.useCount x = 0)

  (* Given the binding
   *	x = y
   * we can replace uses of x by y, which means that y's count increases
   * by x's count - 1.
   *)
    fun mergeUseCounts (VarRep.V{useCnt, ...}, y) = CV.addToCount(y, !useCnt - 1)

  (* Variable renaming *)
    fun rename (env, x, y) = VMap.insert(env, x, y)

    fun rename' (env, [], []) = env
      | rename' (env, x::xs, y::ys) = rename' (VMap.insert(env, x, y), xs, ys)
      | rename' _ = raise Fail "rename': arity mismatch"

    fun applySubst (env, x) = (case VMap.find(env, x)
	   of SOME y => y
	    | NONE => x
	  (* end case *))

    fun applySubst' (env, []) = []
      | applySubst' (env, x::xs) = applySubst(env, x) :: applySubst'(env, xs)

  (* return the function that the label is bound to.  This function should
   * only be called on locally defined labels (e.g., the target of gotos)!
   *)
    fun lookupFunc lab = (case C.Label.kindOf lab
	   of C.LK_Local{func, ...} => func
	    | _ => raise Fail(concat["lookupFunc(", C.Label.toString lab, ")"])
	  (* end case *))

  (* return true if a label is locally defined and not exported *)
    fun isLocalLabel lab = (case C.Label.kindOf lab
	   of C.LK_Local{export=NONE, ...} => true
	    | _ => false
	  (* end case *))

    fun contractExit (env, xfer) = let
	(* contract a jump: rename its arguments and do jump-chain elimination *)
	  fun contractJump (lab, args) = let
		val args = applySubst' (env, args)
		fun chainElim targetLab = (case lookupFunc targetLab
		       of C.FUNC{body=[], exit=C.Goto(lab, _), ...} => (
			    ST.tick cntJumpChain;
			    Census.decLab lab;
			    chainElim lab)
			| _ => targetLab
		      (* end case *))
		in
		  (chainElim lab, args)
		end
	  in
	    case xfer
	     of C.StdApply{f, clos, args, ret, exh} => C.StdApply{
		    f = applySubst(env, f),
		    clos = applySubst(env, clos),
		    args = applySubst'(env, args),
		    ret = applySubst(env, ret),
		    exh = applySubst(env, exh)
		  }
	      | C.StdThrow{k, clos, args} => C.StdThrow{
		    k = applySubst(env, k),
		    clos = applySubst(env, clos),
		    args = applySubst'(env, args)
		  }
	      | C.Apply{f, args} => C.Apply{
		    f = applySubst(env, f),
		    args = applySubst'(env, args)
		  }
	      | C.Goto jmp => C.Goto(contractJump jmp)
	      | C.If(x, jmp1, jmp2) =>
		  C.If(applySubst(env, x), contractJump jmp1, contractJump jmp2)
	      | C.Switch(x, cases, dflt) =>
		  C.Switch(applySubst(env, x),
		    List.map (fn (tag, jmp) => (tag, contractJump jmp)) cases,
		    Option.map contractJump dflt)
	      | C.HeapCheck{szb, nogc} => C.HeapCheck{szb=szb, nogc=contractJump nogc}
	    (* end case *)
	  end

  (* contract a sequence of expressions. *)
    fun contractExps (env, [], exit) = ([], contractExit(env, exit))
      | contractExps (env, e::es, exit) = let
	  fun doRest env = contractExps(env, es, exit)
	  in
	    case e
	     of C.E_Var(lhs, rhs) => (
		  ST.tick cntVarRename;
		  ListPair.appEq mergeUseCounts (lhs, rhs);
		  doRest (rename' (env, lhs, rhs)))
	      | C.E_Const(x, lit) => let
		  val (rest, exit) = doRest env
		  in
		    if unused x
		      then (ST.tick cntUnusedVar; (rest, exit))
		      else (e :: rest, exit)
		  end
	      | C.E_Cast(x, ty, y) => let
		  val y = applySubst (env, y)
		  val (rest, exit) = doRest env
		  in
		    if unused x
		      then (ST.tick cntUnusedVar; Census.dec y; (rest, exit))
		      else (C.E_Cast(x, ty, y) :: rest, exit)
		  end
	      | C.E_Label(x, lab) => let
		  val (rest, exit) = doRest env
		  in
		    if unused x
		      then (ST.tick cntUnusedVar; Census.decLab lab; (rest, exit))
		      else (e :: rest, exit)
		  end
	      | C.E_Select(x, i, y) => let
		  val y = applySubst (env, y)
		  val (rest, exit) = doRest env
		  in
		    if unused x
		      then (ST.tick cntUnusedVar; Census.dec y; (rest, exit))
		      else (C.E_Select(x, i, y) :: rest, exit)
		  end
	      | C.E_Update(i, x, y) => let
		  val (rest, exit) = doRest env
		  in
		    (C.E_Update(i, applySubst(env, x), applySubst(env, y)) :: rest, exit)
		  end
	      | C.E_AddrOf(x, i, y) => let
		  val y = applySubst (env, y)
		  val (rest, exit) = doRest env
		  in
		    if unused x
		      then (ST.tick cntUnusedVar; Census.dec y; (rest, exit))
		      else (C.E_AddrOf(x, i, y) :: rest, exit)
		  end
	      | C.E_Alloc(x, ys) => let
		  val ys = applySubst' (env, ys)
		  val (rest, exit) = doRest env
		  in
		    if unused x
		      then (ST.tick cntUnusedVar; Census.dec' ys; (rest, exit))
		      else (C.E_Alloc(x, ys) :: rest, exit)
		  end
	      | C.E_Wrap(x, y) => let
		  val y = applySubst (env, y)
		  val (rest, exit) = doRest env
		  in
		    if unused x
		      then (ST.tick cntUnusedVar; Census.dec y; (rest, exit))
		      else (C.E_Wrap(x, y) :: rest, exit)
		  end
	      | C.E_Unwrap(x, y) => let
		  val y = applySubst (env, y)
		  val (rest, exit) = doRest env
		  in
		    if unused x
		      then (ST.tick cntUnusedVar; Census.dec y; (rest, exit))
		      else (C.E_Unwrap(x, y) :: rest, exit)
		  end
	      | C.E_Prim(x, prim) => let
		  val (rest, exit) = doRest env
		  in
		    if unused x andalso PrimUtil.isPure prim
		      then (
			ST.tick cntUnusedVar;
			PrimUtil.app (fn x => (Census.dec(applySubst(env, x)))) prim;
			(rest, exit))
		      else (C.E_Prim(x, PrimUtil.map (fn y => applySubst(env, y)) prim)
			:: rest, exit)
		  end
	      | C.E_CCall(lhs, cf, args) => let
		  val (rest, exit) = doRest env
		  in
		  (* NOTE: we could eliminate pure C calls whose results are unused,
		   * but that situation is not very likely at this stage.
		   *)
		    (C.E_CCall(lhs, applySubst(env, cf), applySubst'(env, args))
		      :: rest, exit)
		  end
	      | C.E_HostVProc x => let
		  val (rest, exit) = doRest env
		  in
		    if unused x
		      then (ST.tick cntUnusedVar; (rest, exit))
		      else (e :: rest, exit)
		  end
	      | C.E_VPLoad(x, i, y) => let
		  val y = applySubst (env, y)
		  val (rest, exit) = doRest env
		  in
		    if unused x
		      then (ST.tick cntUnusedVar; Census.dec y; (rest, exit))
		      else (C.E_VPLoad(x, i, y) :: rest, exit)
		  end
	      | C.E_VPStore(i, x, y) => let
		  val (rest, exit) = doRest env
		  in
		    (C.E_VPStore(i, applySubst(env, x), applySubst(env, y)) :: rest, exit)
		  end
	    (* end case *)
	  end

  (* delete a function, which includes decrementing the use counts of any labels *)
    fun deleteFunc (C.FUNC{body, exit, ...}) = let
	  fun deleteExp (C.E_Label(_, lab)) = Census.decLab lab
	    | deleteExp _ = ()
	  fun deleteJump (lab, _) = Census.decLab lab
	  in
	    ST.tick cntUnusedBlock;
	    List.app deleteExp body;
	    case exit
	     of C.StdApply _ => ()
	      | C.StdThrow _ => ()
	      | C.Apply _ => ()
	      | C.Goto jmp => deleteJump jmp
	      | C.If(_, jmp1, jmp2) => (deleteJump jmp1; deleteJump jmp2)
	      | C.Switch(x, cases, dflt) => (
		  List.app (fn (_, jmp) => deleteJump jmp) cases;
		  Option.app deleteJump dflt)
	      | C.HeapCheck{szb, nogc} => deleteJump nogc
	    (* end case *)
	  end

  (* contract a function *)
    fun contractFunc (func as C.FUNC{lab, entry, body, exit}) =
	  if (isLocalLabel lab) andalso (CL.useCount lab = 0)
	    then (deleteFunc func; NONE)
	    else let
	      val (body, exit) = contractExps(VMap.empty, body, exit)
	      in
		SOME(C.FUNC{lab=lab, entry=entry, body=body, exit=exit})
	      end

    fun transform (C.MODULE{name, externs, code}) = let
	(* iterate contraction until we reach a fixed point *)
	  fun ticks () = ST.sum {from = firstCounter, to = lastCounter}
	  fun loop (body, prevSum) = let
		val _ = ST.tick cntIters
		val body = List.mapPartial contractFunc body
		val sum = ticks()
		in
		  if (prevSum <> sum)
		    then loop (body, sum)
		    else body
		end
	  val code = loop (code, ticks())
	  in
	    ST.tick cntPhases;
	    C.MODULE{name = name, externs = externs, code = code}
	  end

  end
