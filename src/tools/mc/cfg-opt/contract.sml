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

  (* return the block that the label is bound to.  This function should
   * only be called on locally defined labels (e.g., the target of gotos)!
   *)
    fun lookupBlock lab = (case C.Label.kindOf lab
           of C.LK_Block block => block
	    | _ => raise Fail(concat["lookupBlock(", C.Label.toString lab, ")"])
	  (* end case *))

  (* return true if a label is a locally defined block *)
    fun isLocalLabel lab = (case C.Label.kindOf lab
	   of C.LK_Block _ => true
	    | _ => false
	  (* end case *))

    fun contractExit (env, xfer) = let
	(* contract a jump: rename its arguments and do jump-chain elimination *)
	  fun contractJump (lab, args) = let
		fun chainElim (lab, args) = (case lookupBlock lab
		       of C.BLK{
                            body=[], args=params, exit=C.Goto(labOut, argsOut), ...
                          } => let
			  (* To ensure that the order of the arguments is correct, we need to
			   * set up a mapping from params to args and apply it to argsOut.  For
			   * example, if the args are "(x, y, z)", the params are "(a, b, c)",
			   * and the argsOut are "(b, a, c)", then the new arguments should
			   * be "(y, x, z)".
			   *)
			    val s = ListPair.foldlEq (fn (p, a, s) => VMap.insert(s, p, a))
				  VMap.empty (params, args)
			    in
			      ST.tick cntJumpChain;
			      Census.decLab lab;
			      Census.incLab labOut;
			      chainElim (labOut, applySubst' (s, argsOut))
			    end
			| _ => (lab, args)
		      (* end case *))
		in
		  chainElim (lab, applySubst' (env, args))
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
	      | C.Apply{f, clos, args} => C.Apply{
		    f = applySubst(env, f),
                    clos = applySubst(env, clos),
		    args = applySubst'(env, args)
		  }
	      | C.Goto jmp => C.Goto(contractJump jmp)
	      | C.If(cond, jmp1, jmp2) =>
		  C.If(CondUtil.map (fn x => applySubst(env, x)) cond,
		    contractJump jmp1,
		    contractJump jmp2)
	      | C.Switch(x, cases, dflt) =>
		  C.Switch(applySubst(env, x),
		    List.map (fn (tag, jmp) => (tag, contractJump jmp)) cases,
		    Option.map contractJump dflt)
	      | C.HeapCheck{hck, szb, nogc} => C.HeapCheck{hck=hck, szb=szb, nogc=contractJump nogc}
	      | C.AllocCCall{lhs, f, args, ret} => 
                   C.AllocCCall{lhs=lhs, f=applySubst(env, f), args=applySubst'(env, args), ret=contractJump ret}
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
	      | C.E_Const(x, lit, ty) => let
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
		      else (C.mkCast(x, ty, y) :: rest, exit)
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
		      else (C.mkSelect(x, i, y) :: rest, exit)
		  end
	      | C.E_Update(i, x, y) => let
		  val (rest, exit) = doRest env
		  in
		    (C.mkUpdate(i, applySubst(env, x), applySubst(env, y)) :: rest, exit)
		  end
	      | C.E_AddrOf(x, i, y) => let
		  val y = applySubst (env, y)
		  val (rest, exit) = doRest env
		  in
		    if unused x
		      then (ST.tick cntUnusedVar; Census.dec y; (rest, exit))
		      else (C.mkAddrOf(x, i, y) :: rest, exit)
		  end
	      | C.E_Alloc(x, ty, ys) => let
		  val ys = applySubst' (env, ys)
		  val (rest, exit) = doRest env
		  in
		    if unused x
		      then (ST.tick cntUnusedVar; Census.dec' ys; (rest, exit))
		      else (C.mkAlloc(x, ty, ys) :: rest, exit)
		  end
	      | C.E_AllocSpecial(x, ty, ys) => let
		  val ys = applySubst' (env, ys)
		  val (rest, exit) = doRest env
		  in
		    if unused x
		      then (ST.tick cntUnusedVar; Census.dec' ys; (rest, exit))
		      else (C.mkAllocSpecial(x, ty, ys) :: rest, exit)
		  end
	      | C.E_GAlloc(x, ty, ys) => let
		  val ys = applySubst' (env, ys)
		  val (rest, exit) = doRest env
		  in
		    if unused x
		      then (ST.tick cntUnusedVar; Census.dec' ys; (rest, exit))
		      else (C.mkGAlloc(x, ty, ys) :: rest, exit)
		  end
	      | C.E_Promote(x, y) => let
		  val y = applySubst (env, y)
		  val (rest, exit) = doRest env
		  in
		    if unused x
		      then (ST.tick cntUnusedVar; Census.dec y; (rest, exit))
		      else (C.mkPromote(x, y) :: rest, exit)
		  end
	      | C.E_Prim0 prim => let
		  val (rest, exit) = doRest env
		  in
		    (C.mkPrim0 prim :: rest, exit)
		  end
	      | C.E_Prim(x, prim) => let
		  val (rest, exit) = doRest env
		  in
		    if unused x andalso PrimUtil.isPure prim
		      then (
			ST.tick cntUnusedVar;
			PrimUtil.app (fn x => (Census.dec(applySubst(env, x)))) prim;
			(rest, exit))
		      else (C.mkPrim(x, PrimUtil.map (fn y => applySubst(env, y)) prim)
			:: rest, exit)
		  end
	      | C.E_CCall(lhs, cf, args) => let
		  val (rest, exit) = doRest env
		  in
		  (* NOTE: we could eliminate pure C calls whose results are unused,
		   * but that situation is not very likely at this stage.
		   *)
		    (C.mkCCall(lhs, applySubst(env, cf), applySubst'(env, args))
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
		      else (C.mkVPLoad(x, i, y) :: rest, exit)
		  end
	      | C.E_VPStore(i, x, y) => let
		  val (rest, exit) = doRest env
		  in
		    (C.mkVPStore(i, applySubst(env, x), applySubst(env, y)) :: rest, exit)
		  end
	      | C.E_VPAddr(x, i, y) => let
		  val y = applySubst (env, y)
		  val (rest, exit) = doRest env
		  in
		    if unused x
		      then (ST.tick cntUnusedVar; Census.dec y; (rest, exit))
		      else (C.mkVPAddr(x, i, y) :: rest, exit)
		  end
	    (* end case *)
	  end

  (* delete a function, which includes decrementing the use counts of any labels *)
    fun deleteFunc (C.FUNC{start, body, ...}) = let
	  fun deleteExp (C.E_Label(_, lab)) = Census.decLab lab
	    | deleteExp _ = ()
          and deleteBlock (C.BLK{body, exit, ...}) = (
              ST.tick cntUnusedBlock;
              List.app deleteExp body;
              deleteExit exit)
          and deleteExit (exit) = (case exit
	     of C.StdApply _ => ()
	      | C.StdThrow _ => ()
	      | C.Apply _ => ()
	      | C.Goto jmp => deleteJump jmp
	      | C.If(_, jmp1, jmp2) => (deleteJump jmp1; deleteJump jmp2)
	      | C.Switch(x, cases, dflt) => (
		  List.app (fn (_, jmp) => deleteJump jmp) cases;
		  Option.app deleteJump dflt)
	      | C.HeapCheck{hck, szb, nogc} => deleteJump nogc
	      | C.AllocCCall{lhs, f, args, ret} => deleteJump ret
	    (* end case *))
	  and deleteJump (lab, _) = Census.decLab lab
	  in
            deleteBlock start;
            List.app deleteBlock body
	  end

  (* contract a function *)
    fun contractFunc (func as C.FUNC{lab, entry, start, body}) = let
        fun contractBlock (block as C.BLK{lab, args, body, exit}) = let
            val (body,exit) = contractExps (VMap.empty, body, exit)
        in
            C.BLK{lab=lab, args=args, body=body, exit=exit}
        end
    in
	  if (isLocalLabel lab) andalso (CL.useCount lab = 0)
	    then (deleteFunc func; NONE)
	    else let
              val start = contractBlock start
              val body = List.map contractBlock body
	      in
		SOME(C.FUNC{lab=lab, entry=entry, start=start, body=body})
	      end
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
