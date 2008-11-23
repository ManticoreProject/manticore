(* group-funs.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * This module restructures E_Fun bindings so that only mutually
 * recursive definitions are grouped together.
 *)

structure GroupFuns : sig

    val transform : BOM.module -> BOM.module

  end = struct

    structure B = BOM
    structure BV = B.Var
    structure BU = BOMUtil
    structure VSet = BV.Set
    structure VMap = BV.Map
    structure C = Census
    structure ST = Stats


  (********** Counters for statistics **********)
 
    val cntGroup	= ST.newCounter "group-fns:group"
    val cntSplit	= ST.newCounter "group-fns:split"


  (* We compute strongly connected components to group functions *)
    structure SCC = GraphSCCFn (VSet.Key)

    fun nameOfFB (B.FB{f, ...}) = f

  (* the information we collect about a group is a mapping from each
   * function to the pair of the set of other definitions in the group
   * that it mentions and its definition.
   *)
    type group_info = (VSet.set ref * B.lambda) VMap.map

  (* the info stack is a functional map that maps the functions of each
   * enclosing group to the info of the current function of that group.
   *)
    type info_stk = VSet.set ref VMap.map

  (* record a reference to a variable f *)
    fun addRef (stk : info_stk) (f : B.var) = (case VMap.find(stk, f)
	   of SOME(info as ref s) => info := VSet.add(s, f)
	    | NONE => ()
	  (* end case *))

    fun doLambda (stk, B.FB{f, params, exh, body}) = B.FB{
	    f = f, params = params, exh = exh,
	    body = doExp (stk, body)
	  }

    and doFBs (stk, fbs, scopeExp) = let
	  fun doDef (B.FB{f, params, exh, body}, gInfo : group_info) = let
		val infoRef = ref VSet.empty
		val stk' = List.foldl
		      (fn (fb, s) => VMap.insert(s, nameOfFB fb, infoRef))
			stk fbs
		val body' = doExp (stk', body)
		in 
		  VMap.insert (gInfo, f, (infoRef, B.FB{f=f, params=params, exh=exh, body=body'}))
		end
	(* collect info for the function bindings *)
	  val groupInfo = List.foldl doDef VMap.empty fbs
	  fun lookup f = valOf(VMap.find(groupInfo, f))
	(* compute the SCC for the function group *)
	  fun follow f = let val (r, _) = lookup f in VSet.listItems(!r) end
	  val comps = SCC.topOrder' {
		  roots = List.map nameOfFB fbs,
		  follow = follow
		}
	  fun mkFun (SCC.SIMPLE f, e) = B.mkFun([#2(lookup f)], e)
	    | mkFun (SCC.RECURSIVE fs, e) = B.mkFun(List.map (#2 o lookup) fs, e)
	  in
	    ST.tick cntGroup;
	    case comps of (_::_::_) => ST.tick cntSplit | _ => ();
	    List.foldl mkFun scopeExp comps
	  end

    and doExp (stk : info_stk, e0 as B.E_Pt(_, t)) = (case t
	   of B.E_Let(xs, e1, e2) => B.mkLet(xs, doExp(stk, e1), doExp(stk, e2))
	    | B.E_Stmt(xs, rhs, e) => (
		BU.appRHS (addRef stk) rhs;  (* check rhs for references *)
		B.mkStmt(xs, rhs, doExp(stk, e)))
	    | B.E_Fun([fb], e) => B.mkFun([doLambda(stk, fb)], doExp(stk, e))
	    | B.E_Fun(fbs, e) => doFBs (stk, fbs, doExp(stk, e))
	    | B.E_Cont(fb, e) => B.mkCont(doLambda(stk, fb), doExp(stk, e))
	    | B.E_If(x, e1, e2) => (
	      (* NOTE: x cannot be a function reference *)
		B.mkIf(x, doExp(stk, e1), doExp(stk, e2)))
	    | B.E_Case(x, cases, dflt) => let
		fun doCase (p, e) = (p, doExp(stk, e))
		val dflt = Option.map (fn e => doExp(stk, e)) dflt
		in
		(* NOTE: x cannot be a function reference *)
		  B.mkCase(x, List.map doCase cases, dflt)
		end
	    | B.E_Apply(f, xs, ys) => (
		List.app (addRef stk) (f::xs);
		List.app (addRef stk) ys;
		e0)
	    | B.E_Throw(_, xs) => (List.app (addRef stk) xs; e0)
	    | B.E_Ret xs => (List.app (addRef stk) xs; e0)
	    | B.E_HLOp(_, xs, ys) => (
		List.app (addRef stk) xs;
		List.app (addRef stk) ys;
		e0)
	  (* end case *))

    fun transform (B.MODULE{name, externs, hlops, body}) = let
	  val body = doLambda (VMap.empty, body)
	  in
	    B.MODULE{
		name = name,
		externs = externs,
		hlops = hlops,
		body = body
	      }
	  end

  end
