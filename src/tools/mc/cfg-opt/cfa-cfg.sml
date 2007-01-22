(* cfa-cfg.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure CFACFG : sig

    val analyse : CFG.module -> unit

    datatype call_sites
      = Unknwn				(* possible unknown call sites *)
      | Known of CFG.Label.Set.set	(* called from known locations *)

    val callSitesOf : CFG.label -> call_sites

    datatype value
      = TOP
      | TUPLE of CFG.ty * value list
      | LABELS of CFG.Label.Set.set
      | BOT

    val valueOf : CFG.var -> value

    val clearInfo : CFG.module -> unit

  end = struct

    structure LSet = CFG.Label.Set.set

    datatype call_sites
      = Unknwn				(* possible unknown call sites *)
      | Known of LSet.set	(* called from known locations *)

    datatype value
      = TOP
      | TUPLE of CFG.ty * value list
      | LABELS of LSet.set
      | BOT

    val {getFn=callSitesOf, ...} = CFG.Label.newProp (fn _ => Known(LSet.empty))
    val {getFn=valueOf, ...} = CFG.Var.newProt (fn _ => BOT)

  (* marks on entry labels to avoid infinite loops in the analysis *)
    local
      val {getFn, setFn} = CFG.Label.newProp(fn _ => 0)
    in
    fun isMarked lab = (getFn x = 0)
    fun mark lab = setFn(lab, getFn x + 1)
    fun unmark lab = setFn(lab, getFn x - 1)
    end

  (* test if a new approximate value is different from an old value; this
   * code assumes that values change according to the lattice order.
   *)
    fun changedValue (new, old) = (case (new, old)
	   of (TOP, TOP) => false
	    | (TOP, _) => true
	    | (BOT, BOT) => false
	    | (_, BOT) => true
	    | (TUPLE(_, vs1), TUPLE(_, vs2)) =>
		ListPairEq.exists changedValue (vs1, vs2)
	    | (LABELS s1, LABELS s2) => if (LSet.numItems s1 > LSet.numItems s2)
		then true
		else false
	    | _ => raise Fail "non-monotonic change"
	  (* end case *))

    val maxDepth = 3

    fun joinValues (v1, v2) = let
	  fun kJoin (0, _, _) = TOP
	    | kJoin (_, TOP, _) = TOP
	    | kJoin (_, _, TOP) = TOP
	    | kJoin (_, BOT, v) = v
	    | kJoin (_, v, BOT) = v
	    | kJoin (k, TUPLE vs1, TUPLE vs2) =
		TUPLE(ListPair.mapEq (fn (v1, v2) => kJoin(k-1, v1, v2)) (v1s, v2s))
	    | kJoin (_, LABELS labs1, LABELS labs2) => LABELS(LSet.union(labs1, labs2))
	    | kJoin _ = raise Fail "type error"
	  in
	    kJoin (maxDepth, v1, v2)
	  end

    fun analyse (CFG.MODULE{code, funcs, ...}) = let
	  fun onePass () = let
		val changed = ref false
	      (* update the approximate value of a variable by some delta and record if
	       * it changed.
	       *)
		fun addInfo (x, BOT) = ()
		  | addInfo (x, v) = (case peekVar x
		       of NONE => setVar(x, v)
			| SOME oldV => let
			    val newV = joinValues(oldV, v)
			    in
			      if changedValue(newV, oldV)
				then (changed := true; setVar(x, newV))
				else ()
			    end
		      (* end case *))
		fun doFunct (CFG.FUNC{lab, params, ...}, args) = (
		      ListPair.appEq addInfo (params, args);
		      if marked lab
			then ()
			else doExp body)
		and doExp (CFG.Exp(_, e)) = (case e
		       of CFG.E_Let(lhs, rhs, e) => (
			    case (lhs, rhs)
			     of ([x], CFG.E_Var y) => updateInfo(x, valueOf y)
			      | ([x], CFG.E_Label lab) =>
				  updateInfo(x, LABELS(LSet.singleton lab))
			      | ([x], CFG.E_Select(i, y)) => (case valueOf y
				   of TUPLE(_, vs) => List.ith(vs, i)
				    | BOT => BOT
				    | TOP => TOP
				    | _ => raise Fail "type error"
				  (* end case *))
			      | ([x], CFG.E_Alloc(ty, xs) =>
				  updateInfo(x, TUPLE(ty, List.map valueOf xs))
			      | (_, CFG.E_Prim _) => ()
			      | (_, CFG.E_CCall(cf, args)) => ??
			    (* end case *);
			    doExp e)
			| CFG.E_HeapCheck(_, e) => doExp e
(* NOTE: if we track booleans, then we can test the condition *)
			| CFG.E_If(_, jmp1, jmp2) => (
			    doJump jmp1;
			    doJump jmp2)
			| CFG.E_Switch(_, cases, dflt) => (
			    List.app (doJump o #2) cases;
			    Option.app doJump dflt)
			| CFG.E_Apply apply => doApply apply
			| CFG.E_Throw apply => doApply apply
			| CFG.E_Goto jmp => doJump jmp
		      (* end case *))
		and doJump (lab, args) = (case LMap.find(funcs, lab)
		       of NONE => raise Fail "jump to unknown label"
			| SOME f => (
			    mark lab;
			    doFunct (f, List.map valueOf args);
			    unmark lab)
		      (* end case *))
		and doApply (f, args) = (case valueOf f
		       of LABELS targets => LSet.app (fn lab => doJump(lab, args)) targets
			| BOT => ()
			| TOP => (* the args escape! *)
		      (* end case *))
		in
(* apply doFunct for each exported function *);
		  !changed
		end
	  fun iterate () = if onePass() then iterate() else ()
	  in
	    iterate ()
(* compute call-side information for labels *)
	  end

  end
