(* cfa-cfg.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure CFACFG : sig

    val analyze : CFG.module -> unit

    datatype call_sites
      = Unknown				(* possible unknown call sites *)
      | Known of CFG.Label.Set.set	(* only called from known locations; the labels are the *)
					(* entry labels of the functions that call the target *)

    val callSitesOf : CFG.label -> call_sites

    datatype value
      = TOP
      | TUPLE of value list
      | WRAP of value
      | LABELS of CFG.Label.Set.set
      | BOT

    val valueOf : CFG.var -> value

  (* return the set of labels that a control transfer targets; the empty set
   * is used to represent unknown control flow.
   *)
    val labelsOf : CFG.transfer -> CFG.Label.Set.set

    val clearInfo : CFG.module -> unit

  end = struct

    structure LSet = CFG.Label.Set
    structure LMap = CFG.Label.Map

    datatype call_sites
      = Unknown			(* possible unknown call sites *)
      | Known of LSet.set	(* only called from known locations; the labels are the *)
				(* entry labels of the functions that call the target *)

    datatype value
      = TOP
      | TUPLE of value list
      | WRAP of value
      | LABELS of LSet.set
      | BOT

    val {getFn=callSitesOf, clrFn=clrLabel, ...} = CFG.Label.newProp (fn _ => Known(LSet.empty))
    val {getFn=valueOf, clrFn=clrVar, peekFn=peekVar, setFn=setVar} = CFG.Var.newProp (fn _ => BOT)

  (* clear CFA annotations from the variables and labels of a module.  Note that we can
   * restrict the traversal to binding instances.
   *)
    fun clearInfo (CFG.MODULE{code, ...}) = let
	  fun doFunct (CFG.FUNC{lab, entry, body, ...}) = (
		clrLabel lab;
		List.app clrVar (CFG.paramsOfConv entry);
		List.app doExp body)
	  and doExp exp = List.app clrVar (CFG.lhsOfExp exp)
	  in
	    List.app doFunct code
	  end

  (* marks on entry labels to avoid infinite loops in the analysis *)
    local
      val {getFn, setFn, ...} = CFG.Label.newProp(fn _ => 0)
    in
    fun isMarked lab = (getFn lab = 0)
    fun mark lab = setFn(lab, getFn lab + 1)
    fun unmark lab = setFn(lab, getFn lab - 1)
    end

  (* test if a new approximate value is different from an old value; this
   * code assumes that values change according to the lattice order.
   *)
    fun changedValue (new, old) = (case (new, old)
	   of (TOP, TOP) => false
	    | (TOP, _) => true
	    | (BOT, BOT) => false
	    | (_, BOT) => true
	    | (TUPLE vs1, TUPLE vs2) => ListPair.exists changedValue (vs1, vs2)
	    | (WRAP v1, WRAP v2) => changedValue(v1, v2)
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
		TUPLE(ListPair.mapEq (fn (v1, v2) => kJoin(k-1, v1, v2)) (vs1, vs2))
	    | kJoin (k, WRAP v1, WRAP v2) = WRAP(kJoin(k, v1, v2))
	    | kJoin (_, LABELS labs1, LABELS labs2) = LABELS(LSet.union(labs1, labs2))
	    | kJoin _ = raise Fail "type error"
	  in
	    kJoin (maxDepth, v1, v2)
	  end

    fun analyze (CFG.MODULE{code, funcs, ...}) = let
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
	      (* record that a given variable escapes *)
		fun escape x = (case valueOf x
		       of LABELS labs => let
			  (* set the parameters to TOP, since the function escapes *)
			    fun doLab lab = let
				  val SOME(CFG.FUNC{entry, ...}) = CFG.Label.Map.find(funcs, lab)
				  in
				    List.app (fn x => addInfo(x, TOP)) (CFG.paramsOfConv entry)
				  end
			    in
			      CFG.Label.Set.app doLab labs
			    end
			| _ => ()
		      (* end case *))
		fun doFunct (CFG.FUNC{lab, entry, body, exit}, args) = (
		      ListPair.appEq addInfo (CFG.paramsOfConv entry, args);
		      if isMarked lab
			then ()
			else (
			  List.app doExp body;
			  doXfer exit))
		and doExp (CFG.E_Var(xs, ys)) =
		      ListPair.appEq (fn (x, y) => addInfo(x, valueOf y)) (xs, ys)
		  | doExp (CFG.E_Label(x, lab)) = addInfo(x, LABELS(LSet.singleton lab))
		  | doExp (CFG.E_Literal(x, lit)) = ()
		  | doExp (CFG.E_Select(x, i, y)) =
		      addInfo(x, case valueOf y
			 of TUPLE vs => List.nth(vs, i)
			  | BOT => BOT
			  | TOP => TOP
			  | _ => raise Fail "type error"
			(* end case *))
		  | doExp (CFG.E_Alloc(x, xs)) = addInfo(x, TUPLE(List.map valueOf xs))
		  | doExp (CFG.E_Wrap(x, y)) = addInfo(x, WRAP(valueOf y))
		  | doExp (CFG.E_Unwrap(x, y)) =
		      addInfo (x, case valueOf y
			 of WRAP v => v
			  | BOT => BOT
			  | TOP => TOP
			  | _ => raise Fail "type error"
			(* end case *))
		  | doExp (CFG.E_Prim(x, _)) = ()
		  | doExp (CFG.E_CCall(x, _, args)) = List.app escape args
		and doXfer (CFG.StdApply{f, clos, arg, ret, exh}) =
		      doApply (f, [clos, arg, ret, exh])
		  | doXfer (CFG.StdThrow{k, clos, arg}) = doApply (k, [clos, arg])
		  | doXfer (CFG.Apply{f, args}) = doApply (f, args)
		  | doXfer (CFG.Goto jmp) = doJump jmp
		  | doXfer (CFG.If(_, jmp1, jmp2)) = (doJump jmp1; doJump jmp2)
		  | doXfer (CFG.Switch(x, cases, dflt)) = (
		      List.app (doJump o #2) cases;
		      Option.app doJump dflt)
		  | doXfer (CFG.HeapCheck{nogc, ...}) = doJump nogc
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
			| TOP => List.app escape args
			| _ => raise Fail "type error"
		      (* end case *))
		in
(* apply doFunct for each exported function *)
		  !changed
		end
	  fun iterate () = if onePass() then iterate() else ()
	(* compute the call-sites for every label *)
(* SOMETHING HERE *)
	  in
	    iterate ()
(* compute call-side information for labels *)
	  end

  (* return the set of labels that a control transfer targets; the empty set
   * is used to represent unknown control flow.
   *)
    fun labelsOf xfer = (case xfer
	   of CFG.StdApply _ => LSet.empty
	    | CFG.StdThrow _ => LSet.empty
	    | CFG.Apply{f, ...} => (case valueOf f
	       of LABELS s => s
		| _ => LSet.empty	(* can this happen?? *)
	      (* end case *))
	    | _ => LSet.addList(LSet.empty, CFG.labelsOfXfer xfer)
	  (* end case *))

  end
