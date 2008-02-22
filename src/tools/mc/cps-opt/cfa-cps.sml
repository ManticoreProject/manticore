(* cfa-ps.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure CFACPS : sig

    val analyze : CPS.module -> unit

    datatype call_sites
      = Unknown				(* possible unknown call sites *)
      | Known of CPS.Var.Set.set	(* only called from known locations; the variables *)
                                        (* are the binding labels of the lambdas that call *)
                                        (* the target *)

    val callSitesOf : CPS.var -> call_sites

    datatype value
      = TOP
      | TUPLE of value list
      | LAMBDAS of CPS.Var.Set.set
      | BOT

    val valueOf : CPS.var -> value

  (* return true if the given lambda variable escapes *)
    val isEscaping : CPS.var -> bool

    val clearInfo : CPS.module -> unit

  end = struct

    val debugFlg = ref false
    val resultsFlg = ref false
    val () = List.app (fn ctl => ControlRegistry.register CPSOptControls.registry {
	      ctl = Controls.stringControl ControlUtil.Cvt.bool ctl,
	      envName = NONE
	    }) [
	      Controls.control {
		  ctl = debugFlg,
		  name = "cfa-debug",
		  pri = [0, 1],
		  obscurity = 0,
		  help = "debug cfa"
		},
	      Controls.control {
		  ctl = resultsFlg,
		  name = "cfa-results",
		  pri = [0, 1],
		  obscurity = 0,
		  help = "print results of cfa"
		}
	    ]

    structure VSet = CPS.Var.Set

    datatype call_sites
      = Unknown			(* possible unknown call sites *)
      | Known of VSet.set	(* only called from known locations; the labels are the *)
				(* entry labels of the functions that call the target *)

    fun callSitesToString v = let
	  fun v2s (Unknown, l) = "?" :: l
	    | v2s (Known s, l) = let
		fun f [] = "}" :: l
		  | f [x] = CPS.Var.toString x :: "}" :: l
		  | f (x::r) = CPS.Var.toString x :: "," :: f r
		in
		  "{" :: f (VSet.listItems s)
		end
	  in
	    concat (v2s(v, []))
	  end

    datatype value
      = TOP
      | TUPLE of value list
      | LAMBDAS of VSet.set
      | BOT

    fun valueToString v = let
	  fun v2s (TOP, l) = "T" :: l
	    | v2s (TUPLE[], l) = "()" :: l
	    | v2s (TUPLE[v], l) = "(" :: v2s (v, ")" :: l)
	    | v2s (TUPLE(v::r), l) =
		"(" :: v2s (v, List.foldr (fn (v, l) => "," :: v2s(v, l)) (")" :: l) r)
	    | v2s (LAMBDAS s, l) = let
		fun f [] = "}" :: l
		  | f [x] = CPS.Var.toString x :: "}" :: l
		  | f (x::r) = CPS.Var.toString x :: "," :: f r
		in
		  "{" :: f (VSet.listItems s)
		end
	    | v2s (BOT, l) = "#" :: l
	  in
	    concat (v2s(v, []))
	  end

  (* property to track call-sites *)
    val {getFn=getSites, clrFn=clrSites, setFn=setSites, ...} =
	  CPS.Var.newProp (fn _ => Known(VSet.empty))
  (* property to track the estimated value of variables *)
    val {getFn=valueOf, clrFn=clrVar, peekFn=peekVar, setFn=setVar} =
	  CPS.Var.newProp (fn _ => BOT)

  (* *)
    val callSitesOf = getSites
  (* return true if the given label escapes *)
    fun isEscaping lab = (case callSitesOf lab of Unknown => true | _ => false)

  (* clear CFA annotations from the variables of a module.  Note that we can
   * restrict the traversal to binding instances.
   *)
    fun clearInfo (CPS.MODULE{body, ...}) = let
          fun doLambda (CPS.FB {f, params, rets, body}) = (
                clrSites f;
                clrVar f;
                List.app clrVar params;
                List.app clrVar rets;
                doExp body)
          and doExp e = (case e 
                 of CPS.Let (xs, _, e) => (List.app clrVar xs; doExp e)
                  | CPS.Fun (fbs, e) => (List.app doLambda fbs; doExp e)
                  | CPS.Cont (fb, e) => (doLambda fb; doExp e)
                  | CPS.If (_, e1, e2) => (doExp e1; doExp e2)
                  | CPS.Switch (_, cases, dflt) => 
                      (List.app (doExp o #2) cases; Option.app doExp dflt)
                  | CPS.Apply _ => ()
                  | CPS.Throw _ => ()
	        (* end case *))
	  in
	    doLambda body
	  end

  (* test if a new approximate value is different from an old value; this
   * code assumes that values change according to the lattice order.
   *)
    fun changedValue (new, old) = (case (new, old)
	   of (TOP, TOP) => false
	    | (TOP, _) => true
	    | (BOT, BOT) => false
	    | (_, BOT) => true
	    | (TUPLE vs1, TUPLE vs2) => let
		fun changed ([], []) = false
		  | changed (x::xs, y::ys) = changedValue(x, y) orelse changed(xs, ys)
		  | changed _ = raise Fail "tuple mis-match"
		in
		  changed (vs1, vs2)
		end
	    | (LAMBDAS s1, LAMBDAS s2) => if VSet.isSubset (s2, s1)
		then (VSet.numItems s2 < VSet.numItems s1)
		else raise Fail "non-monotonic change"
	    | _ => raise Fail "type error"
	  (* end case *))

  (* this global reference is used to mark when a value changes during an anlysis pass;
   * it is global (ugh!) because I wanted to lift the escapingValue code out of the
   * main function.
   *)
    val changed = ref false

  (* depth limit on approximate values *)
    val maxDepth = 3

  (* update the approximate value of a variable by some delta and record if
   * it changed.
   *)
    fun addInfo (x, BOT) = ()
      | addInfo (x, v) = (case peekVar x
	   of NONE => (
		changed := true;
		setVar(x, v))
	    | SOME oldV => let
		val newV = joinValues(oldV, v)
		in
		  if changedValue(newV, oldV)
		    then (changed := true; setVar(x, newV))
		    else ()
		end
	  (* end case *))

  (* if a value escapes (e.g., is passed to an escaping function), we need to mark any
   * labels that it contains as escaping too.
  *)
    and escapingValue (LAMBDAS ls) = let
	(* for each escaping function, we set its call site to Unknown and
	 * set its parameters to TOP.
	 *)
	  fun doVar f = if not(isEscaping f)
		then (case CPS.Var.kindOf f
		   of CPS.VK_Fun (CPS.FB {params, rets, ...}) => (
			setSites (f, Unknown);
			List.app (fn x => addInfo(x, TOP)) params;
                        List.app (fn x => addInfo(x, TOP)) rets)
		    | CPS.VK_Cont (CPS.FB {params, rets, ...}) => (
			setSites (f, Unknown);
			List.app (fn x => addInfo(x, TOP)) params;
                        List.app (fn x => addInfo(x, TOP)) rets)
                    | vk => raise Fail(concat[
                           "type error: escapingValues.doVar(", CPS.Var.toString f,
                           "); Var.kindOf(", CPS.Var.toString f, ") = ", CPS.varKindToString vk
                         ])
		  (* end case *))
		else ()
	  in
	    VSet.app doVar ls
	  end
      | escapingValue (TUPLE vs) = List.app escapingValue vs
      | escapingValue _ = ()

    and joinValues (v1, v2) = let
	  fun kJoin (0, v1, v2) = (
	      (* since the value are going to top, we can't track them so they may be escaping *)
		escapingValue v1; escapingValue v2; TOP)
	    | kJoin (_, TOP, v) = (escapingValue v; TOP)
	    | kJoin (_, v, TOP) = (escapingValue v; TOP)
	    | kJoin (_, BOT, v) = v
	    | kJoin (_, v, BOT) = v
	    | kJoin (k, TUPLE vs1, TUPLE vs2) = let
		fun join ([], []) = []
		  | join (x::xs, y::ys) = kJoin(k-1, x, y) :: join(xs, ys)
                  | join _ = raise Fail "tuple mis-match"
		in
		  TUPLE(join(vs1, vs2))
		end
	    | kJoin (_, LAMBDAS fs1, LAMBDAS fs2) = LAMBDAS(VSet.union(fs1, fs2))
	    | kJoin _ = (
	      (* since the value are going to top, we can't track them so they may be escaping *)
		escapingValue v1; escapingValue v2; TOP)
	  in
	    kJoin (maxDepth, v1, v2)
	  end

  (* select the i'th component of a tuple. *)
    fun select (i, y) = (case valueOf y
	   of TUPLE vs => let
		fun sel (0, v::_) = v
		  | sel (i, v::r) = sel(i-1, r)
		  | sel (i, []) = raise Fail "tuple mis-match"
		in
		  sel (i, vs)
		end
	    | BOT => BOT
	    | TOP => TOP
	    | v => raise Fail(concat[
		  "type error: select(", Int.toString i, ", ", CPS.Var.toString y,
		  "); valueOf(", CPS.Var.toString y, ") = ", valueToString v
		])
	  (* end case *))
  (* update the i'th component of a tuple. *)
    fun update (i, y, z) = (case valueOf y
	   of TUPLE vs => let
		fun upd (0, v::r, ac) = TUPLE ((rev ac) @ ((valueOf z)::r))
		  | upd (i, v::r, ac) = upd(i-1, r, v::ac)
		  | upd (i, [], _) = raise Fail "tuple mis-match"
		in
		  upd (i, vs, [])
		end
	    | BOT => BOT
	    | TOP => (escapingValue (valueOf z); TOP)
	    | v => raise Fail(concat[
		  "type error: update(", Int.toString i, ", ", CPS.Var.toString y, 
                  ", ", CPS.Var.toString z,
		  "); valueOf(", CPS.Var.toString y, ") = ", valueToString v
		])
	  (* end case *))

(* +DEBUG *)
    fun printResults body = let
          fun printExp e = let
                fun doExp e = printExp e
                fun doLambda fb = printLambda fb
		in
		  case e
		   of CPS.Let (_, _, e) => doExp e
                    | CPS.Fun (fbs, e) => (List.app doLambda fbs; doExp e)
                    | CPS.Cont (fb, e) => (doLambda fb; doExp e)
                    | CPS.If (_, e1, e2) => (doExp e1; doExp e2)
                    | CPS.Switch (_, cases, dflt) =>
                        (List.app (doExp o #2) cases;
                         Option.app doExp dflt)
                    | CPS.Apply (f, _, _) => 
                        print(concat["valueOf(", CPS.Var.toString f, ") = ",
                                     valueToString (valueOf f), "\n"])
                    | CPS.Throw (f, _) => 
                        print(concat["valueOf(", CPS.Var.toString f, ") = ",
                                     valueToString (valueOf f), "\n"])
		  (* end case *)
		end
          and printLambda (CPS.FB {f, body, ...}) = (
                print(concat["callSitesOf(", CPS.Var.toString f, ") = ", 
                             callSitesToString (getSites f), "\n"]);
                printExp body)
	  in
	    printLambda body
	  end
(* -DEBUG *)

  (* compute the call-sites of variables.  We visit every function and add its variable
   * to the call sites of any known targets.  Note that this function is called
   * after the main analysis and that the call site of any escaping function
   * should have been set to Unknown.
   *)
    fun computeCallSites body = let
          fun computeExp (e, srcVar) = let
                fun add dstVar = (case getSites dstVar
                       of Unknown => ()
                        | Known s => setSites(dstVar, Known(VSet.add(s, srcVar)))
                      (* end case *))
                fun addSet f = (case valueOf f
                       of LAMBDAS fs => VSet.app add fs
                        | _ => ())
                fun doExp e = computeExp (e, srcVar)
                fun doLambda fb = computeLambda fb
		in
		  case e
		   of CPS.Let (_, _, e) => doExp e
                    | CPS.Fun (fbs, e) => (List.app doLambda fbs; doExp e)
                    | CPS.Cont (fb, e) => (doLambda fb; doExp e)
                    | CPS.If (_, e1, e2) => (doExp e1; doExp e2)
                    | CPS.Switch (_, cases, dflt) =>
                        (List.app (doExp o #2) cases;
                         Option.app doExp dflt)
                    | CPS.Apply (f, _, _) => addSet f
                    | CPS.Throw (f, _) => addSet f
		  (* end case *)
		end
          and computeLambda (CPS.FB {f, body, ...}) = computeExp (body, f)
	  in
	    computeLambda body
	  end

    fun analyze (CPS.MODULE{body, ...}) = let
	  fun onePass () = let
		val addInfo = if !debugFlg
		      then (fn (x, v) => let
			val prevV = valueOf x
			in
			  addInfo (x, v);
			  if changedValue(valueOf x, prevV) 
			    then print(concat[
				"addInfo(", CPS.Var.toString x,  ", ", valueToString v, 
				"): ", valueToString prevV, " ==> ", valueToString(valueOf x),
				"\n"
			      ])
			    else ()
			end)
		      else addInfo
                val addInfo' = fn (x, y) => addInfo (x, valueOf y)
	      (* record that a given variable escapes *)
		fun escape x = escapingValue (valueOf x)
                fun doLambda (CPS.FB {f, body, ...}) = (
                      addInfo(f, LAMBDAS(VSet.singleton f));
                      doExp body)
		and doExp (CPS.Let (xs, rhs, e)) = (doRhs (xs, rhs); doExp e)
                  | doExp (CPS.Fun (fbs, e)) = (List.app doLambda fbs; doExp e)
                  | doExp (CPS.Cont (fb, e)) = (doLambda fb; doExp e)
                  | doExp (CPS.If (_, e1, e2)) = (doExp e1; doExp e2)
                  | doExp (CPS.Switch (_, cases, dflt)) = 
                      (List.app (doExp o #2) cases; Option.app doExp dflt)
                  | doExp (CPS.Apply (f, args, conts)) = doApply (f, args, conts)
                  | doExp (CPS.Throw (f, args)) = doThrow (f, args)
		and doRhs (xs, CPS.Var ys) =
		      ListPair.appEq addInfo' (xs, ys)
                  | doRhs ([x], CPS.Cast (_, y)) = addInfo(x, valueOf y)
                  | doRhs ([x], CPS.Const _) = ()
                  | doRhs ([x], CPS.Select (i, y)) = addInfo(x, select(i, y))
		  | doRhs ([], CPS.Update(i, y, z)) = 
                      addInfo(y, update(i, y, z))
		  | doRhs ([x], CPS.AddrOf(i, y)) = addInfo(x, TOP)
		  | doRhs ([x], CPS.Alloc xs) = addInfo(x, TUPLE(List.map valueOf xs))
		  | doRhs ([x], CPS.GAlloc xs) = addInfo(x, TUPLE(List.map valueOf xs))
		  | doRhs ([x], CPS.Promote y) = addInfo(x, valueOf y)
		  | doRhs ([x], CPS.Prim prim) = 
                      (if PrimUtil.isPure prim
                          then ()
                          else List.app escape (PrimUtil.varsOf prim);
                       addInfo(x, TOP))
		  | doRhs (xs, CPS.CCall (_, args)) =
                      (List.app escape args; List.app (fn x => addInfo(x, TOP)) xs)
		  | doRhs ([x], CPS.HostVProc) = addInfo(x, TOP)
		  | doRhs ([x], CPS.VPLoad _) = addInfo(x, TOP)
		  | doRhs ([], CPS.VPStore (_, y, z)) = escape z
                  | doRhs (xs, rhs) = raise Fail(concat[
                         "type error: doRhs([", 
                         String.concatWith "," (List.map CPS.Var.toString xs), 
                         "], ", CPSUtil.rhsToString rhs, ")"
                      ])
                and doApply (f, args, conts) = (case valueOf f
                       of LAMBDAS fs => VSet.app (fn f => doApplyAux (f, args, conts)) fs
                        | BOT => ()
                        | TOP => (List.app escape args; List.app escape conts)
                        | _ => raise Fail "type error")
                and doApplyAux (f, args, conts) = (case CPS.Var.kindOf f 
                       of CPS.VK_Fun (fb as CPS.FB {f, params, rets, body}) => (
                            ListPair.appEq addInfo' (params, args);
                            ListPair.appEq addInfo' (rets, conts))
                        | _ => raise Fail "type error"
                      (* end case *))
                and doThrow (f, args) = (case valueOf f
                       of LAMBDAS fs => VSet.app (fn f => doThrowAux (f, args)) fs
                        | BOT => ()
                        | TOP => List.app escape args
                        | _ => raise Fail "type error")
                and doThrowAux (f, args) = (case CPS.Var.kindOf f 
                       of CPS.VK_Cont (fb as CPS.FB {f, params, rets, body}) => (
                            ListPair.appEq addInfo' (params, args);
                            ListPair.appEq addInfo' (rets, []))
                        | _ => raise Fail "type error"
                      (* end case *))
		in
		  changed := false;
                  doLambda body;
		  !changed
		end
	  fun iterate () = if onePass() then iterate() else ()
	  in
	  (* initialize the arguments to the module entry to top *)
	    case body
	     of CPS.FB{f, params, rets, ...} => (
                  setSites (f, Unknown);
                  List.app (fn x => setVar (x, TOP)) params;
                  List.app (fn x => setVar (x, TOP)) rets)
	    (* end case *);
	  (* iterate to a fixed point *)
	    iterate ();
	  (* compute call-site information for variables *)
	    computeCallSites body;
          (* print results of cfa *)
            if !resultsFlg then printResults body else ()
	  end
  end
