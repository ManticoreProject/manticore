(* cfa-bom.sml
 *
 * Based off of cfa-cps.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 *)

structure CFABOM : sig

    val analyze : BOM.module -> unit
    val clearInfo : BOM.module -> unit

  (* the callers of a function or continuation is the set of functions and
   * and continuations that call it.
   *)
    datatype callers
      = Unknown                         (* possible unknown callers *)
      | Known of BOM.Var.Set.set        (* only called from known locations; the variables *)
                                        (* are the binding labels of the lambdas that call *)
                                        (* the target *)

    val callersToString : callers -> string

    val callersOf : BOM.var -> callers

  (* For a known function f, the equivalent functions are ones that share calling
   * sites (c.f., Soranno's T property).  If f is an escaping function, then
   * equivalentFuns f = [].
   *)
    val equivalentFuns : BOM.var -> BOM.var list

    datatype hlopc = H of (ProgPt.ppt * HLOp.hlop)

  (* abstract values *)
    datatype value
      = TOP
      | TUPLE of value list
      | HLOPC of hlopc list
      | LAMBDAS of BOM.Var.Set.set
      | BOT

    val valueToString : value -> string

    val valueOf : BOM.var -> value

  (* returs true if the given variable is only a proxy *)
    val isProxy : BOM.var -> bool

  (* return true if the given lambda variable escapes *)
    val isEscaping : BOM.var -> bool

  (* flags to control debugging *)
    val debugFlg : bool ref
    val resultsFlg : bool ref

  end = struct

    val debugFlg = ref false
    val resultsFlg = ref false

    structure BV = BOM.Var
    structure VSet = BV.Set
    structure ST = Stats

    datatype callers
      = Unknown                 (* possible unknown call callers *)
      | Known of VSet.set       (* only called from known locations; the labels are the *)
                                (* entry labels of the functions that call the target *)

  (***** Statistics *****)
    val cntPasses		= ST.newCounter "cps-bom:num-passes"
    val cntPPTsVisited		= ST.newCounter "cps-bom:ppts-visited"

    fun setToString vs = let
	  fun f [] = ["}"]
	    | f [x] = [BV.toString x, "}"]
	    | f (x::r) = BV.toString x :: "," :: f r
	  in
	    concat ("{" :: f (VSet.listItems vs))
	  end

    fun callersToString Unknown = "?"
      | callersToString (Known s) = setToString s

    datatype hlopc = H of (ProgPt.ppt * HLOp.hlop)

    datatype value
      = TOP
      | TUPLE of value list
      | HLOPC of hlopc list
      | LAMBDAS of VSet.set
      | BOT

    fun valueToString v = let
          fun v2s (TOP, l) = "T" :: l
            | v2s (HLOPC(hs), l) = List.foldr (fn (H(ppt,oper),l) => "[" :: ProgPt.toString ppt :: " " :: HLOp.toString oper :: "]" :: l) l hs
            | v2s (TUPLE[], l) = "()" :: l
            | v2s (TUPLE[v], l) = "(" :: v2s (v, ")" :: l)
            | v2s (TUPLE(v::r), l) =
                "(" :: v2s (v, List.foldr (fn (v, l) => "," :: v2s(v, l)) (")" :: l) r)
            | v2s (LAMBDAS s, l) = let
                fun f [] = "}" :: l
                  | f [x] = BV.toString x :: "}" :: l
                  | f (x::r) = BV.toString x :: "," :: f r
                in
                  "{" :: f (VSet.listItems s)
                end
            | v2s (BOT, l) = "#" :: l
          in
            concat (v2s(v, []))
          end

  (* property to track proxy variables *)
    val {getFn=getIsProxy, setFn=setIsProxy, ...} = BV.newProp (fn _ => false)
    val isProxy = getIsProxy


  (* create an approximate value for a function type. *)
    fun valueFromFunType (paramTys, retTys, retValTys) = let
          val ty = BOMTy.T_Fun(paramTys, retTys, retValTys)
          val params = map (fn ty => BV.new("cfaProxyParam", ty)) paramTys
          val () = app (fn x => setIsProxy(x, true)) params
          val exh = map (fn ty => BV.new("cfaProxyRet", ty)) retTys
          val () = app (fn x => setIsProxy(x, true)) exh
          val f = BV.new("cfaProxyF", ty)
          val () = setIsProxy(f, true)
          val z = BV.new("cfaProxyZ", BOMTy.T_Fun([],[],[]))
          val () = setIsProxy(z, true)
          val lambda = BOM.FB {
		  f = f,
		  params = params,
		  exh = exh,
		  body = BOM.mkThrow(z, [])
	        }
           val () = app (fn x => BV.setKind(x, BOM.VK_Param)) params
           val () = app (fn x => BV.setKind(x, BOM.VK_Param)) exh
           val () = if null retTys 
		 then BV.setKind(f, BOM.VK_Cont lambda)
		 else BV.setKind(f, BOM.VK_Fun lambda)
           in 
             LAMBDAS(VSet.singleton f)
           end 

  (* create an approximate value from a type.  These values are used 
   * to initialize the abstract value property for variables.  
   *)
    fun valueFromType ty = (case ty
           of BOMTy.T_Any => BOT (* or should this be TOP? *)
            | BOMTy.T_Enum _ => TOP
            | BOMTy.T_Raw _ => TOP
            | BOMTy.T_Tuple(true, tys) => TUPLE(List.map (fn _ => TOP) tys)
            | BOMTy.T_Tuple(false, tys) => TUPLE(List.map valueFromType tys)
            | BOMTy.T_Addr _ => TOP
            | BOMTy.T_Fun _ => LAMBDAS(VSet.empty)
            | BOMTy.T_Cont _ => LAMBDAS(VSet.empty)
            | BOMTy.T_CFun _ => TOP
            | BOMTy.T_VProc => TOP
            | BOMTy.T_TyCon _ => TOP
          (* end case *))

  (* property to track callers *)
    val {getFn=callersOf, clrFn=clrCallers, setFn=setCallers, ...} =
          BV.newProp (fn _ => Known(VSet.empty))

  (* property to track the estimated value of variables *)
    val {getFn=getValue, clrFn=clrValue, peekFn=peekValue, setFn=setValue} =
          BV.newProp (fn x => valueFromType (BV.typeOf x))
    val valueOf = getValue

  (* property to track the estimated results of functions *)
    val {getFn=getResult, clrFn=clrResult, peekFn=peekResult, setFn=setResult} =
          BV.newProp (fn x => (
                         case BV.typeOf x
                          of BOMTy.T_Fun(_, _, retval) =>(
                             if length retval = 0
                             then BOT (* unit return *)
                             else valueFromType (hd retval))
                           | BOMTy.T_Cont _ => BOT
                           | _ => raise Fail (concat["Invalid variable: ", BV.toString x, " typed: ", BOM.varKindToString (BV.kindOf x), "\n"])
                         ))

  (* return true if the given lambda variable escapes *)
    fun isEscaping f = (case callersOf f of Unknown => true | _ => false)

  (* test if a new approximate value is different from an old value; this
   * code assumes that values change according to the lattice order.
   *)
    fun changedValue (new, old) = (case (new, old)
           of (TOP, TOP) => false
            | (TOP, _) => true
            | (BOT, BOT) => false
            | (_, BOT) => true
            | (HLOPC(h1), HLOPC(h2)) => not(length h1 = length h2)
            | (_, HLOPC _) => true
            | (HLOPC _, _) => true
            | (TUPLE vs1, TUPLE vs2) => let
                fun changed ([], []) = false
                  | changed (x::xs, y::ys) = changedValue(x, y) orelse changed(xs, ys)
                  | changed (l, []) = true
                  | changed ([], l) = raise Fail "non-monotonic change"
                in
                  changed (vs1, vs2)
                end
            | (LAMBDAS s1, LAMBDAS s2) => if VSet.isSubset (s2, s1)
                then (VSet.numItems s2 < VSet.numItems s1)
                else raise Fail "non-monotonic change"
            | _ => raise Fail "non-monotonic change"
          (* end case *))

  (* this global reference is used to mark when a value changes during an anlysis pass;
   * it is global (ugh!) because I wanted to lift the escapingValue code out of the
   * main function.
   *)
    val changed = ref false

  (* depth limit on approximate values *)
    val maxDepth = 5

  (* update the approximate value of a variable by some delta and record if
   * it changed.
   *)
    fun addInfo (x, BOT) = ()
      | addInfo (x, v) = (
        case peekValue x
         of SOME oldV => (let
          val oldV = getValue x
          val newV = joinValues(oldV, v)
          in
            if changedValue(newV, oldV)
              then (changed := true; setValue(x, newV))
              else ()
          end)
          | NONE => setValue (x, v))

  (* update the approximate result associated with a variable by some delta and
   * record if it changed.
   *)
    and addResult (x, BOT) = ()
      | addResult (x, v) = let
          val oldV = getResult x
          val newV = joinValues(oldV, v)
          in
            if changedValue(newV, oldV)
              then (changed := true; setResult(x, newV))
              else ()
          end

  (* if a value escapes (e.g., is passed to an escaping function), we need to mark any
   * labels that it contains as escaping too.
  *)
    and escapingValue (LAMBDAS fs) = let
        (* for each escaping function, we set its callers to Unknown and
         * set its parameters to TOP.
         *)
          fun doVar f = if not(isEscaping f)
                then (case BV.kindOf f
                   of BOM.VK_Fun (BOM.FB {params, exh, ...}) => (
                        setCallers (f, Unknown);
                        addResult (f, TOP);
                        List.app (fn x => addInfo(x, TOP)) params;
                        List.app (fn x => addInfo(x, TOP)) exh)
                    | BOM.VK_Cont (BOM.FB {params, exh, ...}) => (
                        setCallers (f, Unknown);
                        addResult (f, TOP);
                        List.app (fn x => addInfo(x, TOP)) params;
                        List.app (fn x => addInfo(x, TOP)) exh)
                    | vk => raise Fail(concat[
                           "type error: escapingValues.doVar(", BV.toString f,
                           "); Var.kindOf(", BV.toString f, ") = ", BOM.varKindToString vk
                         ])
                  (* end case *))
                else ()
          in
            VSet.app doVar fs
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
            | kJoin (_, HLOPC h1, HLOPC h2) = let
                  val allops = h1 @ h2
                  fun equalOps (H(ppt1,HLOp.HLOp{name=name1,...}), H(ppt2,HLOp.HLOp{name=name2,...})) =
                      (ProgPt.same(ppt1,ppt2) andalso Atom.same(name1,name2))
                  val finalOps = List.foldr (fn (oper,l) => if not (List.exists (fn (x)=>equalOps(x,oper)) l)
                                                            then oper::l
                                                            else l)
                  [] allops 
              in
                  HLOPC(finalOps)
              end
            | kJoin (k, TUPLE vs1, TUPLE vs2) = let
                fun join ([], []) = []
                  | join (x::xs, y::ys) = kJoin(k-1, x, y) :: join(xs, ys)
                  | join ([], l) = l
                  | join (l, []) = l
                in
                  TUPLE(join(vs1, vs2))
                end
            | kJoin (k, v1 as LAMBDAS fs1, v2 as LAMBDAS fs2) = 
                if VSet.isEmpty fs1 then v2
                else if VSet.isEmpty fs2 then v1
                else let
              (* join params and rets of joined lambdas *)
                fun getParamsExh f = (case BV.kindOf f
                       of BOM.VK_Fun (BOM.FB {params, exh, ...}) => (params, exh)
                        | BOM.VK_Cont (BOM.FB {params, exh, ...}) => (params, exh)
                        | vk => raise Fail(concat[
                              "type error: kJoin.getParamsExh(", BV.toString f,
                              "); Var.kindOf(", BV.toString f, ") = ", BOM.varKindToString vk
                            ])
                     (* end case *))
                val SOME f1 = VSet.find (fn _ => true) fs1
                val (params1, exh1) = getParamsExh f1
                val SOME f2 = VSet.find (fn _ => true) fs2
                val (params2, exh2) = getParamsExh f2
                val params' = ListPair.mapEq (fn (x1,x2) => kJoin(k-1, getValue x1, getValue x2)) 
                                             (params1, params2)
                val exh' = ListPair.mapEq (fn (x1,x2) => kJoin(k-1, getValue x1, getValue x2)) 
                                           (exh1, exh2)
              (* join isEscaping of joined lambdas *)
                val isEscaping = isEscaping f1 orelse isEscaping f2
                val fs = VSet.union (fs1, fs2)
                val () = VSet.app (fn f => let
                                   val (params, exh) = getParamsExh f
                                   in
                                     if isEscaping then setCallers(f, Unknown) else ();
                                     ListPair.app addInfo (params, params');
                                     ListPair.app addInfo (exh, exh')
                                   end)
                                  fs
                in
                  LAMBDAS fs
                end
            | kJoin _ = (
              (* since the value are going to top, we can't track them so they may be escaping *)
                escapingValue v1; escapingValue v2; TOP)
          in
            kJoin (maxDepth, v1, v2)
          end

  (* select the i'th component of a tuple. *)
    fun select (i, y) = (case getValue y
           of TUPLE vs => let
                fun sel (0, v::_) = v
                  | sel (j, v::r) = sel(j-1, r)
                  | sel (_, []) = raise Fail(concat[
                        "type error: select(", Int.toString i, ", ", BV.toString y,
                        "); getValue(", BV.toString y, ") = ", valueToString (TUPLE vs)
                      ])
                in
                  sel (i, vs)
                end
            | BOT => BOT
            | TOP => TOP
            | HLOPC _ => TOP
            | v => raise Fail(concat[
                  "type error: select(", Int.toString i, ", ", BV.toString y,
                  "); getValue(", BV.toString y, ") = ", valueToString v
                ])
          (* end case *))
  (* update the i'th component of a tuple. *)
    fun update (i, y, z) = (case getValue y
           of TUPLE vs => let
                fun upd (0, v::r, ac) = TUPLE ((rev ac) @ (z::r))
                  | upd (i, v::r, ac) = upd(i-1, r, v::ac)
                  | upd (_, [], ac) = raise Fail(concat[
                        "type error: update(", Int.toString i, ", ", BV.toString y,
                        ", ", valueToString z,
                        "); getValue(", BV.toString y, ") = ", valueToString (TUPLE vs)
                      ])
                in
                  upd (i, vs, [])
                end
            | BOT => BOT
            | TOP => (escapingValue z; TOP)
            | HLOPC _ => TOP
            | v => raise Fail(concat[
                  "type error: update(", Int.toString i, ", ", BV.toString y, 
                  ", ", valueToString z,
                  "); getValue(", BV.toString y, ") = ", valueToString v
                ])
          (* end case *))

  (* property to track equivalent functions; we use the empty set to mark
   * escaping functions or functions that share call sites with escaping
   * functions.
   *)
    val {getFn=getEquivFns, setFn=setEquivFns, clrFn=clrEquivFns, ...} = let
	  fun init f = if isEscaping f
		then URef.uRef VSet.empty
		else URef.uRef(VSet.singleton f)
	  in
	    BV.newProp init
	  end

    fun equivalentFuns f = VSet.listItems(URef.!!(getEquivFns f))

(* +DEBUG *)
    fun printResults body = let
          fun printCallersOf f = print(concat[
		  "callersOf(", BV.toString f, ") = ",
		  callersToString (callersOf f), "\n"
		])
	  fun printEquivFns f = print(concat[
		  "equivalentFuns(", BV.toString f, ") = ",
		  setToString (URef.!!(getEquivFns f)), "\n"
		])
          fun printValueOf x = (case valueOf x
		 of TOP => ()
		  | v => print(concat[
			"getValue(", BV.toString x, ") = ", valueToString v, "\n"
		      ])
		(* end case *))
          fun printResultOf x = (case peekResult x
                 of SOME r => print (concat["getResult(", BV.toString x, ") = ", valueToString r, "\n"])
                  | _ => ())
          fun printExp (BOM.E_Pt(_, e)) = let
                fun doExp e = printExp e
                fun doLambda fb = printLambda fb
                in
                  case e
                   of BOM.E_Let (xs, e1, e2) => 
                        (List.app printValueOf xs; doExp e1; doExp e2)
                    | BOM.E_Stmt (xs, _, e) => 
                        (List.app printValueOf xs; doExp e)
                    | BOM.E_Fun (fbs, e) => (List.app doLambda fbs; doExp e)
                    | BOM.E_Cont (fb, e) => (doLambda fb; doExp e)
                    | BOM.E_If (_, e1, e2) => (doExp e1; doExp e2)
                    | BOM.E_Case (_, cases, dflt) =>
                        (List.app (doExp o #2) cases;
                         Option.app doExp dflt)
                    | BOM.E_Apply (f, _, _) => (print "Apply:: "; printValueOf f)
                    | BOM.E_Throw (f, _) => (print "Throw:: "; printValueOf f)
                    | BOM.E_Ret xs => (List.app printValueOf xs)
                    | BOM.E_HLOp _ => ()
                  (* end case *)
                end
          and printLambda (BOM.FB {f, params, exh, body, ...}) = (
                printValueOf f;
                printCallersOf f;
                printResultOf f;
                printEquivFns f;
                List.app printValueOf params;
                List.app printValueOf exh;
                printExp body)
          in
            printLambda body
          end
(* -DEBUG *)

  (* compute additional information about functions.  This includes the callers of
   * functions and the set of functions that are equivalent to a function.
   * Note that this function is called after the main analysis so that the callers
   * of any escaping function should have been set to Unknown.
   *
   * If the VSet's length is zero, that means we're processing either unreachable or
   * yet-unreached code. Don't do anything with those values.
   *)
    fun computeFunInfo body = let
          fun computeExp (e, srcVar) = let
		fun doCall f = (case getValue f
		       of LAMBDAS gs => (
                          if VSet.isEmpty gs
                          then ()
                          else let
			    val (gs as g::r) = VSet.listItems gs
			  (* add srcVar to the callers of dstVar *)
			    fun add dstVar = (case callersOf dstVar
				   of Unknown => ()
				    | Known s => setCallers(dstVar, Known(VSet.add(s, srcVar)))
				  (* end case *))
			    val gEq = getEquivFns g
			  (* merge equivalance sets *)
			    fun merge h = let
				  fun unify (vs1, vs2) =
					if VSet.isEmpty vs1 orelse VSet.isEmpty vs2
					  then VSet.empty
					  else VSet.union(vs1, vs2)
				  in
				    ignore (URef.unify unify (gEq, getEquivFns h))
				  end
			    in
			      List.app add gs;
			      List.app merge r
			    end)
			| _ => ()
		      (* end case *))
                fun doExp (BOM.E_Pt(_, t)) = (case t
		       of (BOM.E_Let(_, e1, e2)) => (doExp e1; doExp e2)
                        | BOM.E_Stmt (_, _, e) => doExp e
			| (BOM.E_Fun(fbs, e)) => (List.app computeLambda fbs; doExp e)
			| (BOM.E_Cont(fb, e)) => (computeLambda fb; doExp e)
			| (BOM.E_If(_, e1, e2)) => (doExp e1; doExp e2)
			| (BOM.E_Case(_, cases, dflt)) => (
			    List.app (doExp o #2) cases;
			    Option.app doExp dflt)
			| (BOM.E_Apply(f, _, _)) => doCall f
			| (BOM.E_Throw(f, _)) => doCall f
                        | (BOM.E_Ret vs) => ()
                        | (BOM.E_HLOp _) => ()
		      (* end case *))
                in
                  doExp e
                end
          and computeLambda (BOM.FB{f, body, ...}) = computeExp (body, f)
          in
            computeLambda body
          end

  (* In order to avoid problems when an unreachable function calls a
   * reachable function, we "bump" the abstract value of any variable
   * of function type from LAMBDAS {} to LAMBDAS {proxy}, for a proxy
   * function.  The proxy function provides a variable identifier to 
   * carry the isEscaping property of the variable.
   *
   * It is more efficient to only "bump" the abstract value of 
   * variables that would otherwise be LAMBDAS {}, rather than 
   * initialize all variables of function type to LAMBDAS {proxy},
   * which overwhelms the LAMBDAS sets with proxies (and may also
   * have termination problems due to creating an infinite regress
   * of proxy functions).
   *)
    fun bumpInfo body = let
          fun bumpValue x = (case (BV.typeOf x, getValue x) 
                 of (BOMTy.T_Fun (params,rets,retval), LAMBDAS fs) => (
                      if VSet.isEmpty fs 
                         then addInfo (x, valueFromFunType (params, rets, retval))
                         else ())
                  | _ => ()
                (* end case *))
          fun doLambda (BOM.FB {f, params, exh, body}) = (
                List.app bumpValue params;
                List.app bumpValue exh;
                doExp body)
          and doExp (BOM.E_Pt(_, e)) = (case e 
                 of BOM.E_Let (xs, e1, e2) => (List.app bumpValue xs; doExp e1; doExp e2)
                  | BOM.E_Stmt (xs, _, e) => (List.app bumpValue xs; doExp e)
                  | BOM.E_Fun (fbs, e) => (List.app doLambda fbs; doExp e)
                  | BOM.E_Cont (fb, e) => (doLambda fb; doExp e)
                  | BOM.E_If (_, e1, e2) => (doExp e1; doExp e2)
                  | BOM.E_Case (_, cases, dflt) => 
                      (List.app (doExp o #2) cases; Option.app doExp dflt)
                  | BOM.E_Apply _ => ()
                  | BOM.E_Throw _ => ()
                  | BOM.E_Ret _ => ()
                  | BOM.E_HLOp _ => ()
                (* end case *))
          in
            doLambda body
          end

    fun analyze (BOM.MODULE{body, ...}) = let
          fun onePass () = let
                val _ = ST.tick cntPasses
                val addInfo = if !debugFlg
                      then (fn (x, v) => (
                               case peekValue x
                                of SOME prevV => (
                                   let
                                       val prevV = getValue x
                                   in
                                       addInfo (x, v);
                                       if changedValue(getValue x, prevV) 
                                       then print(concat[
                                                  "addInfo(", BV.toString x,  ", ", valueToString v, 
                                                  "): ", valueToString prevV, " ==> ", valueToString(getValue x),
                                                  "\n"
                                                 ])
                                       else ()
                                   end)
                                 | NONE => (addInfo (x,v);
                                            print(concat[
                                                  "addInfo(", BV.toString x,  ", ", valueToString v, 
                                                  "): assigned new value.\n"]))))

                      else addInfo
                val addResult = if !debugFlg
                      then (fn (x, v) => let
                        val prevV = getResult x
                        in
                          addResult (x, v);
                          if changedValue(getResult x, prevV) 
                            then print(concat[
                                "addResult(", BV.toString x,  ", ", valueToString v, 
                                "): ", valueToString prevV, " ==> ", valueToString(getResult x),
                                "\n"
                              ])
                            else ()
                        end)
                      else addResult
                val addInfo' = fn (x, y) => addInfo (x, getValue y)
                val eqInfo' = fn (x, y) => (addInfo' (x, y))
              (* record that a given variable escapes *)
                fun escape x = escapingValue (getValue x)
                fun doLambda (BOM.FB {f, body, ...}) = (
                      addInfo(f, LAMBDAS(VSet.singleton f));
                      doExp (f, body);
                      ())
                and doExp (f, BOM.E_Pt(ppt, t)) = (
		      ST.tick cntPPTsVisited;
                      case t
		       of (BOM.E_Let (xs, e1 as BOM.E_Pt(_,t2), e2)) => (
                          let
                              val rhs = doExp (f, e1)
                              val _ = if List.length xs = List.length rhs
                                      then ListPair.appEq (fn (x,y) => addInfo (x, y)) (xs, rhs)
                                      else List.app (fn (x) => addInfo (x, TOP)) xs
                          in
                              doExp (f, e2)
                          end)
                        | (BOM.E_Stmt (xs, rhs, e)) => (doRhs (xs, rhs); doExp (f, e))
			| (BOM.E_Fun (fbs, e)) => (List.app doLambda fbs; doExp (f, e))
			| (BOM.E_Cont (fb, e)) => (doLambda fb; doExp (f, e))
			| (BOM.E_If (_, e1, e2)) => (doExp (f, e1); doExp (f, e2))
			| (BOM.E_Case (_, cases, dflt)) => (* TODO: merge doExp results *)
			    (List.app ((fn x=>ignore(doExp(f,x))) o #2) cases; Option.app (fn x=>ignore(doExp(f,x))) dflt; [TOP])
			| (BOM.E_Apply (f, args, conts)) => (doApply (f, args, conts); [getResult f])
			| (BOM.E_Throw (f, args)) => (doThrow (f, args); [BOT])
                        | BOM.E_Ret xs => (
                          let
                              val rets = List.map getValue xs
                          in
                              List.app (fn x => addResult (f, x)) rets;
                              rets
                          end)
                        | (BOM.E_HLOp (h as HLOp.HLOp{constr,...}, _, _)) => (
                          if constr
                          then ([HLOPC([H(ppt, h)])])
                          else [TOP])
		      (* end case *))
                and doRhs ([x], BOM.E_Cast (ty, y)) = eqInfo' (x, y)
                  | doRhs ([x], BOM.E_Const _) = addInfo (x, TOP)
                  | doRhs ([x], BOM.E_Select (i, y)) = (addInfo (x, select (i, y)); addInfo (y, update(i, y, getValue x)))
                  | doRhs ([], BOM.E_Update(i, y, z)) = (addInfo (z, select (i, y)); addInfo (y, update (i, y, getValue z)))
                  | doRhs ([x], BOM.E_AddrOf(i, y)) = (addInfo (y, update (i, y, TOP)); addInfo (x, TOP))
                  | doRhs ([x], BOM.E_Alloc(ty, xs)) = addInfo (x, TUPLE(List.map getValue xs))
                  | doRhs ([x], BOM.E_Promote y) = eqInfo' (x, y)
                  | doRhs ([], BOM.E_Prim prim) = if PrimUtil.isPure prim
                      then ()
                      else List.app escape (PrimUtil.varsOf prim)
                  | doRhs ([x], BOM.E_Prim prim) = (
                      if PrimUtil.isPure prim
                        then ()
                        else List.app escape (PrimUtil.varsOf prim);
                      addInfo (x, TOP))
                  | doRhs ([x], BOM.E_DCon(_, xs)) = addInfo (x, TUPLE(List.map getValue xs))
                  | doRhs (xs, BOM.E_CCall (_, args)) = (List.app escape args; List.app (fn x => addInfo (x, TOP)) xs)
                  | doRhs ([x], BOM.E_HostVProc) = addInfo (x, TOP)
                  | doRhs ([x], BOM.E_VPLoad _) = addInfo (x, TOP)
                  | doRhs ([], BOM.E_VPStore (_, y, z)) = escape z
                  | doRhs ([x], BOM.E_VPAddr _) = addInfo (x, TOP)
                  | doRhs (xs, rhs) = raise Fail(concat[
                       "type error: doRhs([", String.concatWith "," (List.map BV.toString xs), 
                       "], ", BOMUtil.rhsToString rhs, ")"
                     ])
                and doApply (f, args, conts) = (case getValue f
                       of LAMBDAS fs => VSet.app (fn f => doApplyAux (f, args, conts)) fs
                        | BOT => ()
                        | TOP => (List.app escape args; List.app escape conts)
                        | _ => raise Fail (concat["type error: doApply: ", BV.toString f, " val: ", valueToString (getValue f)])
		      (* end case *))
                and doApplyAux (f, args, conts) = (case BV.kindOf f 
                       of BOM.VK_Fun (fb as BOM.FB {f, params, exh, body}) => (
                            ListPair.appEq eqInfo' (params, args);
                            ListPair.appEq eqInfo' (exh, conts))
                        | BOM.VK_Cont (fb as BOM.FB {f, params, exh, body}) => (
                            ListPair.appEq eqInfo' (params, args);
                            ListPair.appEq eqInfo' (exh, conts))
                        | _ => raise Fail (concat["type error: doApplyAux: ", BOM.varKindToString (BV.kindOf f)])
                      (* end case *))
                and doThrow (f, args) = (case getValue f
                       of LAMBDAS fs => VSet.app (fn f => doThrowAux (f, args)) fs
                        | BOT => ()
                        | TOP => List.app escape args
                        | _ => raise Fail "type error: doThrow"
		      (* end case *))
                and doThrowAux (f, args) = (case BV.kindOf f 
                       of BOM.VK_Cont (fb as BOM.FB {f, params, exh, body}) => (
                            ListPair.appEq eqInfo' (params, args);
                            ListPair.appEq eqInfo' (exh, []))
                        | _ => raise Fail "type error: doThrowAux"
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
             of BOM.FB{f, params, exh, ...} => (
                  setCallers (f, Unknown);
                  List.app (fn x => setValue (x, TOP)) params;
                  List.app (fn x => setValue (x, TOP)) exh)
            (* end case *);
          (* iterate to a fixed point *)
            iterate ();
          (* "bump" the abstract value of variables of function type
           * from LAMBDAS {} to LAMBDAS {proxy} 
           *)
            changed := false; 
            bumpInfo body; 
            if !changed then iterate () else ();
          (* compute additional information for functions *)
            computeFunInfo body;
          (* print results of cfa *)
            if !resultsFlg then printResults body else ()
          end

    val analyze = BasicControl.mkTracePassSimple {
	    passName = "cfa",
	    pass = analyze
	  }

  (* clear CFA annotations from the variables of a module.  Note that we can
   * restrict the traversal to binding instances.
   *)
    fun clearInfo (BOM.MODULE{body, ...}) = let
          fun doLambda (BOM.FB{f, params, exh, body}) = (
                clrCallers f;
                clrValue f;
		clrEquivFns f;
                List.app clrValue params;
                List.app clrValue exh;
                doExp body)
          and doExp (BOM.E_Pt(_, e)) = (case e 
                 of BOM.E_Let(xs, e1, e2) => (List.app clrValue xs; doExp e1; doExp e2)
                  | BOM.E_Stmt(xs, _, e) => (List.app clrValue xs; doExp e)
                  | BOM.E_Fun(fbs, e) => (List.app doLambda fbs; doExp e)
                  | BOM.E_Cont(fb, e) => (doLambda fb; doExp e)
                  | BOM.E_If(_, e1, e2) => (doExp e1; doExp e2)
                  | BOM.E_Case(_, cases, dflt) => (
		      List.app (doExp o #2) cases;
		      Option.app doExp dflt)
                  | BOM.E_Apply _ => ()
                  | BOM.E_Throw _ => ()
                  | BOM.E_Ret _ => ()
                  | BOM.E_HLOp _ => ()
                (* end case *))
          in
            doLambda body
          end

  end

