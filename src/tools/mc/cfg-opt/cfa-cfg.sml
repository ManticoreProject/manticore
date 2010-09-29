(* cfa-cfg.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure CFACFG : sig

    val analyze : CFG.module -> unit
    val clearInfo : CFG.module -> unit

    datatype call_sites
      = Unknown                         (* possible unknown call sites *)
      | Known of CFG.Label.Set.set      (* only called from known locations; the labels *)
                                        (* are the entry labels of the functions that call *)
                                        (* the target *)

    val callSitesToString : call_sites -> string

    val callSitesOf : CFG.label -> call_sites

    datatype value
      = TOP
      | TUPLE of value list
      | LABELS of CFG.Label.Set.set
      | BOT

    val valueToString : value -> string

    val valueOf : CFG.var -> value

  (* return true if the given label escapes *)
    val isEscaping : CFG.label -> bool

  (* return the set of labels that a control transfer targets; 
   * NONE is used to represent unknown control flow.
   *)
    val labelsOf : CFG.transfer -> CFG.Label.Set.set option

  end = struct

    val debugFlg = ref false
    val resultsFlg = ref false
    val () = List.app (fn ctl => ControlRegistry.register CFGOptControls.registry {
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

    structure LSet = CFG.Label.Set

    datatype call_sites
      = Unknown                 (* possible unknown call sites *)
      | Known of LSet.set       (* only called from known locations; the labels are the *)
                                (* entry labels of the functions that call the target *)

    fun callSitesToString v = let
          fun v2s (Unknown, l) = "?" :: l
            | v2s (Known s, l) = let
                fun f [] = "}" :: l
                  | f [x] = "$" :: CFG.Label.toString x :: "}" :: l
                  | f (x::r) = "$" :: CFG.Label.toString x :: "," :: f r
                in
                  "{" :: f (LSet.listItems s)
                end
          in
            concat (v2s(v, []))
          end

    datatype value
      = TOP
      | TUPLE of value list
      | LABELS of LSet.set
      | BOT

    fun valueToString v = let
          fun v2s (TOP, l) = "T" :: l
            | v2s (TUPLE[], l) = "()" :: l
            | v2s (TUPLE[v], l) = "(" :: v2s (v, ")" :: l)
            | v2s (TUPLE(v::r), l) =
                "(" :: v2s (v, List.foldr (fn (v, l) => "," :: v2s(v, l)) (")" :: l) r)
            | v2s (LABELS s, l) = let
                fun f [] = "}" :: l
                  | f [x] = "$" :: CFG.Label.toString x :: "}" :: l
                  | f (x::r) = "$" :: CFG.Label.toString x :: "," :: f r
                in
                  "{" :: f (LSet.listItems s)
                end
            | v2s (BOT, l) = "#" :: l
          in
            concat (v2s(v, []))
          end

    fun valueFromType ty = (case ty
           of CFGTy.T_Any => BOT (* or should this be TOP? *)
            | CFGTy.T_Enum _ => TOP
            | CFGTy.T_Raw _ => TOP
            | CFGTy.T_Tuple (true, tys) => TUPLE(List.map (fn _ => TOP) tys)
            | CFGTy.T_Tuple (false, tys) => TUPLE(List.map valueFromType tys)
            | CFGTy.T_OpenTuple tys => TUPLE(List.map valueFromType tys)
            | CFGTy.T_Addr _ => TOP
            | CFGTy.T_CFun _ => TOP
            | CFGTy.T_VProc => TOP
            | CFGTy.T_StdFun _ => LABELS(LSet.empty)
            | CFGTy.T_StdCont _ => LABELS(LSet.empty)
            | CFGTy.T_KnownFunc _ => LABELS(LSet.empty)
            | CFGTy.T_Block _ => LABELS(LSet.empty)
          (* end case *))

  (* property to track call-sites *)
    val {getFn=getSites, clrFn=clrSites, setFn=setSites, ...} =
          CFG.Label.newProp (fn _ => Known(LSet.empty))
    val callSitesOf = getSites
  (* property to track the estimated value of variables *)
    val {getFn=getValue, clrFn=clrValue, peekFn=peekValue, setFn=setValue} =
          CFG.Var.newProp (fn x => valueFromType (CFG.Var.typeOf x))
    val valueOf = getValue

  (* return true if the given label escapes *)
    fun isEscaping lab = (case callSitesOf lab of Unknown => true | _ => false)

  (* clear CFA annotations from the variables and labels of a module.  Note that we can
   * restrict the traversal to binding instances.
   *)
    fun clearInfo (CFG.MODULE{code, ...}) = let
          fun doFunct (CFG.FUNC{lab, entry, start as CFG.BLK{body=sBody,args,...}, body, ...}) = (
                clrSites lab;
                List.app clrValue (CFG.paramsOfConv (entry, args));
                List.app doExp sBody;
                List.app (fn (b as CFG.BLK{lab, body,...}) => (clrSites lab; List.app doExp body)) body)
          and doExp exp = List.app clrValue (CFG.lhsOfExp exp)
          in
            List.app doFunct code
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
                  | changed (l, []) = true
                  | changed ([], l) = raise Fail "non-monotonic change"
                in
                  changed (vs1, vs2)
                end
            | (LABELS s1, LABELS s2) => if LSet.isSubset (s2, s1)
                then (LSet.numItems s2 < LSet.numItems s1)
                else raise Fail "non-monotonic change"
            | _ => raise Fail "type error"
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
      | addInfo (x, v) = let
          val oldV = getValue x
          val newV = joinValues(oldV, v)
          in
            if changedValue(newV, oldV)
              then (changed := true; setValue(x, newV))
              else ()
          end

  (* if a value escapes (e.g., is passed to an escaping function), we need to mark any
   * labels that it contains as escaping too.
  *)
    and escapingValue (LABELS labs) = let
        (* for each escaping function, we set its call site to Unknown and
         * set its parameters to TOP.
         *)
          fun doLab lab = if not(isEscaping lab)
                then (
                  setSites (lab, Unknown);
                  case CFG.Label.kindOf lab
	           of CFG.LK_Func{func as CFG.FUNC{entry, start as CFG.BLK{args, ...}, ...}, ...} =>
                      List.app (fn x => addInfo(x, TOP)) (CFG.paramsOfConv (entry, args))
                    | CFG.LK_Block (block as CFG.BLK{args,...}) => List.app (fn x => addInfo(x, TOP)) args
	            | _ => ()
                  (* end case *))
                else ()
          in
            CFG.Label.Set.app doLab labs
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
                  | join ([], l) = l
                  | join (l, []) = l
                in
                  TUPLE(join(vs1, vs2))
                end
            | kJoin (_, LABELS ls1, LABELS ls2) = LABELS(LSet.union(ls1, ls2))
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
                  | sel (i, v::r) = sel(i-1, r)
                  | sel (_, []) = raise Fail(concat[
                        "type error: select(", Int.toString i, ", ", CFG.Var.toString y,
                        "); getValue(", CFG.Var.toString y, ") = ", valueToString (TUPLE vs)
                      ])
                in
                  sel (i, vs)
                end
            | BOT => BOT
            | TOP => TOP
            | v => raise Fail(concat[
                  "type error: select(", Int.toString i, ", ", CFG.Var.toString y,
                  "); getValue(", CFG.Var.toString y, ") = ", valueToString v
                ])
          (* end case *))
  (* update the i'th component of a tuple. *)
    fun update (i, y, z) = (case getValue y
           of TUPLE vs => let
                fun upd (0, v::r, ac) = TUPLE ((rev ac) @ (z::r))
                  | upd (i, v::r, ac) = upd(i-1, r, v::ac)
                  | upd (_, [], ac) = raise Fail(concat[
                        "type error: update(", Int.toString i, ", ", CFG.Var.toString y,
                        ", ", valueToString z,
                        "); getValue(", CFG.Var.toString y, ") = ", valueToString (TUPLE vs)
                      ])
                in
                  upd (i, vs, [])
                end
            | BOT => BOT
            | TOP => (escapingValue z; TOP)
            | v => raise Fail(concat[
                  "type error: update(", Int.toString i, ", ", CFG.Var.toString y, 
                  ", ", valueToString z,
                  "); getValue(", CFG.Var.toString y, ") = ", valueToString v
                ])
          (* end case *))

(* +DEBUG *)
    fun printResults code = let
          fun printCallSitesOf l =
                print(concat["callSitesOf(", CFG.Label.toString l, ") = ",
                             callSitesToString (callSitesOf l), "\n"])
          fun printValueOf x =
                print(concat["valueOf(", CFG.Var.toString x, ") = ",
                             valueToString (valueOf x), "\n"])
          fun printExp e = List.app printValueOf (CFG.lhsOfExp e)
          and printFunc (CFG.FUNC{lab, entry, start as CFG.BLK{body=sBody,args,...}, body, ...}) = (
                printCallSitesOf lab;
                List.app printValueOf (CFG.paramsOfConv (entry, args));
                List.app printExp sBody;
                List.app (fn (b as CFG.BLK{body,...}) => List.app printExp body) body)
          in
            List.app printFunc code
          end
(* -DEBUG *)

  (* compute the call-sites of labels.  We visit every function and add its label
   * to the call sites of any known targets.  Note that this function is called
   * after the main analysis and that the call site of any escaping function
   * should have been set to Unknown.
   *)
    fun computeCallSites code = let
          fun computeBlock (CFG.BLK{lab=srcLab, exit, ...}) = let
                fun add dstLab = (case getSites dstLab
                       of Unknown => ()
                        | Known s => setSites(dstLab, Known(LSet.add(s, srcLab)))
                      (* end case *))
                fun addSet f = (case getValue f
                       of LABELS s => LSet.app add s
                        | _ => ()
                      (* end case *))
                fun addJump (lab, _) = add lab
                in
                  case exit
                   of CFG.StdApply{f, ...} => addSet f
                    | CFG.StdThrow{k, ...} => addSet k
                    | CFG.Apply{f, ...} => addSet f
                    | CFG.Goto jmp => addJump jmp
                    | CFG.If(_, j1, j2) => (addJump j1; addJump j2)
                    | CFG.Switch(_, cases, dflt) => (
                        List.app (addJump o #2) cases;
                        Option.app addJump dflt)
                    | CFG.HeapCheck{nogc, ...} => addJump nogc
                    | CFG.AllocCCall{ret, ...} => addJump ret
                  (* end case *)
                end
          fun compute (CFG.FUNC{start, body, ...}) = (
              computeBlock start;
              List.app computeBlock body)              
          in
            List.app compute code
          end

    fun analyze (CFG.MODULE{code, ...}) = let
          fun onePass () = let
                val addInfo = if !debugFlg
                      then (fn (x, v) => let
                        val prevV = getValue x
                        in
                          addInfo (x, v);
                          if changedValue(getValue x, prevV) 
                            then print(concat[
                                "addInfo(", CFG.Var.toString x,  ", ", valueToString v, 
                                "): ", valueToString prevV, " ==> ", valueToString(getValue x),
                                "\n"
                              ])
                            else ()
                        end)
                      else addInfo
                val addInfo' = fn (x, y) => addInfo (x, getValue y)
              (* record that a given variable escapes *)
                fun escape x = escapingValue (getValue x)
                fun doExp (CFG.E_Var(xs, ys)) = ListPair.appEq addInfo' (xs, ys)
                  | doExp (CFG.E_Cast(x, _, y)) = addInfo(x, getValue y)
                  | doExp (CFG.E_Const (x, _, _)) = addInfo(x, TOP)
                  | doExp (CFG.E_Label(x, lab)) = addInfo(x, LABELS(LSet.singleton lab))
                  | doExp (CFG.E_Select(x, i, y)) = addInfo(x, select(i, y))
                  | doExp (CFG.E_Update(i, y, z)) = (escape z; addInfo(y, update(i, y, getValue z)))
                  | doExp (CFG.E_AddrOf(x, i, y)) = (addInfo(x, TOP); addInfo(y, update(i, y, TOP)))
                  | doExp (CFG.E_Alloc(x, _, xs)) = addInfo(x, TUPLE(List.map getValue xs))
                  | doExp (CFG.E_AllocSpecial(x, _, xs)) = addInfo(x, TUPLE(List.map getValue xs))
                  | doExp (CFG.E_GAlloc(x, _, xs)) = addInfo(x, TUPLE(List.map getValue xs))
                  | doExp (CFG.E_Promote(x, y)) = addInfo(x, getValue y)
                  | doExp (CFG.E_Prim0 prim) =
		      if PrimUtil.isPure prim 
                         then ()
                         else List.app escape (PrimUtil.varsOf prim)
                  | doExp (CFG.E_Prim(x, prim)) = (
		      if PrimUtil.isPure prim 
                         then ()
                         else List.app escape (PrimUtil.varsOf prim);
                      addInfo(x, TOP))
                  | doExp (CFG.E_CCall(xs, _, args)) = (List.app escape args; List.app (fn x => addInfo(x, TOP)) xs)
                  | doExp (CFG.E_HostVProc x) = addInfo(x, TOP)
                  | doExp (CFG.E_VPLoad(x, _, _)) = addInfo(x, TOP)
                  | doExp (CFG.E_VPStore(_, _, z)) = escape z
                  | doExp (CFG.E_VPAddr(x, _, _)) = addInfo(x, TOP)
                fun doXfer (CFG.StdApply{f, clos, args, ret, exh}) =
                      doApply (f, 
                               ("StdApply{f = " ^ (CFG.Var.toString f) ^ ", ...}", 
                                fn CFG.StdFunc _ => true | _ => false),
                               clos :: args @ [ret, exh])
                  | doXfer (CFG.StdThrow{k, clos, args}) = 
                      doApply (k, 
                               ("StdCont{k = " ^ (CFG.Var.toString k) ^ ", ...}", 
                                fn CFG.StdCont _ => true | _ => false),
                               clos :: args)
                  | doXfer (CFG.Apply{f, clos, args}) = 
                      doApply (f, 
                               ("Apply {f = " ^ (CFG.Var.toString f) ^ ", ...}",
                                fn CFG.KnownFunc _ => true | _ => false),
                               clos :: args)
                  | doXfer (CFG.Goto jmp) = doJump jmp
                  | doXfer (CFG.If(_, jmp1, jmp2)) = (doJump jmp1; doJump jmp2)
                  | doXfer (CFG.Switch(x, cases, dflt)) = (
                      List.app (doJump o #2) cases;
                      Option.app doJump dflt)
                  | doXfer (CFG.HeapCheck{nogc, ...}) = doJump nogc
                  | doXfer (CFG.AllocCCall{ret, args, ...}) = (
                      List.app escape args;
                      doJump ret)
                and doApply (f, chk, args) = (case getValue f 
                       of LABELS targets => LSet.app (fn lab => doLabel (lab, chk, args)) targets
                        | BOT => ()
                        | TOP => List.app escape args
                        | v => raise Fail(concat[
                              "type error: doApply(", CFG.Var.toString f, ", [",
                              String.concatWith "," (List.map CFG.Var.toString args),
                              "]); getValue(", CFG.Var.toString f, ") = ", valueToString v
                            ])
                      (* end case *))
                and doJump (lab, args) =
                      doLabel (lab,
                               ("Jump(" ^ (CFG.Label.toString lab) ^ ")", (fn _ => false)),
                               args)
                and doLabel (lab, chk, args) = (case CFG.Label.kindOf lab
                       of CFG.LK_Func{func, ...} => doFunc (func, chk, args)
                        | CFG.LK_Block block => doBlock (block, (#1 chk), args)
                        | _ => raise Fail "xfer to unknown label"
                      (* end case *))
                and doBlock (start as CFG.BLK{lab, args=params, ...}, dbg, args) = let
                      fun debugMsg () = print (concat[
                              "typeError: doBlock(", CFG.Label.toString lab, 
                              ", ", dbg, 
                              ", [",
                              String.concatWith "," (List.map CFG.Var.toString args),
                              "]); params = ",
                              String.concatWith "," (List.map CFG.Var.toString params),
                              "\n"
                            ])
                      in
                        if List.length args = List.length params
                        then ListPair.appEq addInfo' (params, args)
                        else if !debugFlg then debugMsg () else ()
                      end
                and doFunc (f as CFG.FUNC{lab, entry, start as CFG.BLK{body,args=params, ...}, ...}, chk, args) = let
                      fun debugMsg () = print (concat[
                              "typeError: doFunc(", CFG.Label.toString lab, 
                              ", ", #1 chk, 
                              ", [",
                              String.concatWith "," (List.map CFG.Var.toString args),
                              "]); CFG.paramsOfConv(entry) = ",
                              String.concatWith "," (List.map CFG.Var.toString (CFG.paramsOfConv (entry, params))),
                              "\n"
                            ])
                      in
                      if (#2 chk) entry
                         andalso List.length args = List.length (CFG.paramsOfConv (entry,params))
                         then ListPair.appEq addInfo' (CFG.paramsOfConv (entry,params), args)
                      else if !debugFlg then debugMsg () else ()
                      end
                fun doTopFunc (f as CFG.FUNC{lab, entry, start as CFG.BLK{body=stBody,exit,...}, body}) = (
                      List.app doExp stBody;
                      doXfer exit;
                      List.app (fn (blk as CFG.BLK{body, exit, ...}) => (List.app doExp body; doXfer exit)) body)
                in
                  changed := false;
                  List.app doTopFunc code;
                  !changed
                end
          fun iterate () = if onePass() then iterate() else ()
          in
          (* initialize the arguments to the module entry to top *)
            case code
             of CFG.FUNC{lab, entry=CFG.StdFunc{clos, ret, exh}, start as CFG.BLK{args,...}, ...} :: _ => (
                  setSites (lab, Unknown);
                  setValue (clos, TOP); List.app (fn x => setValue (x, TOP)) args;
                  setValue (ret, TOP); setValue (exh, TOP))
              | _ => raise Fail "strange module entry"
            (* end case *);
          (* iterate to a fixed point *)
            iterate ();
          (* compute call-side information for labels *)
            computeCallSites code;
          (* print results of cfa *)
            if !resultsFlg then printResults code else ()
          end

  (* return the set of labels that a control transfer targets; the empty set
   * is used to represent unknown control flow.
   *)
    fun labelsOf xfer = let
          fun labelSet f = (case getValue f
                 of LABELS s => SOME s
                  | TOP => NONE
                  | BOT => NONE (* because of dead code *)
                  | v => raise Fail(concat[
                        "labelsOf: getValue(", CFG.Var.toString f, ") = ", valueToString v
                      ])
                (* end case *))
          in
            case xfer
             of CFG.StdApply{f, ...} => labelSet f
              | CFG.StdThrow{k, ...} => labelSet k
              | CFG.Apply{f, ...} => labelSet f
              | _ => SOME (LSet.addList(LSet.empty, CFGUtil.labelsOfXfer xfer))
            (* end case *)
          end

  end
