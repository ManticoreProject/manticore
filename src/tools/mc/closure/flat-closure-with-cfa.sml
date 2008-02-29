(* flat-closure-with-cfa.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * This is a simple closure conversion algorithm.  In general, function
 * closures are represented as an environment-pointer (EP) / code-pointer (CP)
 * pair.  Mutually recursive functions share the same environment and use direct
 * calls to each other.  Continuation closures are represented as a record with
 * the first word containing the code pointer.
 *)

structure FlatClosureWithCFA : sig

    val convert : CPS.module -> CFG.module

  end = struct

    structure CFA = struct
      open CFACPS
      (* val valueOf = fn x => TOP *)
      (* val isEscaping = fn x => true *)
      val analyze = BasicControl.mkTracePassSimple {
              passName = "cfa",
              pass = analyze
            }
    end

    structure FV = struct
      open FreeVars
      val analyze = BasicControl.mkTracePassSimple {
              passName = "fv",
              pass = analyze
            }
    end

    structure VMap = CPS.Var.Map

  (* return the variable of a lambda *)
    fun funVar (CPS.FB{f, ...}) = f

(*
  (* convert from CPS types to CFG types, guided by CFA values; *)
  (* BOT values can arise in CFA from dead code. *)
    fun cvtTy (CPSTy.T_Any, _) = CFG.T_Any
      | cvtTy (CPSTy.T_Enum w, CFA.TOP) = CFG.T_Enum w
      | cvtTy (CPSTy.T_Enum w, CFA.BOT) = CFG.T_Enum w
      | cvtTy (CPSTy.T_Raw rTy, CFA.TOP) = CFGTy.T_Raw rTy
      | cvtTy (CPSTy.T_Raw rTy, CFA.TUPLE _) = CFGTy.T_Raw rTy (* datatypes *)
      | cvtTy (CPSTy.T_Raw rTy, CFA.BOT) = CFGTy.T_Raw rTy
      | cvtTy (CPSTy.T_Tuple(mut, tys), CFA.TOP) = 
          CFG.T_Tuple(mut, List.map cvtTyTop tys)
      | cvtTy (CPSTy.T_Tuple(mut, tys), CFA.TUPLE vs) = let
          val tysLen = List.length tys
          val vsLen = List.length vs
          val vs' = if tysLen <= vsLen
                       then List.take (vs, tysLen)
                    else vs @ (List.tabulate (tysLen - vsLen, fn _ => CFA.TOP))
          in
            CFG.T_Tuple(mut, ListPair.map cvtTy (tys, vs))
          end
      | cvtTy (CPSTy.T_Tuple(mut, tys), CFA.BOT) = 
          CFG.T_Tuple(mut, List.map cvtTyBot tys)
      | cvtTy (CPSTy.T_Addr ty, CFA.TOP) = CFG.T_Addr(cvtTyTop ty)
      | cvtTy (CPSTy.T_Addr ty, CFA.BOT) = CFG.T_Addr(cvtTyBot ty)
      | cvtTy (ty as CPSTy.T_Fun(_, []), v) = cvtStdContTy (ty, v, CFGTy.T_Any)
      | cvtTy (ty as CPSTy.T_Fun _, v) = cvtStdFunTy (ty, v, CFGTy.T_Any)
      | cvtTy (CPSTy.T_CFun cproto, _) = CFGTy.T_CFun cproto
      | cvtTy (CPSTy.T_VProc, CFA.TOP) = CFGTy.T_VProc
      | cvtTy (CPSTy.T_VProc, CFA.BOT) = CFGTy.T_VProc
      | cvtTy (ty, v) = raise Fail(concat[
           "bogus type ", CPSTyUtil.toString ty, " : ", CFA.valueToString v])
    and cvtTyTop ty = cvtTy (ty, CFA.TOP)
    and cvtTyBot ty = cvtTy (ty, CFA.BOT)

  (* convert a function type to a standard-function type, guided by CFA values *)
    and cvtStdFunTy (ty, v, closTy) = 
          CFG.T_Tuple(false, [closTy, cvtStdFunTyAux (ty, v, closTy)])
    and cvtStdFunTyAux (ty, CFA.TOP, closTy) = cvtStdFunTyAuxStd (ty, closTy)
      | cvtStdFunTyAux (ty, CFA.BOT, closTy) = cvtStdFunTyAuxStd (ty, closTy)
      | cvtStdFunTyAux (ty, v as CFA.LAMBDAS fs, closTy) = 
          if CPS.Var.Set.isEmpty fs then cvtStdFunTyAuxStd (ty, closTy)
          else let
          val SOME f = CPS.Var.Set.find (fn _ => true) fs
          val CPS.VK_Fun (CPS.FB {params, rets, ...}) = CPS.Var.kindOf f
          in
             if CFA.isEscaping f
                then cvtStdFunTyAuxStd (ty, closTy)
             else cvtStdFunTyAuxKwn (ty, (params, rets), closTy)
          end
      | cvtStdFunTyAux (ty, v, closTy) = raise Fail(concat[
          "bogus function type ", CPSTyUtil.toString ty, " : ", CFA.valueToString v])
    and cvtStdFunTyAuxStd (CPSTy.T_Fun(argTys, [retTy, exhTy]), closTy) = CFGTy.T_StdFun{
            clos = closTy,
            args = List.map cvtTyTop argTys,
            ret = cvtStdContTy (retTy, CFA.TOP, CFGTy.T_Any),
            exh = cvtStdContTy (exhTy, CFA.TOP, CFGTy.T_Any)
          }
      | cvtStdFunTyAuxStd (CPSTy.T_Any, closTy) = CFGTy.T_StdFun{
            clos = closTy,
            args = [CFGTy.T_Any],
            ret = cvtStdContTy (CPSTy.T_Any, CFA.TOP, CFGTy.T_Any),
            exh = cvtStdContTy (CPSTy.T_Any, CFA.TOP, CFGTy.T_Any)
          }
      | cvtStdFunTyAuxStd (ty, closTy) = raise Fail(concat[
          "bogus function type ", CPSTyUtil.toString ty])
    and cvtStdFunTyAuxKwn (CPSTy.T_Fun(argTys, retTys), (args, rets), closTy) = let
          fun cvtTy' (ty, x) = cvtTy (ty, CFA.valueOf x)
          fun cvtStdContTy' (ty, x) = cvtStdContTy (ty, CFA.valueOf x, CFGTy.T_Any)
          in
            CFGTy.T_KnownFunc{
              clos = closTy,
              args = (ListPair.mapEq cvtTy' (argTys, args)) @ 
                     (ListPair.mapEq cvtStdContTy' (retTys, rets))
            }
          end
      | cvtStdFunTyAuxKwn (ty, (args, rets), closTy) = raise Fail(concat[
          "bogus function type ", CPSTyUtil.toString ty])

  (* convert a continuation type to a standard-continuation type, guided by CFA values *)
    and cvtStdContTy (ty, v, closTy) = CFG.T_OpenTuple[cvtStdContTyAux (ty, v, closTy)]
    and cvtStdContTyAux (ty, CFA.TOP, closTy) = cvtStdContTyAuxStd (ty, closTy)
      | cvtStdContTyAux (ty, CFA.BOT, closTy) = cvtStdContTyAuxStd (ty, closTy)
      | cvtStdContTyAux (ty, CFA.LAMBDAS fs, closTy) = 
          if CPS.Var.Set.isEmpty fs then cvtStdContTyAuxStd (ty, closTy)
          else let
          val SOME f = CPS.Var.Set.find (fn _ => true) fs
          val CPS.VK_Cont (CPS.FB {params, rets = [], ...}) = CPS.Var.kindOf f
          in
             if CFA.isEscaping f
                then cvtStdContTyAuxStd (ty, closTy)
             else cvtStdContTyAuxKwn (ty, params, closTy)
          end
      | cvtStdContTyAux (ty, v, closTy) = raise Fail(concat[
          "bogus continuation type ", CPSTyUtil.toString ty, " : ", CFA.valueToString v])
    and cvtStdContTyAuxStd (CPSTy.T_Fun(argTys, []), closTy) = let
          val argTys = List.map cvtTyTop argTys
          in
            CFGTy.T_StdCont{
                clos=CFGTy.T_OpenTuple[CFGTy.T_StdCont{clos=closTy, args=argTys}],
                args=argTys
            }
          end
      | cvtStdContTyAuxStd (CPSTy.T_Any, closTy) = 
          CFGTy.T_StdCont{
              clos=CFGTy.T_OpenTuple[CFGTy.T_StdCont{clos=closTy, args=[CFGTy.T_Any]}],
              args=[CFGTy.T_Any]
          }
      | cvtStdContTyAuxStd (ty, closTy) = raise Fail(concat[
          "bogus continuation type ", CPSTyUtil.toString ty])
    and cvtStdContTyAuxKwn (CPSTy.T_Fun(argTys, []), args, closTy) = let
          fun cvtTy' (ty, x) = cvtTy (ty, CFA.valueOf x)
          val argTys = ListPair.mapEq cvtTy' (argTys, args)
          in
            CFGTy.T_KnownFunc{
              clos = CFGTy.T_OpenTuple[CFGTy.T_KnownFunc{clos=closTy, args=argTys}],
              args = argTys
            }
          end
      | cvtStdContTyAuxKwn (ty, args, closTy) = raise Fail(concat[
          "bogus continuation type ", CPSTyUtil.toString ty])
*)

  (* convert from CPS types to CFG types, guided by CFA values; *)
  (* BOT values can arise in CFA from dead code. *)
    fun cvtTy (CPSTy.T_Any, _) = CFG.T_Any
      | cvtTy (CPSTy.T_Enum w, CFA.TOP) = CFG.T_Enum w
      | cvtTy (CPSTy.T_Enum w, CFA.BOT) = CFG.T_Enum w
      | cvtTy (CPSTy.T_Raw rTy, CFA.TOP) = CFGTy.T_Raw rTy
      | cvtTy (CPSTy.T_Raw rTy, CFA.TUPLE _) = CFGTy.T_Raw rTy (* datatypes *)
      | cvtTy (CPSTy.T_Raw rTy, CFA.BOT) = CFGTy.T_Raw rTy
      | cvtTy (CPSTy.T_Tuple(mut, tys), CFA.TOP) = 
          CFG.T_Tuple(mut, List.map cvtTyTop tys)
      | cvtTy (CPSTy.T_Tuple(mut, tys), CFA.TUPLE vs) = let
          val tysLen = List.length tys
          val vsLen = List.length vs
          val vs' = if tysLen <= vsLen
                       then List.take (vs, tysLen)
                    else vs @ (List.tabulate (tysLen - vsLen, fn _ => CFA.TOP))
          in
            CFG.T_Tuple(mut, ListPair.map cvtTy (tys, vs'))
          end
      | cvtTy (CPSTy.T_Tuple(mut, tys), CFA.BOT) = 
          CFG.T_Tuple(mut, List.map cvtTyBot tys)
      | cvtTy (CPSTy.T_Addr ty, CFA.TOP) = CFG.T_Addr(cvtTyTop ty)
      | cvtTy (CPSTy.T_Addr ty, CFA.BOT) = CFG.T_Addr(cvtTyBot ty)
      | cvtTy (ty as CPSTy.T_Fun(_, []), v) = cvtStdContTy (ty, v)
      | cvtTy (ty as CPSTy.T_Fun _, v) = cvtStdFunTy (ty, v)
      | cvtTy (CPSTy.T_CFun cproto, _) = CFGTy.T_CFun cproto
      | cvtTy (CPSTy.T_VProc, CFA.TOP) = CFGTy.T_VProc
      | cvtTy (CPSTy.T_VProc, CFA.BOT) = CFGTy.T_VProc
      | cvtTy (ty, v) = raise Fail(concat[
           "bogus type ", CPSTyUtil.toString ty, " : ", CFA.valueToString v])
    and cvtTyTop ty = cvtTy (ty, CFA.TOP)
    and cvtTyBot ty = cvtTy (ty, CFA.BOT)

  (* convert a function type to a standard-function type, guided by CFA values *)
    and cvtStdFunTy (ty, v) = CFG.T_Tuple(false, [CFG.T_Any, cvtStdFunTyAux (ty, v)])
    and cvtStdFunTyAux (ty, CFA.TOP) = cvtStdFunTyAuxStd ty
      | cvtStdFunTyAux (ty, CFA.BOT) = cvtStdFunTyAuxStd ty
      | cvtStdFunTyAux (ty, v as CFA.LAMBDAS fs) = 
          if CPS.Var.Set.isEmpty fs then cvtStdFunTyAuxStd ty
          else let
          val SOME f = CPS.Var.Set.find (fn _ => true) fs
          val CPS.VK_Fun (CPS.FB {params, rets, ...}) = CPS.Var.kindOf f
          in
             if CFA.isEscaping f
                then cvtStdFunTyAuxStd ty
             else cvtStdFunTyAuxKwn (ty, (params, rets))
          end
      | cvtStdFunTyAux (ty, v) = raise Fail(concat[
          "bogus function type ", CPSTyUtil.toString ty, " : ", CFA.valueToString v])
    and cvtStdFunTyAuxStd (CPSTy.T_Fun(argTys, [retTy, exhTy])) = CFGTy.T_StdFun{
            clos = CFGTy.T_Any,
            args = List.map cvtTyTop argTys,
            ret = cvtStdContTy (retTy, CFA.TOP),
            exh = cvtStdContTy (exhTy, CFA.TOP)
          }
      | cvtStdFunTyAuxStd (CPSTy.T_Fun(argTys, [retTy])) = CFGTy.T_KnownFunc{
            clos = CFGTy.T_Any,
            args = List.map cvtTyTop argTys @ [cvtStdContTy (retTy, CFA.TOP)]
          }
      | cvtStdFunTyAuxStd (CPSTy.T_Any) = CFGTy.T_StdFun{
            clos = CFGTy.T_Any,
            args = [CFGTy.T_Any],
            ret = cvtStdContTy (CPSTy.T_Any, CFA.TOP),
            exh = cvtStdContTy (CPSTy.T_Any, CFA.TOP)
          }
      | cvtStdFunTyAuxStd ty = raise Fail(concat[
          "bogus function type ", CPSTyUtil.toString ty])
    and cvtStdFunTyAuxKwn (CPSTy.T_Fun(argTys, retTys), (args, rets)) = let
          fun cvtTy' (ty, x) = cvtTy (ty, CFA.valueOf x)
          fun cvtStdContTy' (ty, x) = cvtStdContTy (ty, CFA.valueOf x)
          in
            CFGTy.T_KnownFunc{
              clos = CFGTy.T_Any,
              args = (ListPair.mapEq cvtTy' (argTys, args)) @ 
                     (ListPair.mapEq cvtStdContTy' (retTys, rets))
            }
          end
      | cvtStdFunTyAuxKwn (ty, _) = raise Fail(concat[
          "bogus function type ", CPSTyUtil.toString ty])

  (* convert a continuation type to a standard-continuation type, guided by CFA values *)
    and cvtStdContTy (ty, v) = CFG.T_OpenTuple[cvtStdContTyAux (ty, v)]
    and cvtStdContTyAux (ty, CFA.TOP) = cvtStdContTyAuxStd ty
      | cvtStdContTyAux (ty, CFA.BOT) = cvtStdContTyAuxStd ty
      | cvtStdContTyAux (ty, CFA.LAMBDAS fs) = 
          if CPS.Var.Set.isEmpty fs then cvtStdContTyAuxStd ty
          else let
          val SOME f = CPS.Var.Set.find (fn _ => true) fs
          val CPS.VK_Cont (CPS.FB {params, rets = [], ...}) = CPS.Var.kindOf f
          in
             if CFA.isEscaping f
                then cvtStdContTyAuxStd ty
             else cvtStdContTyAuxKwn (ty, params)
          end
      | cvtStdContTyAux (ty, v) = raise Fail(concat[
          "bogus continuation type ", CPSTyUtil.toString ty, " : ", CFA.valueToString v])
    and cvtStdContTyAuxStd (CPSTy.T_Fun(argTys, [])) =
          CFGTyUtil.stdContTy(CFGTy.T_Any, List.map cvtTyTop argTys)
      | cvtStdContTyAuxStd (CPSTy.T_Any) = 
          CFGTyUtil.stdContTy(CFGTy.T_Any, [CFGTy.T_Any])
      | cvtStdContTyAuxStd ty = raise Fail(concat[
          "bogus continuation type ", CPSTyUtil.toString ty])
    and cvtStdContTyAuxKwn (CPSTy.T_Fun(argTys, []), args) = let
          fun cvtTy' (ty, x) = cvtTy (ty, CFA.valueOf x)
          in
            CFGTyUtil.kwnContTy(CFGTy.T_Any, ListPair.mapEq cvtTy' (argTys, args))
          end
      | cvtStdContTyAuxKwn (ty, _) = raise Fail(concat[
          "bogus continuation type ", CPSTyUtil.toString ty])

    fun cvtTyOfVar x =
       ((cvtTy (CPS.Var.typeOf x, CFA.valueOf x))
        handle Fail s => raise Fail(concat["cvtTyOfVar(", CPS.Var.toString x, ") ==> ", s]))
    val cvtTyOfVar = fn x => let
          val ty = CPS.Var.typeOf x
          val v = CFA.valueOf x
          val ty' = cvtTyOfVar x
          in
            if Controls.get ClosureControls.debug
              then print(concat[
                "cvtTyOfVar(", CPS.Var.toString x, " : ",
                "typeOf => ", CPSTyUtil.toString ty, " ; ",
                "valueOf => ", CFA.valueToString v, ") => ", CFGTyUtil.toString ty', "\n"])
              else ();
            ty'
          end


  (* assign labels to functions and continuations *)
    local
      val {getFn : CPS.var -> CFG.label, setFn, ...} =
            CPS.Var.newProp (fn f => raise Fail(concat["labelOf(", CPS.Var.toString f, ")"]))
    in
    fun assignLabels lambda = let
          fun assignFB (CPS.FB{f, body, ...}) = let
                val lab = CFG.Label.new(CPS.Var.nameOf f, 
                                        cvtStdFunTyAux(CPS.Var.typeOf f,
                                                       CFA.valueOf f))
                in
                  setFn (f, lab);
                  assignExp body
                end
          and assignKB (CPS.FB{f, body, ...}) = let
                val lab = CFG.Label.new(CPS.Var.nameOf f, 
                                        cvtStdContTyAux(CPS.Var.typeOf f,
                                                        CFA.valueOf f))
                in
                  setFn (f, lab);
                  assignExp body
                end
          and assignExp (CPS.Let(_, _, e)) = assignExp e
            | assignExp (CPS.Fun(fbs, e)) = (List.app assignFB fbs; assignExp e)
            | assignExp (CPS.Cont(kb, e)) = (assignKB kb; assignExp e)
            | assignExp (CPS.If(_, e1, e2)) = (assignExp e1; assignExp e2)
            | assignExp (CPS.Switch(_, cases, dflt)) = (
                List.app (assignExp o #2) cases;
                Option.app assignExp dflt)
            | assignExp _ = ()
          in
            assignFB lambda
          end
    val labelOf = getFn
    end

    datatype loc
      = Local of CFG.var        (* bound in the current function *)
      | Global of int           (* at the ith slot of the current closure *)
      | EnclFun                 (* the enclosing function (or one that shares the *)
                                (* same closure). *)
      | EnclCont                (* the enclosing continuation function *)
      | Extern of CFG.label     (* bound to an external variable (e.g., C function *)

  (* an envrionment for mapping from CPS variables to CFG variables.  We also
   * track the current closure.
   *)
    datatype env = E of {ep : CFG.var, env : loc VMap.map}

(* +DEBUG *)
    fun locToString (Local x) = concat["L(", CFG.Var.toString x, ")"]
      | locToString (Global i) = concat["G(", Int.toString i, ")"]
      | locToString EnclFun = "EnclFun"
      | locToString EnclCont = "EnclCont"
      | locToString (Extern lab) = concat["X(", CFG.Label.toString lab, ")"]
    fun prEnv (E{ep, env}) = let
          fun f (x, loc, false) = (print(concat[", ", CPS.Var.toString x, "->", locToString loc]); false)
            | f (x, loc, true) = (print(concat[CPS.Var.toString x, "->", locToString loc]); false)
          in
            print(concat["E{ep = ", CFG.Var.toString ep, " : ", CFGTyUtil.toString(CFG.Var.typeOf ep), "\n"]);
            print "  env = {";
            VMap.foldli f true env;
            print "}\n}\n"
          end
(* -DEBUG *)

    fun envPtrOf (E{ep, ...}) = ep

    fun newEnv externEnv ep = E{ep = ep, env = externEnv}

  (* create a new environment that gives a fresh name to the environment pointer *)
    fun envWithFreshEP (E{ep, env}) = E{ep = CFG.Var.copy ep, env=env}

    fun insertVar (E{ep, env}, x, x') = E{ep=ep, env=VMap.insert(env, x, x')}

    fun findVar' (E{env, ...}, x) = (case VMap.find(env, x)
          of SOME loc => SOME loc
           | NONE => NONE
          (* end case *))
    fun findVar (env, x) = (case findVar'(env, x)
          of SOME loc => loc
           | NONE => raise Fail("unbound variable " ^ CPS.Var.toString x)
          (* end case *))

  (* create a new CFG variable for a CPS variable *)
    fun newVar x = CFG.Var.new (
          CPS.Var.nameOf x,
          cvtTyOfVar x)
    val newVar = fn x => let
          val x' = newVar x
          in
            if Controls.get ClosureControls.debug
              then print(concat[
                "newVar(", CPS.Var.toString x, ") => ", CFG.Var.toString x', "\n"])
              else ();
            x'
          end

    fun newEP [] =
      (* NOTE: T_Enum(0w0) is the correct type here, but that causes problems in CheckCFG. *)
          CFG.Var.new ("ep", CFGTy.T_Any)
      | newEP tys = CFG.Var.new ("ep", CFGTy.T_Tuple(false, tys))
    val newEP = fn tys => let
          val x' = newEP tys
          in
            if Controls.get ClosureControls.debug
              then print(concat[
                "newEP(_) => ", CFG.Var.toString x', "\n"])
              else ();
            x'
          end

    fun newLocal (env, x) = let
          val x' = newVar x
          in
            (insertVar(env, x, Local x'), x')
          end
 
    fun newLocals (E{ep, env}, xs) = let
          fun f (x, (env, xs')) = let
                val x' = newVar x
                in
                  (VMap.insert(env, x, Local x'), x'::xs')
                end
          val (env, xs) = List.foldl f (env, []) xs
          in
            (E{ep=ep, env=env}, List.rev xs)
          end
 
    fun bindLabel lab = let
          val labVar = CFG.Var.new(CFG.Label.nameOf lab, CFG.Label.typeOf lab)
          in
            (CFG.mkLabel(labVar, lab), labVar)
          end
    val bindLabel = fn lab => let
          val (bind, x') = bindLabel lab
          in
            if Controls.get ClosureControls.debug
              then print(concat[
                "bindLabel(", CFG.Label.toString lab, ") => ", CFG.Var.toString x', "\n"])
              else ();
            (bind, x')
          end

  (* lookup a CPS variable in the environment.  If it has to be fetched from
   * a closure, we introduce a new temporary for it.
   * QUESTION: should we cache the temp in the environment?
   *)
    fun lookupVar (E{ep, env}, x) = (case VMap.find(env, x)
           of SOME(Local x') => ([], x')
            | SOME(Global i) => let (* fetch from closure *)
                val tmp = newVar x
                in
                  ([CFG.mkSelect(tmp, i, ep)], tmp)
                end
            | SOME EnclFun => let (* build <ep, cp> pair *)
                val (b, lab) = bindLabel(labelOf x)
                val tmp = CFG.Var.new(
                        CPS.Var.nameOf x,
                        CFGTy.T_Tuple(false, [CFG.Var.typeOf ep, CFG.Var.typeOf lab]))
                in
                  ([CFG.mkAlloc(tmp, [ep, lab]), b], tmp)
                end
            | SOME EnclCont => ([], ep)
            | SOME(Extern lab) => let
                val tmp = newVar x
                in
                  ([CFG.mkLabel(tmp, lab)], tmp)
                end
            | NONE => raise Fail(concat[
                  "unbound variable ", CPS.Var.toString x, "; ep = ", CFG.Var.toString ep
                ])
          (* end case *))
    val lookupVar = fn (env, x) => let
          val (binds, x') = lookupVar (env, x)
          in
            if Controls.get ClosureControls.debug
              then print(concat[
                "lookupVar(_,", CPS.Var.toString x, ") => ", CFG.Var.toString x', "\n"])
              else ();
            (binds, x')
          end

    fun lookupVars (env, xs) = let
          fun lookup ([], binds, xs) = (binds, xs)
            | lookup (x::xs, binds, xs') = let
                val (b, x) = lookupVar(env, x)
                in
                  lookup (xs, b @ binds, x::xs')
                end
          in
            lookup (List.rev xs, [], [])
          end

  (* given a set of free CPS variables that define the environment of a function, create the
   * argument variables and bindings to build the closure and the parameter variables and
   * environment for the function's body.
   *)
    fun mkFunClosure externEnv (env, fv) = let
          fun mkArgs (x, (i, binds, clos, xs)) = let
                val (b, x') = lookupVar(env, x)
                in
                  (i+1, b@binds, VMap.insert(clos, x, Global i), x'::xs)
                end
          val (_, binds, clos, cfgArgs) =
                CPS.Var.Set.foldl mkArgs (0, [], externEnv, []) fv
          val cfgArgs = List.rev cfgArgs
          val ep = newEP (List.map CFG.Var.typeOf cfgArgs)
          in
            (binds, cfgArgs, E{ep = ep, env = clos})
          end

  (* given a set of free CPS variables that define the environment of a continuation, create the
   * argument variables and bindings to build the closure and the parameter variables and
   * environment for the continuation's body.
   *)
    fun mkContClosure externEnv (env, params, fv, mkContTy) = let
          val params' = List.map newVar params
          fun mkArgs (x, (i, binds, clos, xs)) = let
                val (b, x') = lookupVar(env, x)
                in
                  (i+1, b@binds, VMap.insert(clos, x, Global i), x'::xs)
                end
        (* the initial environment is the externs plus the parameters *)
          val env = ListPair.foldl
                (fn (x, x', env) => VMap.insert(env, x, Local x'))
                  externEnv (params, params')
          val (_, binds, clos, cfgArgs) =
                CPS.Var.Set.foldl mkArgs (1, [], env, []) fv
          val cfgArgs = List.rev cfgArgs
          val ep = newEP (
                mkContTy(CFGTy.T_Any, List.map CFG.Var.typeOf params')
                :: List.map CFG.Var.typeOf cfgArgs)
          in
            (binds, cfgArgs, E{ep = ep, env = clos}, params')
          end

    fun convert (m as CPS.MODULE{name, externs, body}) = let
          val blocks = ref []
        (* construct an initial environment that maps the CPS externs to CFG labels *)
          val (externs, externEnv) = let
                fun cvt (CFunctions.CFun{var, name, retTy, argTys, attrs}, (cfs, env)) = let
                      val lab = CFG.Label.new(name, cvtTyOfVar var)
                      val cf = CFG.mkCFun{var=lab, name=name, argTys=argTys, retTy=retTy, attrs=attrs}
                      in
                        (cf::cfs, VMap.insert(env, var, Extern lab))
                      end
                in
                  List.foldl cvt ([], VMap.empty) externs
                end
          val newEnv = newEnv externEnv
          val mkFunClosure = mkFunClosure externEnv
          val mkContClosure = mkContClosure externEnv
        (* convert an expression to a CFG FUNC; note that this function will convert
         * any nested functions first.
         *)
          fun cvtExp (env, lab, conv, e) = let
                val () =
                   if Controls.get ClosureControls.debug
                      then (print(concat["********************\ncvtExp: lab = ", CFG.Label.toString lab, "\n"]); prEnv env)
                   else ()
                fun finish (binds, xfer) = let
                      val func = CFG.mkLocalFunc (lab, conv, List.rev binds, xfer)
                      in
                        if Controls.get ClosureControls.debug
                           then print(concat["******************** finish ", CFG.Label.toString lab, "\n"])
                        else ();
                        blocks := func :: !blocks
                      end
                fun cvt (env, e, stms) = let
                      fun branch (lab, e) = let
                            val needsEP = ref false
                            val argEP = envPtrOf env
                            val paramEP = CFG.Var.copy argEP
                            val branchEnv = newEnv paramEP
                            fun f (x, (bEnv, args, params)) = (case findVar(env, x)
                                   of Local x' => let
                                        val (bEnv', x'') = newLocal(bEnv, x)
                                        in
                                          (bEnv', x' :: args, x'' :: params)
                                        end
                                    | Global i => (
                                        needsEP := true; 
                                        (insertVar(bEnv, x, Global i), args, params))
                                    | EnclFun => (
                                        needsEP := true;
                                        (insertVar(bEnv, x, EnclFun), args, params))
                                    | EnclCont => (
                                        needsEP := true;
                                        (insertVar(bEnv, x, EnclCont), args, params))
                                    | Extern _ => raise Fail "unexpected extern in free-var list"
                                  (* end case *))
                            val (branchEnv, args, params) =
                                  CPS.Var.Set.foldr f (branchEnv, [], []) (FV.freeVarsOfExp e)
                           (* if there are any free globals in e, then we include
                            * the environment pointer as an argument.
                            *)
                            val (args, params) = if !needsEP
                                  then (argEP :: args, paramEP :: params)
                                  else (args, params)
                            val lab = CFG.Label.new(
                                  lab,
                                  CFGTy.T_Block{args = List.map CFG.Var.typeOf params})
                            in
                              cvtExp (branchEnv, lab, CFG.Block{args = params}, e);
                              (lab, args)
                            end
                      in
                        case e
                         of CPS.Let(lhs, rhs, e) => let
                              val (binds, env') = cvtRHS(env, lhs, rhs)
                              in
                                cvt (env', e, binds @ stms)
                              end
                          | CPS.Fun(fbs, e) => let
                              val (binds, env) = cvtFunc(env, fbs)
                              in
                                cvt (env, e, binds @ stms)
                              end
                          | CPS.Cont(fb, e) => let
                              val (binds, env) = cvtCont(env, fb)
                              in
                                cvt (env, e, binds @ stms)
                              end
                          | CPS.If(x, e1, e2) => let
                              val (binds, x) = lookupVar(env, x)
                              in
                                finish(binds @ stms,
                                  CFG.If(x, branch("then", e1), branch("else", e2)))
                              end
                          | CPS.Switch(x, cases, dflt) => let
                              val (binds, x) = lookupVar(env, x)
                              in
                                finish(binds @ stms,
                                  CFG.Switch(x,
                                    List.map (fn (i,e) => (i, branch("case", e))) cases,
                                    case dflt 
                                     of NONE => NONE
                                      | SOME e => SOME (branch("default", e))))
                              end
                          | CPS.Apply(f, args, rets) => let
                              val finish = fn (binds, xfer) => finish (binds @ stms, xfer)
                              in
                                cvtApply (env, f, args, rets, finish)
                              end
                          | CPS.Throw(k, args) => let
                              val finish = fn (binds, xfer) => finish (binds @ stms, xfer)
                              in
                                cvtThrow (env, k, args, finish)
                              end
                        (* end case *)
                      end
                in
                  cvt (env, e, [])
                end
        (* convert a CPS RHS to a list of CFG expressions, plus a new environment *)
          and cvtRHS (env, lhs, rhs) = (case (newLocals(env, lhs), rhs)
                 of ((env, lhs), CPS.Var ys) => let
                      val (binds, ys) = lookupVars (env, ys)
                      in
                        ([CFG.mkVar(lhs, ys)] @ binds, env)
                      end
                  | ((env, [x]), CPS.Const(lit, ty)) => ([CFG.mkConst(x, lit, cvtTyTop ty)], env)
                  | ((env, [x]), CPS.Cast(ty, y)) => let
                      val (binds, y') = lookupVar(env, y)
                      in
                        ([CFG.mkCast(x, cvtTy (ty, CFA.valueOf y), y')] @ binds, env)
                      end
                  | ((env, [x]), CPS.Select(i, y)) => let
                      val (binds, y) = lookupVar(env, y)
                      in
                        ([CFG.mkSelect(x, i, y)] @ binds, env)
                      end
                  | ((env, []), CPS.Update(i, y, z)) => let
                      val (binds, y) = lookupVar(env, y)
                      val (binds', z) = lookupVar(env, z)
                      in
                        ([CFG.mkUpdate(i, y, z)] @ binds' @ binds, env)
                      end
                  | ((env, [x]), CPS.AddrOf(i, y)) => let
                      val (binds, y) = lookupVar(env, y)
                      in
                        ([CFG.mkAddrOf(x, i, y)] @ binds, env)
                      end
                  | ((env, [x]), CPS.Alloc ys) => let
                      val (binds, ys) = lookupVars (env, ys)
                      in
                        ([CFG.mkAlloc(x, ys)] @ binds, env)
                      end
                  | ((env, [x]), CPS.GAlloc ys) => let
                      val (binds, ys) = lookupVars (env, ys)
                      in
                        ([CFG.mkGAlloc(x, ys)] @ binds, env)
                      end
                  | ((env, [x]), CPS.Promote y) => let
                      val (binds, y) = lookupVar (env, y)
                      in
                        ([CFG.mkPromote(x, y)] @ binds, env)
                      end
                  | ((env, [x]), CPS.Prim p) => let
                      val (mkP, args) = PrimUtil.explode p
                      val (binds, args) = lookupVars (env, args)
                      in
                        ([CFG.mkPrim(x, mkP args)] @ binds, env)
                      end
                  | ((env, res), CPS.CCall(f, args)) => let
                      val (binds, f::args) = lookupVars (env, f::args)
                      in
                        ([CFG.mkCCall(res, f, args)] @ binds, env)
                      end
                  | ((env, [vp]), CPS.HostVProc) => ([CFG.mkHostVProc(vp)], env)
                  | ((env, [x]), CPS.VPLoad(offset, vp)) => let
                      val (binds, vp) = lookupVar(env, vp)
                      in
                        ([CFG.mkVPLoad(x, offset, vp)] @ binds, env)
                      end
                  | ((env, []), CPS.VPStore(offset, vp, x)) => let
                      val (binds, [vp, x]) = lookupVars(env, [vp, x])
                      in
                        ([CFG.mkVPStore(offset, vp, x)] @ binds, env)
                      end
                  | (_, rhs) => raise Fail("ill-formed RHS binding: " ^ CPSUtil.rhsToString rhs)
                (* end case *))
        (* create a standard function convention for a list of parameters *)
          and stdFuncConvention (env, args, [ret, exh]) = let
                val env = envWithFreshEP env
                val (env, args) = newLocals (env, args)
                val (env, ret) = newLocal (env, ret)
                val (env, exh) = newLocal (env, exh)
                val conv = CFG.StdFunc{
                        clos = envPtrOf env,
                        args = args, ret = ret, exh = exh
                      }
                in
                  (env, conv)
                end
            | stdFuncConvention (env, args, rets as [_]) = 
                kwnFuncConvention (env, args, rets)
            | stdFuncConvention (env, args, rets) =
                raise Fail "non-standard apply convention"
        (* create a known function convention for a list of parameters *)
          and kwnFuncConvention (env, args, rets) = let
                val env = envWithFreshEP env
                val (env, args) = newLocals (env, args)
                val (env, rets) = newLocals (env, rets)
                val conv = CFG.KnownFunc{
                        clos = envPtrOf env,
                        args = args @ rets
                      }
                in
                  (env, conv)
                end
        (* convert bound functions *)
          and cvtFunc (env, fbs) = let
              (* the functions share a common environment tuple *)
                val (binds, clos, sharedEnv) =
                      mkFunClosure (env, FV.envOfFun(funVar(hd fbs)))
                val ep = newEP (List.map CFG.Var.typeOf clos)
                val bindEP = CFG.mkAlloc(ep, clos)
              (* map the names of the bound functions to EnclFun *)
                val sharedEnv = List.foldl
                      (fn (fb, env) => insertVar(env, funVar fb, EnclFun)) 
                      sharedEnv fbs
              (* convert an individual function binding; this includes creating its
               * code-pointer/environment-pointer pair and converting the function's body.
               *)
                fun cvtFB (CPS.FB{f, params, rets, body}, (binds, env)) = let
                      val lab = labelOf f
                      val (fbEnv, conv) = 
                            if CFA.isEscaping f
                              then stdFuncConvention (sharedEnv, params, rets)
                              else kwnFuncConvention (sharedEnv, params, rets)
                      val (bindLab, labVar) = bindLabel (labelOf f)
                      val (env', f') = newLocal (env, f)
                      val binds = CFG.mkAlloc(f', [ep, labVar]) :: bindLab :: binds
                      in
                      (* convert the function itself *)
                        cvtExp (fbEnv, labelOf f, conv, body);
                        (binds, env')
                      end
                in
                   List.foldl cvtFB (bindEP::binds, env) fbs
                end
        (* convert a bound continuation *)
          and cvtCont (env, CPS.FB{f, params, body, ...}) = let
                val (mkContTy, mkEntry) =
                      if CFA.isEscaping f
                        then (CFGTyUtil.stdContTy, CFG.StdCont)
                        else (CFGTyUtil.kwnContTy, CFG.KnownFunc)
                val (binds, clos, lambdaEnv, params') = 
                      mkContClosure (env, params, FV.envOfFun f, mkContTy)
                val conv = 
                      mkEntry{clos = envPtrOf lambdaEnv, args = params'}
                val (bindLab, labVar) = bindLabel (labelOf f)
                val contEnv = insertVar (lambdaEnv, f, EnclCont)  (* to support recursive conts *)
                val (env', k') = newLocal (env, f)
                val binds = CFG.mkAlloc(k', labVar :: clos) :: bindLab :: binds
                in
                  cvtExp (contEnv, labelOf f, conv, body);
                  (binds, env')
                end
        (* convert an apply *)
          and cvtApply (env, f, args, rets, finish) = (case CFA.valueOf f
                 of CFA.TOP => cvtStdApply (env, f, NONE, args, rets, finish)
                  | CFA.BOT => cvtStdApply (env, f, NONE, args, rets, finish)
                  | CFA.LAMBDAS gs => 
                      if CPS.Var.Set.isEmpty gs 
                        then cvtStdApply (env, f, NONE, args, rets, finish)
                      else let
                      val SOME g = CPS.Var.Set.find (fn _ => true) gs
                      val fTgt = if CPS.Var.Set.numItems gs = 1 then SOME g else NONE
                      in
                        if CFA.isEscaping g
                          then cvtStdApply (env, f, fTgt, args, rets, finish)
                          else cvtKwnApply (env, f, fTgt, args, rets, finish)
                      end
                (* end case *))
          and cvtStdApply (env, f, fTgt, args, rets as [_, _], finish) = let
                val (argBinds, args) = lookupVars(env, args)
                val (retBinds, [ret, exh]) = lookupVars(env, rets)
                fun bindEP () = let
                      val (fBinds, f') = lookupVar(env, f)
                      val ep = CFG.Var.new (CPS.Var.nameOf f ^ "_ep", CFGTy.T_Any)
                      in
                        (CFG.mkSelect(ep, 0, f') :: fBinds, f', ep)
                      end
                val (binds, xfer) = let
                      val (cp, ep, binds') = (case fTgt
                             of SOME g => let
                                  val (gBind, cp) = bindLabel (labelOf g)
                                  in
                                    case findVar'(env, g)
                                     of SOME EnclFun => (cp, envPtrOf env, [gBind])
                                      | _ => let
                                          val (epBinds, _, ep) = bindEP ()
                                          in
                                            (cp, ep, gBind :: epBinds)
                                          end
                                    (* end case *)
                                  end
                              | NONE => let
                                  val (epBinds, f', ep) = bindEP ()
                                  val cp = CFG.Var.new(
                                        CFG.Var.nameOf f',
                                        CFGTyUtil.select(CFG.Var.typeOf f', 1))
                                  val cpBind = CFG.mkSelect(cp, 1, f')
                                  in
                                    (cp, ep, cpBind :: epBinds)
                                  end
                            (* end case *))
                      val xfer = CFG.StdApply{
                              f = cp,
                              clos = ep,
                              args = args,
                              ret = ret,
                              exh = exh
                            }
                      in
                        (binds', xfer)
                      end
                in
                  finish (binds @ retBinds @ argBinds, xfer)
                end
            | cvtStdApply (env, f, fTgt, args, rets as [_], finish) = 
                cvtKwnApply (env, f, fTgt, args, rets, finish)
            | cvtStdApply (env, f, fTgt, args, rets, finish) = 
                raise Fail "non-standard apply convention"
          and cvtKwnApply (env, f, fTgt, args, rets, finish) = let
                val (argBinds, args) = lookupVars(env, args)
                val (retBinds, rets) = lookupVars(env, rets)
                fun bindEP () = let
                      val (fBinds, f') = lookupVar(env, f)
                      val ep = CFG.Var.new (CPS.Var.nameOf f ^ "_ep", CFGTy.T_Any)
                      in
                        (CFG.mkSelect(ep, 0, f') :: fBinds, f', ep)
                      end
                val (binds, xfer) = let
                      val (cp, ep, binds') = (case fTgt
                             of SOME g => let
                                  val (gBind, cp) = bindLabel (labelOf g)
                                  in
                                    case findVar'(env, g)
                                     of SOME EnclFun => (cp, envPtrOf env, [gBind])
                                      | _ => let
                                          val (epBinds, _, ep) = bindEP ()
                                          in
                                            (cp, ep, gBind :: epBinds)
                                          end
                                    (* end case *)
                                  end
                              | NONE => let
                                  val (epBinds, f', ep) = bindEP ()
                                  val cp = CFG.Var.new(
                                        CFG.Var.nameOf f',
                                        CFGTyUtil.select(CFG.Var.typeOf f', 1))
                                  val cpBind = CFG.mkSelect(cp, 1, f')
                                  in
                                    (cp, ep, cpBind :: epBinds)
                                  end
                            (* end case *))
                      val xfer = CFG.Apply{
                              f = cp,
                              clos = ep,
                              args = args @ rets
                            }
                      in
                        (binds', xfer)
                      end
                in
                  finish (binds @ retBinds @ argBinds, xfer)
                end
        (* convert a throw *)
          and cvtThrow (env, k, args, finish) = (case CFA.valueOf k 
                 of CFA.TOP => cvtStdThrow (env, k, NONE, args, finish)
                  | CFA.BOT => cvtStdThrow (env, k, NONE, args, finish)
                  | CFA.LAMBDAS gs => 
                      if CPS.Var.Set.isEmpty gs 
                        then cvtStdThrow (env, k, NONE, args, finish)
                      else let
                      val SOME g = CPS.Var.Set.find (fn _ => true) gs
                      val kTgt = if CPS.Var.Set.numItems gs = 1 then SOME g else NONE
                      in
                        if CFA.isEscaping g
                          then cvtStdThrow (env, k, kTgt, args, finish)
                          else cvtKwnThrow (env, k, kTgt, args, finish)
                      end
                (* end case *))
          and cvtStdThrow (env, k, kTgt, args, finish) = let
                val (kBinds, k') = lookupVar(env, k)
                val (argBinds, args') = lookupVars(env, args)
                val cp = CFG.Var.new(CFG.Var.nameOf k',
                                     CFGTyUtil.select(CFG.Var.typeOf k', 0))
              (* if valueOf(k) = LAMBDAS {g},
               * then we can refer directly to labelOf(g)
               *)
                val bindCP = (case kTgt
                       of SOME g => CFG.mkLabel(cp, labelOf g)
                        | NONE => CFG.mkSelect(cp, 0, k')
                      (* end case *))
                val xfer = CFG.StdThrow {
                        k = cp,
                        clos = k',
                        args = args'
                     }
                in
                  finish (bindCP :: (argBinds @ kBinds), xfer)
                end
          and cvtKwnThrow (env, k, kTgt, args, finish) = let
                val (kBinds, k') = lookupVar(env, k)
                val (argBinds, args') = lookupVars(env, args)
                val cp = CFG.Var.new(CFG.Var.nameOf k',
                                     CFGTyUtil.select(CFG.Var.typeOf k', 0))
              (* if valueOf(k) = LAMBDAS {g},
               * then we can refer directly to labelOf(g)
               *)
                val bindCP = (case kTgt
                       of SOME g => CFG.mkLabel(cp, labelOf g)
                        | NONE => CFG.mkSelect(cp, 0, k')
                      (* end case *))
                val xfer = CFG.Apply {
                        f = cp,
                        clos = k',
                        args = args'
                     }
                in
                  finish (bindCP :: (argBinds @ kBinds), xfer)
                end
        (* create the calling convention for the module *)
          fun cvtModLambda (CPS.FB{f, params, rets, body}) = let
                val ep = CFG.Var.new ("dummyEP", CFGTy.T_Any)
                val (env, conv) = stdFuncConvention (E{ep = ep, env = externEnv}, params, rets)
                in
                  cvtExp (env, labelOf f, conv, body)
                end
          in
            CFA.analyze m;
            FV.analyze m;
            assignLabels body;
            cvtModLambda body;
          (* we need to rebuild the entry function so that it has an exported lambda *)
            let val CFG.FUNC{lab, entry, body, exit} :: r = !blocks
            val init = CFG.mkExportFunc(lab, entry, body, exit, Atom.toString name ^ "_init")
            in
              CFG.mkModule(name, externs, init::r)
            end
          end
  end
