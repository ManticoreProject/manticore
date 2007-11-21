(* rewrite-hlops.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure RewriteHLOps : sig

    (* replace the high-level operators in the module with their
     * definitions; returns NONE if there was no change to the module.
     *)
    val rewrite : BOM.module -> BOM.module option

end = struct

    structure B = BOM
    structure BTy = BOMTy
    structure BU = BOMUtil
    structure H = HLOp
    structure ATbl = AtomTable
    structure RW = Rewrites
    structure PT = RW.PT (* XXX - Hack *)

    (* ____________________________________________________________ *)
    (* findHLOps() - Find the set of HLOps in a given BOM module. *)
    fun findHLOps (module as B.MODULE{name, externs, body}) = let

        val hlopEnv = ATbl.mkTable (32, Fail "hlopEnv")

        fun findHLOpsInExp (e as B.E_Pt(_, t)) = (case t
            of B.E_Let(lhs, e1, e2) => (findHLOpsInExp e1; findHLOpsInExp e2)
             | B.E_Stmt(lhs, rhs, e) => findHLOpsInExp e
             | B.E_Fun(fbs, e) =>
               (List.app findHLOpsInLambda fbs;
                findHLOpsInExp e)
             | B.E_Cont(fb, e) => (findHLOpsInLambda fb; findHLOpsInExp e)
             | B.E_If(x, e1, e2) => (findHLOpsInExp e1; findHLOpsInExp e2)
             | B.E_Case(x, cases, dflt) =>
               (List.app (fn (p, e) => findHLOpsInExp e) cases;
                case dflt
                 of SOME e => findHLOpsInExp e
                  | NONE => ())
             | B.E_Apply _ => ()
             | B.E_Throw _ => ()
             | B.E_Ret _ => ()
             | B.E_HLOp(hlOp, args, rets) =>
               ATbl.insert hlopEnv (H.name hlOp, hlOp)
            (* end case *))
        and findHLOpsInLambda (B.FB{f, params, exh, body}) =
            (findHLOpsInExp body)

    in
        findHLOpsInLambda body;
        hlopEnv
    end (* findHLOps *)

    (* ____________________________________________________________ *)
    (* FIXME: Might want to make a RWState structure. *)

    (* rwstate - Map from nonterminal name to benefit. *)
    type rwstate = IntInf.int AtomMap.map

    val emptyRWState = AtomMap.empty : rwstate

    val baseWeight = IntInf.fromInt 0

    val basePair = (Rewrites.wildcard, baseWeight)

    val isEmptyRWState : rwstate -> bool = AtomMap.isEmpty

    val { clrFn = clearPPRWState,
          getFn = getPPRWState,
          setFn = setPPRWState,
          peekFn = peekPPRWState } = ProgPt.newProp (fn _ => emptyRWState)

    val { clrFn = clearVarRWState,
          getFn = getVarRWState,
          setFn = setVarRWState,
          peekFn = peekVarRWState } = B.Var.newProp (fn _ => emptyRWState)

    (* getRWStateWeight() - Return the weight associated with the
       given production name in the given rewrite state.  Will throw
       an exception if the production name is not in the rewrite
       state. *)
    fun getRWStateWeight (nt, rwState) =
        if Atom.same(Rewrites.wildcard, nt)
        then baseWeight
        else AtomMap.lookup(rwState, nt)

    (* getRWStateKeys() - Get a list of atoms that have a weight
       associated with them in the given state (injects the wildcard
       nonterminal). *)
    fun getRWStateKeys state = Rewrites.wildcard :: (AtomMap.listKeys state)

    (* getRWStateMaxPair() - Get the nonterminal weight pair that is maximal
       for the given state, and is also in the given nonterminal map. *)
    fun getRWStateMaxPair (state, ntMap) = let
        fun cmpPair (p1 as (nt1, wt1), p2opt) =
            if AtomMap.inDomain(ntMap, nt1)
            then (case p2opt
                   of SOME (_, wt2) => 
                      if IntInf.>(wt1, wt2) then SOME p1 else p2opt
                    | NONE => SOME p1
                   (* end case *))
            else p2opt
    in
        foldl cmpPair NONE (AtomMap.listItemsi state)
    end (* getRWStateMaxPair *)

    (* crossRWStates() - Utility for creating list of atom lists.
       Each of these are strings of nonterminals in the grammar that
       are used to match productions. *)
    fun crossRWStates (prefixAtom, states) = let
        fun crossRWStates' (nt1 :: nt1s, ntss) =
            let val ntStrings = map (fn nts => nt1 :: nts) ntss
            in
                ntStrings @ crossRWStates'(nt1s, ntss)
            end
          | crossRWStates' ([], ntss) = []
        val stateKeys = List.map getRWStateKeys states
    in
        List.map (fn x => prefixAtom :: x) (List.foldr crossRWStates' [[]]
                                                       stateKeys)
    end (* crossRWStates() *)

    (* mkApplyProdToRWState() - Curried form of a function that
       mutates a RW state, mapping from the given production to a
       nonterminal and weight pair.  If the weight of the given
       production application is greater than what is already set in the
       RW state, update the state. *)
    fun mkApplyProdToRWState rhsRWStates = let
        fun applyProdToRWState (prod as Rewrites.HLRWProduction{name, rhs,
                                                                ...},
                                rwState) = let
            val prodWeight = Rewrites.getProductionWeight prod
            val childWeights = ListPair.map getRWStateWeight
                                            (tl rhs, rhsRWStates)
            val totalWeight = List.foldl IntInf.+ prodWeight childWeights
            val crntWeight = (case AtomMap.find (rwState, name)
                               of NONE => baseWeight
                                | SOME wt => wt
                               (* end case *))
        in
            if IntInf.>=(totalWeight, crntWeight)
            then (AtomMap.insert(rwState, name, totalWeight))
            else rwState
        end (* applyProdToRWState() *)
    in
        applyProdToRWState
    end (* mkApplyProdToRWState() *)

    (* rwStateToString() - Convert the given rewrite state into a string. *)
    fun rwStateToString rwState = let
        val keys = getRWStateKeys rwState
        fun pairToStr key =
            String.concat [Atom.toString key, " : ",
                           IntInf.toString (getRWStateWeight(key, rwState))]
        val keysAndWeights =
            String.concatWith ", " (List.map pairToStr keys)
    in
        String.concat ["{ ", keysAndWeights, " }"]
    end (* rwStateToString() *)

    (* FIXME: Will need to move these into an environment... *)
    val allocStr = "alloc"
    val allocAtom = Atom.atom allocStr

    (* termToArgVars() - Given a BOM term, return all the variable arguments
       to that term.  XXX - What about exceptions? *)
    fun termToArgVars term = (case term
        of B.E_Apply (_, args, exns) => args
         | B.E_Throw (_, args) => args
         | B.E_Ret args => args
         | B.E_HLOp (_, args, exns) => args
         | _ => []
        (* end case *))

    (* rhsToArgVars() - Give a BOM statement RHS, return all the
       variable arguments to that item. *)
    fun rhsToArgVars rhs = (case rhs
        of B.E_Cast (_, v) => [v]
         | B.E_Select (_, v) => [v]
         | B.E_Update (_, v1, v2) => [v1, v2]
         | B.E_AddrOf (_, v) => [v]
         | B.E_Alloc (_, args) => args
         | B.E_GAlloc (_, args) => args
         | B.E_Promote v => [v]
         | B.E_DCon (_, args) => args
         | B.E_CCall (_, args) => args
         | B.E_VPLoad (_, v) => [v]
         | B.E_VPStore (_, v1, v2) => [v1, v2]
         | _ => []
        (* end case *))

    (* kindToArgVars() - Given a variable kind, return a list of the
       variable arguments passed to the binding of the input variable. *)
    fun kindToArgVars (B.VK_Let (B.E_Pt(_, t))) = termToArgVars t
      | kindToArgVars (B.VK_RHS r) = rhsToArgVars r
      | kindToArgVars _ = []

    (* matchRWPatListToVars() - Given a list of rewrite patterns, a
       list of BOM variable and a rewrite environment, check that the
       pattern and variable lists are of comparable size and then
       recursively call matchRWPatToVar(). *)
    fun matchRWPatListToVars (pats, var_list, env) =
        if ((length pats) <> (length var_list)) then
            raise Fail("Argument count mismatch.")
        else
            (ListPair.foldl matchRWPatToVar env (pats, var_list))
    (* matchRWPatToVar() - Given a BOM variable and a rewrite pattern,
       extend the given rewriting environment, matching metavariables to
       BOM variables.  Note: This chases variable bindings using the
       variable kind. *)
    and matchRWPatToVar (pat, v, env) = (case pat
        of RW.PT.Var a => AtomMap.insert(env, a, v)
         | RW.PT.Call (_, pats) =>
           matchRWPatListToVars(pats, kindToArgVars (B.Var.kindOf v), env)
         | _ => env
        (* end case *))

    (* matchRWPatToTerm() - Given a rewrite pattern and a BOM term,
       return an environment binding pattern meta-variables to BOM
       variables. *)
    fun matchRWPatToTerm (rw_pat, t) = (case rw_pat
        of RW.PT.Call(_, pats) => matchRWPatListToVars(pats, termToArgVars t,
                                                       AtomMap.empty)
         | _ => AtomMap.empty
        (* end case *))

    (* rwEnvToString() - Utility function for displaying a rewrite
       environment (mapping rewrite metavariables to BOM variables). *)
    fun rwEnvToString rw_env = let
        fun vPairToString (a, v) =
            String.concat [Atom.toString a, " : ", B.Var.toString v]
        val kvStrs = List.map vPairToString (AtomMap.listItemsi rw_env)
        val kvPairs = String.concatWith ", " kvStrs
    in
        String.concat ["{", kvPairs, "}"]
    end (* rwEnvToString() *)

    (* cvtLitTy() - Convert a literal parse tree type to its corresponding
       BOM type.

       XXX - This is essentially duplicate code from expand.sml.  Maybe this
       should/could be moved into the BOMTyPT structure? *)
    fun cvtLitTy (ty) = (case ty
        of PT.T_Any => BTy.T_Any
         | (PT.T_Enum w) => BTy.T_Enum w
         | (PT.T_Raw rty) => BTy.T_Raw rty
         | (PT.T_Tuple(mut, tys)) => BTy.T_Tuple(mut, cvtLitTys(tys))
         | (PT.T_Addr ty) => BTy.T_Addr(cvtLitTy(ty))
         | (PT.T_Fun(argTys, exhTys, resTys)) =>
           BTy.T_Fun(cvtLitTys(argTys), cvtLitTys(exhTys),
                     cvtLitTys(resTys))
         | (PT.T_Cont tys) => BTy.T_Cont(cvtLitTys(tys))
         | (PT.T_CFun cproto) => BTy.T_CFun cproto
         | (PT.T_VProc) => BTy.T_VProc
         | (PT.T_TyCon tyc) =>
           raise (Fail "Tycons not currently supported in rewrite language.")
        (* end case *))
    (* cvtLitTys() - Covert a list of literal parse tree types to a list of
       corresponding BOM types. *)
    and cvtLitTys (tys) = List.map cvtLitTy tys

    (* cvtPat() - Convert a rewrite pattern into a BOM binding
       expression, passing the bound temporary to a BOM expression
       continuation, k.  *)
    fun cvtPat (rw_pat, rw_env, exns, k : BOM.var -> BOM.exp) = (case rw_pat
        of RW.PT.Var (var_name) => k(AtomMap.lookup(rw_env, var_name))
           (* FIXME: Again, per the allocAtom definition, I would like to have
              some kind of environment for looking up and differentiating
              between constructors and HLOps. *)
         | RW.PT.Call (ctor, pats) => let
               val ctor_str = Atom.toString ctor
           in
               if String.sub(ctor_str, 0) = #"@"
               then cvtHLOp(Atom.atom(String.extract(ctor_str, 1, NONE)),
                            pats, rw_env, exns, k)
               else cvtCtor(ctor_str, pats, rw_env, exns, k)
           end
         | RW.PT.Const (lit, lit_ty) => let
               val ty = cvtLitTy lit_ty
               val tmp = B.Var.new("_t", ty)
           in
               BOM.mkStmt([tmp], BOM.E_Const(lit, ty), k tmp)
           end
        (* end case *))
    (* cvtPats() - Convert a list of patterns into BOM binding syntax
       and a list of BOM variables.  The resulting list is passed to the
       continuation, which creates the rest of a BOM expression.
       Note: This was stolen almost verbatim from expand.sml. *)
    and cvtPats (rw_pats, rw_env, exns, k : BOM.var list -> BOM.exp) = let
        fun cvtPats' ([], tmps) = k(List.rev tmps)
          | cvtPats' (p::ps, tmps) = cvtPat(p, rw_env, exns,
                                            fn t => cvtPats'(ps, t::tmps))
    in
        cvtPats'(rw_pats, [])
    end (* cvtPats() *)
    (* cvtCtor() - Convert a data constructor pattern into BOM binding
       syntax.
       FIXME: Typical comments about a constructor/primitive environment.
       FIXME: Is there a case where alloc could yield a mutable tuple?
       Currently assuming not. *)
    and cvtCtor (ctor_str, pats, rw_env, exns, k) =
        if ctor_str = allocStr
        then cvtPats(pats, rw_env, exns,
                     fn xs => let
                            val alloc_ty =
                                BTy.T_Tuple(false, map BOM.Var.typeOf xs)
                            val tmp = B.Var.new("_t", alloc_ty)
                        in
                            BOM.mkStmt([tmp], BOM.E_Alloc(alloc_ty, xs),
                                       k tmp)
                        end)
        else raise (Fail ("Unknown ctor in rewrite: " ^ ctor_str))
    (* cvtHLOp() - Covert a HLOp application pattern into BOM syntax.
       FIXME: Exn handlers seem hacked here.  How do I even know the
       actual HLOp has exn handling arguments that are similar to the
       HLOp being replaced? *)
    and cvtHLOp (hlopAtom, pats, rw_env, exns, k) = (case HLOpEnv.find hlopAtom
        of SOME hlop =>
           cvtPats(pats, rw_env, exns,
                   fn xs => let
                          val e = BOM.mkHLOp(hlop, xs, exns)
                          val (tty :: tys) = BU.typeOfExp(e)
                          val tmp = B.Var.new("_t", tty)
                      in
                          if not (List.null tys) then
                              raise (Fail ("Expected lone type for HLOp exp: "
                                           ^ (Atom.toString hlopAtom)))
                          else 
                              BOM.mkLet([tmp], e, k tmp)
                      end)
         | NONE => raise (Fail ("Unkown HLOp in rewrite: " ^
                                (Atom.toString hlopAtom)))
        (* end case *))

    (* mkExpFromRWPat() - Given a pattern and a meta-variable
       environment that references BOM variables, construct a new BOM
       term for the given pattern. *)
    fun mkExpFromRWPat (rw_pat, rw_env, t as B.E_HLOp(hlOp, args, exns)) =
        cvtPat(rw_pat, rw_env, exns, fn v => BOM.mkRet [v])
      | mkExpFromRWPat (_, _, _) =
        raise (Fail "Currently expect to be replacing a HLOP.")

    (* ____________________________________________________________ *)
    (* rewrite() - Rewrite the given BOM module, using HLOp rewrites in the
       library path. *)
    fun rewrite (module as B.MODULE{name, externs, body}) = let
        (* __________________________________________________ *)
        (* XXX Not sure rewrites need to worry about this stuff
        inherrited from the HLOp expander (unless we add C function
        calls to the rewrites). *)
        val importEnv = let
            val importEnv = ATbl.mkTable (32, Fail "importEnv")
            fun ins (cf as CFunctions.CFun{name, ...}) =
                ATbl.insert importEnv (Atom.atom name, cf)
        in
            List.app ins externs;
            importEnv
        end

        val nExterns = ATbl.numItems importEnv

        fun getExterns () =
            if (nExterns = ATbl.numItems importEnv)
            then externs
            else ATbl.listItems importEnv
        (* __________________________________________________ *)
        val hlopEnv = findHLOps module

        val hlrwFiles =
            List.concat (List.map (fn (_, hlop) => HLRWDefLoader.load hlop)
                                  (ATbl.listItemsi hlopEnv))

        val hlrws = List.concat hlrwFiles

        val hlrwGrammar =
            foldl Rewrites.addRWToGrammar (Rewrites.newGrammar ()) hlrws

        val hlrwGrammarHash = Rewrites.getGrammarHash hlrwGrammar

        val rwMap =
            AtomMap.filter Rewrites.productionHasRW
                           (Rewrites.getGrammarProductionMap hlrwGrammar)

        (* XXX - Couldn't find easy way to get something like "oporelse" *)
        val myor = fn (a, b) => a orelse b

        (* __________________________________________________ *)
        (* mkRWState() - Given a hash key (which should be an atom)
           into the hlrwGrammarHash, and a list of rewrite states, create
           a rewrite state based on all permutations of state
           non-terminals. *)
        fun mkRWState (rwRHSKey, rwRHSRest) = (case AtomMap.find(
                                                     hlrwGrammarHash, rwRHSKey)
            of NONE => emptyRWState
             | SOME [] => emptyRWState (* Should not happen. *)
             | SOME candidateProds => let
                   val ntStrings = crossRWStates(rwRHSKey, rwRHSRest)
                   (* +DEBUG
                   val _ =
                       print ((String.concatWith "\n"
                               (List.map (fn alist => String.concatWith " "
                                          (List.map Atom.toString alist))
                                         ntStrings)) ^ "\n")
                      -DEBUG *)
                   fun prodMatchesNtString prod = let
                       fun prodMatchesNtString' (ntString, acc) =
                           acc orelse Rewrites.matchRHS(prod, ntString)
                   in
                       List.foldl prodMatchesNtString' false ntStrings
                   end (* prodMatchesNtString() *)
                   val matchingProds = List.filter prodMatchesNtString
                                                   candidateProds
                   val applyProdToRWState = mkApplyProdToRWState rwRHSRest
                   val rwState = foldl applyProdToRWState emptyRWState
                                       matchingProds
               in
                   (* DEBUG: print ((rwStateToString rwState) ^ "\n"); *)
                   rwState
               end
            (* end case *))
        (* __________________________________________________ *)
        (* matchExp() - Derive a rewrite state for the current
           expression.  Result is both bound to the program point (for
           rewriting in rewriteExp()), and passed up to possibly be
           associated to a variable by matchBindingExp(). *)
        fun matchExp (B.E_Pt(ppt, t)) = (case t
            of B.E_HLOp(hlOp, vars, _) => let
                   val hlOpName = Atom.atom ("@" ^ (Atom.toString
                                                        (H.name hlOp)))
                   val rwState = mkRWState(hlOpName, List.map getVarRWState
                                                              vars)
               in
                   setPPRWState(ppt, rwState); rwState
               end
             | _ => emptyRWState
            (* end case *))
        (* __________________________________________________ *)
        (* matchRHS() - Derive a rewrite state for the current
           statement RHS.  Unlike matchExp, there is no program point to
           associate with the result, so for this to work,
           matchBindingExp() MUST have a binding variable. *)
        fun matchRHS (B.E_Alloc(_, vars)) =
            mkRWState(allocAtom, List.map getVarRWState vars)
          | matchRHS _ = emptyRWState
        (* __________________________________________________ *)
        (* matchBindingExp() - Derive a rewrite state for the
           expression being bound, associating the state with the binding
           variable.

           FIXME: This is only set up for the binding occurances of single
           variables.  Need to talk to someone about any possible cases where
           the variable list does not contain only one variable. *)
        fun matchBindingExp (B.E_Let([v], e, _)) =
            setVarRWState(v, matchExp e)
          | matchBindingExp (B.E_Stmt([v], r, _)) =
            setVarRWState(v, matchRHS r)
          | matchBindingExp _ = ()
        (* __________________________________________________ *)
        (* rewriteExp() - First, label the given expression using
           matchExp().  Next, recursively traverse the children
           expressions. Then, select and apply the maximum benefit
           rewrite, if any.  Finally, return a change flag and the
           (possibly changed) expression.

           JDR: Could have used an option, but I don't like having to "unbox"
           SOME values so frequently. *)
        fun rewriteExp (e as B.E_Pt(ppt, t)) = (matchBindingExp t; case t
            of B.E_Let(lhs, e1, e2) => let
                   val (changed1, e1') = rewriteExp e1
                   val (changed2, e2') = rewriteExp e2
                   val changed = changed1 orelse changed2
               in
                   (changed, if changed then B.mkLet(lhs, e1', e2') else e)
               end
             | B.E_Stmt (lhs, rhs, e1) => let
                   val (changed, e1') = rewriteExp e1
               in
                   (changed, if changed then B.mkStmt(lhs, rhs, e1') else e)
               end
             | B.E_Fun(fbs, e1) => let
                   val (changes, fbs') = ListPair.unzip (List.map rewriteLambda
                                                                  fbs)
                   val (changed1, e1') = rewriteExp e1
                   val changed = foldl myor false (changed1 :: changes)
               in
                   (changed, if changed then B.mkFun(fbs', e1') else e)
               end
             | B.E_Cont(fb, e1) => let
                   val (changedfb, fb') = rewriteLambda fb
                   val (changed1, e1') = rewriteExp e1
                   val changed = changedfb orelse changed1
               in
                   (changed, if changed then B.mkCont(fb', e1') else e)
               end
             | B.E_If(x, e1, e2) => let
                   val (changed1, e1') = rewriteExp e1
                   val (changed2, e2') = rewriteExp e2
                   val changed = changed1 orelse changed2
               in
                   (changed, if changed then B.mkIf(x, e1', e2') else e)
               end
             | B.E_Case(x, cases, dflt) => let
                   fun rewriteCase (crntCase as (p, e)) = let
                       val (changed, e') = rewriteExp e
                   in
                       (changed, (p, if changed then e' else e))
                   end (* rewriteCase *)
                   val (changes, cases') = ListPair.unzip (List.map rewriteCase
                                                                    cases)
                   val (changedd, dflt') = (case dflt
                       of SOME e => let
                              val (c, e') = rewriteExp e
                          in
                              (c, if c then SOME e' else dflt)
                          end
                        | NONE => (false, dflt)
                       (* end case *))
                   val changed = foldl myor false (changedd :: changes)
               in
                   (changed, if changed then B.mkCase(x, cases', dflt') else e)
               end
             | (t as B.E_HLOp(hlOp, args, exns)) => let
                   val ppRWState = getPPRWState ppt
                   val ntWtPairOpt = getRWStateMaxPair(ppRWState, rwMap)
               in case ntWtPairOpt
                   of SOME (nt, _) => let
                          val prod = AtomMap.lookup(rwMap, nt)
                          val (Rewrites.HLRWProduction {rw_opt, ...}) = prod
                          val (rw as RW.PT.Rewrite {label = rw_label,
                                                    lhs = rw_lhs,
                                                    rhs = rw_rhs, ...}) =
                              Option.valOf rw_opt
                          val rw_env = matchRWPatToTerm(rw_lhs, t)
                          val new_exp = mkExpFromRWPat(rw_rhs, rw_env, t)
                      in
                          (* +DEBUG
                          print ("Apply RW: " ^ (Atom.toString rw_label) ^
                                 "\n");
                          print ("rw_env = " ^ (rwEnvToString rw_env) ^ "\n");
                          print "new_exp = \n";
                          PrintBOM.printExp new_exp;
                             -DEBUG *)
                          (false, e)
                      end
                    | _ => (false, e)
               end
             | _ => (false, e)
            (* end case *))
        (* rewriteLambda() - Try to rewrite the body of the given BOM
           lambda.  If any changes were made, reconstruct the BOM lambda,
           and pass up the change flag. *)
        and rewriteLambda (l as B.FB {f, params, exh, body}) = let
            val (changed, body') = rewriteExp body
        in
            (changed, if changed then B.FB {f=f, params=params, exh=exh,
                                            body=body'} 
                      else l)
        end (* rewriteLambda() *)

        val (changed, body') = rewriteLambda body

    in
        (* DEBUG: print (Rewrites.grammarToString hlrwGrammar); *)
	if changed
	then SOME(B.mkModule(name, getExterns(), body'))
	else NONE
    end

end (* RewriteHLOps struct *)
