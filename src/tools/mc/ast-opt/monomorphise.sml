(* monomorphise.sml
 *
 * COPYRIGHT (c) The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * This module translates polymorphic datatype constructors and function
 * definitions into monomorpic ones by copying them for each type
 * application. This module currently leaves inline BOM code alone, as
 * there are no type variables in that code (just ANY types), requiring
 * either significant changes or an additional analysis pass.
 *
 * FIXME:
 * Currenly in progress building the analysis and printout
 * Only analysing raw polymorphic types right now. Need to also look at datatype
 * constructors
 * The expansion code is basically placeholder-level
 *)

(*
Need to build up some maps:
1) funID -> funID list
If funID (in) is instantiated at some type, then the list of funIDs are also instantiated at that type
2) funID -> (funID * type list) list
For each function, the list of monomorphic instantiations of that function at different types
3) <datatypes>
For each datatype with a polymorphic constructor, record the instantiations along with the newly-named constructors/types

Monomorphising just the AST code. Inline BOM does not have type variables, so it would be hard to do anything to it (yet).

Analysis
- If the function is polymorphic, scan the body for all uses of the type variable to instantiate other functions and add to map 1
- Scan all expressions for monomorphic uses of data type constructors and add to map 3
- If a polymorphic function is instantiated, use map 2 and map 1 to record needed instantiations of other functions at appropriate types

Translation
- Create the monomorphic versions mentioned in the above maps alongside their polymorphic original versions. Transform uses of functions within bodies appropriately per map 1 and map 2.
- Leave around the polymorphic original functions in case they are used in inline BOM code
- For each instantiation, make sure that it now uses the monomorphic one directly instead instantiating the polymorphic one

 *)

structure Monomorphise : sig

    val monomorphise : AST.exp -> AST.exp

  end = struct

    structure A = AST
    structure AV = Var
    structure AMap = AV.Map
    structure ASet = AV.Set
    structure ST = Stats

  (***** controls ******)
    val enableMonomorphisation = ref false
    val monomorphisationDebug = ref true

    val () = List.app (fn ctl => ControlRegistry.register ASTOptControls.registry {
              ctl = Controls.stringControl ControlUtil.Cvt.bool ctl,
              envName = NONE
            }) [
              Controls.control {
                  ctl = enableMonomorphisation,
                  name = "monomorpisation",
                  pri = [0, 1],
                  obscurity = 0,
                  help = "enable monomorphisation"
                },
              Controls.control {
                  ctl = monomorphisationDebug,
                  name = "monomorphisation-debug",
                  pri = [0, 1],
                  obscurity = 0,
                  help = "debug monomorphisation"
                  }
              ]

    (***** Statistics *****)
    val cntNewDatatypes		= ST.newCounter "ast-monomorphisation:new-datatype"
    val cntNewFunctions		= ST.newCounter "ast-monomorphisation:new-function"

    fun findInstantiation (funMap, var, types) =
        case AMap.find (funMap, var)
         of NONE => NONE
          | SOME l => let
                val len = length types
                val result = List.find (fn (_, tys) =>
                                           (length tys = len andalso
                                            ListPair.allEq (fn (a, b) => TypeUtil.same (a,b)) (types, tys))) l
            in
                case result
                 of NONE => NONE
                  | SOME z => SOME z
            end

    fun addInstantiation (funMap, var, types) = let
        val Types.TyScheme (vs, ty) = AV.typeOf var
        val newType = TypeUtil.apply (AV.typeOf var, types)
        val newVar = Var.new (AV.nameOf var, newType)
        val _ = print (concat["ADDING: ", Var.toString var, " -> ",
                              Var.toString newVar, " : ",
                              String.concatWith "," (List.map TypeUtil.toString types),
                              "\n"])
    in
        case AMap.find (funMap, var)
         of NONE => AMap.insert (funMap, var, [(newVar, types)])
          | SOME l => AMap.insert (funMap, var, (newVar, types)::l)
    end

    fun printInstantiations funMap = let
        fun vToString (v, tys) = concat[Var.toString v, ":",
                                        String.concatWith "," (List.map TypeUtil.toString tys)]
        fun p (v, l) = print (concat[Var.toString v, " -> ",
                                     String.concatWith " ; " (List.map vToString l), "\n"])
    in
        AMap.appi p funMap
    end

    fun isUniversal ty = false (*
        case ty 
         of (Types.MetaTy (Types.MVar {info,...})) => (
            case !info
             of Types.UNIV _ => true
              | Types.INSTANCE ty => isUniversal ty
              | _ => false)
          | _ => false*)

    fun analyse body = let
      fun exp (A.LetExp (b, e), funMap, dconMap) = let
          val (funMap, dconMap) = binding (b, funMap, dconMap)
      in
          exp (e, funMap, dconMap)
      end
	| exp (A.IfExp (e1, e2, e3, t), funMap, dconMap) = let
              val (funMap, dconMap) = exp (e1, funMap, dconMap)
              val (funMap, dconMap) = exp (e2, funMap, dconMap)
          in
              exp (e3, funMap, dconMap)
          end
	| exp (A.CaseExp (e, ms, t), funMap, dconMap) = let
              val (funMap, dconMap) = List.foldr (fn (m, (funMap, dconMap)) =>
                                                     match (m, funMap, dconMap))
                                                 (funMap, dconMap)
                                                 ms
          in
              exp (e, funMap, dconMap)
          end
	| exp (A.HandleExp (e, ms, t), funMap, dconMap) = let
              val (funMap, dconMap) = List.foldr (fn (m, (funMap, dconMap)) =>
                                                     match (m, funMap, dconMap))
                                                 (funMap, dconMap) 
                                                 ms
          in
              exp (e, funMap, dconMap)
          end
	| exp (A.RaiseExp (e, t), funMap, dconMap) = exp (e, funMap, dconMap)
	| exp (A.FunExp (x, e, t), funMap, dconMap) = exp (e, funMap, dconMap)
	| exp (A.ApplyExp (e1, e2, t), funMap, dconMap) = let
              val (funMap, dconMap) = exp (e1, funMap, dconMap)
          in
              exp (e2, funMap, dconMap)
          end
	| exp (m as A.VarArityOpExp _, funMap, dconMap) = (funMap, dconMap)
	| exp (A.TupleExp es, funMap, dconMap) = 
          List.foldr (fn (e, (funMap, dconMap)) => exp (e, funMap, dconMap)) (funMap, dconMap) es
	| exp (A.SpawnExp e, funMap, dconMap) = exp (e, funMap, dconMap)
	| exp (k as A.ConstExp c, funMap, dconMap) = const (c, funMap, dconMap)
	| exp (x as A.VarExp (v, ts), funMap, dconMap) = (
          if ((length ts = 0) orelse (List.exists isUniversal ts))
          then (funMap, dconMap)
          else (
(*          case AV.kindOf v
           of AST.VK_Fun => ( *)
              case (findInstantiation (funMap, v, ts))
               of NONE => (addInstantiation (funMap, v, ts), dconMap)
                | SOME _ => (funMap, dconMap)))
(*            | _ => (funMap, dconMap)) *)
	| exp (A.SeqExp (e1, e2), funMap, dconMap) = let
              val (funMap, dconMap) = exp (e1, funMap, dconMap)
          in
              exp (e2, funMap, dconMap)
          end
	| exp (A.OverloadExp ovr, funMap, dconMap) = (funMap, dconMap)
	| exp (A.ExpansionOptsExp(opts, e), funMap, dconMap) = exp (e, funMap, dconMap)
	| exp (A.PTupleExp es, funMap, dconMap) = 
          List.foldr (fn (e, (funMap, dconMap)) => exp (e, funMap, dconMap)) (funMap, dconMap) es
	| exp (A.PCaseExp (es, ms, ty), funMap, dconMap) =
          raise Fail "PCaseExp should be removed by UnPar or Elaborate."
	| exp (A.RangeExp (e1, e2, oe3, t), funMap, dconMap) =
          raise Fail "RangeExp should be removed by UnPar or Elaborate."
	| exp (A.PArrayExp (es, t), funMap, dconMap) = 
          raise Fail "PArrayExp should be removed by UnPar or Elaborate."
	| exp (A.PCompExp (e, pes, oe), funMap, dconMap) = 
          raise Fail "PCompExp should be removed by UnPar or Elaborate."
	| exp (A.PChoiceExp (es, t), funMap, dconMap) =
          raise Fail "PChoiceExp should be removed by UnPar or Elaborate."
      and binding (A.ValBind (p, e), funMap, dconMap) = let
          val (funMap, dconMap) = exp (e, funMap, dconMap)
      in
          pat (p, funMap, dconMap)
      end
	| binding (A.FunBind lams, funMap, dconMap) =
          List.foldr (fn (l, (funMap, dconMap)) => lambda (l, funMap, dconMap)) (funMap, dconMap) lams
	| binding (A.PrimVBind (v, code), funMap, dconMap) = (funMap, dconMap)
	| binding (A.PrimCodeBind code, funMap, dconMap) = (funMap, dconMap)
	| binding (A.PValBind (p, e), funMap, dconMap) = 
          raise Fail "PValBind should be removed by UnPar or Elaborate."
      and lambda (A.FB (f, x, e), funMap, dconMap) = exp (e, funMap, dconMap)
      and match (A.PatMatch (p, e), funMap, dconMap) = let
          val (funMap, dconMap) = exp (e, funMap, dconMap)
      in
          pat (p, funMap, dconMap)
      end
	| match (A.CondMatch (p, e1, e2), funMap, dconMap) = let
              val (funMap, dconMap) = exp (e1, funMap, dconMap)
              val (funMap, dconMap) = exp (e2, funMap, dconMap)
          in
              pat (p, funMap, dconMap)
          end
      and pat (A.ConPat (dc, tys, p), funMap, dconMap) = (funMap, dconMap) (* FIXME *)
        | pat (A.TuplePat (ps), funMap, dconMap) =
          List.foldr (fn (p, (funMap, dconMap)) => pat (p, funMap, dconMap)) (funMap, dconMap) ps
        | pat (A.VarPat v, funMap, dconMap) = (funMap, dconMap)
        | pat (A.WildPat ty, funMap, dconMap) = (funMap, dconMap)
        | pat (A.ConstPat c, funMap, dconMap) = const (c, funMap, dconMap)
      and const (A.DConst (dc, tys), funMap, dconMap) = (funMap, dconMap) (*FIXME*)
        | const (A.LConst (l, ty), funMap, dconMap) = (funMap, dconMap)
    in
        exp (body, AMap.empty, AMap.empty)
    end

    (*
     * funMap is an AV.map from var -> (var * type list) list
     * dconMap is FIXME
     * appliedTypes is a (meta * ty) list
     *
     * Only three cases are interesting when performing monomorph:
     * 1) FIX: binding A polymorphic variable (VarExp) will turn into one of:
     *  a) If its types are all concrete and it is a function, the monomorphised version of it.
     *  b) If its types are all concrete and it is non-function, just the monomorphised version.
     *  c) If its types are not concrete, but the meta variables are bound in the
     * monomorphising context (from the appliedTypes list), then close it and do a) or b).
     * 2) A polymorphic function binding (FB) will be split into all of the different
     * monomorphic instantiations listed in the funMap. At each entry in the funmap list,
     * the instantiations of the metavariable are added to the appliedTypes list and
     * it calls monomorphiseExp recursively to handle the body (instead of exp)
     * 3) FIXME: dcons (ConstExp and ConPat)
     *
     *)
    fun monomorphiseExp (funMap, dconMap, body, appliedTypes) = let
      fun substBound ty =
          case List.find (fn (m, _) => TypeUtil.same (m, ty)) appliedTypes
           of SOME (_, ty') => ty'
            | NONE => ty
      fun exp (A.LetExp (b, e)) = A.LetExp (binding b, exp e)
	| exp (A.IfExp (e1, e2, e3, t)) = A.IfExp (exp e1, exp e2, exp e3, substBound t)
	| exp (A.CaseExp (e, ms, t)) = A.CaseExp (exp e, map match ms, substBound t)
	| exp (A.HandleExp (e, ms, t)) = A.HandleExp (exp e, map match ms, substBound t)
	| exp (A.RaiseExp (e, t)) = A.RaiseExp (exp e, substBound t)
	| exp (A.FunExp (param, e, t)) = A.FunExp (param, exp e, substBound t)
	| exp (A.ApplyExp (e1, e2, t)) = A.ApplyExp (exp e1, exp e2, substBound t)
	| exp (m as A.VarArityOpExp _) = m
	| exp (A.TupleExp es) = A.TupleExp (map exp es)
	| exp (A.SpawnExp e) = A.SpawnExp (exp e)
	| exp (k as A.ConstExp c) = k
	| exp (x as A.VarExp (v, ts)) = A.VarExp (v, List.map substBound ts)
	| exp (A.SeqExp (e1, e2)) = A.SeqExp (exp e1, exp e2)
	| exp (A.OverloadExp ovr) = A.OverloadExp ovr
	| exp (A.ExpansionOptsExp(opts, e)) = A.ExpansionOptsExp(opts, exp e)
	| exp (A.PTupleExp es) = A.PTupleExp (map exp es)
	| exp (A.PCaseExp (es, ms, ty)) =
          raise Fail "PCaseExp should be removed by UnPar or Elaborate."
	| exp (A.RangeExp (e1, e2, oe3, t)) =
          raise Fail "RangeExp should be removed by UnPar or Elaborate."
	| exp (A.PArrayExp (es, t)) = 
          raise Fail "PArrayExp should be removed by UnPar or Elaborate."
	| exp (A.PCompExp (e, pes, oe)) = 
          raise Fail "PCompExp should be removed by UnPar or Elaborate."
	| exp (A.PChoiceExp (es, t)) =
          raise Fail "PChoiceExp should be removed by UnPar or Elaborate."
      and binding (A.ValBind (p, e)) = let
          val e' = exp e
          val expTy = TypeOf.exp e'
      in
          A.ValBind (pat (p, expTy), e')
      end
	| binding (A.FunBind lams) = A.FunBind (map lambda lams) (* FIXME: replicate at each type *)
	| binding (A.PrimVBind (v, code)) = A.PrimVBind (v, code)
	| binding (A.PrimCodeBind code) = A.PrimCodeBind code
	| binding (A.PValBind (p, e)) = 
          raise Fail "PValBind should be removed by UnPar or Elaborate."
      and pat (A.ConPat (dc, tys, p), ety) = A.ConPat(dcon dc, tys, pat (p, ety)) (* FIXME *)
        | pat (A.TuplePat (ps), ety) = A.TuplePat (List.map (fn (p) => pat (p, ety)) ps)
        | pat (A.VarPat v, ety) = A.VarPat v  (* FIXME *)
        | pat (A.WildPat ty, ety) = A.WildPat ty
        | pat (A.ConstPat c, ety) = A.ConstPat (const c)
      and const (A.DConst (dc, tys)) = A.DConst(dcon dc, tys) (* FIXME *)
        | const (A.LConst (l, ty)) = A.LConst (l, ty)
      and dcon (Types.DCon {id, name, owner, argTy}) = (* FIXME *)
          Types.DCon {id=id, name=name, owner=owner, argTy=argTy}
      and lambda (A.FB (f, x, e)) = A.FB (f, x, exp e)
      and match (A.PatMatch (p, e)) = A.PatMatch (p, exp e)
	| match (A.CondMatch (p, e1, e2)) = A.CondMatch (p, exp e1, exp e2)
    in
	exp body
    end

    fun monomorphise body =
        if !enableMonomorphisation
        then
            let
                val (funMap, dconMap) = analyse body
                val _ = printInstantiations funMap
            in
                monomorphiseExp (AMap.empty, dconMap, body, [])
            end
        else body
             
  end
