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
 * Processing needs to be done next. 
 * The expansion code is basically placeholder-level
 *
 * There are three maps built during analysis:
 * 1) funID -> (funID * type list) list
 * For each function, the list of monomorphic instantiations of that function at different types
 * 2) metaVar -> funID list
 * Map from meta variables to the functions to which they are applied in typeapp forms
 * 3) dconStamp -> type list list
 * For each type constructor with a polymorphic constructor that is instantiated,
 * record the instantiations
 *
 * Analysis (DONE)
 * - If the function or variable has a type applied to it, add to maps 1 and 2, as appropriate
 * - Scan all expressions for monomorphic uses of data type constructors and add to map 3
 *
 * Processing needs to determine:
 * (fun*types) -> new name
 * fun -> (new name * types) list
 * - For a polymorphic function, at what types should it be copied?
 * - For an instantiation of a poly function at mono types, what is the new name?
 *
 * ?? Do I need to duplicate the entire datatype definition with each of the
 * type constructors at each monomorphic type application?
 * ?? OR, can I just add tons of monomorphic constructors for the same
 * datatype? TODO: look at the MLton documentation again.
 *
 * - For a poly datatype, at what types should it be copied? Do I have
 * to walk all constructors?
 * - For a type appliction of a poly dcon, what is the new name?
 * 
 * Processing (NEXT)
 * - Worklist of all functions and monotypes
 * - For each, look up the corresponding metavariable and instantiate other
 * functions at that monotype
 * - Recursively add additional functions x monotypes that are instantiated
 * by those other functions
 *
 * Translation
 * - Create the monomorphic functions/datatypes mentioned in the above maps
 * alongside their polymorphic original versions.
 * - Leave around the polymorphic original functions and datatypes in case they are
 * used in inline BOM code
 * - For each instantiation, make sure that it now uses the monomorphic
 * one directly instead instantiating the polymorphic one
 *
 *)

structure Monomorphise : sig

    val monomorphise : AST.exp -> AST.exp

  end = struct

    structure A = AST
    structure AV = Var
    structure AMap = AV.Map
    structure SMap = Stamp.Map
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
                  name = "monomorphise",
                  pri = [0, 1],
                  obscurity = 0,
                  help = "enable monomorphisation"
                },
              Controls.control {
                  ctl = monomorphisationDebug,
                  name = "monomorphise-debug",
                  pri = [0, 1],
                  obscurity = 0,
                  help = "debug monomorphisation"
                  }
              ]

    (***** Statistics *****)
    val cntNewDatatypes		= ST.newCounter "ast-monomorphisation:new-datatype"
    val cntNewFunctions		= ST.newCounter "ast-monomorphisation:new-function"

    fun findInstantiation (l, types) = let
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
          | SOME l => (case findInstantiation (l, types)
                        of SOME _ => funMap
                         | NONE => AMap.insert (funMap, var, (newVar, types)::l))
    end

    fun printInstantiations funMap = let
        fun vToString (v, tys) = concat[Var.toString v, ":",
                                        String.concatWith "," (List.map TypeUtil.toString tys)]
        fun p (v, l) = print (concat[Var.toString v, " -> ",
                                     String.concatWith " ; " (List.map vToString l), "\n"])
    in
        AMap.appi p funMap
    end

    (*
     * We track instantiated meta variables by their stamps rather than
     * the entire tyvar. This setup is because we need to be able to map
     * from the tyvar to other data, and the Stamp structure conveniently
     * provides that functionality.
     *)
    fun findMetaVars tys = let
        fun findMetaVar ty =
            case ty
             of Types.MetaTy (Types.MVar {info=info,...}) => (
                case !info
                 of Types.INSTANCE ty => findMetaVar ty
                  | _ => NONE)
              | Types.VarTy (Types.TVar{stamp,...}) => SOME stamp
              | _ => NONE
        fun findFold (ty, ms) =
            case findMetaVar ty
             of NONE => ms
              | SOME mv => (case ms
                             of SOME l => SOME (l@[mv])
                              | NONE => SOME [mv])
    in
        List.foldr findFold NONE tys
    end

    fun addMetaFunctions (metaMap, metas, v) = let
        fun addMetaFunction (meta, metaMap) =
            case SMap.find (metaMap, meta)
             of NONE => SMap.insert (metaMap, meta, [v])
              | SOME vs => (if List.exists (fn (v') => AV.same (v', v)) vs
                            then metaMap
                            else SMap.insert (metaMap, meta, v::vs))
    in
        List.foldr addMetaFunction metaMap metas
    end

    fun printMetas metaMap = let
        fun p (s, l) = print (concat[Stamp.toString s, " -> ",
                                     String.concatWith " ; " (List.map Var.toString l), "\n"])
    in
        SMap.appi p metaMap
    end

    fun addDcon (dconMap, Types.DCon {owner=Types.Tyc{stamp=dcStamp,...},...}, tys) = let
        fun sameTys tys' =
            ListPair.all TypeUtil.same (tys, tys')
    in
        case SMap.find (dconMap, dcStamp)
         of NONE => SMap.insert (dconMap, dcStamp, [tys])
          | SOME tysl => if List.exists sameTys tysl
                         then dconMap
                         else SMap.insert (dconMap, dcStamp, tys::tysl)
    end
                             

    fun printDcons dconMap = let
        fun typesToString tys = 
            String.concatWith "," (List.map TypeUtil.toString tys)
        fun p (s, l) = print (concat[Stamp.toString s, " -> ",
                                     String.concatWith " ; " (List.map typesToString l), "\n"])
    in
        SMap.appi p dconMap
    end

    fun analyse body = let
      fun exp (A.LetExp (b, e), funMap, metaMap, dconMap) = let
          val (funMap, metaMap, dconMap) = binding (b, funMap, metaMap, dconMap)
      in
          exp (e, funMap, metaMap, dconMap)
      end
	| exp (A.IfExp (e1, e2, e3, t), funMap, metaMap, dconMap) = let
              val (funMap, metaMap, dconMap) = exp (e1, funMap, metaMap, dconMap)
              val (funMap, metaMap, dconMap) = exp (e2, funMap, metaMap, dconMap)
          in
              exp (e3, funMap, metaMap, dconMap)
          end
	| exp (A.CaseExp (e, ms, t), funMap, metaMap, dconMap) = let
              val (funMap, metaMap, dconMap) = List.foldr (fn (m, (funMap, metaMap, dconMap)) =>
                                                     match (m, funMap, metaMap, dconMap))
                                                 (funMap, metaMap, dconMap)
                                                 ms
          in
              exp (e, funMap, metaMap, dconMap)
          end
	| exp (A.HandleExp (e, ms, t), funMap, metaMap, dconMap) = let
              val (funMap, metaMap, dconMap) = List.foldr (fn (m, (funMap, metaMap, dconMap)) =>
                                                     match (m, funMap, metaMap, dconMap))
                                                 (funMap, metaMap, dconMap) 
                                                 ms
          in
              exp (e, funMap, metaMap, dconMap)
          end
	| exp (A.RaiseExp (l, e, t), funMap, metaMap, dconMap) = exp (e, funMap, metaMap, dconMap)
	| exp (A.FunExp (x, e, t), funMap, metaMap, dconMap) = exp (e, funMap, metaMap, dconMap)
	| exp (A.ApplyExp (e1, e2, t), funMap, metaMap, dconMap) = let
              val (funMap, metaMap, dconMap) = exp (e1, funMap, metaMap, dconMap)
          in
              exp (e2, funMap, metaMap, dconMap)
          end
	| exp (m as A.VarArityOpExp _, funMap, metaMap, dconMap) = (funMap, metaMap, dconMap)
	| exp (A.TupleExp es, funMap, metaMap, dconMap) = 
          List.foldr (fn (e, (funMap, metaMap, dconMap)) => exp (e, funMap, metaMap, dconMap)) (funMap, metaMap, dconMap) es
	| exp (A.SpawnExp e, funMap, metaMap, dconMap) = exp (e, funMap, metaMap, dconMap)
	| exp (k as A.ConstExp c, funMap, metaMap, dconMap) = const (c, funMap, metaMap, dconMap)
	| exp (x as A.VarExp (v, ts), funMap, metaMap, dconMap) = (
          if (length ts = 0)
          then (funMap, metaMap, dconMap)
          else (let
            val funMap = addInstantiation (funMap, v, ts)
                in
                    (funMap, 
                     case (findMetaVars ts)
                      of SOME metas => addMetaFunctions (metaMap, metas, v)
                       | NONE => metaMap,
                       dconMap)
                end))
	| exp (A.SeqExp (e1, e2), funMap, metaMap, dconMap) = let
              val (funMap, metaMap, dconMap) = exp (e1, funMap, metaMap, dconMap)
          in
              exp (e2, funMap, metaMap, dconMap)
          end
	| exp (A.OverloadExp ovr, funMap, metaMap, dconMap) = (funMap, metaMap, dconMap)
	| exp (A.ExpansionOptsExp(opts, e), funMap, metaMap, dconMap) = exp (e, funMap, metaMap, dconMap)
	| exp (A.PTupleExp es, funMap, metaMap, dconMap) = 
          List.foldr (fn (e, (funMap, metaMap, dconMap)) => exp (e, funMap, metaMap, dconMap)) (funMap, metaMap, dconMap) es
	| exp (A.PCaseExp (es, ms, ty), funMap, metaMap, dconMap) =
          raise Fail "PCaseExp should be removed by UnPar or Elaborate."
	| exp (A.RangeExp (e1, e2, oe3, t), funMap, metaMap, dconMap) =
          raise Fail "RangeExp should be removed by UnPar or Elaborate."
	| exp (A.PArrayExp (es, t), funMap, metaMap, dconMap) = 
          raise Fail "PArrayExp should be removed by UnPar or Elaborate."
	| exp (A.PCompExp (e, pes, oe), funMap, metaMap, dconMap) = 
          raise Fail "PCompExp should be removed by UnPar or Elaborate."
	| exp (A.PChoiceExp (es, t), funMap, metaMap, dconMap) =
          raise Fail "PChoiceExp should be removed by UnPar or Elaborate."
      and binding (A.ValBind (p, e), funMap, metaMap, dconMap) = let
          val (funMap, metaMap, dconMap) = exp (e, funMap, metaMap, dconMap)
      in
          pat (p, funMap, metaMap, dconMap)
      end
	| binding (A.FunBind lams, funMap, metaMap, dconMap) =
          List.foldr (fn (l, (funMap, metaMap, dconMap)) => lambda (l, funMap, metaMap, dconMap)) (funMap, metaMap, dconMap) lams
	| binding (A.PrimVBind (v, code), funMap, metaMap, dconMap) = (funMap, metaMap, dconMap)
	| binding (A.PrimCodeBind code, funMap, metaMap, dconMap) = (funMap, metaMap, dconMap)
	| binding (A.PValBind (p, e), funMap, metaMap, dconMap) = 
          raise Fail "PValBind should be removed by UnPar or Elaborate."
      and lambda (A.FB (f, x, e), funMap, metaMap, dconMap) = exp (e, funMap, metaMap, dconMap)
      and match (A.PatMatch (p, e), funMap, metaMap, dconMap) = let
          val (funMap, metaMap, dconMap) = exp (e, funMap, metaMap, dconMap)
      in
          pat (p, funMap, metaMap, dconMap)
      end
	| match (A.CondMatch (p, e1, e2), funMap, metaMap, dconMap) = let
              val (funMap, metaMap, dconMap) = exp (e1, funMap, metaMap, dconMap)
              val (funMap, metaMap, dconMap) = exp (e2, funMap, metaMap, dconMap)
          in
              pat (p, funMap, metaMap, dconMap)
          end
      and pat (A.ConPat (dc, [], p), funMap, metaMap, dconMap) = pat (p, funMap, metaMap, dconMap)
        | pat (A.ConPat (dc, tys, p), funMap, metaMap, dconMap) = 
              pat (p, funMap, metaMap, addDcon (dconMap, dc, tys))
        | pat (A.TuplePat (ps), funMap, metaMap, dconMap) =
          List.foldr (fn (p, (funMap, metaMap, dconMap)) => pat (p, funMap, metaMap, dconMap)) (funMap, metaMap, dconMap) ps
        | pat (A.VarPat v, funMap, metaMap, dconMap) = (funMap, metaMap, dconMap)
        | pat (A.WildPat ty, funMap, metaMap, dconMap) = (funMap, metaMap, dconMap)
        | pat (A.ConstPat c, funMap, metaMap, dconMap) = const (c, funMap, metaMap, dconMap)
      and const (A.DConst (dc, []), funMap, metaMap, dconMap) =
          (funMap, metaMap, dconMap)
        | const(A.DConst (dc, tys), funMap, metaMap, dconMap) =
          (funMap, metaMap, addDcon (dconMap, dc, tys))
        | const (A.LConst (l, ty), funMap, metaMap, dconMap) = (funMap, metaMap, dconMap)
    in
        exp (body, AMap.empty, SMap.empty, SMap.empty)
    end

    (*
     * Extends the funMap by closing it over all of the functions that are
     * recursively instantiated during type application.
     *
     * TODO
     *)
    fun processFuns (funMap, metaMap) =
        ()
                                       
    (*
     * Need to create new constructors for the datatypes at each of the
     * monomorphic types.
     * 
     * TODO
     *)
    fun processDatatypes dconMap =
        ()

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
	| exp (A.RaiseExp (l, e, t)) = A.RaiseExp (l, exp e, substBound t)
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
                val (funMap, metaMap, dconMap) = analyse body
                val _ = printInstantiations funMap
                val _ = printMetas metaMap
                val _ = printDcons dconMap
                val funs = processFuns (funMap, metaMap)
                val dcons = processDatatypes dconMap
            in
                monomorphiseExp (AMap.empty, dconMap, body, [])
            end
        else body
             
  end
