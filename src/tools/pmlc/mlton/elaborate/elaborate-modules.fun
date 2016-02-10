(* Copyright (C) 2012 Matthew Fluet.
 * Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor ElaborateModules (S: ELABORATE_MODULES_STRUCTS): ELABORATE_MODULES =
struct

(* Save these so we can use the normal option/vector operations later *)
structure MLOption = Option
structure MLVector = Vector
structure MLList = List
structure Option = MLtonOption
structure Vector = MLtonVector
structure List = MLtonList

open S

(* [PML] TODO: figure out where the best place to instantiate this is *)
local
   open Control.Elaborate
in
   val resolveScope = fn () => current resolveScope
end

local
   open Ast
in
   structure FctArg = FctArg
   structure Fctid = Fctid
   structure Longstrid = Longstrid
   structure SigConst = SigConst
   structure Sigexp = Sigexp
   structure Strdec = Strdec
   structure Strexp = Strexp
   structure Strid = Strid
   structure Symbol = Symbol
   structure Topdec = Topdec
end

local
   open Env
in
   structure Decs = Decs
   structure FunctorClosure = FunctorClosure
   structure Structure = Structure
end

structure ElaborateSigexp = ElaborateSigexp (structure Ast = Ast
                                             structure Env = Env)

structure ElaborateCore = ElaborateCore (structure Ast = Ast
                                         structure CoreML = CoreML
                                         structure Decs = Decs
                                         structure Env = Env)

(* structure PrimBOM = PrimBOM ( *)
(*   structure Ast = Ast *)
(*   structure CoreBOM = CoreBOM) *)

structure ElaborateBOMCore = ElaborateBOMCore (
  structure Ast = Ast
  structure CoreBOM = CoreBOM
  structure Decs = Decs
  structure BOMEnv = BOMEnv
  structure Env = Env
  structure CoreML = CoreML
)

structure ElaborateBOMImports = ElaborateBOMImports (
  structure Ast = Ast
  structure CoreBOM = CoreBOM
  structure ElaborateCore = ElaborateCore
  structure ElaborateBOMCore = ElaborateBOMCore
  structure BOMEnv = BOMEnv
  structure Env = Env
  structure CoreML = CoreML
)

structure AstBOM = Ast.BOM
structure BOMType = CoreBOM.BOMType

structure BOMTyConKV = struct
  type ord_key = CoreBOM.TyCon.t
  val compare = CoreBOM.TyCon.compare
end
structure TyConMap = RedBlackMapFn (BOMTyConKV)

val elabStrdecInfo = Trace.info "ElaborateModules.elabStrdec"
val elabStrexpInfo = Trace.info "ElaborateModules.elabStrexp"
val elabTopdecInfo = Trace.info "ElaborateModules.elabTopdec"

fun elaborateTopdec (topdec, {env = E: Env.t, bomEnv: BOMEnv.t}) =
   let
      fun elabSigexp s = ElaborateSigexp.elaborateSigexp (s, {env = E})
      fun elabSigexpConstraint (cons: SigConst.t,
                                S: Structure.t option,
                                nest: string list)
         : Decs.t * Structure.t option =
         let
            fun s (sigexp, opaque) =
               let
                  val prefix =
                     case nest of
                        [] => ""
                      | _ => concat (List.fold (nest, [], fn (s, ac) =>
                                                s :: "." :: ac))
               in
                  case S of
                     NONE => (Decs.empty, NONE)
                   | SOME S =>
                        let
                           val (S, decs) =
                              case elabSigexp sigexp of
                                 NONE => (S, Decs.empty)
                               | SOME I =>
                                    Env.cut (E, S, I,
                                             {isFunctor = false,
                                              opaque = opaque,
                                              prefix = prefix},
                                             Sigexp.region sigexp)
                        in
                           (decs, SOME S)
                        end
               end
         in
            case cons of
               SigConst.None => (Decs.empty, S)
             | SigConst.Opaque sigexp => s (sigexp, true)
             | SigConst.Transparent sigexp => s (sigexp, false)
         end
      fun elabStrdec (arg: Strdec.t * string list): Decs.t =
         Trace.traceInfo' (elabStrdecInfo,
                           Layout.tuple2 (Strdec.layout,
                                          List.layout (*String.layout*)Layout.str),
                           Decs.layout)
         (fn (d: Strdec.t, nest: string list) =>
          let
             val d = Strdec.coalesce d
             val elabStrdec = fn d => elabStrdec (d, nest)
             val decs =
                (case Strdec.node d of
                   Strdec.Core d => (* rule 56 *)
                      ElaborateCore.elaborateDec
                      (d, {env = E, nest = nest})
                 | Strdec.Local (d, d') => (* rule 58 *)
                      Env.localModule (E,
                                       fn () => elabStrdec d,
                                       fn d => Decs.append (d, elabStrdec d'))
                 | Strdec.Seq ds => (* rule 60 *)
                      List.fold
                      (ds, Decs.empty, fn (d, decs) =>
                       Decs.append (decs, elabStrdec d))
                 | Strdec.Structure strbinds => (* rules 57, 61 *)
                      let
                         val strbinds =
                            Vector.map
                            (strbinds, fn {name, def, constraint} =>
                             let
                                val nest = Strid.toString name :: nest
                                val (decs', S) = elabStrexp (def, nest)
                                val (decs'', S) =
                                   elabSigexpConstraint (constraint, S, nest)
                             in
                                {decs = Decs.append (decs', decs''),
                                 name = name,
                                 S = S}
                             end)
                         val () =
                            Vector.foreach
                            (strbinds, fn {name, S, ...} =>
                             Option.app (S, fn S => Env.extendStrid (E, name, S)))
                       in
                          Decs.appendsV (Vector.map (strbinds, #decs))
                       end
                 | Strdec.BOMExportDec export =>
                     let
                         (* FIXME: we need to save these envs *)
                         (* FIXME: pass in a real mltyenv *)
                       val (newEnv, newMLTyEnv) =
                         ElaborateBOMImports.elaborateBOMExport (export, {env =
                         E, bomEnv = bomEnv})
                     in
                       (* TODO(wings): should we be doing more work here? probably, according
                       to the above comments which applied when this case simply returned
                       Decs.empty *)
                       (case newEnv
                       of SOME bomdec => Decs.single (CoreML.Dec.BOMExport bomdec)
                        | NONE => Decs.empty
                       (* end case *))
                     end
                (* end case *))
(* TODO: add cases for _datatype, _type, and _val *)
             val () =
                case resolveScope () of
                   Control.Elaborate.ResolveScope.Strdec =>
                      (ElaborateCore.reportUnresolvedFlexRecords ()
                       ; ElaborateCore.resolveOverloads ())
                 | _ => ()
          in
             decs
          end) arg
      and elabStrexp (arg: Strexp.t * string list): Decs.t * Structure.t option =
         Trace.traceInfo' (elabStrexpInfo,
                           Layout.tuple2 (Strexp.layout,
                                          List.layout (*String.layout*)Layout.str),
                           Layout.tuple2 (Decs.layout,
                                          Option.layout Structure.layout))
         (fn (e: Strexp.t, nest: string list) =>
          let
             val elabStrexp = fn e => elabStrexp (e, nest)
          in
             case Strexp.node e of
                Strexp.App (fctid, strexp) => (* rules 54, 154 *)
                   let
                      val (decs, S) = elabStrexp strexp
                   in
                      case S of
                         NONE => (decs, NONE)
                       | SOME S =>
                            case Env.lookupFctid (E, fctid) of
                               NONE => (decs, NONE)
                             | SOME fct  =>
                                  let
                                     val (S, decs') =
                                        Env.cut
                                        (E, S,
                                         FunctorClosure.argInterface fct,
                                         {isFunctor = true,
                                          opaque = false,
                                          prefix = ""},
                                         Strexp.region strexp)
                                     val (decs'', S) =
                                        FunctorClosure.apply
                                        (fct, S, [Fctid.toString fctid])
                                  in
                                     (Decs.appends [decs, decs', decs''], S)
                                  end
                   end
              | Strexp.Constrained (e, c) => (* rules 52, 53 *)
                   let
                      val (decs, S) = elabStrexp e
                      val (decs', S) = elabSigexpConstraint (c, S, nest)
                   in
                      (Decs.append (decs, decs'), S)
                   end
              | Strexp.Let (d, e) => (* rule 55 *)
                   Env.scope
                   (E, fn () =>
                    let
                       val decs = elabStrdec (d, nest)
                       val (decs', S) = elabStrexp e
                    in
                       (Decs.append (decs, decs'), S)
                    end)
              | Strexp.Struct d => (* rule 50 *)
                   let
                      val (decs, S) =
                         Env.makeStructure (E, fn () => elabStrdec (d, nest))
                   in
                      (decs, SOME S)
                   end
              | Strexp.Var p => (* rule 51 *)
                   (Decs.empty, Env.lookupLongstrid (E, p))
          end) arg
      fun elabFunctor {arg, body, name, result}: FunctorClosure.t option =
         let
            val body = Strexp.constrained (body, result)
            val (arg, argSig, body, prefix) =
               case FctArg.node arg of
                  FctArg.Structure (arg, argSig) =>
                     (arg, argSig, body, concat [Strid.toString arg, "."])
                | FctArg.Spec spec =>
                     let
                        val strid =
                           Strid.fromSymbol (Symbol.fromString "ZZZNewStridZZZ",
                                             Region.bogus)
                     in
                        (strid,
                         Sigexp.spec spec,
                         Strexp.lett (Strdec.openn (Vector.new1
                                                    (Longstrid.short strid)),
                                      body),
                         "")
                     end
         in
            Option.map (elabSigexp argSig, fn argInt =>
                        Env.functorClosure
                        (E, arg, [Fctid.toString name], prefix, argInt,
                         fn (formal, nest) =>
                         Env.scope (E, fn () =>
                                    (Env.extendStrid (E, arg, formal)
                                     ; elabStrexp (body, nest)))))
         end

      (* creates an ML Tycon.t from a BOM TyCon.t.

      this is needed because defunctorization and XML analyze usage of data
      constructors, and the best way for BOM to benefit from that analysis is to
      make its types into the MLton representation beforehand (and pick them out
      afterward)

      therefore, we traverse all BOM datatype definitions (as well as any BOM
      primitive types present in the program), and lazily create corresponding
      MLton tycons for the BOM types referenced from the datatype definitions

      this allows BOM types being exported into MLton code via simply giving
      names to the underlying MLton type

      most of this code just manages the cache of type and tycon mappings,
      looking up BOM datatype definitions out of the given BOMEnv and storing
      the MLton datatypes it creates into the ML Env

      makeMLTycon is the heart of datatype translation, and it may be a little
      confusing, but mostly it cribs from elaborate-core's elabDatBind, since it
      does a similar (if larger) job *)
      fun makeMLDatatype (env): (BOMEnv.t) -> ((*tyvars, *)CoreBOM.TyCon.t) ->
         (CoreML.Tycon.t * CoreML.PrimConDef.t list) =
         fn (bomEnv) =>
         let
            val mapping = ref TyConMap.empty

            (* define builtin tycons corresponding to CoreBOM.Type.t variants *)
            val recordTycon = Env.newTycon ("record", Env.TypeStr.Kind.Nary,
               Env.TypeEnv.Tycon.AdmitsEquality.Sometimes, Region.bogus)
            (* tuples are handled specially in MLton so no tycon is needed *)
            val bomfunTycon = Env.newTycon ("fun", Env.TypeStr.Kind.Arity 3,
               Env.TypeEnv.Tycon.AdmitsEquality.Always, Region.bogus)
            val bignumTycon = Env.newTycon ("bignum", Env.TypeStr.Kind.Arity 0,
               Env.TypeEnv.Tycon.AdmitsEquality.Always, Region.bogus)
            val anyTycon = Env.newTycon ("any", Env.TypeStr.Kind.Arity 0,
               Env.TypeEnv.Tycon.AdmitsEquality.Always (* XXX(wings): does Any admit equality? *), Region.bogus)
            val vprocTycon = Env.newTycon ("vproc", Env.TypeStr.Kind.Arity 0,
               Env.TypeEnv.Tycon.AdmitsEquality.Always, Region.bogus)
            val arrayTycon = Env.newTycon ("array", Env.TypeStr.Kind.Arity 1,
               Env.TypeEnv.Tycon.AdmitsEquality.Always (* instance identity *), Region.bogus)
            val vectorTycon = Env.newTycon ("vector", Env.TypeStr.Kind.Arity 1,
               Env.TypeEnv.Tycon.AdmitsEquality.Sometimes, Region.bogus)
            val contTycon = Env.newTycon ("cont", Env.TypeStr.Kind.Nary,
               Env.TypeEnv.Tycon.AdmitsEquality.Never (* XXX(wings): do conts admit equality? *), Region.bogus)
            val addrTycon = Env.newTycon ("addr", Env.TypeStr.Kind.Arity 1,
               Env.TypeEnv.Tycon.AdmitsEquality.Always, Region.bogus)

            fun convertMLTy(bomTy: BOMType.t): CoreML.Type.t =
               case bomTy of
                  (* TODO(wings): handle bool!  => CoreML.Type.bool *)
                  BOMType.Tuple elems => CoreML.Type.tuple (Vector.fromList (List.map (elems, fn (mutable, ty) => convertMLTy ty)))
                | _ =>
                     let
                        val (tycon, args) = extractTyconAndArgs(bomTy)
                        val args' = Vector.fromList (List.map (args, convertMLTy))
                     in
                        Env.TypeStr.apply (Env.TypeStr.tycon (tycon, raise Fail "kind"), args')
                     end

            and convertMLTycon(bomTyc: CoreBOM.TyCon.t): (CoreML.Tycon.t * CoreML.PrimConDef.t list) =
               case TyConMap.find(!mapping, bomTyc) of
                        SOME mlTyconDef => mlTyconDef
                      | NONE =>
                           let
                              val mlTyconDef = makeMLTycon bomTyc
                           in
                              mapping := TyConMap.insert(!mapping, bomTyc, mlTyconDef);
                              mlTyconDef
                           end
            (* XXX(wings): the "TODO" failures here are unimplemented but the
            other raises here must fail *)
            and extractTyconAndArgs(bomTy: BOMType.t): (CoreML.Tycon.t * BOMType.t list) =
               case bomTy of
                  BOMType.Param tyParam => raise Fail "trying to find tycon for TyParam"
                | BOMType.TyCon {con, args} => (#1 (convertMLTycon(con)), args)
                | BOMType.Con {dom, rng} => (raise Fail "TODO(wings): find tycon for Con", [dom, rng])
                | BOMType.Record fields => (recordTycon, raise Fail "TODO(wings): fields")
                | BOMType.Tuple elems => raise Fail "trying to find tycon for tuple"
                | BOMType.Fun {dom, cont, rng} => (bomfunTycon, dom @ cont @ rng)
                  (* TODO(wings): concatenating here cannot be right; maybe
                  we should have a separate BOMType for BOM function types,
                  since their continuation types are known "early" *)
                | BOMType.BigNum => (bignumTycon, [])
                | BOMType.Exn => raise Fail "trying to find tycon for exception"
                | BOMType.Any => (anyTycon, [])
                | BOMType.VProc => (vprocTycon, [])
                | BOMType.Array elemty => (arrayTycon, [elemty])
                | BOMType.Vector elemty => (vectorTycon, [elemty])
                | BOMType.Cont elemtys => (contTycon, elemtys)
                | BOMType.CFun cproto => raise Fail "trying to find tycon for cfun"
                | BOMType.Addr destty => (addrTycon, [destty])
                | BOMType.Raw r => (raise Fail "TODO(wings): find tycon for raw type", [])
                | BOMType.Error => raise Fail "trying to find tycon for <type error>"

            and makeMLTycon (bomtyc as CoreBOM.TyC {id, definition, params, ...}):
               (CoreML.Tycon.t * CoreML.PrimConDef.t list) =
               let
                  val primConDefs = !definition

                  fun makeMLTyVar tyvar = Env.TypeEnv.Tyvar.newString ("__bomtyvar_" ^ CoreBOM.TyParam.name tyvar, {left=SourcePos.bogus, right=SourcePos.bogus})
                  (* elaborate tyvars with a simple conversion *)
                  val tyvars = MLVector.fromList (MLList.map makeMLTyVar params)
                  val tyvars' = MLVector.map Env.TypeEnv.Type.var tyvars
                  (* the name (to be used in ML) of the BOM definitions being exported *)
                  val tyId = id

                  (* make an ML AST Tycon based on the BOM name *)
                  val tyconName = "__bomtycon_" ^ CoreBOM.TyId.toString id
                  val tyconSymbol = Ast.Symbol.fromString (tyconName)
                  val astTycon = Ast.Tycon.fromSymbol (tyconSymbol, Region.bogus)

                  val kind = Env.TypeStr.Kind.Arity (Vector.length tyvars)
                  (* TODO(wings): check kind matches *)
                  val mlTycon = Env.newTycon (tyconName, kind,
                     Env.TypeEnv.Tycon.AdmitsEquality.Never, Ast.Tycon.region astTycon)
                  val resultMLTy: CoreML.Type.t =
                     Env.TypeEnv.Type.con (mlTycon, tyvars')

                  (* mutate the ML type env to store the tycon *)
                  val _ = Env.extendTycon (env, astTycon, Env.TypeStr.tycon (mlTycon,
                     kind), {forceUsed = false, isRebind = false})

                  val _ = (print "pre-primcondef search, bomEnv="; BOMEnv.ValEnv.printKeys bomEnv)

                  (* place all data cons into the environment and translate them to CoreML *)
                  val (primConDefs, cons) = ListPair.unzip (List.map (primConDefs, (fn CoreBOM.DataConsDef.ConsDef (bomId, maybeArgTy) =>
                     let
                        val bomVal =
                          case BOMEnv.ValEnv.lookup (bomEnv, CoreBOM.ValId.fromBOMId' bomId) of
                             SOME bv => bv
                           | NONE => raise Fail ("failed to find referenced BOM name: "
                             ^ CoreBOM.BOMId.toString bomId)

                        val conName = "__bomcon_" ^ CoreBOM.BOMId.toString bomId
                        val astCon = Ast.Con.fromSymbol (Ast.Symbol.fromString (conName), Region.bogus)
                        val mlCon = CoreML.Con.fromString (conName)

                        val maybeArgMLTy = Option.map (maybeArgTy, convertMLTy)
                        val conMLTy =
                          case maybeArgMLTy of
                             SOME argMLTy => CoreML.Type.arrow (argMLTy, resultMLTy)
                           | NONE => resultMLTy
                      in
                         (CoreML.PrimConDef.T (mlCon, maybeArgMLTy, resultMLTy, bomVal),
                            {con=mlCon,
                            name=astCon,
                            arg=maybeArgMLTy,
                            ty=conMLTy})
                      end)
                    ))

                  (* make various records the same way elaborate-core does *)
                  val (schemes, datatypeCons) =
                     ListPair.unzip
                     (List.map
                     (cons, fn {con, name, arg, ty} =>
                         let
                            val scheme =
                               Env.TypeEnv.Scheme.make {canGeneralize = true,
                                                        ty = ty,
                                                        tyvars = tyvars}
                         in
                            (scheme, {arg = arg, con = con})
                         end))
                  val (schemes, datatypeCons) = (Vector.fromList schemes, Vector.fromList datatypeCons)
                  val makeCons =
                     Env.newCons (env, Vector.fromList (List.map (cons, fn {con, name, ...} => {con=con, name=name})))
                  val typeStr = Env.TypeStr.data (mlTycon, kind, makeCons schemes)

                  (* mutate the ML type env to associate the cons with the tycon *)
                  val _ = Env.extendTycon (env, astTycon, typeStr,
                     {forceUsed = false,
                     isRebind = true})
               in
                  (mlTycon, primConDefs)
               end
         in
            convertMLTycon
         end

      fun elabTopdec arg: (Decs.t * BOMEnv.t) =
         (* TODO: I had to disable this to add in the BOMEnv, fix it later *)
         (* Trace.traceInfo' (elabTopdecInfo, *)
         (*                   Topdec.layout, *)
         (*                   Decs.layout) *)
         (fn (d: Topdec.t) =>
          let
             val decs =
                case Topdec.node d of
                   Topdec.Signature sigbinds =>
                      let
                         val sigbinds =
                            Vector.map
                            (sigbinds, fn (sigid, sigexp) =>
                             (sigid, elabSigexp sigexp))
                         val () =
                            Vector.foreach
                            (sigbinds, fn (sigid, I) =>
                             Option.app (I, fn I => Env.extendSigid (E, sigid, I)))
                      in
                         (Decs.empty, bomEnv)
                      end
                 | Topdec.Strdec d => (elabStrdec (d, []), bomEnv)
                 | Topdec.Functor funbinds =>
                      (* Rules 85, 86. Appendix A, p.58 *)
                      let
                         val funbinds =
                            Vector.map
                            (funbinds, fn {arg, body, name, result} =>
                             {closure = elabFunctor {arg = arg,
                                                     body = body,
                                                     name = name,
                                                     result = result},
                              name = name})
                         val () =
                            Vector.foreach (funbinds, fn {closure, name} =>
                                            Option.app
                                            (closure, fn closure =>
                                             Env.extendFctid (E, name, closure)))
                         (* Check for errors here so that we don't report duplicate
                          * errors when re-elaborating the functor body.
                          *)
                         val () = Control.checkForErrors "elaborate"
                      in
                         (Decs.empty, bomEnv)
                      end
                | Topdec.PrimModule (id, imports, bomDecs) =>
                  let
                    val namedEnv = BOMEnv.setName' (bomEnv, id)
                    (* FIXME: pull in the basis  *)
                    val mlTyEnv = BOMEnv.MLTyEnv.empty

                    fun appendMaybe (xs: 'a list, x: 'a option) =
                      case x of
                        SOME x => x::xs
                      | NONE => xs

                    fun importToDef import = case import of
                        CoreML.BOMImport.Val (pcd, var, bomVal, bomTy) =>
                          NONE (*SOME (CoreBOM.Definition.Import (mlTycon, (bomTyc, [pcd])))*)
                      | CoreML.BOMImport.Datatype (mlTycon, bomTyc, pcds) =>
                          SOME (CoreBOM.Definition.Import (bomTyc, (mlTycon, pcds)))
                      | CoreML.BOMImport.Exception (mlCon, newVal, maybeArgMlTy, resultMlTy, mlTycon) =>
                          SOME (CoreBOM.Definition.ImportExn (mlTycon,
                            [CoreML.PrimConDef.T (mlCon, maybeArgMlTy, CoreML.Type.bool, newVal)]
                          ))

                    fun foldOverEnv (elabStmt: 'a * 'b -> 'g option * 'b)
                        (stmt: 'a, (oldStmts: 'g list, oldEnv: 'b)) =
                      (fn (maybeNewStmt, newEnv) =>
                        (appendMaybe (oldStmts, maybeNewStmt), newEnv)) (
                        elabStmt (stmt, oldEnv))

                    (* TODO(wings): ensure that imports are not reversed *)
                    val (imports, {env, bomEnv}) =
                     MLVector.foldl (foldOverEnv
                       ElaborateBOMImports.elaborateBOMImport) ([], {env = E,
                       bomEnv = namedEnv}) imports

                    val imports' = MLList.mapPartial importToDef imports

                    val (defs, bomEnv') = MLVector.foldl (foldOverEnv (ElaborateBOMCore.elaborateBOMDec (makeMLDatatype env)))
                        ([], bomEnv) bomDecs

                    val bomModule = CoreML.BOMModule.T {
                        defs = imports' @ (rev defs)}

                    val () = Control.checkForErrors "elaborate"
                  in
                    (*print "bomEnv="; BOMEnv.ValEnv.printKeys bomEnv';*) (Decs.single (CoreML.Dec.BOMModule bomModule), bomEnv')
                  end
            val () =
               case resolveScope () of
                  Control.Elaborate.ResolveScope.Topdec =>
                     (ElaborateCore.reportUnresolvedFlexRecords ()
                      ; ElaborateCore.resolveOverloads ())
                | _ => ()
            val _ = ElaborateCore.reportUndeterminedTypes ()
            val _ = ElaborateCore.reportSequenceNonUnit ()
          in
             decs
          end) arg
   in
      elabTopdec topdec
   end

val reportSequenceNonUnit = ElaborateCore.reportSequenceNonUnit
val reportUndeterminedTypes = ElaborateCore.reportUndeterminedTypes
val reportUnresolvedFlexRecords = ElaborateCore.reportUnresolvedFlexRecords
val resolveOverloads = ElaborateCore.resolveOverloads

end
