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

structure Option = MLtonOption
structure List = MLtonList
structure Vector = MLtonVector

open S

(* TODO: figure out where the best place to instantiate this is *)


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
  structure CoreML = CoreML)

structure AstBOM = Ast.AstBOM

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
                case Strdec.node d of
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
                | Topdec.PrimModule (id, bomDecs) =>
                  let
                    fun loop (index,
                        currentEnv: BOMEnv.t,
                        acc: Decs.dec list): (Decs.t * BOMEnv.t) =
                      if index >= (Vector.length bomDecs) then
                        (Decs.fromList (rev acc), currentEnv)
                      else
                        let
                          val (newDec: Decs.dec, newEnv: BOMEnv.t) =
                            ElaborateBOMCore.elaborateBomDec (
                              Vector.sub (bomDecs, index),
                              {env = E, bomEnv = currentEnv})
                        in
                          loop (index + 1, newEnv, newDec::acc)
                        end
                    val namedEnv = BOMEnv.setName' (bomEnv, id)
                    val (newDecs, newEnv) = loop (0, namedEnv, [])
                    val () = Control.checkForErrors "elaborate"
                  in
                  (* TODO: return something real here *)
                    (Decs.empty, newEnv)
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