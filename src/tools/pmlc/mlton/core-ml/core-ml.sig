(* Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature CORE_ML_STRUCTS =
   sig
      include ATOMS
      structure Type:
         sig
            type t

            val arrow: t * t -> t
            val bool: t
            val deConOpt: t -> (Tycon.t * t vector) option
            val deRecord: t -> (Record.Field.t * t) vector
            val isCharX: t -> bool
            val isInt: t -> bool
            val layout: t -> Layout.t
            val makeHom: {con: Tycon.t * 'a vector -> 'a,
                          var: Tyvar.t -> 'a} -> {destroy: unit -> unit,
                                                  hom: t -> 'a}
            val tuple: t vector -> t
            val unit: t
         end
       structure CoreBOM: CORE_BOM
   end

signature CORE_ML =
   sig
      include CORE_ML_STRUCTS

      structure Pat:
         sig
            type t
            datatype node =
               Con of {arg: t option,
                       con: Con.t,
                       targs: Type.t vector}
             | Const of unit -> Const.t
             | Layered of Var.t * t
             | List of t vector
             | Record of t Record.t
             | Tuple of t vector
             | Var of Var.t
             | Wild

            val dest: t -> node * Type.t
            val falsee: t
            val foreachVar: t * (Var.t -> unit) -> unit
            (* true if pattern contains a constant, constructor or variable *)
            val isRefutable: t -> bool
            val isUnit: t -> bool
            val isWild: t -> bool
            val layout: t -> Layout.t
            val make: node * Type.t -> t
            val node: t -> node
            val var: Var.t * Type.t -> t
            val truee: t
            val tuple: t vector -> t
            val ty: t -> Type.t
            val wild: Type.t -> t
         end

      structure Exp:
         sig
            type dec
            type lambda
            type t
            datatype noMatch = Impossible | RaiseAgain | RaiseBind | RaiseMatch
            datatype node =
               App of t * t
             | Case of {kind: string,
                        lay: unit -> Layout.t,
                        nest: string list,
                        noMatch: noMatch,
                        nonexhaustiveExnMatch: Control.Elaborate.DiagDI.t,
                        nonexhaustiveMatch: Control.Elaborate.DiagEIW.t,
                        redundantMatch: Control.Elaborate.DiagEIW.t,
                        region: Region.t,
                        rules: {exp: t,
                                lay: (unit -> Layout.t) option,
                                pat: Pat.t} vector,
                        test: t}
             | Con of Con.t * Type.t vector
             | Const of unit -> Const.t
             | EnterLeave of t * SourceInfo.t
             | Handle of {catch: Var.t * Type.t,
                          handler: t,
                          try: t}
             | Lambda of lambda
             | Let of dec vector * t
             | List of t vector
             | PrimApp of {args: t vector,
                           prim: Type.t Prim.t,
                           targs: Type.t vector}
             | Raise of t
             | Record of t Record.t
             | Seq of t vector
             | Var of (unit -> Var.t) * (unit -> Type.t vector)

            val andAlso: t * t -> t
            val casee: {kind: string,
                        lay: unit -> Layout.t,
                        nest: string list,
                        noMatch: noMatch,
                        nonexhaustiveExnMatch: Control.Elaborate.DiagDI.t,
                        nonexhaustiveMatch: Control.Elaborate.DiagEIW.t,
                        redundantMatch: Control.Elaborate.DiagEIW.t,
                        region: Region.t,
                        rules: {exp: t,
                                lay: (unit -> Layout.t) option,
                                pat: Pat.t} vector,
                        test: t} -> t
            val dest: t -> node * Type.t
            val iff: t * t * t -> t
            val falsee: t
            val foreachVar: t * (Var.t -> unit) -> unit
            (* true if the expression may side-effect. See p 19 of Definition *)
            val isExpansive: t -> bool
            val lambda: lambda -> t
            val layout: t -> Layout.t
            val layoutWithType: t -> Layout.t
            val make: node * Type.t -> t
            val node: t -> node
            val orElse: t * t -> t
            val truee: t
            val tuple: t vector -> t
            val ty: t -> Type.t
            val unit: t
            val var: Var.t * Type.t -> t
            val whilee: {expr: t, test: t} -> t
         end

      structure Lambda:
         sig
            type t

            val bogus: t
            val dest: t -> {arg: Var.t,
                            argType: Type.t,
                            body: Exp.t,
                            mayInline: bool}
            val make: {arg: Var.t,
                       argType: Type.t,
                       body: Exp.t,
                       mayInline: bool} -> t
         end
      sharing type Exp.lambda = Lambda.t

      (* This needs to live here, rather than in CoreBOM, because
      CoreBOM doesn't know anything about ML types, which we need here *)
      structure PrimConDef: sig
        datatype t = T of Con.t * Type.t option * Type.t * CoreBOM.Val.t

        (*val arity = arityOfDataCons
        val error = ConsDef (BOMId.bogus, NONE)*)
      end

      structure BOMExport:
        sig
          datatype t
            = Datatype of Tycon.t * CoreBOM.TyCon.t * PrimConDef.t list
            | TypBind of Tycon.t * CoreBOM.TyCon.t
            | ValBind of Var.t * Type.t * CoreBOM.Val.t
          (* FIXME: fill in the rest *)
        end

      structure BOMImport:
        sig
          datatype t
            = Datatype of Tycon.t * CoreBOM.TyCon.t * PrimConDef.t list
            | Exception of Con.t * CoreBOM.Val.t * Type.t option * Type.t * Tycon.t
            | Val of PrimConDef.t * Var.t * CoreBOM.Val.t * CoreBOM.BOMType.t
        end

      structure BOMModule:
        sig
          datatype t
            = T of {defs: (Tycon.t * PrimConDef.t list) CoreBOM.Definition.t list}
        end


      structure Dec:
         sig
            datatype t =
               Datatype of {cons: {arg: Type.t option,
                                   con: Con.t} vector,
                            tycon: Tycon.t,
                            tyvars: Tyvar.t vector} vector
             | Exception of {arg: Type.t option,
                             con: Con.t}
             | Fun of {decs: {lambda: Lambda.t,
                              var: Var.t} vector,
                       tyvars: unit -> Tyvar.t vector}
             | Val of {nonexhaustiveExnMatch: Control.Elaborate.DiagDI.t,
                       nonexhaustiveMatch: Control.Elaborate.DiagEIW.t,
                       rvbs: {lambda: Lambda.t, (* recursive value bindings *)
                              var: Var.t} vector,
                       tyvars: unit -> Tyvar.t vector,
                       vbs: {exp: Exp.t, (* value bindings *)
                             lay: unit -> Layout.t,
                             nest: string list,
                             pat: Pat.t,
                             patRegion: Region.t} vector}
             | BOMExport of BOMExport.t
             | BOMModule of BOMModule.t

            val layout: t -> Layout.t
         end
      sharing type Exp.dec = Dec.t

      structure Program:
         sig
            datatype t = T of {decs: Dec.t vector}

            val layouts: t * (Layout.t -> unit) -> unit
         end
   end
