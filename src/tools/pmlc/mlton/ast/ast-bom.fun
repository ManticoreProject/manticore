(* ast-bom.fun
 *
 * COPYRIGHT (c) 2013 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

functor AstBOM (S: AST_BOM_STRUCTS) : AST_BOM =
  struct
  open S

  structure Wrap = Region.wrap

  (* Helper Functions *)

  fun layoutListOption (maybeXs : 'a option, layoutXs : 'a -> Layout.t) =
      let
          val toLayout = case maybeXs of
                             SOME xs => map layoutXs xs
                           | NONE => Layout.seq []
      in
          Layout.seq toLayout
      end


  (* Structures *)

    (* Helper Structures *)
    functor DoWrap(type node) : sig
                type t = node Wrap.t
                              include WRAPPED
                              sharing type node' = node
                              sharing type obj = t
            end = struct
                                open Wrap
                                type t = node Wrap.t
                                type node' = node
                                type obj = t
            end


    (* Suitable for mutually recursive structures, where the wrapped
    type will already have been declared, but we still need to fill
    out node' and obj *)

    functor DoPartialWrap(type node) : sig
                include WRAPPED
                        sharing type node' = node
                        sharing type obj = t
            end = struct
                                open Wrap
                                type node' = node
                                type obj = t
            end





    (* Atoms *)
    structure BomId = AstId (structure Symbol = Symbol)
    structure HLOpId = AstId (structure Symbol = Symbol)
    structure TyParam = AstId (structure Symbol = Symbol)
    structure TyArg = AstId (structure Symbol = Symbol)
    structure Param = AstId (structure Symbol = Symbol)
    structure FunParam = AstId (structure Symbol = Symbol)
    (* structure LongId = Longid (structure Id = BomId) *)
	structure LongTyId = Longid(structure Id = BomId)
	structure LongConId = Longid(structure Id = BomId)
	structure LongValueId = Longid(structure Id = BomId)


    (* Non-recursive types, part 1 -- types that do not depend on recursive types *)

    structure Attrs = struct
    datatype node = Attributes of string list

    local
        structure Wrapped = DoWrap(type node = node)
    in
    open Wrapped
    end

    fun layout (Attributes ss) =
        Layout.seq (map Layout.str ss)
    end


    (* structure LongId = struct *)
    (* datatype node = *)
    (*       Id of BomId.t * TyArg.t list option *)
    (*       | QualifiedId of TyArg.t list option *)

    (* local *)
    (*  structure Wrapped = DoWrap(type node = node) *)
    (* in *)
    (* open Wrapped *)
    (* end *)

    (* fun layout (Id (bomId, maybeTyArgs)) = *)
    (*  Layout.seq [BomId.layout bomId, layoutListOption (maybeTyArgs, TyArg.layout)] *)
    (*   | layout (QualifiedId (maybeTyArgs))  = layoutListOption maybeTyArgs *)

    (* end *)

    structure RawTy = struct
    datatype node
      = Int8
      | Uint8
      | Int16
      | Uint16
      | Int32
      | Uint32
      | Int64
      | Uint64
      | Float32
      | Float64
    local
        structure Wrapped = DoWrap(type node = node)
    in
    open Wrapped
    end

    fun toString myNode =
        case myNode of
            Int8 => "Int8"
          | Uint8 => "Uint8"
          | Int16 => "Int16"
          | Uint16 => "Uint16"
          | Int32 => "Int32"
          | Uint32 => "Uint32"
          | Int64 => "Int64"
          | Uint64 => "Uint64"
          | Float32 => "Float32"
          | Float64 => "Float64"

    val layout = Layout.str o toString

    end

    structure CArgTy = struct
    datatype node
      = Raw of RawTy.t
      | VoidPointer
    local
        structure Wrapped = DoWrap(type node = node)
    in
    open Wrapped
    end

    fun layout (Raw rawTy) = RawTy.layout rawTy
      | layout VoidPointer = Layout.str "void*"

    end

    structure CReturnTy = struct
    datatype node
      = CArg of CArgTy.t
      | Void


    local
        structure Wrapped = DoWrap(type node = node)
    in
    open Wrapped
    end

    fun layout (CArg cArgTy) = CArgTy.layout cArgTy
      | layout Void = Layout.str "void"
    end


    structure Literal = struct
    datatype node
      = PosInt of int
      | Float of real
      | String of string
      | NullVP

    local
        structure Wrapped = DoWrap(type node = node)
    in
    open Wrapped
    end

    fun layout myNode =
        let
            val toLayout = case myNode of
                               PosInt n => Int.toString n
                             | Float x => Real.toString x
                             | String s => s
                             | NullVP => "nullVP"
        in
            Layout.str toLayout
        end

    end


    (* Recursive types *)

    (* foo_t represents Foo.t, foo_node represents Foo.node *)

    datatype type_node
      = Param of TyParam.t
      | LongId of LongTyId.t * TyArg.t list option
      | Offset of field_t * field list option
      | List of type_t list
      | Fun of type_t list * t list
      | Any
      | VProc
      | Cont of TyArg.t list option
      | Addr of type_t
         and dataconsdef_node
             = ConsDef of BomId.t * type_t option
         and field_node
             = Immutable of int * type_t
             | Mutable of int * type_t
         and fundef_node
             = Def of Attrs.t option * BomId.t * TyParam.t list option
                      * Param.t list option * Param.t list option * type_t * exp_t
         and varpat_node
             = Wild
             | Var of BomId.t * type_t option
         and caserule_node
             = LongRule of LongConId.t * varpat_t list * exp_t
           | LiteralRule Literal.t * exp_t
           | DefaultRule of varpat_t * exp_t
         and tycaserule_node
             = TyRule of type_t * exp_t
             | Default of exp_t
         and simpleexp_node
             = PrimOp of 'var Prim.prim * simpleexp_t list
             | AllocId of LongValueId.t * simpleexp_t list
             | AllocType of Type.t * simpleexp_t list
             | AtIndex of int * simpleexp_t * simpleexp_t option
             | TypeCast of Type.t * simpleexp_t
             | HostVproc
             | VpLoad of int * simpleexp_t
             | VpAddr of int * simpleexp_t
             | VpStore of int * simpleexp_t * simpleexp_t
             | Id of LongValueId.t
             | Lit of Literal.t
             | MLString of string
         and exp_node
             = Let of varpat_t list * rhs_t * exp_t
             | Do of simpleexp_t * exp_t
             | Fun of fundef_t list * exp_t
             | Cont of BomId.t * Param.t list option * exp_t * exp_t
             | If of simpleexp_t * exp_t * exp_t
             | Case of simpleexp_t * caserule_t list
             | Typecase of TyParam.t * tycaserule_t list
             | Apply of LongId.t * simpleexp_t list option * simpleexp_t list option
             | Throw of LongId.t * simpleexp_t list option
             | Return of simpleexp_t list option
  and rhs_node
      = Composite of exp
    | Simple of simpleexp_t
  withtype type_t = type_node Wrap.t
  and field_t = field_node  Wrap.t
  and fundef_t = fundef_node Wrap.t
  and varpat_t = varpat_node Wrap.t
  and caserule_t = caserule_node Wrap.t
  and tycaserule_t = tycaserule_node Wrap.t
  and simpleexp_t = simpleexp_node Wrap.t
  and exp_t = exp_node Wrap.t
  and rhs_t = rhs_node wrap.t

  local
      fun stubLayout s = Layout.str s
  in
  fun layoutType myNode = stubLayout "Type"
  and layoutDataConsDef myNode = stubLayout "DataConsDef"
  and layoutField myNode = stubLayout "Field"
  and layoutFunDef myNode = stubLayout "FunDef"
  and layoutVarPat myNode = stubLayout "VarPat"
  and layoutCaseRule myNode = stubLayout "CaseRule"
  and layoutTyCaseRule myNode = stubLayout "TyCaseRule"
  and layoutSimpleExp myNode = stubLayout "SimpleExp"
  and layoutExp myNode = stubLayout "Exp"
  and layoutRhs myNode = stubLayout "RHS"
  end


    structure Type = struct
    type t = type_t
    type field  = field_t
    datatype node = datatype type_node
    local
        structure Wrapped = DoPartialWrap(type node = node)
    in
    open Wrapped
    end

    val layout = layoutType

    end


    structure DataConsDef = struct
    type t = dataconsdef_t
    datatype node = datatype dataconsdef_node

    local
        structure Wrapped = DoPartialWrap(type node = node)
    in
    open Wrapped
    end

    val layout = layoutDataConsDef

    end

    structure Field = struct
    type t = field_t
    datatype node = datatype field_node

    local
        structure Wrapped = DoPartialWrap(type node = node)
    in
    open Wrapped
    end

    val layout = layoutField

    end

    structure FunDef = struct
    type t = fundef_t
    type exp = exp_t
    datatype node = datatype fundef_node

    local
        structure Wrapped = DoPartialWrap(type node = node)
    in
    open Wrapped
    end

    end

    structure VarPat = struct
    type t = varpat_t

    datatype node = datatype varpat_node

    local
        structure Wrapped = DoPartialWrap(type node = node)
    in
    open Wrapped
    end

    val layout = layoutVarPat

    end

    structure CaseRule = struct
    type t = caserule_t
    type exp = exp_t
    datatype node = datatype casserule_node

    local
        structure Wrapped = DoPartialWrap(type node = node)
    in
    open Wrapped
    end

    val layout = layoutCaseRule

    end

    structure TyCaseRule = struct
    type t = tycaserule_t
    type exp = exp_t
    datatype node = datatype tycaserule_node

    local
        structure Wrapped = DoPartialWrap(type node = node)
    in
    open Wrapped
    end

    val layout = layoutTyCaseRule

    end


    structure SimpleExp = struct
    type t = simpleexp_t
    datatype node = datatype simpleexp_node

    local
        structure Wrapped = DoPartialWrap(type node = node)
    in
    open Wrapped
    end

    val layout = layoutSimpleExp

    end

    structure Exp = struct
    type t = exp_t
    type rhs = rhs_t
    datatype node = datatype exp_node

    local
        structure Wrapped = DoPartialWrap(type node = node)
    in
    open Wrapped
    end

    val layout = layoutExp

    end

    structure RHS = struct
    type t = rhs_t
    type exp = exp_t
    datatype node = datatype rhs_node

    local
        structure Wrapped = DoPartialWrap(type node = node)
    in
    open Wrapped
    end

    val layout = layoutRhs

    end


    (* Non-recursive types, part 2 -- types that depend on recursive types *)
    structure DataTypeDef = struct
    datatype node
      = ConsDef of BomId.t * TyParam.t list option * DataConsDef.t list
      | SimpleDef of BomId.t * TyParam.t list option * LongTyId.t

    local
        structure Wrapped = DoWrap(type node = node)
    in
    open Wrapped
    end

    fun layout myNode =
        let
            fun layoutConsDef (bomId, maybeTyParams, dataConsDefs) =
                Layout.seq [BomId.layout bomId, layoutListOption maybeTyParams,
                            Layout.str "=", Layout.seq (map DataConsDef.layout dataConsDefs)]
            fun layoutSimpleDef (bomId, maybeTyParams, longId) =
                Layout.seq [BomId.layout bomId, layoutListOption maybeTyParams,
                            Layout.str "=", Layout.str "datatype", LongId.layout longId]
        in
            case myNode of
                ConsDef (bomId, maybeTyParams, dataConsDefs) =>
                layoutConsDef (bomId, maybeTyParams, dataConsDefs)
              | SimpleDef (bomId, maybeTyParams, longId) =>
                layoutSimpleDef (bomId, maybeTyParams, longId)
        end


    end

    structure Definition = struct
    datatype node
      = Extern of CReturnTy.t * BomId.t * CArgTy.t list * Attrs.t
      | Datatype of DatatypeDef.t * DataTypeDef.t list option
      | Type of BomId.t * TyParam.t list option * Type.t
      | DefineShortId of Attrs.t option * HLOpId.t *
                         TyParam.t list option * FunParam.t list *
                         Type.t * Exp.t option
      | DefineLongId of HLOpId.t * TyParam.t list option * LongValueId.t
      | Fun of FunDef.t * FunDef.t list option
	  | InstanceType of LongTyId.t * TyArg.t list
	  | Instance of LongValueId.t * TyArg.t list
    local
        structure Wrapped = DoWrap(type node = node)
    in
    open Wrapped
    end

    fun layout myNode = Layout.str "Definition"

    end


  end
