(* ast-bom.fun
 *
 * COPYRIGHT (c) 2013 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

functor AstBOM (S: AST_BOM_STRUCTS) : AST_BOM =
  struct
  open S

  structure Region = Region
  structure Wrap = Region.Wrap

  (* Helper Functions *)

  fun defaultIndent (toIndent : Layout.t) : Layout.t =
    let
      (* Following what I see in other files *)
      val defaultIndent = 3
    in
      Layout.indent (toIndent, defaultIndent)
    end

  fun defaultIndentAlign (toIndents : Layout.t list) =
    defaultIndent (Layout.align toIndents)

  fun defaultIndentMayAlign (toIndents : Layout.t list) =
    defaultIndent (Layout.mayAlign toIndents)

  fun layoutOption (opt : 'a option,
      doLayout : 'a -> Layout.t): Layout.t =
    if (Option.isSome opt) then
      doLayout (Option.valOf opt)
    else
      Layout.empty

  fun layoutOptions (opts : 'a list option,
      doLayouts : 'a list -> Layout.t list) : Layout.t list =
    if (Option.isSome opts) then
      doLayouts (Option.valOf opts)
    else
      [Layout.empty]

  local
    fun separateDelimited (els : Layout.t list, sep : string,
      leftDelim : string option, rightDelim : string option,
      doIndent : Layout.t list -> Layout.t) : Layout.t list =
    let
      val leftLayout : Layout.t = layoutOption (leftDelim, Layout.str)
      val rightLayout : Layout.t = layoutOption (rightDelim, Layout.str)
      val centerLayout : Layout.t =
        doIndent (Layout.separateRight (els, sep))
    in
      [leftLayout, centerLayout, rightLayout]
    end
  in
    fun delimitWithIndent (els, sep,
        leftDelim : string option,
        rightDelim : string option) : Layout.t =
      Layout.align (separateDelimited (els, sep, leftDelim, rightDelim,
        defaultIndentMayAlign))
    fun delimitWithIndent' (els, sep, leftDelim : string,
      rightDelim : string) : Layout.t =
        delimitWithIndent (els, sep, SOME leftDelim, SOME rightDelim)
    fun delimitNoIndent (els, sep,
        leftDelim : string option,
        rightDelim : string option) : Layout.t =
      Layout.mayAlign (separateDelimited (els, sep, leftDelim, rightDelim,
      Layout.mayAlign))
    fun rightDelimitWithIndent (els, sep, rightDelim) =
      delimitWithIndent (els, sep, NONE, SOME rightDelim)
    fun leftDelimitWithIndent (els, sep, leftDelim) =
      delimitWithIndent (els, sep, SOME leftDelim, NONE)
    fun indentedList els =
      delimitWithIndent (els, ",", SOME "[", SOME "]")
    fun indentedSchemeList els =
      delimitWithIndent (els, ",", SOME "(", SOME ")")
    fun unindentedSchemeList els =
      delimitNoIndent (els, ",", SOME "(", SOME ")")
    fun indentedSlashList (leftEls : Layout.t list,
        rightEls : Layout.t list) : Layout.t =
      Layout.align [
        (delimitWithIndent
          (leftEls, ",", SOME "(", NONE)),
        (delimitWithIndent
          (rightEls, ",", SOME "/", SOME ")"))
      ]
    fun unindentedSlashList (leftEls, rightEls) =
      Layout.mayAlign [
        (delimitNoIndent
          (leftEls, ",", SOME "(", NONE)),
        (delimitNoIndent
          (rightEls, ",", SOME "/", SOME ")"))
      ]
  end

  (* Structures *)

  (* Atoms *)
  structure BOMId = AstId (structure Symbol = Symbol)
  structure HLOpId = AstId (structure Symbol = Symbol)
  structure TyParam = AstId (structure Symbol = Symbol)
  structure SymbolicId = AstId (structure Symbol = Symbol)
  structure PrimOp = AstId(structure Symbol = Symbol)

  structure LongId = Longid (
    structure Id = BOMId
    structure Strid = BOMId
    structure Symbol = Symbol)

  structure HLOpQId = Longid (
    structure Id = HLOpId
    structure Strid = BOMId
    structure Symbol = Symbol)

    (* Non-recursive types, part 1 -- types that do not depend on recursive types *)

  structure Attrs = struct
  open Wrap
  datatype node = T of string list
  type t = node Wrap.t

  type node' = node
  type obj = t

  fun layout myNode  =
    let
      val T ss = node myNode
    in
      Layout.mayAlign [
        Layout.str "__attributes__",
        unindentedSchemeList (map Layout.str ss)
      ]
    end
  end


  structure TyParams = struct
  (* open Wrap *)
  (* datatype node *)
  (*   = T of TyParam.t list *)
  (* type t = node Wrap.t *)
  type t = TyParam.t list

  (* type node' = node *)
  (* type obj = t *)

  fun layout typs =
    (* let *)
    (*   val T typs = node myNode *)
    (* in *)
      delimitWithIndent (map TyParam.layout typs, ",", SOME "<", SOME ">")
    (* end *)
  end

  (* structure PrimOp = struct *)
  (*   open Wrap *)
  (*   datatype node *)
  (*     = T of CharVector.vector *)
  (*   type t = node Wrap.t *)

  (*   type node' = node *)
  (*   type obj = t *)

  (*   fun layout (myNode : t) = *)
  (*     let *)
  (*       val T s = node myNode *)
  (*     in *)
  (*       Layout.str s *)
  (*     end *)

  (* end *)

  structure BOMValueId = struct
    open Wrap
    datatype node
      = LongId of LongId.t
      | HLOpQId of HLOpQId.t
    type t = node Wrap.t

    type node' = node
    type obj = t

    fun layout (myNode) =
      case node myNode of
        LongId longId => LongId.layout longId
      | HLOpQId hlOpQId => HLOpQId.layout hlOpQId

  end

    (* structure LongId = struct *)
    (* datatype node = *)
    (*       Id of BOMId.t * TyArg.t list option *)
    (*       | QualifiedId of TyArg.t list option *)

    (* local *)
    (*  structure Wrapped = DoWrap(type node = node) *)
    (* in *)
    (* open Wrapped *)
    (* end *)

    (* fun layout (Id (bomId, maybeTyArgs)) = *)
    (*  Layout.seq [BOMId.layout bomId, layoutListOption (maybeTyArgs, TyArg.layout)] *)
    (*   | layout (QualifiedId (maybeTyArgs))  = layoutListOption maybeTyArgs *)

    (* end *)

    (* FIXME: this should use RawTypes.raw_ty *)
    structure RawTy = struct
        open Wrap
        (* datatype node = datatype RawTypes.raw_ty *)
        datatype node = datatype RawTypes.raw_ty
        type t = node Wrap.t

        type node' = node
        type obj = t

        (* fun toString (myNode : t) = RawTypes.toString (node myNode) *)
  fun toString myNode = ""

        fun layout myNode  = (Layout.str o toString) myNode

      end

    structure CArgTy = struct
    datatype node
      = Raw of RawTy.t
      | VoidPointer

    open Wrap
    type t = node Wrap.t
    type node' = node
    type obj = t

    fun layout myNode =
      case node myNode of
        Raw rawTy => RawTy.layout rawTy
      | VoidPointer => Layout.str "void*"

    end

    structure CReturnTy = struct
    open Wrap
    datatype node
      = CArg of CArgTy.t
      | Void

    type t = node Wrap.t
    type node' = node
    type obj = t

    fun layout myNode =
      case node myNode of
        CArg cArgTy => CArgTy.layout cArgTy
      | Void => Layout.str "void"
    end


    structure Literal = struct
    datatype node
      = PosInt of IntInf.int
      | Float of real
      | String of string
      | NullVP

    open Wrap
    type t = node Wrap.t
    type node' = node
    type obj = t

    fun layout myNode =
      Layout.str (
         case node myNode of
             PosInt n => IntInf.toString n
           | Float x => Real.toString x
           | String s => s
           | NullVP => "nullVP")

    end



    (* Recursive types *)

    (* foo_t represents Foo.t, foo_node represents Foo.node *)

    datatype type_node
      = Param of TyParam.t
      | TyCon of LongId.t * type_t list
      | Record of field_t list
      | Tuple of (bool * type_t) list
      | Array of type_t
      | Vector of type_t
      | Fun of type_t list * type_t list * type_t list
      | Any
      | VProc
      | Cont of type_t list
      | Addr of type_t
      | Raw of RawTy.t
      | Exn
      | BigNum
    and tyargs_node
      = ArgTypes of type_t list
    and dataconsdef_node
      = ConsDef of BOMId.t * type_t option
    and field_node
      = Immutable of IntInf.int * type_t
      | Mutable of IntInf.int * type_t
    and fundef_node
      = Def of Attrs.t option * BOMId.t * TyParam.t list
        * varpat_t list * varpat_t list * type_t list * exp_t
    and varpat_node
      = Wild of type_t option
      | Var of BOMId.t * type_t option
    and caserule_node
      = PatRule of LongId.t * varpat_t list * exp_t
      | LiteralRule of Literal.t * exp_t
    and tycaserule_node
      = TyRule of type_t * exp_t
      | Default of exp_t
    and simpleexp_node
      = PrimOp of PrimOp.t * simpleexp_t list
      | AllocId of LongId.t * simpleexp_t list
      | AllocType of type_t * simpleexp_t
      | Select of IntInf.int * simpleexp_t
      | Assign of IntInf.int * simpleexp_t * simpleexp_t
      | TypeCast of type_t * simpleexp_t
      | Promote of simpleexp_t
      | HostVproc
      | VpLoad of IntInf.int * simpleexp_t
      | VpAddr of IntInf.int * simpleexp_t
      | VpStore of IntInf.int * simpleexp_t * simpleexp_t
      | Id of LongId.t
      | Lit of Literal.t
      | MLString of IntInf.int vector
    and exp_node
      = Let of varpat_t list * rhs_t * exp_t
      | Do of simpleexp_t * exp_t
      | FunExp of fundef_t list * exp_t
      | ContExp of BOMId.t * varpat_t list * exp_t * exp_t
      | If of simpleexp_t * exp_t * exp_t
      | Case of simpleexp_t * caserule_t list
      | Typecase of TyParam.t * tycaserule_t list
      | Apply of LongId.t * simpleexp_t list * simpleexp_t list
      | Throw of BOMId.t * simpleexp_t list
      | Return of simpleexp_t list
    and rhs_node
      = Composite of exp_t
      | Simple of simpleexp_t

  withtype type_t = type_node Wrap.t
  and tyargs_t = type_t list (* tyargs_node Wrap.t *)
  and dataconsdef_t = dataconsdef_node Wrap.t
  and field_t = field_node  Wrap.t
  and fundef_t = fundef_node Wrap.t
  and varpat_t = varpat_node Wrap.t
  and caserule_t = caserule_node Wrap.t
  and tycaserule_t = tycaserule_node Wrap.t
  and simpleexp_t = simpleexp_node Wrap.t
  and exp_t = exp_node Wrap.t
  and rhs_t = rhs_node Wrap.t

  fun layoutType myNode =
    let
      fun layoutTupleField (false, ty) = layoutType ty
        | layoutTupleField (true, ty) = Layout.seq [Layout.str "!", layoutType ty]
      fun layoutTypes (ts : type_t list) : Layout.t list =
        map layoutType ts
      fun layoutTyArgOpts (maybeTyArgs : tyargs_t option) =
        layoutOption (maybeTyArgs, layoutTyArgs)
      fun layoutTyApp (tyc, ty) = Layout.seq [
	       Layout.str tyc, Layout.str "<", layoutType ty, Layout.str ">"
	    ]
    in
      case Wrap.node myNode
       of Param p => TyParam.layout p
        | TyCon (longId, maybeTyArgs) =>
          Layout.mayAlign [
            LongId.layout longId,
            layoutTyArgs maybeTyArgs
          ]
        | Record fields =>
          delimitWithIndent' (map layoutField fields, ",", "{", "}")
        | Tuple fields => delimitWithIndent' (map layoutTupleField fields, ",", "{", "}")
        | Fun (inputTys, exnTys, rangeTys)  =>
          let
            val layoutDomainTys = indentedSlashList (layoutTypes inputTys, layoutTypes exnTys)
            val layoutRangeTys = indentedSchemeList (layoutTypes rangeTys)
          in
            Layout.seq (
              Layout.separate ([layoutDomainTys, layoutRangeTys], "->"))
          end
        | Cont (maybeTyArgs) =>
          Layout.seq [
            Layout.str "cont",
            layoutTyArgs maybeTyArgs
          ]
        | Array myType => layoutTyApp ("array", myType)
        | Vector myType => layoutTyApp ("vector", myType)
        | Addr myType => layoutTyApp ("addr", myType)
        | BigNum => Layout.str "any"
        | Exn => Layout.str "bignum"
        | Any => Layout.str "exn"
        | VProc => Layout.str "vproc"
        | Raw (raw) => RawTy.layout raw
    end

  and layoutTyArgs types =
    (* let *)
    (*   val ArgTypes types = Wrap.node myNode *)
    (* in *)
      delimitWithIndent' (map layoutType types, ",", "<", ">")
    (* end *)

  and layoutDataConsDef myNode =
    let
      val ConsDef (bomId, maybeMyType) = Wrap.node myNode
      val ofType = if Option.isSome maybeMyType then
        Layout.seq [
          Layout.str "of ",
          layoutType (Option.valOf maybeMyType)
        ]
      else Layout.empty
    in
      Layout.seq [BOMId.layout bomId, ofType]
    end

  and layoutField myNode : Layout.t =
    let
      fun fieldWithSep (offset, myType, sep) =
        (Layout.mayAlign o Layout.separate) ([
          Layout.str (IntInf.toString offset),
          layoutType myType
        ], sep)
    in
      case Wrap.node myNode of
          Immutable (offset, myType) =>
            fieldWithSep (offset, myType, " : ")
        | Mutable (offset, myType) =>
            fieldWithSep (offset, myType, " ! ")
    end

  and layoutFunDef myNode : Layout.t =
    let
      val Def (maybeAttrs, bomId, maybeTyParams,
        inputParams, exnParams, returnTy, exp) = Wrap.node myNode
      fun layoutParams params = map layoutVarPat params
      val inputParamsLayout = layoutParams inputParams
      val exnParamsLayout = layoutParams exnParams
    in
      Layout.mayAlign [
        layoutOption (maybeAttrs, Attrs.layout),
        BOMId.layout bomId,
        TyParams.layout maybeTyParams,
        indentedSlashList (inputParamsLayout, exnParamsLayout),
        indentedSchemeList (map layoutType returnTy),
        Layout.str " = ",
        layoutExp exp
      ]
    end

  and layoutVarPat (myNode : varpat_t) : Layout.t =
    case Wrap.node myNode of
        Wild maybeType =>
          Layout.mayAlign [
            Layout.str "_",
            Layout.str "=>",
            unindentedSchemeList [
              Layout.str ":",
              layoutOption (maybeType, layoutType)
            ]
          ]
      | Var (bomId, NONE) => BOMId.layout bomId
      | Var (bomId, SOME myType) =>
          Layout.mayAlign [
            BOMId.layout bomId,
            Layout.str " : ",
            layoutType myType
          ]

  and layoutCaseRule (myNode : caserule_t) : Layout.t =
    let
      fun defaultFormat (leftEls : Layout.t list,
          rightEl : Layout.t) : Layout.t =
        Layout.mayAlign (leftEls@[Layout.str " => ", rightEl])
    in
      case Wrap.node myNode of
        PatRule (longId, varPats, exp) =>
            defaultFormat ([
               LongId.layout longId,
               indentedSchemeList (map layoutVarPat varPats)
            ], layoutExp exp)
        | LiteralRule (lit, exp) =>
            defaultFormat
              ([Literal.layout lit],
              layoutExp exp)
        (* | DefaultRule (varpat, exp) => *)
        (*     defaultFormat *)
        (*       ([layoutVarPat varpat], *)
        (*       layoutExp exp) *)
    end

  and layoutTyCaseRule myNode =
    case Wrap.node myNode of
        TyRule (myType, exp) =>
          Layout.mayAlign [
            layoutType myType,
            Layout.str " => ",
            layoutExp exp
          ]
      | Default exp =>
          Layout.mayAlign [
            Layout.str "_",
            Layout.str " => ",
            layoutExp exp
          ]

  and layoutSimpleExp (myNode : simpleexp_t) : Layout.t =
    let
      fun layoutSimpleExps simpleExps =
        map layoutSimpleExp simpleExps

      fun layoutVpOp (descStr, posInt, fromExp, maybeToExp) =
        Layout.mayAlign [
          Layout.str descStr,
          unindentedSchemeList ([
            Layout.str (IntInf.toString posInt),
            layoutSimpleExp fromExp
            ]@(if Option.isSome maybeToExp then
              [layoutSimpleExp (Option.valOf maybeToExp)]
            else
              []
            ))
          ]

    in
      case Wrap.node myNode of
          PrimOp (prim, simpleExps) =>
            Layout.mayAlign [
              PrimOp.layout prim,
              indentedSchemeList (layoutSimpleExps simpleExps)
            ]
        | AllocId (longId, simpleExps) =>
            Layout.mayAlign [
              Layout.str "alloc",
              LongId.layout longId,
              indentedSchemeList (layoutSimpleExps simpleExps)
            ]
        | AllocType (myTy, simpleExps) =>
            Layout.mayAlign [
              Layout.str "alloc",
              layoutType myTy,
              unindentedSchemeList ([layoutSimpleExp simpleExps])
            ]
        | Select (posInt, simpleExp) =>
            Layout.mayAlign [
              Layout.str ("#" ^ (IntInf.toString posInt)),
              unindentedSchemeList [layoutSimpleExp simpleExp]
            ]
        | Assign (posInt, simpleExp1, simpleExp2) =>
            Layout.mayAlign [
              Layout.str ("#" ^ (IntInf.toString posInt)),
              unindentedSchemeList [layoutSimpleExp simpleExp1],
              Layout.seq [Layout.str " := ", layoutSimpleExp simpleExp2]
            ]
        | TypeCast (myType, simpleExp) =>
            Layout.mayAlign [
              unindentedSchemeList [layoutType myType],
              layoutSimpleExp simpleExp
            ]
        | Promote (simpleExp) =>
            Layout.mayAlign [
              Layout.str "promote",
              unindentedSchemeList([layoutSimpleExp simpleExp])
            ]
        | HostVproc => Layout.str "host_vproc"
        | VpLoad (posInt, simpleExp) =>
            layoutVpOp ("vpload", posInt, simpleExp, NONE)
        | VpAddr (posInt, simpleExp) =>
            layoutVpOp ("vpaddr", posInt, simpleExp, NONE)
        | VpStore (posInt, fromExp, toExp) =>
            layoutVpOp ("vpstore", posInt, fromExp, SOME toExp)
        | Id longId => LongId.layout longId
        | Lit lit => Literal.layout lit
        | MLString s
            => Layout.vector (
              Vector.map (Layout.str o IntInf.toString) s)  (* ??? *)
    end

  and layoutExp myNode =
    case Wrap.node myNode of
        Let (varPats, rhs, exp) =>
          Layout.align [
            Layout.str "let",
            rightDelimitWithIndent (map layoutVarPat varPats, ",", "="),
            defaultIndent (layoutRhs rhs),
            layoutExp exp
          ]
      | Do (simpleExp, exp) =>
          Layout.align [
            Layout.str "do",
            defaultIndentAlign [layoutSimpleExp simpleExp, layoutExp exp]
          ]
      | FunExp (fundefs, exp) =>
          Layout.align [
            leftDelimitWithIndent (map layoutFunDef fundefs, "and", "fun"),
            layoutExp exp
          ]
      | ContExp (bomId, varPats, exp, exp') =>
          Layout.align [
            Layout.mayAlign [
              Layout.str "cont",
              BOMId.layout bomId,
              Layout.schemeList (map layoutVarPat varPats)
            ],
          Layout.mayAlign [
            Layout.str "=",
            layoutExp exp,
            layoutExp exp'
          ]
        ]
      | If (simpleExp, exp, exp') =>
          Layout.align [
            Layout.mayAlign [
              Layout.str "if",
              layoutSimpleExp simpleExp,
              Layout.str "then"
            ],
            layoutExp exp,
            Layout.str "else",
            layoutExp exp'
          ]
      | Case (simpleExp, caseRules) =>
          Layout.align [
            Layout.mayAlign [
              Layout.str "case",
              layoutSimpleExp simpleExp,
              Layout.str "of"
            ],
            rightDelimitWithIndent (
              map layoutCaseRule caseRules, " | ", "end")
          ]
      | Typecase (tyParam, tyCaseRules) =>
          Layout.align [
            Layout.mayAlign [
              Layout.str "typecase",
              TyParam.layout tyParam,
              Layout.str "of"
            ],
            rightDelimitWithIndent (
              map layoutTyCaseRule tyCaseRules, " | ", "end")
          ]
      | Apply (longId, maybeLeftArgs, maybeRightArgs) =>
          Layout.align [
            Layout.mayAlign [
              Layout.str "apply",
              LongId.layout longId
            ],
            indentedSlashList (
              map layoutSimpleExp maybeLeftArgs,
              map layoutSimpleExp maybeRightArgs)
          ]
      | Throw (bomId, maybeSimpleExps) =>
          Layout.align [
            Layout.mayAlign [
              Layout.str "throw",
              BOMId.layout bomId
            ],
            (* layoutOption (maybeTyArgs, layoutTyArgs), *)
            indentedSchemeList (map layoutSimpleExp maybeSimpleExps)
          ]
      | Return maybeSimpleExps =>
          Layout.align [
            Layout.str "return",
            indentedSchemeList (map layoutSimpleExp maybeSimpleExps)
          ]

  and layoutRhs myNode =
    case Wrap.node myNode of
        Composite exp => layoutExp exp
      | Simple simpleExp => layoutSimpleExp simpleExp


  structure BOMType = struct
  open Wrap
  datatype node = datatype type_node
  type t = node Wrap.t

  type node' = node
  type obj = t

  type field  = field_t
  (* type tyArgs = tyargs_t *)



  val layout = layoutType

  end


  structure TyArgs = struct
  open Wrap
  datatype node = datatype tyargs_node
  type t = node Wrap.t

  type node' = node
  type obj = t
  val layout = layoutTyArgs

  end

  structure DataConsDef = struct
  open Wrap
  datatype node = datatype dataconsdef_node
  type t = node Wrap.t

  type node' = node
  type obj = t

  val layout = layoutDataConsDef

  end

  structure Field = struct
  open Wrap
  datatype node = datatype field_node
  type t = node Wrap.t

  type node' = node
  type obj = t

  val layout = layoutField

  end

  structure FunDef = struct
  open Wrap
  datatype node = datatype fundef_node
  type t = node Wrap.t

  type node' = node
  type obj = t


  type exp = exp_t

  val layout = layoutFunDef

  end

  structure VarPat = struct
  open Wrap
  datatype node = datatype varpat_node
  type t = node Wrap.t

  type node' = node
  type obj = t

  val layout = layoutVarPat

  end

  structure CaseRule = struct
  open Wrap
  datatype node = datatype caserule_node
  type t = node Wrap.t

  type node' = node
  type obj = t

  type exp = exp_t

  val layout = layoutCaseRule

  (* fun isDefault c = *)
  (*   case node c of *)
  (*     DefaultRule c => true *)
  (*   | _ => false *)

  end

  structure TyCaseRule = struct
  open Wrap
  datatype node = datatype tycaserule_node
  type t = node Wrap.t

  type node' = node
  type obj = t

  type exp = exp_t

  val layout = layoutTyCaseRule

  fun isDefault c =
    case Region.Wrap.node c of
      Default c => true
    | _ => false

  end


  structure SimpleExp = struct
  open Wrap
  datatype node = datatype simpleexp_node
  type t = node Wrap.t

  type node' = node
  type obj = t

  val layout = layoutSimpleExp

  end

  structure Exp = struct
  open Wrap
  datatype node = datatype exp_node
  type t = node Wrap.t

  type node' = node
  type obj = t


  type rhs = rhs_t

  val layout = layoutExp

  end


  structure RHS = struct
  open Wrap
  datatype node = datatype rhs_node
  type t = node Wrap.t

  type node' = node
  type obj = t

  type exp = exp_t

  val layout = layoutRhs

  end


  (* Non-recursive types, part 2 -- types that depend on recursive types *)
  structure DataTypeDef = struct
  datatype node
    = ConsDefs of BOMId.t * TyParam.t list * DataConsDef.t list
    (* | SimpleDef of BOMId.t * TyParams.t  option * LongId.t *)

  open Wrap
  type t = node Wrap.t
  type node' = node
  type obj = t

  fun layout myNode =
    let
      fun layoutConsDef (bomId, maybeTyParams, dataConsDefs) =
        Layout.align [
          Layout.mayAlign [
            BOMId.layout bomId,
            TyParams.layout maybeTyParams
          ],
          leftDelimitWithIndent (
            map DataConsDef.layout dataConsDefs, "=", " | ")
        ]
      in
          case node myNode of
            ConsDefs (bomId, maybeTyParams, dataConsDefs) =>
              layoutConsDef (bomId, maybeTyParams, dataConsDefs)
      end
  end

  structure Definition = struct
  datatype node
    = Datatype of DataTypeDef.t list
    | TypeDefn of BOMId.t * TyParam.t list * BOMType.t
    | Exception of DataConsDef.t
    | DefineHLOp of Attrs.t option * HLOpId.t * TyParam.t list *
        VarPat.t list * VarPat.t list * BOMType.t list * Exp.t
    | Fun of FunDef.t list
    | Extern of CReturnTy.t * BOMId.t * CArgTy.t list * Attrs.t

  open Wrap
  type t = node Wrap.t
  type node' = node
  type obj = t

  fun layout myNode =
    case node myNode of
      Datatype dataTypeDefs =>
        leftDelimitWithIndent (
          map DataTypeDef.layout dataTypeDefs,
          "and",
          "datatype")
    | TypeDefn (bomId, maybeTyParams, myType) =>
        Layout.align [
          Layout.mayAlign [
            Layout.str "type",
            BOMId.layout bomId,
            TyParams.layout maybeTyParams,
            Layout.str "="
          ],
          BOMType.layout myType
        ]
    | Exception condef =>
        Layout.align [
          Layout.mayAlign [
            Layout.str "exception",
            DataConsDef.layout condef
          ]
        ]
    | DefineHLOp (maybeAttrs, hlOpId, maybeTyParams, inputPats, exnPats, bomTypes, exp)  =>
        let
          val layoutPats = map VarPat.layout
        in
            Layout.mayAlign ([
                Layout.str "define",
                layoutOption (maybeAttrs, Attrs.layout),
                HLOpId.layout hlOpId,
                TyParams.layout maybeTyParams,
                indentedSlashList (layoutPats inputPats, layoutPats exnPats),
                unindentedSchemeList (map BOMType.layout bomTypes)
              ] @ [leftDelimitWithIndent ([Exp.layout exp], "", "=")])
        end
    | Fun (fundefs) =>
        leftDelimitWithIndent (map FunDef.layout fundefs, "fun", "and")
    | Extern (cReturnTy, bomId, cArgTys, attrs) =>
        Layout.align [
          Layout.mayAlign [
            Layout.str "extern",
            CReturnTy.layout cReturnTy,
            BOMId.layout bomId
          ],
          indentedSchemeList (map CArgTy.layout cArgTys),
          Attrs.layout attrs
        ]
    end

  structure PrimConDef = struct
      datatype node
	= T of Con.t * Type.t option * BOMId.t

      open Wrap
      type t = node Wrap.t
      type node' = node
      type obj = t
    end

  structure ImportCon = struct
      datatype node
	= T of Con.t * Type.t option * BOMId.t option

      open Wrap
      type t = node Wrap.t
      type node' = node
      type obj = t
    end

  structure Import = struct
      datatype node
	= Datatype of Type.t vector * Longtycon.t * BOMId.t option * ImportCon.t list
	| Exn of Longcon.t * Type.t option * BOMId.t option
	| Val of Longvid.t * Type.t * BOMId.t option

     open Wrap
     type t = node Wrap.t
     type node' = node
     type obj = t

    end

end
