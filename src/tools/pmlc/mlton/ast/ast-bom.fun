(* ast-bom.fun
 *
 * COPYRIGHT (c) 2013 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

functor AstBOM (S: AST_BOM_STRUCTS) : AST_BOM =
  struct
  open S

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
    fun delimitNoIndent (els, sep,
        leftDelim : string option,
        rightDelim : string option) : Layout.t =
      Layout.mayAlign (separateDelimited (els, sep, leftDelim, rightDelim,
      Layout.mayAlign))
    fun rightDelimitWithIndent (els, sep, rightDelim) =
      separateDelimited (els, sep, NONE, SOME rightDelim, defaultIndentMayAlign)
    fun leftDelimitWithIndent (els, sep, leftDelim) =
      separateDelimited (els, sep, SOME leftDelim, NONE, defaultIndentMayAlign)
    fun indentedList els =
      delimitWithIndent (els, ",", SOME "[", SOME "]")
    fun indentedSchemeList els =
      delimitWithIndent (els, ",", SOME "(", SOME ")")
    fun unindentedSchemeList els =
      delimitNoIndent (els, ",", SOME "(", SOME ")")
    fun indentedSlashList (leftEls : Layout.t list, rightEls : Layout.t list) =
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
    structure BomId = AstId (structure Symbol = Symbol)
    structure HLOpId = AstId (structure Symbol = Symbol)
    structure TyParam = AstId (structure Symbol = Symbol)
    (* structure Param = AstId (structure Symbol = Symbol) *)
    (* structure FunParam = AstId (structure Symbol = Symbol) *)
    structure LongTyId = Longid (
      structure Id = BomId
      structure Strid = Strid
      structure Symbol = Symbol)
    structure LongConId = Longid (
      structure Id = BomId
      structure Strid = Strid
      structure Symbol = Symbol)
    structure LongValueId = Longid (
      structure Id = BomId;
      structure Strid = Strid;
      structure Symbol = Symbol)

    structure PrimTycons = PrimTycons (
      structure AdmitsEquality = AdmitsEquality()
      structure CharSize = CharSize()
      structure IntSize = IntSize()
      structure Kind = TyconKind()
      structure RealSize = RealSize()
      structure WordSize = WordSize()
        open BomId
        fun fromString s =
          BomId.fromSymbol (Symbol.fromString s, Region.bogus))
    (* following ast/ast-atoms.fun *)


    (* Non-recursive types, part 1 -- types that do not depend on recursive types *)

    structure Attrs = struct
    open Wrap
    datatype node = T of string list
    type t = node Wrap.t

    type node' = node
    type obj = t

    fun layout node (T ss)  =
        Layout.mayAlign [
          Layout.str "__attributes__",
          unindentedSchemeList (map Layout.str ss)
        ]
    end


  structure TyParams = struct
  open Wrap
  datatype node
    = T of TyParam.t list
  type t = node Wrap.t

  type node' = node
  type obj = t

  fun layout node (T tyParams) =
    delimitWithIndent (map TyParam.layout tyParams, ",", SOME "<", SOME ">")
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
    open Wrap
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
    type t = node Wrap.t

    type node' = node
    type obj = t


    fun toString (myNode : t) =
      case node myNode of
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

    fun layout myNode  =
      (Layout.str o toString) myNode

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
        ((Raw rawTy) : node) => (RawTy.layout rawTy)
      | (VoidPointer : node) => Layout.str "void*"

    end

    structure CReturnTy = struct
    datatype node
      = CArg of CArgTy.t
      | Void


    open Wrap
    type t = node Wrap.t
    type node' = node
    type obj = t

    fun layout myNode =
      case node myNode of
        CArg (cArgTy) => (CArgTy.layout cArgTy )
      | Void => (Layout.str "void")
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

    (* fun layout myNode = *)
    (*     let *)
    (*         val toLayout = case myNode *)
    (*           of PosInt n => Int.toString n *)
    (*           | Float x => Real.toString x *)
    (*           | String s => s *)
    (*           | NullVP => "nullVP" *)
    (*     in *)
    (*         Layout.str toLayout *)
    (*     end *)

    end


    (* Recursive types *)

    (* foo_t represents Foo.t, foo_node represents Foo.node *)

    datatype type_node
      = Param of TyParam.t
      | LongId of LongTyId.t * tyargs_t option
      | Record of field_t list
      | Tuple of type_t list
      | Fun of type_t list * type_t list option * type_t list
      | Any
      | VProc
      | Cont of tyargs_t option
      | Addr of type_t
    and tyargs_node
      = ArgTypes of type_t list
    and dataconsdef_node
      = ConsDef of BomId.t * type_t option
    and field_node
      = Immutable of IntInf.int * type_t
      | Mutable of IntInf.int * type_t
    and fundef_node
      = Def of Attrs.t option * BomId.t * TyParams.t option
        * varpat_t list option * varpat_t list option * type_t list option * exp_t
    and varpat_node
      = Wild of type_t option
      | Var of BomId.t * type_t option
    and caserule_node
      = LongRule of LongConId.t * varpat_t list option * exp_t
      | LiteralRule of Literal.t * exp_t
      | DefaultRule of varpat_t * exp_t
    and tycaserule_node
      = TyRule of type_t * exp_t
      | Default of exp_t
    and simpleexp_node
      = PrimOp of PrimTycons.tycon * simpleexp_t list
      | AllocId of LongValueId.t * simpleexp_t list
      | AllocType of tyargs_t * simpleexp_t list
      | AtIndex of IntInf.int * simpleexp_t * simpleexp_t option
      | TypeCast of type_t * simpleexp_t
      | HostVproc
      | VpLoad of IntInf.int * simpleexp_t
      | VpAddr of IntInf.int * simpleexp_t
      | VpStore of IntInf.int * simpleexp_t * simpleexp_t
      | Id of LongValueId.t
      | Lit of Literal.t
      | MLString of IntInf.int vector
    and exp_node
      = Let of varpat_t list * rhs_t * exp_t
      | Do of simpleexp_t * exp_t
      | FunExp of fundef_t list * exp_t
      | ContExp of BomId.t * varpat_t list option * exp_t * exp_t
      | If of simpleexp_t * exp_t * exp_t
      | Case of simpleexp_t * caserule_t list
      | Typecase of TyParam.t * tycaserule_t list
      | Apply of LongValueId.t * simpleexp_t list option * simpleexp_t list option
      | Throw of BomId.t * tyargs_t option * simpleexp_t list option
      | Return of simpleexp_t list option
    and rhs_node
      = Composite of exp_t
      | Simple of simpleexp_t

  withtype type_t = type_node Wrap.t
  and tyargs_t = tyargs_node Wrap.t
  and dataconsdef_t = dataconsdef_node Wrap.t
  and field_t = field_node  Wrap.t
  and fundef_t = fundef_node Wrap.t
  and varpat_t = varpat_node Wrap.t
  and caserule_t = caserule_node Wrap.t
  and tycaserule_t = tycaserule_node Wrap.t
  and simpleexp_t = simpleexp_node Wrap.t
  and exp_t = exp_node Wrap.t
  and rhs_t = rhs_node Wrap.t

  (* fun layoutType myNode = *)
  (*   let *)
  (*     fun layoutTypes (ts : type_node list) = *)
  (*       map layoutType ts *)
  (*     fun getDefault (types : type_t list option) = layoutOptions (types, layoutTypes) *)
  (*     fun layoutTyArgOpts (maybeTyArgs) = *)
  (*       layoutOption (maybeTyArgs, layoutTyArgs) *)
  (*   in *)
  (*     case type_node myNode of *)
  (*         Param p  => TyParam.layout p *)
  (*       | LongId (longTyId, maybeTyArgs) => *)
  (*         Layout.mayAlign [ *)
  (*           LongTyId.layout longTyId, *)
  (*           layoutOption (maybeTyArgs, TyArgs.layout) *)
  (*         ] *)
  (*       | Record (fields) => *)
  (*         delimitWithIndent (map layoutField fields, ",", "{", "}") *)
  (*       | Tuple (types) => *)
  (*         indentedList (layoutTypes types) *)
  (*       | Fun (inputTys, exnTys, rangeTys)  => *)
  (*         let *)
  (*           val layoutDomainTys = Layout.align *)
  (*             (indentedSlashList (getDefault inputTys, getDefault exnTys)) *)
  (*           val layoutRangeTys = Layout.align *)
  (*             (indentedSchemeList (getDefault rangeTs)) *)
  (*         in *)
  (*           Layout.seq (Layout.separate ([layoutDomainTys, layoutRangeTys], "->")) *)
  (*         end *)
  (*       | Any => Layout.str "any" *)
  (*       | VProc => Layout.str "vproc" *)
  (*       | Cont (maybeTyArgs) => *)
  (*         Layout.seq ["cont", layoutOption (maybeTyArgs, layoutTyArgs)] *)
  (*       | Addr (myType) => Layout.seq ["addr <", layoutType myType, ">"] *)
  (*   end *)

  (* and layoutTyArgs tyargs_node (ArgTypes types) = *)
  (*   delimitWithIndent (map layoutType types, ",", "<", ">") *)

  (* and layoutDataConsDef dataconsdef_node (ConsDef (bomId, maybeMyType)) = *)
  (*   let *)
  (*     val ofType = if Option.isSome maybeMyType then *)
  (*       Layout.seq ["of ", layoutType (Option.getVal maybeMyType)] *)
  (*     else Layout.empty *)
  (*   in *)
  (*     Layout.seq [BomId.layout bomId, ofType] *)
  (*   end *)

  (* and layoutField myNode = *)
  (*   let *)
  (*     fun fieldWithSep (offset, myType, sep) = *)
  (*       Layout.separate ([Int.toString offset, layoutType myType], sep) *)
  (*   in *)
  (*     case field_node myNode of *)
  (*         Immutable (offset, myType) => *)
  (*           fieldWithSep (offset, myType, " : ") *)
  (*       | Mutable (offset, myType) => *)
  (*           fieldWithSep (offset, myType, " ! ") *)
  (*   end *)

  (* and layoutFunDef fundef_node (Def (attrs, bomId, maybeTyParams, *)
  (*     maybeInputParams, maybeExnParams, returnTy, exp)) = *)
  (*   let *)
  (*     fun maybeLayoutParams (maybeParams) = layoutOptions (maybeParams, Param.layout) *)
  (*     val inputParamsLayout = maybeLayoutParams maybeInputParams *)
  (*     val exnParamsLayout = maybeLayoutParams maybeExnParams *)
  (*   in *)
  (*     Layout.mayAlign [ *)
  (*       Attrs.layout attrs, *)
  (*       BomId.layout bomId, *)
  (*       layoutOption (maybeTyParams, TyParams.layout), *)
  (*       indentedSlashList (inputParamsLayout, exnParamsLayout), *)
  (*       layoutType returnTy, *)
  (*       Layout.str " = ", *)
  (*       layoutExp exp *)
  (*     ] *)
  (*   end *)

  (* and layoutVarPat myNode = *)
  (*   case varpat_node myNode of *)
  (*       Wild => Layout.str "_" *)
  (*     | Var (bomId, NONE) => BomId.layout bomId *)
  (*     | Var (bomId, SOME myType) => *)
  (*         Layout.mayAlign [ *)
  (*           BomId.layout bomId, *)
  (*           Layout.str " : ", *)
  (*           layoutType myType *)
  (*         ] *)

  (* and layoutCaseRule myNode = *)
  (*   let *)
  (*     fun defaultFormat (leftEls, rightEl) = *)
  (*       Layout.mayAlign leftEls::[Layout.str " => ", rightEl] *)
  (*   in *)
  (*     case caserule_node myNode of *)
  (*       LongRule (longConId, varPats, exp) => *)
  (*           defaultFormat *)
  (*             ([LongConId.layout longConId, *)
  (*             indentedSchemeList (map layoutVarPat varPats)], *)
  (*             layoutExp exp) *)
  (*       | LiteralRule (lit, exp) => *)
  (*           defaultFormat *)
  (*             ([Literal.layout lit], *)
  (*             layoutExp exp) *)
  (*       | DefaultRule (varpat, exp) => *)
  (*           defaultFormat *)
  (*             ([layoutVarPat varpat], *)
  (*             layoutExp exp) *)
  (*   end *)

  (* and layoutTyCaseRule myNode = *)
  (*   case tycaserule_node myNode of *)
  (*       tyRule (myType, exp) => *)
  (*         Layout.mayAlign [ *)
  (*           layoutType myType, *)
  (*           Layout.str " => ", *)
  (*           layoutExp exp *)
  (*         ] *)
  (*     | Default exp => *)
  (*         Layout.mayAlign [ *)
  (*           Layout.str "_", *)
  (*           Layout.str " => ", *)
  (*           layoutExp exp *)
  (*         ] *)

  (* and layoutSimpleExp myNode = *)
  (*   let *)
  (*     fun layoutSimpleExps simpleExps = *)
  (*       map layoutSimpleExp simpleExps *)

  (*     fun layoutVpOp (descStr, posInt, fromExp, maybeToExp) = *)
  (*       Layout.mayAlign [ *)
  (*         Layout.str descStr, *)
  (*         unindentedSchemeList [ *)
  (*           Layout.str (Int.toString posInt), *)
  (*           layoutSimpleExp fromExp *)
  (*           ]@(if Option.isSome maybeToExp then *)
  (*             [layoutSimpleExp (Option.valOf maybeToExp)] *)
  (*           else *)
  (*             [] *)
  (*           ) *)
  (*         ] *)

  (*   in *)
  (*     case simpleexp_node myNode of *)
  (*         PrimOp (Prim.prim var', simpleExps) => *)
  (*           Layout.mayAlign [ *)
  (*             Prim.layout var', *)
  (*             indentedSchemeList (layoutSimpleExps simpleExps) *)
  (*           ] *)
  (*       | AllocId (longValueId, simpleExps) => *)
  (*           Layout.mayAlign [ *)
  (*             Layout.str "alloc", *)
  (*             LongValueId.layout longValueId, *)
  (*             indentedSchemeList (layoutSimpleExps simpleExps) *)
  (*           ] *)
  (*       | AllocType (myType, simpleExps) => *)
  (*           Layout.mayAlign [ *)
  (*             Layout.str "alloc", *)
  (*             layoutType myType, *)
  (*             indentedSchemeList (layoutSimpleExps simpleExps) *)
  (*           ] *)
  (*       | AtIndex (posInt, simpleExp, maybeSimpleExp) => *)
  (*           Layout.mayAlign [ *)
  (*             Layout.str ("#" ^ (Int.toString posInt)), *)
  (*             unindentedSchemeList [layoutSimpleExp simpleExp], *)
  (*             if Option.isSome maybeSimpleExp then *)
  (*               Layout.seq [Layout.str " := ", *)
  (*                 layoutSimpleExp (Option.valOf simpleExp) *)
  (*               ] *)
  (*             else *)
  (*               Layout.empty *)
  (*           ] *)
  (*       | TypeCast (myType, simpleExp) => *)
  (*           Layout.mayAlign [ *)
  (*             unindentedSchemeList [layoutType myType], *)
  (*             layoutSimpleExp simpleExp *)
  (*           ] *)
  (*       | HostVProc => Layout.str "host_vproc" *)
  (*       | VPload (posInt, simpleExp) => *)
  (*           layoutVpOp ("vpload", posInt, simpleExp, NONE) *)
  (*       | VPAddr (posInt, simpleExp) => *)
  (*           layoutVpOp ("vpaddr", posInt, simpleExp, NONE) *)
  (*       | VPStore (posInt, fromExp, maybeToExp) => *)
  (*           layoutVpOp ("vpstore", posInt, fromExp, maybeToExp) *)
  (*       | Id longValueId => LongValueId.layout longValueId *)
  (*       | Lit lit => Literal.layout lit *)
  (*       | MLString s => Layout.str s *)
  (*   end *)

  (* and layoutExp myNode = *)
  (*   case exp_node myNode of *)
  (*       Let (varPats, rhs, exp) => *)
  (*         Layout.align [ *)
  (*           Layout.str "let", *)
  (*           Layout.mayAlign *)
  (*             rightDelimitWithIndent (map layoutVarPat varPats, ",", "="), *)
  (*           doIndent (layoutRhs rhs), *)
  (*           layoutExp exp *)
  (*         ] *)
  (*     | Do (simpleExp, exp) => *)
  (*         Layout.align [ *)
  (*           Layout.str "do", *)
  (*           defaultIndentAlign [layoutSimpleExp simpleExp, layoutExp exp] *)
  (*         ] *)
  (*     | Fun (fundefs, exp) => *)
  (*         Layout.align [ *)
  (*           leftDelimitWithIdent (map layoutFunDef fundefs, "and", "fun"), *)
  (*           layoutExp exp *)
  (*         ] *)
  (*     | If (simpleExp, exp, exp') => *)
  (*         Layout.align [ *)
  (*           Layout.mayAlign [Layout.str "if", layoutSimpleExp simpleExp, "then"], *)
  (*           layoutExp exp, *)
  (*           "else", *)
  (*           layoutExp exp' *)
  (*         ] *)
  (*     | Case (simpleExp, caseRules) => *)
  (*         Layout.align [ *)
  (*           Layout.mayAlign [ *)
  (*             Layout.str "case", *)
  (*             layoutSimpleExp simpleExp, *)
  (*             Layout.str "of" *)
  (*           ], *)
  (*           rightDelimitWithIndent (map layoutCaseRule caseRules, " | ", "end") *)
  (*         ] *)
  (*     | TypeCase (tyParam, tyCaseRules) => *)
  (*         Layout.align [ *)
  (*           Layout.mayAlign [ *)
  (*             Layout.str "typecase", *)
  (*             TyParam.layout tyParam, *)
  (*             Layout.str "of" *)
  (*           ], *)
  (*           rightDelimitWithIndent (map layoutTyCaseRule tyCaseRules, " | ", "end") *)
  (*         ] *)
  (*     | Apply (longValueId, maybeLeftArgs, maybeRightArgs) => *)
  (*         Layout.align [ *)
  (*           Layout.mayAlign [ *)
  (*             Layout.str "apply", *)
  (*             LongValueId.layout longValueId *)
  (*           ], *)
  (*           indentedSlashList *)
  (*             (layoutOptions (maybeLeftArgs, map layoutSimpleExp), *)
  (*             layoutOptions (maybeRightArgs, map layoutSimpleExp)) *)
  (*         ] *)
  (*     | Throw (bomId, maybeTyArgs, maybeSimpleExps) => *)
  (*         Layout.align [ *)
  (*           Layout.mayAlign [ *)
  (*             Layout.str "throw", *)
  (*             BomId.layout bomId *)
  (*           ], *)
  (*           layoutOption (maybeTyArgs, layoutTyArgs), *)
  (*           indentedSchemeList (layoutOptions (maybeSimpleExps, map layoutSimpleExp)) *)
  (*         ] *)
  (*     | return maybeSimpleExps => *)
  (*         Layout.align [ *)
  (*           Layout.str "return", *)
  (*           indentedSchemeList (layoutOptions (maybeSimpleExps, map layoutSimpleExp)) *)
  (*         ] *)

  (* and layoutRhs myNode = *)
  (*   case rhs_node myNode of *)
  (*       Composite exp => layoutExp exp *)
  (*     | Simple simpleExp => layoutSimpleExp simpleExp *)


  structure BomType = struct
  open Wrap
  datatype node = datatype type_node
  type t = node Wrap.t

  type node' = node
  type obj = t

  type field  = field_t
  type tyArgs = tyargs_t



  (* val layout = layoutType *)

  end


  structure TyArgs = struct
  open Wrap
  datatype node = datatype tyargs_node
  type t = node Wrap.t

  type node' = node
  type obj = t
  (* val layout = layoutTyArgs *)

  end

  structure DataConsDef = struct
  open Wrap
  datatype node = datatype dataconsdef_node
  type t = node Wrap.t

  type node' = node
  type obj = t

  (* val layout = layoutDataConsDef *)

  end

  structure Field = struct
  open Wrap
  datatype node = datatype field_node
  type t = node Wrap.t

  type node' = node
  type obj = t

  (* val layout = layoutField *)

  end

  structure FunDef = struct
  open Wrap
  datatype node = datatype fundef_node
  type t = node Wrap.t

  type node' = node
  type obj = t


  type exp = exp_t



  (* val layout = layoutFunDef *)

  end

  structure VarPat = struct
  open Wrap
  datatype node = datatype varpat_node
  type t = node Wrap.t

  type node' = node
  type obj = t

  (* val layout = layoutVarPat *)

  end

  structure CaseRule = struct
  open Wrap
  datatype node = datatype caserule_node
  type t = node Wrap.t

  type node' = node
  type obj = t

  type exp = exp_t

  (* val layout = layoutCaseRule *)

  end

  structure TyCaseRule = struct
  open Wrap
  datatype node = datatype tycaserule_node
  type t = node Wrap.t

  type node' = node
  type obj = t

  type exp = exp_t

  (* val layout = layoutTyCaseRule *)

  end


  structure SimpleExp = struct
  open Wrap
  datatype node = datatype simpleexp_node
  type t = node Wrap.t

  type node' = node
  type obj = t

  (* val layout = layoutSimpleExp *)

  end

  structure Exp = struct
  open Wrap
  datatype node = datatype exp_node
  type t = node Wrap.t

  type node' = node
  type obj = t


  type rhs = rhs_t

  (* val layout = layoutExp *)

  end


  structure RHS = struct
  open Wrap
  datatype node = datatype rhs_node
  type t = node Wrap.t

  type node' = node
  type obj = t

  type exp = exp_t

  (* val layout = layoutRhs *)

  end


  (* Non-recursive types, part 2 -- types that depend on recursive types *)
  structure DataTypeDef = struct
  datatype node
    = ConsDefs of BomId.t * TyParams.t option * DataConsDef.t list
    | SimpleDef of BomId.t * TyParams.t  option * LongTyId.t

  open Wrap
  type t = node Wrap.t
  type node' = node
  type obj = t

  (* fun layout myNode = *)
  (*   let *)
  (*     fun layoutConsDef (bomId, maybeTyParams, dataConsDefs) = *)
  (*       Layout.align [ *)
  (*         Layout.mayAlign [ *)
  (*           BomId.layout bomId, *)
  (*           layoutOption (maybeTyParams, TyParams.layout) *)
  (*         ], *)
  (*         leftDelimitWithIndent *)
  (*           (map DataConsDeflayout dataConsDefs, "=", " | ") *)
  (*       ] *)

  (*       fun layoutSimpleDef (bomId, maybeTyParams, longId) = *)
  (*           Layout.align [ *)
  (*             Layout.mayAlign [ *)
  (*               BomId.layout bomId, *)
  (*               layoutOption (maybeTyParams, TyParams.layout) *)
  (*             ], *)
  (*             Layout.mayAlign [ *)
  (*               Layout.str "datatype", *)
  (*               LongId.layout longId *)
  (*             ] *)
  (*           ] *)
  (*     in *)
  (*         case node myNode of *)
  (*           ConsDef (bomId, maybeTyParams, dataConsDefs) => *)
  (*             layoutConsDef (bomId, maybeTyParams, dataConsDefs) *)
  (*         | SimpleDef (bomId, maybeTyParams, longId) => *)
  (*             layoutSimpleDef (bomId, maybeTyParams, longId) *)
  (*     end *)
  end

  structure Definition = struct
  datatype node
    = Extern of CReturnTy.t * BomId.t * CArgTy.t list * Attrs.t
    | Datatype of DataTypeDef.t * DataTypeDef.t list option
    | TypeDefn of BomId.t * TyParams.t option * BomType.t
    | DefineShortId of Attrs.t option * HLOpId.t *
        TyParams.t option * VarPat.t list option * VarPat.t list option *
        BomType.t list option * Exp.t option
    | DefineLongId of HLOpId.t * TyParams.t option * LongValueId.t
    | Fun of FunDef.t list
    | InstanceType of LongTyId.t * TyArgs.t
    | Instance of LongValueId.t * TyArgs.t

  open Wrap
  type t = node Wrap.t
  type node' = node
  type obj = t

  (* fun layout myNode = *)
  (*   case node myNode of *)
  (*     Extern (cReturnTy, bomId, cArgTys, attrs) => *)
  (*       Layout.align [ *)
  (*         Layout.mayAlign [ *)
  (*           Layout.str "extern", *)
  (*           CReturnTy.layout cReturnTy, *)
  (*           BomId.layout bomId *)
  (*         ], *)
  (*         indentedSchemeList (map CArgTy.layout cArgTys), *)
  (*         Attrs.layout attrs *)
  (*       ] *)
  (*   | Datatype (datatypeDef, maybeDatatypeDefs) => *)
  (*       leftDelimitWithIndent *)
  (*         (map DatatypeDef.layout *)
  (*           datatypeDef::(Option.getOpt (maybeDatatypeDefs, [])), *)
  (*         "and", *)
  (*         "datatype") *)
  (*   | TypeDefn (bomId, maybeTyParams, myType) => *)
  (*       Layout.align [ *)
  (*         Layout.mayAlign [ *)
  (*           Layout.str "type", *)
  (*           BomId.layout bomId, *)
  (*           layoutOption (maybeTyParams, TyParams.layout), *)
  (*           Layout.str "=" *)
  (*         ], *)
  (*         Type.layout myType *)
  (*       ] *)
  (*   | DefineShortId (maybeAttrs, hlOpId, maybeTyParams, funParams, *)
  (*         myType, maybeExp)  => *)
  (*       Layout.align *)
  (*           (Layout.mayAlign [ *)
  (*             Layout.str "define", *)
  (*             layoutOption (maybeAttrs, Attrs.layout), *)
  (*             HLOpId.layout hlOpId, *)
  (*             layoutOption (maybeTyParams, TyParams.layout), *)
  (*             map FunParams.layout funParams, *)
  (*             Type.layout myType *)
  (*           ])::(if Option.isSome maybeExp then *)
  (*             [leftDelimitWithIdent *)
  (*               ([Exp.layout (Option.valOf maybeExp)], "", "=")] *)
  (*           else *)
  (*             []) *)
  (*   | DefineLongId (hlOpId, maybeTyParams, longValueId) => *)
  (*       leftDelimitWithIdent ([ *)
  (*         Layout.seq [ *)
  (*           Layout.str "=", *)
  (*           layoutOption (maybeTyParams, TyParams.layout) *)
  (*         ], *)
  (*         LongValueId.layout longValueId *)
  (*       ], *)
  (*       "define", *)
  (*       "") *)
  (*   | Fun (fundefs) => *)
  (*       leftDelimitWithIdent ([map FunDef.layout fundefs], "fun", "and") *)
  (*   | InstanceType (longTyId, tyargs) => *)
  (*       Layout.mayAlign [ *)
  (*         Layout.str "instance type", *)
  (*         LongTyId.layout longTyId, *)
  (*         TyArgs.layout tyargs *)
  (*       ] *)
  (*   | Instance (longValueId, tyargs) => *)
  (*       Layout.mayAlign [ *)
  (*         Layout.str "instance", *)
  (*         LongValueId.layout longValueId, *)
  (*         TyArgs.layout tyargs *)
  (*       ] *)
    end


end
