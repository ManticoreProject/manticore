functor CoreBOM (S: CORE_BOM_STRUCTS) : CORE_BOM = struct
  open S

  structure AstBOM = Ast.AstBOM



  local
    open Region
  in
  (* need this to unwrap regions since they're opaque *)
    fun regionToRecord (region: Region.t):
        {left: SourcePos.t, right: SourcePos.t} = let
      fun fromOpt maybeRegion =
        Option.getOpt (maybeRegion, SourcePos.bogus)
    in
      {left = fromOpt (left region),
      right = fromOpt (right region)}
    end

    fun keepRegion (f: 'a -> 'b, wrapped: 'a Wrap.t): 'b Wrap.t =
      Wrap.makeRegion ((f o Wrap.node) wrapped, Wrap.region wrapped)
  end


  structure BomId = struct
    open AstBOM.BomId
  end

  structure HLOpId = struct
  end

  structure BomValueId = struct
  end


  structure TyParam = struct
    open Ast.Tyvar

    fun fromAst (tyParam : AstBOM.TyParam.t) = let
        (* val asNode = AstBOM.TyParam.node tyParam *)
        val asRegion = regionToRecord (AstBOM.TyParam.region tyParam)
    in
        newString (AstBOM.TyParam.toString tyParam, asRegion)
    end
  end

  structure RawTy = struct
    open AstBOM.RawTy

    fun fromAst myRawTy = myRawTy
  end

  structure PrimOp = struct
  end

  structure LongTyId = struct
    open AstBOM.LongTyId
  end

  structure LongConId = struct
  end

  structure LongValueId = struct
    open AstBOM.LongValueId
  end

  structure HLOpQId = struct
  end

  structure SymbolicId = struct
  end

  structure Attrs = struct
  end

  (* Mutually recursive types *)
  datatype type_node
    = Param of TyParam.t
    | MLType of type_t
    | Record of field_t list
    | Tuple of type_t list
    | Fun of type_t list * type_t list * type_t list
    | Any
    | VProc
    | Cont of type_t list
    | Addr of type_t
    | Raw of RawTy.t
  and field_node
    = Immutable of IntInf.int * type_t
    | Mutable of IntInf.int * type_t
  and tyargs_node
    = ArgTypes of type_t list

  withtype type_t = type_node Region.Wrap.t
  and field_t = field_node Region.Wrap.t
  and tyargs_t = tyargs_node Region.Wrap.t

  (* Functions over mutually recursive types *)
  local
      (* define some synonyms so we don't end up with painfully long
      datatype names *)
    structure AstTy = AstBOM.BomType
  in
  fun typeFromAst (astType: AstBOM.BomType.t) =
    let
      fun app3 (f, (x, y, z)) = (f x, f y, f z)
      fun maybe f x =
        case x of
          SOME y => f y
        | NONE => []
      fun convertNode (oldNode: AstBOM.BomType.node) : type_node =
        case oldNode of
          AstTy.Param param => Param (TyParam.fromAst param)
        (* | LongId (longid, tyargs) => *)
        (*     resolveLongTyId (longid, tyargs) *)
        | AstTy.Record records => Record (map fieldFromAst records)
        | AstTy.Tuple els => Tuple (map typeFromAst els)
        | AstTy.Fun funTuple => Fun (app3 (map typeFromAst, funTuple))
        | AstTy.Any => Any
        | AstTy.VProc => VProc
        | AstTy.Cont maybeTyArgs => maybe typesOfTyArgs maybeTyArgs
    in
      keepRegion (convertNode, astType)
    end
  and arityOfType (ty: type_t): int =
    let
      fun sumArity toSums = foldl (fn (x, y) => (arityOfType x) + y) 0 toSums
    in
        case Region.Wrap.node ty of
            Param param => 1
      | MLType mlTy => arityOfType mlTy
      | Record fields => sumArity (map typeOfField fields)
      | Tuple els  => sumArity els
      | Fun (domTy, contTy, rangeTy) =>
            (arityOfType domTy) + (arityOfType contTy) + (arityOfType rangeTy)
      | Cont conts => sumArity conts
      | Addr addrTy => arityOfType addrTy
      | _ => 0
    end
  and resolveLongTyId (longid: AstBOM.LongTyId.t,
      tyargs: tyargs_t option) : type_t =
        Region.Wrap.makeRegion (Any, Region.bogus) (* TODO *)
  and fieldFromAst (astField: AstBOM.Field.t): field_t =
    let
      fun doConvert (offset: IntInf.int, ty: AstBOM.BomType.t) =
        (offset, typeFromAst ty)
      fun convertNode (oldNode: AstBOM.Field.node) =
        case oldNode of
          Immutable myNode => Immutable (doConvert myNode)
        | Mutable myNode => Mutable (doConvert myNode)
    in
      keepRegion (convertNode, astField)
    end
  and typeOfField (myField: field_t): type_t =
    case Region.Wrap.node myField of
        Immutable (offset, ty) => ty
      | Mutable (offset, ty) => ty
  and typesOfTyArgs (argTys: tyargs_t): type_t list =
    let
      val (ArgTypes tys) = Region.Wrap.node argTys
    in
      tys
    end
  end


  structure TyArgs = struct
    datatype node = tyargs_node
    type t = tyargs_t

    fun getTypes argTys =
      Region.Wrap.node argTys
  end

  structure BomType = struct
    (* open AstBOM.BomType *)
    open Region.Wrap

    datatype node = datatype type_node
    type t = type_t

    type node' = node
    type obj = t

    val arity = arityOfType
    val fromAst = typeFromAst
    (* val resolveLongTyId = resolveLongTyId *)

  end


  structure Field = struct
    (* datatype node = datatype field_node *)
    type t = field_t

    val fromASt = fieldFromAst
    val getType = typeOfField


  end


  structure DataConsDef = struct
    open AstBOM.DataConsDef
  end

  structure DataTypeDef = struct
    open AstBOM.DataTypeDef
  end

  structure CArgTy = struct
  end

  structure CReturnTy = struct
  end

  structure VarPat = struct
  end

  structure FunDef = struct
  end

  structure Literal = struct
  end

  structure CaseRule = struct
  end

  structure TyCaseRule = struct
  end

  structure SimpleExp = struct
  end

  structure Exp = struct
  end

  structure RHS = struct
  end

  structure Definition = struct
  end

  structure HLOp = struct
  end

  structure TyCon = struct
  (* TODO *)
  end

  structure ValId = struct
  (* TODO *)
  end

  structure Decs = struct
  (* TODO *)
  end

  (* structure BomId = struct  *)
  (*   open AstBOM.BomId *)
  (* end  *)

  (* ... *)

  end
