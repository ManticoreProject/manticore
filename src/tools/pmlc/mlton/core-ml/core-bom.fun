signature CORE_LONGID_STRUCTS = sig
  structure AstId: LONGID
end

functor LongId (S: CORE_LONGID_STRUCTS) = struct
  open S.AstId

  fun fromAst (astId: S.AstId.t) = astId

  local
    fun make (myId) =
      let
        val T idBody = node myId
      in
        idBody
      end
    val strids = #strids o make
    val id = #id o make
  in
    val hasQualifier = not o null o strids
    val truncate = id
  end
end

functor CoreBOM (S: CORE_BOM_STRUCTS) : CORE_BOM = struct
  open S

  structure AstBOM = Ast.AstBOM

  local
    open Region
  in
  (* need this to unwrap regions since they're opaque *)
    fun regionToRecord (region: t):
        {left: SourcePos.t, right: SourcePos.t} = let
      fun fromOpt maybeRegion =
        Option.getOpt (maybeRegion, SourcePos.bogus)
    in
      {left = fromOpt (left region),
      right = fromOpt (right region)}
    end

    (* fun keepRegion (f: 'a -> 'b, wrapped: 'a Wrap.t): 'b Wrap.t = *)
    (*   Wrap.makeRegion ((f o Wrap.node) wrapped, Wrap.region wrapped) *)
    fun keepRegion (f: 'a -> 'b, (node, region): 'a * Region.t): 'b Wrap.t =
      Wrap.makeRegion (f node, region)
  end

  fun flatten f maybeEls =
    case maybeEls of
      SOME els => f els
    | NONE => []


  structure BomId = struct
    open AstBOM.BomId

    fun fromAst (oldId) = oldId
    fun fromStrid (strid, region) = fromSymbol (
      Ast.Strid.toSymbol strid, region)
  end

  structure HLOpId = struct
  end

  structure BomValueId = struct
  end


  structure TyParam = struct
    open Region.Wrap

    datatype node' = T of {
      name: string,
      hash: int          (* keeps track of insertion order *)
      (* plist: PropertyList.t *)
    }

    type t = node' Region.Wrap.t
    type obj = t
    (* type node' = node *)

    local
      val counter = Counter.new 0
    in
      fun fromAst (tyParam : AstBOM.TyParam.t) = let
          (* val asNode = AstBOM.TyParam.node tyParam *)
          val asRegion = AstBOM.TyParam.region tyParam
      in
          (* newString (AstBOM.TyParam.toString tyParam, asRegion) *)
          makeRegion (T {
            name = AstBOM.TyParam.toString tyParam,
            hash = Counter.next counter
          }, asRegion): t
      end
    end

    local
      fun unwrap myParam =
        let
          val T param = node myParam
        in
          param
        end
    in
      fun name myParam = #name (unwrap myParam)
      fun hash myParam = #hash (unwrap myParam)

      (* for map *)
      fun compare (param, param') =
        String.compare (name param, name param')
    end



    fun flattenFromAst (maybeTyParams: AstBOM.TyParams.t option) =
      flatten (fn els =>
        let
          val AstBOM.TyParams.T tyPs = AstBOM.TyParams.node els
        in
          tyPs
        end) maybeTyParams
      (* case maybeTyParams of *)
      (*   SOME tyParams => *)
      (*     let *)
      (*       val AstBOM.TyParams.T tyPs = AstBOM.TyParams.node tyParams *)
      (*     in *)
      (*       tyPs *)
      (*     end *)
      (* | NONE => [] *)
  end




  structure RawTy = struct
    open AstBOM.RawTy

    fun fromAst myRawTy = myRawTy
  end

  structure PrimOp = struct
  end

  structure LongTyId = LongId (structure AstId = AstBOM.LongTyId)
  structure LongValueId = LongId (structure AstId = AstBOM.LongValueId)

  (* structure LongTyId = struct *)
  (*   open AstBOM.LongTyId *)

  (*   fun fromAst (myLongTy: AstBOM.LongTyId.t) = myLongTy *)

  (*   local *)
  (*     fun make (myTy) = *)
  (*       let *)
  (*         val T tyBody = node myTy *)
  (*       in *)
  (*         tyBody *)
  (*       end *)
  (*     val strids = #strids o make *)
  (*     val id = #id o make *)
  (*   in *)
  (*     val hasQualifier = not o null o strids *)
  (*     val truncate = id *)
  (*   end *)
  (* end *)

  structure LongConId = struct
  end

  (* structure LongValueId = struct *)
  (*   open AstBOM.LongValueId *)
  (* end *)

  structure HLOpQId = struct
  end

  structure SymbolicId = struct
  end

  structure Attrs = struct
  end

  (* Mutually recursive types *)
   datatype tycon_node
    = TyC of {
      id: BomId.t,
      definition: dataconsdef_t list ref,
      params: TyParam.t list
    }
  (* and tycdef_node *)
  (*   = TycDef of dataconsdef_t list ref *)
  and dataconsdef_node
    = ConsDef of BomId.t * type_t option
  and type_node
    = Param of TyParam.t
    | TyCon of {
        con: tycon_t,
        args: type_t list
      }
    | Record of field_t list
    | Tuple of type_t list
    | Fun of {
        dom: type_t list,
        cont: type_t list,
        rng: type_t list
      }
    | Any
    | VProc
    | Cont of type_t list
    | Addr of type_t
    | Raw of RawTy.t
    | Error
  and field_node
    = Immutable of IntInf.int * type_t
    | Mutable of IntInf.int * type_t
  and tyargs_node
    = ArgTypes of type_t list

  withtype tycon_t = tycon_node Region.Wrap.t
  (* and tycdef_t = tycdef_node Region.Wrap.t *)
  and dataconsdef_t = dataconsdef_node Region.Wrap.t
  and type_t = type_node Region.Wrap.t
  and field_t = field_node Region.Wrap.t
  and tyargs_t = tyargs_node Region.Wrap.t

  (* Functions over mutually recursive types *)
  local
      (* define some synonyms so we don't end up with painfully long
      datatype names *)
    structure AstTy = AstBOM.BomType
    structure AstField = AstBOM.Field
    structure AstTyArgs = AstBOM.TyArgs
    fun app3 (f, (x, y, z)) = (f x, f y, f z)
  in
  fun typeFromAst (astType: AstBOM.BomType.t) =
    let
      fun maybe f x =
        case (x: 'a option) of
          SOME (y: 'a) => (f: 'a -> 'b list) y
        | NONE => []
      fun convertNode (oldNode: AstBOM.BomType.node) : type_node =
        case oldNode of
          AstTy.Param param => Param (TyParam.fromAst param)
        | AstTy.LongId (longid, tyargs) =>
            resolveLongTyId (longid, tyargs)
        | AstTy.Record records => Record (map fieldFromAst records)
        | AstTy.Tuple els => Tuple (map typeFromAst els)
        | AstTy.Fun funTuple =>
            let
              val (dom, cont, rng) = app3 (map typeFromAst, funTuple)
            in
               Fun ({dom = dom, cont = cont, rng = rng})
            end
        | AstTy.Any => Any
        | AstTy.VProc => VProc
        | AstTy.Cont maybeTyArgs =>
            Cont (maybe (typesOfTyArgs o tyArgsFromAst) maybeTyArgs)
            (* case maybeTyArgs of *)
            (*   SOME tyArgs => typesOfTyArgs tyArgs *)
            (* | NONE => [] *)
        | AstTy.Raw ty => Raw ty
        | AstTy.Addr ty => Addr (typeFromAst ty)
    in
      keepRegion (convertNode, AstTy.dest astType)
    end
  and arityOfType (ty: type_t): int =
    let
      fun sumArity toSums = foldl (fn (x, y) => (arityOfType x) + y) 0 toSums
    in
      case Region.Wrap.node ty of
        Param param => 1
      (* | MLType mlTy => arityOfType mlTy *)
      | Record fields => sumArity (map typeOfField fields)
      | Tuple els  => sumArity els
      (* | Fun tys => *)
      (*   let *)
      (*     val (bomAr, conAr, rangeAr) = app3 (sumArity, tys) *)
      (*   in *)
      (*     bomAr + conAr + rangeAr *)
      (*   end *)
      | Cont conts => sumArity conts
      | Addr addrTy => arityOfType addrTy
      | _ => 0
    end
  and resolveLongTyId (longid: AstBOM.LongTyId.t,
      tyargs: AstTyArgs.t option) : type_node =
        Any (* TODO *)
  and fieldFromAst (astField: AstBOM.Field.t): field_t =
    let
      fun doConvert (offset: IntInf.int, ty: AstBOM.BomType.t) =
        (offset, typeFromAst ty)
      fun convertNode (oldNode: AstField.node) =
        case oldNode of
          AstField.Immutable myNode => Immutable (doConvert myNode)
        | AstField.Mutable myNode => Mutable (doConvert myNode)
    in
      keepRegion (convertNode, AstField.dest astField)
    end
  and typeOfField (myField: field_t): type_t =
    case Region.Wrap.node myField of
        Immutable (offset, ty) => ty
      | Mutable (offset, ty) => ty
  and tyArgsFromAst (tyArgs: AstTyArgs.t): tyargs_t =
    let
      fun convertNode (AstTyArgs.ArgTypes tys) =
        ArgTypes (map typeFromAst tys)
    in
      keepRegion (convertNode, AstTyArgs.dest tyArgs)
    end

  and typesOfTyArgs (argTys: tyargs_t): type_t list =
    let
      val (ArgTypes tys) = Region.Wrap.node argTys
    in
      tys
    end
  end

  structure DataConsDef = struct
    open Region.Wrap

    datatype node = datatype dataconsdef_node
    type t = dataconsdef_t
    type ty = type_t

    type node' = node
    type obj = t

  end

  structure TyCon = struct
      open Region.Wrap

      datatype node = datatype tycon_node
      type t = tycon_t
      type node' = node
      type obj = t

  end


  structure TyArgs = struct
    datatype node = datatype tyargs_node
    type t = tyargs_t

    val getTypes = typesOfTyArgs
    val fromAst = tyArgsFromAst

    fun flattenFromAst maybeTyArgs =
      flatten (fn els =>
        let
          val AstBOM.TyArgs.ArgTypes tyArgs = AstBOM.TyArgs.node els
        in
          tyArgs
        end) maybeTyArgs
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
    val keepRegion = keepRegion
    fun errorFromAst astTy =
      keepRegion (fn x => Error, AstBOM.BomType.dest astTy)


    (* local  *)
    (*   fun walk (ty: t, applyTo: t -> 'a) =  *)
    (*     case node ty of  *)
    (*       Param p => applyTo p *)
    (*     | TyCon con => applyTo con *)
    (*     | Record fields => applyTo fields *)
    (*     | Tuple ts => applyTo ts *)
    (*     | Fun f => applyTo f *)
    (*     | Cont ts => applyTo ts *)
    (*     | Addr t => applyTo t *)
    (*     | Raw raw => applyTo raw *)
    (*     | _ => _ *)

    (*   fun swap (t  *)
    (* in *)

    local
      structure TyParamSet = RedBlackSetFn (struct
        type ord_key = TyParam.t
        val compare = TyParam.compare
      end)
    in
      fun allTyParams (ty, acc) =
        let
          fun foldTyList (tys, acc') =
            foldr
              (fn (ty', acc') => (allTyParams (ty', []))@acc')
              acc'
              tys
        in
          case node ty of
            Param p => p::acc
          | Tuple tys =>
              foldTyList (tys, acc)
          | Fun {dom, cont, rng} =>
              List.concat (map (fn x => foldTyList (x, [])) [dom, cont, rng])
          | Cont tys => foldTyList (tys, acc)
          | Addr ty' => allTyParams (ty', acc)
          | _ => acc
          (* TODO: record, tycon *)
        end
      fun uniqueTyParams ty =
        TyParamSet.addList (TyParamSet.empty, allTyParams (ty, []))
      val arity = TyParamSet.numItems o uniqueTyParams
    end

    (* swap out the named TyParam for the given type *)
    fun applyArg (ty: t, toSwap: TyParam.t, swapFor: t): t =
      let
        fun swap (param: TyParam.t): node =
          case TyParam.compare (param, toSwap) of
            EQUAL => node swapFor
          | _ => Param param
        fun doApply ty' = applyArg (ty', toSwap, swapFor)
        fun doApplys tys = map doApply tys
      in
        makeRegion (case node ty of
          Param p => swap p
        (* | TyCon {con = con, args = args} => *)
        (*     TyCon {con = doApply con, args = doApplys args} *)
         (* TODO: deal with tycons *)
        (* | Record fields =>    (* TODO: deal with fields *) *)
        (*     Record (map applyArg fields) *)
        | Tuple ts => Tuple (doApplys ts)
        | Fun {dom, cont, rng} => Fun {
            dom = doApplys dom,
            cont = doApplys cont,
            rng = doApplys rng
          }
        | Cont tys => Cont (doApplys tys)
        | Addr t => Addr (doApply t)
        (* | Raw raw => applyArg raw *)
        | x => x,
        region ty)
      end


  end


  structure Field = struct
    open Region.Wrap
    (* datatype node = datatype field_node *)
    type t = field_t
    type ty = type_t
    datatype node = datatype field_node

    type node' = node
    type obj = t

    val fromAst = fieldFromAst
    val getType = typeOfField
    val keepRegion = keepRegion
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


  (* structure TyCon = struct *)
  (* (* TODO *) *)
  (* end *)


  structure ModuleId = struct
    type t = BomId.t list

    val compare = List.collate BomId.compare

    fun fromLong' (splitFn, regionFn) longId  =
      let
        val (qualifiers: AstBOM.BomId.t list, id: AstBOM.BomId.t) =
          splitFn longId
        val region = regionFn longId
      in
        (qualifiers, id): (t * BomId.t)
      end


    val fromLongTyId' = fromLong' (AstBOM.LongTyId.split,
      AstBOM.LongTyId.region)

    (* fun fromLongTyId' longTyId = *)
    (*   let *)
    (*     val (qualifiers: AstBOM.BomId.t list, id: AstBOM.BomId.t) = *)
    (*       AstBOM.LongTyId.split longTyId *)
    (*     val region = AstBOM.LongTyId.region longTyId *)

    (*     (* val modId: t = map *) *)
    (*     (*   (fn strid => BomId.fromStrid (strid, region)) *) *)
    (*     (*   qualifiers *) *)
    (*   in *)
    (*     (qualifiers, id): (t * BomId.t) *)
    (*   end *)

    val fromLongTyId = #1 o fromLongTyId'

    val fromLongValueId' = fromLong' (AstBOM.LongValueId.split,
      AstBOM.LongValueId.region)

    val fromLongValueId = #1 o fromLongValueId'

    val toString = String.concatWith "." o (map BomId.toString)

    fun fromBomId bomId = [bomId]

    val bogus = [BomId.bogus]
  end


  structure TyId = struct
    datatype t
      = BomTy of BomId.t
      | QBomTy of ModuleId.t * BomId.t
      (* | MLTy *)

    val fromAstBomId = BomTy o BomId.fromAst

    fun fromLongTyId longTyId =
      let
        val longTyId' = LongTyId.fromAst longTyId
      in
        if LongTyId.hasQualifier longTyId' then
          QBomTy (ModuleId.fromLongTyId' longTyId')
        else
          BomTy (LongTyId.truncate longTyId')
      end

    fun maybeQualify (tyId, defaultId) =
      case tyId of
        BomTy ty => QBomTy (defaultId, ty)
      | _ => tyId

    local
      fun app2 f (x, y) = (f x, f y)
    in
      fun toString id =
        case id of
          BomTy id' => BomId.toString id'
        | QBomTy (module, id) =>
            String.concatWith "." [ModuleId.toString module, BomId.toString id]

      val compare = String.compare o (app2 toString)
    end
  end


  structure ValId = struct
    datatype t
      = BomVal of BomId.t
      | QBomVal of ModuleId.t * BomId.t

    val fromAstBomId = BomVal o BomId.fromAst

    fun fromLongValueId (longValId: AstBOM.LongValueId.t) =
      let
        val longValId': LongValueId.t = LongValueId.fromAst longValId
      in
        if LongValueId.hasQualifier longValId' then
          QBomVal (ModuleId.fromLongValueId' longValId')
        else
          BomVal (LongValueId.truncate longValId')
      end

    fun maybeQualify (valId, defaultId) =
      case valId of
        BomVal id => QBomVal (defaultId, id)
      | _ => valId

  end

  structure Decs = struct
  (* TODO *)
  end

  (* structure BomId = struct  *)
  (*   open AstBOM.BomId *)
  (* end  *)

  (* ... *)

  end
