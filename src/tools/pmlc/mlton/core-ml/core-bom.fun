signature CORE_LONGID_STRUCTS = sig
  structure AstId: LONGID
end

functor DependencyWrapper (S: DEPENDENCY_WRAPPER_STRUCTS) = struct
  open S

  type t = node'

  fun identity x = x

  val wrap = identity
  val node = identity
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



    (* fun flattenFromAst (maybeTyParams: AstBOM.TyParams.t option) = *)
    (*   flatten (fn els => *)
    (*     let *)
    (*       val AstBOM.TyParams.T tyPs = AstBOM.TyParams.node els *)
    (*     in *)
    (*       tyPs *)
    (*     end) maybeTyParams *)
    (* fun flattenFromAst' maybeTyParams = *)
    (*   map fromAst (flattenFromAst maybeTyParams) *)
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
    datatype t = datatype node

    fun fromAst myRawTy = AstBOM.RawTy.node myRawTy
  end

  structure PrimOp = struct
  end

  structure LongTyId = LongId (structure AstId = AstBOM.LongTyId)
  structure LongValueId = LongId (structure AstId = AstBOM.LongValueId)

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

    fun toBomId moduleId = List.last moduleId
  end



  (* TODO: refactor these to be cleaner. I don't want to collapse them
  into a functor because TyId.t will be changing in the future and it
  won't be easy to extend *)

  fun app2 f (x, y) = (f x, f y)

  structure TyId = struct
    datatype t
      = BomTy of BomId.t
      | QBomTy of ModuleId.t * BomId.t
      (* | MLTy *)

    val fromAstBomId = BomTy o BomId.fromAst

    fun fromLongTyId (longTyId: AstBOM.LongTyId.t): t =
      let
        val longTyId' = LongTyId.fromAst longTyId
      in
        if LongTyId.hasQualifier longTyId' then
          QBomTy (ModuleId.fromLongTyId' longTyId')
        else
          BomTy (LongTyId.truncate longTyId')
      end

    fun toString id =
      case id of
        BomTy id' => BomId.toString id'
      | QBomTy (module, id) =>
          String.concatWith "." [ModuleId.toString module, BomId.toString id]

    fun maybeQualify (tyId, defaultId) =
      case tyId of
        BomTy ty => (print ("qualifying with " ^ (ModuleId.toString defaultId) ^ "\n")
        ;  QBomTy (defaultId, ty))
      | _ => (print "not qualifying\n"; tyId)



    val compare = String.compare o (app2 toString)
  end


  structure ValId = struct
    datatype t
      = BomVal of BomId.t
      | QBomVal of ModuleId.t * BomId.t

    val fromAstBomId = BomVal o BomId.fromAst

    fun fromLongValueId (longValId: AstBOM.LongValueId.t): t =
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


    fun toString (id: t) =
      case id of
        BomVal id => BomId.toString id
      | QBomVal (module, id) =>
          String.concatWith "." [ModuleId.toString module, BomId.toString id]

    val compare = String.compare o (app2 toString)

  end

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
   datatype tycon_t
    = TyC of {
      id: TyId.t,
      definition: dataconsdef_t list ref,
      params: TyParam.t list
    }
  (* and tycdef_t *)
  (*   = TycDef of dataconsdef_t list ref *)
  and dataconsdef_t
    = ConsDef of BomId.t * type_t option
  and type_t
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
  and field_t
    = Immutable of IntInf.int * type_t
    | Mutable of IntInf.int * type_t
  and tyargs_t
    = ArgTypes of type_t list

  (* Functions over mutually recursive types *)
  local
    fun app3 (f, (x, y, z)) = (f x, f y, f z)
  in
  (* TODO: only count unique typarams *)
  fun arityOfType (ty: type_t): int =
    let
      fun sumArity toSums = foldl (fn (x, y) => (arityOfType x) + y) 0 toSums
    in
      case ty of
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
  and typeOfField (myField: field_t): type_t =
    case myField of
      Immutable (offset, ty) => ty
    | Mutable (offset, ty) => ty
  and typesOfTyArgs (ArgTypes tys): type_t list =
    tys
  and arityOfDataCons (ConsDef (id, maybeTy)): int =
    case maybeTy of
      SOME ty => arityOfType ty
    | NONE => 0
  and tyEqual (ty, ty'): bool =
    case (ty, ty') of
      (Param p, Param p') => TyParam.compare (p, p') = EQUAL
     (* TODO: tyConEquals needs to check that the arity is the same *)
    | (TyCon {con, args}, TyCon {con=con', args=args'}) =>
        tyConEquals (con, con') andalso tysEqual (args, args')
    (* TODO: make sure fields are sorted so we can compare them pairwise *)
    | (Record fields, Record fields') => false
    | (Tuple tys, Tuple tys') => tysEqual (tys, tys')
    | (Fun funTy, Fun funTy') =>
        List.all (fn x => x) (
        List.map (fn select => tysEqual (select funTy, select funTy'))
        [(fn fTy => #dom fTy), (fn fTy => #cont fTy), (fn fTy => #rng fTy)])
    | (VProc, VProc) => true
    | (Cont tys, Cont tys') => tysEqual (tys, tys')
    | (Addr ty, Addr ty') => tyEqual (ty, ty')
    | (Raw raw, Raw raw') => rawEquals (raw, raw')
    | (Error, _) => false
    | (_, Error) => false
    | (Any, _) => true
    | (_, Any) => true
    | _ => false
  and tysEqual (tys, tys') =
    (length tys = length tys') andalso (ListPair.allEq tyEqual (tys, tys'))
  and rawEquals (raw: RawTy.t, raw': RawTy.t): bool =
    case (raw, raw') of
      (RawTy.Int8, RawTy.Int8) => true
    | (RawTy.Uint8, RawTy.Uint8) => true
    | (RawTy.Int16, RawTy.Int16) => true
    | (RawTy.Uint16, RawTy.Uint16) => true
    | (RawTy.Int32, RawTy.Int32) => true
    | (RawTy.Uint32, RawTy.Uint32) => true
    | (RawTy.Int64, RawTy.Int64) => true
    | (RawTy.Uint64, RawTy.Uint64) => true
    | (RawTy.Float32, RawTy.Float32) => true
    | (RawTy.Float64, RawTy.Float64) => true
    | (_, _) => false
  (* TODO: MAKE THIS ROBUST, ADD A UID *)
  and tyConEquals (TyC tyC, TyC tyC') =
    let
      val toString = TyId.toString o #id
    in
      String.compare (toString tyC, toString tyC') = EQUAL
    end
  end

  structure DataConsDef = struct
    (* open Region.Wrap *)

    datatype node = datatype dataconsdef_t
    (* type t = dataconsdef_t *)
    type ty = type_t

    local
      structure Wrapper = DependencyWrapper (struct
        datatype node' = datatype node
      end)
    in
      open Wrapper
    end

    (* type node' = node *)
    (* type obj = t *)

    val arity = arityOfDataCons
    val error = ConsDef (BomId.bogus, NONE)
  end


  (* structure TyArgs = struct *)
  (*   datatype t = datatype tyargs_t *)
  (*   (* type t = tyargs_t *) *)

  (*   val getTypes = typesOfTyArgs *)
  (*   (* val fromAst = tyArgsFromAst *) *)

  (*   fun flattenFromAst maybeTyArgs = *)
  (*     flatten (fn els => *)
  (*       let *)
  (*         val AstBOM.TyArgs.ArgTypes tyArgs = AstBOM.TyArgs.node els *)
  (*       in *)
  (*         tyArgs *)
  (*       end) maybeTyArgs *)
  (* end *)

  structure BomType = struct
    (* open AstBOM.BomType *)
    (* open Region.Wrap *)

    datatype t = datatype type_t
    (* type t = type_t *)

    (* type node' = node *)
    (* type obj = t *)

    val arity = arityOfType


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
          case ty of
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
      fun uniqueTyParamsSet ty =
        TyParamSet.addList (TyParamSet.empty, allTyParams (ty, []))
      val uniqueTyParams = TyParamSet.listItems o uniqueTyParamsSet
      val arity = TyParamSet.numItems o uniqueTyParamsSet
    end

    (* swap out the named TyParam for the given type *)
    fun applyArg (ty: t, toSwap: TyParam.t, swapFor: t): t =
      let
        fun swap (param: TyParam.t) =
          case TyParam.compare (param, toSwap) of
            EQUAL => swapFor
          | _ => Param param
        fun doApply ty' = applyArg (ty', toSwap, swapFor)
        fun doApplys tys = map doApply tys
      in
        case ty of
          Param p => swap p
        | TyCon {con = con, args = args} =>
            TyCon {con = con, args = doApplys args}
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
        | x => x
      end

    val equal = tyEqual
    val equals = tysEqual

    local
      fun boolToOpt (comparison: ('a * 'a) -> bool) (left, right) =
        if comparison (left, right) then
          SOME left
        else
          NONE
    in
      val equal' = boolToOpt equal
      val equals' = boolToOpt equals
    end

  end

  structure TyCon = struct
      (* open Region.Wrap *)

      (* datatype node = datatype tycon_node *)
      (* type t = tycon_t *)
      datatype t = datatype tycon_t
      type ty = BomType.t
      (* type node' = node *)
      (* type obj = t *)

      fun toBomTy (tyCon as TyC {params,...}) =
        BomType.TyCon {
          con = tyCon,
          args = []         (* TODO: is this what should go here? *)
        }

      fun arity (TyC {params,...}) =
        length params

      fun applyToArgs (tyCon, tys) =
        if length tys = arity tyCon then
          SOME (BomType.TyCon {
            con = tyCon,
            args = tys
          })
        else
          NONE


  end

  structure Field = struct
    (* open Region.Wrap *)
    (* datatype node = datatype field_node *)
    type ty = type_t
    datatype node = datatype field_t

    local
      structure Wrapper = DependencyWrapper(struct
        datatype node' = datatype node
      end)
    in
      open Wrapper
    end

    val getType = typeOfField
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


  structure Decs = struct
  (* TODO *)
  end

  (* structure BomId = struct  *)
  (*   open AstBOM.BomId *)
  (* end  *)

  (* ... *)

  end
