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

  (* val error = long ([], Id.bogus) *)
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

  structure Attr = struct
    type t = string

    fun flattenFromAst attrs =
      case attrs of
        SOME attrs =>
          let
            val AstBOM.Attrs.T unwrapped = AstBOM.Attrs.node attrs
          in
            unwrapped
          end
      | _ => []
  end

  structure HLOpId = struct
  end

  structure BomValueId = struct
  end


  structure TyParam = struct
  (* TODO: remove wrapping *)
    open Region.Wrap

    datatype node' = T of {
      name: string,
      hash: int          (* keeps track of insertion order *)
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
  structure LongConId = LongId (structure AstId = AstBOM.LongConId)

  structure ModuleId = struct
    type t = BomId.t list

    val compare = List.collate BomId.compare

    local
      fun fromLong' (splitFn, regionFn) longId  =
        let
          val (qualifiers: AstBOM.BomId.t list, id: AstBOM.BomId.t) =
            splitFn longId
          val region = regionFn longId
        in
          (qualifiers, id): (t * BomId.t)
        end
    in
      val fromLongTyId' = fromLong' (AstBOM.LongTyId.split,
        AstBOM.LongTyId.region)
      val fromLongTyId = #1 o fromLongTyId'

      val fromLongValueId' = fromLong' (AstBOM.LongValueId.split,
        AstBOM.LongValueId.region)
      val fromLongValueId = #1 o fromLongValueId'

      val fromLongConId' = fromLong' (AstBOM.LongConId.split,
        AstBOM.LongConId.region)
      val fromLongConId = #1 o fromLongConId'
  end

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

    local
      fun fromLongId (fromAst, hasQual, toModId, truncate) longId =
        let
          val longId' = fromAst longId
        in
          if hasQual longId' then
            QBomVal (toModId longId')
          else
            BomVal (truncate longId')
        end
    in
       val fromLongValueId = fromLongId (LongValueId.fromAst,
         LongValueId.hasQualifier, ModuleId.fromLongValueId',
         LongValueId.truncate)
       val fromLongConId = fromLongId (LongConId.fromAst,
         LongConId.hasQualifier, ModuleId.fromLongConId',
         LongConId.truncate)
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

    val error = BomVal BomId.bogus
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
    | Con of {
        dom: type_t,
        rng: type_t
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
    | NoReturn
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
    datatype t = datatype dataconsdef_t

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
    datatype t = datatype type_t

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

    fun applyArgs (ty, paramsAndArgs) = foldr (fn ((toSwap, swapFor), ty) =>
      applyArg (ty, toSwap, swapFor)) ty paramsAndArgs

    fun applyArgs' (ty, params, args) =
      SOME (applyArgs (ty, ListPair.zipEq (params, args)))
        handle ListPair.UnequalLengths => NONE


    val equal = tyEqual
    val equals = tysEqual

    fun isCon ty =
      case ty of
        Con con => SOME ty
      | _ => NONE

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

    val unit = Tuple []
  end

  structure TyCon = struct
      datatype t = datatype tycon_t

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
    datatype t = datatype field_t

    val getType = typeOfField
    fun index field =
      case field of
        Immutable (index, _) => index
      | Mutable (index, _) => index

    val bogus = Immutable (~1, Error)
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

  structure RHS = struct
  end

  structure HLOp = struct
  end

  structure Val = struct
    datatype t = T of {
      id: ValId.t,
      ty: BomType.t,
      params: TyParam.t list,
      stamp: Stamp.stamp
    }

    fun typeOf (T {ty, ...}) = ty
    fun idOf (T {id, ...}) = id
    fun stampOf (T {stamp, ...}) = stamp

    local
      fun stampsOf (lhs, rhs) = (stampOf lhs, stampOf rhs)
    in
      val compare = Stamp.compare o stampsOf
      val same = Stamp.same o stampsOf
    end

    fun hasId (thisVal, valId) =
      ValId.compare (idOf thisVal, valId) = EQUAL

    fun new (valId, ty, params) = T {
      id = valId,
      ty = ty,
      params = params,
      stamp = Stamp.new()
    }

    fun applyToArgs (T {ty, params, id, stamp}, args) =
      case BomType.applyArgs' (ty, params, args) of
        SOME ty => SOME (T {ty = ty, params = params, id = id, stamp = stamp})
      | NONE => NONE

    val error = new (ValId.error, BomType.Error, [])
  end

  structure Exp = struct
    datatype t = T of {node: node, ty: BomType.t}
    and node
      = Let of Val.t list * t * t
      | If of t * t * t
      | Case              (* TODO *)
      | TyCase            (* TODO *)
      | Apply of Val.t * t list * t list
      | Throw of Val.t * t list
      | Return of t list
      | PrimOp of Val.t * t list
      | Alloc of Val.t * t list
      | RecAccess of IntInf.int * t * t
      | Promote of t
      | HostVproc
      | VpLoad of IntInf.int * t
      | VpAddr of IntInf.int * t
      | VpStore of IntInf.int * t * t
      | Val of Val.t

    fun new (node, ty) = T {node = node, ty = ty}

    local
      fun unwrap (T exp) = exp
    in
      val typeOf = #ty o unwrap
      val node = #node o unwrap
    end

  end



  structure PrimOp = struct
    type arg = Val.t
    type result = BomType.t Prim.prim

    local
      structure PrimTyFn = PrimTyFn (struct
        type var = arg
        type ty = BomType.t

        val typeOf = Val.typeOf

        val noTy = BomType.unit
        val anyTy = BomType.Any
        fun raw rawType =
          BomType.Raw (
            case rawType of
              RawTypes.T_Byte => RawTy.Int8
            | RawTypes.T_Short => RawTy.Int16
            | RawTypes.T_Int => RawTy.Int32
            | RawTypes.T_Long => RawTy.Int64
            | RawTypes.T_Float => RawTy.Float32
            | RawTypes.T_Double => RawTy.Float64)

        val addr = BomType.Addr
      end)
    in
      open PrimTyFn
    end

    fun nullaryCon primOp =
      case AstBOM.PrimOp.toString primOp of
        "Pause" => SOME Prim.Pause
      | "FenceRead" => SOME Prim.FenceRead
      | "FenceWrite" => SOME Prim.FenceWrite
      | "FenceRW" => SOME Prim.FenceRW
      | _ => NONE

    fun unaryCon primOp =
      case AstBOM.PrimOp.toString primOp of
        "I32Neg" => SOME Prim.I32Neg
      | "I64Neg" => SOME Prim.I64Neg
      | "F32Neg" => SOME Prim.F32Neg
      | "F32Sqrt" => SOME Prim.F32Sqrt
      | "F32Abs" => SOME Prim.F32Abs
      | "F64Neg" => SOME Prim.F64Neg
      | "F64Sqrt" => SOME Prim.F64Sqrt
      | "F64Abs" => SOME Prim.F64Abs
      | "I32ToI64X" => SOME Prim.I32ToI64X
      | "I32ToI64" => SOME Prim.I32ToI64
      | "I64ToI32" => SOME Prim.I64ToI32
      | "I32ToF32" => SOME Prim.I32ToF32
      | "I32ToF64" => SOME Prim.I32ToF64
      | "I64ToF32" => SOME Prim.I64ToF32
      | "I64ToF64" => SOME Prim.I64ToF64
      | "F64ToI32" => SOME Prim.F64ToI32
      | "AdrLoadI8" => SOME Prim.AdrLoadI8
      | "AdrLoadU8" => SOME Prim.AdrLoadU8
      | "AdrLoadI16" => SOME Prim.AdrLoadI16
      | "AdrLoadU16" => SOME Prim.AdrLoadU16
      | "AdrLoadI32" => SOME Prim.AdrLoadI32
      | "AdrLoadI64" => SOME Prim.AdrLoadI64
      | "AdrLoadF32" => SOME Prim.AdrLoadF32
      | "AdrLoadF64" => SOME Prim.AdrLoadF64
      | "AdrLoadAdr" => SOME Prim.AdrLoadAdr
      | "AdrLoad" => SOME Prim.AdrLoad
      | "AllocIntArray" => SOME Prim.AllocIntArray
      | "AllocLongArray" => SOME Prim.AllocLongArray
      | "AllocFloatArray" => SOME Prim.AllocFloatArray
      | "AllocDoubleArray" => SOME Prim.AllocDoubleArray
      | _ => NONE

    fun binaryCon primOp =
      case AstBOM.PrimOp.toString primOp of
        "I32Add" => SOME Prim.I32Add
      | "I32Sub" => SOME Prim.I32Sub
      | "I32Mul" => SOME Prim.I32Mul
      | "I32Div" => SOME Prim.I32Div
      | "I32Mod" => SOME Prim.I32Mod
      | "I32LSh" => SOME Prim.I32LSh
      | "I64Add" => SOME Prim.I64Add
      | "I64Sub" => SOME Prim.I64Sub
      | "I64Mul" => SOME Prim.I64Mul
      | "I64Div" => SOME Prim.I64Div
      | "I64Mod" => SOME Prim.I64Mod
      | "I64LSh" => SOME Prim.I64LSh
      | "U64Mul" => SOME Prim.U64Mul
      | "U64Div" => SOME Prim.U64Div
      | "U64Rem" => SOME Prim.U64Rem
      | "F32Add" => SOME Prim.F32Add
      | "F32Sub" => SOME Prim.F32Sub
      | "F32Mul" => SOME Prim.F32Mul
      | "F32Div" => SOME Prim.F32Div
      | "F64Add" => SOME Prim.F64Add
      | "F64Sub" => SOME Prim.F64Sub
      | "F64Mul" => SOME Prim.F64Mul
      | "F64Div" => SOME Prim.F64Div
      | "AdrAddI32" => SOME Prim.AdrAddI32
      | "AdrAddI64" => SOME Prim.AdrAddI64
      | "AdrSubI32" => SOME Prim.AdrSubI32
      | "AdrSubI64" => SOME Prim.AdrSubI64
      | "AdrStoreI8" => SOME Prim.AdrStoreI8
      | "AdrStoreI16" => SOME Prim.AdrStoreI16
      | "AdrStoreI32" => SOME Prim.AdrStoreI32
      | "AdrStoreI64" => SOME Prim.AdrStoreI64
      | "AdrStoreF32" => SOME Prim.AdrStoreF32
      | "AdrStoreF64" => SOME Prim.AdrStoreF64
      | "AdrStoreAdr" => SOME Prim.AdrStoreAdr
      | "AdrStore" => SOME Prim.AdrStore
      | "ArrLoadI32" => SOME Prim.ArrLoadI32
      | "ArrLoadI64" => SOME Prim.ArrLoadI64
      | "ArrLoadF32" => SOME Prim.ArrLoadF32
      | "ArrLoadF64" => SOME Prim.ArrLoadF64
      | "ArrLoad" => SOME Prim.ArrLoad
      | "I32FetchAndAdd" => SOME Prim.I32FetchAndAdd
      | "I64FetchAndAdd" => SOME Prim.I64FetchAndAdd
      | "AllocPolyVec " => SOME Prim.AllocPolyVec
      | _ => NONE

    fun ternaryCon primOp =
      case AstBOM.PrimOp.toString primOp of
        "ArrStoreI32" => SOME Prim.ArrStoreI32
      | "ArrStoreI64" => SOME Prim.ArrStoreI64
      | "ArrStoreF32" => SOME Prim.ArrStoreF32
      | "ArrStoreF64" => SOME Prim.ArrStoreF64
      | "ArrStore" => SOME Prim.ArrStore
      | "CAS" => SOME Prim.CAS
      | _ => NONE


  end


  structure Definition = struct
    datatype t
      = Fun of Attr.t list * ValId.t * Exp.t
      | HLOp of Attr.t list * ValId.t * Exp.t
      | Import of BomType.t
  end

  (* structure Decs = struct *)
  (*   datatype t = T of Definition.t list *)

  (*   val empty = T [] *)
  (* end *)

  end
