(* translate.sml
 *
 * COPYRIGHT (c) 2013 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure Translate : sig

(*    structure Sxml : SXML*)
    val translate : PMLFrontEnd.Sxml.Program.t -> BOM.program

  end = struct

    structure S = PMLFrontEnd.Sxml
    structure V = Vector
    structure BV = BOMVar
    structure P = Prim

  (***** Define environment map types/structures *****)

    structure VMap = BV.Map

    structure TyCacheKV = struct
        type ord_key = S.Type.dest
        fun compare(t1: S.Type.dest, t2: S.Type.dest) = (case (t1, t2)
           of (S.Type.Con (ltc1, tys1), S.Type.Con (ltc2, tys2)) => (case String.compare (S.Tycon.toString ltc1, S.Tycon.toString ltc2)
               of EQUAL => V.collate compare (V.map S.Type.dest tys1, V.map S.Type.dest tys2)
                | relation => relation
               (* end case *))
            | (S.Type.Con _, S.Type.Var _) => LESS
            | (S.Type.Var v1, S.Type.Var v2) => String.compare (S.Tyvar.toString v1, S.Tyvar.toString v2)
            | (S.Type.Var _, S.Type.Con _) => GREATER
        (* end case *))
    end
    structure TyCache = RedBlackMapFn (TyCacheKV)
    type tycache_t = BOMTy.t TyCache.map (* map from AST types to BOM types *)


    structure TycMapKV = struct
        type ord_key = S.Tycon.t
        fun compare(tyc1: S.Tycon.t, tyc2: S.Tycon.t) = String.compare (Layout.toString (S.Tycon.layout tyc1), Layout.toString (S.Tycon.layout tyc1))
    end
    structure TycMap = RedBlackMapFn (TycMapKV)
    type tycmap_t = BOMTyc.t TycMap.map (* map from AST tycs to BOM datatype constructors *)

    structure VarCacheKV = struct
        type ord_key = (S.Var.t)
        fun compare(v1, v2) = Word.compare (S.Var.hash v1, S.Var.hash v2)
    end
    structure VarCache = RedBlackMapFn (VarCacheKV)
    type varcache_t = BOM.var VarCache.map (* map from AST variables to BOM variables *)

    structure BOMValKV = struct
        type ord_key = (S.CoreBOM.Val.t)
        fun compare(v1, v2) = Stamp.compare (S.CoreBOM.Val.stampOf v1, S.CoreBOM.Val.stampOf v2)
    end
    structure BOMValMap = RedBlackMapFn (BOMValKV)
    type bomvalmap_t = BOM.var BOMValMap.map (* map from CoreBOM (frontend) values to (middle-end) BOM variables *)

  (***** Translation environment *****)

    datatype var_bind
      = Lambda of (BOMTy.t -> BOM.lambda) (* used for primops and high-level ops *)
      | Var of BOM.var
      | EqOp                        (* either "=" or "<>" *)


    fun cacheTyDest (cache, sxmltyd, bomty) = TyCache.insert(cache, sxmltyd, bomty)
    fun cacheTy (cache, sxmlty, bomty) = cacheTyDest(cache, S.Type.dest sxmlty, bomty)

    fun peekTyDest (cache, sxmltyd) = TyCache.find(cache, sxmltyd)
    fun peekTy (cache, sxmlty) = peekTyDest (cache, S.Type.dest sxmlty)

    fun lookupTyDest (cache, sxmltyd) = (case peekTyDest(cache, sxmltyd)
           of SOME bomty => bomty
            | NONE => raise Fail(concat["lookupTyDest(cache, <S.Type.dest>) = NONE"])
          (* end case *))
    fun lookupTy (cache, sxmlty) = (case peekTyDest(cache, S.Type.dest sxmlty)
           of SOME bomty => bomty
            | NONE => raise Fail(concat["lookupTy(cache, ", Layout.toString (S.Type.layout sxmlty), ") = NONE"])
          (* end case *))

    fun exhTy (tyCache) = BOMTy.T_Cont[lookupTy(tyCache, S.Type.exn)]


    val tupleTyc = BOMTyc.new("tuple", 0) (* TODO(wings): is this right? *)
    val boolTyc = BOMTyc.new("bool", 0)
      val falseCon = BOMDataCon.new boolTyc ("false", []);
      val trueCon = BOMDataCon.new boolTyc ("true", []);
    val exnTyc = BOMTyc.new("exn", 0)
    val exnTy0 = BOMTy.T_Con (exnTyc, [])
    val exhTy0 = BOMTy.T_Cont [exnTy0] (* TODO(wings): construct from SXML's S.Type.exn *)

    val progExterns : BOM.var CFunctions.c_fun list ref = ref []

    datatype env = E of {
        tyCache : tycache_t, (* map from AST types to BOM types *)
        tycMap : tycmap_t, (* map from AST tycs to BOM datatype constructors *)
        varCache : varcache_t, (* map from AST variables to BOM variables *)
        varEnv : BOMTy.t VMap.map, (* map from BOM variables to their BOM types *)
        bomValMap : bomvalmap_t, (* map from CoreBOM (frontend) values to BOM variables *)
        binds : (*var_bind*)BOM.exp VMap.map, (* map from BOM variables to their bound expressions *)
        exh : BOM.var (* current exception handler continuation *)
      }

    fun newHandler (E{tyCache, tycMap, varCache, varEnv, bomValMap, binds, exh=oldExh}) = let
          val exh = BV.new("_exh", exhTy (tyCache))
          val env = E{tyCache = tyCache, tycMap=tycMap, varCache=varCache, varEnv=varEnv, bomValMap=bomValMap, binds=binds, exh=exh}
          in
            (exh, env)
          end

    fun handlerOf (E{exh, ...}) = exh

    fun writeBind (E{tyCache, tycMap, varCache, varEnv, bomValMap, binds, exh}, x, x') = E{tyCache=tyCache, tycMap=tycMap, varCache=varCache, varEnv=varEnv, bomValMap=bomValMap, binds=VMap.insert(binds, x, x'), exh=exh}

    fun lookupBind (E{binds, ...}, x) : BOM.exp = (case VMap.find(binds, x)
           of SOME x' => x'
            | NONE => raise Fail(concat["lookupBind(env, ", BV.toString x, ") = NONE"])
          (* end case *))

    fun writeVarTy (E{tyCache, tycMap, varCache, varEnv, bomValMap, binds, exh}, var, ty) = E{tyCache=tyCache, tycMap=tycMap, varCache=varCache, varEnv=VMap.insert(varEnv, var, ty), bomValMap=bomValMap, binds=binds, exh=exh}

    fun lookupVarTy (E{varEnv, ...}, bomvar) = (case VMap.find(varEnv, bomvar)
           of SOME bomty => bomty
            | NONE => raise Fail(concat["lookupVarTy(env, ", BV.toString bomvar, ") = NONE"])
          (* end case *))

    fun lookupVar (E{varCache, ...}, sxmlvar : S.Var.t) : BOM.var = (case VarCache.find(varCache, sxmlvar)
           of SOME bomvar => bomvar
            | NONE => raise Fail(concat["lookupVar(env, ", Layout.toString (S.Var.layout sxmlvar), ") = NONE"])
          (* end case *))

    fun writeVar (E{tyCache, tycMap, varCache, varEnv, bomValMap, binds, exh}, sxmlvar, bomvar) = E{tyCache=tyCache, tycMap=tycMap, varCache=VarCache.insert(varCache, sxmlvar, bomvar), varEnv=varEnv, bomValMap=bomValMap, binds=binds, exh=exh}

    fun lookupBOMVal (E{bomValMap, ...}, bomVal : S.CoreBOM.Val.t) : BOM.var = (case BOMValMap.find(bomValMap, bomVal)
           of SOME bomvar => bomvar
            | NONE => raise Fail(concat["lookupBOMVal(env, ", S.CoreBOM.ValId.toString (S.CoreBOM.Val.idOf bomVal), ") = NONE"])
          (* end case *))

    fun writeBOMVal (E{tyCache, tycMap, varCache, varEnv, bomValMap, binds, exh}, bomVal, bomvar) = E{tyCache=tyCache, tycMap=tycMap, varCache=varCache, varEnv=varEnv, bomValMap=BOMValMap.insert(bomValMap, bomVal, bomvar), binds=binds, exh=exh}

  (***** Translation functions *****)

(*
    datatypes: {cons: {arg: Type.t option,
                    con: Con.t} vector,
             tycon: Tycon.t,
             tyvars: Tyvar.t vector} vector,
*)
    fun transTy (env as E{tyCache, tycMap, varCache, varEnv, bomValMap, binds, exh}, ty (*as {tycon, tyvars, cons}*)) =
    let
          (*val _ = print (concat["SXML.Type->BOMTy: ", Layout.toString (S.Type.layout ty), "\n"])*)
          (* convert an uncached type from the MLton represenation to a BOM type *)
          fun tr(tyCache: tycache_t, dest: S.Type.dest) = (case dest
            of S.Type.Var _ => raise Fail "Type variable found in SXML"
                | (S.Type.Con(tyc, args)) => let
                    val (tyCache': tycache_t, args': BOMTy.t list) = V.foldl (fn (ty, (tyCache, tys)) => let val (tyCache', ty') = tr (tyCache, S.Type.dest ty) in (tyCache', ty' :: tys) end) (tyCache, []) args
                    val nArgs = length args';
                    fun singleArg () = if nArgs = 1 then List.hd args' else raise Fail (concat["Expected exactly one argument to tycon ", S.Type.Tycon.toString tyc, ", got ", Int.toString nArgs])
                    fun noArgs () = if nArgs = 0 then () else raise Fail (concat["Expected zero arguments to tycon ", S.Type.Tycon.toString tyc, ", got ", Int.toString nArgs])
                    val ty' = if S.Tycon.equals(tyc, S.Tycon.arrow)
                        then let (* function type *)
                            val [domTy, rngTy] = args'
                            in
                              BOMTy.T_Fun([domTy], [exhTy (tyCache)], [rngTy])
                            end
                        else if S.Tycon.equals(tyc, S.Tycon.array)
                          then BOMTy.T_Con (BOMTyc.arrayTyc, [singleArg ()])
                        else if S.Tycon.equals(tyc, S.Tycon.bool)
                          then BOMTy.T_Con (boolTyc, [])
                        else if S.Tycon.equals(tyc, S.Tycon.cpointer)
                          then BOMTy.T_Con(BOMTyc.addrTyc, [singleArg ()])
                        else if S.Tycon.equals(tyc, S.Tycon.exn)
                          then (noArgs (); exhTy (tyCache))
                        else if S.Tycon.equals(tyc, S.Tycon.intInf)
                          then (noArgs (); raise Fail "TODO(wings): BOMTy.T_Bignum")
                        else if S.Tycon.equals(tyc, S.Tycon.list)
                          then let (* convert each specialization of 'a list to a Datatype *)
                            val dataTyc = BOMTyc.new(S.Tycon.toString tyc, 1)
                            in
                              (* XXX(wings): should data constructor names be obtained from somewhere else? *)
                              BOMDataCon.new dataTyc ((*S.Con.toString con1*)"nil", []);
                              BOMDataCon.new dataTyc ((*S.Con.toString con2*)"::", [singleArg ()]);
                              BOMTy.T_Con (dataTyc, []) (* XXX(wings): should this tyc have an arg? *)
                            end
                        else if S.Tycon.equals(tyc, S.Tycon.reff)
                          then BOMTy.T_Con(BOMTyc.arrayTyc, [singleArg()]) (* XXX(wings): should we really treat refs as one-item arrays? *)
                        else if S.Tycon.equals(tyc, S.Tycon.thread)
                          then (noArgs (); BOMTy.T_Con(BOMTyc.vprocTyc, []))
                        else if S.Tycon.equals(tyc, S.Tycon.tuple)
                          then BOMTy.T_Con(tupleTyc, args')
                        else if S.Tycon.equals(tyc, S.Tycon.vector)
                          then BOMTy.T_Con (BOMTyc.vectorTyc, [singleArg ()])
                        else if S.Tycon.equals(tyc, S.Tycon.weak)
                          then raise Fail "Weak pointer type found in SXML"
                        else BOMTy.T_Con(TycMap.lookup(tycMap, tyc), args')
                        (*else raise Fail (concat["Failed to translate SXML tycon ", S.Type.Tycon.toString tyc, " with ", Int.toString nArgs," arguments"])*)
                    in
                      (cacheTy (tyCache', ty, ty'), ty')
                    end
              (* end case *))
          in (case peekTy (tyCache, ty)
                 of SOME ty' => (env, ty')
                  | NONE => let
                      val (tyCache', ty') = tr (tyCache, S.Type.dest ty)
                    in
                      (E{tyCache=tyCache', tycMap=tycMap, varCache=varCache, varEnv=varEnv, bomValMap=bomValMap, binds=binds, exh=exh}, ty')
                    end
              (* end case *))
          end

     (*XXX(wings): should we set kind? one of:
        VK_None
      | VK_Let of exp
      | VK_RHS of rhs
      | VK_Param
      | VK_Fun of lambda
      | VK_Cont of lambda
      | VK_CFun of c_fun
      *)
    fun newVar (env, x, ty) = let
      val (env', ty') = transTy (env, ty)
      val x' = BV.new (S.Var.toString x ^ ".", ty')
      val env' = writeVar (env, x, x') (* construct explicit map from x to x' so x can be used to look up x' and x' can be used to look up the type *)
      in
        (x', writeVarTy (env', x', ty'))
      end

    fun transExp (e, env) : BOM.exp = let
          val {decs, result} = S.Exp.dest e
          fun transDecs (env, decs, result as S.VarExp.T{targs, var}) =
             (case decs of
                [] => (*lookupBind(env,*) BOM.mkRet [lookupVar(env, var)] (*)*)
                | d::rest => transDec (env, d, fn (env) => ((*print "k called\n";*) transDecs(env, rest, result)))
             (* end case *))
          in
            transDecs(env, decs, result)
          end

    and transDec (env, d, k : env -> BOM.exp) = ( case d
           of S.Dec.Exception{arg, con} => raise Fail "exception declaration?"
            | S.Dec.Fun{decs, ...} => let
                (*val _ = print "transDec Fun\n"*)
                fun bind ({var, ty, lambda}, (fns, env)) = let
                  val (var', env) = newVar(env, var, ty)
                  in
                    ((var', lambda)::fns, env)
                  end
                val (fns, env) = V.foldr bind ([], env) decs
                in
                  BOM.mkFun(List.map (transLambda env) fns, k env)
                end
            | S.Dec.MonoVal{var, ty, exp} => let
                (*val _ = print "transDec MonoVal\n"*)
                val (var', env) = newVar(env, var, ty)
                in
                  transPrimExp (env, var', ty, exp, k)
                end
            | S.Dec.BOM{bom=bomdecs} => let
                (*val _ = print "transDec BOM\n"*)
                val (env, k) = Vector.foldl (fn (bomdec, (env, k: env -> BOM.exp)) => (case bomdec
                   of S.CoreBOM.Definition.Fun fundefs => raise Fail "TODO(wings): translate CoreBOM.Definition.Fun"(*let
                       fun bind ({var, ty, lambda}, (fns, env)) = let
                         val (var', env) = newVar(env, var, ty)
                         in
                           ((var', lambda)::fns, env)
                         end
                       val (fns, env) = V.foldr bind ([], env) fundefs
                       in
                         BOM.mkFun(List.map (transLambda env) fns, env, k)
                       end*)
                    | S.CoreBOM.Definition.HLOp (attrs, valid, exp) => raise Fail "Polymorphic HLOps found in SXML"
                    | S.CoreBOM.Definition.Extern (bomVal, S.CoreBOM.CProto.T (cfun, attrs)) => let
                       (* translate the C function; place in env and global externs *)
                       val (cfun, env') = transCFun (cfun, attrs, env)
                       (* references to cfuns elsewhere in the program are via BOMVal,
                       so put a mapping from it to the cfun's var into the env *)
                       val env' = writeBOMVal(env', bomVal, CFunctions.varOf cfun)
                       in
                         progExterns := cfun :: !progExterns; (env', k)
                     end
                  (* end case *))) (env, k) bomdecs
                in
                  k env
                end
            | S.Dec.PolyVal _ => raise Fail "Polymorphic declaration found in SXML"
          (* end case *))

    (* translate a C function, put it in the global list of externs, return it and
    a modified environment in which it's bound *)
    and transCFun (S.CoreBOM.CFunction.T {convention, kind, target, prototype, ...}, attrs, env) = let
          fun transCTy cty = (case cty
             of S.CType.CPointer => CFunctions.PointerTy
              | S.CType.Int8 => CFunctions.BaseTy RawTypes.Int8
              | S.CType.Int16 => CFunctions.BaseTy RawTypes.Int16
              | S.CType.Int32 => CFunctions.BaseTy RawTypes.Int32
              | S.CType.Int64 => CFunctions.BaseTy RawTypes.Int64
              | S.CType.Objptr => raise Fail "objptr?"
              | S.CType.Real32 => CFunctions.BaseTy RawTypes.Float32
              | S.CType.Real64 => CFunctions.BaseTy RawTypes.Float64
              | S.CType.Word8 => CFunctions.BaseTy RawTypes.UInt8
              | S.CType.Word16 => CFunctions.BaseTy RawTypes.UInt16
              | S.CType.Word32 => CFunctions.BaseTy RawTypes.UInt32
              | S.CType.Word64 => CFunctions.BaseTy RawTypes.UInt64
            (* end case *))
          val name = (case target
             of S.CoreBOM.CFunction.Target.Direct name => name
              | S.CoreBOM.CFunction.Target.Indirect => raise Fail "indirect function encountered"
            (* end case *))

          (* convert argument and return types from MLton's C-function repr to BOM's *)
          val (argTys, retTy) = prototype
          val retTy' = (case retTy
             of SOME t => transCTy t
              | NONE => CFunctions.VoidTy
            (* end case *))
          val argTys' = Vector.foldl (fn (x, rest) => transCTy x :: rest) [] argTys

          (* destringify attributes *)
          val attrs' = List.mapPartial (fn(attr) => case attr
             of "pure" => SOME CFunctions.A_pure (* technically, we shouldn't ever see this case since MLton puts purity into the kind *)
              | "alloc" => SOME CFunctions.A_alloc
              | "noreturn" => SOME CFunctions.A_noreturn
              | _ => (print ("unknown extern function attribute: " ^ attr ^ "\n"); NONE)) attrs
          (* MLton reifies purity in kind whereas PML stores it in an attribute *)
          val attrs' = if kind = S.CFunction.Kind.Pure then CFunctions.A_pure::attrs' else attrs'

          (* build the appropriate BOM type and make a variable with it to place in the env *)
          val ty = BOMTy.T_CFun (CFunctions.CProto (retTy', argTys', attrs'))
          val var = BV.new ("extern " ^ name, ty)
          val env' = writeVarTy (env, var, ty) (* construct explicit map from x to x' so x can be used to look up x' and x' can be used to look up the type *)
          val cfun = BOM.mkCFun { var = var, name = name, retTy = retTy',
                                  argTys = argTys', varArg = false, attrs = attrs' }
          in
            (cfun, env')
          end

    and transLambda env (f, lambda) = let
          val {arg, argType, body, ...} = S.Lambda.dest lambda
          val (param, env) = newVar(env, arg, argType)
          val (exh, env) = newHandler env
          in
            BOM.mkLambda{f = f, params = [param], exh = [exh], body = transExp(body, env)}
          end

  (* translate a constant word value of a given size into a (Literal.literal * BOMTy.t) pair *)
  (* XXX(wings): make sure this handles [un]signedness properly *)
    and transConst (sz, word) : (Literal.literal * BOMTy.t) = let
          val lit = Literal.Int (S.Atoms.WordX.toIntInf word)
          val ty = (if sz = S.Atoms.WordSize.word8
              then BOMTy.T_Raw RawTypes.Int8
            else if sz = S.Atoms.WordSize.word16
              then BOMTy.T_Raw RawTypes.Int16
            else if sz = S.Atoms.WordSize.word32
              then BOMTy.T_Raw RawTypes.Int32
            else if sz = S.Atoms.WordSize.word64
              then BOMTy.T_Raw RawTypes.Int64
            else raise Fail "TODO(wings): BOMTy.T_Bignum")
        in
          (lit, ty)
        end

    (* TODO(wings): I have no idea how to make this work when maybeSxmlty is NONE, because conTycon seems to fail consistently on bool... *)
    and transDCon (env as E{tyCache, tycMap, varCache, varEnv, bomValMap, binds, exh}, con, maybeSxmlty, targs) : BOMDataCon.t = let
        (*val boolCons = PMLFrontEnd.tyconCons (S.Tycon.bool)*)
        val dest: S.Type.dest = case maybeSxmlty
          of NONE => S.Type.Con (PMLFrontEnd.conTycon con, targs)
           | SOME sxmlty => S.Type.dest sxmlty
        val tyName = "..."
        (*val tyName = Layout.toString (S.Type.layout sxmlty)*)
        val _ = print ("transDcon for ty " ^ tyName ^ "\n")
        val tycon = case maybeSxmlty
          of NONE => PMLFrontEnd.conTycon con
           | SOME sxmlty => S.Type.tycon sxmlty
        val bomTy = if S.Tycon.equals (tycon, S.Tycon.bool) then BOMTy.T_Con(boolTyc, []) else lookupTyDest (tyCache, dest)
        val (name, dcons) = case bomTy
          of BOMTy.T_Con (tyc, params) => (BOMTyc.nameOf tyc, BOMTyc.consOf tyc)
           | _ => raise Fail (concat ["Failed to translate data constructor for ", tyName, ".", S.Con.toString con])
      in
        case List.find (fn (dcon) => BOMDataCon.nameOf dcon = S.Con.toString con) dcons
          of SOME x => x
           | NONE => raise Fail (concat ["Failed to translate data constructor for ", tyName, ".", S.Con.toString con])
      end

  (* translate a PrimExp.t term.  These occur as the rhs of the a MonoVal binding; the
   * translated lhs variable is passed as an argument.
   *)
    and transPrimExp (env, lhs, sxmlty, e, k : env -> BOM.exp) : BOM.exp = let
          fun mkLet e' = BOM.mkLet([lhs], e', k env)
          fun mkStmt rhs = BOM.mkStmt([lhs], rhs, k env)
          val (env, _) = transTy(env, sxmlty)
          in
            case e
             of S.PrimExp.App{func, arg} => let
                  val func' = transVarExp(env, func)
                  val arg' = transVarExp(env, arg)
                  in
                    mkLet(BOM.mkApply(func', [arg'], [handlerOf env]))
                  end
              | S.PrimExp.Case{test, cases, default} => let
                  val test' = transVarExp(env, test)
                  val default' = Option.map (fn (e, region) => transExp(e, env)) default
                  val cases' = (case cases
                        (* for cases on inductive datatypes, ... *)
                         of S.Cases.Con (rules) => V.map (fn (pat as S.Pat.T{arg, con, targs}, exp) => let
                                 (*Pat.t = T of {
                                        arg: (Var.t * Type.t) option,
                                        con: Con.t,
                                        targs: Type.t vector
                                }*)
                                  val dcon=transDCon (env, con, NONE, targs)
                                  val vars=(case arg
                                    of NONE => []
                                     | SOME (var, sxmlty) => [lookupVar(env, var)] (* XXX(wings): unpack tuple into multiple vars? *)
                                   (* end case *))
                                in
                                  (* P_DCon of data_con * var list *)
                                  (BOM.P_DCon (dcon, vars), transExp(exp, env))
                                end) rules
                        (* for cases on int, word, char, ... *)
                          | S.Cases.Word (sz, rules) => V.map (fn (word, exp) => (BOM.P_Const(transConst(sz, word)), transExp(exp, env))) rules
                        (* end case *))
                  in
                    mkLet(BOM.mkCase(test', V.foldl (fn(x, y) => x::y) [] cases', default'))
                  end
                (* QUESTION: are there primitive dcons that have targs?  No! *)
              | S.PrimExp.ConApp{con, arg, targs} => let
                  val con' = transDCon (env, con, SOME sxmlty, targs)
                  in
                    (* translate dcon and tupled args *)
                    mkStmt(BOM.E_DCon(con', case arg
                       of SOME arg' => (* XXX(wings): unpack typaram tuple into its own list? *) [transVarExp(env, arg')]
                        | NONE => []))
                  end
              | S.PrimExp.Const c => let
                (* XXX(wings): verify that this produces the correct endianness. will fail obviously if at all, but whether it does is architecture-dependent. *)
                    fun bytesIn (w) = let
                                fun lit(n) = S.Atoms.WordX.fromChar (Char.chr n)
                                val byte = Word8.fromInt (S.Atoms.WordX.toInt (S.Atoms.WordX.andb(w, (lit 255))))
                                val rest = S.Atoms.WordX.rshift(w, (lit 8), {signed=false})
                          in
                            if S.Atoms.WordX.equals(rest, lit 0)
                            then [byte]
                            else byte::(bytesIn rest)
                          end
                  val lit = (case c
                         of S.Const.IntInf i => Literal.Int i
                          | S.Const.Null => Literal.StateVal 0w0 (* XXX(wings): there is no Addr literal but it seems like genLit in codegen-fn.sml will treat this properly *)
                          | S.Const.Real flt => Literal.Float (FloatLit.fromBytes (Word8Vector.fromList (bytesIn (valOf (S.Atoms.RealX.castToWord flt))))) (* XXX(wings): this is terrible. *)
                          | S.Const.Word w => Literal.Int (S.Atoms.WordX.toIntInf w)
                          | S.Const.WordVector v => Literal.String (S.Atoms.WordXVector.toString v) (* XXX(wings): can this corrupt strings? see word-x-vector.fun *)
                        (* end case *))
                  in
                    mkStmt(BOM.E_Const(lit, BV.typeOf lhs))
                  end
              | S.PrimExp.Handle{try, catch=(x, ty), handler} => let
                  val (x', handlerEnv) = newVar(env, x, ty)
                  val (exh, tryEnv) = newHandler env
                  in
                    mkLet(BOM.mkCont(
                      BOM.mkLambda{f = exh, params = [x'], exh = [], body = transExp(handler, handlerEnv)},
                      transExp(try, tryEnv)))
                  end
              | S.PrimExp.Lambda lambda => BOM.mkFun([transLambda env (lhs, lambda)], k env)
                (* XXX: These could be BOM HLOps? *)
              | S.PrimExp.PrimApp{prim, targs, args} => let
                  fun expFromRhs (rhs, ret) = let
                      val var = BV.new("rhs", ret)
                    in
                      BOM.mkStmt([var], rhs, BOM.mkRet[var])
                  end
                val args = V.foldl (fn (argi, rest) => transVarExp(env,argi)::rest) [] args
                fun argn(n) = List.nth(args, n)
                val unitTy = BOMTy.T_Con(tupleTyc, [])
                val boolTy = BOMTy.T_Con(boolTyc, [])
                fun r2e (rhs, ty): BOM.exp = mkLet(expFromRhs (rhs, ty))
                val exp =
                  if S.Prim.equals (prim, S.Prim.array) then raise Fail "array" else
                  if S.Prim.equals (prim, S.Prim.arrayLength) then raise Fail "arrayLength" else
                  if S.Prim.equals (prim, S.Prim.assign) then r2e (BOM.E_Prim (P.AdrStore(argn 0, argn 1)), unitTy) else
                  if S.Prim.equals (prim, S.Prim.bogus) then raise Fail "bogus" else
                  if S.Prim.equals (prim, S.Prim.bug) then r2e (BOM.E_CCall(BV.new("_ccall_bug", unitTy), []), unitTy) else
                  if S.Prim.equals (prim, S.Prim.deref) then r2e (BOM.E_Prim (P.AdrLoad (argn 0)), BV.typeOf (argn 0)) else
                  if S.Prim.equals (prim, S.Prim.reff) then r2e (BOM.E_Alloc (BV.typeOf (argn 0), [argn 0]), BOMTy.arrayTy(BV.typeOf (argn 0))) else

                  if S.Prim.equals (prim, S.Prim.equal) then BOM.mkIf (Prim.Equal (argn 0, argn 1),
                                                                        expFromRhs (BOM.E_Const (Literal.trueLit, boolTy), boolTy),
                                                                        expFromRhs (BOM.E_Const (Literal.falseLit, boolTy), boolTy)) else

                  if S.Prim.equals (prim, S.Prim.touch) then r2e (BOM.E_Promote (argn 0), unitTy) (* TODO: the backend needs a notion of 'touch' *) else
                  (*raise Fail "ref" else *)
                  if S.Prim.equals (prim, S.Prim.cpointerSub) then raise Fail "cpointerSub" else
                raise Fail ("failed to translate PrimApp " ^ S.Prim.toString prim)
                in
                  exp
                end
              | S.PrimExp.Profile info => k env (* ignore for now *)
                (* QUESTION: what does the "extend" flag mean? *)
              | S.PrimExp.Raise{exn, ...} =>
                  BOM.mkThrow (handlerOf env, [transVarExp(env, exn)])
              | S.PrimExp.Select{offset, tuple} =>
                  mkStmt(BOM.E_Select(offset, transVarExp(env, tuple)))
              | S.PrimExp.Tuple args =>
                  mkStmt(BOM.E_Alloc(
                    BV.typeOf lhs,  (* type *)
                    V.foldr (fn (x, xs) => transVarExp(env, x)::xs) [] args))
              | S.PrimExp.Var x => mkLet(BOM.mkRet[transVarExp(env, x)])
              | S.PrimExp.BOMVal bomVal => mkLet(BOM.mkRet[lookupBOMVal(env, bomVal)])
            (* end case *)
          end

    and transVarExp (env, S.VarExp.T{var, ...}) = lookupVar(env, var)

(*
Datatype.t = {cons: {arg: Type.t option,
                       con: Con.t} vector,
                tycon: Tycon.t,
                tyvars: Tyvar.t vector}
*)

    fun transDatatype ({cons, tycon, tyvars}, env) = let
            val () = print ("transDatatype " ^ S.Tycon.toString tycon ^ " '" ^ Int.toString (V.length tyvars) ^ "\n")
        (* XXX(wings): remove nullary dcon count *)
        fun isNullary {arg, con} = not (isSome arg)
        val nNullary = V.foldl (fn (elem, n) => n + (if isNullary elem then 1 else 0)) 0 cons
        val dataTyc = BOMTyc.new(S.Tycon.toString tycon, V.length tyvars)
        val E{tyCache, tycMap, varCache, varEnv, bomValMap, binds, exh} = env;
        val tyCache' = cacheTyDest(tyCache, S.Type.Con(tycon, V.fromList []), BOMTy.T_Con(dataTyc, [](* TODO(wings): convert these lazily so we can handle type arguments instead of assuming none! *)))
        val env = E{tyCache=tyCache', tycMap=TycMap.insert(tycMap, tycon, dataTyc), varCache=varCache, varEnv=varEnv, bomValMap=bomValMap, binds=binds, exh=exh}
        val env = V.foldl (fn({con: S.Con.t, arg: S.Type.t option}, env) => (case arg
              of SOME a => let
                   val (env, a') = transTy (env, a)
                 in
                   (* this mutates dataTyc's cons which is a ref-cell, and returns the data_con it stores: *)
                   BOMDataCon.new dataTyc (S.Con.toString con, [a']);
                   (* XXX(wings): unpack typaram tuple into its own list? *)
                   env
                 end
               | NONE => (BOMDataCon.new dataTyc (S.Con.toString con, [(* no arguments *)]); env)
               (* end case *))) env cons
        in
            env
        end

(* QUESTION: what does the overflow option mean?  Used to keep Overflow exn from being
 * shaken out.
 *)
    fun translate (S.Program.T{datatypes, overflow, body}) = let
          val env = E{tyCache = TyCache.empty, tycMap=TycMap.empty, varCache = VarCache.empty, varEnv = VMap.empty, bomValMap=BOMValMap.empty, binds = VMap.empty, exh = BV.new("_exh", exhTy0) }
          (* initially translate all the datatypes listed by MLton *)
          val env as E{tycMap=tycMap, ...} = V.foldl transDatatype env datatypes
          val body = transExp (body, env)
          (* val argTy = BOMTy.T_Raw RawTypes.Int32 *)
          (* val arg = BV.new("_arg", argTy) *)
          val mainFun = BOM.FB{
                  f = BV.new("main", BOMTy.T_Fun([(* argTy *)], [(* BOMTy.exhTy *)], [(* TypeOf.exp body *)])),
                  params = [(* arg *)],
                  exh = [(* exh *)],
                  body = body
                }
          val program = BOM.PROGRAM{exnTyc = exnTyc, dataTycs = TycMap.listItems tycMap, hlops = [], externs = !progExterns, body = mainFun}
          in(* raise Fail "hi";*)
            PrintBOM.print (program); program (*raise Fail "done"*)
          end
        end
