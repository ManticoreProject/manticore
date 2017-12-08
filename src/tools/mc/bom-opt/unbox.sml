(* unbox.sml
 *
 * COPYRIGHT (c) 2017 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

functor UnboxFn (Spec : TARGET_SPEC) : sig

  (* A simplistic pass that unboxes (or flattens) some arguments
     and/or the return type for known functions. 
     
     This is essentially a less sophisticated version
     of arity raising that exists for the CPS IR.
     
     This pass depends on accurate Census information,
     and expects a BOM contraction pass to be run
     immediately afterwards.
   *)
    val transform : BOM.module -> BOM.module

  end = struct

    structure B = BOM
    structure BV = B.Var
    structure M = BV.Map
    structure BTy = BOMTy
    structure TU = BOMTyUtil
    structure C = Census
    structure L = List
    structure ST = Stats
    
    val cntSigChanges       = ST.newCounter "unbox:funs-changed"
    
    (********** Get variable info **********)
    fun useCntRef (VarRep.V{useCnt, ...}) = useCnt
    fun useCntOf v = !(useCntRef v)
    val appCntOf = BV.appCntOf
    
    (* functions to update census counts *)
    fun inc x = BV.addToCount(x, 1)
    fun dec x = BV.addToCount(x, ~1)
    
    local 
        (** translation environment utils **)
        type newParams = (BV.var * (BTy.ty list)) option list
        type newRet    = BTy.ty list option
        type newSig    = newParams * newRet
        
        datatype gamma = E of { info : newSig M.map, cxt : BV.var option }
    in
        val emptyEnv = E { info = M.empty, cxt = NONE }
        
        fun insertSig (E{info, cxt}, f, sgn) = E{info = M.insert(info, f, sgn),
                                                cxt = cxt}
                                                
        fun findSig (E{info,...}, f) = M.find(info, f) 
                                                
        fun setCxt (E{info,...}, cxt') = E{info=info, cxt=cxt'}
        
        fun getCxt (E{cxt,...}) = cxt
        
        fun getCxtRetTy env = 
            (case getCxt env
                of SOME func => 
                    (case findSig (env, func)
                        of SOME (_, SOME [rty]) => SOME rty
                         | _ => NONE
                        (* esac *))
                 | _ => NONE
                 (* esac *))
 
    end
    
    
    (* we take the min because right now I don't
       count parameter types based on their eventual register assignment. *)
    val maxParams = Int.min(Spec.maxFPRArgs, Spec.maxGPRArgs)
    
    (* utility functions *)  
    
    fun unwrap f p = (case BV.typeOf p
        of BTy.T_Tuple(false, tys) => SOME (p, L.map f tys)
         | _ => NONE
        (* end case *))
    and newParam ty = let
        val var = BV.newWithKind("unboxP", B.VK_Param, ty)
    in
        (inc var ; var)  (* it will be used exactly once later on. *)
    end
    
    and newArg ty = let
        val var = BV.new("unboxA", ty)
    in
        (inc var; var)
    end
    
    and justTy ty = ty
    
    (* we only look for functions that return exactly one value: 
       a tuple with a single element. *)
    and unwrapRet func = (case BV.typeOf func
        of BTy.T_Fun(_,_,[rty]) => (case rty
            of BTy.T_Tuple(false, [innerTy]) => SOME [innerTy]
             | _ => NONE
            (* esac *))
         | _ => NONE
        (* esac *))
    
    and withNewTy (f, newParams, maybeNewRet) = let
            val (_, exhTys, oldRets) = BOMTyUtil.asFunTy (BV.typeOf f)
            val newTy = BTy.T_Fun (L.map BV.typeOf newParams, exhTys,
                                   getOpt (maybeNewRet, oldRets))
        in
            (BV.setType (f, newTy) ; f)
        end
    
    and newSignature (params, unwrapped) = let
        fun lp ([], [], new) = new
          | lp (p::ps, repl as ((v, items)::ws), new) =
                if BV.same(p, v) then
                    lp(ps, ws, new @ items)
                else
                    lp(ps, repl, new @ [p])
          | lp (ps, [], new) = new @ ps 
    in
        lp (params, unwrapped, [])
    end
      
    fun shouldUnbox (B.FB{f,params,...}) = let
            fun count ((_, items), tot) = L.length items + tot
            val newParams = L.mapPartial (unwrap justTy) params
            val numUnboxed = L.length newParams
            val totalParams = L.length params 
                              + L.foldl count 0 newParams
                              - numUnboxed
            
            val maybeNewRetTy = unwrapRet f
            val uses = useCntOf f
        in
            (* TODO: be lazy about computing the above stuff for more speed. *)
            
            uses > 0                   (* don't bother with dead functions, or "main" *)
            andalso uses = appCntOf f  (* this means its call-sites are obviously known *)
            andalso (numUnboxed > 0 orelse isSome maybeNewRetTy)    (* we actually unbox something *)
            andalso totalParams <= maxParams
        end
    (*****************************)
    
    
    (* transform functions *)
    
    fun doFn env (fb as B.FB{f, params, exh, body}) =
        if (not o shouldUnbox) fb then
            B.FB {f=f, params=params, exh=exh, body = doExp (env, body)}
        else let
            (* all we do is move the binding of unboxed 
               parameters into the function body:
               
                fun f (param : int * int) = 
                    let x = #0(param)
                    let y = #1(param)
                    ...
                
                ~~ turns into ~~>
                
                fun f (unbox1 : int, unbox2 : int) = 
                    let param = alloc(unbox1, unbox2)
                    let x = #0(param)
                    let y = #1(param)
                    ...
                    
                We can get away with this because BOM's contract
                pass will clean this up, since it already looks through
                a select to see if it can find a local alloc.
             *)
            
            val unboxed = L.mapPartial (unwrap newParam) params
            val params' = newSignature (params, unboxed)
                     
            fun replaceParam ((p, items), acc) =
                ([p], B.E_Alloc(BV.typeOf p, items)) :: acc
            
            val bodyFixups = L.foldr replaceParam [] unboxed
            val body' = B.mkStmts (bodyFixups, body)

        in
            B.FB {f = withNewTy (f, params', unwrapRet f), 
                  params = params', 
                  exh = exh, 
                  body = doExp (env, body')
                 }
        end
        
        
    and doExp (env, B.E_Pt(ppt, term)) = let 
        fun withP exp = B.E_Pt(ppt, exp)
        fun collectSigs (fb as B.FB{f, params, ...}, env) = 
            if shouldUnbox fb
                then (ST.tick cntSigChanges ; 
                      insertSig(env, f, (L.map (unwrap justTy) params, unwrapRet f))
                      )
                else env
    in
      case term
        of B.E_Let (v, e1, e2)      => withP (B.E_Let (v, doExp (setCxt(env, NONE), e1), 
                                                            doExp (env, e2)))
         | B.E_Stmt (lhs, rhs, exp) => withP (B.E_Stmt(lhs, rhs, doExp (env, exp)))
         | B.E_Fun (funs, exp)      => let
                val newEnv = L.foldl collectSigs env funs
                fun unboxIt (fb as B.FB{f,...}) = doFn (setCxt (newEnv, SOME f)) fb
             in
                withP (B.E_Fun (L.map unboxIt funs, doExp (newEnv, exp)))
             end
            
         | B.E_Cont (B.FB{f, params, exh, body}, exp) => let
                val newBody = doExp (env, body)
                val k = B.FB{f=f, params=params, exh=exh, body=newBody}
            in
                withP (B.E_Cont(k, doExp (env, exp)))
            end
         | B.E_If (c, e1, e2)       => withP (B.E_If(c, doExp (env, e1), doExp (env, e2)))
         | B.E_Case (v, arms, maybe)  => withP (B.E_Case(
                v,
                L.map (fn (p, exp) => (p, doExp (env, exp))) arms,
                Option.map (fn exp => doExp (env, exp)) maybe
             ))
         | B.E_Apply (xs as (f, _, _)) => (case findSig(env, f)
            of SOME newSig => doApply (env, newSig, withP) xs
             | NONE => withP term
            (* end case *))
            
         | B.E_Ret [var] => (case getCxtRetTy env
                (* this return is in the context of a function who's return type changed *)
             of SOME rty => doReturn (withP, rty, var)
              | NONE => withP term (* this return is either not in the context of a func,
                                      or is in the context of a func that did not change. *)
             (* esac *))
         | _ => withP term
    end
    
    and doReturn (withP, rty, var) = let
        val rv = BV.new("unboxRet", rty)
        val _ = inc rv
    in
        B.mkStmt([rv], B.E_Select(0, var),
            withP (B.E_Ret [rv]))
    end
         
    and doApply (env, (newPs, newRet), withP) (f, args, exh) = let
        (* Similar to the above, instead of introducing an alloc,
           we introduce selects for the arguments: 
           
           let tpl = alloc(int1, int2)
           let x = apply f (tpl)
           ...
           
           ~~ turns into ~~>
           
           let tpl = alloc(int1, int2)
           let unbox1 = #0(tpl)
           let unbox2 = #1(tpl)
           let x = 
               let x' = apply f (unbox1, unbox2)
               let box = alloc (x')
               return (box)
            ...
           
           and then contract cleans it up later. Note that a tail-position
           apply will turn into a non-tail position to box up the argument, if needed.
           This does not effect self-recursive functions.
        *)
        
        fun matchArgs (SOME (p, tys), arg) = SOME (BV.typeOf p, arg, L.map newArg tys)
          | matchArgs (NONE, _) = NONE
          
        val unboxed = ListPair.mapPartialEq matchArgs (newPs, args)
        
        
        fun replaceArg ((paramTy, tpl, items), acc) = let
                val (castTpl, castBinds) = maybeCast (paramTy, tpl)
                val (_, stms) = List.foldl (selectFrom castTpl) (0, []) items
            in
                castBinds @ stms @ acc
            end
            
        and selectFrom tpl (arg, (i, stms)) = (
                inc tpl ;
                (i+1, ([arg], B.E_Select(i, tpl)) :: stms)
            )
            
        and maybeCast (paramTy, tpl) = 
            if TU.equal(paramTy, BV.typeOf tpl)
                then (dec tpl ; (tpl, []))
                else let
                        val newV = BV.new("cast", paramTy)
                        val newStm = ([newV], B.E_Cast(paramTy, tpl))
                    in
                        (newV, [newStm])
                    end
        
        val argBinds = L.foldr replaceArg [] unboxed
        
        val unboxed = L.map (fn (_, b, c) => (b, c)) unboxed
        val args' = newSignature (args, unboxed)
        
        fun withNewRet ty = let
            val rv = BV.new("unboxRV", ty)
            val box = BV.new("rebox", BTy.T_Tuple(false, [ty]))
            val appExp = 
                B.mkLet([rv], withP (B.E_Apply(f, args', exh)),
                    B.mkStmt([box], B.E_Alloc(BV.typeOf box, [rv]),
                        B.mkRet([box])))
                        
            val _ = (inc rv ; inc box)
        in
            B.mkStmts (argBinds, appExp)
        end
        
        (* we avoid the nested-let in this case *)
        and withoutRet () = 
            B.mkStmts (argBinds,
                withP (B.E_Apply(f, args', exh)))
    in
        case newRet
          of SOME [newTy] => (case getCxtRetTy env
              (* it's a tail apply *)
              of SOME cxtTy => if TU.match(newTy, cxtTy)
                    (* the encl function's type matches. *)
                    then withoutRet ()
                    else withNewRet newTy  (* encl type doesn't match, rebox *)
               (* non-tail apply, rebox *)
               | NONE => withNewRet newTy
              (* esac *))
              
           (* no return type change *)
           | NONE       => withoutRet ()
    end

    fun transform (m as B.MODULE{name, externs, hlops, rewrites, body}) = 
    (* There's a bug somewhere deep in the implementation of parray and its hand-written
       basis that is exposed by this pass. I gave up trying to debug it because
       it's not relevant for me. It seems to only manifest when compiling a parallel
       program that uses parray (specifying a range like [| 1 to 10 |] is all it takes).
        ~kavon (12/7/17) *)
        if Controls.get BasicControl.noparray 
            orelse Controls.get BasicControl.sequential
            then let
                val newBody = doFn emptyEnv body
                (* we iterate at most twice to unwrap things like [[int],[int]] *)
                val newBody = 
                    if ST.count cntSigChanges > 0
                    then doFn emptyEnv newBody (* try once more *)
                    else newBody
            in
                B.MODULE {
                    name = name,
                    externs = externs,
                    hlops = hlops,
                    rewrites = rewrites,
                    body = newBody
                }
            end
        else m

  end
