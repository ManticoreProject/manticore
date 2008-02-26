(* implement-calls.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Implement calling conventions.
 *  + StdFunc/StdApply -- require a single, uniform argument.
 *  + StdCont/StdThrow -- require a single, uniform argument.
 *     - zero arguments are replaced by a unit argument
 *     - a single raw argument is replaced by a wrapped argument
 *     - multiple arguments are replaced by a tupled argument 
 *  + KnownFunc --
 *     - fewer than or equal to Target.maxGPRArgs arguments
 *        - all arguments are passed in registers
 *          - an integral raw argument is preserved
 *          - a floating-point or vector raw argument is replaced by a wrapped argument
 *          - a non-raw argument is preserved
 *     - more than Target.maxGPRArgs arguments
 *        - Target.maxGPRArg - 1 arguments are passed in registers
 *          - an integral raw argument is preserved
 *          - a floating-point or vector raw argument is replaced by a wrapped argument
 *          - a non-raw argument is preserved
 *        - remaining arguments are replaced by a tupled argument, which is passed in a register
 *  + Block -- arbitrary (handled by codegen)
 *
 *)

functor ImplementCallsFn (Target : TARGET_SPEC) : sig

      val transform : CFG.module -> CFG.module

   end = struct

   structure Label = CFG.Label
   structure Var = CFG.Var

    fun wrapRaw rt = CFGTy.T_Tuple(false, [CFGTy.T_Raw rt])

   fun transform (m as CFG.MODULE{name, externs, code}) = let
         fun transTyUniformArg (arg : CFGTy.ty) : CFGTy.ty = (case arg
		 of CFGTy.T_Raw rt => wrapRaw rt
                  | _ => transTy arg
		(* end case *))
         and transTyStdArgs (args : CFGTy.ty list) : CFGTy.ty =
            case args of
               [] => CFGTy.unitTy
             | [arg] => transTyUniformArg arg
             | args => CFGTy.T_Tuple (false, List.map transTy args)
         and transTyNonUniformArg (arg : CFGTy.ty) : CFGTy.ty =
            case arg of
               CFGTy.T_Raw rt => let
                  fun wrap () = wrapRaw rt
                  fun keep () = CFGTy.T_Raw rt
                  in
                    case rt
		     of RawTypes.T_Byte => keep ()
                      | RawTypes.T_Short => keep ()
                      | RawTypes.T_Int => keep ()
                      | RawTypes.T_Long => keep ()
                      | RawTypes.T_Float => wrap ()
                      | RawTypes.T_Double => wrap ()
                      | RawTypes.T_Vec128 => wrap ()
                  end
             | _ => transTy arg
         and transTyKFncArgs (args: CFGTy.ty list) : CFGTy.ty list =
            let
               fun loop (args, gprs) =
                  if gprs = 1 andalso length args > 1
                     then [transTyStdArgs args]
                  else (case args of
                           [] => []
                         | arg::args => 
                              (transTyNonUniformArg arg) ::
                              (loop (args, gprs - 1)))
            in
               loop (args, Target.maxGPRArgs - 1 (* for the closure *))
            end
         and transTy (ty : CFGTy.ty) : CFGTy.ty =
            case ty of
               CFGTy.T_Any => CFGTy.T_Any
             | CFGTy.T_Enum w => CFGTy.T_Enum w
             | CFGTy.T_Raw rt => CFGTy.T_Raw rt
             | CFGTy.T_Tuple (m, tys) => CFGTy.T_Tuple (m, List.map transTy tys)
             | CFGTy.T_OpenTuple tys => CFGTy.T_OpenTuple (List.map transTy tys)
             | CFGTy.T_Addr ty => CFGTy.T_Addr (transTy ty)
             | CFGTy.T_CFun cp => CFGTy.T_CFun cp
             | CFGTy.T_VProc => CFGTy.T_VProc
             | CFGTy.T_StdFun {clos, args, ret, exh} =>
                  CFGTy.T_StdFun {clos = transTy clos, args = [transTyStdArgs args],
                                  ret = transTy ret, exh = transTy exh}
             | CFGTy.T_StdCont {clos, args} =>
                  CFGTy.T_StdCont {clos = transTy clos, args = [transTyStdArgs args]}
             | CFGTy.T_KnownFunc {clos, args} => 
                  CFGTy.T_KnownFunc {clos = transTy clos, args = transTyKFncArgs args}
             | CFGTy.T_Block {args} => CFGTy.T_Block {args = List.map transTy args}

         local
            val {getFn, peekFn, setFn, clrFn, ...} = 
               Var.newProp (fn v => let
                                       val oldTy = Var.typeOf v
                                       val newTy = transTy oldTy
                                    in
                                       Var.setType (v, newTy);
                                       {oldTy = oldTy,
                                        newTy = newTy}
                                    end)
         in
            val getVarOldType = #oldTy o getFn
            val getVarNewType = #newTy o getFn
            val updVarType = ignore o getFn
         end
         local
            val {getFn, peekFn, setFn, clrFn, ...} = 
               Label.newProp (fn l => let
                                         val oldTy = Label.typeOf l
                                         val newTy = transTy oldTy
                                      in
                                         Label.setType (l, newTy);
                                         {oldTy = oldTy,
                                          newTy = newTy}
                                      end)
         in
            val getLabelOldType = #oldTy o getFn
            val getLabelNewType = #newTy o getFn
            val updLabelType = ignore o getFn
         end

         fun transFormalUniformArg (arg : CFG.var) : (CFG.var * CFG.exp list) =
            case getVarOldType arg of
               CFGTy.T_Raw rt => 
                  let
                     val newArgTy = wrapRaw rt
                     val newArg = CFG.Var.new ("argFormalWrap", newArgTy)
                  in 
                     (newArg, [CFG.mkUnwrap(arg,newArg)])
                  end 
             | _ => (arg, [])
         fun transFormalStdArgs (args : CFG.var list) : (CFG.var * CFG.exp list) =
            case args of
               [] => 
                  let
                     val newArgTy = CFGTy.unitTy
                     val newArg = CFG.Var.new ("argFormalUnit", newArgTy)
                  in
                     (newArg, [])
                  end
             | [arg] => transFormalUniformArg arg
             | args => 
                  let
                     val newArgTy = CFGTy.T_Tuple (false, List.map getVarNewType args)
                     val newArg = CFG.Var.new ("argFormalTuple", newArgTy)
                     val (_, sels) =
                        List.foldl
                        (fn (arg, (i, sels)) => 
                         (i + 1, (CFG.mkSelect (arg, i, newArg)) :: sels))
                        (0, [])
                        args
                  in
                     (newArg, List.rev sels)
                  end
         fun transFormalNonUniformArg (arg : CFG.var) : (CFG.var * CFG.exp list) =
            case getVarOldType arg of
               CFGTy.T_Raw rt => 
                  let
                     fun wrap () =
                        let
                           val newArgTy = wrapRaw rt
                           val newArg = CFG.Var.new ("argFormalWrap", newArgTy)
                        in 
                           (newArg, [CFG.mkUnwrap(arg,newArg)])
                        end 
                     fun keep () = (arg, [])
                  in
                     case rt of
                        RawTypes.T_Byte => keep ()
                      | RawTypes.T_Short => keep ()
                      | RawTypes.T_Int => keep ()
                      | RawTypes.T_Long => keep ()
                      | RawTypes.T_Float => wrap ()
                      | RawTypes.T_Double => wrap ()
                      | RawTypes.T_Vec128 => wrap ()
                  end
             | _ => (arg, [])
         fun transFormalKFncArgs (args : CFG.var list) : (CFG.var list * CFG.exp list) =
            let
               fun loop (args, gprs) =
                  if gprs = 1 andalso length args > 1
                     then let 
                             val (arg, binds) = transFormalStdArgs args
                          in 
                             ([arg], binds)
                          end
                  else (case args of
                           [] => ([],[])
                         | arg::args => 
                              let
                                 val (arg,bindsArg) = transFormalNonUniformArg arg
                                 val (args,bindsArgs) = loop (args, gprs - 1)
                              in
                                 (arg :: args, bindsArg @ bindsArgs)
                              end)
            in
               loop (args, Target.maxGPRArgs - 1 (* for the closure *))
            end
         fun transConvention (c : CFG.convention) : (CFG.convention * CFG.exp list) =
            (List.app updVarType (CFG.paramsOfConv c);
             case c of
                CFG.StdFunc {clos, args, ret, exh} =>
                   let
                      val (arg, binds) = transFormalStdArgs args
                   in
                      (CFG.StdFunc {clos = clos, args = [arg], ret = ret, exh = exh}, 
                       binds)
                   end
              | CFG.StdCont {clos, args} =>
                   let
                      val (arg, binds) = transFormalStdArgs args
                   in
                      (CFG.StdCont {clos = clos, args = [arg]}, 
                       binds)
                   end
              | CFG.KnownFunc {clos, args} =>
                   let
                      val (args, binds) = transFormalKFncArgs args
                   in
                      (CFG.KnownFunc {clos = clos, args = args}, 
                       binds)
                   end
              | _ => (c, []))
         fun transExp (exp : CFG.exp) : CFG.exp =
            (List.app updVarType (CFG.lhsOfExp exp);
             case exp of
                CFG.E_Cast (x, ty, y) => CFG.mkCast (x, transTy ty, y)
              | _ => exp)
         fun transActualUniformArg (arg : CFG.var) : (CFG.exp list * CFG.var) =
            case getVarOldType arg of
               CFGTy.T_Raw rt => 
                  let
                     val newArgTy = wrapRaw rt
                     val newArg = CFG.Var.new ("argActualWrap", newArgTy)
                  in 
                     ([CFG.mkWrap(newArg, arg)], newArg)
                  end 
             | _ => ([], arg)
         fun transActualStdArgs (args : CFG.var list) : (CFG.exp list * CFG.var) =
            case args of
               [] => 
                  let
                     val newArgTy = CFGTy.unitTy
                     val newArg = CFG.Var.new ("argActualUnit", newArgTy)
                  in
                     ([CFG.mkConst (newArg, Literal.unitLit)], newArg)
                  end
             | [arg] => transActualUniformArg arg
             | args => 
                  let
                     val newArgTy = CFGTy.T_Tuple (false, List.map getVarNewType args)
                     val newArg = CFG.Var.new ("argActualTuple", newArgTy)
                  in
                     ([CFG.mkAlloc (newArg, args)], newArg)
                  end
         fun transActualNonUniformArg (arg : CFG.var) : (CFG.exp list * CFG.var) =
            case getVarOldType arg of
               CFGTy.T_Raw rt => 
                  let
                     fun wrap () =
                        let
                           val newArgTy = wrapRaw rt
                           val newArg = CFG.Var.new ("argActualWrap", newArgTy)
                        in 
                           ([CFG.mkWrap(newArg, arg)], newArg)
                        end 
                     fun keep () = ([], arg)
                  in
                     case rt of
                        RawTypes.T_Byte => keep ()
                      | RawTypes.T_Short => keep ()
                      | RawTypes.T_Int => keep ()
                      | RawTypes.T_Long => keep ()
                      | RawTypes.T_Float => wrap ()
                      | RawTypes.T_Double => wrap ()
                      | RawTypes.T_Vec128 => wrap ()
                  end
             | _ => ([], arg)
         fun transActualKFncArgs (args : CFG.var list) : (CFG.exp list * CFG.var list) =
            let
               fun loop (args, gprs) =
                  if gprs = 1 andalso length args > 1
                     then let 
                             val (binds, arg) = transActualStdArgs args
                          in 
                             (binds, [arg])
                          end
                  else (case args of
                           [] => ([],[])
                         | arg::args => 
                              let
                                 val (bindsArg,arg) = transActualNonUniformArg arg
                                 val (bindsArgs,args) = loop (args, gprs - 1)
                              in
                                 (bindsArg @ bindsArgs, arg :: args)
                              end)
            in
               loop (args, Target.maxGPRArgs - 1 (* for the closure *))
            end
         fun transTransfer (t : CFG.transfer) : (CFG.exp list * CFG.transfer) =
            case t of
               CFG.StdApply {f, clos, args, ret, exh} => 
                  let
                     val (binds, arg) = transActualStdArgs args
                  in
                     (binds, CFG.StdApply {f = f, clos = clos, args = [arg], 
                                           ret = ret, exh = exh})
                  end
             | CFG.StdThrow {k, clos, args} => 
                  let
                     val (binds, arg) = transActualStdArgs args
                  in
                     (binds, CFG.StdThrow {k = k, clos = clos, args = [arg]})
                  end
             | CFG.Apply {f, clos, args} =>
                  let
                     val (binds, args) = transActualKFncArgs args
                  in
                     (binds, CFG.Apply {f = f, clos = clos, args = args})
                  end
             | _ => ([], t)
	  fun transFunc (CFG.FUNC {lab, entry, body, exit} : CFG.func) : CFG.func = let
		val () = updLabelType lab
		val (entry, entryBinds) = transConvention entry
		val body = List.map transExp body
		val (exitBinds, exit) = transTransfer exit
		val export = (case CFG.Label.kindOf lab
		       of CFG.LK_Local{export, ...} => export
			| _ => raise Fail "bogus label kind"
		      (* end case *))
		in
		   CFG.mkFunc (lab, entry, entryBinds @ body @ exitBinds, exit, export)
		end
	  val module = CFG.mkModule (name, externs, List.map transFunc code)
	  in
	    module
	  end
      
  end
