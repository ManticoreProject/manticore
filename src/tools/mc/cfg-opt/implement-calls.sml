(* implement-calls.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Implement calling conventions.
 *)

structure ImplementCalls : 
   sig
      val transform : CFG.module -> CFG.module
   end = 
struct

   structure Label = CFG.Label
   structure Var = CFG.Var

   fun transform (m as CFG.MODULE{name, externs, code}) =
      let
         fun transTyArgs (args : CFGTy.ty list) : CFGTy.ty =
            case args of
               [] => CFGTy.unitTy
             | [arg] => arg
             | args => CFGTy.T_Tuple (false, List.map transTy args)
         and transTy (ty : CFGTy.ty) : CFGTy.ty =
            case ty of
               CFGTy.T_Any => CFGTy.T_Any
             | CFGTy.T_Enum w => CFGTy.T_Enum w
             | CFGTy.T_Raw rt => CFGTy.T_Raw rt
             | CFGTy.T_Wrap rt => CFGTy.T_Wrap rt
             | CFGTy.T_Tuple (m, tys) => CFGTy.T_Tuple (m, List.map transTy tys)
             | CFGTy.T_OpenTuple tys => CFGTy.T_OpenTuple (List.map transTy tys)
             | CFGTy.T_Addr ty => CFGTy.T_Addr (transTy ty)
             | CFGTy.T_CFun cp => CFGTy.T_CFun cp
             | CFGTy.T_VProc => CFGTy.T_VProc
             | CFGTy.T_StdFun {clos, args, ret, exh} =>
                  CFGTy.T_StdFun {clos = transTy clos, args = [transTyArgs args],
                                  ret = transTy ret, exh = transTy exh}
             | CFGTy.T_StdCont {clos, args} =>
                  CFGTy.T_StdCont {clos = transTy clos, args = [transTyArgs args]}
             | CFGTy.T_Code tys => CFGTy.T_Code (List.map transTy tys)

         fun updVarType v = Var.setType (v, transTy (Var.typeOf v))
         fun updLabelType l = Label.setType (l, transTy (Label.typeOf l))

         fun transFormalArgs (args : CFG.var list) : (CFG.var * CFG.exp list) =
            case args of
               [] => 
                  let
                     val argTy = CFGTy.unitTy
                     val arg = CFG.Var.new ("argFormalUnit", argTy)
                  in
                     (arg, [])
                  end
             | [arg] => 
                  let
                     val () = updVarType arg
                  in
                     (arg, [])
                  end
             | args => 
                  let
                     val () = List.app updVarType args
                     val argTy = CFGTy.T_Tuple (false, List.map (transTy o Var.typeOf) args)
                     val arg = CFG.Var.new ("argFormalTuple", argTy)
                     val (_, sels) =
                        List.foldr
                        (fn (a, (i, sels)) => (i + 1, (CFG.mkSelect (a, i, arg)) :: sels))
                        (0, [])
                        args
                  in
                     (arg, List.rev sels)
                  end
         fun transConvention (c : CFG.convention) : (CFG.convention * CFG.exp list) =
            case c of
               CFG.StdFunc {clos, args, ret, exh} =>
                  let
                     val () = updVarType clos
                     val (arg, binds) = transFormalArgs args
                     val () = updVarType ret
                     val () = updVarType exh
                  in
                     (CFG.StdFunc {clos = clos, args = [arg], ret = ret, exh = exh}, 
                      binds)
                  end
             | CFG.StdCont {clos, args} =>
                  let
                     val () = updVarType clos
                     val (arg, binds) = transFormalArgs args
                  in
                     (CFG.StdCont {clos = clos, args = [arg]}, 
                      binds)
                  end
             | _ => 
                  let
                     val () = List.app updVarType (CFG.paramsOfConv c)
                  in
                     (c, [])
                  end
         fun transExp (exp : CFG.exp) : CFG.exp =
            (List.app updVarType (CFG.lhsOfExp exp);
             case exp of
                CFG.E_Cast (x, ty, y) => CFG.mkCast (x, transTy ty, y)
              | _ => exp)
         fun transActualArgs (args : CFG.var list) : (CFG.exp list * CFG.var) =
            case args of
               [] => 
                  let
                     val argTy = CFGTy.unitTy
                     val arg = CFG.Var.new ("argActualUnit", argTy)
                  in
                     ([CFG.mkConst (arg, Literal.unitLit)], arg)
                  end
             | [arg] => ([], arg)
             | args => 
                  let
                     val argTy = CFGTy.T_Tuple (false, List.map Var.typeOf args)
                     val arg = CFG.Var.new ("argActualTuple", argTy)
                  in
                     ([CFG.mkAlloc (arg, args)], arg)
                  end
         fun transTransfer (t : CFG.transfer) : (CFG.exp list * CFG.transfer) =
            case t of
               CFG.StdApply {f, clos, args, ret, exh} => 
                  let
                     val (binds, arg) = transActualArgs args
                  in
                     (binds, CFG.StdApply {f = f, clos = clos, args = [arg], ret = ret, exh = exh})
                  end
             | CFG.StdThrow {k, clos, args} => 
                  let
                     val (binds, arg) = transActualArgs args
                  in
                     (binds, CFG.StdThrow {k = k, clos = clos, args = [arg]})
                  end
             | _ => ([], t)
         fun transFunc (CFG.FUNC {lab, entry, body, exit} : CFG.func) : CFG.func =
            let
               val () = updLabelType lab
               val (entry, entryBinds) = transConvention entry
               val body = List.map transExp body
               val (exitBinds, exit) = transTransfer exit
            in
               CFG.mkFunc (lab, entry, entryBinds @ body @ exitBinds, exit)
            end
      in
         CFG.mkModule (name, externs, List.map transFunc code)
      end
   
   val transform =
      BasicControl.mkKeepPassSimple
      {output = PrintCFG.output {types=true},
       ext = "cfg",
       passName = "implementCalls",
       pass = transform,
       registry = CFGOptControls.registry}
      
end