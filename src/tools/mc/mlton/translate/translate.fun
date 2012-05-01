(* translate.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

signature TRANSLATE_STRUCTS =
  sig
      include SXML_STRUCTS
  end

functor Translate (S: SXML_STRUCTS) : sig

    val translate : S.Program.t -> BOM.module

end = struct

open S

structure B = BOM
structure BTy = BOMTy
structure BV = BOM.Var


fun doProg (Program.T{body, datatypes, overflow}) = let
      fun doConExp (c: Con.t) = ()
      (* Ignore targs because they are always empty in SXML *)
      fun doVarExp (VarExp.T {var, ...}) = ()
      fun doVarExps xs = Vector.app doVarExp xs

      fun doPat (p as Pat.T {con, targs, arg}) =
         let
            val t = doConExp con
         in
             ()
         end
      fun doExp (exp: Exp.t) = let
          val {decs, result} = Exp.dest exp
      in
          List.app doDec decs;
          doVarExp result
      end
      and doPrimExp (e: PrimExp.t) = let
          val _ = 1 (* FIXME *)
      in
          case e of
              PrimExp.App {arg, func} => (doVarExp func ; doVarExp arg)
            | PrimExp.Case {cases, default, test} => let
                  val _ = doVarExp test
                  val _ = Option.app (fn (e,_) => doExp e) default
                  in
                     case cases of
                        Cases.Con cases =>
                        (Vector.app (fn (p, e) =>
                                        (doPat p; doExp e)) cases)
                      | Cases.Word (_, cs) =>
                           (Vector.app (fn (_, e) => doExp e) cs)
                  end
             | PrimExp.ConApp {con, targs, arg} =>
                  let
                     val t = doConExp con
                  in
                      Option.app doVarExp arg
                  end
             | PrimExp.Const c => ()
             | PrimExp.Handle {try, catch = (c, catchType), handler, ...} =>
                  let
                     val _ = doExp try
                     val _ = doExp handler
                  in
                      ()
                  end
             | PrimExp.Lambda l => doLambda l
             | PrimExp.PrimApp {args, prim, targs} =>
                  let
                     val _ = doVarExps args
                  in
                     ()
                  end
             | PrimExp.Profile _ => ()
             | PrimExp.Raise {exn, ...} =>
               doVarExp exn
             | PrimExp.Select {tuple, offset} =>
               doVarExp tuple
             | PrimExp.Tuple xs =>
               doVarExps xs
             | PrimExp.Var x => doVarExp x
         end
      and doLambda (l: Lambda.t) = let
          val {arg, argType, body, ...} = Lambda.dest l
      in
          doExp body
      end
      and doDec (d: Dec.t) = (
          case d of
              Dec.Exception {arg,con} => doConExp con
            | Dec.Fun {tyvars, decs} => let
                  fun doFunDec {lambda, ty, var} =
                      doLambda lambda
              in
                  Vector.app doFunDec decs
              end
            | Dec.MonoVal {var, ty, exp} => (
              doPrimExp exp)
            | Dec.PolyVal _ =>
              raise Fail "SXML should never have a PolyVal")
      fun doDatatype {tycon, tyvars, cons} = 
          Vector.app (fn {con,arg} => doConExp con)  cons
in
    Vector.app doDatatype datatypes;
    doExp body
end

fun translate (sxml:S.Program.t) = let
    val name = Atom.atom "main"
    val f = BV.new ("f", BTy.T_Any)
    val body = B.E_Ret []
    val bodyLam = B.FB {f=f,
                        params=[],
                        exh=[],
                        body=B.E_Pt (ProgPt.new (), body)}
    val _ = print "About to walk the SXML\n"
    val _ = doProg sxml
    val _ = print "Done\n"
in
    B.MODULE {name = name ,
              externs = [],
              hlops = [],
              rewrites = [],
              body = bodyLam}
end
                         
(*    val translate = BasicControl.mkKeepPass {
	              preOutput = fn (outS, (_, ast)) => PrintAST.outputExp(outS, ast),
	              preExt = "sxml",
	              postOutput = PrintBOM.output,
	              postExt = "bom",
	              passName = "translate",
	              pass = translate,
	              registry = TranslateControls.registry
	              }*)

end
