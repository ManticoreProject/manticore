(* print-bom.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure PrintPT : sig

    val output : TextIO.outstream * ProgramParseTree.PML2.decl list -> unit
    val print : ProgramParseTree.PML2.decl list -> unit

  end = struct

    structure PT = ProgramParseTree.PML2
    structure BPT = PT.BOMParseTree
    structure Var = ProgramParseTree.Var

    fun output' (outS, decl) = let
	  fun pr s = TextIO.output(outS, s)
	  fun prl s = pr(String.concat s)
	  fun prIndent 0 = ()
	    | prIndent n = (pr "  "; prIndent(n-1))
	  fun indent i = prIndent i
	  fun prList' toS [] = ()
	    | prList' toS [x] = toS x
	    | prList' toS l = let
		fun prL [] = ()
		  | prL [x] = toS x
		  | prL (x::r) = (toS x; pr ","; prL r)
		in
		  prL l
		end
	  fun prList toS [] = pr "()"
	    | prList toS [x] = toS x
	    | prList toS l = let
		fun prL [] = ()
		  | prL [x] = toS x
		  | prL (x::r) = (toS x; pr ","; prL r)
		in
		  pr "("; prL l; pr ")"
                end
          fun prSign sign =  (
              case sign
               of PT.MarkSig {tree,...} => prSign tree
                | PT.NameSig (id, tydecls) => (
                  prVar id; pr " "; prList prTyDecl tydecls)
                | PT.ExpSig (specs) => prList prSpec specs)
          and prVar v = pr (Var.toString v)
          and prSpec spec : unit = (
              case spec
               of PT.MarkSpec {tree,...} => prSpec tree
                | PT.IncludeSpec (sign) => (pr "Include " ; prSign sign ; pr "\n")
                | PT.ModuleSpec (name, sign) => (
                  pr "Module "; prVar name;
                  pr "\n"; prSign sign; pr "\n")
                | PT.TypeSpec (tys) => (
                  pr "TypeSpec "; prTyDecl tys; pr "\n")
                | PT.ConstSpec (con, tyvars) => (
                  pr "Const "; prVar con; " = "; prList prAtom tyvars; pr "\n")
                | PT.ValSpec (v, tv, typ) =>(
                  pr "Val "; prVar v; " = "; prList prAtom tv; " : "; prTy typ; pr "\n"))
          and prModule module = (
              case module
               of PT.MarkMod {tree,...} => prModule tree
                | PT.DeclsMod (decls) => List.app prDecl decls
                | PT.NamedMod (modu) => (
                  pr "Named: "; prVar modu; pr "\n")
                | PT.ApplyMod (modu, mods) => (
                  pr "Apply: "; prVar modu; pr "\n";
                  List.app prModule mods))
          and prDecl (decl : PT.decl) = (
              case decl
               of PT.MarkDecl {tree,...} => prDecl tree
                | PT.ModuleDecl (mb, signOpt, module) => (
                  pr "module "; prVar mb;
                  (case signOpt
                    of SOME sign => prSign sign
                     | _ => ());
                  pr " = "; prModule module; pr "\n")
                | PT.TyDecl (decl) => (prTyDecl decl; pr "\n")
                | PT.ExnDecl (con, tyOpt) => (
                  pr "exception "; prVar con;
                  (case tyOpt
                    of SOME ty => (pr " : "; prTy ty)
                     | _ => ());
                  pr "\n")
                | PT.ValueDecl (vald) => prValDecl vald
                | PT.LocalDecl (decls1, decls2) => (
                  pr "local\n"; List.app prDecl decls1;
                  pr "in\n"; List.app prDecl decls2;
                  pr "end\n")
                | PT.SignDecl (id, sgn) => (
                  prVar id; " = "; prSign sgn; pr "\n")
                | PT.PrimCodeDecl (bomCode) => (
                  pr "FIXME: printing inline bom...\n")
                | PT.ExpansionOptsDecl (_, _) => (
                  pr "Compiler-injected expanion opts...\n"))
          and prAtom a = pr (Atom.toString a)
          and prTyDecl tyd = (
              case tyd
               of PT.MarkTyDecl {tree,...} => prTyDecl tree
                | PT.TypeTyDecl (tyvars, tyb, ty) => (
                  pr "type "; prVar tyb; pr " "; prList prAtom tyvars;
                  pr " : "; prTy ty; pr "\n")
                | PT.DataTyDecl (decls) => let
                      fun prDataTy (tyvars, name, cons) = (
                          pr "datatype "; prVar name;
                          pr " "; prList prAtom tyvars; pr " ";
                          prList prConDecl cons; pr "\n"
                      )
                  in
                      List.app prDataTy decls
                  end
                | PT.DataTyReplDecl (bind, use) => (
                  pr "datatypeRepl "; prVar bind; " = "; prVar use; pr "\n")
                | PT.AbsTyDecl (tyvars, binder) => (
                  pr "absty "; prVar binder; " = "; prList prAtom tyvars; pr "\n")
                | PT.PrimTyDecl (tyvars, binder, bomty) => (
                  pr "primty "; prVar binder; " = "; prList prAtom tyvars; pr "\n"))
          and prConDecl con = (
              case con
               of PT.MarkConDecl {tree,...} => prConDecl tree
                | PT.ConDecl (binder, tyOpt) => (
                  pr "condecl "; prVar binder;
                  case tyOpt
                   of SOME ty => (pr ":"; prTy ty)
                    | _ => ();
                  pr "\n"))
          and prValDecl vald = (
              case vald
               of PT.MarkVDecl {tree,...} => prValDecl tree
                | PT.ValVDecl (pat, exp) => (
                  pr "val "; prPat pat; pr " = ";
                  prExp exp; pr "\n")
                | PT.PValVDecl (pat, exp) => (
                  pr "pval "; prPat pat; pr " = ";
                  prExp exp; pr "\n")
                | PT.PrimVDecl (pat, bomprim) => (
                  pr "prim "; prPat pat; pr " = bomprim\n")
                | PT.FunVDecl (funs) => List.app prFun funs)
          and prFun func = (
              case func
               of PT.MarkFunct {tree,...} => prFun tree
                | PT.Funct (functs) => let
                      fun prFunct(name, pats, tyOpt, e) = (
                          pr "fun "; prVar name; prList prPat pats;
                          case tyOpt
                           of SOME ty => (pr " : "; prTy ty)
                            | _ => ();
                          pr " = \n"; prExp e; pr "\n")
                  in
                      List.app prFunct functs
                  end)
          and prTy ty = (
              case ty
               of PT.MarkTy {tree,...} => prTy tree
                | PT.NamedTy (tys, name) => (
                  prVar name; pr " : "; prList prTy tys)
                | PT.VarTy (tyvar) => prAtom tyvar
                | PT.TupleTy(tys) => prList prTy tys
                | PT.FunTy(argsTy, retTy) => (
                  prTy argsTy; pr " -> "; prTy retTy))
          and prExp exp = (
              case exp
               of PT.MarkExp {tree,...} => prExp tree
                | PT.LetExp (valdecls, exp) => (
                  pr "let\n";
                  List.app prValDecl valdecls;
                  pr "in\n";
                  prExp exp; pr "end\n")
                | PT.IfExp (c, t, e) => (
                  pr "if "; prExp c; pr "\n";
                  pr "then "; prExp t; pr "\n";
                  pr "else "; prExp e; pr "\n")
                | PT.CaseExp (e, matches) => (
                  pr "case "; prExp e; pr "\n";
                  List.app prMatch matches)
                | PT.PCaseExp (exps, pmatches) => (
                  pr "pcase "; prList prExp exps; pr "\n";
                  List.app prPMatch pmatches)
                | PT.HandleExp (exp, matches) => (
                  prExp exp; pr " handle "; prList prMatch matches)
                | PT.RaiseExp (exp) => (
                  pr "raise "; prExp exp)
                | PT.AndAlsoExp (e1, e2) => (
                  prExp e1; pr " andalso "; prExp e2)
                | PT.OrElseExp  (e1, e2) => (
                  prExp e1; pr " orelse "; prExp e2)
                | PT.BinaryExp (e1, oper, e2) => (
                  prExp e1; pr " "; prVar oper; pr " "; prExp e2)
                | PT.PChoiceExp (exps) => (
                  pr "pchoice "; prList prExp exps)
                | PT.ApplyExp (e1, e2) => (
                  prExp e1; pr "("; prExp e2; pr ")")
                | PT.ConstExp (const) => prConst const
                | PT.TupleExp (exps) => prList prExp exps
                | PT.ListExp (exps) => (pr "["; prList' prExp exps; pr "]")
                | PT.RangeExp (e1, e2, eopt) => (
                  pr "from "; prExp e1; " to "; prExp e2;
                  case eopt
                   of SOME e => (pr " by "; prExp e)
                    | _ => ())
                | PT.PTupleExp (exps) => (pr "(|"; prList' prExp exps; pr "|)")
                | PT.PArrayExp (exps) => (pr "[|"; prList' prExp exps; pr "|]")
                | PT.PCompExp (exp, pbinds, eopt) => (
                  pr "{"; prExp exp; pr " | where "; prList' prPBind pbinds;
                  case eopt
                   of SOME e => (pr " by "; prExp e)
                    | _ => (); pr "}")
                | PT.SpawnExp (exp) => (pr "spawn "; prExp exp)
                | PT.SeqExp (exps) => prList' prExp exps
                | PT.IdExp (v) => prVar v
                | PT.ConstraintExp (e, ty) => (prExp e; pr " : "; prTy ty)
                | PT.FnExp (fns) => let
                      fun prFn (pat, exp) = (
                          pr "fn "; prPat pat; pr " = ";
                          prExp exp; pr "\n")
                  in
                      List.app prFn fns
                  end)
          and prMatch match = (
              case match
               of PT.MarkMatch {tree,...} => prMatch tree
                | PT.Match (pat, exp) => (
                  pr "match "; prPat pat; pr " "; prExp exp))
          and prPMatch pmatch = (
              case pmatch
               of PT.MarkPMatch {tree,...} => prPMatch tree
                | PT.PMatch (ppats, exp) => (
                  pr "pmatch "; prList prPPat ppats; pr " "; prExp exp)
                | PT.Otherwise (exp) => (
                  pr "otherwise "; prExp exp))
          and prPBind pbind = (
              case pbind
               of PT.MarkPBind {tree,...} => prPBind tree
                | PT.PBind (pat, exp) => (
                  pr "pbind "; prPat pat; pr " "; prExp exp))
          and prPat pat = (
              case pat
               of PT.MarkPat {tree,...} => prPat tree
                | PT.BinaryPat (pat1, con, pat2) => (
                  prPat pat1; pr " "; prVar con; pr " "; prPat pat2)
                | PT.ConPat (con, pat) => (
                  prVar con; pr " "; prPat pat)
                | PT.TuplePat (pats) => prList prPat pats
                | PT.ConstPat (const) => (pr "("; prConst const; pr ")")
                | PT.WildPat => pr "(_)"
                | PT.IdPat (var) => (pr "("; prVar var; pr ")")
                | PT.ConstraintPat (pat, ty) => (prPat pat; pr " : "; prTy ty))
          and prPPat ppat = (
              case ppat
               of PT.MarkPPat {tree,...} => prPPat tree
                | PT.NDWildPat => pr "?"
                | PT.Pat (pat) => prPat pat)
          and prConst const = (
              case const
               of PT.IntLit (i) => pr (IntInf.toString i)
                | PT.FltLit (fl) => pr (FloatLit.toString fl)
                | PT.StrLit (str) => pr str)
	  in
            prDecl decl
	  end

    fun output (strm, l) = List.app (fn (l') => output' (strm, l')) l
    fun print l = output (TextIO.stdOut, l)
  end
