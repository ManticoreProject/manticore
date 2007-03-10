(* print-cfg.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure PrintCFG : sig

    type flags = {types : bool}

    val output : flags -> (TextIO.outstream * CFG.module) -> unit

    val print : CFG.module -> unit

  end = struct

    type flags = {types : bool}

    fun output (flags : flags) (outS, CFG.MODULE{name, code}) = let
	  fun pr s = TextIO.output(outS, s)
	  fun prl s = pr(String.concat s)
	  fun prIndent 0 = ()
	    | prIndent n = (pr "  "; prIndent(n-1))
	  fun indent i = prIndent i
	  fun prList toS [] = pr "()"
	    | prList toS [x] = pr("("^toS x^")")
	    | prList toS l = let
		fun prL [] = ()
		  | prL [x] = pr(toS x)
		  | prL (x::r) = (pr(toS x); pr ","; prL r)
		in
		  pr "("; prL l; pr ")"
		end
	  fun varBindToString x = if (#types flags)
		then String.concat[
		    CFG.Var.toString x, ":", CFGTy.toString(CFG.Var.typeOf x)
		  ]
		else CFG.Var.toString x
	  fun varUseToString x = CFG.Var.toString x
	  fun labelToString lab = "$" ^ (CFG.Label.toString lab)
	  fun prFunc (CFG.FUNC{lab, entry, body, exit}) = let
		val (kind, params) = (case (CFG.Label.kindOf lab, entry)
		       of (CFG.LK_Local{export=SOME name, ...}, CFG.StdFunc{clos, arg, ret, exh}) =>
			    ("export function ", [clos, arg, ret, exh])
			| (CFG.LK_Local _, CFG.StdFunc{clos, arg, ret, exh}) =>
			    ("function ", [clos, arg, ret, exh])
			| (CFG.LK_Local _, CFG.StdCont{clos, arg}) => ("cont ", [clos, arg])
			| (CFG.LK_Local _, CFG.KnownFunc args) => ("local function ", args)
			| (CFG.LK_Local _, CFG.Block args) => ("block ", args)
			| _ => raise Fail "bogus function"
		      (* end case *))
		in
		  indent 1;
		  pr kind;
		  prl [labelToString lab, " "]; prList varBindToString params; pr "\n";
		  List.app (prExp 2) body;
		  prXfer (2, exit)
		end
	  and prExp i e = (
		indent i;
		pr "let "; prList varBindToString (CFG.lhsOfExp e); pr " = ";
		case e
		 of (CFG.E_Var(_, ys)) => prList varUseToString ys
		  | (CFG.E_Enum(_, w)) => prl["enum(", Word.fmt StringCvt.DEC w, ")"]
		  | (CFG.E_Cast(_, ty, y)) => prl["(", CFGTy.toString ty, ")", varUseToString y]
		  | (CFG.E_Label(_, lab)) => pr(labelToString lab)
		  | (CFG.E_Literal(_, lit)) => pr(Literal.toString lit)
		  | (CFG.E_Select(_, i, x)) =>
		      prl ["#", Int.toString i, " ", varUseToString x]
		  | (CFG.E_Alloc(_, args)) => (pr "alloc"; prList varUseToString args)
		  | (CFG.E_Wrap(_, y)) => prl["wrap(", varUseToString y, ")"]
		  | (CFG.E_Unwrap(_, y)) => prl["unwrap(", varUseToString y, ")"]
		  | (CFG.E_Prim(_, p)) => pr (PrimUtil.fmt varUseToString p)
		  | (CFG.E_CCall(_, f, args)) => (
		      prl ["ccall ", varUseToString f, " "];
		      prList varUseToString args)
		(* end case *);
		pr "\n")
	  and prXfer (i, xfer) = (
		indent i;
		case xfer
		 of CFG.StdApply{f, clos, arg, ret, exh} =>
		      prApply("apply", f, [clos, arg, ret, exh])
		  | CFG.StdThrow{k, clos, arg} =>
		      prApply("throw", k, [clos, arg])
		  | CFG.Apply{f, args} => prApply("apply", f, args)
		  | CFG.Goto jmp => prJump("goto", jmp)
		  | CFG.HeapCheck{szb, nogc} => (
		      pr "check (avail-mem < "; pr(Word.fmt StringCvt.DEC szb); pr ")\n";
		      indent (i+1); pr "then GC()\n";
		      indent (i+1); prJump("else", nogc))
		  | CFG.If(x, j1, j2) => (
		      prl ["if ", varUseToString x, "\n"];
		      indent (i+1); prJump("then", j1);
		      indent (i+1); prJump("else", j2))
		  | CFG.Switch(x, cases, dflt) => let
		      fun prCase (c, jmp) = (
			    indent (i+1);
			    prl ["case ", Int.toString c, ": "];
			    prJump("", jmp))
		      in
			prl ["switch ", varUseToString x, "\n"];
			List.app prCase cases;
			case dflt
			 of NONE => ()
			  | SOME jmp => (indent(i+1); prJump("default: ", jmp))
			(* end case *)
		      end
		(* end case *))
	  and prApply (prefix, x, args) = (
		prl [prefix, " ", varUseToString x];
		prList varUseToString args;
		pr "\n")
	  and prJump (prefix, (lab, args)) = (
		prl [prefix, " ", labelToString lab];
		prList varUseToString args;
		pr "\n")
	  in
	    prl ["module ", Atom.toString name, " {\n"];
	    List.app prFunc code;
	    pr "}\n"
	  end

    fun print m = output {types=false} (TextIO.stdErr, m)

  end
