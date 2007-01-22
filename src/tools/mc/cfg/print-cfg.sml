(* print-cfg.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure PrintCFG : sig

    type flags = {prog_pts : bool}

    val output : flags -> (TextIO.outstream * CFG.module) -> unit

    val print : CFG.module -> unit

  end = struct

    type flags = {prog_pts : bool}

    fun output (flgs : flags) (outS, CFG.MODULE{code, ...}) = let
	  fun pr s = TextIO.output(outS, s)
	  fun prl s = pr(String.concat s)
	  fun prIndent 0 = ()
	    | prIndent n = (pr "  "; prIndent(n-1))
	  fun indent i = if (#prog_pts flgs)
		then prIndent(i + 8)
		else prIndent i
	  fun indentWithPPt (ppt, i) = if (#prog_pts flgs)
		then (
		  pr (StringCvt.padRight #" " 8 (ProgPt.toString ppt ^ ":"));
		  prIndent i)
		else prIndent i
	  fun prList toS [] = pr "()"
	    | prList toS [x] = pr(toS x)
	    | prList toS l = let
		fun prL [] = ()
		  | prL [x] = pr(toS x)
		  | prL (x::r) = (pr(toS x); pr ","; prL r)
		in
		  pr "("; prL l; pr ")"
		end
(* FIXME: add type info *)
	  fun varBindToString x = CFG.Var.toString x
	  fun varUseToString x = CFG.Var.toString x
	  val labelToString = CFG.Label.toString
	  fun prFunc (CFG.FUNC{lab, kind, params, body}) = (
		indent 1;
		case (CFG.Label.kindOf lab, kind)
		 of (CFG.Export name, CFG.StandardFunc) => pr "export function "
		  | (CFG.Local, CFG.KnownFunc) => pr "local function "
		  | (CFG.Local, CFG.StandardFunc) => pr "function "
		  | (CFG.Local, CFG.ContFunc) => pr "cont "
		  | _ => raise Fail "bogus function/label kind"
		(* end case *);
		prl [labelToString lab, " "]; prList varBindToString params; pr "\n";
		prExp (2, body))
	  and prExp (i, CFG.Exp(ppt, e)) = (
		indentWithPPt (ppt, i);
		case e
		 of CFG.E_Let(lhs, rhs, e) => (
		      pr "let "; prList varBindToString lhs; pr " = "; prRHS rhs; pr "\n";
		      prExp (i, e))
		  | CFG.E_HeapCheck(sz, e) => (
		      pr "check "; pr(Word.fmt StringCvt.DEC sz); pr "\n";
		      prExp (i, e))
		  | CFG.E_If(x, j1, j2) => (
		      prl ["if ", varUseToString x, "\n"];
		      indent (i+1); prJump("then", j1);
		      indent (i+1); prJump("else", j2))
		  | CFG.E_Switch(x, cases, dflt) => let
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
		  | CFG.E_Apply jmp => prApply("apply", jmp)
		  | CFG.E_Throw jmp => prApply("throw", jmp)
		  | CFG.E_Goto jmp => prJump("goto", jmp)
		(* end case *))
	  and prRHS (CFG.E_Var x) = pr(varUseToString x)
	    | prRHS (CFG.E_Label lab) = pr(labelToString lab)
	    | prRHS (CFG.E_Literal lit) = pr(Literal.toString lit)
	    | prRHS (CFG.E_Select(i, x)) =
		prl ["#", Int.toString i, " ", varUseToString x]
	    | prRHS (CFG.E_Alloc(ty, args)) = pr "<alloc>" (* FIXME *)
	    | prRHS (CFG.E_Prim p) = pr (Prim.fmt varUseToString p)
	    | prRHS (CFG.E_CCall(f, args)) = (
		prl ["ccall ", varUseToString f, " "];
		prList varUseToString args)
	  and prApply (prefix, (x, args)) = (
		prl [prefix, " ", varUseToString x];
		prList varUseToString args;
		pr "\n")
	  and prJump (prefix, (lab, args)) = (
		prl [prefix, " ", labelToString lab];
		prList varUseToString args;
		pr "\n")
	  in
	    pr "module {\n";
	    List.app prFunc code;
	    pr "}\n"
	  end

    fun print m = output {prog_pts=false} (TextIO.stdErr, m)

  end
