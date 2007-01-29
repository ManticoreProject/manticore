(* print-cps.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure PrintCPS : sig

    val print : CPS.module -> unit

  end = struct

    fun output (outS, CPS.MODULE body) = let
	  fun pr s = TextIO.output(outS, s)
	  fun prl s = pr(String.concat s)
	  fun prIndent 0 = ()
	    | prIndent n = (pr "  "; prIndent(n-1))
	  fun indent i = prIndent i
	  fun prList toS [] = pr "()"
	    | prList toS [x] = pr(toS x)
	    | prList toS l = let
		fun prL [] = ()
		  | prL [x] = pr(toS x)
		  | prL (x::r) = (pr(toS x); pr ","; prL r)
		in
		  pr "("; prL l; pr ")"
		end
	  fun varBindToString x = String.concat[
		  CPS.Var.toString x, ":", CPSTy.toString(CPS.Var.typeOf x)
		]
	  fun varUseToString x = CPS.Var.toString x
	  fun prExp (i, e) = (
		indent i;
		case e
		 of CPS.Let(xs, rhs, e) => (
		      pr "let "; prList varBindToString xs; pr " = "; prRHS rhs; pr "\n";
		      prExp (i, e))
		  | CPS.Fun(fb::fbs, e) => (
		      prLambda(i, "fun ", fb);
		      List.app (fn fb => prLambda(i, "and ", fb)) fbs;
		      prExp (i, e))
		  | CPS.Cont(fb, e) => (prLambda(i, "cont ", fb); prExp (i, e))
		  | CPS.If(x, e1, e2) => prIf(i, x, e1, e2)
		  | CPS.Switch(x, cases, dflt) => let
		      fun prCase (c, e) = (
			    indent (i+1);
			    prl ["case ", Int.toString c, ":\n"];
			    prExp (i+2, e))
		      in
			prl ["switch ", varUseToString x, "\n"];
			List.app prCase cases;
			case dflt
			 of NONE => ()
			  | SOME e => (indent(i+1); pr "default:\n"; prExp(i+2, e))
			(* end case *)
		      end
		  | CPS.Apply(f, args) => (
		      prl["apply ", varUseToString f, " "];
		      prList varUseToString args;
		      pr "\n")
		  | CPS.Throw(k, args) => (
		      prl["throw ", varUseToString k, " "];
		      prList varUseToString args;
		      pr "\n")
		(* end case *))
	  and prRHS (CPS.Var ys) = prList varUseToString ys
	    | prRHS (CPS.Literal lit) = pr(Literal.toString lit)
	    | prRHS (CPS.Select(i, y)) = prl ["#", Int.toString i, "(", varUseToString y, ")"]
	    | prRHS (CPS.Alloc ys) = (pr "alloc"; prList varUseToString ys)
	    | prRHS (CPS.Wrap y) = prl["wrap(", varUseToString y, ")"]
	    | prRHS (CPS.Unwrap y) = prl["unwrap(", varUseToString y, ")"]
	    | prRHS (CPS.Prim p) = pr (PrimUtil.fmt varUseToString p)
	    | prRHS (CPS.CCall(f, args)) = (
		prl ["ccall ", varUseToString f, " "];
		prList varUseToString args)
	  and prLambda (i, prefix, (f, params, e)) = (
		prl [prefix, varUseToString f, " "];
		prList varBindToString params;
		pr " =\n";
		prExp (i+2, e))
	  and prIf (i, x, e1, CPS.If(y, e2, e3)) = (
		prl ["if ", varUseToString x, " then\n"];
		prExp(i+1, e1);
		indent (i); pr "else "; prIf(i, y, e2, e3))
	    | prIf (i, x, e1, e2) = (
		prl ["if ", varUseToString x, " then\n"];
		prExp(i+1, e1);
		indent (i); pr "else\n"; prExp(i+1, e2))
	  in
	    prLambda (0, "module ", body)
	  end

    fun print m = output (TextIO.stdErr, m)

  end
