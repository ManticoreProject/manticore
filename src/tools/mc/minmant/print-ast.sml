(* print-ast.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure PrintAST (* : sig

    val output : TextIO.outstream * AST.module -> unit
    val print  : AST.module -> unit

  end *) = struct

    structure A = AST
    structure P = PrettyPrint

    val (^^, ^/^) = (P.^^, P.^/^)
    infixr ^^ ^/^

    (* catw : string -> doc list -> doc  *)
    fun catw sep [] = P.empty
      | catw sep [d] = d
      | catw sep (d::ds) = d ^^ (P.text sep) ^^ (catw sep ds)

    (* delim : string * string -> P.doc -> P.doc *)
    fun delim (l,r) d = (P.text l) ^^ d ^^ (P.text r)

    (* exp : A.exp -> P.doc *)
    fun exp (A.IfExp (ec, et, ef, t)) =
	(P.text "if ") ^^ (exp ec) ^^ (P.text " then") ^^
          (P.nest 2 (P.break ^^ exp et)) ^/^
        (P.text "else") ^^
          (P.nest 2 (P.break ^^ exp ef))
      | exp (A.ApplyExp (e1, e2, t)) = exp e1 ^^ P.text " " ^^ exp e2
      | exp (A.TupleExp es)  = delim ("(", ")")   (catw "," (map exp es))
      | exp (A.PTupleExp es) = delim ("(|", "|)") (catw "," (map exp es))
      | exp (A.ConstExp c) = const c
      | exp (A.VarExp (v, ts)) = var v
      | exp _ = raise Fail "todo"

    (* const : A.const -> P.doc *)
    and const (A.DConst (d, ts)) = raise Fail "todo"
      | const (A.LConst (l, _)) = P.text (Literal.toString l)

    and var (VarRep.V {name, ...}) = P.text name

    fun module m = exp m ^^ P.break

    fun output (outS, m) = TextIO.output (outS, P.toString (module m, 80))

    fun print m = output (TextIO.stdErr, m)

    (* tests *)
		  
    fun int n = A.ConstExp (A.LConst (Literal.Int n, Basis.intTy))
    val t = A.TupleExp [int 0, int 1, int 5, int 7, int 9]

    fun test0 () = print t

    fun test1 () = print (A.PTupleExp [t, t])

    fun test2 () = print (A.PTupleExp [A.PTupleExp [t, int 18], 
				       A.TupleExp [int 12, int 20, t]])
	

    fun test3 () = print (A.IfExp (int 0,
				   int 1,
				   int 2,
				   Basis.intTy))
  end
