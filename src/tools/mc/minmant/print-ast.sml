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
    structure S = TextIOPP

    val str = S.openOut {dst = TextIO.stdErr, wid = 80}

    val openHBox   = (fn () => S.openHBox str)
    val openVBox   = S.openVBox str
    val openHVBox  = S.openHVBox str
    val openHOVBox = S.openHOVBox str
    val openBox    = S.openBox str
    val closeBox   = (fn () => S.closeBox str)
    val flush      = (fn () => S.flushStream str)

    (* rel : int -> S.indent *)
    fun rel n = S.Rel n

    (* abs : int -> S.indent *)
    fun abs n = S.Abs n

    (* pr: string -> unit *)
    val pr = S.string str

    (* ln: unit -> unit *)
    fun ln () = S.newline str
		
    (* prln : string -> unit *)
    fun prln s = (pr s; ln ())

    (* appwith : (unit -> unit) -> ('a -> unit) -> 'a list -> unit *)
    fun appwith s f xs =
	let fun a [] = ()
	      | a (x::[]) = f x
	      | a (x::xs) = (f x; s (); a xs)
	in
	    a xs
	end

    (* exp : A.exp -> unit *)
    fun exp (A.IfExp (ec, et, ef, t)) = 
	  (openVBox (rel 0);
	   pr "if (";
	   exp ec;
	   pr ") then";
	   openVBox (abs 2);
	   ln ();
	   exp et;
	   closeBox ();
	   ln ();
	   pr "else";
	   openVBox (abs 2);
	   ln ();
	   exp ef;
	   closeBox ();
	   closeBox ())
      | exp (A.ApplyExp (e1, e2, t)) = 
	  (openVBox (rel 0);
	   exp e1;
	   pr " ";
	   exp e2;
	   closeBox ())
      | exp (A.TupleExp es) =
	  (openVBox (rel 0);
	   pr "(";
	   appwith (fn () => pr ",") exp es;
	   pr ")";
	   closeBox ())
      | exp (A.PTupleExp es) =
	  (openVBox (rel 0);
	   pr "(|";
	   appwith (fn () => pr ",") exp es;
	   pr "|)";
	   closeBox ())
      | exp (A.ConstExp c) = const c
      | exp (A.VarExp (v, ts)) = var v
      | exp _ = raise Fail "todo"

    (*
     (A.IfExp (ec, et, ef, t)) =
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
     *)

    (* const : A.const -> unit *)
     and const (A.LConst (lit, t)) = pr (Literal.toString lit)
       | const _ = raise Fail "todo"

     and var (VarRep.V {name, ...}) = pr name

     (* module : A.module -> unit *)
     fun module m = exp m

     fun output (outS, m) = raise Fail "todo"
 (* TextIO.output (outS, P.toString (module m, 80)) *)

     (* print : A.module -> unit *)
     fun print m = (module m;
		    ln ();
		    flush ())
		       
     (**** tests ****)
		       
     local 
	 fun int n = A.ConstExp (A.LConst (Literal.Int n, Basis.intTy))
	 fun string s = A.ConstExp (A.LConst (Literal.String s, Basis.stringTy))
	 fun tup es = A.TupleExp es
	 fun ptup es = A.PTupleExp es
	 val t = tup [int 0, int 1, int 5, int 7, int 9]
	 val dummyType = Basis.unitTy
	 fun ifexp (e1, e2, e3) = A.IfExp (e1, e2, e3, dummyType)
	 fun var name = A.VarExp (VarRep.V {name   = name,
					    id     = Stamp.new (),
					    kind   = ref A.VK_None,
					    useCnt = ref 0,
					    ty     = ref (A.TyScheme ([], 
								      dummyType)),
					    props  = PropList.newHolder ()},
				  [])
	 fun app (e1, e2) = A.ApplyExp (e1, e2, dummyType)
     in
     
         fun test0 () = print (int 0)
		      
	 fun test1 () = print (ifexp (app (var "isPositive", int 0), int 1, int 2))
			
	 fun test2 () = print (ifexp (app (var "isZero", int 10),
				      ifexp (app (var "isEven", int 0), 
					     int 1, 
					     int 2),
				      ifexp (app (var "isOdd", int 3), 
					     int 4, 
					     int 5)))
			
	 fun test3 () = print (ptup [tup [int 0, int 1],
				     string "hello",
				     ptup [string "a", string "b", string "c"]])
			
			
	 fun test4 () = print (var "x")
			
     end

  end
