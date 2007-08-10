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
    structure T = Types
    structure S = TextIOPP

    val str = ref (S.openOut {dst = TextIO.stdErr, wid = 80})

    val openHBox   = (fn () => S.openHBox (!str))
    val openVBox   = (fn i => S.openVBox (!str) i)
    val openHVBox  = (fn i => S.openHVBox (!str))
    val openHOVBox = (fn i => S.openHOVBox (!str))
    val openBox    = (fn i => S.openBox (!str))
    val closeBox   = (fn () => S.closeBox (!str))
    val flush      = (fn () => S.flushStream (!str))

    (* rel : int -> S.indent *)
    fun rel n = S.Rel n

    (* abs : int -> S.indent *)
    fun abs n = S.Abs n

    (* pr: string -> unit *)
    val pr = (fn s => S.string (!str) s)

    (* ln: unit -> unit *)
    fun ln () = S.newline (!str)
		
    (* prln : string -> unit *)
    fun prln s = (pr s; ln ())

    (* appwith : (unit -> unit) -> ('a -> unit) -> 'a list -> unit *)
    fun appwith s f xs =
	let fun aw [] = ()
	      | aw (x::[]) = f x
	      | aw (x::xs) = (f x; s (); aw xs)
	in
	    aw xs
	end

    (* exp : A.exp -> unit *)
    fun exp (A.LetExp (b, e)) = 
	  (openVBox (rel 0);
	   pr "let";
	   openVBox (abs 4);
	   ln ();
	   binding b;
	   closeBox ();
	   ln ();
	   pr "in";
	   openVBox (abs 4);
	   ln ();
	   exp e;
	   closeBox ();
	   ln ();
	   pr "end";
	   closeBox ())
      |	exp (A.IfExp (ec, et, ef, t)) = 
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
      | exp (A.CaseExp (e, pes, t)) =
	  (openVBox (rel 0);
	   pr "(case (";
	   exp e;
	   pr ")";
	   openVBox (abs 2);
	   ln ();
	   ignore (case pes
		    of m::ms => (pe " of" m;
				 app (pe "  |") ms)
		     | nil => raise Fail "case without any branches");
	   pr "(* end case *))";
	   closeBox ();
	   closeBox ())
      | exp (A.ApplyExp (e1, e2, t)) = 
	  (openVBox (rel 0);
	   exp e1;
	   pr "(";
 	   exp e2;
	   pr ")";
	   closeBox ())
      | exp (A.TupleExp es) =
	  (openVBox (rel 0);
	   pr "(";
	   appwith (fn () => pr ",") exp es;
	   pr ")";
	   closeBox ())
      | exp (A.RangeExp (e1, e2, oe3, t)) =
	  (openVBox (rel 0);
	   pr "[| ";
	   exp e1;
	   pr " to ";
	   exp e2;
	   case oe3
  	     of SOME e3 => (pr " by ";
			    exp e3)
	      | NONE => ();
	   pr " |]";
	   closeBox ())
      | exp (A.PTupleExp es) =
	  (openVBox (rel 0);
	   pr "(|";
	   appwith (fn () => pr ",") exp es;
	   pr "|)";
	   closeBox ())
      | exp (A.PArrayExp (es, t)) =
	  (openVBox (rel 0);
	   pr "[| ";
	   appwith (fn () => pr ",") exp es;
	   pr " |]";
	   closeBox ())
      | exp (A.PCompExp (e, pes, oe)) = 
	  (openVBox (rel 0);
	   pr "[| ";
	   exp e;
	   pr " | ";
	   appwith (fn () => pr ", ") pbind pes;
	   case oe
	     of NONE => ()
	      | SOME pred => (pr " where ";
			      exp pred);
	   pr " |]";
	   closeBox ())
      | exp (A.PChoiceExp (es, t)) = 
	  (openVBox (rel 0);
	   appwith (fn () => pr " |?| ") exp es;
	   closeBox ())
      | exp (A.SpawnExp e) = 
	  (openVBox (rel 0);
	   pr "spawn ";
	   exp e;
	   closeBox ())
      | exp (A.ConstExp c) = const c
      | exp (A.VarExp (v, ts)) = var v
      | exp (A.SeqExp (e1, e2)) = 
	  (openVBox (rel 0);
	   pr "(";
	   exp e1;
	   pr "; ";
	   exp e2;
	   pr ")";
	   closeBox ())
      | exp (A.OverloadExp ovr) = raise Fail "todo: OverloadExp"

    (* pe : string -> A.pat * A.exp -> unit *)
    and pe s (p, e) =
	  (openVBox (rel 0);
	   pr s;
	   pr " ";
	   pat p;
	   pr " => ";
	   exp e;
	   ln ();
	   closeBox ())

    (* pbind : A.pat * A.exp -> unit *)
    and pbind (p, e) =
	  (openVBox (rel 0);
	   pat p;
	   pr " in ";
	   exp e;
	   closeBox ())

    (* binding : A.binding -> unit *)
    and binding (A.ValBind (p, e)) =
	  (openVBox (rel 0);
	   pr "val ";
	   pat p;
	   pr " = ";
	   exp e;
	   closeBox ())
      | binding (A.PValBind (p, e)) =
	  (openVBox (rel 0);
	   pr "pval ";
	   pat p;
	   pr " = ";
	   exp e;
	   closeBox ())
      | binding (A.FunBind lams) = 
	  (case lams
	     of [] => raise Fail "empty FunBind"
	      | (d::ds) =>
		  (openVBox (rel 0);
		   lambda "fun" d;
		   app (lambda "and") ds;
		   closeBox ()))

    (* lambda : string -> A.lambda -> unit *)
    and lambda kw (A.FB (f, x, b)) =
	  (openVBox (rel 0);
	   pr kw;
	   pr " ";
	   var f;
	   pr " ";
	   var x;
	   pr " = ";
	   exp b;
	   closeBox ())
	   
    (* pat : A.pat -> unit *)
    and pat (A.ConPat (c, ts, p)) = 
	  (openVBox (rel 0);
	   dcon c;
	   pr "(";
	   pat p;
	   pr ")";
	   closeBox ())
      | pat (A.TuplePat ps) =
	  (openVBox (rel 0);
	   pr "(";
	   appwith (fn () => pr ",") pat ps;
	   pr ")";
	   closeBox ())
      | pat (A.VarPat v) = var v
      | pat (A.ConstPat c) = const c

    (* const : A.const -> unit *)
    and const (A.DConst (c, ts)) = dcon c
      | const (A.LConst (lit, t)) = pr (Literal.toString lit)

    and dcon (T.DCon {name, ...}) = pr (Atom.toString name)
				    
    (* var : A.var -> unit *)
    and var (VarRep.V {name, ...}) = pr name
				     
    (* module : A.module -> unit *)
    fun module m = exp m
		   
    (* output : TextIO.outstream * A.module -> unit *)
    fun output (outS, m) = (str := outS;
			    module m)
				 
    (* print : A.module -> unit *)
    fun print m = (module m;
		   ln ();
		   flush ())
		  
    (* printComment : string -> unit *)       
    (* for debugging purposes *)
    fun printComment s = (prln ("(* " ^ s ^ " *)"))
			  
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
	fun some e = 
	      let val t = TypeOf.exp e
		  val someConExp = A.ConstExp (A.DConst (Basis.optionSOME, [t]))
		  val topt = T.ConTy ([t], Basis.optionTyc)
	      in
		  A.ApplyExp (someConExp, e, topt) 
	      end
	val none = A.ConstExp (A.DConst (Basis.optionNONE, []))
	fun somePat (p, ts) = A.ConPat (Basis.optionSOME, ts, p)
	val nonePat = A.ConstPat (A.DConst (Basis.optionNONE, []))
	fun varPat (x, t) = A.VarPat (Var.new (x, t))
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
		       
		       
	fun test4 () =
	      let val intTy = Basis.intTy
	      in
		  print (A.CaseExp 
			     (var "x",
			      [(somePat (varPat ("n", intTy), [intTy]), int 1),
			       (nonePat, int 0)],
			      intTy))
	      end
		       
    end
		      
end
	   
