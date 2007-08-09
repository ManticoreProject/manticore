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
	   pr "(case ";
	   exp e;
	   openVBox (abs 3);
	   ln ();
	   ignore (case pes
		    of m::ms => (pe "of" m;
				 app (pe " |") ms)
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
      | exp (A.PCompExp (e, pes, oe)) = raise Fail "todo: PCompExp"
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
      | exp (A.OverloadExp ovr) = raise Fail "todo: OverloadVarExp"

    (* pe : string -> pat * exp -> unit *)
    and pe s (p, e) =
	  (openVBox (rel 0);
	   pr s;
	   pr " ";
	   pat p;
	   pr " => ";
	   exp e;
	   ln ();
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
    and pat (A.ConPat (d, ts, p)) = raise Fail "todo: ConPat" 
      | pat (A.TuplePat ps) =
	  (openVBox (rel 0);
	   pr "(";
	   appwith (fn () => pr ",") pat ps;
	   pr ")";
	   closeBox ())
      | pat (A.VarPat v) = var v
      | pat (A.ConstPat c) = const c

    (* const : A.const -> unit *)
    and const (A.DConst (c, ts)) = raise Fail "todo: DConst"
      | const (A.LConst (lit, t)) = pr (Literal.toString lit)
				    
    (* var : A.var -> unit *)
    and var (VarRep.V {name, ...}) = pr name
				     
    (* module : A.module -> unit *)
    fun module m = exp m
		   
    fun output (outS, m) = raise Fail "todo"
				 
    (* print : A.module -> unit *)
    fun print m = (module m;
		   ln ();
		   flush ())
		  
    (* printComment : string -> unit *)       
    (* for debugging purposes *)
    fun printComment s = (prln ("(* " ^ s ^ " *)"))

    structure Debug = struct
    fun print s = prln s
    end
			  
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
	   
