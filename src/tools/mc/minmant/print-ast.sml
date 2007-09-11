(* print-ast.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure PrintAST : sig

    val output : TextIO.outstream * AST.module -> unit
    val print  : AST.module -> unit
    val printComment : string -> unit

  end = struct

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

    (* sp : unit -> unit *)
    fun sp () = S.space (!str) 1

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
      | exp (A.FunExp(arg, body, _)) = (
	  openHBox ();
	    pr "(fn"; sp(); var arg; sp(); pr "=>"; sp(); exp body; pr ")";
	  closeBox())
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
      | exp (A.OverloadExp ovr) = overload_var (!ovr)

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
      | pat (A.WildPat ty) = pr "_"
      | pat (A.ConstPat c) = const c

    (* const : A.const -> unit *)
    and const (A.DConst (c, ts)) = dcon c
      | const (A.LConst (lit, t)) = pr (Literal.toString lit)

    (* dcon : T.dcon -> unit *)
    and dcon (T.DCon {name, ...}) = pr (Atom.toString name)

    (* overload_var : A.overload_var -> unit *)
    and overload_var (A.Unknown (t, vs)) = raise Fail "overload_var.Unknown"
      | overload_var (A.Instance v) = var v
				    
    (* var : A.var -> unit *)
    and var (VarRep.V {name, ...}) = pr name
				     
    (* module : A.module -> unit *)
    fun module m = exp m
		   
    (* output : TextIO.outstream * A.module -> unit *)
    fun output (outS : TextIO.outstream, m : A.module) = 
       let
          val oldStr = !str
          val () = str := S.openOut {dst = outS, wid = 80}
          val () = module m
          val () = str := oldStr
       in
          ()
       end
				 
    (* print : A.module -> unit *)
    fun print m = (module m;
		   ln ();
		   flush ())
		  
    (* printComment : string -> unit *)       
    (* for debugging purposes *)
    fun printComment s = (prln ("(* " ^ s ^ " *)");
			  flush ())

  end

