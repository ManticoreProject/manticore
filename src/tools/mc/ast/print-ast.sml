(* print-ast.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure PrintAST : sig

    val output          : TextIO.outstream * AST.comp_unit -> unit
    val outputExp       : TextIO.outstream * AST.exp -> unit
    val print           : AST.comp_unit -> unit
    val printExp        : AST.exp -> unit
    val printExpNoTypes : AST.exp -> unit
    val printComment    : string -> unit

  end = struct

    structure A = AST
    structure T = Types
    structure S = TextIOPP

    val str = ref (S.openOut {dst = TextIO.stdErr, wid = 85})

    val showTypes = ref true

  (* for debugging boxes *)
    fun debug _ = ()
(*
    fun debug s = print s
*)

    fun openHBox () = (debug "(openHBox\n"; S.openHBox (!str))
    fun openVBox i = (debug "(openVBox\n"; S.openVBox (!str) i)
    fun openHVBox i = (debug "(openHVBox\n"; S.openHVBox (!str) i)
    fun openHOVBox i = (debug "(openHOVBox\n"; S.openHOVBox (!str) i)
    fun openBox i = (debug "(openBox\n"; S.openBox (!str) i)
    fun closeBox () = (debug "closeBox)\n"; S.closeBox (!str))
    fun flush () = (S.flushStream (!str))

  (* rel : int -> S.indent *)
    fun rel n = S.Rel n

  (* abs : int -> S.indent *)
    fun abs n = S.Abs n

  (* pr: string -> unit *)
    val pr = (fn s => S.string (!str) s)

  (* ln: unit -> unit *)
    fun ln () = S.newline (!str)

  (* sp : unit -> unit *)
    fun sp () = S.nbSpace (!str) 1

  (* dotimes : int * (unit -> unit) -> unit *)
    fun dotimes (n, th) = let
      fun loop 0  = ()
	| loop n = (th (); loop (n-1))
      in
	if n > 0 then loop n else ()
      end 

  (* sps : int -> unit *)
    fun sps n = dotimes (n, sp)

  (* prln : string -> unit *)
    fun prln s = (pr s; ln ())

  (* appwith : (unit -> unit) -> ('a -> unit) -> 'a list -> unit *)
    fun appwith s f xs = let
	  fun aw [] = ()
	    | aw (x::[]) = f x
	    | aw (x::xs) = (f x; s (); aw xs)
	  in
	    aw xs
	  end

  (* is an expression syntactically atomic? *)
    fun atomicExp (A.TupleExp _) = true
      | atomicExp (A.RangeExp _) = true
      | atomicExp (A.PTupleExp _) = true
      | atomicExp (A.PArrayExp _) = true
      | atomicExp (A.PCompExp _) = true
      | atomicExp (A.ConstExp _) = true
      | atomicExp (A.VarExp _) = true
      | atomicExp (A.OverloadExp _) = true
      | atomicExp _ = false

    fun atomicPat (A.ConPat _) = false
      | atomicPat (A.TuplePat []) = true
      | atomicPat (A.TuplePat (p::_)) = false
      | atomicPat (A.VarPat _) = true
      | atomicPat (A.WildPat _) = true
      | atomicPat (A.ConstPat _) = true

  (* FIXME: should have proper pretty printing for types *)
    fun tyScheme ts = pr(TypeUtil.schemeToString ts)

  (* exp : A.exp -> unit *)
    fun exp (A.LetExp (b, e)) = (
	  openVBox (rel 0);
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
      | exp (A.CaseExp (e, pes, t)) = (
	  openVBox (rel 0);
	     pr "(case (";
	     exp e;
	     pr ")";
	     openVBox (abs 1);
	       ln ();
	       case pes
		of m::ms => (sp (); pe "of" m;  
			     app (pe " |") ms)
		 | nil => raise Fail "case without any branches"
	       (* end case *);	       
	     closeBox ();
	     pr "(* end case *))";
	  closeBox ())
      | exp (A.PCaseExp (es, pms, t)) = (
	  openVBox (rel 2);
	  pr "(pcase ";
	  appwith (fn () => pr " & ") exp es;
	  openVBox (abs 1);
	    ln ();
	    case pms
	     of m::ms => (ppm " of" m; app (ppm "  |") ms)
	      | nil => raise Fail "pcase without any branches"
	    (* end case *);
            closeBox ();
          pr "(* end pcase *))";
	  closeBox ())
      | exp (A.HandleExp(e, matches, ty)) = (
	  openHOVBox (rel 2);
	    openHBox ();
	      pr "(";
	      exp e;
	      pr ")";
	    closeBox ();
	    sp ();
	    openVBox (abs 2);
	      case matches
	       of m::ms => (pe " handle" m;  app (pe "  |") ms)
		| nil => raise Fail "handle without any branches"
	      (* end case *);
	      pr "(* end handle *))";
	    closeBox ();
	 closeBox ())
      | exp (A.RaiseExp(e, ty)) = (
	  openHVBox (rel 2);
	    pr "raise"; sp (); exp e;
	  closeBox ())
      | exp (A.FunExp(arg, body, _)) = (
	  openHBox ();
	    pr "(fn"; sp(); var arg; sp(); pr "=>"; sp(); exp body; pr ")";
	  closeBox())
      | exp (A.ApplyExp (e1, e2, t)) = (
          (* handle cons separately *)
          openHBox ();
          exp e1;
	  if atomicExp e2
	    then (sp(); exp e2)
	    else (pr "("; exp e2; pr ")");
	  closeBox ())
      | exp (A.VarArityOpExp (oper, n, t)) = (
          openHBox ();
            var_arity_op oper;
            pr "_";
            pr (Int.toString n);
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

    and var_arity_op (A.MapP) = pr "mapP"

  (* pe : string -> A.match -> unit *)
    and pe s (A.PatMatch(p, e)) = (
	  openVBox (rel 0);
	    pr s;
	    sp ();
	    pat p;
	    sp ();
	    pr "=>";
	    sp ();
            openVBox (rel 2);
	      exp e;
	    closeBox();
	    ln ();
	  closeBox ())
      | pe s (A.CondMatch(p, cond, e)) = (
	  openHBox ();
	    pr s;
	    sp();
	    pat p;
	    sp(); pr "where"; sp();
	    exp cond;
	    sp(); pr "=>"; sp();
	    exp e;
	    ln ();
	  closeBox ())

  (* ppm : string -> A.pmatch -> unit *)
    and ppm s (A.PMatch (pps, e)) = (
          openVBox (rel 0);
            pr s;
	    sp ();
            appwith (fn () => pr " & ") ppat pps;
            pr " =>";
            sp ();
            exp e;
            ln ();
          closeBox ())
      | ppm s (A.Otherwise e) = (
          openVBox (rel 0);
            pr "| otherwise =>";
            sp ();
            exp e;
            ln ();
          closeBox ())

    and ppat (A.NDWildPat _) = pr "?"
      | ppat (A.HandlePat (p, _)) = (
          pr "handle(";
          pat p;
          pr ")")
      | ppat (A.Pat p) = pat p

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
	   pr " =";
	   ln ();
	   exp b;
	   ln ();
	   closeBox ())
	   
  (* pat : A.pat -> unit *)
    and pat (A.ConPat (c, ts, p)) = (
	  openVBox (rel 0);
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
      | pat (A.VarPat v) = (
	  openHBox ();
	    var v;
	  closeBox ())
      | pat (A.WildPat ty) = pr "_"
      | pat (A.ConstPat c) = const c

  (* const : A.const -> unit *)
    and const (A.DConst (c, ts)) = dcon c
      | const (A.LConst (lit, t)) = pr (Literal.toString lit)

  (* dcon : T.dcon -> unit *)
    and dcon (dc as T.DCon{name, owner, ...}) = let
      val s = if !showTypes 
	      then Atom.toString name ^ ":" ^ TyCon.toString owner
	      else Atom.toString name
      in
        pr s
      end


  (* overload_var : A.overload_var -> unit *)
    and overload_var (A.Unknown (t, vs)) = raise Fail "overload_var.Unknown"
      | overload_var (A.Instance v) = var v
				    
  (* var : A.var -> unit *)
    and var (v as VarRep.V{name, ...}) = let
      val s = if !showTypes 
	      then Var.toString v ^ " : " ^ TypeUtil.schemeToString (Var.typeOf v) 
	      else Var.toString v
      in
	pr s
      end

  (* prettyprint an exception declaration *)
    fun ppExn (T.DCon{name, argTy, ...}) = (
	  openHOVBox (abs 0);
	    pr "exception"; sp(); pr (Atom.toString name);
	    case argTy
	     of SOME ty => (
		  openHBox();
		    sp(); pr "of"; sp(); pr(TypeUtil.toString ty);
		  closeBox ())
	      | NONE => ()
	    (* end case *);
	    pr ";";
	  closeBox(); ln())

  (* module : A.module -> unit *)
(*    fun module (A.Module{exns, body}) = (
	  openVBox (abs 0);
	    List.app ppExn exns;
	    openHBox ();
	      exp body;
	    closeBox ();
	    ln ();
	  closeBox ())
	*)

    fun compUnit (decls) = raise Fail "todo"

    fun outputExp (outS : TextIO.outstream, e) = let
          val oldStr = !str
	  in
	    str := S.openOut {dst = outS, wid = 80};
            exp e;
	    S.closeStream (!str);
            str := oldStr
	  end

  (* output : TextIO.outstream * A.module -> unit *)
    fun output (outS : TextIO.outstream, c) = let
          val oldStr = !str
	  in
	    str := S.openOut {dst = outS, wid = 80};
            compUnit c;
	    S.closeStream (!str);
            str := oldStr
	  end
				 
    fun print c = (compUnit c; ln (); flush ())

  (* printExp : A.exp -> unit *)
    fun printExp e = (exp e; ln (); flush ())

  (* printExpNoTypes : A.exp -> unit *)
    fun printExpNoTypes e = (showTypes := false;
			     printExp e;
			     showTypes := true)
		  
  (* printComment : string -> unit *)       
  (* for debugging purposes *)
    fun printComment s = (prln ("(* " ^ s ^ " *)"); flush ())

  end

