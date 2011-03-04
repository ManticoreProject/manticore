(* print-flast.sml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure PrintFLAST : sig

    val output          : TextIO.outstream * FLAST.comp_unit -> unit
    val outputExp       : TextIO.outstream * FLAST.exp -> unit
    val print           : FLAST.comp_unit -> unit
    val printExp        : FLAST.exp -> unit
    val printExpNoTypes : FLAST.exp -> unit
    val printComment    : string -> unit

    val printExpNoTypesNoStamps : FLAST.exp -> unit

  end = struct

    structure F = FLAST
    structure T = FTTypes
    structure S = TextIOPP

    val str = ref (S.openOut {dst = TextIO.stdErr, wid = 85})

    val showTypes = ref true
    val showStamps = ref true

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
    fun atomicExp (F.TupleExp _) = true
      | atomicExp (F.RangeExp _) = true
      | atomicExp (F.PTupleExp _) = true
      | atomicExp (F.PArrayExp _) = true
      | atomicExp (F.PCompExp _) = true
      | atomicExp (F.ConstExp _) = true
      | atomicExp (F.VarExp _) = true
      | atomicExp (F.OverloadExp _) = true
      | atomicExp _ = false

    fun atomicPat (F.ConPat _) = false
      | atomicPat (F.TuplePat []) = true
      | atomicPat (F.TuplePat (p::_)) = false
      | atomicPat (F.VarPat _) = true
      | atomicPat (F.WildPat _) = true
      | atomicPat (F.ConstPat _) = true

  (* FIXME *)
    fun tyScheme ts = raise Fail "todo"

    fun exp (e as F.LetExp _) = let
	  fun prBinds (F.LetExp(b, e)) = (
		ln ();
		binding b;
		prBinds e)
	    | prBinds e = e
	  val body = (
		pr "let";
		openVBox (abs 2);
		prBinds e before closeBox())
	  in
	    ln ();
	    pr "in";
	    openVBox (abs 2);
	      ln ();
	      exp body;
	    closeBox ();
	    ln ();
	    pr "end"
	  end
      |	exp (F.IfExp (ec, et, ef, t)) = (
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
	   closeBox ())
      | exp (F.CaseExp (e, pes, t)) = (
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
	  pr "(* end case *))")
      | exp (F.PCaseExp (es, pms, t)) = (
	  openVBox (abs 2);
	  pr "(pcase ";
	  appwith (fn () => pr " & ") exp es;
	  openVBox (abs 1);
	    ln ();
	    case pms
	     of m::ms => (ppm "of" m; app (ppm " |") ms)
	      | nil => raise Fail "pcase without any branches"
	    (* end case *);
            closeBox ();
          pr "(* end pcase *))";
	  closeBox ())
      | exp (F.HandleExp(e, matches, ty)) = (
          pr "(";
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
      | exp (F.RaiseExp(e, ty)) = (
	  openHVBox (rel 2);
	    pr "raise"; sp (); exp e;
	  closeBox ())
      | exp (F.FunExp(arg, body, _)) = (
	  openHBox ();
	    pr "(fn"; sp(); var arg; sp(); pr "=>"; sp(); exp body; pr ")";
	  closeBox())
      | exp (F.ApplyExp (e1, e2, t)) = (
          (* handle cons separately *)
          openHBox ();
          exp e1;
	  if atomicExp e2
	    then (sp(); exp e2)
	    else (pr "("; exp e2; pr ")");
	  closeBox ())
      | exp (F.VarArityOpExp (oper, n, t)) = (
          openHBox ();
            var_arity_op oper;
            pr "_";
            pr (Int.toString n);
          closeBox ())
      | exp (F.TupleExp (es, _)) =
	  (openVBox (rel 0);
	   pr "(";
	   appwith (fn () => pr ",") exp es;
	   pr ")";
	   closeBox ())
      | exp (F.RangeExp (e1, e2, oe3, t)) =
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
      | exp (F.PTupleExp es) =
	  (openVBox (rel 0);
	   pr "(|";
	   appwith (fn () => pr ",") exp es;
	   pr "|)";
	   closeBox ())
      | exp (F.PArrayExp (es, t)) =
	  (openVBox (rel 0);
	   pr "[| ";
	   appwith (fn () => pr ",") exp es;
	   pr " |]";
	   closeBox ())
      | exp (F.FArrayExp (es, n, t)) = 
          (openVBox (rel 0);
	   pr "{[";
	   appwith (fn () => pr ",") exp es;
	   pr "];";
	   ntree n;
	   pr "}";
	   closeBox())
      | exp (F.PCompExp (e, pes, oe)) = 
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
      | exp (F.PChoiceExp (es, t)) = 
	  (openVBox (rel 0);
	   appwith (fn () => pr " |?| ") exp es;
	   closeBox ())
      | exp (F.SpawnExp e) = (
	  openHBox();
	  pr "spawn"; sp();
	  exp e;
	  closeBox ())
      | exp (F.ConstExp c) = const c
      | exp (F.VarExp (v, ts)) = var v
      | exp (F.SeqExp (e1, e2)) = (
	  pr "(";
	  exp e1;
	  pr "; ";
	  exp e2;
	  pr ")")
      | exp (F.OverloadExp ovr) = overload_var (!ovr)
      | exp (F.ExpansionOptsExp (_, e)) = exp e

    and var_arity_op (F.MapP) = pr "mapP"

    and ntree (F.Lf (e1, e2)) = (
          openVBox (rel 0);
	  pr "Lf(";
	  exp e1;
	  pr ",";
	  exp e2;
	  pr ")")
      | ntree (F.Nd ns) = (
	  pr "Nd(";
	  appwith (fn () => ",") ntree ns;
	  pr ")")

  (* pe : string -> F.match -> unit *)
    and pe s (F.PatMatch(p, e)) = (
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
      | pe s (F.CondMatch(p, cond, e)) = (
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

  (* ppm : string -> F.pmatch -> unit *)
    and ppm s (F.PMatch (pps, e)) = (
          openVBox (rel 0);
            pr s;
	    sp ();
            appwith (fn () => pr " & ") ppat pps;
            pr " =>";
            sp ();
            exp e;
            ln ();
          closeBox ())
      | ppm s (F.Otherwise (ts, e)) = (
          openVBox (rel 0);
            pr s;
	    sp ();
            pr "otherwise =>";
            sp ();
            exp e;
            ln ();
          closeBox ())

    and ppat (F.NDWildPat _) = pr "?"
      | ppat (F.HandlePat (p, _)) = (
          pr "handle(";
          pat p;
          pr ")")
      | ppat (F.Pat p) = pat p

  (* pbind : F.pat * F.exp -> unit *)
    and pbind (p, e) =
	  (openVBox (rel 0);
	   pat p;
	   pr " in ";
	   exp e;
	   closeBox ())

  (* binding : F.binding -> unit *)
    and binding (F.ValBind (p, e)) =
	  (openVBox (rel 0);
	   pr "val ";
	   pat p;
	   pr " = ";
	   exp e;
	   closeBox ())
      | binding (F.PValBind (p, e)) =
	  (openVBox (rel 0);
	   pr "pval ";
	   pat p;
	   pr " = ";
	   exp e;
	   closeBox ())
      | binding (F.FunBind lams) = 
	  (case lams
	     of [] => ()
	      | (d::ds) =>
		  (openVBox (rel 0);
		   lambda "fun" d;
		   app (lambda "and") ds;
		   closeBox ()))
      | binding (F.PrimVBind(x, _)) = (
	  openHBox ();
	    pr "val"; sp(); var x; sp(); pr "="; sp(); pr"_prim(...)";
	  closeBox())
      | binding (F.PrimCodeBind _) = pr "_primcode(...)"

  (* lambda : string -> F.lambda -> unit *)
    and lambda kw (F.FB (f, x, b)) =
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
	   
  (* pat : F.pat -> unit *)
    and pat (F.ConPat (c, ts, p)) = (
	  openVBox (rel 0);
	  dcon c;
	  pr "(";
	  pat p;
	  pr ")";
	  closeBox ())
      | pat (F.TuplePat ps) =
	  (openVBox (rel 0);
	   pr "(";
	   appwith (fn () => pr ",") pat ps;
	   pr ")";
	   closeBox ())
      | pat (F.VarPat v) = var v
      | pat (F.WildPat ty) = 
          pr (if !showTypes then ("(_:" ^ FTTypeUtil.toString ty ^ ")")
	      else "_")
      | pat (F.ConstPat c) = const c

  (* const : F.const -> unit *)
    and const (F.DConst (c, ts)) = dcon c
      | const (F.LConst (lit, t)) = pr (Literal.toString lit)

  (* dcon : Types.dcon -> unit *)
    and dcon (dc as FTTypes.DCon{name, owner, ...}) = let
      val s = if !showTypes 
	      then Atom.toString name ^ ":" ^ FTTyCon.toString owner
	      else Atom.toString name
      in
        pr s
      end

  (* overload_var : F.overload_var -> unit *)
    and overload_var (F.Unknown (t, vs)) = raise Fail "overload_var.Unknown"
      | overload_var (F.Instance v) = var v
				    
  (* var : F.var -> unit *)
    and var (v : F.var) = let
      val x = if !showStamps 
	      then FTVar.toString v 
	      else FTVar.nameOf v
      val t = FTTypeUtil.schemeToString (FTVar.typeOf v) 
      val s = if !showTypes then "(" ^ x ^ ":" ^ t ^ ")" else x
      in
	pr s
      end

  (* prettyprint an exception declaration *)
    fun ppExn (Types.DCon{name, argTy, ...}) = (
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

  (* module : F.module -> unit *)
(*    fun module (F.Module{exns, body}) = (
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

  (* output : TextIO.outstream * F.module -> unit *)
    fun output (outS : TextIO.outstream, c) = let
          val oldStr = !str
	  in
	    str := S.openOut {dst = outS, wid = 80};
            compUnit c;
	    S.closeStream (!str);
            str := oldStr
	  end
				 
    fun print c = (compUnit c; ln (); flush ())

  (* printExp : F.exp -> unit *)
    fun printExp e = (exp e; ln (); flush ())

  (* printExpNoTypes : F.exp -> unit *)
    fun printExpNoTypes e = (showTypes := false;
			     printExp e;
			     showTypes := true)

  (* printComment : string -> unit *)       
  (* for debugging purposes *)
    fun printComment s = (prln ("(* " ^ s ^ " *)"); flush ())

  (* printExpNoTypesNoStamps : F.exp -> unit *)
    fun printExpNoTypesNoStamps e = (showTypes := false;
				     showStamps := false;
				     printExp e;
				     showTypes := true;
				     showStamps := true)
  end

