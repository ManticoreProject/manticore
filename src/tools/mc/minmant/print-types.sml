(* print-types.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure PrintTypes (* : sig

    val outputTyScheme : TextIO.outstream * Types.ty_scheme -> unit
    val printTyScheme  : Types.ty_scheme -> unit

    val outputTy : TextIO.outstream * Types.ty -> unit
    val printTy  : Types.ty -> unit

  end *) = struct

    structure A = AST
    structure T = Types
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

    (* ty : T.ty -> unit *)
    fun ty (T.ErrorTy) = pr "errorTy"
      | ty (T.MetaTy m) = meta m
      | ty (T.ClassTy c) = class c
      | ty (T.VarTy tv) = tyvar tv
      | ty (T.ConTy (ts, c)) =
	  (openVBox (rel 0);
	   appwith (fn () => pr " ") ty ts;
	   case ts
	     of [] => tycon c
	      | _  => (pr " ";
		       tycon c);
	   closeBox ())
      | ty (T.FunTy (t1, t2)) = 
	  (openVBox (rel 0);
	   pr "(";
	   ty t1;
	   pr " -> ";
	   ty t2;
	   pr ")";
	   closeBox ())
      | ty (T.TupleTy ts) =
	  (openVBox (rel 0);
	   pr "(";
	   appwith (fn () => pr ",") ty ts;
	   pr ")";
	   closeBox ())

    and meta m = raise Fail "todo: meta"

    and class c = raise Fail "todo: class"

    and class_info ci = raise Fail "todo: class_info"

    and ty_class c = raise Fail "todo: ty_class"

    and tyvar (T.TVar {name, ...}) = pr (Atom.toString name)

    and tycon (T.AbsTyc {name, ...})  = pr (Atom.toString name)
      | tycon (T.DataTyc {name, ...}) = pr (Atom.toString name)

    and dcon _ = raise Fail "todo: dcon"

    (* ty_scheme : T.ty_scheme -> unit *)
    fun ty_scheme (T.TyScheme (tvs, t)) =
	  (openVBox (rel 0);
	   ty t;
	   closeBox ())

    (* outputTyScheme : TextIO.outstream * T.ty_scheme -> unit *)
    fun outputTyScheme (outS, s) = raise Fail "todo: outputTyScheme"

    (* outputTy : TextIO.outstream * T.ty -> unit *)
    fun outputTy (outS, t) = raise Fail "todo: outputTy"
			
    (* printTyScheme : T.ty_scheme -> unit *)
    fun printTyScheme s = raise Fail "todo: printTyScheme"

    (* printTy : T.ty -> unit *)
    fun printTy t = (ty t;
		     ln ();
		     flush ())
		  
    (* printComment : string -> unit *)       
    (* for debugging purposes *)
    fun printComment s = (prln ("(* " ^ s ^ " *)"))

    (**** tests ****)
			  
end
	   
