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

    val toString : Types.ty -> string

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

    (* ty : T.ty -> unit *)
    fun ty t = pr (TypeUtil.toString t)

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
    fun printTyScheme s = (ty_scheme s;
			   ln ();
			   flush ())

    (* printTy : T.ty -> unit *)
    fun printTy t = (ty t;
		     ln ();
		     flush ())
		  
    (* printComment : string -> unit *)       
    (* for debugging purposes *)
    fun printComment s = (prln ("(* " ^ s ^ " *)"))

    (* toString : T.ty -> string *)
    fun toString t = TypeUtil.toString t

    (**** tests ****)
			  
end
	   
