(* basis-names.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Based on CMSC 22610 Sample code (Winter 2007)
 *
 * Names of identifiers and operators bound in the MinML basis.
 *)

structure BasisNames =
  struct

    nonfix mod div

  (* predefined type names *)
    val unit =		Atom.atom "unit"
    val bool =		Atom.atom "bool"
    val exn =		Atom.atom "exn"
    val int =		Atom.atom "int"
    val long =		Atom.atom "long"
    val integer =	Atom.atom "integer"
    val float =		Atom.atom "float"
    val double =	Atom.atom "double"
    val char =		Atom.atom "char"
    val rune =		Atom.atom "rune"
    val string =	Atom.atom "string"
    val list =		Atom.atom "list"
    val option =	Atom.atom "option"
    val parray =        Atom.atom "parray"
    val chan =		Atom.atom "chan"
    val ivar =		Atom.atom "ivar"
    val mvar =		Atom.atom "mvar"
    val event =		Atom.atom "event"
    val thread_id =     Atom.atom "tid"

  (* operators *)
    val eq =		Atom.atom "="
    val neq =		Atom.atom "<>"
    val gte =		Atom.atom ">="
    val gt =		Atom.atom ">"
    val lte =		Atom.atom "<="
    val lt =		Atom.atom "<"
    val append =	Atom.atom "@"
    val concat =	Atom.atom "^"
    val plus =		Atom.atom "+"
    val minus =		Atom.atom "-"
    val times =		Atom.atom "*"
    val div =		Atom.atom "div"
    val mod =		Atom.atom "mod"
    val fdiv =		Atom.atom "/"
    val uMinus =	Atom.atom "~"

  (* pre-defined data constructors *)
    val boolTrue =	Atom.atom "true"
    val boolFalse =	Atom.atom "false"
    val listCons =	Atom.atom "::"
    val listNil =	Atom.atom "nil"
    val optionNONE =	Atom.atom "NONE"
    val optionSOME =	Atom.atom "SOME"

  (* predefined variables *)
    val not =		Atom.atom "not"
    val sqrtf =		Atom.atom "sqrtf"
    val lnf =		Atom.atom "lnf"
    val log2f =		Atom.atom "log2f"
    val log10f =	Atom.atom "log10f"
    val powf =		Atom.atom "powf"
    val expf =		Atom.atom "expf"
    val sinf =		Atom.atom "sinf"
    val cosf =		Atom.atom "cosf"
    val tanf =		Atom.atom "tanf"
    val itof =		Atom.atom "itof"
    val sqrtd =		Atom.atom "sqrtd"
    val lnd =		Atom.atom "lnd"
    val log2d =		Atom.atom "log2d"
    val log10d =	Atom.atom "log10d"
    val powd =		Atom.atom "powd"
    val expd =		Atom.atom "expd"
    val sind =		Atom.atom "sind"
    val cosd =		Atom.atom "cosd"
    val tand =		Atom.atom "tand"
    val itod =		Atom.atom "itod"
    val channel =	Atom.atom "channel"
    val send =		Atom.atom "send"
    val sendEvt =	Atom.atom "sendEvt"
    val recv =		Atom.atom "recv"
    val recvEvt =	Atom.atom "recvEvt"
    val wrap =		Atom.atom "wrap"
    val choose =	Atom.atom "choose"
    val always =	Atom.atom "always"
    val never =		Atom.atom "never"
    val sync =		Atom.atom "sync"
    val iVar =		Atom.atom "iVar"
    val iGet =		Atom.atom "iGet"
    val iPut =		Atom.atom "iPut"
    val mVar =		Atom.atom "mVar"
    val mGet =		Atom.atom "mGet"
    val mTake =		Atom.atom "mTake"
    val mPut =		Atom.atom "mPut"
    val itos =		Atom.atom "itos"
    val ltos =		Atom.atom "ltos"
    val ftos =		Atom.atom "ftos"
    val dtos =		Atom.atom "dtos"
    val print =		Atom.atom "print"
    val args =		Atom.atom "args"
    val fail =		Atom.atom "fail"
(*
    val size =		Atom.atom "size"
    val sub =		Atom.atom "sub"
    val substring =	Atom.atom "substring"
    val concat =	Atom.atom "concat"
*)

  end
