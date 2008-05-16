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
    val trap =          Atom.atom "trap"
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
    val psub =          Atom.atom "!"
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
    val trapVal =       Atom.atom "Val"
    val trapExn =       Atom.atom "Exn"

  (* pre-defined exception constructors *)
    val exnBind =	Atom.atom "Bind"
    val exnDiv =	Atom.atom "Div"
    val exnFail =	Atom.atom "Fail"
    val exnMatch =	Atom.atom "Match"

  (* predefined variables *)
    val not =		Atom.atom "not"
    val sqrtf =		Atom.atom "sqrtf"
    val absf =		Atom.atom "absf"
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
    val absd =		Atom.atom "absd"
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
    val todo =          Atom.atom "todo"
    val rev =           Atom.atom "rev"
    val length =        Atom.atom "length"
    val nth =           Atom.atom "nth"
    val gettimeofday =	Atom.atom "gettimeofday"
    val readint =	Atom.atom "readint"
    val readfloat =	Atom.atom "readfloat"
    val readdouble =	Atom.atom "readdouble"
    val drand =	        Atom.atom "drand"
    val compose =       Atom.atom "compose"
    val map =           Atom.atom "map"
    val filter =        Atom.atom "filter"
    val app =           Atom.atom "app"
    val tab =           Atom.atom "tab"
    val foldl =         Atom.atom "foldl"
    val foldr =         Atom.atom "foldr"
    val concatWith =    Atom.atom "concatWith"

(* parray operations *)
    val plen =          Atom.atom "plen"
    val prev =          Atom.atom "prev"
    val pdivide =       Atom.atom "pdivide"
    val psubseq =       Atom.atom "psubseq"
    val pappend =       Atom.atom "pappend"
    val sumP =          Atom.atom "sumP"
    val reduceP =       Atom.atom "reduceP"
    val papp =          Atom.atom "papp"
    val dist =          Atom.atom "dist"

(*
    val size =		Atom.atom "size"
    val sub =		Atom.atom "sub"
    val substring =	Atom.atom "substring"
    val concat =	Atom.atom "concat"
*)

  (* extras *)
    val image =		Atom.atom "image"
    val newImage =	Atom.atom "newImage"
    val updateImage3f =	Atom.atom "updateImage3f"
    val updateImage3d =	Atom.atom "updateImage3d"
    val outputImage =	Atom.atom "outputImage"
    val freeImage =	Atom.atom "freeImage"
    val getNumProcs =	Atom.atom "getNumProcs"
    val getNumVProcs =	Atom.atom "getNumVProcs"
    val ltcWaitForAll =	Atom.atom "ltcWaitForAll"
    val por =           Atom.atom "por"

  (* arrays *)
    val arrayTyc =      Atom.atom "array"
    val array =         Atom.atom "array"
    val aupdate =       Atom.atom "aupdate"
    val asub =          Atom.atom "asub"
    val alength =       Atom.atom "alength"

  end
