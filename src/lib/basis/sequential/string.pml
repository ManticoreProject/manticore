(* string.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)


structure String =
  struct

    structure PT = PrimTypes

    _primcode(
      typedef ml_string = PT.ml_string;

      extern void *M_StringConcatList (void *) __attribute__((pure,alloc));
      extern void *M_StringTokenize (void *, void *) __attribute__((pure,alloc));
      extern int M_StringSame (void *, void *) __attribute__((pure));
      extern int strcmp (void *, void *);
      extern void * M_StringExplode(void *) __attribute__((pure,alloc));
      extern int M_StringCountOccurrences(void*, void*);
      extern int M_StringFirstLastSame(void*);
      extern void M_ReplaceChar(void *, void *, void*);

      
      define @first-last-same(arg : ml_string / exh:exh) : bool = 
        let x : int = ccall M_StringFirstLastSame(arg)
        if I32Eq(x, 1)
        then return(true)
        else return(false)
      ;
      
      define @count-occurrences(arg : [ml_string, ml_string] / exh:exh) : ml_int = 
        let count : int = ccall M_StringCountOccurrences(#0(arg), #1(arg))
        let count : [int] = alloc(count)
        return(count);
      
      define @string-explode(arg : ml_string / exh : exh) : List.list = 
        let res : List.list = ccall M_StringExplode(arg)
        return(res);

      define @string-compare(arg : [ml_string, ml_string] / exh:exh) : order = 
        let res : int = ccall strcmp(#0(#0(arg)), #0(#1(arg)))
        if I32Lt(res, 0)
        then return(LESS)
        else if I32Gt(res, 0)
             then return(GREATER)
             else return(EQUAL);
  
      define inline @data (s : ml_string / exh : PT.exh) : any =
	  let res : any = #0(s)
	    return (res)
      ;

      define inline @lit (s : PT.string_data, len : int / exh : PT.exh) : ml_string =
	  let res : ml_string = alloc (s, len)
	    return (res)
      ;

      define inline @size (s : ml_string / exh : PT.exh) : PT.ml_int =
	  let len : int = #1(s)
	  let res : PT.ml_int = alloc(len)
	    return (res)
      ;

      define inline @string-concat-list (arg : List.list / exh : PT.exh) : ml_string =
	  let res : ml_string = ccall M_StringConcatList (arg)
	    return (res)
      ;

      define inline @tokenize (arg : [ml_string, ml_string] / exh : exh) : (* string *) List.list =
	  let ls : List.list = ccall M_StringTokenize (#0(arg), #1(arg))
	  return (ls)
	;

      define inline @same (arg : [ml_string, ml_string] / exh : exh) : bool =
	  let res : int = ccall M_StringSame (#0(arg), #1(arg))
	  if I32Eq (res, 1) then return (true)
	  else return (false)
	;
    )

    val concat : string list -> string = _prim(@string-concat-list)
    val size : string -> int = _prim(@size)
    local
	val tok : string * string -> string list = _prim(@tokenize)
    in
    fun tokenize sep str = List.rev (tok (str, sep))
    end

    val same : string * string -> bool = _prim (@same)

    val compare : string * string -> order = _prim(@string-compare)

    val explode : string -> string list = _prim(@string-explode)

    val countOccurrences : string * string -> int = _prim(@count-occurrences)

    val firstLastSame : string -> bool = _prim(@first-last-same)

    fun concatWith s ss = let
	  fun lp xs = (case xs
		 of nil => nil
		  | x :: nil => x :: nil
		  | x :: xs => x :: s :: lp xs
		(* end case *))
	  in
	    concat(lp ss)
	  end

  end
