(* doc.sml
 *
 * COPYRIGHT (c) 2008 Adam Shaw (http://people.cs.uchicago.edu/~adamshaw)
 * All rights reserved.
 *
 * A direct implementation of Section 1 of Phil Wadler's "A Prettier Printer."
 * No originality here.
 *)

structure Doc : sig

  type doc
  val ^^ : doc * doc -> doc
  val null : doc
  val text : string -> doc
  val line : doc
  val nest : int -> doc -> doc
  val layout : doc -> string

(* a few convenience items *)
  val space : doc
  val concat : doc list -> doc
  val concatWith : doc -> doc list -> doc
  val layoutToFile : doc * string -> unit

end = struct

  datatype doc 
    = Null
    | Text of string * doc
    | Line of int * doc

  val null = Null

  fun text s = Text (s, Null)

  val line = Line (0, Null)

  fun ^^ (Text (s, x), y) = Text (s, ^^ (x, y))
    | ^^ (Line (i, x), y) = Line (i, ^^ (x, y))
    | ^^ (Null, y) = y

  fun nest i (Text (s, x)) = Text (s, nest i x)
    | nest i (Line (j, x)) = Line (i+j, nest i x)
    | nest i Null = Null

  fun stringof (i, s) = let
    fun loop (0, ss) = String.concat ss
      | loop (i, ss) = loop (i-1, s::ss)
    in
      if (i <= 0) then "" else loop (i, [])
    end

  fun layout (Text (s, x)) = String.concat [s, layout x]
    | layout (Line (i, x)) = String.concat ["\n", stringof (i, " "), layout x]
    | layout Null = ""

  val space = text " "

  val concat = foldr ^^ null

  fun concatWith sep ds = let
    fun cat [] = null
      | cat [d] = d
      | cat (d::ds) = ^^ (d, ^^ (sep, cat ds))
    in
      cat ds
    end 

  fun layoutToFile (d, outfile) = let
    val outstream = TextIO.openOut outfile
    fun out s = TextIO.output (outstream, s)
    fun loop (Text (s, x)) = (out s;
			      loop x)
      | loop (Line (i, x)) = (out "\n";
			      out (stringof (i, " "));
			      loop x)
      | loop Null = ()
    in
      loop d before TextIO.closeOut outstream
    end

end
