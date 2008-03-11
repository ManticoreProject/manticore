(* fib.pml
 * 
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Synthetic fib benchmark.
 *)

divert(-1)
changequote({,})   #change quotes to curly braces 
divert(0)

val seqSz = readint();
define({_SEQ_SZ_}, {seqSz})dnl

define({_PFIB_}, {
  define({_PFIB_FN_}, {$1})dnl
  define({_VAL_}, {$2})dnl
  define({_GOTO_SEQ_}, {$3})dnl
  define({_SEQ_FN_}, {$4})dnl
    fun _PFIB_FN_ (n : int) = if (_GOTO_SEQ_)
        then _SEQ_FN_ (n)
        else (case n
         of 0 => 0
	  | 1 => 1
	  | n => let
              _VAL_ x = _PFIB_FN_ (n-1)
	      val y = _PFIB_FN_ (n-2)
	      in
	        x + y
	      end
         (* end case *))
;
})dnl
_PFIB_({seqFib}, {val}, {false}, {seqFib})
_PFIB_({pFib}, {dval}, {(n <= _SEQ_SZ_)}, {seqFib})

fun timeTest () = let
    val n = readint()

    val b = gettimeofday ()
    val _ = pFib(n)
    val e = gettimeofday ()
    in
        print (dtos (e-b)^"\n")
    end
;

timeTest()
