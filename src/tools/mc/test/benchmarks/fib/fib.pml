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

    fun lg (n) = let
	fun loop (x, y) = if (x = 1)
            then y
            else loop(x div 2, y + 1)
        in
           loop(n, 0)
        end


val seqSz = lg(readint())
define({_SEQ_SZ_}, {seqSz})dnl

define({_PFIB_}, {
  define({_PFIB_FN_}, {$1})dnl
  define({_VAL_}, {$2})dnl
  define({_GOTO_SEQ_}, {$3})dnl
  define({_SEQ_FN_}, {$4})dnl
    fun _PFIB_FN_ (n : int) = if (_GOTO_SEQ_)
        then _SEQ_FN_ (n)
        else if n < 2
          then n 
	  else let
              _VAL_ x = _PFIB_FN_ (n-1)
	      val y = _PFIB_FN_ (n-2)
	      in
	        x + y
	      end

})dnl
_PFIB_({seqFib}, {val}, {false}, {seqFib})
_PFIB_({pFib}, {pval}, {(n <= _SEQ_SZ_)}, {seqFib})

fun timeTest () = let
    val n = readint()

    val b = gettimeofday ()
    val _ = pFib(n)
    val e = gettimeofday ()
    in
        print (dtos (e-b)^"\n")
    end


val _ = timeTest()
