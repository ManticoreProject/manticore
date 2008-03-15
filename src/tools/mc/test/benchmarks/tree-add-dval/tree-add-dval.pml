
    fun lg (n) = let
	fun loop (x, y) = if (x = 1)
            then y
            else loop(x div 2, y + 1)
        in
           loop(n, 0)
        end
;

datatype tree = LF | ND of (int * double * tree * tree);

fun lvlOf (t) = (case t
    of LF => 0
     | ND(lvl, _, _, _) => lvl
    (* end case *))

fun mkTree (d) = if (d <= 0) 
    then LF 
    else ND(d, 1.0, mkTree(d-1), mkTree(d-1));

val seqSz = readint();

divert(-1)
changequote({,})   #change quotes to curly braces 
divert(0)
define({_SEQ_SZ_LG_}, {lg(seqSz)})dnl
define({_SEQ_SZ_}, {seqSz})dnl
define({_TREE_ADD_}, {
  define({_TREE_ADD_FN_}, {$1})dnl
  define({_VAL_}, {$2})dnl
  define({_GOTO_SEQ_}, {$3})dnl
  define({_SEQ_FN_}, {$4})dnl
    fun _TREE_ADD_FN_ t = if (_GOTO_SEQ_)
        then _SEQ_FN_ (t)
        else (case t
               of LF => 0.0
		| ND(_, x, t1, t2) => let
                  _VAL_ l = _TREE_ADD_FN_ (t1)
                  val r = _TREE_ADD_FN_ (t2) + x
                  in
    	             l + r
                  end
             (* end case *));
})dnl
_TREE_ADD_({seqTreeAdd}, {val}, {false}, {seqTreeAdd})
_TREE_ADD_({treeAdd}, {dval}, {(lvlOf(t) < _SEQ_SZ_LG_)}, {seqTreeAdd})

val depth = readint();
val t = mkTree (depth);
val b = gettimeofday ();
val x = treeAdd (t);
val e = gettimeofday ();

print (dtos(e-b) ^ "\n")
