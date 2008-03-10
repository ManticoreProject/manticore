datatype tree = LF | ND of (int * double * tree * tree);

fun lvlOf (t) = (case t
    of LF => 0
     | ND(lvl, _, _, _) => lvl
    (* end case *))

fun mkTree d = if (d <= 0) 
    then LF 
    else ND(d, 1.0, mkTree(d-1), mkTree(d-1));

divert(-1)
changequote({,})   #change quotes to curly braces 
divert(0)
define({_SEQ_SZ_LG_}, {10})dnl
define({_SEQ_SZ_}, {1024})dnl
define({_TREE_ADD_}, {
  define({_TREE_ADD_FN_}, {$1})dnl
  define({_VAL_}, {$2})dnl
  define({_GOTO_SEQ_}, {$3})dnl
  define({_SEQ_FN_}, {$4})dnl
    fun _TREE_ADD_FN_ t = if (_GOTO_SEQ_)
        then _SEQ_FN_ (t)
        else (case t
               of LF => 0.0
		| ND(x, t1, t2) => let
                  _VAL_ l = _TREE_ADD_FN_ (t1)
                  val r = _TREE_ADD_FN_ (t2) + x
                  in
    	             l + r
                  end
             (* end case *));
})dnl
_TREE_ADD_({seqTreeAdd}, {val}, {false}, {seqTreeAdd})
_TREE_ADD_({treeAdd}, {dval}, {(lvlOf(t) < _SEQ_SZ_LG_)}, {seqTreeAdd})

val b = gettimeofday ();
val x = treeAdd (mkTree (readint()));
val e = gettimeofday ();

print (dtos(e-b) ^ "\n")
