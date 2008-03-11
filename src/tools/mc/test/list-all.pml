(* There's a strange bug in case simplification of ttt-par.pml when the
 * function all is written with an inner function, as below. This file is
 * an attempt to isolate the bug, which I still haven't done. 
 * See related comments in ttt-par.pml. (look for "fun all" in that file) - ams 
 *)

datatype XO = X | O;

type foo = XO option list;

fun isSome q = case q
  of SOME _ => true
   | NONE => false;

fun all (pred, xs) = let
  fun f arg = (case arg
          of nil => true
           | x::xs => pred(x) andalso f(xs))
  in
    f xs
  end;

fun tryThis (L : foo) = if (all (isSome, L)) then true else false;

val opts = (SOME X)::NONE::nil;

if (tryThis(opts)) then 
  print "true\n"
else 
  print "false\n"