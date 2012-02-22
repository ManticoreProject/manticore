(* intlist.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Single-precision floating-point math functions
 *)

structure Intlist =
  struct
 
  
datatype intlist = Nil 
                | Cons of (int * intlist)

fun createintlist (n) : intlist = 
        if (n > 0)
        then Cons(1,createintlist(n-1))
        else Nil

fun append (list1 : intlist, list2 : intlist) : intlist = 
    case list1 of
      Nil => list2
    | Cons(i,tl) => Cons(i,append(tl,list2))

fun reverse(list : intlist) : intlist = 
    case list of
      Nil => Nil
    | Cons(hd,tl) => append(reverse(tl), Cons(hd,Nil)) 


fun split ns = let
        (* O(n) algorithm to split the list in half *)
        fun intloop (Cons(x,Cons(y,zs)), xs, ys) = intloop (zs, Cons(x,xs), Cons(y,ys))
            | intloop (Cons(x,Nil), xs, ys) = (Cons(x,xs), ys)
            | intloop (Nil, xs, ys) = (xs, ys)
        in
                intloop (reverse ns, Nil, Nil)
        end
  
type intlist = intlist  
  
end