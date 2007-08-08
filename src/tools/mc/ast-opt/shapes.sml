(* shapes.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * This structure is for computing and manipulating the shapes of tuples.
 * The tuple (1,(2,(3,4),5),(6,7)) has the shape
 *  (.,(.,(.,.),.),(.,.))
 *)

structure Shapes : SHAPES = 
   
  struct

    structure A = AST
    structure T = Types

    datatype shape = Tup of shapes | Dot
    and shapes = Seq of shape * shapes | Sing of shape

    (* shapeOf : A.exp -> shape *)
    fun shapeOf (A.PTupleExp es) = Tup (shapesOf es)
      | shapeOf _ = Dot

    (* shapesOf : A.exp list -> shapes *)
    (* pre: the is not nil (o/w exception is raised) *)
    and shapesOf (e::[]) = Sing (shapeOf e)
      | shapesOf (e::es) = Seq (shapeOf e, shapesOf es)
      | shapesOf ([]) = raise Fail "empty"

    (* interp: 'a -> 'a list -> 'a list *)
    fun interp a xs =
	let (* i : 'a list -> 'a list *)
	    fun i [] = []
	      | i [x] = [x]
	      | i (x::xs) = x :: a :: (i xs)
	in
	    i xs
	end

    (* toString : shape -> string *)
    fun toString s = 
	let (* shape : shape -> string list *)
	    fun shape (Tup ss) = "(" :: shapes ss @ ")" :: []
	      | shape (Dot) = "." :: []
	    (* shapes : shapes -> string list *)
	    and shapes (Seq (s, ss)) = interp "," (shape s @ shapes ss)
	      | shapes (Sing s) = shape s
	in
	    concat (shape s)
	end

  end
