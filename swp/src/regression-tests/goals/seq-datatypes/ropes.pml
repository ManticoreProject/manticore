type 'a seq = 'a list

    datatype 'a rope
      = CAT of (int *     (* depth *)
		int *     (* length *)
		'a rope * (* left subtree *)
		'a rope   (* right subtree *))
      | LEAF of (int *    (* length *)
		 'a seq   (* sequence *))


  (* toString : ('a -> string) -> 'a rope -> string *)
    fun toString show r = let
      fun build r =
       (case r
	 of LEAF (_, xs) => let 
              fun b args = 
               (case args
	         of (nil, acc) => "]" :: acc
		  | (x::nil, acc) => b (nil, show x :: acc)
		  | (x::xs, acc) => b (xs, "," :: show x ::acc)
	         (* end case *))
              in
		b (xs, nil)
              end
	  | CAT (_, _, r1, r2) => let 
              val ss1 = build r1
	      val ss2 = build r2
	      in
	        ( ss1) @ ( ( ss2))
	      end	
         (* end case *))
      in
       String.concat(build r)
      end

val () = Print.printLn(toString Int.toString (CAT(0, 0, LEAF(0, [1]), LEAF(0, [1]))))

