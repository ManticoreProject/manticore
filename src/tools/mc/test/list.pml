fun rev l = let
      fun rev' (l, l') = (case l
	     of nil => l'
	      | x::xs => rev' (xs, x::l')
	    (* end case *))
      in
	rev' (l, nil)
      end;
 
val l = rev (1::2::3::nil)

