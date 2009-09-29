structure Utilities : sig

    val highest : int list -> int
    val lowest  : int list -> int

    (* extreme is given a relational operator and a list of pairs of type 'a * int.
     * It is given the 'a with the most extreme int companion, where extreme is
     * determined by the given relop.
     *)
    val extreme : (int * int -> bool) -> ('a * int) list -> 'a

  end = struct

  fun highest (ns : int list) : int =
    (case ns
     of [] => raise Fail "undefined"
      | m::ms => foldl (fn (a,b) => if (a>b) then a else b) m ms
    (* end case *))

  fun lowest (ns : int list) : int =
   (case ns
     of [] => raise Fail "undefined"
      | m::ms => foldl (fn (a,b) => if (a<b) then a else b) m ms
    (* end case *))

  fun extreme relop pairs = let
    fun h ([], (m, _)) = m
      | h ((m',n')::t, (m,n)) =
	  if relop (n',n) then h (t, (m', n'))
	  else h (t, (m, n))
    in
      case pairs
       of [(x,n)] => x
	| (x,n)::t => h (t, (x,n))
	| [] => raise Fail "undefined"
    end


end
