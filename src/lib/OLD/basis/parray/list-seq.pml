structure ListSeq = struct
    type 'a seq = 'a list
    val empty = List.nil
    fun singleton s = s :: List.nil
    val null = List.null
    val length = List.length
    val sub = List.nth
    fun update (s, i, x) = let
	  fun lp (s, j) =
	        if j = i then
		    x :: List.tl s
		else if null s then
		    (raise Fail "invalid index")
		else
		    List.hd s :: lp (List.tl s, j + 1)
	  in
	    if i < 0 then
		(raise Fail "invalid index")
	    else
		lp (s, 0)
	  end
    fun concat (x, y) = x @ y
    fun splitAt (ls, i) = (List.take(ls, i+1), List.drop(ls, i+1))
    fun fromList x = x
    fun toList x = x 
    val rev = List.rev
    fun map (f, s) = List.map f s
    fun map2 (f, s1, s2) = ListPair.map f (s1, s2)
    fun reduce (oper, unit, s) = List.foldl oper unit s
    val take = List.take
    val drop = List.drop
    fun cut (s, n) = (List.take (s, n), List.drop (s, n))
    fun filter (f, s) = List.filter f s
    fun tabulate (n, f) = List.tabulate (n, f)
    val zip = List.zip
    val unzip = List.unzip
    val foldr = List.foldr
    fun foldl (f, acc, xs) = List.foldl f acc xs
  end
