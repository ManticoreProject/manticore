(* test-ropes.sml
 *)

structure TestRopes = struct

  structure ListSeq : SEQ = struct
    type 'a seq = 'a list
    val empty = []
    fun singleton s = [s]
    val null = null
    val length = length
    val sub = List.nth
    val concat = op @
    fun splitAt _ = raise Fail "todo"
    fun fromList x = x
    fun toList x = x 
  end

  structure R = RopesFn (structure S = ListSeq 
                         val maxLeafSize = 2)

  type 'a rope = 'a R.rope

(* int * int -> int rope *)
  fun spineRope (lo, hi) = let
    val len = hi - lo + 1
    fun f n = lo + n
    in
      if len <= R.maxLeafSize
      then R.fromList (List.tabulate (len, f))
      else R.concat (R.fromList (List.tabulate (R.maxLeafSize, f)),
		     spineRope (lo + R.maxLeafSize, hi))
    end

  fun println s = (print s; print "\n")

  fun prope show r = print (R.toString show r)

  fun test 0 = let
        val r = spineRope (0, 7)
        in
          (prope Int.toString) (R.balance r)
        end
   | test 1 = let
        val r = spineRope (0, 14)
        in
          (prope Int.toString) (R.balance r)
        end
    | test n = (println ("No such test: " ^ Int.toString n);
		raise Fail "")

end
