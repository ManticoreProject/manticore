fun f x y = x + y
(* behaves the same as *)
(*fun f x = let
    fun f' y = x + y
    in
        f'
    end
*)
val z = (f 123) 100
val _ = print (itos z^"\n")
