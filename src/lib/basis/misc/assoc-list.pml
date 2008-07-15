structure AssocList =
  struct

(*    structure O = Option*)
(*

    val _ = print (o2s (O.SOME 1023)^"\n")
*)

    fun o2s opt = (case opt
           of Option.SOME _ => ""
)

  end
