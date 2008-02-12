structure LTCParTup : sig

    val transform : AST.module -> AST.module

  end = struct

    structure A = AST
    structure B = Basis

    fun transform m = let
        in
	    raise Fail "todo"
        end (* transform *)

  end (* LTCParTup *)
