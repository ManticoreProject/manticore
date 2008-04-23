structure CompilationUnit : sig

    val check : Error.err_stream * ParseTree.program -> AST.comp_unit

  end = struct

    fun check (err, {span, tree}) = raise Fail "todo"

  end (* CompilationUnit *)
