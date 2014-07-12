_primcode (
  (* THIS SHOULD FAIL, because "primOp" hasn't been defined in the lexer yet *)
  fun id1 () -> 'a = do primOp (nullVP) return ();
)
