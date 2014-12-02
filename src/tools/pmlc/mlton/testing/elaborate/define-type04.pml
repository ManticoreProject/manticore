(* FAIL [unbound typaram] *)
_module myId _prim (
  type myType <'a, 'b> = (['a, 'b] / ['b, 'a]) -> 'g;
)
