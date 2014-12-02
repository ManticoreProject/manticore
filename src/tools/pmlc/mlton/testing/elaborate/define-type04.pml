(* FAIL [unbound typaram] *)
_module myId (
  type myType <'a, 'b> = (['a, 'b] / ['b, 'a]) -> 'g;
)
