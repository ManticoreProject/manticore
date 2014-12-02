(* FAIL [unbound typaram] *)
_module myId _prim (
  type myType <'a, 'b> = cont<'a, 'g>;
)
