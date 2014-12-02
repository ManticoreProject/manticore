(* FAIL [unbound typaram] *)
_module myId _prim (
  type myType<'a> = ['a, 'a];
  type myOtherType<'b> = ['a, 'a];
)
