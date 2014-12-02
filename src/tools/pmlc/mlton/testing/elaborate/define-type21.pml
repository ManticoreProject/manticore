(* FAIL [unbound typaram] *)
_module myId (
  type myType<'a> = ['a, 'a];
  type myOtherType<'b> = ['a, 'a];
)
