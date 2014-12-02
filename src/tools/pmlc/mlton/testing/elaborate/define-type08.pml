(* FAIL [unbound typaram] *)
_module myId (
  type myType <'a, 'b> = cont<'a, 'g>;
)
