(* FAIL [unbound typaram] *)
_module myId _prim (
  datatype myDt  = A of 'a | B | C;
)
