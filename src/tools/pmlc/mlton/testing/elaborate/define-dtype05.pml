(* FAIL [unbound typaram] *)
_module myId (
  datatype myDt  = A of 'a | B | C;
)
