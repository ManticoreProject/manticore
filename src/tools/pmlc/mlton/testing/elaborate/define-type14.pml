(* FAIL [arity mismatch] *)
_module myId (
  type myType <'a> = ['a, 'a];
  type myType' = myType<int32, int32>;
)
