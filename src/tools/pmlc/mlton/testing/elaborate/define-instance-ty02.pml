(* FAIL [arity mismatch] *)
_module aModule (
  type myType<'a, 'b> = ['a, 'b];
)

_module myId (
  instance type aModule.myType<int32>;
)
