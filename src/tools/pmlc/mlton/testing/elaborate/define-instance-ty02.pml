(* FAIL [arity mismatch] *)
_module aModule _prim (
  type myType<'a, 'b> = ['a, 'b];
)

_module myId _prim (
  instance type aModule.myType<int32>;
)
