(* FAIL [arity mismatch] *)
_module myModule (
  type myType <'a> = ['a, 'a];
)

_module myOtherModule (
  type myType <'a, 'b> = ['a, 'b];
  type myOtherType = myModule.myType<int32, int32>;
)
