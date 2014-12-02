(* FAIL [arity mismatch] *)
_module myModule _prim (
  type myType <'a> = ['a, 'a];
)

_module myOtherModule _prim (
  type myType <'a, 'b> = ['a, 'b];
  type myOtherType = myModule.myType<int32, int32>;
)
