(* FAIL [undefined type] *)
_module myId _prim (
  type myType <'a, 'b> = ['a, 'b];
  type myType' = anUndefinedType<int32, int32>;
)
